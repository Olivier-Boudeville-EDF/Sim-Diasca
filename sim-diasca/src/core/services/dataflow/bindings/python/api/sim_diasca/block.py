# Copyright (C) 2016-2017 EDF R&D
#
# This file is part of Sim-Diasca.
#
# Sim-Diasca is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# Sim-Diasca is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Sim-Diasca.
# If not, see <http://www.gnu.org/licenses/>.

# Author: Robin Huart (robin-externe.huart@edf.fr)


# Currently, workbench-related developments rely on a Python virtual
# environment, whereas the engine itself directly imports modules.

try:
    # If run directly from the engine:
    from sim_diasca.dataflow_types import *
    from common.trace_emitter import TraceEmitter

except ImportError:
    # If run from a Python virtual environment:
    from .dataflow_types import *
    from .common.trace_emitter import TraceEmitter


class Block(TraceEmitter):
    """Abstract class representing dataflow blocks generically, concentrating
    most of their state and behaviour definition.

    All the processing units defined by users of the API must inherit from
    the ProcessingUnit class, not directly from this Blockclass.
    """

    # Predefined sequence of attributes:
    __slots__ = ('name', 'tick_offset', 'simulation_date', 'input_port_specs',
                 'output_port_specs', 'input_ports', 'input_port_iterations',
                 'output_ports', 'output_port_iterations',
                 'activation_results')

    # Constructor:
    def __init__(self, name: BlockName,
                 input_port_specs: List[InputPortSpecification],
                 output_port_specs: List[OutputPortSpecification]):
        """Constructs a new base block instance, from specified name and port
        specs.
        """

        # Initialises the TraceEmitter base class:
        super().__init__()

        # Stores initial information, and encodes those whose purpose is to be
        # sent to the Erlang world:

        self.name = name
        self.input_port_specs = input_port_specs
        self.output_port_specs = output_port_specs

        # Default values for attributes that are expected to be updated:
        self.tick_offset = 0
        self.simulation_date = (0, 0, 0), (0, 0, 0)

        self.input_ports = {}
        self.output_ports = {}

        self.input_port_iterations = {}
        self.output_port_iterations = {}

        # Builds the list of input ports from the specifications:
        for ips in self.input_port_specs:

            # If a standard port is specified, directly stores its name:
            if ips.is_iteration is False:
                self.input_ports[ips.name] = InputPort(name=ips.name)

            # Otherwise it is an iteration (is_iteration is not a mere
            # boolean):
            else:

                input_port_iteration = InputPortIteration(
                    base_name=ips.name,
                    iteration_spec=ips.is_iteration)

                self.input_port_iterations[ips.name] = input_port_iteration

                # Records the initial iterated ports (if any):
                if input_port_iteration.indexes is not None:
                    for index in input_port_iteration.indexes:
                        iterated_name = self.get_input_iterated_port_name(
                            ips.name, index)
                        self.input_ports[iterated_name] = InputPort(
                            name=iterated_name)

        # Then builds similarly the list of output ports from the
        # specifications:

        for ops in self.output_port_specs:

            # If a standard port is specified:
            if ops.is_iteration is False:
                self.output_ports[ops.name] = OutputPort(name=ops.name)

            # Otherwise it is an iteration:
            else:
                output_port_iteration = OutputPortIteration(
                    base_name=ops.name,
                    iteration_spec=ops.is_iteration)

                self.output_port_iterations[ops.name] = output_port_iteration

                # Records the initial iterated ports (if any):
                if output_port_iteration.indexes is not None:
                    for index in output_port_iteration.indexes:
                        iterated_name = self.get_output_iterated_port_name(
                            ops.name, index)
                        self.output_ports[iterated_name] = OutputPort(
                            name=iterated_name)

        # Initializes the list in which output results will be accumulated
        # during the activation process:
        #
        self.activation_results = []

    def _set_simulation_state(self, simulation_state: list):
        """Sets the part of the state that must be synchronised with the
        counterpart Erlang process (typically initial data, built in Erlang at
        the end of the construction, and required in Python).
        """

        # Current tick offset:
        self.tick_offset = simulation_state[0]

        # Current date in the simulation:
        self.simulation_date = simulation_state[1]

    ##
    # API getters and input utilities.
    ##

    # Basic getters.

    def get_block_name(self):
        """Returns the name of the current dataflow block.
        """
        return self.name

    def get_initial_date(self):
        """Returns the initial date of this block (i.e. when that instance was
        created).
        """
        return self.initial_date

    def get_simulation_date(self):
        """Returns the current, absolute, date of the simulation.
        """
        return self.simulation_date

    def is_input_port_iteration_set(self, iteration_name: PortName):
        """Returns True if every input port in this iteration
        is in state 'set'
        """

        input_port_iteration = None

        try:
            input_port_iteration = self.input_port_iterations[iteration_name]
        except:
            msg = "unable to find an iterated port named '{}' ".format(
                iteration_name)
            self.send_error(msg)
            raise Exception(msg)

        # returns the value
        for index in input_port_iteration.indexes:

            iterated_port_name = self.get_input_iterated_port_name(
                iteration_name, index)

            input_port = self.input_ports[iterated_port_name]

            if input_port.status != 'set':
                return False

        return True

    def is_input_port_normal_set(self, input_port_name: PortName):
        """Returns True if this normal input port is set.
        """

        input_port = None

        try:
            input_port = self.input_ports[input_port_name]
        except:
            msg = "unable to find a normal port named '{}' ".format(
                input_port_name)
            self.send_error(msg)
            raise Exception(msg)

        return input_port.status == 'set'

    def is_input_port_set(self, input_port_name: PortName):
        """Returns True if the input port (normal or iterated) is set.
        """

        try:
            if input_port_name in self.input_port_iterations:
                return self.is_input_port_iteration_set(input_port_name)
            else:
                return self.is_input_port_normal_set(input_port_name)
        except Exception as e:
            # post-mortem analysis
            if (
                (input_port_name not in self.input_ports) and
                (input_port_name not in self.input_port_iterations)
            ):
                msg = "unable to find a normal or " \
                      "iterated port named '{}' ".format(input_port_name)
                self.send_error(msg)
                raise Exception(msg)
            else:
                raise e

    def get_input_port_value(self, input_port_name: PortName):
        """Returns the (raw) value currently held by the specified, supposedly
        set, standard input port.
        """

        # Gets that port from its name:
        input_port = self.input_ports[input_port_name]

        # Returns the value if the status is 'set', sends an error otherwise:
        if input_port.status == 'set':
            return input_port.value

        elif input_port.status == 'unset':
            self.send_error("Attempt to read the value of input port '{}', "
                            "whereas it is unset.".format(input_port_name))

        else:
            self.send_error(
                "Unexpected status for the"
                " input port named '{}': '{}'.".format(
                    input_port_name, input_port.status))

    # Gets all the values borne by the members of an input port iteration:
    def get_all_input_iteration_values(self, iteration_name: PortName):
        """Returns the values held by all the iterated ports of specified "
        input port iteration, expecting all of them to be set.
        """

        # Gets the input port iteration identified by its name:
        input_port_iteration = self.input_port_iterations[iteration_name]

        # Gathers in a list the values held by the corresponding iterated
        # ports, while detecting any unset port:

        port_values = []
        unset_ports = []

        for index in input_port_iteration.indexes:

            iterated_port_name = self.get_input_iterated_port_name(
                iteration_name, index)

            input_port = self.input_ports[iterated_port_name]

            if input_port.status == 'unset':
                unset_ports.append(iterated_port_name)

            port_values.append(input_port.value)

        # Tests whether list is non-empty:
        if unset_ports:
            self.send_warning("While getting the values corresponding to "
                              "input port iteration '{}', following iterated "
                              "ports have been found unset: {}".format(
                                  iteration_name, unset_ports))

        # Returns the list of port values:
        return port_values

    ##
    # API setters and output utilities:
    ##

    def create_channel_value(self, value, value_semantics: List[str],
                             value_unit: str, value_type: str):
        """Returns a channel value, obtained from the specified actual value
        and its related metadata.
        """
        return ChannelValue(value, value_semantics, value_unit, value_type)

    def create_channel_values(self, values, value_semantics: List[str],
                              value_unit: str, value_type: str):
        """Returns a list of channel values, obtained from the specified
        actual values and their common, related metadata.
        """
        return [ChannelValue(val, value_semantics, value_unit,
                             value_type) for val in values]

    # Assigns specified channel value to the output port identified by its name,
    # and encodes both this name and the value for a later sending to the
    # engine.
    def set_output_port_value(self, output_port_name: PortName,
                              channel_value: ChannelValue):
        """Sets specified output port to the specified channel value.
        """

        # Encoding for a further sending to Erlang:
        output_port_value = (output_port_name.encode(),
                             channel_value.encode())

        self.activation_results.append(output_port_value)

    def set_output_port_values(self,
                               output_port_values: List[Tuple[PortName,
                                                              ChannelValue]]):
        """Sets specified output ports to the specified channel values.
        """

        # Iterates on the pairs, and applies them:
        for port_value in output_port_values:
            OutputPortName = port_value[0]
            ChannelValue = port_value[1]
            self.set_output_port_value(OutputPortName, ChannelValue)

    # Sets the channel values of all members of an output port iteration:
    def set_all_output_iteration_values(self, iteration_name: PortName,
                                        channel_values: List[ChannelValue]):
        """Sets all the iterated ports of the specified output port iteration
        to the specified channel values, relying on their respective order for
        that.
        """

        # Gets the specified output port iteration:
        output_port_iteration = self.output_port_iterations[iteration_name]

        # Checks that the list of channel values matches the port count of the
        # iteration:

        number_of_values = len(channel_values)

        port_count = output_port_iteration.port_count

        if port_count == 0:

            # Should a port iteration not be connected at all (no iterated port
            # created, because the model is not itself connected to a downstream
            # block), writings will be nevertheless tolerated.
            #
            # This action is considered as legit, hence no warning is issued.

            #msg = "Trying to feed the output port iteration named '{}' " \
            #      "with {} values, whereas it has no iterated port "  \
            #      "(this writing is thus ignored as a whole).".format(
            #          iteration_name, number_of_values)
            #self.send_warning(msg)

            # Nothing done.

            pass

        else:

            if number_of_values < port_count:
                msg = "Trying to feed the {} iterated ports of " \
                      "the output iteration '{}' with too few "  \
                      "channel values ({}).".format(port_count,
                                                    iteration_name,
                                                    number_of_values)
                self.send_error(msg)
                raise Exception(msg)

            elif number_of_values > port_count:
                msg = "Trying to feed the {} iterated ports of " \
                      "the output iteration '{}' with too many " \
                      "channel values ({}).".format(port_count,
                                                    iteration_name,
                                                    number_of_values)
                self.send_error(msg)
                raise Exception(msg)

            # Correct match here:
            output_port_iteration.indexes = range(1, number_of_values + 1)

            iteration_base_name = output_port_iteration.base_name

            for port_index, channel_value in zip(output_port_iteration.indexes,
                                                 channel_values):
                output_port_name = self.get_output_iterated_port_name(
                    iteration_base_name, port_index)

                self.set_output_port_value(output_port_name, channel_value)



    ##
    # Helpers section.
    ##

    def get_input_iterated_port_name(self, input_iteration_name: PortName,
                                     index: int):
        """Returns the name of the iterated input port corresponding to
        the specified input iteration and port index.
        """
        try:
            iteration_base_name = self.input_port_iterations[
                input_iteration_name].base_name
        except KeyError as e:
            self.send_error("unable to find iterated input named {}; "
                            "available iterated inputs are: {}".format(
                                input_iteration_name,
                                self.input_port_iterations.keys()))
            raise e

        return Block.get_iterated_port_name(iteration_base_name, index)

    def get_output_iterated_port_name(self, output_iteration_name: PortName,
                                      index: int):
        """Returns the name of the iterated output port corresponding to
        the specified output iteration and port index.
        """
        try:
            iteration_base_name = self.output_port_iterations[
                output_iteration_name].base_name
        except KeyError as e:
            self.send_error("unable to find iterated output named {}; "
                            "available iterated outputs are: {}".format(
                                output_iteration_name,
                                self.output_port_iterations.keys()))
            raise e

        return Block.get_iterated_port_name(iteration_base_name, index)

    def get_iterated_port_name(iteration_base_name, index):
        """Common to input and ouput port iterations."""
        return iteration_base_name + '_iterated_' + str(index)
