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

    # The abstract mother class from which all (Python) processing units
    # inherit:
    from sim_diasca.block import Block
    from sim_diasca.dataflow_types import *

except ImportError:
    # If run from a Python virtual environment:

    from .block import Block
    from .dataflow_types import *


from abc import abstractmethod


class ProcessingUnit(Block):
    """Base class for all actual dataflow processing units.

    All the processing units defined by the users of this API must inherit from
    this class. The API is defined as the set of methods bound to this class
    (and actually also to its parent, the (abstract) Block class, which cannot
    be used directly).

    The life of a processing unit mainly boils down to:

     - first being created, its construction being driven thanks to the
       classical __init__ method, allowing to define what are its input and
       output ports, and how it shall be activated (typically whenever at least
       one of its input ports is set, or if and only if all of them are set)

     - then being activated (any number of times), resulting on its associated
       computations to be performed, fed by its own state and by the values (if
       any) set in its input ports, and possibly feeding in turn its output
       ports
    """

    # Predefined sequence of attributes:
    __slots__ = ('activation_policy', 'activation_results')

    # Constructor:
    def __init__(self, name: BlockName, activation_policy: ActivationPolicy,
                 input_port_specs: List[InputPortSpecification],
                 output_port_specs: List[OutputPortSpecification]):
        """The constructor of all dataflow processing units.

        It allows to define the name of the unit, on which policy it should be
        activated, and the full specifications of the input and output ports it
        comprises.
        """

        # Construction of the mother class:
        super().__init__(name, input_port_specs, output_port_specs)

        # Stores the activation policy as the only attribute (currently)
        # specific to processing units:
        self.activation_policy = activation_policy

        # Initializes the list in which output results will be accumulated
        # during the activation process:
        #
        self.activation_results = []

    def __del__(self):
        """The destructor of all dataflow processing units.

        It sends a trace in order to notify that the instance has indeed been
        deleted and that no reference should remain on the instance. Memory
        being theoretically freed by the garbage collector, there sould be no
        more memory usage due to this instance. (This notification might help
        debug memory issues.)
        """
        try:
            self.send_debug("The Python processing unit {} is being deleted"
                            .format(self.name))
        # May happen apparently at simulation tear-down:
        except EOFError:
            pass

    # Wraps the 'activate' method of any (concrete) child unit class:
    def manage_activation(self, simulation_state: tuple,
                          input_ports_data: list,
                          input_port_iterations_data: list,
                          output_port_iterations_data: list):
        """Interface layer between the Erlang and Python activate/1 methods.

        Called by the Erlang side; prepares for, calls the activate/1 method,
        and handles its result.
        """

        # Translates and binds to the local state the data received from
        # Erlang:
        self.__prepare_activation(simulation_state, input_ports_data,
                                  input_port_iterations_data,
                                  output_port_iterations_data)

        # Trace informing that the user-defined activate/1 method can start:
        #
        self.send_debug("Entering the core python activation method for the "
                        "unit: {}.".format(self.name))

        # Calls the user-defined activate/1 method:
        #
        self.activate()

        # Replaces the internal reference to the (potentially big) list of
        # activation results by a locally defined one, so that as soon as this
        # method has ended, no reference remains and memory is freed:
        #
        activation_results = self.activation_results

        self.activation_results = []

        # Activation results were already encoded by the functions of the API,
        # so they can be return as is:
        #
        return activation_results

    # Binds all the data requestable through the API to the state of the
    # current instance.
    #
    def __prepare_activation(self, simulation_state: tuple,
                             input_ports_data: list,
                             input_port_iterations_data: list,
                             output_port_iterations_data: list):
        """Internal method decoding the data received from Erlang and binding
        them to the local state of the processng unit.

        The state imported from Erlang, which is to update the corresponding
        parts of the Python one, is separated in three parts (arguments):

            - *simulation_state* is about everything not related to any port:
              currently, it stores only the tick offset (integer) and the
              simulation date (2-tuple of the form
              ((Yr,Mon,Day),(Hr,Min,Sec))).

            - *input_ports_data* stores pairs of the form
              (port_name,port_value)

            - *input_port_iterations_data* stores the up-to-date structural
              metadata associated to input port iterations, under the form of
              3-tuples storing the base name, the multiplicity and the list of
              indexes (this latter element is maybe useless ?).

            - *output_port_iterations_data* stores the up-to-date structural
              metadata associated to output port iterations, under the form of
              3-tuples storing the base name, the multiplicity and the list of
              indexes (this latter element is maybe useless ?).

        """

        # Copies the pieces of simulation state to update:
        self._set_simulation_state(simulation_state)

        # Decodes the statuses of input ports received from Erlang:
        for ipd in input_ports_data:

            # Gets the input port name:
            input_port_name = ipd[0].decode()

            # Updates the status of the corresponding input port:
            if input_port_name not in self.input_ports:
                self.input_ports[input_port_name] = InputPort(
                    name=input_port_name)

            self.input_ports[input_port_name].update(ipd[1])

        # Decodes the port iterations received from Erlang:
        for ipid in input_port_iterations_data:

            # Gets and decodes the base components of an iteration description:
            iteration_base_name = ipid[0].decode()
            iteration_multiplicity = ipid[1]
            iteration_indexes = ipid[2]

            # Binds these information to their names in the dictionary of input
            # port iterations:
            #
            self.input_port_iterations[iteration_base_name].update(
                iteration_multiplicity, iteration_indexes)

        # Decodes the port iterations received from Erlang:
        for opid in output_port_iterations_data:

            # Gets and decodes the base components of an iteration description:
            iteration_base_name = opid[0].decode()
            iteration_multiplicity = opid[1]
            iteration_indexes = opid[2]

            # Binds these information to their names in the dictionary of input
            # port iterations:
            #
            self.output_port_iterations[iteration_base_name].update(
                iteration_multiplicity, iteration_indexes)

    # Declares the activate/1 method that must be overridden by the child
    # (user-defined) units.
    #
    @abstractmethod
    def activate(self):
        """This abstract method (mandatorily overridden in each actual unit)
        defines the computations that it is to perform.

        This is the place where, based on the state of its input ports (their
        values feeding the processings to be done) and any state it would have,
        the unit is able to perform its domain-specific computations.

        The results thereof are generally used, at least partly, in order to
        assign in turn the output ports of this unit.
        """
        raise NotImplementedError("The activate/1 method is not implemented.")
