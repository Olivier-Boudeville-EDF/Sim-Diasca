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


# Standard utilities:
import importlib


try:
    # If run directly from the engine:

    # For the state of the interpreter:
    from common.interpreter_state import InterpreterState

    # For TraceEmitter (parent class):
    from common.trace_emitter import TraceEmitter

except ImportError:
    # If run from a Python virtual environment:
    from .common.interpreter_state import InterpreterState
    from .common.trace_emitter import TraceEmitter


class MessageHandler(TraceEmitter):
    """Class defining how to handle incoming messages (received by the Python
    interpreter) and which actions they must trigger (including sending back new
    messages to the Erlang part).

    The key method describing the behaviour of the message handler is the
    handle_message/2 function.

    To preserve genericity, any incoming message will be decomposed by the
    communicator from a triplet with one leading technical element to a pair
    formed by a title and a body (in this order), which will have had its
    structure checked, i.e.:

        original_message == ( message_header, message_title, message_body )
        filtered_message == ( message_title, message_body )

    Then the message title is interpreted as an action to perform (it must match
    the name of an available method), the message body containing all the data
    required to perform it:

        method_name, data_list = filtered_message
    """

    __slots__ = ()

    def __init__(self):
        """
        A default constructor.
        """
        super().__init__()

    ##
    # Methods acting on messages themselves:
    ##

    def handle_message(self, message):
        """Generic method handling all incoming messages and translating their
        title part automatically into an action (calling a method bearing the
        same name as this title).
        """

        # First let the communicator filter the message at will:
        message_title, message_body = self.communicator.filter(message)

        # Then performs the actions identified by the message_title, while
        # catching the exceptions in order to propagate them outside, through
        # the binding:
        #
        try:
            return getattr(self, message_title)(message_body)

        except Exception as any_exception:
            return self.send_exception(any_exception)

    def request_return(self, request_result):
        """Defines what to do with the result of a request when successful.
        """

        return_message = 'request_completed', request_result

        return self.communicator.send(return_message)

    ##
    # Particular actions triggered by particular messages:
    ##

    def get_port_specifications(self, message_body):
        """Looks after the static declaration of the full description of all
        ports of a target processing unit.
        """

        # Interprets the only argument as the name of the module where the
        # processing unit is implemented:
        #
        module_name, class_name = self.__get_module_and_class(message_body)

        # Reaches the target module and the target class inside it:
        target_class = self.__reach_class(module_name, class_name)

        if hasattr(target_class, 'get_port_specifications'):

            # Calls the static method defining the port specifications:
            port_specs = target_class.get_port_specifications()
            input_port_specs = port_specs[0]
            output_port_specs = port_specs[1]

            # Encodes the port specifications before sending them to Erlang:
            #
            encoded_input_port_specs = [ips.encode() for ips in
                                        input_port_specs]

            encoded_output_port_specs = [ops.encode() for ops in
                                         output_port_specs]

            # "Returns" these encoded port specifications:
            return self.request_return((encoded_input_port_specs,
                                        encoded_output_port_specs))

        else:

            # If the method is not defined, returns an informative message:
            return self.request_return(b'no_port_specifications_declared')

    def get_declared_semantics(self, message_body):
        """Looks after the static declaration of all the semantics used by
        the ports of a target processing unit.
        """

        # Interprets the only argument as the name of the module where the
        # processing unit is implemented:
        #
        module_name, class_name = self.__get_module_and_class(message_body)

        # Reaches the target module and the target class inside it:
        target_class = self.__reach_class(module_name, class_name)

        if hasattr(target_class, 'get_declared_semantics'):

            # Calls the static method defining the port specifications:
            semantics = target_class.get_declared_semantics()

            # Encodes the semantics before sending them to Erlang:
            encoded_semantics = [sem.encode() for sem in semantics]

            # "Returns" the encoded port semantics:
            return self.request_return(encoded_semantics)

        else:

            # If the method is not defined, returns an informative message:
            return self.request_return(b'no_semantics_declared')

    def get_declared_types(self, message_body):
        """Looks after the static declaration of all the types supported for
        the values borne by the ports of a target processing unit.
        """

        # Interprets the only argument as the name of the module where the
        # processing unit is implemented:
        module_name, class_name = self.__get_module_and_class(message_body)

        # Reaches the target module and the target class inside it:
        target_class = self.__reach_class(module_name, class_name)

        if hasattr(target_class, 'get_declared_types'):

            # Calls the static method defining the port specifications:
            value_types = target_class.get_declared_types()

            # Encodes the semantics before sending them to Erlang:
            encoded_value_types = [vt.encode() for vt in value_types]

            # "Returns" the encoded port semantics:
            return self.request_return(encoded_value_types)

        else:

            # If the method is not defined, returns an informative message:
            return self.request_return(b'no_types_declared')

    def instantiate_unit(self, message_body):
        """Instantiates a target processing unit with the construction
        parameters found in the incoming message.
        """

        # Extracts the arguments necessary to reach the target class and
        # instantiate it properly:
        module_name, class_name = self.__get_module_and_class(message_body)
        construction_parameters = message_body[2:]

        # The first construction parameter is mandatorily the unit name, which
        # is a string that needs to be decoded:
        #
        construction_parameters[0] = self.communicator.decode_string(
            construction_parameters[0])

        # Announces the creation of a processing unit in a trace:
        self.send_debug("Creating a processing unit {} of type {} from "
                        "{}.py.".format(construction_parameters[0],
                                        class_name, module_name))

        # Instantiates the requested unit class:
        TargetClass = self.__reach_class(module_name, class_name)
        target_instance = TargetClass(*construction_parameters)

        # Informs that the processing unit was successfully instantiated:
        self.send_debug("Processing unit successfully created: {}"
                        .format(target_instance.get_unit_description()))

        # Saves this unit object and creates a reference on it:
        new_instance_ID = InterpreterState.units_creation_counter
        InterpreterState.processing_units[new_instance_ID] = target_instance

        # Increments the ever-increasing global counter used to make unique
        # IDs:
        InterpreterState.units_creation_counter += 1

        # Encodes some instantiation results before sending them to Erlang:
        encoded_activation_policy = target_instance.activation_policy.encode()

        encoded_input_port_specs = [ips.encode() for ips in
                                    target_instance.input_port_specs]

        encoded_output_port_specs = [ops.encode() for ops in
                                     target_instance.output_port_specs]

        # Sends back the reference to the sender Erlang process, along with
        # the data generated here that are requested by the Erlang process:
        #
        instantiation_results = (new_instance_ID,
                                 encoded_activation_policy,
                                 encoded_input_port_specs,
                                 encoded_output_port_specs)

        return self.request_return(instantiation_results)

    def activate_unit(self, message_body):
        """Triggers the activate/1 method of a target processing unit and taking
        into account the inputs found in the incoming message.

        Although the activate/1 method of a processing unit does not take any
        parameter in input, the incoming message contains data which is used to
        update the state of the processing unit prior to activating it. These
        last actions are achieved through the call to a manage_activation/4
        method that wraps the call to activate/1.

        The input data taken by this manage_activation/4 method are mainly, if
        not only, the context of the simulation and the statuses of the input
        ports (and of the port iterations, if any).

        The activate/1 method is then called.

        Once completed, the manage_activation/4 wrapper method returns the
        encoded version of its outputs, which are the values of the output ports
        that have been fed by the activate/1 method. The encoding makes them
        ready to be sent to the native coupling infrastructure.
        """

        # Extracts the arguments necessary to reach the target processing unit,
        # along with the list of data that may be needed for the computations
        # involved in the activate/1 method (i.e. the list of input ports, with
        # names and statuses):
        #
        instance_ID = message_body[0]
        activation_data = message_body[1:]

        # Calls the method managing the activation process:
        target_instance = InterpreterState.processing_units[instance_ID]

        encoded_activation_results = target_instance.manage_activation(
            *activation_data)

        # Sends back the reference to the sender Erlang process, along with the
        # data generated here that are requested by this controlling process:
        #
        return self.request_return(encoded_activation_results)

    def delete_unit(self, message_body):
        """Triggers the deletion method of a target processing unit.
        """

        # Extracts the argument necessary to reach the target processing unit:
        # expectedly the only component (not a list)
        instance_ID = message_body

        # Calls the destruction of the dictionary entry, which removes the only
        # reference to the processing unit and thus triggers its destruction by
        # the garbage collector:
        del InterpreterState.processing_units[instance_ID]

        # Note: Since 'InterpreterState' and its 'processing_units' field (the
        # mentioned dictionary) are both mutable types, this should result in
        # an updated (smaller by 1 entry) dictionary inside 'InterpreterState'.

        return None

    ##
    # Miscellaneous helper methods:
    ##

    def __get_module_and_class(self, message_body):
        """Helper method extracting the target module and class from a standard
        message body.
        """

        module_name = message_body[0].decode()
        class_name = message_body[1].decode()

        return module_name, class_name

    def __reach_class(self, module_name, class_name):
        """Imports the target module and gets the target class inside."""

        target_module = importlib.import_module(module_name)

        return getattr(target_module, class_name)
