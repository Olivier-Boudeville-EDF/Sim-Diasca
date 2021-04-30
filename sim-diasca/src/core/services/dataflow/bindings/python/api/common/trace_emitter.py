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


# Standard library tools:
from abc import ABC as AbstractBaseClass
from traceback import format_exception


# Global state of the interpreter, where to find the global_communicator:
try:
    # If run directly from the engine:
    from common.interpreter_state import InterpreterState

except ImportError:
    # If run from a Python virtual environment:
    from .common.interpreter_state import InterpreterState


class TraceEmitter(AbstractBaseClass):
    """Class defining how to send applicative traces (including errors and
    exceptions) outside of the Python binding, directly in the coupling
    infrastructure.
    """

    __slots__ = ('communicator')

    def __init__(self, communicator=None):
        """Constructor that binds once for all the communicator (a singleton) to
        the instantiated trace emitter.
        """
        if communicator:
            self.communicator = communicator
        else:
            self.communicator = InterpreterState.global_communicator

    def send_debug(self, trace_message: str):
        """Specialised trace sending API method, used for debug information.
        """
        self.__send_generic_trace('debug', trace_message)

    def send_trace(self, trace_message: str):
        """Specialised trace sending API method, used for basic traces.
        """
        self.__send_generic_trace('trace', trace_message)

    def send_info(self,  trace_message: str):
        """Specialised trace sending API method, used for more important
        notifications.
        """
        self.__send_generic_trace('info', trace_message)

    def send_warning(self, trace_message: str):
        """Specialised trace sending API method, used for warning messages.
        """
        self.__send_generic_trace('warning', trace_message)

    def send_error(self, trace_message: str):
        """Specialised trace sending API method, used for reporting errors.
        """
        self.__send_generic_trace('error', trace_message)

    def __send_generic_trace(self, trace_type: str, trace_message: str):
        """Generic internal function used for sending traces to the external
        world.

        Sends the trace message (a triplet) via the communicator. The trace
        message is characterized by the prepended 'trace_emitted' string.
        """

        # Sends the trace message (a 3-tuple) via the communicator:
        self.communicator.send(('trace_emitted', trace_type, trace_message))

    def send_exception(self, my_exception: Exception):
        """Generic internal function used for redirecting caught exceptions
        towards the external world.

        Sends the exception as a message (a triplet) via the communicator. The
        exception message is characterized by the prepended 'exception_raised'
        string. The type of the exception is extracted from the exception object
        and formatted, in order to be separated from the representation.
        """

        # Formats the exception and its type:
        formatted_exception_strings = format_exception( type(my_exception),
            my_exception, my_exception.__traceback__)

        formatted_exception = ''.join(formatted_exception_strings)

        exception_type = my_exception.__str__()

        self.communicator.send( ('exception_raised', exception_type,
                                 formatted_exception) )
