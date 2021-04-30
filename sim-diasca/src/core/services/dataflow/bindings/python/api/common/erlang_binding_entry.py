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


# ErlPort (http://erlport.org/) is the library used under the hood for
# performing the actual communications between Erlang and Python.


# Erlang-specific dependencies of the Python binding API:
import erlport.erlang

import platform
import os
import sys


try:
    # If run directly from the engine:
    from common.erlang_communicator import ErlangCommunicator

    # For the state of the interpreter:
    from common.interpreter_state import InterpreterState

    # Imports the MessageHandler class defining the treatment of incoming
    # messages:
    from common.message_handler import MessageHandler

except ImportError:
     # If run from a Python virtual environment:
    from .common.erlang_communicator import ErlangCommunicator
    from .common.interpreter_state import InterpreterState
    from .common.message_handler import MessageHandler


def handle_message(message):
    """Message-handling function required by ErlPort, redirecting to the
    global_message_handler singleton.

    The real message handling method is implemented by the MessageHandler class
    which is instantiated only once.

    Thus we would expect to bind this method to ErlPort's message handling
    loop. However, our custom method is not of the form expected by
    ErlPort.

    Indeed, since it is an instance method, it takes *two* arguments: 'self' and
    the message to treat. On its side, ErlPort expects a function taking exactly
    one argument.

    Our way of circumventing this issue is to create this function whose only
    purpose is to redirect the message to the handle_message/1' method of the
    unique global_message_handler.
    """
    InterpreterState.global_message_handler.handle_message(message)


def init_binding(erlang_pid):
    """Entry point of the Python binding for Erlang processes.

    This function initialises the global state of the interpreter, stored in the
    (never instantiated) InterpreterState class. This global state is most
    notably comprising a unique communicator and a unique message handler.

    It also has to tell ErlPort which method of the message handler is to be
    used for processing incoming messages.

    Returns, for convenience, the current Python version and code path (the list
    of directories searched for modules and packages).
    """

    # Sets the global_communicator responsible for communicating with
    # the (Erlang) core of the engine:
    InterpreterState.global_communicator = ErlangCommunicator(erlang_pid)

    # Initializes some other class variables that form, along with the
    # global_communicator, our definition of the state of an interpreter:
    InterpreterState.global_message_handler = MessageHandler()

    # Binds the function defined below, handling messages from the Erlang part
    # of the engine, to ErlPort's own message handler:
    #
    erlport.erlang.set_message_handler(handle_message)

    return (platform.python_version(), os.getcwd(), sys.path)
