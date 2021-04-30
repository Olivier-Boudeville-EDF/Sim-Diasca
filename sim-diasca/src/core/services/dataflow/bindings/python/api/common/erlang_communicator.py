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


# Erlport dependencies:
import erlport.erlang
from erlport.erlterms import Atom
from erlport.erlterms import List as ErlangList


try:
    # If run directly from the engine:

    # Imports the base class from which any communicator should inherit:
    from common.communicator import BaseCommunicator

    # To be able to send exceptions to Erlang as a TraceEmitter:
    from common.trace_emitter import TraceEmitter

except ImportError:
    # If run from a Python virtual environment:
    from .common.communicator import BaseCommunicator
    from .common.trace_emitter import TraceEmitter


class ErlangCommunicator(BaseCommunicator):
    """Communicator class defining how this Python binding is to communicate
    with the (Erlang) core of the engine.

    An instance of this class is intended to be *the unique communicator*
    (singleton) used within the corresponding Python interpreter to communicate
    with Erlang.
    """

    __slots__ = ('erlang_pid')

    def __init__(self, initiator_erlang_pid):
        """Constructor of a communicator that is specific to Erlang.

        The only attribute of such a communicator is the Erlang PID of the
        process which sent the message being processed.
        """

        # (Erlang) PID of the process which triggered an action:
        self.erlang_pid = initiator_erlang_pid

    def send(self, message):
        """Sends a message from Python to the Erlang core.

        Here the 'send' operation is achieved by the 'cast' function implemented
        by the ErlPort library.

        Moreover, in order to be able to perform selective receives in the
        Erlang world onto the messages originating from Python, we add a flag to
        the beginning of the message that is to be sent (a header), which takes
        the form of an Erlang atom.

        The struture of an **output message** is conventionnaly defined as a
        2-tuple of the form **( message_headers, message_body )**.

        The 'message_headers' part is itself a tuple of size 2 or 3: its first
        element is always 'python_message', while the remaining ones depend on
        the type of message:

            - for traces (trace_emitter.py), the 2 additional headers are:
              ( 'trace_emitted', trace_type:str )

            - for exceptions (idem):
              ( 'exception_raised', exception_type:str )

            - for request results, the only additional header is:
              'request_completed'

        These additional headers must be the first members of the 'message'
        argument received here, received as strings. They will be turned into
        Erlang atoms (actually the ErlPort equivalent type) before sending.

        The 'message_body' part of the final output message must be set inside a
        container stored in the last element of the argument.

        Considering all this, the 'message' argument is thus also expected to be
        a tuple of total size 3 or 4 (additional headers above + message_body).

        """

        # Converts the conventional headers of an outgoing message in Erlang
        # atoms:

        message_size = len(message)
        message_headers = list(message[:message_size - 1])

        for i, header_item in enumerate(message_headers):
            if isinstance(header_item, str):
                message_headers[i] = Atom(header_item.encode())
            elif isinstance(header_item, bytes):
                message_headers[i] = Atom(header_item)

        # Isolates the message_body:
        message_body = message[message_size - 1]

        # Prepends a binding-specific atom used as a flag to allow the receiving
        # Erlang process to perform selective receives:

        new_message = ((Atom(b'python_message'), *tuple(message_headers)),
                       message_body)

        # Examples of resulting output messages received by Erlang:
        #
        #     - a trace: ( ( python_message, trace_emitted, debug ),
        #                  "This is an example of debug-level trace" )
        #
        #     - an exception: ( ( python_message, exception_raised,
        #                         'NotImplementedError' ),
        #                       "Traceback in lines below:[...]" )
        #
        #     - a request result:
        #       ( ( python_message, request_completed ),
        #         [ [ <<"port1">>, 2.3 ], [ <<"port2">>, "Stay classy" ] ] )

        # Performs the actual sending operation to Erlang thanks to ErlPort:
        erlport.erlang.cast(self.erlang_pid, new_message)

    def filter(self, message):
        """Describes what filtering operations are to be done on messages
        coming from Erlang.
        """

        if not isinstance(message, tuple):
            raise TypeError("The incoming message is not a tuple: {}".format(
                message))

        # The first component of a message coming from Erlang must mandatorily
        # be the PID of the sending process:
        #
        self.erlang_pid = message[0]

        # Checks the basic structure of the message, assuming that the first
        # element of the tuple was indeed an Erlang PID (to be able to send
        # traces):
        #
        if not isinstance(message[1], Atom) or len(message) != 3:
            local_trace_emitter = TraceEmitter(self)
            invalid_message_structure_exception = TypeError(
                "The message structure does not follow the convention: {}"
                .format(message))
            local_trace_emitter.send_exception(
                invalid_message_structure_exception)

        # Then two distinct parts must follow inside this same message, a title
        # and a body, the former being transmitted as an Erlang atom.
        #
        # These two last parts form the filtered message:
        #
        return message[1].decode(), message[2]

    def decode_string(self, string: ErlangList):
        """Decodes for Python a string sent from Erlang (seen as a list, yet
        known to actually be a string).

        Erlang and ErlPort treat lists and strings the same way. However, the
        Python 'List' class, into which ErlPort turns Erlang lists, provides a
        'to_string' method. Thus, one can make use of this method whenever one
        knows that a list received from Erlang corresponds actually to a string.
        """

        return string.to_string()
