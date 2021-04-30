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
from enum import Enum, unique
from typing import List, Union, Tuple


# Currently, workbench-related developments rely on a Python virtual
# environment, whereas the engine itself directly imports modules.

try:
    # If run directly from the engine:
    from common.trace_emitter import TraceEmitter

except ImportError:
    # If run from a Python virtual environment:
    from .common.trace_emitter import TraceEmitter


##
##
# --- ENUMERATIONS ---
##
##


# Base class defining enums with automatic numbering and hidden values:
#
# Note: inspired by the official Python documentation:
#           https://docs.python.org/3/library/enum.html
#
class AutoHidingEnum(Enum):
    """
    Base class defining enums with automatic numbering and hidden values.
    """

    def __new__(cls):
        value = len(cls.__members__) + 1
        obj = object.__new__(cls)
        obj._value_ = value
        return obj

    def __repr__(self):
        return '<{}.{}>'.format(self.__class__.__name__, self.name)

    def encode(self):
        lower_case_name = self.name.lower()
        return lower_case_name.encode()


# The activation policy governing the condition(s) to match before triggering
# the evaluation of the 'activate' method of a processing unit:
#
@unique
class ActivationPolicy(AutoHidingEnum):
    """Enumeration matching the activation policies available in the core of the
    engine.
    """
    ACTIVATE_ON_NEW_SET = ()
    ACTIVATE_WHEN_ALL_SET = ()
    CUSTOM_ACTIVATION = ()


# The available constraints, against which any port value can be checked:
#
# Note: these must match the list of constraints in Sim-Diasca, which can be
# found in class_DataflowBlock_functions.hrl:validate_constraints/2 as Erlang
# atoms.
#
@unique
class PortConstraints(AutoHidingEnum):
    """
    Enumeration matching the constraints available in Sim-Diasca.
    """
    NON_NULL = ()
    STRICTLY_NEGATIVE = ()
    NEGATIVE = ()
    STRICTLY_POSITIVE = ()
    POSITIVE = ()
    IN = ()                      # to be used as ( IN, list )
    BETWEEN = ()                 # to be used as ( BETWEEN, x, y )
    LOWER_THAN = ()              # to be used as ( LOWER_THAN, x )
    GREATER_THAN = ()            # to be used as ( GREATER_THAN, x )


##
##
# --- CLASSES ---
##
##


# Definition of the type used for specifying the input port(s) of a processing
# unit:
#
class InputPortSpecification:
    """
    User-defined specifications describing an input port (iterated or not).
    """

    # Predefined sequence of attributes:
    __slots__ = ('name', 'comment', 'is_iteration', 'value_semantics',
                 'value_unit', 'value_type_description', 'value_constraints')

    # Initial definition of an input port specification:
    def __init__(self, name, comment, value_semantics, value_unit,
                 value_type_description, value_constraints=[],
                 is_iteration=False):
        """
        Initial specification of all the characteristics of an input port.
        """

        self.name = name
        self.comment = comment
        self.is_iteration = is_iteration
        self.value_semantics = value_semantics
        self.value_unit = value_unit
        self.value_type_description = value_type_description
        self.value_constraints = value_constraints

    # Encoding funtion:
    def encode(self) -> list:
        """
        Transforms an InputPortSpecification object into a list of tuple pairs,
        more easily handled by ErlPort.
        """

        encoded_semantics = [semstring.encode() for semstring
                             in self.value_semantics]

        encoded_constraints = []
        # each of the constraints might be encoded in different ways
        for constraint in self.value_constraints:

            encoded_constraint = None

            if (isinstance(constraint, tuple) and
                    len(constraint) == 2 and
                    isinstance(constraint[1], list)):
                # it might be a tuple made of a string and list, such as ("IN",
                # ["a","b"])
                encoded_constraint = (constraint[0].encode(), [
                                      v.encode() for v in constraint[1]])
            else:
                # by default we encode anything we find (hope it works)
                encoded_constraint = constraint.encode()

            encoded_constraints.append(encoded_constraint)

        return [(b'name', self.name.encode()),
                (b'comment', self.comment.encode()),
                (b'is_iteration', self.is_iteration),
                (b'value_semantics', encoded_semantics),
                (b'value_unit', self.value_unit.encode()),
                (b'value_type_description',
                 self.value_type_description.encode()),
                (b'value_constraints', encoded_constraints)]


# Definition of the type used for specifying the output port(s) of a processing
# unit:
#
class OutputPortSpecification:
    """
    User-defined specifications describing an output port (iterated or not).
    """

    # Predefined sequence of attributes:
    __slots__ = ('name', 'comment', 'is_iteration', 'produces_result',
                 'value_semantics', 'value_unit', 'value_type_description',
                 'value_constraints')

    # Initial definition of an output port specification:
    def __init__(self, name, comment, value_semantics, value_unit,
                 value_type_description, value_constraints=[],
                 is_iteration=False, produces_result=False):
        """
        Initial specification of all the characteristics of an output port.
        """

        self.name = name
        self.comment = comment
        self.is_iteration = is_iteration
        self.produces_result = produces_result
        self.value_semantics = value_semantics
        self.value_unit = value_unit
        self.value_type_description = value_type_description
        self.value_constraints = value_constraints

    # Encoding funtion:
    def encode(self) -> list:
        """Transforms an OutputPortSpecification instance into a list of pairs,
        more easily handled by ErlPort.
        """

        encoded_semantics = [semstring.encode() for semstring
                             in self.value_semantics]

        return [(b'name', self.name.encode()),
                (b'comment', self.comment.encode()),
                (b'is_iteration', self.is_iteration),
                (b'produces_result', self.produces_result),
                (b'value_semantics', encoded_semantics),
                (b'value_unit', self.value_unit.encode()),
                (b'value_type_description',
                 self.value_type_description.encode()),
                (b'value_constraints',
                 [constraint.encode() for constraint in
                  self.value_constraints])]


# Base class for input and output ports:
#
class GenericPort(TraceEmitter):
    """
    Abstract, base class describing any kind of ports (input or output) of a
    dataflow block.
    """

    # Fixed set of attributes of an input port:
    __slots__ = ('name', 'status', 'value')

    # Constructor of an input port:
    def __init__(self, name, status='unset', value=None):
        """Contructs a (generic, abstract) port.
        """

        # Stores the name of that port (also available in the keys of the input
        # or output port dictionary):
        #
        self.name = name

        # Possibly using default values then:
        self.status = status
        self.value = value

    # Updates the status of a port with a status received from Sim-Diasca:
    def update(self, encoded_status):
        """Updates the status of this port, from the counterpart information
        received from the engine.
        """

        # The status received from Erlang is either the 'unset' binary or a
        # pair in the form (in Erlang): { 'set', Value }

        if encoded_status == b'unset':
            self.status = 'unset'
            self.value = None

        elif (isinstance(encoded_status, tuple) and
              len(encoded_status) == 2 and
              encoded_status[0] == b'set'):
            self.status = 'set'
            self.value = encoded_status[1]

        else:
            self.send_error("Invalid port status received from the engine "
                            "while updating: {}".format(encoded_status))


# Definition of an input port, actually a subset of its Erlang counterpart,
# made by taking only the data relevant for the computations of an
# 'activate' method:
#
class InputPort(GenericPort):
    """
    Definition of an input port.
    """

    # This is actually a subset of its Erlang counterpart, made by taking only
    # the data relevant for the computations of the activate/1 (Python) method:

    __slots__ = ()

    # Constructor of an input port:
    def __init__(self, name, status='unset', value=None):
        """Contructs an input port.
        """
        super().__init__(name, status, value)

    def __str__(self):
        """Returns a textual representation of this input port.
        """
        return "Input port named '" + self.name + "'"


# Definition of an output port, actually a subset of its Erlang counterpart,
# made by taking only the data relevant for the computations of an 'activate'
# method:
#
class OutputPort(GenericPort):
    """
    Output port of a Python dataflow block.
    """

    __slots__ = ()

    # Constructor of an output port:
    def __init__(self, name, status='unset', value=None):
        """
        Contructor of an output port.
        """
        super().__init__(name, status, value)

    # Representation of an output port:
    def __repr__(self):
        """
        Representation of an output port.
        """
        return 'OutputPort : ' + self.name


# Base class for input and output port iterations:
#
class GenericPortIteration(TraceEmitter):
    """
    Generic port iteration of a Python dataflow block.
    """

    # Fixed set of attributes of a port iteration:
    __slots__ = ('base_name', 'port_count', 'bounds', 'indexes')

    # Constructor of a generic port iteration:
    def __init__(self, base_name, iteration_spec, indexes=None):
        """
        Contructor of a generic port iteration.
        """

        # Saves the name of the port iteration, which is the base name for all
        # its members:
        self.base_name = base_name

        # Decodes the user-defined multiplicity specification:
        if isinstance(iteration_spec, tuple):
            self.port_count = iteration_spec[0]
            bound_spec = iteration_spec[1]
            if isinstance(bound_spec, tuple):
                self.bounds = bound_spec
            else:
                self.bounds = (0, bound_spec)
        elif isinstance(iteration_spec, bool):
            self.port_count = 0
            self.bounds = (0, None)
        elif isinstance(iteration_spec, int):
            self.port_count = iteration_spec
            self.bounds = (0, None)
        else:
            self.port_count = 0
            self.bounds = (0, None)

        # List of port indexes:
        self.indexes = indexes

        if indexes is not None and self.port_count != len(indexes):
            self.send_error("Initially, the list of indexes (length {}) "
                            "does not match the declared multiplicity ({}) "
                            "for the input port iteration {}.".format(
                                len(indexes), self.port_count, base_name))

    # Updates the port iteration according to the data received from Erlang:
    def update(self, erlang_multiplicity, erlang_indexes: List[int]):
        """
        Updates the port iteration according to the data received from Erlang.
        """

        # In the Erlang part of Sim-Diasca, multiplicity is defined as a tuple
        # of length 2 of the form { port_count, { min_bound, max_bound } }:
        if (isinstance(erlang_multiplicity, tuple) and
                len(erlang_multiplicity) == 2):

            # The count of member ports is just copied:
            self.port_count = erlang_multiplicity[0]

            # The bounds are stored as tuple of length 2 which is also copied:
            bounds_tuple = erlang_multiplicity[1]
            if (isinstance(bounds_tuple, tuple) and
                    len(bounds_tuple) == 2):
                self.bounds = bounds_tuple

            else:
                self.send_error("Ill-formed bounds tuple received from "
                                "Erlang for the port iteration '{}': {}"
                                .format(self.base_name, bounds_tuple))

        else:
            self.send_error("Ill-formed multiplicity received from Erlang "
                            "while updating the port iteration '{}': {}".format(
                                self.base_name, erlang_multiplicity))

        # The port indexes are a simple list being copied:
        self.indexes = erlang_indexes


# Definition of an input port, actually a subset of its Erlang counterpart,
# made by taking only the data relevant for the computations of an
# 'activate' method:
#
class InputPortIteration(GenericPortIteration):
    """
    Input port iteration of a Python dataflow block.
    """

    __slots__ = ()

    # Constructor of an input port iteration:
    def __init__(self, base_name, iteration_spec, indexes=None):
        """
        Contructor of an input port iteration.
        """
        super().__init__(base_name, iteration_spec, indexes)

    # Representation of an input port iteration:
    def __repr__(self):
        """
        Representation of an input port iteration.
        """
        return 'InputPortIteration : ' + self.base_name


# Definition of an input port, actually a subset of its Erlang counterpart,
# made by taking only the data relevant for the computations of an
# 'activate' method:
#
class OutputPortIteration(GenericPortIteration):
    """
    Output port iteration of a Python dataflow block.
    """

    __slots__ = ()

    # Constructor of an output port iteration:
    def __init__(self, base_name, iteration_spec, indexes=None):
        """
        Contructor of an output port iteration.
        """
        super().__init__(base_name, iteration_spec, indexes)

    # Representation of an output port iteration:
    def __repr__(self):
        """
        Representation of an output port iteration.
        """
        return 'OutputPortIteration : ' + self.base_name


# Definition of a channel value, which is an enrichment of the simple value
# borne by a port: just aside the value, inside a tuple, we join some metadata
# used by Sim-Diasca for checking that inter-block communications make sense.
# This tuple containing a value and its metadata is called a channel value.
#
class ChannelValue:
    """
    Class representing a Python alias of the kind of values that are expected
    in all channels of a dataflow by Sim-Diasca.
    """

    __slots__ = ('value', 'value_semantics', 'value_unit', 'value_type')

    def __init__(self, value, value_semantics: List[str], value_unit: str,
                 value_type: str):
        """
        Creates a new channel value from a basic value and some metadata: the
        semantics revealing what the value represents to humans, the physical
        unit with which it should be accompanied and its computational type.
        """

        self.value = value
        self.value_semantics = value_semantics
        self.value_unit = value_unit
        self.value_type = value_type

    def encode(self):
        """
        Encodes the metadata and zips the basic value with them in a 4-tuple.
        """

        encoded_semantics = [semstring.encode() for semstring
                             in self.value_semantics]

        return (self.value, encoded_semantics, self.value_unit.encode(),
                self.value_type.encode())

    def __repr__(self):
        """
        Textual representation of this channel value
        """
        return 'ChannelValue {} {} typed {} with semantics {}'.format(
            self.value, self.value_unit, self.value_type,
            self.value_semantics)

##
##
# --- DERIVED TYPES ---
##
##

PortName = str

BlockName = str
