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


class InterpreterState:
    """Container class (never instantiated) keeping track of the whole state of
    its interpreter, notably in terms of instantiated objects.
    """

    # Unique communicator, in charge of performing the actual communications
    # with the (Erlang) core:
    #
    global_communicator = None

    # Unique message handler object defining the actions corresponding to all
    # messages possibly received:
    #
    global_message_handler = None

    # Unique dictionary of all the processing units instantiated and persisting
    # in the current interpreter, along with their creation counter:
    units_creation_counter = 0
    processing_units = {}

    # Unique dictionary of all the unit managers instantiated and persisting in
    # the current interpreter, along with their creation counter:
    managers_creation_counter = 0
    unit_managers = {}
