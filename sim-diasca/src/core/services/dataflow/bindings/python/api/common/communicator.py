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


# Standard library:
from abc import ABC as AbstractBaseClass
from abc import abstractmethod


class BaseCommunicator(AbstractBaseClass):
    """Abstract class only defining the base pattern that any specialised
    communicator class should follow.
    """

    __slots__ = ()

    def __init__(self):
        """Empty constructor."""
        pass

    @abstractmethod
    def send(self, message):
        """Defines how to communicate outputs to the user of the binding, i.e
        the (Erlang) core of the engine.

        Must be overriden.
        """
        raise NotImplementedError("The send/2 method is not implemented.")

    @abstractmethod
    def filter(self, message):
        """Defines which treatments, specific to the user of the binding
        (i.e the (Erlang) core of the engine), are to be performed on incoming
        messages.

        Must be overriden.
        """
        raise NotImplementedError("The filter/2 method is not implemented.")

    @abstractmethod
    def decode_string(self, string):
        """Defines how to decode strings coming from the user of the binding,
        i.e the (Erlang) core of the engine.

        Must be overriden.
        """
        raise NotImplementedError("The decode_string/2 method is not "
                                  "implemented.")
