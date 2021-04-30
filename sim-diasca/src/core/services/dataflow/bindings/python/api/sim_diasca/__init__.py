# Copyright (C) 2016-2016 EDF R&D

# This file is part of Sim-Diasca.

# Sim-Diasca is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.

# Sim-Diasca is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public
# License along with Sim-Diasca.
# If not, see <http://www.gnu.org/licenses/>.

# Author: Olivier Boudeville (olivier.boudeville@edf.fr)

# This file pertains to the Python binding of Sim-Diasca's dataflow support.

# The purpose of this file is to define a Sim-Diasca Dataflow Python package.


# Currently, workbench-related developments rely on a Python virtual
# environment, whereas the engine itself directly imports modules.

try:
    # If run directly from the engine:
    from processing_unit import *  # noqa: F401, F403

except ImportError:
    # If run from a Python virtual environment:
    from .processing_unit import *  # noqa: F401, F403
