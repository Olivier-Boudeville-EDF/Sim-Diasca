% Copyright (C) 2016-2023 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]


-include("dataflow_defines.hrl").

-include("bindings.hrl").


% The base, default semantics for test:
-define( base_test_semantics, "http://foobar.org/test/base" ).


-define( some_test_semantics, "http://foobar.org/test/some" ).

-define( other_test_semantics, "http://foobar.org/test/other" ).


-type test_dataflow_object_pid() :: class_Actor:actor_pid().
-type test_processing_unit_pid() :: class_Actor:actor_pid().
