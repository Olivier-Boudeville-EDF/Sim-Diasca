% Copyright (C) 2016-2021 EDF R&D

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

% Author: Robin Huart (robin-externe.huart@edf.fr)


% Module storing all the helper functions to facilitate the support of the
% binding APIs for the Java language.
%
-module(dataflow_java_binding_utils).


% For agent_pid() in next include:
-include("engine_common_defines.hrl").

% For types and names related to dataflows:
-include("dataflow_defines.hrl").


% Exports of helpers:
-export([ get_erlang_unit_type/0 ]).



% Returns the default Erlang-based unit type that is associated with any Java
% unit type.
%
-spec get_erlang_unit_type() -> dataflow_unit_type().
get_erlang_unit_type() ->
	class_DataflowJavaProcessingUnit.
