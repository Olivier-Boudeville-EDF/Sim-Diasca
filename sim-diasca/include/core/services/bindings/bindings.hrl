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


% A few type shorthands:


% Type of a language:
-type language() :: language_utils:language().


% PID of a runtime manager:
-type binding_manager_pid() :: class_LanguageBindingManager:manager_pid().


% PID of the Python runtime manager:
-type python_binding_manager_pid() :: class_PythonBindingManager:manager_pid().


% PID of the Java runtime manager:
-type java_binding_manager_pid() :: class_JavaBindingManager:manager_pid().



% Record gathering all the binding managers of the various languages involved.
%
-record( binding_managers, {

		   % For Python:
		   python_binding_manager = none ::
			 'none' | python_binding_manager_pid(),

		   % For Java:
		   java_binding_manager = none :: 'none' | java_binding_manager_pid()

} ).

-type binding_managers() :: #binding_managers{}.
