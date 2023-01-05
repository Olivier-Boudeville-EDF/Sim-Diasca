% Copyright (C) 2018-2023 EDF R&D

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


% Actor-specific type shorthands (to avoid having to prefix them with a module
% name):

-type actor_pid() :: class_Actor:actor_pid().
% PID of an actor.

-type sending_actor_pid() :: class_Actor:sending_actor_pid().
% PID of an actor having sent a message to the current actor.

-type created_actor_pid() :: class_Actor:created_actor_pid().
% PID of a just created actor.



% Possible result types for the specs of actor oneways:

-type actor_oneway_return() :: class_Actor:actor_oneway_return().
% A type designating the (non-const) returns of actor oneways.

-type const_actor_oneway_return() :: class_Actor:const_actor_oneway_return().
% A type designating the const returns of actor oneways.


-export_type([ actor_pid/0, sending_actor_pid/0, created_actor_pid/0,
			   actor_oneway_return/0, const_actor_oneway_return/0 ]).
