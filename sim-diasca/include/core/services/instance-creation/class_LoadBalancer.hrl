% Copyright (C) 2008-2021 EDF R&D

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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)



% The name under which the load balancer process is to be registered (globally):
% (this information must be shared with its clients)
%
-define( load_balancer_name, sim_diasca_load_balancer ).


% (load_balancer_pid() defined in engine_common_defines.hrl)


% Describes how the load balancing of actors should be performed.
-record( load_balancing_settings, {

	% Determines how actors should be placed onto computing resources.
	%
	% The 'round_robin' policy creates actors on each computing node in turn.
	%
	% All placement policies are bypassed when a placement hint is specified for
	% an actor creation: this actor is then created on the computing node that
	% is determined from the placement hint.
	%
	% The 'select_least_loaded_first' policy will attempt to create actors on
	% the computing node that is least loaded, i.e. on the host that has the
	% least (number of actors) / (processing power) ratio. This policy is not
	% implemented yet.
	%
	placement_policy = round_robin :: class_LoadBalancer:placement_policy()

} ).


% For convenience:
-type load_balancing_settings() :: #load_balancing_settings{}.



% Describes the actor settings, as determined by the load balancer, at actor
% creation.
%
-record( actor_settings, {

		   % The AAI of this newer actor:
		   aai :: class_Actor:aai(),

		   % The seed of this actor:
		   seed :: random_utils:seed(),

		   % How it is to reorder its incoming messages:
		   message_ordering_mode :: class_Actor:message_ordering_mode()

} ).
