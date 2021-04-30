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


-module(class_UniformRepairModel).


-define( class_description,
		 "Class modeling the reparation behaviour of equipments according "
		 "to a uniform law (probability density)."
		 "It is a less common repair model than the gaussian one."
		 "See: class_GaussianRepairModel." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_RepairModel ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Actor.StochasticActor.RepairModel.UniformRepairModel" ).



% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% Constructs a new repair model actor.
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - MTTR is Mean Time To Repair, the mean (average) time that a device will take
% to recover from a non-terminal failure
%
% The MaxTTR could be taken here as a workload: with one resource set to repair
% the equipment, it evaluates the time to repair.
%
% Affecting more resources leads to decreased repair durations. So it is a
% measure of the failure gravity in itself, not depending on the repair
% resources.
%
% Note: a random manager must be running beforehand.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_RepairModel:mttr() ) -> wooper:state().
construct( State, ActorSettings,
		   _MTTR={ MaxTTRday, MaxTTRhour, MaxTTRminute, MaxTTRsecond } ) ->

	% First the direct mother classes:

	% Computing maximum TTR (expressed in seconds):
	MaxTTR = MaxTTRsecond + 60*( MaxTTRminute + 60*(MaxTTRhour+24*MaxTTRday) ),

	% Defines an uniform repair profile, for stochastic class to manage it:
	RepairState = class_RepairModel:construct( State, ActorSettings,
		?trace_categorize("Uniform repair model"), { uniform, MaxTTR } ),

	?send_info_fmt( RepairState,
		"Creating a new uniform repair model "
		"whose maximum time-to-repair is ~B seconds.", [ MaxTTR ] ),

	RepairState.



% Methods section.


% The actSpontaneous/1 method is inherited from class_RepairModel.
