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


-module(class_GaussianRepairModel).


-define( class_description,
		 "Class modelling the reparation behaviour of equipments according to "
		 "a gaussian (normal) law (probability density). "
		 "It is the most common repair model, most equipments respect this "
		 "statistical rule. "
		 "See also: class_UniformRepairModel." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_RepairModel ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Actor.StochasticActor.RepairModel.GaussianRepairModel" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").




% Constructs a new gaussian repair model actor.
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - MTTR is Mean time to repair, the mean (average) time that a device will take
% to recover from a non-terminal failure; it is measured thanks to a tuple
% {Days, Hours, Minutes, Seconds}
%
% - RepairStdDeviation is the standard deviation of the repair gaussian law
%
% Note: a random manager must be running beforehand.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_RepairModel:mttr(), math_utils:standard_deviation() ) ->
					   wooper:state().
construct( State, ActorSettings,
		  { MTTRday, MTTRhour, MTTRminute, MTTRsecond }, MTTRStdDeviation ) ->

	% First the direct mother classes:

	% Computing MTTR (expressed in seconds):
	MTTR = MTTRsecond + 60*( MTTRminute + 60*( MTTRhour + 24*MTTRday ) ),

	% Defines a gaussian repair profile, for stochastic class to manage it:
	RepairState = class_RepairModel:construct( State, ActorSettings,
		?trace_categorize("Gaussian repair model"),
		{ positive_integer_gaussian, MTTR, MTTRStdDeviation } ),

	% Then the class-specific actions:

	?send_info_fmt( RepairState, "Creating a new gaussian repair model "
		"whose MTTR is ~B and whose standard deviation is ~B.",
		[ MTTR, MTTRStdDeviation ] ),

	RepairState.


% Methods section.


% The actSpontaneous/1 method is inherited from class_RepairModel.
