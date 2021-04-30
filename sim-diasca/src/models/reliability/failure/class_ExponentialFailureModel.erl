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


-module(class_ExponentialFailureModel).


-define( class_description,
		 "Class modeling the failure behaviour of equipments according to an "
		 "exponential law (probability density)."
		 "Note: it is the most common failure model, most electronic "
		 "equipments respect this statistical rule."
		 "See also: class_GaussianFailureModel." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_FailureModel ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Actor.StochasticActor.FailureModel.ExponentialFailureModel" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Constructs a new exponential failure model actor.
%
% MTTF is Mean Time To Failure, the mean (average) time before a working system
% fails.
%
% It is measured thanks to a tuple {days,hours,minutes,seconds}.
%
% All equipments using a given instance of this failure model will therefore
% have the same MTTF, but the shared random manager (thanks to the stochastic
% inheritance) will ensure each will act independently from the other
% equipments, according to the exponential distribution.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_FailureModel:mttf() ) -> wooper:state().
construct( State, ActorSettings, MTTF ) ->

	% First the direct mother classes:

	% Computing MTTF (expressed in seconds):
	MTTFInSec = time_utils:dhms_to_seconds( MTTF ),

	% The mean value of drawn samples is 1/Lambda:
	% (it is a floating-point value)

	Lambda = 1 / MTTFInSec,

	% Defines an exponential failure profile, for stochastic class to manage it:
	FailureState = class_FailureModel:construct( State, ActorSettings,
		?trace_categorize( "Exponential failure model" ),
		{ exponential, Lambda } ),

	% Then the class-specific actions:

	?send_info_fmt( FailureState,
		"Creating a new exponential failure model whose MTTF is ~B seconds "
		"(lambda is ~f).", [ MTTFInSec, Lambda ] ),

	FailureState.



% Methods section.


% The actSpontaneous/1 method is inherited from class_FailureModel.
