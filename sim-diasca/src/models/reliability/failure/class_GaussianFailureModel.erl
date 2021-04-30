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


-module(class_GaussianFailureModel).


-define( class_description,
		 "Class modeling the failure behaviour of equipments according to a "
		 "gaussian (normal) law (probability density). "
		 "It is a less common failure model than the exponential one. "
		 "See: class_ExponentialFailureModel." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_FailureModel ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Actor.StochasticActor.FailureModel.GaussianFailureModel" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Constructs a new gaussian failure model actor.
%
% MTTF is Mean Time To Failure, the mean (average) duration before a working
% system fails.
%
% All equipments using a given instance of this failure model will therefore
% have the same MTTF, but the shared random manager (thanks to the stochastic
% inheritance) will ensure each will act independently from the other
% equipments, according to the gaussian distribution.
%
% Note: a random manager must be running beforehand.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_FailureModel:mttf(), math_utils:standard_deviation() ) ->
					   wooper:state().
construct( State, ActorSettings,
		  _MTTF={ MTTFday, MTTFhour, MTTFminute, MTTFsecond }, MTTFStdDeviation ) ->

	% First the direct mother classes:

	% Computing MTTF (expressed in seconds):
	MTTF = MTTFsecond + 60 * ( MTTFminute + 60 * ( MTTFhour + 24*MTTFday ) ),

	% Defines an exponential failure profile, for stochastic class to manage it:
	FailureState = class_FailureModel:construct( State, ActorSettings,
		?trace_categorize("Gaussian failure model"),
		{ positive_integer_gaussian, MTTF, MTTFStdDeviation } ),

	?send_info_fmt( FailureState,
		"Creating a new gaussian failure model whose MTTF is ~B seconds "
		"and whose standard deviation is ~B.", [ MTTF, MTTFStdDeviation ] ),

	FailureState.




% Methods section.


% Management section of the actor.


% The actSpontaneous/1 method is inherited from class_FailureModel.
