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


% @doc <b>Base dataflow object</b>, defined for testing.
-module(class_BaseTestDataflowObject).


-define( class_description,
		 "Basic test dataflow object."
		 "No specific state of its own nor additional method." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowObject ] ).


% Helpers:
-export([ to_string/1 ]).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Unit-testing.BaseTestDataflowObject" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% For common information:
-include("dataflow_unit_test_defines.hrl").


-type object_name() :: ustring().


% Two dataflow attributes are declared by this object:
% - foo, of type integer
% - bar, of type string


% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Constructs a test dataflow object instance:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - DataflowPid is the PID of the dataflow instance
%
-spec construct( wooper:state(), class_Actor:actor_settings(), object_name(),
				 [ integer() | ustring() ], dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, ObjectName,
		   InitialAttributeValues=[ _FooAttributeValue, _BarAttributeValue ],
		   DataflowPid ) ->

	AttributeSpecs = get_dataflow_attribute_specs(),

	% First the direct mother class:
	class_DataflowObject:construct( State, ActorSettings,
		?trace_categorize(ObjectName), AttributeSpecs,
		InitialAttributeValues, _SpecForUniquePeers=[],
		_SpecForMultiplePeers=[], DataflowPid ).



% Static section.


% @doc Allows to fully specify the dataflow attributes of this object.
-spec get_dataflow_attribute_specs() ->
							static_return( [ dataflow_attribute_spec() ] ).
get_dataflow_attribute_specs() ->
	wooper:return_static( [

	 #dataflow_attribute_spec{
		attribute_name="foo",
		semantics=[ ?base_test_semantics, ?some_test_semantics ],
		unit="W",
		type_description="integer",
		constraints=[ positive ] },

	 #dataflow_attribute_spec{
		attribute_name="bar",
		semantics=[ ?other_test_semantics ],
		unit="dimensionless",
		type_description="float" } ] ).




% Helper section.


% @doc Returns a textual description of this household dataflow object.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->
	text_utils:format( "Basic test dataflow object named '~ts', having ~ts",
		[ ?getAttr(name),
		  class_DataflowObject:attributes_to_string( State ) ] ).
