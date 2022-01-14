% Copyright (C) 2016-2022 EDF R&D

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


% @doc Base processing unit, defined for testing.
-module(class_BaseTestProcessingUnit).


-define( class_description,
		 "Basic test processing unit:~n"
		 " - ruled by the 'activate_on_new_set' policy~n"
		 " - having one input port (named 'my_input_port') and one output "
		 "port ('my_output_port'), both conveying integers~n"
		 " - having no specific state of its own nor additional method" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowProcessingUnit ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Unit-testing.BasicTestProcessingUnit" ).


% For types and shorthands:
-include("sim_diasca_for_actors.hrl").


-include("dataflow_unit_test_defines.hrl").


% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Constructs a test dataflow unit instance.
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - UnitName is a human-readable name for that unit instance (as a plain,
% non-empty string)
%
% - DataflowPid is the PID of the dataflow instance
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_DataflowProcessingUnit:unit_name(), dataflow_pid() ) ->
						wooper:state().
construct( State, ActorSettings, UnitName, DataflowPid ) ->

	% We start with two input iterations and two outputs:
	{ InputPortSpecs, OutputPortSpecs } = get_port_specifications(),

	% First the direct mother class:
	UnitState = class_DataflowProcessingUnit:construct(
		State, ActorSettings, ?trace_categorize(UnitName),
		_ActivationPolicy=activate_on_new_set, InputPortSpecs,
		OutputPortSpecs, DataflowPid ),

	% No class-specific actions:
	UnitState.




% Methods section.



% @doc Callback executed automatically whenever this unit gets activated.
%
% Meant to be overridden.
%
-spec activate( wooper:state() ) -> oneway_return().
activate( State ) ->

	% Expected by design to be set:
	InputRawValue = class_DataflowBlock:get_input_port_value( "my_input_port",
															  State ),

	NewRawValue = InputRawValue + 1,

	OutputChannelValue = class_Dataflow:create_channel_value( NewRawValue,
									[ ?base_test_semantics ], "W", "integer" ),

	SetState = class_DataflowBlock:set_output_port_value( "my_output_port",
												 OutputChannelValue, State ),

	?notice_fmt( "Activated! Current state: ~ts; read from input: ~p; "
		"written to output: ~ts.",
		[ to_string( State ), InputRawValue,
		  class_DataflowBlock:value_to_string( OutputChannelValue ) ] ),

	wooper:return_state( SetState ).




% Static section.


% @doc Returns the specifications for the input and output ports of that
% dataflow processing unit.
%
-spec get_port_specifications() ->
			static_return( { [ input_port_spec() ], [ output_port_spec() ] } ).
get_port_specifications() ->
	wooper:return_static( { get_input_port_specs(), get_output_port_specs() } ).



% @doc Returns a list of the specifications of the (initial) input ports for
% that dataflow block.
%
-spec get_input_port_specs() -> static_return( [ input_port_spec() ] ).
get_input_port_specs() ->

	MyInputPort = #input_port_spec{ name="my_input_port",
									value_semantics=[ ?base_test_semantics ],
									value_unit="W",
									value_type_description="integer" },

	wooper:return_static( [ MyInputPort ] ).



% @doc Returns a list of the specifications of the (initial) output ports for
% that unit.
%
-spec get_output_port_specs() -> static_return( [ output_port_spec() ] ).
get_output_port_specs() ->

	MyOutputPort = #output_port_spec{ name="my_output_port",
									  value_semantics=[ ?base_test_semantics ],
									  value_unit="W",
									  value_type_description="integer" },

	wooper:return_static( [ MyOutputPort ] ).



% Helper functions.


% @doc Returns a textual description of this unit.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->
	text_utils:format( "Basic test unit; this is a ~ts",
					   [ class_DataflowProcessingUnit:to_string( State ) ] ).
