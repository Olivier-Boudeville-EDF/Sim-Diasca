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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


-module(class_DataflowProcessingUnit).


-define( class_description,
		 "Dataflow processing unit class, corresponding to the implementation "
		 "of the computational parts of a dataflow. "
		 "This is a specialization of the generic dataflow block actor, meant "
		 "to be the mother class from which the actual processing units "
		 "inherit. "
		 "This class should provide most of the basics needed to properly "
		 "describe most processing units, including various built-in activation"
		 " policies. It may be subclassed if needed to introduce variants."
		 "Please refer to the 'Sim-Diasca Dataflow HOWTO' for further "
		 "information." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowBlock ] ).



% Each processing unit class ought to define following static methods:
%
% - either get_port_specifications/0 (preferable, if possible - i.e. if only
% initial ports are used)
%
% - otherwise dedicated, separate get_declared_semantics/0 and
% get_declared_types/0
%
% If none of these static methods is defined and exported, no blocking error
% will be raised, however no static information will be reported, and at the
% very least runtime checkings will be hindered.


% So, prefer defining (if only initial ports are used):

% -spec get_port_specifications() ->
%		static_return( { [ input_port_spec() ], [ output_port_spec() ] } ).

% otherwise:

% -spec get_declared_semantics() ->
%              static_return( class_SemanticServer:user_vocabulary() ).
%
% and
%
% -spec get_declared_types() ->
%              static_return( class_TypeServer:type_entries() ).



% Returns the semantics statically declared by this processing unit.
%
% Defining this method allows to ensure that all the ports ever created by this
% processing unit will rely on user-level semantics among this explicitly stated
% list.
%
% Otherwise the list would be deduced from the initial port specifications, with
% no specific control.
%
% -spec get_declared_semantics() ->
%     static_return( class_SemanticServer:user_vocabulary() ).
% get_declared_semantics() ->
%	 wooper:return_static( [ "an example" ] ).


% Returns the types statically declared by this processing unit.
%-spec get_declared_types() -> static_return( class_TypeServer:type_entries() ).
%get_declared_types() ->
%	wooper:return_static( [ { 'my_type', "'something'|'other thing'" } ] ).


% For the input_port record and all:
-include("class_DataflowBlock_defines.hrl").



% Name of an instance of processing unit:
-type unit_name() :: string().



% A processing unit activation corresponds to the execution of its (probably
% overridden) activate/1 oneway.


% About processing unit activation policies:
%
% - activate_on_new_set: the processing unit will be activated at most once per
% diasca, at the one immediately following the diasca at which at least one of
% its input ports was triggered; it is up to the processing unit to reset the
% input ports (i.e. to set them back to the unset state) when deemed appropriate
%
% - activate_when_all_set: the processing unit is activated if and only if all
% of its input ports are set; after an activation, all input ports used to be
% automatically reset to the 'unset' state, however having lingering values
% proved more convenient at least in some use cases (were stable inputs were not
% to be refreshed), hence no automatic reset is performed anymore
%
% - custom_activation: the activation of the processing unit is managed by the
% unit itself (not expected to be a common case)
%
-type activation_policy() :: 'activate_on_new_set'
						   | 'activate_when_all_set' | 'custom_activation'.


-export_type([ unit_name/0, activation_policy/0 ]).



% Processing units may define following static method:
%
% -spec get_declared_types() ->
%         static_return( [ class_TypeServer:type_entry() ] ).
%
% in order that their manager(s) are able to obtain from them the specific types
% that they may define and use.


% Helpers:
-export([ check_policy/1, unset_all_input_ports/1, to_string/1 ]).


% The attributes that are specific to a processing unit are:
-define( class_attributes, [

	{ activation_policy, activation_policy(), "policy applied to decide "
	  "when this processing unit should be activated" },

	{ activation_requested, boolean(), "tells whether an activation has "
	  "already been requested this diasca (allows to activate an unit ruled "
	  "by the activate_on_new_set policy only once, even if multiple of its "
	  "input ports have been set at the same diasca)" } ] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.ProcessingUnit" ).


% For app_error_fmt/2:
-include_lib("traces/include/traces_app_header.hrl").


% For port_timestamp(), value_status() and all:
-include("dataflow_defines.hrl").


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").


% Shorthands:
-type ustring() :: text_utils:ustring().



% Constructs a new dataflow processing unit:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as automatically assigned by the load balancer
%
% - ProcessingUnitName is a human-readable name for that processing unit (as a
% plain, non-empty string)
%
% - ActivationPolicy is the policy driving the activations of this processing
% unit
%
% - InputPortSpecs is a list of the specifications of the input ports defined
% for this processing unit
%
% - OutputPortSpecs is a list of the specifications of the output ports defined
% for this processing unit
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 unit_name(), activation_policy(), [ input_port_spec() ],
				 [ output_port_spec() ], dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, ProcessingUnitName, ActivationPolicy,
		   InputPortSpecs, OutputPortSpecs, DataflowPid ) ->

	% First the direct mother class:
	BlockState = class_DataflowBlock:construct( State, ActorSettings,
			?trace_categorize(ProcessingUnitName),
			InputPortSpecs, OutputPortSpecs, DataflowPid ),

	ActualPolicy = check_policy( ActivationPolicy ),

	% Then the class-specific actions:
	setAttributes( BlockState, [ { activation_policy, ActualPolicy },
								 { activation_requested, false } ] ).




% Methods section.


% Callback executed on the first diasca of existence of this processing unit.
%
% Note: should this method be overridden in a child class, this version should
% be called from there as well (as must be called in all cases).
%
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?debug_fmt( "Created a ~ts", [ to_string( State ) ] ),

	DataflowPid = ?getAttr(dataflow_pid),

	% As this class may have been specialised:
	ActualClassname = wooper:get_classname( State ),

	SentState = class_Actor:send_actor_message( DataflowPid,
		{ registerDataflowUnit, [ ActualClassname ] }, State ),

	actor:return_state( SentState ).



% Sets explicitly the specified input port to the specified fully-specified
% channel value.
%
% Note: calling this method bypasses the (channel-based) dataflow system; it is
% mostly useful in order to feed source units from outside of the dataflow
% (typically from the experiment entry point). Such an explicit setting will
% perform activations exactly like a standard setting.
%
% Counterpart of the setAttributeValue/4 of class_DataflowObject.
%
-spec setInputPortValue( wooper:state(),
			input_port_name() | input_port_string_name(), channel_value(),
			sending_actor_pid() ) -> actor_oneway_return().
% Sending binaries shall be preferred (more efficient):
setInputPortValue( State, InputPortName, ChannelValue, SendingActorPid )
  when is_binary( InputPortName )
	   andalso is_record( ChannelValue, channel_value ) ->

	?debug_fmt( "Explicit setting of input port '~ts' to the value '~ts' "
		"for this processing unit, as requested by process ~w.",
		[ InputPortName, class_DataflowBlock:value_to_string( ChannelValue ),
		  SendingActorPid ] ),

	% Values conveyed by channels obey by design some rules - here we have to
	% validate them from scratch as they are introduced with no control:
	%
	% (only lightweight checking done currently)

	InputPortTable = ?getAttr(input_ports),

	InputPort = class_DataflowBlock:get_input_port( InputPortName,
													InputPortTable, State ),

	class_DataflowBlock:validate_value_for_input_port( ChannelValue, InputPort,
													   InputPortName, State ),

	NewInputPort = class_DataflowBlock:assign_input_value( ChannelValue,
											InputPort, InputPortName, State ),

	NewInputPortTable =
		table:update_entry( InputPortName, NewInputPort, InputPortTable ),

	SetState = setAttribute( State, input_ports, NewInputPortTable ),

	ScheduledState = consider_activation_after_input( SetState ),

	actor:return_state( ScheduledState );


setInputPortValue( State, InputPortName, ChannelValue, SendingActorPid )
  when is_list( InputPortName ) ->

	NewState = setInputPortValue( State,
		text_utils:string_to_binary( InputPortName ), ChannelValue,
		SendingActorPid ),

	actor:return_state( NewState );


setInputPortValue( _State, Unexpected, _ChannelValue, _SendingActorPid ) ->
	% Probably an atom here:
	throw( { invalid_type_for_port_name, Unexpected } ).




% Notifies this dataflow unit that, for the specified input port, one of its
% upstream blocks just emitted a new (channel) value, possibly resulting in an
% activation being triggered.
%
% Note: an immediate value (with no specific metadata) could have sufficed, as
% they are supposed to have been checked at channel creation.
%
-spec notifyNewInput( wooper:state(), input_port_name(), channel_value(),
					  block_pid() ) -> actor_oneway_return().
notifyNewInput( State, InputPortName, ChannelValue, UpstreamBlockPid )
  when is_binary( InputPortName )
	   andalso is_record( ChannelValue, channel_value ) ->

	?debug_fmt( "Dataflow-based setting of input port '~ts' to the value '~ts' "
		"for this unit, as requested by upstream block ~w.",
		[ InputPortName, class_DataflowBlock:value_to_string( ChannelValue ),
		  UpstreamBlockPid ] ),

	% Values conveyed by channels obey by design some rules - here we validate
	% them for an increased safety:
	%
	% (only lightweight checking done currently)

	InputPortTable = ?getAttr(input_ports),

	InputPort = class_DataflowBlock:get_input_port( InputPortName,
													InputPortTable, State ),

	class_DataflowBlock:validate_value_for_input_port( ChannelValue, InputPort,
													   InputPortName, State ),

	NewInputPort = class_DataflowBlock:assign_input_value( ChannelValue,
											InputPort, InputPortName, State ),

	NewInputPortTable = table:update_entry( InputPortName, NewInputPort,
											InputPortTable ),

	SetState = setAttribute( State, input_ports, NewInputPortTable ),

	ScheduledState = consider_activation_after_input( SetState ),

	actor:return_state( ScheduledState );


notifyNewInput( _State, InputPortName, _ChannelValue, _UpstreamBlockPid )
  when is_list( InputPortName ) ->
	throw( { non_binary_port_name, InputPortName } );


notifyNewInput( _State, InputPortName, _ChannelValue, _UpstreamBlockPid ) ->
	% Probably an atom here:
	throw( { invalid_type_for_port_name, InputPortName } ).



% Considers whether the activation criteria are met for this processing unit,
% knowing that an input port has just been set.
%
% (helper)
%
-spec consider_activation_after_input( wooper:state() ) -> wooper:state().
consider_activation_after_input( State ) ->

	ActivationPolicy = ?getAttr(activation_policy),

	IsActivated = case ActivationPolicy of

		activate_on_new_set ->
			true;

		activate_when_all_set ->
			are_all_input_ports_set( State );

		custom_activation ->
			%is_custom_activation_triggered( State )
			throw( not_implemented_yet )

	end,

	case IsActivated of

		true ->
			case ?getAttr(activation_requested) of

				true ->
					?info_fmt( "An input port was set that would have led to "
						"an activation of this processing unit (ruled "
						"by the ~w policy), should it be not already "
						"planned to be activated.", [ ActivationPolicy ] ),
					State;

				false ->
					?info_fmt( "An input port was set, and this led to an "
						"activation of this processing unit "
						"(ruled by the ~w policy).", [ ActivationPolicy ] ),

					ActivatedState = class_Actor:send_actor_message( self(),
													triggerActivation, State ),

					setAttribute( ActivatedState, activation_requested, true )

			end;

		false ->
			?debug_fmt( "An input port was set, yet this did not lead to an "
				"activation of this processing unit "
				"(ruled by the ~w policy), as ~ts",
				[ ActivationPolicy, list_unset_input_ports( State ) ] ),
			State

	end.



% Tells whether all input ports are set.
%
% Note: should there be no input port, this property (actually telling whether
% none is unset) is thus considered true.
%
-spec are_all_input_ports_set( wooper:state() ) -> boolean().
are_all_input_ports_set( State ) ->
	InputPorts = table:values( ?getAttr(input_ports) ),
	are_all_set( InputPorts ).



% Lists the input ports that are not (yet) set.
%
% (helper)
%
-spec list_unset_input_ports( wooper:state() ) -> ustring().
list_unset_input_ports( State ) ->

	% In all the (actual, i.e. standard or iterated) input ports, filters the
	% ports that are not set, and keep their names:

	InputTable = ?getAttr(input_ports),

	UnsetPortNames = [ text_utils:binary_to_string( PortName )
		|| { PortName, #input_port{ value_status=unset } }
			   <- table:enumerate( InputTable ) ],

	text_utils:format(
	  "the following ~B input ports (over ~B) are not set: ~ts",
	  [ length( UnsetPortNames ), table:size( InputTable ),
		text_utils:strings_to_sorted_string( UnsetPortNames ) ] ).



% Tells whether all specified input ports are set.
-spec are_all_set( [ input_port() ] ) -> boolean().
are_all_set( _InputPorts=[] ) ->
	% As a result, a unit with no input port could be activated, if not called
	% relevantly (i.e. from consider_activation_after_input/1):
	%
	true;

are_all_set( _InputPorts=[ #input_port{ value_status=unset } | _T ] ) ->
	false;

% A bit of extraneous checking is welcome:
are_all_set( _InputPorts=[ #input_port{ value_status={ set, _V } } | T ] ) ->
	are_all_set( T ).



% Unsets all the input ports of this processing unit.
-spec unset_all_input_ports( wooper:state() ) -> wooper:state().
unset_all_input_ports( State ) ->

	InputPorts = table:enumerate( ?getAttr(input_ports) ),

	ResetInputPorts = reset_ports( InputPorts, _Acc=[] ),

	NewInputPortTable = table:new( ResetInputPorts ),

	setAttribute( State, input_ports, NewInputPortTable ).



% Returns a reset version of the specified list of pairs.
reset_ports( _InputPorts=[], Acc ) ->
	Acc;

reset_ports( _InputPorts=[ { PortName, Port } | T ], Acc ) ->

	NewPort = Port#input_port{ value_status=unset },

	reset_ports( T, [ { PortName, NewPort } | Acc ] ).



% Delayed activation, so that by design all input ports that may have been set
% during the previous diasca have already been recorded: then (at the following
% diasca) this unit is activated only once, regardless of the number of the
% previous input port assignments.
%
% (self-triggered actor oneway)
%
-spec triggerActivation( wooper:state(), sending_actor_pid() ) ->
								actor_oneway_return().
triggerActivation( State, _SelfSendingActorPid ) ->

	PreActOutputPorts = class_DataflowBlock:get_output_entries( State ),

	?notice( "Activation of this processing unit triggered now." ),

	ActivatedState = executeOneway( State, activate ),

	PostActOutputPorts =
		class_DataflowBlock:get_output_entries( ActivatedState ),

	ChangeString = case list_utils:difference( PostActOutputPorts,
											   PreActOutputPorts ) of

		[] ->
			"no change in output ports";

		ChangedOutputPorts ->
			Strings = [ text_utils:format( "'~ts' has just been set to ~p",
										   [ OPName, V ] )
						|| { OPName, V } <- ChangedOutputPorts ],

			text_utils:format( "~B newly set output ports: ~ts",
							   [ length( ChangedOutputPorts ),
								 text_utils:strings_to_string( Strings ) ] )

	end,

	?notice_fmt( "Activation is over, with ~ts", [ ChangeString ] ),

	PortResetState = case ?getAttr(activation_policy) of

		activate_when_all_set ->
			% Maybe not resetting the input ports is more convenient after all:
			ActivatedState;
			%unset_all_input_ports( ActivatedState );

		_ ->
			ActivatedState

	end,

	ActResetState = setAttribute( PortResetState, activation_requested, false ),

	actor:return_state( ActResetState ).



% Callback executed automatically whenever the processing unit is activated.
%
% Meant to be overridden.
%
% (oneway)
%
-spec activate( wooper:state() ) -> const_oneway_return().
activate( State ) ->

	?warning_fmt( "Default, do-nothing activation triggered for ~ts.",
				  to_string( State ) ),

	wooper:const_return().



% Triggers the destruction of this processing unit.
%
% Typically called from its unit manager when having to destruct a unit after
% being notified that an associated dataflow object has been destructed.
%
-spec triggerDestruction( wooper:state(), class_DataflowUnitManager:action_id(),
						  sending_actor_pid() ) -> actor_oneway_return().
triggerDestruction( State, ActionId, SendingActorPid ) ->

	?debug_fmt( "Destruction triggered by ~w, in the context of action #~B.",
				[ SendingActorPid, ActionId ] ),

	% Regardless of upstream or downstream:
	ConnectedBlocks = set_utils:to_list(
			class_DataflowBlock:get_directly_connected_blocks( State ) ),

	ConnectState = class_Actor:send_actor_messages( ConnectedBlocks,
									_Oneway=disconnectFromBlock, State ),

	ActualClassname = wooper:get_classname( ConnectState ),

	UnregisterState = class_Actor:send_actor_message( ?getAttr(dataflow_pid),
		{ unregisterDataflowUnit, [ ActualClassname ] }, ConnectState ),

	DestructState = class_Actor:send_actor_message( SendingActorPid,
		{ onUnitDestructed, [ ActionId, ActualClassname ] }, UnregisterState ),

	DeclaredState = executeOneway( DestructState, declareTermination ),

	EmptyPortTable = table:new(),

	FinalState = setAttributes( DeclaredState, [
					{ input_ports, EmptyPortTable },
					{ output_ports, EmptyPortTable },
					{ run_status, terminating } ] ),

	actor:return_state( FinalState ).





% Helper functions.


% Checks the processing unit activation policy provided by the user.
-spec check_policy( basic_utils:user_data() ) -> activation_policy().
check_policy( P=activate_on_new_set ) ->
	P;

check_policy( P=activate_when_all_set ) ->
	P;

check_policy( P=custom_activation ) ->
	P;

check_policy( P ) when is_atom( P ) ->
	?app_error_fmt( "Unknown activation policy encountered: ~p (unsupported).",
					[ P ] ),
	throw( { unsupported_activation_policy, P } );

check_policy( P ) ->
	?app_error_fmt( "Invalid type of activation policy definition: ~p.",
					[ P ] ),
	throw( { invalid_activation_policy_type, P } ).



% Returns a textual description of this processing unit.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	{ InputDetailed, OutputDetailed } =
		class_DataflowBlock:io_to_string( State ),

	text_utils:format( "processing unit named '~ts', being ~ts, "
		"applying the ~ts activation policy, having ~ts~nand ~ts",
		[ ?getAttr(name), ?getAttr(run_status), ?getAttr(activation_policy),
		  InputDetailed, OutputDetailed ] ).
