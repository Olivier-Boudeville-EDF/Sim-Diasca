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



% Main entry point for the dataflow services that are typically exposed to the
% simulation case.
%
% Please refer to the 'Sim-Diasca Dataflow HOWTO' for further information.
%
-module(dataflow_support).


% Basics:
-export([ start/0, start/1, declare_vocabulary/2,
		  declare_type/3, declare_types/2, stop/0 ]).


% String-related helpers:
-export([ comment_to_string/1, user_semantics_to_string/1,
		  semantics_to_string/1, semantics_to_string/2,
		  value_unit_to_string/1, string_unit_to_string/1,
		  value_type_to_string/1,
		  value_constraint_to_string/1, value_constraint_to_string/2,
		  value_status_to_string/1, multiplicity_to_string/1,
		  type_description_to_string/1, port_id_to_string/1,
		  changeset_to_string/1, changeset_to_string/2,
		  world_event_to_string/1, world_event_to_string/2 ]).


% Other helpers:
-export([ find_event_by_id/2, get_event_id/1, get_port_id/2 ]).



% Options supported by dataflow system:
-type option() :: atom().


-export_type([ option/0 ]).


% For agent_pid():
-include("engine_common_defines.hrl").


% For dataflow_version and all:
-include("dataflow_defines.hrl").


-define( emitter_categorization, "Core.Dataflow" ).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type indentation_level() :: text_utils:indentation_level().



% Starts the overall dataflow support.
%
% Note: the engine is expected to be already initialised.
%
-spec start() -> dataflow_state().
start() ->
	start( _Opts=[] ).



% Starts the overall dataflow support with specified options.
%
% Note: the engine is expected to be already initialised.
%
-spec start( [ option() ] ) -> dataflow_state().
start( Options ) ->

	% Checking that the engine is most probably launched:
	case sim_diasca:is_running() of

		true ->
			ok;

		false ->
			throw( { dataflow_cannot_be_started, engine_not_running } )

	end,

	OptionString = case Options of

		[] ->
			"no option specified";

		_ ->
			OptionStrings = [ text_utils:format( "~p", [ Opt ] )
							  || Opt <- Options ],

			text_utils:format( "following ~B options: ~ts", [ length( Options ),
				text_utils:strings_to_string( OptionStrings ) ] )

	end,

	class_TraceEmitter:send_standalone( info, "Starting dataflow support "
		"version " ++ ?dataflow_version ++ ", with " ++ OptionString,
		_EmitterName="Dataflow", ?emitter_categorization ),

	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	WorldManagerPid = class_Actor:create_initial_actor( class_WorldManager, [],
														LoadBalancerPid ),

	% Now that the engine and dataflow support are initialised, let's create the
	% initial instances.

	ExperimentManagerPid = class_Actor:create_initial_actor(
		class_ExperimentManager, [ WorldManagerPid ], LoadBalancerPid ),

	IdentificationServerPid = class_IdentificationServer:start(),

	SemanticServerPid =
		class_SemanticServer:start( ?min_lexicographic_distance ),

	TypeServerPid = class_TypeServer:start(),

	#dataflow_state{ world_manager_pid=WorldManagerPid,
					 experiment_manager_pid=ExperimentManagerPid,
					 identification_manager_pid=IdentificationServerPid,
					 semantic_server_pid=SemanticServerPid,
					 type_server_pid=TypeServerPid }.



% Declares directly, asynchronously, a user-level vocabulary, i.e. a list of
% semantic elements (as plain strings).
%
% Typically meant to be called directly from the simulation case.
%
-spec declare_vocabulary( user_vocabulary(), dataflow_state() ) -> void().
declare_vocabulary( Vocabulary,
			#dataflow_state{ semantic_server_pid=SemanticServerPid } ) ->
	class_SemanticServer:declare_vocabulary( Vocabulary,  SemanticServerPid ).



% Declares directly, asynchronously, a type.
%
% Typically meant to be called directly from the simulation case.
%
-spec declare_type( class_TypeServer:type_name(),
			class_TypeServer:type_definition(), dataflow_state() ) -> void().
declare_type( TypeName, TypeDefinition,
			  #dataflow_state{ type_server_pid=TypeServerPid } ) ->
	TypeServerPid ! { declareType, [ TypeName, TypeDefinition ] }.



% Declares directly, asynchronously, a set of types.
%
% Typically meant to be called directly from the simulation case.
%
-spec declare_types( class_TypeServer:type_entries(), dataflow_state() ) ->
							void().
declare_types( TypeEntries,
			   #dataflow_state{ type_server_pid=TypeServerPid } ) ->
	TypeServerPid ! { declareTypes, [ TypeEntries ] }.



% Stops the dataflow support.
-spec stop() -> void().
stop() ->
	class_TypeServer:stop(),
	class_SemanticServer:stop(),
	class_TraceEmitter:send_standalone( info, "Dataflow support stopped.",
										?emitter_categorization ).



% Helper section.


% Section for the textual description of second-order elements (comments, types,
% etc.).


% Returns a textual description of the specified port comment.
-spec comment_to_string( internal_comment() ) -> ustring().
comment_to_string( _Comment=undefined ) ->
	"uncommented";

comment_to_string( Comment ) ->
	text_utils:format( "commented as '~ts'", [ Comment ] ).



% Returns a textual description of the specified user-specified port semantics.
-spec user_semantics_to_string( maybe( value_semantics() ) ) -> ustring().
user_semantics_to_string( _Semantics=undefined ) ->
	"with no semantics specified";

user_semantics_to_string( SemanticList ) ->

	case length( SemanticList ) of

		L when L > 2 ->
			SemStrings = [ text_utils:format( "'~ts'", [ S ] )
						   || S <- SemanticList ],

			text_utils:format( "relying on following ~B semantics: ~ts",
						 [ L, text_utils:strings_to_string( SemStrings ) ] );

		_ ->
			text_utils:format( "relying on the '~p' semantics",
							   [ SemanticList ] )

	end.



% Returns a textual description of the specified port semantics.
-spec semantics_to_string( maybe( value_semantics() ) ) -> ustring().
semantics_to_string( Semantics ) ->
	semantics_to_string( Semantics, _IndentationLevel=0 ).



% Returns a textual description of the specified port semantics.
-spec semantics_to_string( maybe( value_semantics() ),
						   indentation_level()  ) -> ustring().
semantics_to_string( _Semantics=undefined, _IndentationLevel ) ->
	"with no semantics specified";

semantics_to_string( Semantics, IndentationLevel ) ->

	SemanticList = set_utils:to_list( Semantics ),

	% Semantic elements are binaries:
	case length( SemanticList ) of

		L when L > 2 ->
			SemStrings = [ text_utils:format( "'~ts'", [ S ] )
						   || S <- SemanticList ],

			text_utils:format( "relying on following ~B semantics: ~ts",
						 [ L, text_utils:strings_to_string( SemStrings,
													IndentationLevel ) ] );

		_ ->
			SemStrings = [ text_utils:format( "~ts", [ S ] )
						   || S <- SemanticList ],
			text_utils:format( "relying on the '~p' semantics", [ SemStrings ] )

	end.



% Returns a textual description of the specified value unit.
-spec value_unit_to_string( value_unit() ) -> ustring().
value_unit_to_string( _ValueUnit={ UnitBinString, CanonicalUnit } ) ->

	CanonicalUnitString = unit_utils:unit_to_string( CanonicalUnit ),

	text_utils:format( "of unit '~ts' (as specified by the user), "
		"translated as '~ts'", [ UnitBinString, CanonicalUnitString ] ).



% Returns a textual description of the specified string unit.
-spec string_unit_to_string( ustring() ) -> ustring().
string_unit_to_string( UnitString ) ->
	text_utils:format( "of unit '~ts' (as specified by the user)",
					   [ UnitString ] ).



% Returns a textual description of the specified value type.
-spec value_type_to_string( value_type() ) -> ustring().
value_type_to_string( _Type=undefined ) ->
	"untyped values";

value_type_to_string( Type ) ->
	TextualType = type_utils:type_to_description( Type ),
	text_utils:format( "values of type ~ts", [ TextualType ] ).



% Returns a textual description of the specified value constraints.
-spec value_constraint_to_string( value_constraints() ) -> ustring().
value_constraint_to_string( Constraints ) ->
	value_constraint_to_string( Constraints, _IndentationLevel=0 ).


% Returns a textual description of the specified value constraints.
-spec value_constraint_to_string( value_constraints(),
								  indentation_level() ) -> ustring().
value_constraint_to_string( _Constraints=[], _IndentationLevel ) ->
	"with no constraint enforced";

value_constraint_to_string( _Constraints=[ Constraint ], _IndentationLevel ) ->
	text_utils:format( "with a single constraint enforced: ~p",
					   [ Constraint ] );

value_constraint_to_string( Constraints, IndentationLevel ) ->

	ConstDescs = [ text_utils:format( "~p", [ C ] ) || C <- Constraints ],

	Bullet = text_utils:get_bullet_for_level( IndentationLevel + 1 ),

	ConstBulletList = text_utils:strings_to_string( ConstDescs, Bullet ),

	text_utils:format( "with ~B constraints enforced: ~ts",
					   [ length( Constraints ), ConstBulletList ] ).



% Returns a textual description of the specified value constraints.
-spec value_status_to_string( value_status() ) -> ustring().
value_status_to_string( _Status=unset ) ->
	"not set";

value_status_to_string( _Status={ set, Value } ) ->
	text_utils:format( "set to value '~p'", [ Value ] ).



% Returns a textual description of the specified value constraints.
-spec multiplicity_to_string( iteration_multiplicity() ) -> ustring().
multiplicity_to_string( { Current, { Min, Max } } ) ->
	% Max can be 'unbounded':
	text_utils:format( "currently having ~B iterated ports "
					   "(min: ~B, max: ~p)", [ Current, Min, Max ] ).



% Returns a textual description of the specified type description.
-spec type_description_to_string( value_type_description() ) -> ustring().
type_description_to_string( TypeDescription ) ->
	text_utils:format( "type described as '~ts'", [ TypeDescription ] ).



% Returns a textual description of the specified port identifier.
-spec port_id_to_string( port_id() ) -> ustring().
port_id_to_string( _PortIdentifier={ BlockPid, PortBinName } ) ->
	text_utils:format( "~w:'~ts'", [ BlockPid, PortBinName ] ).



% Returns a textual description of the specified changeset.
-spec changeset_to_string( changeset(), boolean() ) -> ustring().
changeset_to_string( WorldEvents, _IsVerbose=false ) ->
	text_utils:format( "changeset comprising ~B world events",
					   [ length( WorldEvents ) ] );

changeset_to_string( WorldEvents, _IsVerbose=true ) ->
	changeset_to_string( WorldEvents ).



% Returns a textual description of the specified changeset.
-spec changeset_to_string( changeset() ) -> ustring().
changeset_to_string( _WorldEvents=[] ) ->
	"empty changeset";

changeset_to_string( _WorldEvents=[ WorldEvent ] ) ->
	text_utils:format( "changeset comprising a single world event: ~ts",
			   [ world_event_to_string( WorldEvent, _IsVerbose=false ) ] );

changeset_to_string( WorldEvents ) ->

	Strings = [ world_event_to_string( W, _IsVerbose=true, _IndentationLevel=1 )
				|| W <- WorldEvents ],

	text_utils:format( "changeset comprising ~B world events: ~ts",
		[ length( WorldEvents ),
		  text_utils:strings_to_string( Strings ) ] ).



% Returns a (verbose) textual description of the specified world event.
-spec world_event_to_string( world_event() ) -> ustring().
world_event_to_string( WorldEvent ) ->
	world_event_to_string( WorldEvent, _IsVerbose=true ).



% Returns a textual description of the specified world event, with specified
% verbosity (induced events will be described iff verbose).
%
-spec world_event_to_string( world_event(), boolean() ) -> ustring().
world_event_to_string( WorldEvent, IsVerbose ) ->
	world_event_to_string( WorldEvent, IsVerbose, _IndentationLevel=0 ).



% (helper)
-spec world_event_to_string( world_event(), boolean(), indentation_level() ) ->
									ustring().
world_event_to_string( #creation_event{
		id=EventId,
		timestamp=Timestamp,
		object_type=ObjectType,
		external_id=ExternalId,
		object_pid=ObjectPid,
		construction_parameters=ConstructionParameters,
		dataflow_pid=DataflowPid,
		induced_events=InducedEvents }, IsVerbose, IndentationLevel ) ->

	TimestampString = case Timestamp of

		undefined ->
			"not timestamped";

		_ ->
			text_utils:format( "timestamped as ~p", [ Timestamp ] )

	end,

	ExtString = case ExternalId of

		undefined ->
			"no external identifier";

		_ ->
			text_utils:format( "external identifier '~p'", [ ExternalId ] )

	end,

	PidString = case ObjectPid of

		undefined ->
			"no object PID";

		_ ->
			text_utils:format( "object PID ~p", [ ObjectPid ] )

	end,

	% Avoids that pretty-printing generates unreadable outputs on the right with
	% newlines:
	%
	ConstrParamString = text_utils:format( "~p", [ ConstructionParameters ] ),

	case IsVerbose of

		true ->

			InducedString = induced_events_to_string( InducedEvents,
													  IndentationLevel+1 ),

			text_utils:format( "creation event (id: ~p), ~ts, "
				"for object type '~ts', with ~ts and ~ts defined, "
				"using construction parameters '~ts', "
				"applying to dataflow ~w and inducing ~ts",
				[ EventId, TimestampString, ObjectType, ExtString, PidString,
				  ConstrParamString, DataflowPid, InducedString ] );

		false ->
			text_utils:format( "creation event (id: ~p), ~ts, "
				"for object type '~ts', with ~ts and ~ts defined, "
				"using ~B construction parameters, applying "
				"to dataflow ~w, inducing ~B events",
				[ EventId, TimestampString, ObjectType, ExtString, PidString,
				  length( ConstructionParameters ), DataflowPid,
				  length( InducedEvents ) ] )

	end;



world_event_to_string( #destruction_event{
		id=EventId,
		timestamp=Timestamp,
		object_type=ObjectType,
		external_id=ExternalId,
		object_pid=ObjectPid,
		dataflow_pid=DataflowPid,
		induced_events=InducedEvents }, IsVerbose, IndentationLevel ) ->

	TimestampString = case Timestamp of

		undefined ->
			"not timestamped";

		_ ->
			text_utils:format( "timestamped as ~p", [ Timestamp ] )

	end,

	ExtString = case ExternalId of

		undefined ->
			"no external identifier";

		_ ->
			text_utils:format( "external identifier '~p'", [ ExternalId ] )

	end,

	PidString = case ObjectPid of

		undefined ->
			"no object PID";

		_ ->
			text_utils:format( "object PID ~p", [ ObjectPid ] )

	end,

	case IsVerbose of

		true ->
			InducedString = induced_events_to_string( InducedEvents,
													  IndentationLevel+1 ),

			text_utils:format( "destruction event (id: ~p), ~ts, "
				"of object type '~ts', with ~ts and ~ts defined, "
				"applying to dataflow ~w and inducing ~ts",
				[ EventId, TimestampString, ObjectType, ExtString, PidString,
				  DataflowPid, InducedString ] );

		false ->
			text_utils:format( "destruction event (id: ~p), ~ts, "
				"of object type '~ts', with ~ts and ~ts defined, "
				"applying to dataflow ~w, inducing ~B events",
				[ EventId, TimestampString, ObjectType, ExtString, PidString,
				  DataflowPid, length( InducedEvents ) ] )

	end;


world_event_to_string( #association_event{
		id=EventId,
		timestamp=Timestamp,
		object_type=ObjectType,
		external_id=ExternalId,
		object_pid=ObjectPid,
		association_information=AssocInfo,
		dataflow_pid=DataflowPid,
		induced_events=InducedEvents }, IsVerbose, IndentationLevel ) ->

	TimestampString = case Timestamp of

		undefined ->
			"not timestamped";

		_ ->
			text_utils:format( "timestamped as ~p", [ Timestamp ] )

	end,

	ExtString = case ExternalId of

		undefined ->
			"no external identifier";

		_ ->
			text_utils:format( "external identifier '~p'", [ ExternalId ] )

	end,

	PidString = case ObjectPid of

		undefined ->
			"no object PID";

		_ ->
			text_utils:format( "object PID ~p", [ ObjectPid ] )

	end,

	AssocString = text_utils:format( "with association information '~p'",
									 [ AssocInfo ] ),

	case IsVerbose of

		true ->
			InducedString = induced_events_to_string( InducedEvents,
													  IndentationLevel+1 ),

			text_utils:format( "association event (id: ~p), ~ts, "
				"for object type '~ts', with ~ts and ~ts defined, ~ts, "
				"applying to dataflow ~w and inducing ~ts",
				[ EventId, TimestampString, ObjectType, ExtString, PidString,
				  AssocString, DataflowPid, InducedString ] );

		false ->
			text_utils:format( "association event (id: ~p), ~ts, "
				"for object type '~ts', with ~ts and ~ts defined, ~ts, "
				"applying to dataflow ~w, inducing ~B events",
				[ EventId, TimestampString, ObjectType, ExtString, PidString,
				  AssocString, DataflowPid, length( InducedEvents ) ] )

	end;


world_event_to_string( #binary_association_event{
		id=EventId,
		timestamp=Timestamp,
		association_type=AssociationType,
		source_object_type=SourceObjectType,
		target_object_type=TargetObjectType,
		source_external_id=SourceExternalId,
		target_external_id=TargetExternalId,
		source_object_pid=SourceObjectPid,
		target_object_pid=TargetObjectPid,
		association_information=AssocInfo,
		dataflow_pid=DataflowPid,
		induced_events=InducedEvents }, IsVerbose, IndentationLevel ) ->

	TimestampString = case Timestamp of

		undefined ->
			"not timestamped";

		_ ->
			text_utils:format( "timestamped as ~p", [ Timestamp ] )

	end,

	SourceExtString = case SourceExternalId of

		undefined ->
			"no external identifier";

		_ ->
			text_utils:format( "external identifier '~p'",
							   [ SourceExternalId ] )

	end,

	TargetExtString = case TargetExternalId of

		undefined ->
			"no external identifier";

		_ ->
			text_utils:format( "external identifier '~p'",
							   [ TargetExternalId ] )

	end,

	SourcePidString = case SourceObjectPid of

		undefined ->
			"no object PID";

		_ ->
			text_utils:format( "object PID ~p", [ SourceObjectPid ] )

	end,

	TargetPidString = case TargetObjectPid of

		undefined ->
			"no object PID";

		_ ->
			text_utils:format( "object PID ~p", [ TargetObjectPid ] )

	end,

	AssocString = text_utils:format( "with association information '~p'",
									 [ AssocInfo ] ),

	case IsVerbose of

		true ->
			InducedString = induced_events_to_string( InducedEvents,
													  IndentationLevel+1 ),

			text_utils:format( "binary association event (id: ~p), ~ts, "
				"of type '~ts' from source object type '~ts' "
				"with ~ts and ~ts to target object type '~ts' with ~ts and "
				"~ts, ~ts, applying to dataflow ~w and inducing ~ts",
				[ EventId, TimestampString, AssociationType, SourceObjectType,
				  SourceExtString, SourcePidString, TargetObjectType,
				  TargetExtString, TargetPidString, AssocString, DataflowPid,
				  InducedString ] );

		false ->
			text_utils:format( "binary association event (id: ~p), ~ts, "
				"of type '~ts' from source object type '~ts' "
				"with ~ts and ~ts to target object type '~ts' with ~ts "
				"and ~ts, ~ts, applying to dataflow ~w and inducing ~B events",
				[ EventId, TimestampString, AssociationType, SourceObjectType,
				  SourceExtString, SourcePidString, TargetObjectType,
				  TargetExtString, TargetPidString, AssocString, DataflowPid,
				  length( InducedEvents ) ] )

	end;



world_event_to_string( #disassociation_event{
		id=EventId,
		timestamp=Timestamp,
		object_type=ObjectType,
		external_id=ExternalId,
		object_pid=ObjectPid,
		disassociation_information=DisassocInfo,
		dataflow_pid=DataflowPid,
		induced_events=InducedEvents }, IsVerbose, IndentationLevel ) ->

	TimestampString = case Timestamp of

		undefined ->
			"not timestamped";

		_ ->
			text_utils:format( "timestamped as ~p", [ Timestamp ] )

	end,

	ExtString = case ExternalId of

		undefined ->
			"no external identifier";

		_ ->
			text_utils:format( "external identifier '~p'", [ ExternalId ] )

	end,

	PidString = case ObjectPid of

		undefined ->
			"no object PID";

		_ ->
			text_utils:format( "object PID ~p", [ ObjectPid ] )

	end,

	DisassocString = text_utils:format( "with association information '~p'",
										[ DisassocInfo ] ),

	case IsVerbose of

		true ->
			InducedString = induced_events_to_string( InducedEvents,
													  IndentationLevel+1 ),

			text_utils:format( "disassociation event (id: ~p), ~ts, "
				"for object type '~ts', with ~ts and ~ts defined, ~ts, "
				"applying to dataflow ~w and inducing ~ts",
				[ EventId, TimestampString, ObjectType, ExtString, PidString,
				  DisassocString, DataflowPid, InducedString ] );

		false ->
			text_utils:format( "disassociation event (id: ~p), ~ts, "
				"for object type '~ts', with ~ts and ~ts defined, ~ts, "
				"applying to dataflow ~w, inducing ~B events",
				[ EventId, TimestampString, ObjectType, ExtString, PidString,
				  DisassocString, DataflowPid, length( InducedEvents ) ] )

	end;



world_event_to_string( #connection_event{
		id=EventId,
		timestamp=Timestamp,
		source_block_type=SourceBlockType,
		target_block_type=TargetBlockType,
		source_external_id=SourceExternalId,
		target_external_id=TargetExternalId,
		source_block_pid=SourceBlockPid,
		target_block_pid=TargetBlockPid,
		output_port_name=OutputPortName,
		input_port_name=InputPortName,
		dataflow_pid=DataflowPid,
		induced_events=InducedEvents }, IsVerbose, IndentationLevel ) ->

	TimestampString = case Timestamp of

		undefined ->
			"not timestamped";

		_ ->
			text_utils:format( "timestamped as ~p", [ Timestamp ] )

	end,

	PortString = text_utils:format( "from output port '~ts' to "
		"input port '~ts'", [ OutputPortName, InputPortName ] ),

	SourceString = case SourceExternalId of

		undefined ->

			case SourceBlockPid of

				undefined ->
					"for which no external identifier or PID is defined";

				_ ->
					text_utils:format( "designated by PID ~w (no external "
						"identifier available)", [ SourceBlockPid ] )

			end;


		_ ->

			case SourceBlockPid of

				undefined ->
					text_utils:format( "designated by external identifier '~p'"
						" (no PID defined)", [ SourceExternalId ] );

				_ ->
					text_utils:format( "designated by external identifier '~p'"
						" and PID ~w", [ SourceExternalId, SourceBlockPid ] )

			end

	end,


	TargetString = case TargetExternalId of

		undefined ->

			case TargetBlockPid of

				undefined ->
					"for which no external identifier or PID is defined";

				_ ->
					text_utils:format( "designated by PID ~w (no external "
						"identifier available)", [ TargetBlockPid ] )

			end;


		_ ->

			case TargetBlockPid of

				undefined ->
					text_utils:format( "designated by external identifier '~p'"
						" (no PID defined)", [ TargetExternalId ] );

				_ ->
					text_utils:format( "designated by external identifier '~p'"
						" and PID ~w", [ TargetExternalId, TargetBlockPid ] )

			end

	end,

	case IsVerbose of

		true ->
			InducedString = induced_events_to_string( InducedEvents,
													  IndentationLevel+1 ),

			text_utils:format( "connection event (id: ~p), ~ts, "
				"from block type '~ts' to block type '~ts', ~ts, with a "
				"source ~ts and a target ~ts (both in dataflow ~w), "
				"and inducing ~ts",
				[ EventId, TimestampString, SourceBlockType, TargetBlockType,
				  PortString, SourceString, TargetString, DataflowPid,
				  InducedString ] );

		false ->
			text_utils:format( "connection event (id: ~p), ~ts, "
				"from block type '~ts' to block type '~ts', ~ts, with a "
				"source ~ts and a target ~ts (both in dataflow ~w), "
				"inducing ~B events",
				[ EventId, TimestampString, SourceBlockType,
				  TargetBlockType, PortString, SourceString, TargetString,
				  DataflowPid, length( InducedEvents ) ] )

	end;



world_event_to_string( #disconnection_event{
		id=EventId,
		timestamp=Timestamp,
		source_block_type=SourceBlockType,
		target_block_type=TargetBlockType,
		source_external_id=SourceExternalId,
		target_external_id=TargetExternalId,
		source_block_pid=SourceBlockPid,
		target_block_pid=TargetBlockPid,
		output_port_name=OutputPortName,
		input_port_name=InputPortName,
		dataflow_pid=DataflowPid,
		induced_events=InducedEvents }, IsVerbose, IndentationLevel ) ->

	TimestampString = case Timestamp of

		undefined ->
			"not timestamped";

		_ ->
			text_utils:format( "timestamped as ~p", [ Timestamp ] )

	end,

	PortString = text_utils:format( "from output port '~ts' to "
		"input port '~ts'",	[ OutputPortName, InputPortName ] ),

	SourceString = case SourceExternalId of

		undefined ->

			case SourceBlockPid of

				undefined ->
					"for which no external identifier or PID is defined";

				_ ->
					text_utils:format( "designated by PID ~w (no external "
						"identifier available)", [ SourceBlockPid ] )

			end;


		_ ->

			case SourceBlockPid of

				undefined ->
					text_utils:format( "designated by external identifier '~p'"
						" (no PID defined)", [ SourceExternalId ] );

				_ ->
					text_utils:format( "designated by external identifier '~p'"
						" and PID ~w", [ SourceExternalId, SourceBlockPid ] )

			end

	end,


	TargetString = case TargetExternalId of

		undefined ->

			case TargetBlockPid of

				undefined ->
					"for which no external identifier or PID is defined";

				_ ->
					text_utils:format( "designated by PID ~w (no external "
						"identifier available)", [ TargetBlockPid ] )

			end;


		_ ->

			case TargetBlockPid of

				undefined ->
					text_utils:format( "designated by external identifier '~p'"
						" (no PID defined)", [ TargetExternalId ] );

				_ ->
					text_utils:format( "designated by external identifier '~p'"
						" and PID ~w", [ TargetExternalId, TargetBlockPid ] )

			end

	end,

	case IsVerbose of

		true ->
			InducedString = induced_events_to_string( InducedEvents,
													  IndentationLevel+1 ),

			text_utils:format( "disconnection event (id: ~p), ~ts, of "
				"block type '~ts' to block type '~ts', ~ts, "
				"with a source designated "
				"by ~ts and a target designated by ~ts "
				"(both in dataflow ~w), and inducing ~ts",
				[ EventId, TimestampString, SourceBlockType, TargetBlockType,
				  PortString, SourceString, TargetString, DataflowPid,
				  InducedString ] );

		false ->
			text_utils:format( "disconnection event (id: ~p), ~ts, of "
				"block type '~ts' to block type '~ts', ~ts, with a "
				"source ~ts and a target ~ts (both in dataflow ~w), "
				"inducing ~B events",
				[ EventId, TimestampString, SourceBlockType, TargetBlockType,
				  PortString, SourceString, TargetString, DataflowPid,
				  length( InducedEvents ) ] )

	end;



world_event_to_string( #update_event{
		id=EventId,
		timestamp=Timestamp,
		object_type=ObjectType,
		external_id=ExternalId,
		object_pid=ObjectPid,
		updates=Updates,
		dataflow_pid=DataflowPid,
		induced_events=InducedEvents }, IsVerbose, IndentationLevel ) ->

	TimestampString = case Timestamp of

		undefined ->
			"not timestamped";

		_ ->
			text_utils:format( "timestamped as ~p", [ Timestamp ] )

	end,


	ExtString = case ExternalId of

		undefined ->
			"no external identifier";

		_ ->
			text_utils:format( "external identifier '~p'", [ ExternalId ] )

	end,


	PidString = case ObjectPid of

		undefined ->
			"no object PID";

		_ ->
			text_utils:format( "object PID ~p", [ ObjectPid ] )

	end,


	UpdateString = case Updates of

		[] ->
			"no attribute update";

		_ ->
			UpdateStrings = [ text_utils:format( "attribute '~ts' being set "
												 "to value '~p'", [ N, V ] )
							  || { N, V } <- Updates ],

			text_utils:format( "~B attribute updates: ~ts",
				[ length( Updates ),
				  text_utils:strings_to_string( UpdateStrings ) ] )

	end,

	case IsVerbose of

		true ->

			InducedString = induced_events_to_string( InducedEvents,
													  IndentationLevel+1 ),

			text_utils:format( "update event (id: ~p), ~ts, "
				"for object type '~ts', with ~ts and ~ts defined, "
				"specifying ~ts, applying to dataflow ~w and inducing ~ts",
				[ EventId, TimestampString, ObjectType, ExtString,
				  PidString, UpdateString, DataflowPid, InducedString ] );

		false ->
			text_utils:format( "update event (id: ~p), ~ts, "
				"for object type '~ts', with ~ts and ~ts defined, "
				"specifying ~ts, applying to dataflow ~w, inducing ~B events",
				[ EventId, TimestampString, ObjectType, ExtString, PidString,
				  UpdateString, DataflowPid, length( InducedEvents ) ] )

	end;


world_event_to_string( UnknownEvent, _IsVerbose, _IndentationLevel ) ->
	throw( { unexpected_world_event, UnknownEvent } ).




% Returns a textual description of the specified induced world events.
-spec induced_events_to_string( [ world_event() ], indentation_level() ) ->
										ustring().
induced_events_to_string( _WorldEvents=[], _IndentationLevel ) ->
	"no specific world event";

induced_events_to_string( _WorldEvents=[ Event ], IndentationLevel ) ->
	text_utils:format( "a single world event: ~ts",
					   [ world_event_to_string( Event, _IsVerbose=true,
												IndentationLevel ) ] );

induced_events_to_string( WorldEvents, IndentationLevel ) ->

	Strings = [ world_event_to_string( W, _IsVerbose=true, IndentationLevel )
				|| W <- WorldEvents ],

	text_utils:format( "following ~B world events: ~ts",
		[ length( WorldEvents ), text_utils:strings_to_string( Strings ) ] ).



% Finds specified event, based on its specified identifier, in the specified
% list; returns a pair made of that event and of the rest of the list.
%
-spec find_event_by_id( event_id(), [ world_event() ] ) ->
								{ world_event(), [ world_event() ] }.
find_event_by_id( EventId, EventList ) ->

	% We see here the event records as tagged tuples in order to avoid writing
	% one clause for each event type (as by design the ID is always the second
	% element of each tuple):
	%
	case lists:keytake( _KeyMatching=EventId, _N=2, EventList ) of

		% First matching returned, shall be the only one (not checked):
		{ value, TargetEvent, RemainingEventList } ->
			{ TargetEvent, RemainingEventList };

		false ->
			throw( { event_id_not_found, EventId, EventList } )

	end.



% Returns the identifier of specified event.
-spec get_event_id( world_event() ) -> event_id().
get_event_id( Event ) ->
	% Record seen as a tuple to abstract-out the record tag:
	element( _N=2, _Tuple=Event ).



% Returns the port identifier corresponding to specified block and port name.
-spec get_port_id( block_pid(), port_string_name() ) -> port_id().
get_port_id( BlockPid, PortName ) ->
	{ BlockPid, text_utils:string_to_binary( PortName ) }.
