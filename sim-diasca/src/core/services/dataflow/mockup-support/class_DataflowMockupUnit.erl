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

% Author: Robin Huart (robin-externe.huart@edf.fr)


-module(class_DataflowMockupUnit).

-define( class_description,
		 "Dataflow mock-up unit class, corresponding to the implementation "
		 "of the mock-up computational parts of a dataflow. "
		 "The main purpose of this specialised dataflow processing unit is "
		 "to emulate the inputs/outputs of a real unit, by applying "
		 "mock-up clauses that are attached to every mock-up instance during "
		 "its construction. "
		 "Please refer to the 'Sim-Diasca Dataflow HOWTO' for further "
		 "information." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowProcessingUnit ] ).


% Attributes that are specific to a mock-up unit are:
-define( class_attributes, [

	{ mockup_clauses, [ mockup_clause() ],
	  "the clauses describing the behaviour of this unit" } ] ).


% Helpers:
-export([ read_mockup_unit_spec/1 ]).


% Design notes:
%
% A mock-up unit exhibit all the attributes of a given classical processing
% unit, but emulates its behaviour, thanks to mock-up clauses.
%
% Each of these clauses defines which computations must be made in the
% activate/1 method (resulting in output ports being assigned) based on the
% context (the simulation time and the values on the input ports).
%
% The application rule is: when this mock-up unit is activated (as usual based
% on its activation policy), these clauses will be evaluated sequentially, and
% the first one matching the context will be selected and will apply its changes
% in the output ports.
%
% The idea is that a user defining a mock-up unit will want to emulate a model
% that is at least partially known. Thus, it is up to the user to define mock-up
% clauses that build a unit behaviour close enough to the emulated model.
%
% The concept of 'variety' is also introduced. One may define mock-up units that
% emulate a given model (hence a same class, in the computational sense) while
% applying different behavioral clauses (from a mock-up instance of a given
% class to another, clauses may vary).
%
% Such clauses are neither class-specific nor instance-specific (generally many
% instances will obey a given set of clauses), they are deemed
% variety-specific. So a mock-up variety is a set of instances sharing the same
% mock-up clauses.
%
% It is still an open question to know whether this concept of variety is of any
% use for mock-up units. Letting this freedom of specifying different clauses
% for a same unit type (a unit type being a child class of the present class)
% has no impact on the source code contained in this file. It is only a question
% of vocabulary for documentation purposes. The question is to know to what
% extent the association between a unit type and a real model, and between a
% real model and a set of clauses, is close or not.



% Current (last) version of the native DUMF file format:
-define( dumf_format_version, "0.3.1" ).


% Helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.MockupUnit" ).


% To number clauses:
-type clause_count() :: basic_utils:count().


% For mockup_unit_spec:
-include("dataflow_defines.hrl").


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").


% For notify_error_fmt/2:
-include_lib("traces/include/traces.hrl").


% Shorthands:
-type ustring() :: text_utils:ustring().



% Constructs a new dataflow mock-up unit:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as automatically assigned by the load balancer
%
% - MockupUnitName is a human-readable name for that mock-up unit (as a plain,
% non-empty string)
%
% - ActivationPolicy is the policy driving the activations of the mock-up unit
%
% - InputPortSpecs is a list of the specifications of the input ports defined
% for this mock-up unit
%
% - OutputPortSpecs is a list of the specifications of the output ports defined
% for this mock-up unit
%
% - MockupClauses is a list of the specifications of the clauses ruling the
% behaviour of this mock-up unit (determining the state of its output ports from
% the simulation time and the state of its input ports)
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_DataflowProcessingUnit:unit_name(), mockup_unit_spec(),
				 dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, UnitName,
		   _MockupUnitSpec=#mockup_unit_spec{
							  activation_policy=ActivationPolicy,
							  input_port_specs=InputPortSpecs,
							  output_port_specs=OutputPortSpecs,
							  mockup_clauses=MockupClauses },
		   DataflowPid ) ->

	% First, sets the attributes of the direct mother class:
	ProcUnitState = class_DataflowProcessingUnit:construct( State,
		ActorSettings, ?trace_categorize(UnitName),
		ActivationPolicy, InputPortSpecs, OutputPortSpecs, DataflowPid ),

	% Second, checks the validity of the mock-up clauses:
	check_clauses( MockupClauses ),

	% Then sets the class-specific attributes:
	setAttribute( ProcUnitState, mockup_clauses, MockupClauses ).




% Methods section.


% Callback executed on the first diasca of existence of this mock-up unit.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							const_actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?debug_fmt( "Created ~ts.", [ to_string( State ) ] ),

	actor:const_return().



% Callback executed automatically whenever the mock-up unit is activated.
%
% Meant to be overridden.
%
-spec activate( wooper:state() ) -> oneway_return().
activate( State ) ->

	?info_fmt( "Evaluating now the ~ts.", [ to_string( State ) ] ),

	SetState = apply_clauses( State ),

	wooper:return_state( SetState ).



% Triggers the behaviour of the mock-up unit, based on the registered mock-up
% clauses.
%
% Note: this activation embeds several actions usually defined in the child unit
% classes (designed by a model implementor):
%
% - reading/getting the input port values
%
% - deducing the output values from input ones (this is what really "applying
% the clauses" means)
%
% - setting these output values to the corresponding outputs ports (including
% the creation of the channel values)
%
% Indeed, the genericity of the behaviour of mock-ups (following predefined
% rules) allows the automation of these first and third operations, and the
% execution of the second operation based on data (not code).
%
-spec apply_clauses( wooper:state() ) -> wooper:state().
apply_clauses( State ) ->

	MockupClauses = ?getAttr(mockup_clauses),

	% Applies the first matching clause, by comparing the MockupInputSpecs with
	% the SimulationTime, the InputPortNames and the InputPortStatuses, and
	% then, if all matching tests succeeded, applying the corresponding
	% MockupOutputSpecs:
	%
	try_clause( MockupClauses, State ).



% Helper section.


% Checking the structure of a set of mock-up clauses, that they are indeed a
% list.
%
-spec check_clauses( [ mockup_clause() ] ) -> void().
check_clauses( MockupClauses ) when is_list( MockupClauses ) ->
	check_each_clause( MockupClauses, _ClauseCount=1 );

check_clauses( MockupClauses ) ->
	?notify_error_fmt( "The clauses defined for the mock-up unit are not a "
					   "list:~n~p", [ MockupClauses ] ),
	throw( { non_list_clauses, MockupClauses } ).



% Checks that a particular mock-up clause is a triplet:
-spec check_each_clause( [ mockup_clause() ], clause_count() ) -> void().
check_each_clause( _MockupClauses=[], _ClauseCount ) ->
	ok;

check_each_clause( [ _MockupClause={ TimeSpec, InputMatchSpecs,
						   OutputMatchSpecs } | T ], ClauseCount )->

	check_time_spec_format( TimeSpec, ClauseCount ),
	check_input_match_spec_format( InputMatchSpecs, ClauseCount ),
	check_output_match_spec_format( OutputMatchSpecs, ClauseCount ),

	check_each_clause( T, ClauseCount+1 );

check_each_clause( [ MockupClause | _T ], ClauseCount ) ->
	?notify_error_fmt( "The clause #~B of the current mock-up unit is not "
					   "a triplet:~n~p", [ ClauseCount, MockupClause ] ),
	throw( { non_triplet_clause, MockupClause } ).



% Checks that the time specification of a clause is legit:
-spec check_time_spec_format( clause_time_spec(), clause_count() ) -> void().
check_time_spec_format( any_time, _ClauseCount ) ->
	ok;

check_time_spec_format( TimeSpec, _ClauseCount )
  when is_integer( TimeSpec ) ->
	ok;

check_time_spec_format( TimeSpec, ClauseCount ) ->
	?notify_error_fmt( "The time specification of the clause #~B of the "
		"current mock-up unit is neither an integer nor 'any_time': ~p",
		[ ClauseCount, TimeSpec ] ),
	throw( { invalid_time_spec, TimeSpec } ).



% Checks that the input match specifications of a clause are valid choices:
-spec check_input_match_spec_format( [ clause_input_match_spec() ],
									 clause_count() ) -> void().
check_input_match_spec_format( [], _ClauseCount ) ->
	ok;

check_input_match_spec_format( [ _InputMatchSpec={ IPName, IPMatchSpec } | T ],
							   ClauseCount ) when is_list( IPName ) ->

	case IPMatchSpec of

		any_state ->
			ok;

		unset ->
			ok;

		set ->
			ok;

		{ set, _Value } ->
			ok;

		{ between, Value1, Value2 } when is_number( Value1 ) andalso
										 is_number( Value2 ) ->
			case Value1 =< Value2 of

				true ->
					ok;

				false ->
					?notify_error_fmt( "Inconsistent bounds for the 'between' "
						"input match spec in the clause #~B of the current "
						"mock-up unit: ~p", [ ClauseCount, IPMatchSpec ] ),
					throw( { invalid_input_match_spec, IPMatchSpec } )

			end;

		{ around, Value, Tolerance } when is_number( Value ) andalso
										  is_number( Tolerance ) ->
			ok;

		{ around, Value } when is_number( Value ) ->
			ok;

		{ among, ValuesList } when is_list( ValuesList ) ->
			ok;

		_ ->
			?notify_error_fmt( "Unknown input match spec encountered in the "
				"clause #~B of the current mock-up unit: ~p",
				[ ClauseCount, IPMatchSpec ] ),
			throw( { invalid_input_match_spec, IPMatchSpec } )

	end,

	check_input_match_spec_format( T, ClauseCount );

check_input_match_spec_format(
  [ _InputMatchSpec={ IPName, _IPMatchSpec } | _T ], ClauseCount ) ->
	?notify_error_fmt( "The input port name of an input match specification of "
		"the clause #~B of the current mock-up unit is not a "
		"string: ~p", [ ClauseCount, IPName ] ),
	throw( { non_string_input_port_name, IPName } );


check_input_match_spec_format( [ InputMatchSpec | _T ], ClauseCount ) ->
	?notify_error_fmt( "An input match specification of the clause #~B of "
		"the current mock-up unit is invalid: ~p",
		[ ClauseCount, InputMatchSpec ] ),
	throw( { invalid_input_match_spec, InputMatchSpec } );

check_input_match_spec_format( InputMatchSpecs, ClauseCount ) ->
	?notify_error_fmt( "The input match specifications of the clause #~B of "
		"the current mock-up unit are not a list:~n~p",
		[ ClauseCount, InputMatchSpecs ] ),
	throw( { non_list_input_match_specs, InputMatchSpecs } ).




% Checks that the output match specifications of a clause are valid choices:
-spec check_output_match_spec_format( [ clause_output_match_spec() ],
									  clause_count() ) -> void().
check_output_match_spec_format( _OutputMatchSpecs=[], _ClauseCount ) ->
	ok;

check_output_match_spec_format(
  [ _OutputMatchSpec={ OPName, OPMatchSpec } | T ], ClauseCount )
  when is_list( OPName ) ->

	case OPMatchSpec of

		reassign ->
			ok;

		unset ->
			ok;

		{ set, _Value } ->
			ok;

		{ state_of, OutputPortName } when is_list( OutputPortName ) ->
			ok ;

		{ state_of, _Anything } ->
			?notify_error_fmt( "Invalid 'state_of' output match spec in the "
				"clause #~B of the current mock-up unit: ~p",
				[ ClauseCount, OPMatchSpec ] ),
			throw( { invalid_output_match_spec, OPMatchSpec } );

		_ ->
			?notify_error_fmt( "Unknown output match spec encountered in the "
				"clause #~B of the current mock-up unit: ~p",
				[ ClauseCount, OPMatchSpec ] ),
			throw( { invalid_output_match_spec, OPMatchSpec } )

	end,

	check_output_match_spec_format( T, ClauseCount );

check_output_match_spec_format(
  [ _OutputMatchSpec={ OPName, _OPMatchSpec } | _T ], ClauseCount ) ->
	?notify_error_fmt( "The output port name of an output match specification "
		"of the clause #~B of the current mock-up unit is not "
		"a string: ~p", [ ClauseCount, OPName ] ),
	throw( { non_string_output_port_name, OPName } );

check_output_match_spec_format( [ OutputMatchSpec | _T ], ClauseCount ) ->
	?notify_error_fmt( "An output match specification of the clause #~B of "
		"the current mock-up unit is invalid: ~p",
		[ ClauseCount, OutputMatchSpec ] ),
	throw( { invalid_output_match_spec, OutputMatchSpec } );

check_output_match_spec_format( OutputMatchSpecs, ClauseCount ) ->
	?notify_error_fmt( "The output match specifications of the clause #~B of "
		"the current mock-up unit are not a list:~n~p",
		[ ClauseCount, OutputMatchSpecs ] ),
	throw( { non_list_output_match_specs, OutputMatchSpecs } ).




% Clause per clause application.
-spec try_clause( [ mockup_clause() ], wooper:state() ) -> wooper:state().
try_clause( _Clauses=[], State ) ->
	State;

try_clause( [ _Clause={ any_time, InputSpecs, OutputSpecs } | T ], State ) ->

	% Checks the specifications regarding the states of input ports:
	case check_input_specs( InputSpecs, State ) of

		% If they do match in turn, sets the states of output ports
		% according to the corresponding specifications:
		%
		true ->
			apply_output_specs( OutputSpecs, State );

		% If they do not, we try to match next clause (if any):
		false ->
			try_clause( T, State )

	end;

try_clause( [ _Clause={ TimeSpec, InputSpecs, OutputSpecs } | T ], State )
  when is_integer( TimeSpec ) ->

	% Gets the simulation time:
	SimulationStep = class_Actor:getSimulationTickOffset( State ),

	% Compares this time to the one specified in the current clause:
	case TimeSpec of

		% If the time specification is matched, checks the specifications
		% regarding the states of input ports:
		%
		SimulationStep ->

			case check_input_specs( InputSpecs, State ) of

				% If they do match in turn, sets the states of output ports
				% according to the corresponding specifications:
				%
				true ->
					apply_output_specs( OutputSpecs, State );

				% If they do not, we try to match next clause (if any):
				false ->
					try_clause( T, State )

			end;

		% ... otherwise the current clause is simply ignored, and we continue:
		_OtherTime ->
			try_clause( T, State )

	end;

try_clause( [ Clause | _T ], State ) ->

	?error_fmt( "A mock-up clause does not respect the expected structure, "
		"namely a triplet whose first element (time specification) "
		"is either the atom 'any_time' or an integer timestamp, "
		"the latter two being {Key,Value} pairs, keys being "
		"strings referring to port names:~n~p"
		"Please refer to the 'Sim-Diasca Dataflow HOWTO' for further "
		"information.", [ Clause ] ),

	throw( { invalid_mockup_clause, Clause } ).



% Checks if the input specifications match the states of the input ports.
%
% This is done recursively over the list of port specifications. Any mismatch
% will break the recursion and will make the function return 'false' (spec not
% matched).
%
% (refer to the Dataflow-HOWTO documentation for more details about each kind of
%  specification)
%
-spec check_input_specs( [ clause_input_match_spec() ], wooper:state() ) ->
								boolean().
check_input_specs( _InputMatchSpecs=[], _State ) ->
	true;

check_input_specs( _InputMatchSpecs=[
			 { InputPortNameSpec, InputPortStatusSpec } | T ], State ) ->

	% Finds an input port with name InputPortNameSpec and gets its status:
	InputPortStatus =
		class_DataflowBlock:get_input_port_status( InputPortNameSpec, State ),

	% Checks if the current input specification matches this status:
	case InputPortStatus of

		unset ->
			check_unset_input_port( InputPortStatusSpec, State )
				andalso check_input_specs( T, State );

		{ set, InputPortValue } ->
			check_set_input_port( InputPortStatusSpec, InputPortValue, State )
				andalso check_input_specs( T, State )

	end.



% Performs the match checking whether an input port is unset.
check_unset_input_port( _InputPortStatusSpec=any_state, _State ) ->
	true ;

check_unset_input_port( _InputPortStatusSpec=unset, _State ) ->
	true;

check_unset_input_port( _InputPortStatusSpec=set, _State ) ->
	false;

check_unset_input_port( _InputPortStatusSpec={ set, _Value }, _State ) ->
	false;

check_unset_input_port( _InputPortStatusSpec={ between, _Value1, _Value2 },
						_State ) ->
	false;

check_unset_input_port( _InputPortStatusSpec={ around, _Value, _Tolerance },
						_State ) ->
	false;

check_unset_input_port( _InputPortStatusSpec={ around, _Value }, _State ) ->
	false;

check_unset_input_port( _InputPortStatusSpec={ among, _ValueList },
						_State ) ->
	false;

check_unset_input_port( InputPortStatusSpec, State ) ->
	?error_fmt( "Invalid input port specification '~p' encountered. "
		"Please refer to the 'Sim-Diasca Dataflow HOWTO' for further "
		"information.", [ InputPortStatusSpec ] ),
	throw( { invalid_mockup_input_spec, InputPortStatusSpec } ).



% Performs the match checking whether an input port is set.
check_set_input_port( _InputPortStatusSpec=any_state, _InputPortValue,
					  _State ) ->
	true;

check_set_input_port( _InputPortStatusSpec=unset, _InputPortValue, _State ) ->
	false;

check_set_input_port( _InputPortStatusSpec=set, _InputPortValue, _State ) ->
	true;

% Matching:
check_set_input_port( _InputPortStatusSpec={ set, InputPortValue },
					  InputPortValue, _State ) ->
	true;

% Non-matching:
check_set_input_port( _InputPortStatusSpec={ set, _Value }, _InputPortValue,
					  _State ) ->
	false;

check_set_input_port( _InputPortStatusSpec={ between, Value1, Value2 },
					  InputPortValue, _State ) ->
	Value1 =< InputPortValue andalso InputPortValue =< Value2;

check_set_input_port( _InputPortStatusSpec={ around, Value, Tolerance },
					  InputPortValue, _State ) ->
	math_utils:are_relatively_close( InputPortValue, Value, Tolerance );

check_set_input_port( _InputPortStatusSpec={ around, Value }, InputPortValue,
					  _State ) ->
	math_utils:are_relatively_close( InputPortValue, Value );

check_set_input_port( _InputPortStatusSpec={ among, ValueList }, InputPortValue,
					  _State ) ->
	lists:member( InputPortValue, ValueList );

check_set_input_port( InputPortStatusSpec, _InputPortValue, State ) ->
	?error_fmt( "Invalid input port specification ~p encountered. Please refer "
		"to the 'Sim-Diasca Dataflow HOWTO' for further information.",
		[ InputPortStatusSpec ] ),
	throw( { invalid_mockup_input_spec, InputPortStatusSpec } ).



% Sets the specified output values to the specified output ports, as would have
% to do any user-defined model unit inheriting from class_ProcessingUnit.
%
% When successful, this process automatically generates the channel values from
% the specifications used to build the output ports (during the construction of
% the mock-up unit).
%
% (refer to the Dataflow-HOWTO documentation for more details about each kind of
%  specification)
%
-spec apply_output_specs( [ clause_output_match_spec() ], wooper:state() ) ->
								wooper:state().
apply_output_specs( _OutputMatchSpecs=[], State ) ->
	State ;


% Sets again the same value (this is not a constant, do-nothing case, since the
% corresponding actor message will be sent again):
%
apply_output_specs( [ _OutputMatchSpecs={ OutputPortNameSpec,
							_OutputPortStatusSpec=reassign } | T ], State ) ->

	OutputPortStatus = class_DataflowBlock:get_output_port_status(
						 OutputPortNameSpec, State ),

	case OutputPortStatus of

		unset ->
			apply_output_specs( T, State );

		{ set, OutputPortValue } ->
			SetState = set_output_port_from_name( OutputPortNameSpec,
												  OutputPortValue, State ),
			apply_output_specs( T, SetState )

	end;


% Do-nothing case, leaves the output port in the default unset state:
apply_output_specs( [ _OutputMatchSpecs={ _OutputPortNameSpec,
							_OutputPortStatusSpec=unset } | T ], State ) ->
	apply_output_specs( T, State );


% In this case, the value to be set is explicitly given:
apply_output_specs( [ _OutputMatchSpecs={ OutputPortNameSpec,
							_OutputPortStatusSpec={ set, Value } } | T ],
					State ) ->
	SetState = set_output_port_from_name( OutputPortNameSpec, Value, State ),
	apply_output_specs( T, SetState );


% Copies the state of an input port identified by the specified name:
apply_output_specs( [ _OutputMatchSpecs={ OutputPortNameSpec,
						   _OutputPortStatusSpec={ state_of, InputPortName } }
					  | T ], State ) ->

	InputPortStatus = class_DataflowBlock:get_input_port_status( InputPortName,
																 State ),

	case InputPortStatus of

		unset ->
			apply_output_specs( T, State );

		{ set, InputPortValue } ->
			SetState = set_output_port_from_name( OutputPortNameSpec,
												  InputPortValue, State ),
			apply_output_specs( T, SetState )

	end;


% Raises an error if the output port specification is not a supported one:
apply_output_specs( [ _OutputMatchSpecs={ OutputPortNameSpec,
							OutputPortStatusSpec } | _T ], State ) ->

	?error_fmt( "Invalid output port specification ~p for output port ~p. "
		"Please refer to the 'Sim-Diasca Dataflow HOWTO' for further "
		"information.", [ OutputPortStatusSpec, OutputPortNameSpec ] ),

	throw( { invalid_mockup_output_spec, OutputPortStatusSpec } ).



% All-in-one function constructing a channel value from the user-defined port
% description and a value, then using it to set the state of the specified
% output port.
%
% (helper)
%
-spec set_output_port_from_name( output_port_string_name(), actual_value(),
								 wooper:state() ) -> wooper:state().
set_output_port_from_name( OutputPortName, Value, State ) ->

	% Looks for an output port with the specified name, and gets its SUTC
	% metadata:
	%
	{ ValueSemantics, ValueUnit, ValueType, _ValueConstraints } =
		class_DataflowBlock:get_output_port_metadata( OutputPortName, State ),

	% Creates the channel value:
	ValueTypeDescription = type_utils:type_to_description( ValueType ),

	ChannelValue = class_Dataflow:create_channel_value( Value, ValueSemantics,
										ValueUnit, ValueTypeDescription ),

	% Sets the output port with this channel value:
	class_DataflowBlock:set_output_port_value( OutputPortName, ChannelValue,
											   State ).




% Helper functions dealing with the parameters defining mock-up varieties.


% Reads a DUMF file and generates from it a mock-up variety that will be
% further instantiated by adding a name to its attributes.
%
% (helper)
%
-spec read_mockup_unit_spec( file_utils:file_name() ) -> mockup_unit_spec().
read_mockup_unit_spec( DUMFFilename ) ->

	% Checks if the file exists, and tries to read it as a table:
	?notify_info_fmt( "Reading the DUMF file ~ts", [ DUMFFilename ] ),

	AbsolutePath = file_utils:ensure_path_is_absolute( DUMFFilename ),

	case file_utils:is_existing_file_or_link( AbsolutePath ) of

		true ->
			ok;

		false ->
			?notify_error_fmt( "Error: the mock-up specification file '~ts' "
				"could not be found on node ~p.", [ AbsolutePath, node() ] ),
			throw( { mockup_variety_file_not_found, AbsolutePath, node() } )

	end,

	MockupSpecTable = try table:new( file_utils:read_terms( AbsolutePath ) ) of

		Table ->
			Table

	catch

		_AnyKind:Exception ->
			?notify_error_fmt( "This DUMF file looks corrupted or incomplete. "
				"Please make sure that every value is in a line, or a block of "
				"lines, that is dot-terminated (one data = one dot). Each of "
				"these values is expected to be a pair of the form '{ Key, "
				"Value }' where 'Key' is an Erlang atom and 'Value' is any "
				"data type natively supported by Erlang (including basic "
				"containers such as lists and tuples).~n~n~p",
				[ Exception ] ),
			throw( { reading_from_dumf_file_failed, DUMFFilename, Exception } )

	end,

	% From this table, gets all the necessary information for building the
	% mock-up unit and checks that the description is complete (with metadata)
	% and does not contain unexpected data not detected by the previous step:
	%
	AllExpectedKeys = [ dumf_version, unit_type, mockup_author,
		mockup_author_contact, mockup_version, mockup_date, activation_policy,
		input_port_specs, output_port_specs, mockup_clauses ],

	Keys = table:keys( MockupSpecTable ),

	case lists:all( fun( ReadKey ) ->
							lists:member( ReadKey, AllExpectedKeys )
					end,
					Keys ) of

		true ->
			ok;

		false ->

			?notify_error_fmt( "This DUMF file contains a table with either "
				"corrupted or extra keys, among:~n~p", [ Keys ] ),
			throw( { invalid_keys_in_dumf_table, DUMFFilename, Keys } )

	end,

	[ ReadDUMFVersion, ReadUnitType, ReadAuthor, ReadAuthorContact, UnitVersion,
	  ReadDate, ReadActPolicy, ReadIPSpecs, ReadOPSpecs, ReadMockupClauses ] =
		[ parse_mockup_spec_table( Key, MockupSpecTable, AbsolutePath )
		  || Key <- AllExpectedKeys ],

	% Checks that the version of the DUMF format to which the file declares to
	% conform is the current (last) version:
	%
	ExpectedVersion = ?dumf_format_version,

	case ReadDUMFVersion of

		ExpectedVersion ->
			ok;

		_AnyOtherVersion ->
			?notify_error_fmt( "This mock-up definition file conforms to an "
				"unknown or outdated version of the DUMF format: "
				"~ts instead of ~ts",
				[ ReadDUMFVersion, ?dumf_format_version ] ),
			throw( { invalid_dumf_version, ReadDUMFVersion, ExpectedVersion } )

	end,

	% Checks the class name (should begin with 'class_' for consistency):
	StringUnitType = text_utils:atom_to_string( ReadUnitType ),

	UnitType = case text_utils:split_after_prefix( "class_", StringUnitType ) of

		no_prefix ->
			?notify_error_fmt( "Invalid unit type specification: ~p. "
				"This must be an atom starting with 'class_' "
				"and statically known from its expected unit manager.",
				[ ReadUnitType ] ),
			throw( { invalid_mockup_unit_type, ReadUnitType } );

		_String ->
			ReadUnitType

	end,

	% Checks if the ActivationPolicy provided is supported:
	ActivationPolicy =
				   class_DataflowProcessingUnit:check_policy( ReadActPolicy ),

	% Gets the specifications of the input and output ports of the targeted
	% mock-up variety.
	%
	% For this, convert both lists, ReadInputPortSpecs and ReadOutputPortSpecs,
	% from two lists of {Key,Value} lists to two lists of records defined
	% respectively by the input_port_spec() and output_port_spec() types:
	%
	InputPortSpecs = [ class_DataflowBlock:parse_raw_input_port_spec( IPS )
					   || IPS <- ReadIPSpecs ],

	OutputPortSpecs = [ class_DataflowBlock:parse_raw_output_port_spec( OPS )
						|| OPS <- ReadOPSpecs ],

	% Checks then copies, if valid, the mockup clauses from the file:
	check_clauses( ReadMockupClauses ),
	MockupClauses = ReadMockupClauses,

	% Sends the metadata in a trace, for information:
	?notify_info_fmt( "~ts: the mock-up specification file ~ts has been "
		"succesfully read. ~ts wrote this file on ~ts (contact: ~ts). "
		"The version of this mock-up variety is ~ts and the "
		"activation policy of the generated units will be: ~p.",
		[ UnitType, AbsolutePath, ReadAuthor, ReadDate,
		  ReadAuthorContact, UnitVersion, ActivationPolicy ] ),

	% Returns the variety parameters in the form of a relevant record:
	#mockup_unit_spec{ unit_type=UnitType,
					   activation_policy=ActivationPolicy,
					   input_port_specs=InputPortSpecs,
					   output_port_specs=OutputPortSpecs,
					   mockup_clauses=MockupClauses }.



% Searches for specified key in specified table.
-spec parse_mockup_spec_table( table:key(), [ { atom(), term() } ],
							   file_utils:file_name() ) -> [ table:value() ].
parse_mockup_spec_table( Key, MockupSpecTable, FileName ) ->

	case table:lookup_entry( Key, MockupSpecTable ) of

		{ value, Value } ->
			Value;

		key_not_found ->
			?notify_error_fmt( "Key '~p' not found in mock-up file '~ts'.",
							   [ Key, FileName ] ),
			throw( { mockup_key_not_found, Key, FileName } )

	end.




% Helper functions dealing with formatted strings for displays.


% Returns a textual description of specified mock-up clauses.
-spec mockup_clauses_to_string( [ mockup_clause() ] ) -> ustring().
mockup_clauses_to_string( MockupClauses ) ->

	ClauseStrings = [ text_utils:format( "~p", [ MC ] )
					  || MC <- MockupClauses ],

	text_utils:strings_to_enumerated_string( ClauseStrings ).



% Returns a textual description of this mock-up unit.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	{ InputDetailed, OutputDetailed } =
		class_DataflowBlock:io_to_string( State ),

	MockupClauses = ?getAttr(mockup_clauses),

	NumberOfClauses = length( MockupClauses ),

	ClauseStrings = mockup_clauses_to_string( MockupClauses ),

	text_utils:format( "Mock-up unit named '~ts', applying the '~ts' "
		"activation policy, having ~ts and ~ts and following ~B clauses:~n~ts",
		[ ?getAttr(name), ?getAttr(activation_policy), InputDetailed,
		  OutputDetailed, NumberOfClauses, ClauseStrings ] ).
