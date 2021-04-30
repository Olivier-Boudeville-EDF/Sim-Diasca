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


% This header file is private to the class_DataflowBlock module, and defined to
% make it a bit more tractable and modular.
%
% It focuses mainly on the functions to manage the internal datastructures.



% Validation section.



% For most validate_X functions (used for standard ports), a get_X counterpart
% (used for iterated ports) is defined, in order to be able to assign, with no
% extra validation, values that have already been statically checked with
% iterations.

% State variables are specified as parameters so that traces can be emitted.

% We also define check_X functions, which perform more basic validations.



% Validates user-specified port name (expecting a string()) and returns a
% correct internal form thereof.
%
-spec validate_port_name( basic_utils:user_data(), wooper:state() ) ->
								port_name().
validate_port_name( Name, State ) ->

	case text_utils:is_non_empty_string( Name ) of

		true ->
			% To avoid accidental name collision with the names of iterated
			% ports, the port name must not contain said token:
			%
			case string:str( Name, ?iterated_port_token ) of

				% Not found, perfect:
				0 ->
					get_port_name( Name );

				_ ->
					?error_fmt( "Port name '~p' is invalid "
						"(as containing '~ts'), thus rejected.",
						[ Name, ?iterated_port_token ] ),

					throw( { invalid_port_name, Name, ?iterated_port_token } )

			end;

		false ->
			?error_fmt( "Port name '~p' is invalid (as not a non-empty "
						"string), thus rejected.", [ Name ] ),
			throw( { invalid_port_name, empty_string } )

	end.



% Validates user-specified port iteration name (expecting a string()) and
% returns a correct internal form thereof.
%
-spec validate_iteration_name( basic_utils:user_data(), wooper:state() ) ->
										  iteration_name().
validate_iteration_name( Name, State ) ->

	% Basically the same as for port names (with adapted error messages):
	case text_utils:is_non_empty_string( Name ) of

		true ->
			% To avoid accidental name collision with the names of iterated
			% ports, the iteration name must not contain said token:
			%
			case string:str( Name, ?iterated_port_token ) of

				% Not found, perfect:
				0 ->
					get_port_name( Name );

				_ ->
					?error_fmt( "Iteration name '~p' is invalid "
						"(as containing '~ts'), thus rejected.",
						[ Name, ?iterated_port_token ] ),

					throw( { invalid_iteration_name, Name,
							 ?iterated_port_token } )

			end;

		false ->
			?error_fmt( "Iteration name '~p' is invalid (as not a non-empty "
						"string), thus rejected.", [ Name ] ),
			throw( { invalid_iteration_name, empty_string } )

	end.



% Returns directly the corresponding (internal) port name.
-spec get_port_name( string() ) -> port_name().
get_port_name( Name ) ->
	text_utils:string_to_binary( Name ).





% Validates user-specified port iteration (expecting an iteration_spec()), and
% returns a canonical form for this iteration, which is:
%
% {CurrentCount :: port_count(),
%     {MinCount :: min_port_count(), MaxCount :: max_port_count()}}.
%
% CurrentCount being the corresponding required number of initial port
% instances.
%
% Iterations relying on a boolean value expected to already have been filtered.
%
-spec validate_iteration( basic_utils:user_data(), wooper:state() ) ->
							{ port_count(), iteration_spec() }.
validate_iteration( Spec={ InitialCount, { MinPortCount, MaxPortCount } },
					State ) ->

	validate_port_count( MinPortCount ),

	validate_iteration( { InitialCount, MaxPortCount }, State ),

	case InitialCount >= MinPortCount of

		true ->
			% Already in canonical form:
			Spec;

		false ->
			?error_fmt( "The port iteration specification '~p' is invalid "
				"(the initial port count, ~B, is lower than the "
				"minimum one, ~B), thus rejected.",
				[ Spec, InitialCount, MinPortCount ] ),
			throw( { too_low_initial_port_count, InitialCount, MinPortCount } )

	end;

validate_iteration( { InitialCount, _MaxPortCount=unbounded }, _State ) ->
	validate_port_count( InitialCount ),
	{ InitialCount, { 0, unbounded } };

validate_iteration( Spec={ InitialCount, MaxPortCount }, State ) ->
	validate_port_count( InitialCount ),
	validate_port_count( MaxPortCount ),

	case InitialCount =< MaxPortCount of

		true ->
			{ InitialCount, { 0, MaxPortCount } };

		false ->
			?error_fmt( "The port iteration specification '~p' is invalid "
				"(the initial port count, ~B, is higher than the "
				"maximum one, ~B), thus rejected.",
				[ Spec, InitialCount, MaxPortCount ] ),
			throw( { too_high_initial_port_count, InitialCount, MaxPortCount } )

	end;

validate_iteration( true, _State ) ->
	{ 0, { 0, unbounded } };

validate_iteration( InitialCount, State ) when not is_tuple( InitialCount ) ->
	validate_iteration( { InitialCount, _MaxPortCount=unbounded }, State ).



% (helper)
validate_port_count( PortCount ) when is_integer( PortCount )
									  andalso PortCount >= 0 ->
	PortCount;

validate_port_count( PortCount ) ->
	throw( { invalid_iteration_port_count, PortCount } ).


% No get_iteration/2 applies here.



% Validates a user-specified port comment (expecting a string() or 'undefined'),
% and returns a correct internal form thereof.
%
-spec validate_comment( basic_utils:user_data(), wooper:state() ) ->
							     internal_comment().
validate_comment( _Comment=undefined, _State ) ->
	undefined;

validate_comment( Comment, State ) ->

	case text_utils:is_string( Comment ) of

		true ->
			text_utils:string_to_binary( Comment );

		false ->
			?error_fmt( "The port comment '~p' is invalid (not a string), "
						"thus rejected.", [ Comment ] ),
			throw( { invalid_port_comment, Comment } )

	end.



% Validates user-specified result settings, telling whether this port is to
% produce results (expecting a boolean()) and returns a correct internal form
% thereof.
%
-spec validate_result_settings( basic_utils:user_data(), wooper:state() ) ->
									    boolean().
validate_result_settings( V, _State ) when is_boolean( V ) ->
	get_result_settings( V );

validate_result_settings( Other, State ) ->
	?error_fmt( "The result producer settings ('~p') for output port is "
				"invalid (not a boolean), thus rejected.", [ Other ] ),
	throw( { invalid_result_producer_setting, Other } ).



% Returns directly the corresponding result producer settings.
-spec get_result_settings( boolean() ) -> boolean().
get_result_settings( ResultSettings ) ->
	ResultSettings.



% Validates user-specified semantics (expecting a user_value_semantics(), i.e. a
% list of plain strings) and returns the internal counterpart form thereof.
%
-spec validate_semantics( basic_utils:user_data(), semantic_server_pid(),
						  wooper:state() ) -> value_semantics().
validate_semantics( Semantics, SemanticServerPid, State )
  when is_list( Semantics ) ->

	% Check mostly added because semantics used to be strings, not list of
	% strings:
	%
	BinSemantics = case text_utils:are_strings( Semantics ) of

		true ->
			text_utils:strings_to_binaries( Semantics );

		false ->
			case Semantics of

				List when is_list( List ) ->
					throw( { invalid_semantics, non_string_elements, List } );

				Other ->
					throw( { invalid_semantics, must_be_a_list, Other } )

			end

	end,

	SemanticServerPid ! { validateSemantics, [ BinSemantics ], self() },

	% No easy interleaving here:
	receive

		{ wooper_result, semantics_accepted } ->
			set_utils:from_list( BinSemantics );

		{ wooper_result, { semantics_rejected, Reason } } ->
			?error_fmt( "Port semantics '~p' rejected: ~p.",
						[ Semantics, Reason ] ),
			throw( { rejected_semantics, Semantics, Reason } )

	end;

validate_semantics( Semantics, _SemanticServerPid, State ) ->
	?error_fmt( "Invalid type for port semantics '~p' (not a list), "
				"thus rejected.", [ Semantics ] ),
	throw( { invalid_type_for_port_semantics, Semantics } ).




% Validates user-specified unit (expecting a unit_utils:unit_string()) and
% returns the corresponding internal form.
%
-spec validate_unit( basic_utils:user_data(), wooper:state() ) -> value_unit().
validate_unit( Unit, State ) ->

	case text_utils:is_non_empty_string( Unit ) of

		true ->
			BinString = text_utils:string_to_binary( Unit ),
			ActualUnit = try

				get_unit( Unit )

			catch

				E ->
					% Constraints relaxed from the moment, replacing faulty unit
					% by a placeholder:

					PlaceholderUnit =
						unit_utils:parse_unit( "invalid unit specified" ),

					?warning_fmt( "Parsing of port unit '~ts' failed: '~p', "
						"injecting instead unit '~ts'.",
						[ Unit, E, 
						  unit_utils:unit_to_string( PlaceholderUnit ) ] ),

					%throw( { port_unit_parsing_failed, Unit, E } )

					PlaceholderUnit

			end,

			%trace_utils:debug_fmt( "Parsed unit '~ts' as '~ts'.",
			%		   [ Unit, unit_utils:unit_to_string( ActualUnit ) ] ),
			{ BinString, ActualUnit };

		false ->
			?error_fmt( "Invalid port unit specified ('~p', i.e. not a "
						"non-empty string), thus rejected.", [ Unit ] ),
			throw( { invalid_port_unit_specified, Unit } )

	end.



% Returns directly the corresponding unit.
-spec get_unit( unit_utils:unit_string() ) -> value_unit().
get_unit( UnitString ) ->
	%trace_utils:debug_fmt( "Parsing unit '~ts'.", [ UnitString ] ),
	unit_utils:parse_unit( UnitString ).



% Validates the user-specified described type (expecting a
% value_type_description()), and returns its corresponding internal form.
%
-spec validate_type_description( basic_utils:user_data(), type_server_pid(),
								 wooper:state() ) -> value_type().
validate_type_description( _TypeDescription, _TypeServerPid, _State ) ->

	% Not implemented yet; will parse as much as possible the (textual) type
	% description, try to resolve it locally, and, only if necessary (typically
	% if depending on types that are not known locally), will request the type
	% server to resolve it instead.

	%TypeServerPid ! { getType...

	any.



% Validates the user-specified described type (expecting a list of
% value_constraint()), and returns its corresponding internal form.
%
-spec validate_constraints( basic_utils:user_data(), wooper:state() ) ->
								  value_type().
validate_constraints( Constraints, State ) when is_list( Constraints ) ->
	[ validate_constraint( C, State ) || C <- Constraints ];

validate_constraints( Constraints, State ) ->
	?error_fmt( "Invalid constraints specified ('~p', i.e. not a list), "
				"thus rejected.", [ Constraints ] ),
	throw( { invalid_constraints_type, Constraints } ).



% Ideally we should ensure that the constraints can apply to the type at hand
% (ex: 'greater_than' meaningless for atoms here) and to the bounds (ex:
% N=foobar).
%
validate_constraint( C={ greater_than, N }, _State ) when is_number( N ) ->
	C;

validate_constraint( C={ lower_than, N }, _State ) when is_number( N ) ->
	C;

validate_constraint( C={ between, A, B }, _State ) when is_number( A )
						  andalso is_number( B ) andalso A < B ->
	C;

validate_constraint( C={ in, L }, _State ) when is_list( L ) ->
	C;

validate_constraint( C=positive, _State ) ->
	C;

validate_constraint( C=strictly_positive, _State ) ->
	C;

validate_constraint( C=negative, _State ) ->
	C;

validate_constraint( C=strictly_negative, _State ) ->
	C;

validate_constraint( C=non_null, _State ) ->
	C;

validate_constraint( C, State ) ->
	?error_fmt( "Unknown constraint specified ('~p'), thus rejected; maybe it "
				"was provided with wrong parameters?", [ C ] ),
	throw( { invalid_constraint, C } ).




% Connects the specified output port of the specified upstream block to the
% specified, local, (standard) input port.
%
% Allows to factor code between:
%
% - initial connections, for which InitiatorInfo is the caller PID, typically
% the PID of the upstream block (for synchronicity)
%
% - connections in the course of the simulation, for which InitiatorInfo is the
% 'direct' atom (called from an actor oneway, hence the actor sender is already
% available here)
%
% (helper)
%
-spec connect_to_input_port( input_port_name(), input_port_table(),
			block_pid(), output_port_name(), port_description(),
			pid() | 'direct', wooper:state() ) ->
								   { input_port_table(), wooper:state() }.
connect_to_input_port( InputPortBinName, InputPortTable,
					   UpstreamBlockPid, OutputPortBinName,
					   OutputPortDescription, InitiatorInfo, State ) ->

	% Will be the target endpoint of the channel:
	InputPort = case table:lookup_entry( InputPortBinName, InputPortTable ) of

		{ value, IPort } ->
			IPort;

		key_not_found ->
			?error_fmt( "Request to connect a non-existing input port '~ts', "
						"whereas ~ts", [ InputPortBinName,
										list_input_ports( InputPortTable ) ] ),

			throw( { input_port_not_found,
					 text_utils:binary_to_string( InputPortBinName ) } )

	end,

	OutputPortId = { UpstreamBlockPid, OutputPortBinName },
	InputPortId = { self(), InputPortBinName },

	case InputPort#input_port.feeder_port of

		undefined ->
			ok;

		% Error case, as this input port is already connected:
		FeederPortId={ CurrentUpstreamBlockPid, CurrentOutputPortBinName } ->

			% Useful to be able to factor this helper:
			InitiatorString = case InitiatorInfo of

				direct ->
					text_utils:format( "upstream block ~w to connect its "
							"output port '~ts'",
							[ UpstreamBlockPid, OutputPortBinName ] );

				CallerPid ->
					text_utils:format(
					  "~w to connect the output port '~ts' of block ~w",
					  [ CallerPid, OutputPortBinName, UpstreamBlockPid ] )

			end,

			?error_fmt( "Request from ~ts to local input port '~ts', whereas "
						"this latter is already connected (to the output "
						"port '~ts' of block ~w).",
						[ InitiatorString, InputPortBinName,
						  CurrentOutputPortBinName, CurrentUpstreamBlockPid ] ),

			% Here, input port specified, then { currently linked output one,
			% requested new one }:
			throw( { input_port_already_connected, InputPortId,
					 { FeederPortId, OutputPortId } } )

	end,


	% Checking now that the port descriptions match:
	case can_be_connected( OutputPortDescription, OutputPortBinName,
						   InputPort, InputPortBinName ) of

		true ->
			?info_fmt( "Connection of (remote) output port ~w:'~ts' to "
						"(local) input port '~ts' granted.",
						[ UpstreamBlockPid, OutputPortBinName,
						  InputPortBinName ] );

		{ true, WarningMsg } ->
			% Temporarily silenced a bit:
			%?warning_fmt
			?debug_fmt( "Connection from (remote) output port ~w:'~ts' to "
						  "(local) input port '~ts' accepted, yet ~ts.",
						  [ UpstreamBlockPid, OutputPortBinName,
							InputPortBinName, WarningMsg ] );

		{ false, Reason } ->
			?error_fmt( "Error, connection attempt from (remote) output "
						"port ~w:'~ts' to (local) input port '~ts' rejected "
						"(remote ~ts); reason:~n  ~p",
						[ UpstreamBlockPid, OutputPortBinName, InputPortBinName,
						  port_description_to_string( OutputPortDescription ),
						  Reason ] ),
			throw( { port_connection_rejected, Reason, OutputPortId,
					 InputPortId } )

	end,

	% Not updating the value_status yet:
	NewInputPort = InputPort#input_port{ feeder_port=OutputPortId },

	NewInputPortTable = table:update_entry( InputPortBinName, NewInputPort,
										   InputPortTable ),

	InputState = setAttribute( State, input_ports, NewInputPortTable ),

	?info_fmt( "Input port '~ts' now fed by output port '~ts' of "
				"upstream block ~w.",
				[ InputPortBinName, OutputPortBinName, UpstreamBlockPid ] ),

	% Now examinig whether an output value shall be resent; depends on whether
	% that output port has already been set in the past (and whether the
	% upstream is suspended):

	NewStatus = OutputPortDescription#port_description.status,

	% If the newer port status is 'set', and if the simulation is already
	% running (Initiator is 'direct' - otherwise by design these initial blocks
	% are suspended), then this connection may trigger an activation of this
	% downstream block:
	%
	% (note that the status in this port description will be 'unset' if the
	% upstream block is suspended, regardless of any actual value it may hold)

	case { InitiatorInfo, NewStatus } of

		{ direct, { set, Value } } ->

			ChannelValue = create_channel_value_for_input_port( Value,
																InputPort ),

			EnablePortReemission = true,

			case EnablePortReemission of

				true ->

					RunStatus = ?getAttr(run_status),

					case RunStatus of

						suspended ->
							% Do not trigger premature activations:
							?debug( "No re-emission, as this downstream block "
									"is suspended." ),
							InputState;

						_ ->

							% Will be set to info_fmt/2 when awareness of
							% re-emission will be sufficient:
							?warning_fmt( "Connecting the output port '~ts' "
								  "of upstream block ~w to local input "
								  "port '~ts' led to re-emitting a past "
								  "value (~ts) held by that output port "
								  "(run status: ~p).",
								  [ OutputPortBinName, UpstreamBlockPid,
									InputPortBinName,
									value_to_string( ChannelValue ),
									RunStatus ] ),

						   % As overridden by the DataflowObject,
						   % ProcessingUnit, etc. classes:
						   %
						   executeOneway( InputState, notifyNewInput,
							 [ InputPortBinName, ChannelValue,
							   UpstreamBlockPid ] )

					end;


				false ->
					?warning_fmt( "Port re-emission on connection is currently "
								  "disabled, otherwise a past value (~ts) held "
								  "by the output port '~ts' of upstream "
								  "block ~w would have been assigned to local "
								  "input port '~ts'.",
								  [ value_to_string( ChannelValue ),
									OutputPortBinName, UpstreamBlockPid,
									InputPortBinName ] ),
					InputState

			end;

		_ ->
			InputState

	end.



% Creates, from the specified raw value, a channel value that is suitable for
% the specified input port.
%
% (helper, for internal use only)
%
-spec create_channel_value_for_input_port( actual_value(), input_port() ) ->
												 channel_value().
create_channel_value_for_input_port( ActualValue, #input_port{
													value_semantics=Semantics,
													value_unit=Unit,
													value_type=ActualType } ) ->
	class_Dataflow:create_direct_channel_value( ActualValue, Semantics, Unit,
												ActualType ).



% Tells whether an (upstream) output port, described as specified, can be
% connected to the specified input port.
%
% Note: constraints are currently ignored here (only enforced at value-feeding
% time, not at port-connecting time), yet could also be checked at least to some
% extent, as some may be statically incompatible ('lower than X' versus 'higher
% than X+1').
%
-spec can_be_connected( port_description(), output_port_name(),
						input_port(), input_port_name() ) ->
				  'true' | { 'true', string() } | { 'false', term() }.
can_be_connected( OutputPortDescription=#port_description{
							semantics=OutputPortSemantics,
							unit={ OutputPortBinUnit, OutputPortCanonicalUnit },
							type=OutputPortType,
							constraints=_OutputPortConstraints
							% status field of no use here
										  },
				  OutputPortBinName,
				  InputPort=#input_port{
							   value_semantics=InputPortSemantics,
							   value_unit={ InputPortBinUnit,
											InputPortCanonicalUnit },
							   value_type=InputPortType,
							   value_constraints=_InputPortConstraints },
				  InputPortBinName ) ->

	% Currently, for a channel connection to be acknowledged, only the units and
	% the types have to be compatible.
	%
	% A warning (only) will be issued should semantics not be compliant.
	%
	% The constraints as such are ignored here (yet will be checked on a
	% per-value basis).
	%
	case check_semantics( OutputPortSemantics, InputPortSemantics,
						  OutputPortDescription, OutputPortBinName,
						  InputPort, InputPortBinName ) of

		SemRejected={ false, _SemRefusedTerm } ->
			SemRejected;

		% Either 'true' or { 'true', WarningMsg }:
		SemAccepted ->

			case unit_utils:are_units_identical( OutputPortCanonicalUnit,
												 InputPortCanonicalUnit ) of

				true ->
					case type_utils:are_types_identical( OutputPortType,
														 InputPortType ) of

						true ->
							SemAccepted;

						false ->
							{ false, { unmatching_types, OutputPortType,
									   InputPortType } }

					end;

				false ->
					 { false,
					   { unmatching_units,
						 text_utils:binary_to_string( OutputPortBinUnit ),
						 text_utils:binary_to_string( InputPortBinUnit ) } }

			end

	end.



% Checks that the semantics in the specified output port description are
% compliant with the ones of the specified input port, so that the former may
% feed the latter.
%
-spec check_semantics( value_semantics(), value_semantics(), port_description(),
					   output_port_name(), input_port(), input_port_name() ) ->
						 'true' | { 'true', string() } | { 'false', term() }.
check_semantics( OutputPortSemantics, InputPortSemantics, OutputPortDescription,
				 OutputPortBinName, InputPort, InputPortBinName ) ->

	case class_SemanticServer:are_semantics_compliant(
		   _Emitter=OutputPortSemantics, _Receiver=InputPortSemantics ) of

		true ->
			true;

		false ->

			OutputSemString = rdf_utils:vocabulary_to_string(
								OutputPortSemantics ),

			InputSemString = rdf_utils:vocabulary_to_string(
							   InputPortSemantics ),

			NotCoveredSet = set_utils:difference( InputPortSemantics,
												  OutputPortSemantics ),

			NotCoveredSemList = set_utils:to_list( NotCoveredSet ),

			LackString = text_utils:binaries_to_sorted_string(
						   NotCoveredSemList ),

			% Returns a warning string, but currently still accepts the
			% semantics (as a result they are always deemed compliant here,
			% value-wise):
			%
			WarningMsg = text_utils:format(
					"the semantics for upstream output port '~ts' are not "
					"compliant with the ones of the local input port '~ts', "
					"since following ~B semantics are lacking: ~ts~n"
					"More precisely, regarding output, a ~ts~n would have "
					"been connected to an ~ts.~n~nKnowing that the output "
					"port has a ~ts~nand the input port has a ~ts",
					[ OutputPortBinName, InputPortBinName,
					  length( NotCoveredSemList ), LackString,
					  port_description_to_string( OutputPortDescription ),
					  input_port_to_string( InputPortBinName, InputPort ),
					  OutputSemString, InputSemString ] ),

			{ true, WarningMsg }

	end.




% Section for checking.



% Ensures that any class-level information about semantics and types matches the
% ones for the ports actually specified and created.
%
% Note: the same could be done for iteration ports (as they may have no initial
% iterated port).

% (helper)
%
-spec check_static_consistency( input_port_table(), output_port_table(),
								wooper:state() ) -> void().
check_static_consistency( InputTable, OutputTable, State ) ->

	ActualClassname = wooper:get_classname( State ),

	% Actual port semantics will be checked against static ones iff the latter
	% ones have been defined:
	%
	case get_declared_semantics( ActualClassname ) of

		no_semantics_declared ->
			ok;

		UserVocabulary ->

			% Thus we can check:

			Vocabulary = class_SemanticServer:transform_as_internal(
						   UserVocabulary ),

			case check_input_semantics( Vocabulary, InputTable ) of

				ok ->
					ok;

				{ IPortName, IPortSemantics } ->
					?error_fmt( "The '~ts' input port declared the ~p "
								"semantics, whereas they do not belong to "
								"the explicitly and statically declared "
								"vocabulary, which is: ~ts",
								[ IPortName, IPortSemantics,
								  text_utils:atoms_to_string( Vocabulary ) ] ),
					throw( { unexpected_semantics, IPortSemantics,
							 text_utils:binary_to_string( IPortName ),
							 Vocabulary } )

			end,

			case check_output_semantics( Vocabulary, OutputTable ) of

				ok ->
					ok;

				{ OPortName, OPortSemantics } ->
					?error_fmt( "The '~ts' output port declared the '~ts' "
								"semantics, whereas it does not belong to "
								"the explicitly and statically declared "
								"vocabulary, which is: ~ts",
								[ OPortName, OPortSemantics,
								  text_utils:atoms_to_string( Vocabulary ) ] ),
					throw( { unexpected_semantics, OPortSemantics,
							 text_utils:binary_to_string( OPortName ),
							 Vocabulary } )

			end

	end,

	% Later, types will be checked as well, but at the level of the type server,
	% which is the best placed to resolve all types.

	ok.



% Checks that the semantics used by all input ports in the specified table are
% listed in the specified vocabulary.
%
-spec check_input_semantics( class_SemanticServer:vocabulary(),
		input_port_table() ) ->
					 'ok' | { input_port_name(), value_semantics() }.
check_input_semantics( Vocabulary, PortTable ) ->

	% { Name, Semantics } pairs, to handle input and output ports identically:
	Pairs = [ { Name, Port#input_port.value_semantics }
			  || { Name, Port } <- table:enumerate( PortTable ) ],

	check_pair_semantics( Pairs, Vocabulary ).



% Checks that the semantics used by all output ports in the specified table are
% listed in the specified vocabulary.
%
-spec check_output_semantics( class_SemanticServer:vocabulary(),
		output_port_table() ) ->
					  'ok' | { output_port_name(), value_semantics() }.
check_output_semantics( Vocabulary, PortTable ) ->

	% { Name, Semantics } pairs, to handle output and output ports identically:
	Pairs = [ { Name, Port#output_port.value_semantics }
			  || { Name, Port } <- table:enumerate( PortTable ) ],

	check_pair_semantics( Pairs, Vocabulary ).



-spec check_pair_semantics( [ { port_name(), value_semantics() } ],
		class_SemanticServer:vocabulary() ) ->
								  'ok' | { port_name(), value_semantics() }.
check_pair_semantics( _Pairs=[], _Vocabulary ) ->
	ok;

check_pair_semantics( _Pairs=[ E={ _PortName, PortSemantics } | T ],
					  Vocabulary ) ->
	% Checks that these port semantics are all included in the vocabulary:
	case set_utils:is_subset( PortSemantics, Vocabulary ) of

		true ->
			check_pair_semantics( T, Vocabulary );

		false ->
			E

	end.




% Section for the checking of values against constraints.



% Tells whether specified value satisfies the specified constraints.
-spec satisfies_constraints( actual_value(), value_constraints() ) ->
								   'true' | { 'false', value_constraint() }.
satisfies_constraints( _Value, _Constraints=[] ) ->
	true;

satisfies_constraints( Value, _Constraints=[ C | T ] ) ->

	case satisfies_constraint( Value, C ) of

		true ->
			satisfies_constraints( Value, T );

		RejectDiagnosis -> % { false, Diagnosis }
			RejectDiagnosis

	end.



% Checks that the specified value complies with the specified constraint (tries
% to be relatively conservative, i.e. more prone to returning false than true).
%
satisfies_constraint( Value, _Constraint={ greater_than, N } )
  when is_number( Value ) andalso Value >= N ->
	true;

satisfies_constraint( _Value, Constraint={ greater_than, _N } ) ->
	{ false, Constraint };

satisfies_constraint( Value, _Constraint={ lower_than, N } )
  when is_number( Value ) andalso Value =< N ->
	true;

satisfies_constraint( _Value, Constraint={ lower_than, _N } ) ->
	{ false, Constraint };

satisfies_constraint( Value, _Constraint={ between, A, B } )
  when is_number( Value ) andalso Value >= A andalso Value =< B ->
	true ;

satisfies_constraint( _Value, Constraint={ between, _A, _B } ) ->
	{ false, Constraint };

satisfies_constraint( Value, Constraint={ in, List } ) ->
	case lists:member( text_utils:ensure_string( Value ), List ) of

		true ->
			true;

		false ->
			{ false, Constraint }

	end;

satisfies_constraint( Value, _Constraint=positive )
  when is_number( Value ) andalso Value >= 0 ->
	true;

satisfies_constraint( _Value, Constraint=positive ) ->
	{ false, Constraint };

satisfies_constraint( Value, _Constraint=strictly_positive )
  when is_number( Value ) andalso Value > 0 ->
	true;

satisfies_constraint( _Value, Constraint=strictly_positive ) ->
	{ false, Constraint };

satisfies_constraint( Value, _Constraint=negative )
  when is_number( Value ) andalso Value =< 0 ->
	true;

satisfies_constraint( _Value, Constraint=negative ) ->
	{ false, Constraint };

satisfies_constraint( Value, _Constraint=strictly_negative )
  when is_number( Value ) andalso Value < 0 ->
	true;

satisfies_constraint( _Value, Constraint=strictly_negative ) ->
	{ false, Constraint };

satisfies_constraint( Value, _Constraint=non_null )
  when is_number( Value ) andalso Value =/= 0 ->
	true;

satisfies_constraint( undefined, Constraint=non_null ) ->
	{ false, Constraint };

satisfies_constraint( _Value, _Constraint=non_null ) ->
	true;

satisfies_constraint( _Value, Constraint ) ->
	throw( { unsupported_constraint, Constraint } ).




% Returns a list of the input ports that are currently set, along with the
% corresponding values.
%
% (const helper)
%
-spec get_input_entries( wooper:state() ) ->
								[ { input_port_name(), actual_value() } ].
get_input_entries( State ) ->

	InputPortPairs = table:enumerate( ?getAttr(input_ports) ),

	% Keep only the set ports:
	[ { InputPortName, Value } || { InputPortName,
		   #input_port{ value_status={ set, Value } } } <- InputPortPairs ].



% Returns a list of the output ports that are currently set, along with the
% corresponding values.
%
% (const helper)
%
-spec get_output_entries( wooper:state() ) ->
								[ { output_port_name(), actual_value() } ].
get_output_entries( State ) ->

	OutputPortPairs = table:enumerate( ?getAttr(output_ports) ),

	% Keep only the set ports:
	[ { OutputPortName, Value } || { OutputPortName,
		   #output_port{ value_status={ set, Value } } } <- OutputPortPairs ].







% Section for turning data declared by block modellers (as proplists) into
% appropriate records.



% Turns a {key,value} table into an input_port_spec() record.
-spec parse_raw_input_port_spec( [ { atom(), term() } ] ) -> input_port_spec().
parse_raw_input_port_spec( RawInputPortSpec ) ->

	% Loads the specified pairs in a table, for an easier look-up (actually a
	% no-op):
	%
	IPSTable = list_table:new( RawInputPortSpec ),

	% Tries to assign all the input_port_spec fields from the specified pairs:
	#input_port_spec{
	   name=list_table:get_value( input_port_name, IPSTable ),
	   comment=list_table:get_value( comment, IPSTable ),
	   is_iteration=list_table:get_value( is_iteration, IPSTable ),
	   value_semantics=list_table:get_value( value_semantics, IPSTable ),
	   value_unit=list_table:get_value( value_unit, IPSTable ),
	   value_type_description=list_table:get_value( value_type_description,
												   IPSTable ),
	   value_constraints=list_table:get_value( value_constraints, IPSTable ) }.



% Turns a {key,value} table into an output_port_spec() record.
-spec parse_raw_output_port_spec( [ { atom(), term() } ] ) ->
										output_port_spec().
parse_raw_output_port_spec( RawOutputPortSpec ) ->

	% Loads the specified pairs in a table, for an easier look-up (actually it
	% is a no-op):
	%
	OPSTable = list_table:new( RawOutputPortSpec ),

	% Tries to assign all the output_port_spec fields from the specified pairs:
	#output_port_spec{
	   name=list_table:get_value( output_port_name, OPSTable ),
	   comment=list_table:get_value( comment, OPSTable ),
	   is_iteration=list_table:get_value( is_iteration, OPSTable ),
	   produces_result=list_table:get_value_with_defaults( produces_result,
														false, OPSTable ),
	   value_semantics=list_table:get_value( value_semantics, OPSTable ),
	   value_unit=list_table:get_value( value_unit, OPSTable ),
	   value_type_description=list_table:get_value( value_type_description,
												   OPSTable ),
	   value_constraints=list_table:get_value( value_constraints, OPSTable ) }.




% Section for textual descriptions (to_string).



% Section for the textual description of first-order elements (dataflow, ports,
% iterations, etc.)



% Returns a textual description of this input port specification.
%
% Made not to crash even in the presence of faulty field entries.
%
% (helper)
%
-spec input_port_spec_to_string( input_port_spec() ) -> string().
input_port_spec_to_string( #input_port_spec{
							  name=Name,
							  comment=Comment,
							  is_iteration=false,
							  value_semantics=Semantics,
							  value_unit=UnitString,
							  value_type_description=TypeDescription,
							  value_constraints=Constraints } ) ->
	% Not an iteration:
	text_utils:format( "specification for a standard input port named '~ts' "
					   "(comment: '~ts'), of semantics '~p', "
					   "user-defined unit '~ts', type description '~ts' and "
					   "constraints '~p'.",
					   [ Name, Comment, Semantics, UnitString,
						 TypeDescription, Constraints ] );


input_port_spec_to_string( Spec=#input_port_spec{ is_iteration=true } ) ->
	input_port_spec_to_string( Spec#input_port_spec{ is_iteration=0 } );


input_port_spec_to_string( #input_port_spec{
							  name=Name,
							  comment=Comment,
							  is_iteration=Iteration,
							  value_semantics=Semantics,
							  value_unit=UnitString,
							  value_type_description=TypeDescription,
							  value_constraints=Constraints } ) ->
	% Iteration here:
	IterationString = iteration_spec_to_string( Iteration ),

	text_utils:format( "specification for an input port iteration named '~ts' ~ts"
					   " (comment: '~ts'), of semantics '~p', user-defined unit "
					   "'~ts', type description '~ts' and constraints '~p'.",
					   [ Name, IterationString, Comment, Semantics,
						 UnitString, TypeDescription, Constraints ] ).



% Returns a textual description of this output port specification.
%
% Made not to crash even in the presence of faulty field entries.
%
% (helper)
%
-spec output_port_spec_to_string( output_port_spec() ) -> string().
output_port_spec_to_string( #output_port_spec{
							  name=Name,
							  comment=Comment,
							  produces_result=IsProducingResult,
							  is_iteration=false,
							  value_semantics=Semantics,
							  value_unit=UnitString,
							  value_type_description=TypeDescription,
							  value_constraints=Constraints } ) ->

	ResultString = case IsProducingResult of

		true ->
			"producing results";

		false ->
			"not producing results"

	end,

	% Not an iteration:
	text_utils:format( "specification for a standard output port named '~ts' "
					   "(comment: '~ts'), ~ts, of semantics '~p', user-defined "
					   "unit '~ts', type description '~ts' and constraints '~p'.",
					   [ Name, Comment, ResultString, Semantics, UnitString,
						 TypeDescription, Constraints ] );


output_port_spec_to_string( Spec=#output_port_spec{ is_iteration=true } ) ->
	output_port_spec_to_string( Spec#output_port_spec{ is_iteration=0 } );


output_port_spec_to_string( #output_port_spec{
							  name=Name,
							  comment=Comment,
							  produces_result=IsProducingResult,
							  is_iteration=Iteration,
							  value_semantics=Semantics,
							  value_unit=UnitString,
							  value_type_description=TypeDescription,
							  value_constraints=Constraints } ) ->
	% Iteration here:

	ResultString = case IsProducingResult of

		true ->
			"producing results";

		false ->
			"not producing results"

	end,

	IterationString = iteration_spec_to_string( Iteration ),

	text_utils:format( "specification for an output port iteration named '~ts' "
					   "~ts (comment: '~ts'), ~ts, of semantics '~p', "
					   "user-defined unit '~ts', type description '~ts' and "
					   "constraints '~p'.",
					   [ Name, IterationString, Comment, ResultString,
						 Semantics, UnitString, TypeDescription,
						 Constraints ] ).



% Returns a textual description of this iteration specification.
%
% (const helper)
%
-spec iteration_spec_to_string( iteration_spec() ) -> string().
% Not possible here: iteration_spec_to_string( false ) -> ...
iteration_spec_to_string( { PortCount, { MinPortCount, unbounded } } ) ->
	text_utils:format( "including ~B initial iterated ports (minimal number "
					   "thereof: ~B, no maximum defined)",
					   [ PortCount, MinPortCount ] );

iteration_spec_to_string( { PortCount, { MinPortCount, MaxPortCount } } ) ->
	text_utils:format( "including ~B initial iterated ports (minimal number "
					   "thereof: ~B, maximum: ~B)",
					   [ PortCount, MinPortCount, MaxPortCount ] );

iteration_spec_to_string( { PortCount, MaxPortCount } ) ->
	iteration_spec_to_string( { PortCount, { 0, MaxPortCount } } );

iteration_spec_to_string( PortCount ) when is_integer( PortCount ) ->
	iteration_spec_to_string( { PortCount, { 0, unbounded } } );

% Not a licit value for iteration_spec():
%iteration_spec_to_string( false ) ->
%	"not a port iteration";

iteration_spec_to_string( Other ) ->
	%text_utils:format( "(invalid iteration specification: '~p')",
	%				   [ Other ] ).
	throw( { invalid_iteration_specification, Other } ).



% Returns a textual description of this dataflow block.
%
% (const helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	LinkString = text_utils:format( "registered in dataflow ~p, using the "
									"semantic server ~p and the type server ~p",
									[ ?getAttr(dataflow_pid),
									  ?getAttr(semantic_server_pid),
									  ?getAttr(type_server_pid) ] ),

	{ InputDetailed, OutputDetailed } = io_to_string( State ),

	text_utils:format( "dataflow block named '~ts', ~ts, having ~tsand ~ts~ts",
					   [ ?getAttr(name), ?getAttr(run_status), InputDetailed,
						 OutputDetailed, LinkString ] ).



% Returns a textual description (as a pair of strings) respectively of the
% inputs and outputs (hence, as ports) of this dataflow block.
%
% (const helper)
%
-spec io_to_string( wooper:state() ) -> { string(), string() }.
io_to_string( State ) ->
	io_to_string( _IndentationLevel=0, State ).



% Returns a textual description (as a pair of strings) respectively of the
% inputs and outputs (ports) of this block, at specified indentation level.
%
% (const helper)
%
-spec io_to_string( text_utils:indentation_level(), wooper:state() ) ->
						  { string(), string() }.
io_to_string( IndentationLevel, State ) ->

	InputPorts = ?getAttr(input_ports),

	% Alphabetically, by port name:
	SortedInputs = lists:sort( table:enumerate( InputPorts ) ),

	InputDetailed = case SortedInputs of

		[] ->
			"no input port ";

		[ { SingleIName, SingleIDesc } ] ->
			text_utils:format( "a single ~ts ", [ input_port_to_string(
						 SingleIName, SingleIDesc, IndentationLevel+1 ) ] );

		_ ->
			InputStrings = [ input_port_to_string( IName, IPort,
												   IndentationLevel+1 )
							 || { IName, IPort } <- SortedInputs ],
			InputDesc = text_utils:strings_to_string( InputStrings ),
			text_utils:format( "~B input ports: ~ts",
							   [ length( SortedInputs ), InputDesc ] )

	end,

	OutputPorts = ?getAttr(output_ports),

	% Alphabetically, by port name:
	SortedOutputs = lists:sort( table:enumerate( OutputPorts ) ),

	OutputDetailed = case SortedOutputs of

		[] ->
			"no output port ";

		[ { SingleOName, SingleODesc } ] ->
			text_utils:format( "a single ~ts ", [ output_port_to_string(
						 SingleOName, SingleODesc, IndentationLevel+1 ) ] );

		_ ->
			OutputStrings = [ output_port_to_string( OName, OPort,
													 IndentationLevel+1 )
							  || { OName, OPort } <- SortedOutputs ],
			OutputDesc = text_utils:strings_to_string( OutputStrings ),
			text_utils:format( "~B output ports: ~ts",
							   [ length( SortedOutputs ), OutputDesc ] )

	end,

	{ InputDetailed, OutputDetailed }.



% Returns a textual description of specified input port, using a top-level
% (bullet) indentation for that.
%
input_port_to_string( PortName, InputPort ) ->
	% Using default bullet style and identation:
	input_port_to_string( PortName, InputPort, _IndentationLevel=0 ).



% Returns a textual description of specified input port, using specified bullet
% for that.
%
input_port_to_string( PortName, _InputPort=#input_port{
											  comment=Comment,
											  value_semantics=Semantics,
											  value_unit=Unit,
											  value_type=Type,
											  value_constraints=Constraints,
											  value_status=Status,
											  last_receiving=LastTimestamp,
											  feeder_port=Feeder },
					  IndentationLevel ) ->

	CommentString = dataflow_support:comment_to_string( Comment ),

	SemanticString = dataflow_support:semantics_to_string( Semantics,
											   IndentationLevel + 1 ),

	UnitString = dataflow_support:value_unit_to_string( Unit ),

	TypeString = dataflow_support:value_type_to_string( Type ),

	ConstraintString = dataflow_support:value_constraint_to_string( Constraints,
												   IndentationLevel ),

	StatusString = dataflow_support:value_status_to_string( Status ),


	TimeString = case LastTimestamp of

		none ->
			"never having received a value";

		% Most probably an initial block:
		{ undefined, undefined } ->
			"having last received a value before simulation start";

		_ ->
			text_utils:format( "having last received a value at ~p",
							   [ LastTimestamp ] )

	end,

	FeederString = case Feeder of

		undefined ->
			"not fed by an output port";

		{ OutputBlockPid, OutputPortName }  ->
			text_utils:format( "fed by output port ~ts of dataflow block ~p",
							   [ OutputPortName, OutputBlockPid ] )

	end,

	text_utils:format( "input port named '~ts', ~ts, ~ts, expecting values "
					   "~ts, ~ts, ~ts; port is ~ts, ~ts, ~ts",
					   [ PortName, CommentString, SemanticString, UnitString,
						 TypeString, ConstraintString, StatusString, TimeString,
						 FeederString ] ).



% Returns a textual description of specified output port, using a top-level
% (bullet) indentation for that.
%
output_port_to_string( PortName, OutputPort ) ->
	% Using default bullet style and identation:
	output_port_to_string( PortName, OutputPort, _IndentationLevel=0 ).


% Returns a textual description of specified output port, using specified bullet
% for that.
%
output_port_to_string( PortName, _OutputPort=#output_port{
											  comment=Comment,
											  produces_result=ResultSettings,
											  value_semantics=Semantics,
											  value_unit=Unit,
											  value_type=Type,
											  value_constraints=Constraints,
											  value_status=Status,
											  last_sending=LastTimestamp,
											  fed_ports=FedPorts },
					   IndentationLevel ) ->

	CommentString = dataflow_support:comment_to_string( Comment ),

	ResultString = case ResultSettings of

		true ->
			"producing results";

		false ->
			"not producing results"

	end,

	SemanticString = dataflow_support:semantics_to_string( Semantics ),

	UnitString = dataflow_support:value_unit_to_string( Unit ),

	TypeString = dataflow_support:value_type_to_string( Type ),

	ConstraintString = dataflow_support:value_constraint_to_string( Constraints,
															IndentationLevel ),

	StatusString = dataflow_support:value_status_to_string( Status ),

	TimeString = case LastTimestamp of

		none ->
			"never having sent a value";

		send_on_resume ->
			"storing a value that will be sent once resumed";

		_ ->
			text_utils:format( "having last sent a value at ~p",
							   [ LastTimestamp ] )

	end,

	FedString = case FedPorts of

		[] ->
			"not feeding any input port";

		[ { InputBlockPid, InputPortName } ] ->
			text_utils:format( "feeding input port '~ts' of dataflow block ~p",
							   [ InputPortName, InputBlockPid ] );

		_ ->
			FedStrings = [ text_utils:format( "port '~ts' of dataflow block ~p",
					 [ PName, BName ] ) || { BName, PName } <- FedPorts ],

			text_utils:format( "feeding following ~B input ports: ~ts",
							   [ length( FedPorts ),
								 text_utils:strings_to_string( FedStrings ) ] )

	end,

	text_utils:format( "output port named '~ts', ~ts, ~ts, expecting values "
					   "~ts, ~ts, ~ts, ~ts; port is ~ts, ~ts, ~ts",
					   [ PortName, CommentString, SemanticString, ResultString,
						 UnitString, TypeString, ConstraintString, StatusString,
						 TimeString, FedString ] ).



% Returns a textual description of the specified input port iteration.
-spec input_port_iteration_to_string( input_port_iteration() ) -> string().
input_port_iteration_to_string( #input_port_iteration{
								   base_name=BaseName,
								   comment=Comment,
								   multiplicity=Multiplicity,
								   value_semantics=Semantics,
								   value_unit=Unit,
								   value_type=Type,
								   value_constraints=Constraints,
								   port_indexes=Indexes } ) ->

	IndentationLevel = 0,

	CommentString = dataflow_support:comment_to_string( Comment ),

	MultiplicityString = dataflow_support:multiplicity_to_string(
						   Multiplicity ),

	SemanticString = dataflow_support:semantics_to_string( Semantics ),

	UnitString = dataflow_support:value_unit_to_string( Unit ),

	TypeString = dataflow_support:value_type_to_string( Type ),

	ConstraintString = dataflow_support:value_constraint_to_string( Constraints,
													 IndentationLevel ),

	text_utils:format( "input port iteration having for base name '~ts', "
					   "~ts, ~ts, ~ts, ~ts, ~ts, ~ts and following indexes of "
					   "iterated ports: ~w",
					   [ BaseName, CommentString, MultiplicityString,
						 SemanticString, UnitString, TypeString,
						 ConstraintString, Indexes ] ).



% Returns a textual description of the specified output port iteration.
%
-spec output_port_iteration_to_string( output_port_iteration() ) -> string().
output_port_iteration_to_string( #output_port_iteration{
									base_name=BaseName,
									comment=Comment,
									multiplicity=Multiplicity,
									value_semantics=Semantics,
									value_unit=Unit,
									value_type=Type,
									value_constraints=Constraints,
									port_indexes=Indexes } ) ->

	IndentationLevel = 0,

	CommentString = dataflow_support:comment_to_string( Comment ),

	MultiplicityString = dataflow_support:multiplicity_to_string(
						   Multiplicity ),

	SemanticString = dataflow_support:semantics_to_string( Semantics ),

	UnitString = dataflow_support:value_unit_to_string( Unit ),

	TypeString = dataflow_support:value_type_to_string( Type ),

	ConstraintString = dataflow_support:value_constraint_to_string( Constraints,
												   IndentationLevel ),

	text_utils:format( "output port iteration having for base name '~ts', "
					   "~ts, ~ts, ~ts, ~ts, ~ts, ~ts and following indexes of "
					   "iterated ports: ~w",
					   [ BaseName, CommentString, MultiplicityString,
						 SemanticString, UnitString, TypeString,
						 ConstraintString, Indexes ] ).



% Returns a textual description of the specified port description.
-spec port_description_to_string( port_description() ) -> string().
port_description_to_string( PortDescription ) ->
	port_description_to_string( PortDescription, _IndentationLevel=0 ).



% Returns a textual description of the specified port description.
-spec port_description_to_string( port_description(),
								  text_utils:indentation_level() ) -> string().
port_description_to_string( #port_description{
							   semantics=Semantics,
							   unit={ BinUnitString, CanonicalUnit },
							   type=Type,
							   constraints=Constraints,
							   status=Status }, IndentationLevel ) ->

	SemanticString = rdf_utils:vocabulary_to_string( Semantics,
													 IndentationLevel ),

	StatusString = case Status of

		unset ->
			"unset";

		{ set, V } ->
			text_utils:format( "set to value ~p", [ V ] )

	end,

	text_utils:format( "port with, as semantics, ~ts "
					   "and user-defined unit '~ts' (corresponding to ~ts), "
					   "type '~ts', constraints '~w', being currently ~ts",
					   [ SemanticString, BinUnitString,
						 unit_utils:unit_to_string( CanonicalUnit ),
						 type_utils:type_to_description( Type ),
						 Constraints, StatusString ] ).



% Returns a textual description of the specified list of connection information.
-spec inbound_connection_infos_to_string( [ inbound_connection_info() ] ) ->
											   string().
inbound_connection_infos_to_string( ConnInfos ) ->

	Strings = [ inbound_connection_info_to_string( C ) || C <- ConnInfos ],

	text_utils:format( "~B inbound channels "
					   "('remote output'/'local input'): ~ts",
					   [ length( ConnInfos ),
						 text_utils:strings_to_string( Strings ) ] ).



% Returns a textual description of the specified connection information.
-spec inbound_connection_info_to_string( inbound_connection_info() ) ->
											   string().
inbound_connection_info_to_string(
  _ConnInfos={ OutputPortName, OutputPortDesc, DownstreamPortSpec } ) ->

	DownSpecString = class_DataflowUnitManager:downstream_spec_to_string(
				   DownstreamPortSpec ),

	text_utils:format( "from output port '~ts' (~ts) to ~ts", [ OutputPortName,
							port_description_to_string( OutputPortDesc,
														_IndentationLevel=1 ),
							DownSpecString ] ).



% Returns a textual description of the specified port pair informations.
-spec port_pairs_to_string( [ { output_port_name(), input_port_name() } ] ) ->
								  string().
port_pairs_to_string( PortPairs ) ->

	Strings = [ port_pair_to_string( PP ) || PP <- PortPairs ],

	text_utils:strings_to_string( Strings ).



% Returns a textual description of the specified port pair information.
-spec port_pair_to_string( { output_port_name(), input_port_name() } ) ->
								  string().
port_pair_to_string( _PortPair={ OutputPortName, InputPortName } ) ->
	text_utils:format( "from '~ts' to '~ts'", [ OutputPortName, InputPortName ] );

port_pair_to_string( PortPair ) ->
	throw( { invalid_port_pair, PortPair } ).



% Returns a textual description of the specified channel value.
-spec value_to_string( channel_value() ) -> string().
value_to_string( #channel_value{ actual_value=Value,
								 semantics=Semantics,
								 unit={ BinUnitString, CanonicalUnit },
								 type=Type } ) ->

	SemanticString = rdf_utils:vocabulary_to_string( Semantics ),

	text_utils:format( "channel value '~p', having, as semantics, ~ts "
					   "and for unit '~ts' (~ts) and type '~ts'",
					   [ Value, SemanticString, BinUnitString,
						 unit_utils:unit_to_string( CanonicalUnit ), Type ] );

% Probably that at least some metadata are lacking:
value_to_string( #channel_value{ actual_value=Value,
								 semantics=Semantics,
								 unit=Unit,
								 type=Type } ) ->

	SemanticString = rdf_utils:vocabulary_to_string( Semantics ),

	text_utils:format( "invalid channel value '~p', having, as semantics, ~ts, "
					   "and for unit '~p' and type '~p'",
					   [ Value, SemanticString, Unit, Type ] ).
