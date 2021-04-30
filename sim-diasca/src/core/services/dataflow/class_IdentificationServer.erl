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


-module(class_IdentificationServer).


-define( class_description,
		 "Class in charge of maintaining a two-way relationship between "
		 "an external identifier (ex: set by a more global platform) and an "
		 "internal one (i.e. the PID of a dataflow block). "
		 "External identifiers of type string shall better be transmitted as "
		 "binaries. "
		 "Generally instantiated as a singleton. "
		 "Also referred to as the 'ID server'." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).



% Design notes:
%
% This server is not an actor, and other technical components are expected to
% interact with it mostly thanks to (synchronous) requests.
%
% One should ensure that no unsynchronised concurrent access to this server is
% performed in the course of the simulation, not to jeopardise its expected
% properties.



% The identifier bijection is maintained thanks to two synchronised tables:

% Inner table to convert external identifiers into (internal) block PIDs:
-type inbound_table() :: table( external_id(), block_pid() ).


% Inner table to convert (internal) block PIDs into external identifiers:
-type outbound_table() :: table( block_pid(), external_id() ).



% Class-specific attributes:
-define( class_attributes, [

	{ inbound_table, inbound_table(), "a table able to convert external "
	  "identifiers into (internal) block PIDs" },

	{ outbound_table, outbound_table(), "a table able to convert (internal) "
	  "block PIDs into external identifiers" } ] ).


-include("engine_common_defines.hrl").


% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").



% Must be included before class_EngineBaseObject header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.World.Identification" ).


% For registration:
-define( id_server_name, sim_diasca_identification_server ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% For identification_server_pid() and all:
-include("dataflow_defines.hrl").


% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().



% Implementation notes:
%
% We expect look-ups of PIDs to be faster than look-ups of external IDs
% (typically binaries), hence we prioritize the accesses to the outbound table
% over the ones to the inbound table.



% Constructs a new identification server.
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% First the direct mother class:
	TraceState = class_EngineBaseObject:construct( State,
							?trace_categorize("IdentificationServer") ),

	naming_utils:register_as( self(), ?id_server_name, global_only ),

	?send_info( TraceState, "Identification server started." ),

	EmptyTable = table:new(),

	% Then the class-specific actions:
	setAttributes( TraceState, [
		{ inbound_table, EmptyTable },
		{ outbound_table, EmptyTable } ] ).



-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Expected to match:
	InCount = table:size( ?getAttr(inbound_table) ),
	OutCount = table:size( ?getAttr(outbound_table) ),

	?notice_fmt( "Stopping identification server; it knew ~B inbound "
				 "associations and ~B outbound ones.", [ InCount, OutCount ] ),

	State.




% Methods section.



% Declares the specified identifier association.
%
% Multiple declarations for the same identifier pair are allowed, provided that
% they match.
%
% (request, for synchronisation)
%
-spec declareIdentifierAssociation( wooper:state(), block_pid(),
		external_id() ) -> request_return( 'identifier_association_declared' ).
declareIdentifierAssociation( State, BlockPid, ExternalId ) ->

	InboundTable = ?getAttr(inbound_table),
	OutboundTable = ?getAttr(outbound_table),

	{ NewInboundTable, NewOutboundTable } = declare_association( BlockPid,
							 ExternalId, InboundTable, OutboundTable, State ),

	NewState = setAttributes( State, [ { inbound_table, NewInboundTable },
									   { outbound_table, NewOutboundTable } ] ),

	wooper:return_state_result( NewState, identifier_association_declared ).



% Declares specified association.
%
% State is const, only used for traces.
%
-spec declare_association( block_pid(), external_id(), inbound_table(),
  outbound_table(), wooper:state() ) -> { inbound_table(), outbound_table() }.
declare_association( BlockPid, ExternalId, InboundTable, OutboundTable,
					 State ) ->

	?void_fmt( "Identifier association of block PID '~w' to external "
			   "identifier '~p'.~n", [ BlockPid, ExternalId ] ),

	% Tables expected to be consistent, and look-up in outbound probably faster:
	case table:lookup_entry( BlockPid, OutboundTable ) of

		key_not_found ->

			% Not add_entry for inbound, as we are paranoid:
			NewIn = table:add_new_entry( ExternalId, BlockPid, InboundTable ),

			NewOut = table:add_entry( BlockPid, ExternalId, OutboundTable ),

			{ NewIn, NewOut };


		{ value, ExternalId } ->
			% OK, consistent, no more checking:
			{ InboundTable, OutboundTable };


		{ value, OtherExternalId } ->
			?error_fmt( "Error, received an association declaration for block "
				"PID ~w and external identifier '~ts', whereas the external "
				"identifier '~ts' was already registered for this block.",
				[ BlockPid, ExternalId, OtherExternalId ] ),
			throw( { inconsistent_association_declaration, BlockPid,
						{ ExternalId, OtherExternalId } } )

	end.



% Declares the specified identifier associations.
%
% Multiple declarations for the same identifier pair are allowed, provided that
% they match.
%
% (request, for synchronisation)
%
-spec declareIdentifierAssociations( wooper:state(),
							[ { block_pid(), external_id() } ]  ) ->
				 request_return( 'identifier_associations_declared' ).
declareIdentifierAssociations( State, IdPairs ) ->

	InboundTable = ?getAttr(inbound_table),
	OutboundTable = ?getAttr(outbound_table),

	{ NewInboundTable, NewOutboundTable } = declare_associations( IdPairs,
								   InboundTable, OutboundTable, State ),

	NewState = setAttributes( State, [ { inbound_table, NewInboundTable },
									   { outbound_table, NewOutboundTable } ] ),

	wooper:return_state_result( NewState, identifier_associations_declared ).



% Declares specified identifier associations.
%
% State is const, only used for traces.
%
-spec declare_associations( [ { block_pid(), external_id() } ],
				inbound_table(), outbound_table(), wooper:state() ) ->
									{ inbound_table(), outbound_table() }.
declare_associations( _IdPairs=[], InboundTable, OutboundTable, _State ) ->
	{ InboundTable, OutboundTable };

declare_associations( _IdPairs=[ { BlockPid, ExternalId } | T ], InboundTable,
					  OutboundTable, State ) ->

   { NewInboundTable, NewOutboundTable } = declare_association( BlockPid,
							ExternalId, InboundTable, OutboundTable, State ),

	declare_associations( T, NewInboundTable, NewOutboundTable, State ).



% Removes the known identifier association for specified block PID.
%
% Throws if the specified block PID is not known.
%
% (request, for synchronicity)
%
-spec removeIdentifierAssociation( wooper:state(), block_pid() ) ->
				request_return( 'identifier_associations_removed' ).
removeIdentifierAssociation( State, BlockPid ) ->

	OutboundTable = ?getAttr(outbound_table),

	% Better control than extract_entry/2:
	case table:lookup_entry( BlockPid, OutboundTable ) of

		{ value, ExtId } ->

			%?debug_fmt( "The association between the block PID ~w and the "
			%			"external identifier '~p' has been removed.",
			%			[ BlockPid, ExtId ] ),

			NewOutboundTable = table:remove_entry( BlockPid, OutboundTable ),

			NewInboundTable =
				table:remove_entry( ExtId, ?getAttr(inbound_table) ),

			RemovedState = setAttributes( State, [
							{ outbound_table, NewOutboundTable },
							{ inbound_table, NewInboundTable } ] ),

			wooper:return_state_result( RemovedState,
										identifier_associations_removed );

		key_not_found ->
			throw( { unknown_block_pid, BlockPid } )

	end.



% Returns the external identifier corresponding to the specified block PID.
%
% Throws if the specified block PID is not known.
%
-spec getExternalIdentifier( wooper:state(), block_pid() ) ->
									const_request_return( external_id() ).
getExternalIdentifier( State, BlockPid ) ->

	case table:lookup_entry( BlockPid, ?getAttr(outbound_table) ) of

		{ value, ExternalId } ->
			wooper:const_return_result( ExternalId );

		key_not_found ->
			throw( { unknown_block_pid, BlockPid } )

	end.



% Returns the external identifier corresponding to the specified block
% PID.
%
% Returns 'undefined' if the specified block PID is not known.
%
-spec getAnyExternalIdentifier( wooper:state(), block_pid() ) ->
			const_request_return( maybe( external_id() ) ).
getAnyExternalIdentifier( State, BlockPid ) ->

	case table:lookup_entry( BlockPid, ?getAttr(outbound_table) ) of

		{ value, ExternalId } ->
			wooper:const_return_result( ExternalId );

		key_not_found ->
			wooper:const_return_result( undefined )

	end.



% Returns the external identifiers corresponding to the specified block PIDs.
%
% The returned list is in the same order as the input one, i.e. the external
% identifier of a block PID is at the same rank as it was in the list of block
% PIDs.
%
% Throws an exception if a specified block PID is not known.
%
-spec getExternalIdentifiers( wooper:state(), [ block_pid() ] ) ->
									const_request_return( [ external_id() ] ).
getExternalIdentifiers( State, BlockPids ) ->

	OutboundTable = ?getAttr(outbound_table),

	ExtIds = get_external_ids( BlockPids, OutboundTable, State, _Acc=[] ),

	wooper:const_return_result( ExtIds ).



% (helper)
get_external_ids( _BlockPids=[], _OutboundTable, _State, Acc ) ->
	% Preserve the right order:
	lists:reverse( Acc );


get_external_ids( _BlockPids=[ BlockPid | T ], OutboundTable, State, Acc ) ->

	ExtId = case table:lookup_entry( BlockPid, OutboundTable ) of

		{ value, ExternalId } ->
			ExternalId;

		key_not_found ->
			throw( { unknown_block_pid, BlockPid } )

	end,

	get_external_ids( T, OutboundTable, State, [ ExtId | Acc ] ).



% Returns the external identifiers corresponding to the specified block PIDs.
%
% The returned list is in the same order as the input one, i.e. the external
% identifier of a block PID is at the same rank as it was in the list of block
% PIDs.
%
% Returns 'undefined' for any specified block PID that is not known.
%
-spec getAnyExternalIdentifiers( wooper:state(), [ block_pid() ] ) ->
					const_request_return( [ external_id() ] ).
getAnyExternalIdentifiers( State, BlockPids ) ->

	OutboundTable = ?getAttr(outbound_table),

	ExtIds = get_any_external_ids( BlockPids, OutboundTable, State, _Acc=[] ),

	wooper:const_return_result( ExtIds ).



% (helper)
get_any_external_ids( _BlockPids=[], _OutboundTable, _State, Acc ) ->
	% Preserve the right order:
	lists:reverse( Acc );


get_any_external_ids( _BlockPids=[ BlockPid | T ], OutboundTable, State,
					  Acc ) ->

	ExtId = case table:lookup_entry( BlockPid, OutboundTable ) of

		{ value, ExternalId } ->
			ExternalId;

		key_not_found ->
			undefined

	end,

	get_any_external_ids( T, OutboundTable, State, [ ExtId | Acc ] ).




% Returns the block PID corresponding to the specified external identifier.
%
% Throws an exception if the specified external identifier is not known.
%
-spec getBlockPID( wooper:state(), external_id() ) ->
									const_request_return( block_pid() ).
getBlockPID( State, ExternalIdentifier ) ->

	InboundTable = ?getAttr(inbound_table),

	case table:lookup_entry( ExternalIdentifier, InboundTable ) of

		{ value, BlockPid } ->
			wooper:const_return_result( BlockPid );

		key_not_found ->
			% So that WOOPER does not think it is a non-request return:
			wooper:throwing(
			  throw_on_external_id_not_found( ExternalIdentifier, State ) )

	end.



% Returns the block PID corresponding to the specified external identifier.
%
% Returns 'undefined' if the specified external identifier is not known.
%
-spec getAnyBlockPID( wooper:state(), external_id() ) ->
							const_request_return( maybe( block_pid() ) ).
getAnyBlockPID( State, ExternalIdentifier ) ->

	InboundTable = ?getAttr(inbound_table),

	case table:lookup_entry( ExternalIdentifier, InboundTable ) of

		{ value, BlockPid } ->
			wooper:const_return_result( BlockPid );

		key_not_found ->
			wooper:const_return_result( undefined )

	end.



% Returns the block PIDs corresponding to the specified external identifiers.
%
% The returned list is in the same order as the input one, i.e. the block PID of
% an external identifier is at the same rank as it was in the list of external
% identifiers.
%
% Throws an exception if a specified block PID is not known.
%
-spec getBlockPIDs( wooper:state(), [ external_id() ] ) ->
							const_request_return( [ block_pid() ] ).
getBlockPIDs( State, ExternalIdentifiers ) ->

	InboundTable = ?getAttr(inbound_table),

	BlockPids = get_block_pids( ExternalIdentifiers, InboundTable, State,
								_Acc=[] ),

	wooper:const_return_result( BlockPids ).



% (helper)
get_block_pids( _ExtIDs=[], _InboundTable, _State, Acc ) ->
	% Preserve the right order:
	lists:reverse( Acc );


get_block_pids( _ExtIDs=[ ExtID | T ], InboundTable, State, Acc ) ->

	 BlockPid = case table:lookup_entry( ExtID, InboundTable ) of

		{ value, Pid } ->
			Pid;

		key_not_found ->
			throw_on_external_id_not_found( ExtID, State )

	end,

	get_block_pids( T, InboundTable, State, [ BlockPid | Acc ] ).



% Returns the block PIDs corresponding to the specified external identifiers.
%
% The returned list is in the same order as the input one, i.e. the block PID of
% an external identifier is at the same rank as it was in the list of external
% identifiers.
%
% Returns 'undefined' for any specified external identifier that is not known.
%
-spec getAnyBlockPIDs( wooper:state(), [ external_id() ] ) ->
									const_request_return( [ block_pid() ] ).
getAnyBlockPIDs( State, ExternalIdentifiers ) ->

	InboundTable = ?getAttr(inbound_table),

	BlockPids = get_any_block_pids( ExternalIdentifiers, InboundTable, State,
									_Acc=[] ),

	wooper:const_return_result( BlockPids ).



% (helper)
get_any_block_pids( _ExtIDs=[], _InboundTable, _State, Acc ) ->
	% Preserve the right order:
	lists:reverse( Acc );


get_any_block_pids( _ExtIDs=[ ExtID | T ], InboundTable, State, Acc ) ->

	 BlockPid = case table:lookup_entry( ExtID, InboundTable ) of

		{ value, Pid } ->
			Pid;

		key_not_found ->
			undefined

	end,

	get_any_block_pids( T, InboundTable, State, [ BlockPid | Acc ] ).




% Returns the current status of this identification server.
-spec getStatus( wooper:state() ) -> const_request_return( bin_string() ).
getStatus( State ) ->

	Status = text_utils:string_to_binary( to_string( State ) ),

	wooper:const_return_result( Status ).



% Static section.


% Launches the identification server, with default settings.
-spec start() -> static_return( identification_server_pid() ).
start() ->
	wooper:return_static( new_link() ).


% Stops the identification server.
-spec stop() -> static_void_return().
stop() ->
	IdentificationServerPid = get_server(),
	stop( IdentificationServerPid ),
	wooper:return_static_void().


% Stops the specified identification server.
-spec stop( identification_server_pid() ) -> static_void_return().
stop( IdentificationServerPid ) ->
	IdentificationServerPid ! delete,
	wooper:return_static_void().


% Returns the PID of the identification server.
-spec get_server() -> static_return( identification_server_pid() ).
get_server() ->
	Pid = naming_utils:get_registered_pid_for( ?id_server_name, global ),
	wooper:return_static( Pid ).


% Returns the PID of the identification server, if any.
-spec get_any_server() -> static_return( maybe( identification_server_pid() ) ).
get_any_server() ->

	case naming_utils:is_registered( ?id_server_name, global ) of

		not_registered ->
			wooper:return_static( undefined );

		Pid ->
			wooper:return_static( Pid )

	end.



% Creates an external identifier in the form of a binary string, from specified
% block PID.
%
% Returns for example <<"sim-diasca-0.57.0">>.
%
-spec forge_external_identifier( block_pid() ) -> static_return( bin_string() ).
forge_external_identifier( Pid ) when is_pid( Pid ) ->

	ExtIdString = text_utils:format( "sim-diasca-~ts",
									 [ text_utils:pid_to_string( Pid ) ] ),

	wooper:return_static( text_utils:string_to_binary( ExtIdString ) ).




% Helper section:


% Throws an exception because specified external identifier could not be
% resolved.
%
-spec throw_on_external_id_not_found( external_id(), wooper:state() ) ->
											no_return().
throw_on_external_id_not_found( ExternalIdentifier, State ) ->

	% Should they be not textual already:
	ExtIdStrings = [ text_utils:format( "~p", [ K ] )
					 || K <- table:keys( ?getAttr(inbound_table) ) ],

	?error_fmt( "External identifier '~p' not found among the ~B known "
		"ones: ~ts", [ ExternalIdentifier, length( ExtIdStrings ),
					   text_utils:strings_to_sorted_string( ExtIdStrings ) ] ),

	throw( { unknown_external_identifier, ExternalIdentifier } ).



% Returns a textual description of the state of this identification server.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	IndentationLevel = 0,

	InString = inbound_to_string( IndentationLevel, ?getAttr(inbound_table) ),

	OutString = outbound_to_string( IndentationLevel,
									?getAttr(outbound_table) ),

	text_utils:format( "Identification server with an ~ts and with an ~ts",
					   [ InString, OutString ] ).



% Returns a textual description of the specified inbound table.
-spec inbound_to_string( text_utils:indentation_level(), inbound_table() ) ->
								ustring().
inbound_to_string( IndentationLevel, InboundTable ) ->

	InEntries = table:enumerate( InboundTable ),

	case InEntries of

		[] ->
			"empty inbound table (not registering any external identifier)";

		_ ->

			InStrings = [ text_utils:format(
				"external identifier '~ts' translated to block PID ~w",
				[ Ext, Pid ] ) || { Ext, Pid } <- InEntries ],

			ListString = text_utils:strings_to_sorted_string( InStrings,
														IndentationLevel ),

			text_utils:format( "inbound table able to convert ~B external "
				"identifiers into as many block PIDs: ~ts",
				[ length( InEntries ), ListString ] )

	end.



% Returns a textual description of the specified outbound table.
-spec outbound_to_string( text_utils:indentation_level(), outbound_table() ) ->
								string().
outbound_to_string( IndentationLevel, OutboundTable ) ->

	OutEntries = table:enumerate( OutboundTable ),

	case OutEntries of

		[] ->
			"empty outbound table (not registering any block PID)";

		_ ->

			OutStrings = [ text_utils:format(
				 "block PID ~w translated to external identifier '~ts'",
				 [ Pid, Ext ] ) || { Pid, Ext } <- OutEntries ],

			ListString = text_utils:strings_to_sorted_string( OutStrings,
													   IndentationLevel ),

			text_utils:format( "outbound table able to convert ~B block PIDs "
				"into as many external identifiers: ~ts",
				[ length( OutEntries ), ListString ] )

	end.
