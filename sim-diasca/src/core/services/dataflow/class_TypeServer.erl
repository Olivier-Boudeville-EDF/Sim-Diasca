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


-module(class_TypeServer).

-define( class_description,
		 "Class in charge of managing typing information, notably to establish "
		 "the definition of a given type and to tell whether a given term is "
		 "of a given type. "
		 "Generally instantiated as a singleton." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).


% Attributes that are specific to a type server are:
-define( class_attributes, [

	{ type_table, type_table(), "a table associating the name of a type to "
	  "its actual (canonical) definition; we do not try to expand types here "
	  "(i.e. sub-types are not replaced by their actual definition)" } ] ).



% Design notes:
%
% Currently the types are only checked for equality.


% Shorthands:

-type ustring() :: text_utils:ustring().

-type type_name() :: type_utils:type_name().

% We rely here only on fully-expanded, pure, explicit type definitions:
-type type_definition() :: type_utils:explicit_type().

-type type_entry() :: { type_name(), type_definition() }.

-type type_entries() :: [ type_entry() ].


% PID of the type server:
-type type_server_pid() :: pid().


% Inner table, may be reused by the type clients:
-type type_table() :: table( type_name(), type_definition() ).

-export_type([ type_name/0, type_definition/0, type_entry/0, type_entries/0,
			   type_server_pid/0, type_table/0 ]).



% Possible outcomes of a type validation (possibly involving multiple types):
-type validation_outcome() :: 'type_accepted'
							| { 'type_rejected', basic_utils:error_reason() }.


% Helpers:
-export([ to_string/1, type_table_to_string/1 ]).




% For registration:
-define( type_server_name, sim_diasca_type_server ).



-include_lib("wooper/include/wooper.hrl").

% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Types" ).

% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



% Constructs a new type server.
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% First the direct mother class:
	TraceState = class_EngineBaseObject:construct( State,
									?trace_categorize("TypeServer") ),

	naming_utils:register_as( self(), ?type_server_name, global_only ),

	?send_info( TraceState, "Type server started." ),

	% Then the class-specific actions:
	setAttribute( TraceState, type_table, table:new() ).



-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	TypeTable = ?getAttr(type_table),

	TypeNames = [ Name || { Name, _Def } <- table:enumerate( TypeTable ) ],

	?notice_fmt( "Stopping type server; it knew following ~B types: ~ts",
				 [ length( TypeNames ),
				   text_utils:atoms_to_sorted_string( TypeNames ) ] ),

	State.




% Methods section.



% Type declarations are oneways here (that may throw), while type validations
% are requests (that may return errors).


% Declares a (possibly new) type to this server.
-spec declareType( wooper:state(), type_name(), type_definition() ) ->
						oneway_return().
declareType( State, TypeName, TypeDefinition ) ->

	TypeTable = ?getAttr(type_table),

	case record_type( TypeName, TypeDefinition, TypeTable, State ) of

		{ ok, NewTypeTable } ->
			NewState = setAttribute( State, type_table, NewTypeTable ),
			wooper:return_state( NewState );

		{ error, Error } ->
			throw( { invalid_type_declared, Error, TypeName, TypeDefinition } )

	end.



% Declares a set of (possibly new) types to this server.
-spec declareTypes( wooper:state(), [ type_entry()] ) -> oneway_return().
declareTypes( State, TypeEntries ) ->

	TypeTable = ?getAttr(type_table),

	case record_types( TypeEntries, TypeTable, State ) of

		{ ok, NewTypeTable } ->
			NewState = setAttribute( State, type_table, NewTypeTable ),
			wooper:return_state( NewState );

		{ error, Error } ->
			throw( { invalid_type_declared, Error, TypeEntries } )

	end.



% Requests this type server to validate specified type entry (the corresponding
% type may or may not be new).
%
-spec validateType( wooper:state(), type_name(), type_definition() ) ->
		request_return( validation_outcome() ).
validateType( State, TypeName, TypeDefinition ) ->

	?debug_fmt( "Validation of type '~p' defined as '~p' requested.",
				[ TypeName, TypeDefinition ] ),

	TypeTable = ?getAttr(type_table),

	case record_type( TypeName, TypeDefinition, TypeTable, State ) of

		{ ok, NewTypeTable } ->
			NewState = setAttribute( State, type_table, NewTypeTable ),
			wooper:return_state_result( NewState, type_accepted );

		{ error, Error } ->
			wooper:const_return_result( { type_rejected, Error } )

	end.



% Requests this type server to validate specified type entries (the
% corresponding types may or may not be new).
%
-spec validateTypes( wooper:state(), [ type_entry() ] ) ->
		request_return( validation_outcome() ).
validateTypes( State, TypeEntries ) ->

	?debug_fmt( "Validation of types '~p' requested.", [ TypeEntries ] ),

	TypeTable = ?getAttr(type_table),

	case record_types( TypeEntries, TypeTable, State ) of

		{ ok, NewTypeTable } ->
			NewState = setAttribute( State, type_table, NewTypeTable ),
			wooper:return_state_result( NewState, type_accepted );

		{ error, Error } ->
			wooper:const_return_result( { type_rejected, Error } )

	end.



% Checks and records the specified type.
-spec record_type( type_name(), type_definition(), type_table(),
				   wooper:state() ) -> fallible( type_table() ).
record_type( TypeName, TypeDefinition, TypeTable, State )
  when is_atom( TypeName ) ->

	% Received type definition may or may not be already known, and, if yes, may
	% or may not correspond to the previous registered one:

	% We need a canonical form in all cases:
	case check_type( TypeDefinition, State ) of

		{ accepted, CanonicalTypeDefinition } ->

		   case table:lookup_entry( TypeName, TypeTable ) of

			   { value, CanonicalTypeDefinition } ->
				   % Matching an already known type, nothing changed:
				   { ok, TypeTable };

			   { value, OtherTypeDefinition } ->
				   % A more precise checking comparison shall be conducted:
				   ?error_fmt( "Received a non-matching definition for "
						"type '~ts': '~p', translated as '~p', "
						"instead of the known one '~p'.",
						[ TypeName, TypeDefinition, CanonicalTypeDefinition,
						  OtherTypeDefinition ] ),
				   { error, { unmatching_type_definition_for, TypeName,
							  TypeDefinition, OtherTypeDefinition } };

			   key_not_found ->
				   ?info_fmt( "Type '~ts', defined as '~ts', "
						"recorded as '~ts'.",
						[ TypeName, TypeDefinition, CanonicalTypeDefinition ] ),
				   { ok,
					 table:add_entry( TypeName, TypeDefinition,  TypeTable ) }

		   end;

		{ rejected, Reason } ->
			?error_fmt( "Type definition '~p' rejected, reason: ~p",
						[ TypeDefinition, Reason ] ),
			{ error, { type_rejected, TypeName, TypeDefinition, Reason } }

   end;

record_type( TypeName, TypeDefinition, _TypeTable, State ) ->
	?error_fmt( "Type name '~p' rejected (not an atom).", [ TypeName ] ),
	throw( { error, { invalid_type_name, TypeName, TypeDefinition } } ).



% Checks and records the specified types.
-spec record_types( [ type_entries() ], type_table(), wooper:state() ) ->
						fallible( type_table() ).
record_types( _TypeEntries=[], TypeTable, _State ) ->
	{ ok, TypeTable };

record_types( _TypeEntries=[ { TypeName, TypeDef } | T ], TypeTable, State ) ->

	case record_type( TypeName, TypeDef, TypeTable, State ) of

		{ ok, NewTypeTable } ->
			record_types( T, NewTypeTable, State );

		Error -> % Error={ error, Reason }
			% Stop recursing:
			Error

	end;

record_types( _TypeEntries=[ InvalidTypeEntry | _T ], _TypeTable, State ) ->

	?error_fmt( "Invalid type entry received: '~p', whereas a "
		"{TypeName,TypeDefinition} pair was expected.", [ InvalidTypeEntry ] ),

	throw( { invalid_type_entry, InvalidTypeEntry } ).



% Returns the definition of specified type, expected to be already known.
-spec getType( wooper:state(), type_name() ) ->
				const_request_return( type_definition() | 'unknown_type' ).
getType( State, TypeName ) ->

	TypeTable = ?getAttr(type_table),

	case table:lookup_entry( TypeName, TypeTable ) of

		{ value, TypeDefinition } ->
			wooper:const_return_result( TypeDefinition );

		key_not_found ->
			wooper:const_return_result( unknown_type )

	end.



% Returns a low-level (broken into elementary constructs), context-free
% definition of specified type.
%
%-spec resolveType( wooper:state(), type_definition() ) ->
%                               const_request_return( type_definition() ).
%resolveType( State, TypeDefinition ) ->
%
%	ResolvedType = TypeDefinition,
%
%	wooper:const_return_result( ResolvedType ).



% Checks specified type definition.
-spec check_type( basic_utils:unchecked_data(), wooper:state() ) ->
						{ validation_outcome(), wooper:state() }.
check_type( TypeDefinition, _State ) ->

	% Expected: type_definition().

	% Not implemented yet:
	{ accepted, TypeDefinition }.



% Returns a textual description of the state of this type server.
-spec getStatus( wooper:state() ) -> const_request_return( ustring() ).
getStatus( State ) ->
	wooper:const_return_result( to_string( State ) ).




% Helper section.


% Static section.


% Launches the type server, with default settings.
-spec start() -> static_return( type_server_pid() ).
start() ->
	wooper:return_static( new_link() ).



% Stops the type server.
-spec stop() -> static_void_return().
stop() ->
	TypeServerPid = get_server(),
	stop( TypeServerPid ),
	wooper:return_static_void().


% Stops specified type server.
-spec stop( type_server_pid() ) -> static_void_return().
stop( TypeServerPid ) ->
	TypeServerPid ! delete,
	wooper:return_static_void().



% Returns the PID of the type server (if any).
-spec get_server() -> static_return( type_server_pid() ).
get_server() ->
	ServerPid =
		naming_utils:get_registered_pid_for( ?type_server_name, global ),
	wooper:return_static( ServerPid ).




% Returns the names of the built-in types, i.e. the names of the types exposed
% to the user and that cannot be further decomposed.
%
-spec get_names_of_builtin_types() -> static_return( [ type_name() ] ).
get_names_of_builtin_types() ->
	%TypeNames = type_utils:get_elementary_types().
	TypeNames = [ boolean, integer, float, string, any ],

	wooper:return_static( TypeNames ).



% Resolves the specified type.
-spec resolve_type( type_name(), type_table(), type_server_pid() ) ->
							static_return( type_definition() ).
resolve_type( _TypeName, _TypeTable, _TypeServerPid ) ->
	% Still to be implemented:
	wooper:return_static( any ).



% Helper section:


% Returns a textual description of the state of this type server.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	TypeString = type_table_to_string( ?getAttr(type_table) ),

	text_utils:format( "Type server with ~ts", [ TypeString ] ).



% Returns a textual description of the specified type table.
-spec type_table_to_string( type_table() ) -> ustring().
type_table_to_string( TypeTable ) ->

	case table:enumerate( TypeTable ) of

		[] ->
			"no type known";

		Types ->

			TypeStrings = [ text_utils:format( "type '~ts' is: ~p", [ Tn, Td ] )
							|| { Tn, Td } <- Types ],

			BulletString = text_utils:strings_to_sorted_string( TypeStrings ),
			text_utils:format( "~B type(s) known, namely, in "
				"alphabetical order: ~ts", [ length( Types ), BulletString ] )

	end.
