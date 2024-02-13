% Copyright (C) 2022-2024 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Sunday, April 24, 2022.


% @doc Management of (higher-level) <b>widget identifiers</b>.
%
% User-defined, atom-based identifiers can be introduced in order to simplify
% the GUI-related processings.
%
% They can be managed thanks to a separate server (process), or directly through
% an embedded allocation table (like the MyriadGUI main loop).
%
-module(gui_id).



% Usage notes:
%
% There are two main kinds of identifiers, designated collectively as gui:id():
%
% - gui:name_id(), the higher-level ones, introduced by MyriadGUI, which are
% atoms (e.g. select_color_button) and that may be defined by the user
%
% - gui:backend_id(): the native, backend-specific (wx) ones, that are integers
% (e.g. 5153); while they can be used directly, MyriadGUI promotes more the
% native_id() ones
%
% Backend identifiers are either allocated dynamically, each time a new widget
% is created, or correspond to static, stock identifiers.
%
% Dynamic identifiers are managed thanks to a bijective table that is kept up to
% date by the MyriadGUI main loop, thanks to this gui_id module.
%
% Static identifiers are managed thanks to a build-time bijective table, thanks
% to the gui_constants module.



-type name_id() :: atom().
% A higher-level, user-defined identifier of a widget as a name (an atom,
% e.g. 'my_file_menu_id'); internally translated transparently to a relevant
% backend identifier (wx_id()).
%
% The 'undefined' atom is reserved.


-type backend_id() :: wx_id().
% Lowest-level, backend-specific identifier.


-type id() :: maybe( backend_id() ) | name_id().
% Object identifier, backend-specific or not.
%
% Defined here so that no user-level datastructure (like the event_context
% public record) bears any trace of any GUI actual backend.
%
% An 'undefined' value means that any identifier may be used, as chosen by the
% backend rather than by the user.
%
% May not be defined if the actual event comes from MyriadGUI itself (and thus
% not wx).
%
% As a consequence, such an identifier can be 'undefined', 'my_action_id', 147,
% and thus shall better be printed as "~w" (see id_to_string/1).


-type button_id() :: id().
% An identifier of a button.

-type button_backend_id() :: backend_id().
% A backend-level button identifier.


-type id_allocator_pid() :: pid().
% The PID of a MyriadGUI allocator of (unique) object identifiers.

-type id_allocator_ref() :: pid_ref().
% Any kind of reference onto a MyriadGUI allocator of (unique) object
% identifiers.


-type myriad_instance_id() :: count().
% Myriad-specific instance identifier, corresponding to a Myriad object
% reference in the internal MyriadGUI type table.
%
% This is a different identifier from id(), name_id() or backend_id(): it is not
% a standalone, user-level symbol to be used to designate directly an instance,
% but a part of its internal technical reference, like in {myriad_object_ref,
% myr_canvas, CanvasId} (similar in spirit to the integer in wx object
% references, like {wx_ref, 35, wxFrame, []}).


-export_type([ name_id/0, id/0, button_id/0, button_backend_id/0,
			   id_allocator_pid/0, myriad_instance_id/0,
			   backend_id/0, wx_id/0, id_name_alloc_table/0 ]).


% Management of widget identifiers.
-export([ get_first_allocatable_id/0, get_initial_allocation_table/0,

		  allocate_backend_id/0, allocate_backend_id/1,
		  allocate_backend_ids/1, allocate_backend_ids/2,

		  declare_name_id/1, declare_name_id/2, declare_name_id_internal/3,
		  declare_name_ids/1, declare_name_ids/2,
		  declare_any_id/1,

		  resolve_named_id/1, resolve_named_id/2,
		  resolve_named_ids/1, resolve_named_ids/2,
		  resolve_named_id_internal/2,
		  resolve_any_id/1,

		  maybe_resolve_backend_id/1, maybe_resolve_backend_id_internal/2,

		  get_any_id/0,
		  get_best_id/1, get_best_id/2, get_best_id_internal/2,
		  get_best_menu_item_id_internal/2, get_best_button_id_internal/2,
		  get_maybe_name_id/1,

		  id_to_string/1 ]).




% Internals:

-export([ embody_as_id_allocator/0 ]).

-compile({ inline, [ resolve_any_id/1 ]}).



% For related, public defines like gui_id_alloc_reg_name:
-include("gui_base.hrl").

% For related, internal, wx-related defines, like wxID_HIGHEST:
-include("gui_internal_defines.hrl").


% Identifier allocation.
%
% Any identifier allocation server shall hold:
% - the next free numerical identifier, to ensure that no two new ones collide
% - a conversion table so that the user can only handle symbolic (atom, named)
% identifiers (name_id()) instead of direct, raw numerical identifiers
%
% Note that the standard name/id associations (typically the standard menu
% items, like 'undo_menu_item') are reserved and automatically registered.


% The smallest (backend-specific) identifier that can be allocated by MyriadGUI.
%
% So that the first identifier to be returned will typically be 10000:
-define( min_allocated_id, ?wxID_HIGHEST + 4001 ).


-type id_name_alloc_table() :: bijective_table( name_id(), backend_id() ).
% A table to convert between (MyriadGUI) name identifiers and backend ones.




% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type pid_ref() :: type_utils:pid_ref().

-type wx_id() :: gui_wx_backend:wx_id().
% Just an integer.

-type bijective_table( F, S ) :: bijective_table:bijective_table( F, S ).



% Section concentrating functions to implement an identifier allocator.


% @doc Returns the first backend-specific widget identifier that may be
% allocated by MyriadGUI.
%
-spec get_first_allocatable_id() -> backend_id().
get_first_allocatable_id() ->
	% Just a sanity check:
	cond_utils:if_defined( myriad_debug_gui_id,
		begin
			MaxStandard = lists:max(
				[ gui_menu:to_wx_menu_item_id( N )
					|| N <- gui:get_standard_item_names() ] ),

			% Typically 5154 vs highest bound 5999:
			%trace_utils:debug_fmt(
			%   "Maximum standard ID: ~B; highest bound: ~B",
			%   [ MaxStandard, Highest ] ),

			MaxStandard >= ?wxID_HIGHEST andalso
				throw( { invalid_standard_identifiers, MaxStandard, Highest } )
		end ),

	?min_allocated_id.



% @doc Returns the initial allocation table for named identifiers.
-spec get_initial_allocation_table() -> id_name_alloc_table().
get_initial_allocation_table() ->

	% Statically known name/id associations:
	%
	% (seems to be a bad idea as it would prevent to create a standard item
	% afterwards)
	%
	%InitialEntries = [ { N, gui_menu:to_wx_menu_item_id( N ) }
	%                           || N <- gui:get_standard_item_names() ],

	%bijective_table:new( _InitialEntries ).
	bijective_table:new().



% @doc Uses the calling process so that it becomes the identifier allocator.
%
% At least currently, with wx, no service seems to be provided in order to
% generate unique user-level wx_id(), so we provide ours.
%
% Otherwise the user would have to hardcode their own identifiers (e.g. for menu
% items), which is fragile and error-prone.
%
% Note that the standard name/id associations (typically the standard buttons /
% menu items, like 'zoom_factor_one' or 'undo_menu_item') are reserved and
% automatically registered.
%
% This created identifier allocator shall be best looked-up thanks to the
% gui:get_id_allocator_pid/{0,1} functions.
%
-spec embody_as_id_allocator() -> no_return().
embody_as_id_allocator() ->

	naming_utils:register_as( ?gui_id_alloc_reg_name, _RegScope=local_only ),

	id_allocator_main_loop( get_first_allocatable_id(),
							get_initial_allocation_table() ).



% The main loop of the process serving widget identifiers.
-spec id_allocator_main_loop( backend_id(), id_name_alloc_table() ) ->
													no_return().
id_allocator_main_loop( NextId, NameTable ) ->

	% Still pseudo WOOPER-like conventions (list parameters not wrapped, no
	% empty list if no parameter):
	%
	receive

		% For non-name identifiers:

		{ allocateBackendIdentifier, RequesterPid } ->
			RequesterPid ! { notifyAllocatedBackendIdentifier, NextId },
			id_allocator_main_loop( NextId+1, NameTable );

		{ allocateBackendIdentifiers, Count, RequesterPid } ->
			Ids = lists:seq( NextId, NextId+Count-1 ),
			RequesterPid ! { notifyAllocatedBackendIdentifiers, Ids },
			NewNextId = NextId + Count,
			id_allocator_main_loop( NewNextId, NameTable );


		% For name identifiers:

		{ declareNameIdentifier, NameId, RequesterPid } ->

			{ AllocatedId, NewNextId, NewNameTable } =
				declare_name_id_internal( NameId, NextId, NameTable ),

			%trace_utils:debug_fmt( "Name '~ts' declared as ~B.",
			%                       [ NameId, AllocatedId ] ),

			RequesterPid ! { notifyDeclaredNameIdentifier, AllocatedId },
			id_allocator_main_loop( NewNextId, NewNameTable );

		{ declareNameIdentifiers, NameIds, RequesterPid } ->
			% Using here foldr to avoid a reversing of the accumulated ids:
			{ FinalNextId, FinalNameTable, Ids } = lists:foldr(
				fun( NameId, { AccNextId, AccNameTable, AccIds } ) ->
					{ AllocatedId, NewNextId, NewNameTable } =
						declare_name_id_internal( NameId, AccNextId,
												  AccNameTable ),
					{ NewNextId, NewNameTable, [ AllocatedId | AccIds ] }

				end,
				_Acc0={ NextId, NameTable, _Ids=[] },
				_List=NameIds ),

			RequesterPid ! { notifyDeclaredNameIdentifiers, Ids },
			id_allocator_main_loop( FinalNextId, FinalNameTable );


		{ resolveNameIdentifier, NameId, RequesterPid } ->
			BackendId = resolve_named_id_internal( NameId, NameTable ),
			RequesterPid ! { notifyResolvedNameIdentifier, BackendId },
			id_allocator_main_loop( NextId, NameTable );

		{ resolveNameIdentifiers, NameIds, RequesterPid } ->
			BackendIds = [ resolve_named_id_internal( NId, NameTable )
							|| NId <- NameIds ],
			RequesterPid ! { notifyResolvedNameIdentifiers, BackendIds },
			id_allocator_main_loop( NextId, NameTable );


		{ resolveBackendIdentifier, BackendId, RequesterPid } ->
			MaybeNameId =
				maybe_resolve_backend_id_internal( BackendId, NameTable ),
			RequesterPid ! { notifyResolvedBackendIdentifier, MaybeNameId },
			id_allocator_main_loop( NextId, NameTable );


		{ getBestId, BackendId, RequesterPid } ->
			BestId = get_best_id_internal( BackendId, NameTable ),
			RequesterPid ! { notifyResolvedAnyIdentifier, BestId },
			id_allocator_main_loop( NextId, NameTable );



		% Generally no real interest in having the client know the actual
		% numerical identifiers:
		%
		{ requestIdentifier, NameId, RequesterPid } ->
			NewNameTable =
				bijective_table:add_new_entry( NameId, NextId, NameTable ),
			RequesterPid ! { notifyRequestedIdentifier, NextId },
			id_allocator_main_loop( NextId+1, NewNameTable );

		{ requestIdentifiers, NameIds, RequesterPid } ->
			Count = length( NameIds ),
			Ids = lists:seq( NextId, NextId+Count-1 ),
			NewEntries = lists:zip( NameIds, Ids ),
			NewNameTable =
				bijective_table:add_new_entries( NewEntries, NameTable ),
			RequesterPid ! { notifyRequestedIdentifiers, Ids },
			id_allocator_main_loop( NextId+Count, NewNameTable );

		terminate ->
			cond_utils:if_defined( myriad_debug_gui_id,
				trace_utils:debug_fmt( "Identifier allocator ~w terminating.",
									   [ self() ] ) ),
			ok

	end.



% @doc Declares the specified name identifier internally (directly from the
% current process), possibly enriching the specified name allocation table,
% returning updated information, notably the corresponding backend identifier.
%
% Typically called directly through the MyriadGUI main loop (thus without
% involving messages), which takes care of the identifier management as well.
%
-spec declare_name_id_internal( name_id(), backend_id(),
								id_name_alloc_table() ) ->
			{ backend_id(), backend_id(), id_name_alloc_table() }.
declare_name_id_internal( NameId, NextId, NameTable ) ->

	% As standard/stock identifiers do not have to be stored (thanks to the
	% bijective table):
	%
	case gui_generated:get_maybe_second_for_button_id( NameId ) of

		undefined ->
			case gui_generated:get_maybe_second_for_menu_item_id( NameId ) of

				undefined ->
					% Neither a standard button nor a menu item name, hence this
					% identifier must be auto-allocated:
					%
					NewNameTable = bijective_table:add_new_entry( NameId,
						NextId, NameTable ),

					{ _AllocatedId=NextId, _NewNextId=NextId+1, NewNameTable };

				MenuItemWxId ->
					% Standard menu item name, hence no new identifier to issue:
					{ _AllocatedId=MenuItemWxId, _NewNextId=NextId, NameTable }

			end;

		ButtonWxId ->
			% Standard button name, hence no allocation change either:
			{ _AllocatedId=ButtonWxId, _NewNextId=NextId, NameTable }

	end.



% @doc Resolves the specified name identifier into a backend one, internally
% (directly from the current process), from the specified name allocation table.
%
% Throws an exception if no corresponding backend identifier can be found.
%
% Typically called directly through the MyriadGUI main loop (thus without
% involving messages), which takes care of the identifier management as well.
%
-spec resolve_named_id_internal( name_id(), id_name_alloc_table() ) ->
											backend_id().
resolve_named_id_internal( NameId, NameTable ) ->

	case maybe_resolve_named_id_internal( NameId, NameTable ) of

		undefined ->
			throw( { unresolvable_name_id, NameId } );

		BackendId ->
			BackendId

	end.





% Section to manage user-originating identifier operations.


% @doc Returns a backend identifier corresponding to the specified identifier of
% any type (MyriadGUI name identifier, possibly an undefined one, or already a
% backend identifier).
%
% Throws an exception should the resolution fail.
%
-spec resolve_any_id( id() ) -> backend_id().
% Module-local, meant to resolve quickly most cases.
resolve_any_id( undefined ) ->
	?gui_any_id;

resolve_any_id( BackendId ) when is_integer( BackendId ) ->
	BackendId;

resolve_any_id( NameId ) when is_atom( NameId ) ->
	% Relies on the fact that the MyriadGUI main process now impersonates a
	resolve_named_id( NameId, _IdAllocRef=gui:get_id_allocator_pid() ).



% @doc Resolves, if possible, the specified name identifier into a backend one
% and internally (directly from the current process), from the specified name
% allocation table.
%
-spec maybe_resolve_named_id_internal( name_id(), id_name_alloc_table() ) ->
											maybe( backend_id() ).
maybe_resolve_named_id_internal( NameId, NameTable ) ->

	% As standard/stock identifiers do not have to be stored (thanks to the
	% bijective table):
	%
	case gui_generated:get_maybe_second_for_button_id( NameId ) of

		undefined ->
			case gui_generated:get_maybe_second_for_menu_item_id( NameId ) of

				undefined ->
					% Neither a standard button nor a menu item name, hence this
					% identifier may have been auto-allocated:
					%
					bijective_table:get_maybe_second_for( NameId, NameTable );

				MenuItemWxId ->
					MenuItemWxId

			end;

		ButtonWxId ->
			ButtonWxId

	end.




% Allocation section.


% @doc Returns a new, original (never used) unnamed backend object identifier,
% obtained from the MyriadGUI identifier allocator (if any).
%
-spec allocate_backend_id() -> backend_id().
allocate_backend_id() ->
	allocate_backend_id( _IdAllocRef=gui:get_id_allocator_pid()  ).


% @doc Returns a new, original (never used) unnamed backend object identifier,
% obtained from the specified identifier allocator.
%
-spec allocate_backend_id( id_allocator_ref() ) -> backend_id().
allocate_backend_id( IdAllocRef ) ->
	IdAllocRef ! { allocateBackendIdentifier, self() },
	receive

		{ notifyAllocatedBackendIdentifier, AllocatedBackendId } ->
			AllocatedBackendId

	end.



% @doc Returns the specified number of new, original (never used) unnamed
% backend object identifiers, obtained from the MyriadGUI identifier allocator
% (if any).
%
-spec allocate_backend_ids( count() ) -> [ backend_id() ].
allocate_backend_ids( Count ) ->
	allocate_backend_ids( Count, _IdAllocRef=gui:get_id_allocator_pid() ).


% @doc Returns the specified number of new, original (never used) unnamed
% backend object identifiers, obtained from the specified identifier allocator.
%
-spec allocate_backend_ids( count(), id_allocator_ref() ) -> [ backend_id() ].
allocate_backend_ids( Count, IdAllocRef ) ->
	IdAllocRef ! { allocateBackendIdentifiers, Count, self() },
	receive

		{ notifyAllocatedBackendIdentifiers, AllocatedIds } ->
			AllocatedIds

	end.



% Declaration section.


% @doc Declares the specified named identifier, so that it becomes registered by
% the MyriadGUI identifier allocator (if any), and returns the associated
% backend object identifier.
%
% Note that the standard name/id association (typically the standard menu items
% like 'undo_menu_item') are reserved (but not already registered).
%
-spec declare_name_id( name_id() ) -> backend_id().
declare_name_id( NameId ) ->
	declare_name_id( NameId, _IdAllocRef=gui:get_id_allocator_pid() ).


% @doc Declares the specified named identifier, so that it becomes registered by
% the specified identifier allocator, and returns the associated backend object
% identifier.
%
% Note that the standard name/id association (typically the standard menu items
% like 'undo_menu_item') are reserved (but not already registered).
%
-spec declare_name_id( name_id(), id_allocator_ref() ) -> void().
declare_name_id( NameId, IdAllocRef ) ->
	IdAllocRef ! { declareNameIdentifier, NameId, self() },
	receive

		{ notifyDeclaredNameIdentifier, AllocatedId } ->
			AllocatedId

	end.



% @doc Declares the specified named identifiers, so that they become registered
% by the MyriadGUI identifier allocator (if any).
%
% Note that the standard name/id associations (typically the standard menu
% items, like 'undo_menu_item') are reserved and automatically registered.
%
-spec declare_name_ids( [ name_id() ] ) -> void().
declare_name_ids( NameIds ) ->
	declare_name_ids( NameIds, _IdAllocRef=gui:get_id_allocator_pid() ).


% @doc Declares the specified named identifiers, so that they become registered
% by the specified identifier allocator.
%
% Note that the standard name/id associations (typically the standard menu
% items, like 'undo_menu_item') are reserved and automatically registered.
%
-spec declare_name_ids( [ name_id() ], id_allocator_ref() ) -> void().
declare_name_ids( NameIds, IdAllocRef ) ->
	IdAllocRef ! { declareNameIdentifiers, NameIds, self() },
	receive

		{ notifyDeclaredNameIdentifiers, AllocatedIds } ->
			AllocatedIds

	end.



% @doc Returns a backend-specific widget identifier associated to the specified
% new identifier, expected not to have already been declared.
%
% Specifying a name identifier requires interacting with the MyriadGUI process
% in charge of the identifier allocation.
%
-spec declare_any_id( id() ) -> backend_id().
declare_any_id( undefined ) ->
	?gui_any_id;

% Integers are set by the (wx) backend (wx_id()):
declare_any_id( Id ) when is_integer( Id ) ->
	Id;

% Atoms are higher-level identifiers set at the MyriadGUI level:
declare_any_id( NameId ) when is_atom( NameId ) ->
	% Relies on the fact that the MyriadGUI main process now impersonates a
	% standalone gui_id server:
	%
	declare_name_id( NameId, _IdAllocRef=gui:get_id_allocator_pid() ).



% Resolution section.


% @doc Returns the low-level backend object identifier corresponding to the
% specified named identifier, which is expected to be already registered by the
% MyriadGUI identifier allocator (if any).
%
-spec resolve_named_id( name_id() ) -> backend_id().
resolve_named_id( NameId ) ->
	resolve_named_id( NameId, _IdAllocRef=gui:get_id_allocator_pid() ).


% @doc Returns the low-level backend object identifier corresponding to the
% specified named identifier, which is expected to be already registered by the
% specified identifier allocator.
%
-spec resolve_named_id( name_id(), id_allocator_ref() ) -> backend_id().
resolve_named_id( NameId, IdAllocRef ) ->
	IdAllocRef! { resolveNameIdentifier, NameId, self() },
	receive

		{ notifyResolvedNameIdentifier, ResolvedId } ->
			ResolvedId

	end.



% @doc Returns the low-level backend object identifiers corresponding to the
% specified named identifiers, which are expected to be already registered by
% the MyriadGUI identifier allocator (if any).
%
-spec resolve_named_ids( [ name_id() ] ) -> [ backend_id() ].
resolve_named_ids( NameIds ) ->
	resolve_named_ids( NameIds, _IdAllocRef=gui:get_id_allocator_pid() ).


% @doc Returns the low-level backend object identifiers corresponding to the
% specified named identifiers, which are expected to be already registered by
% the specified identifier allocator.
%
-spec resolve_named_ids( [ name_id() ], id_allocator_ref() ) ->
								[ backend_id() ].
resolve_named_ids( NameIds, IdAllocRef ) ->
	IdAllocRef ! { resolveNameIdentifiers, NameIds, self() },
	receive

		{ notifyResolvedNameIdentifiers, ResolvedIds } ->
			ResolvedIds

	end.


% @doc Tries to resolve the specified backend identifier into a named one,
% otherwise returns 'undefined', based on the MyriadGUI identifier allocator (if
% any).
%
-spec maybe_resolve_backend_id( backend_id() ) -> maybe( name_id() ).
maybe_resolve_backend_id( BackendId ) ->
	maybe_resolve_backend_id( BackendId, _IdAllocRef=gui:get_id_allocator_pid() ).


% @doc Tries to resolve the specified backend identifier into a named one,
% otherwise returns 'undefined', based on the specified identifier allocator.%
% any).
%
-spec maybe_resolve_backend_id( backend_id(), id_allocator_ref() ) ->
										maybe( name_id() ).
maybe_resolve_backend_id( BackendId, IdAllocRef ) ->
	IdAllocRef ! { resolveBackendIdentifier, BackendId, self() },
	receive

		{ notifyResolvedBackendIdentifier, MaybeNameId } ->
			MaybeNameId

	end.



% @doc Tries to resolve the specified backend-specific identifier into a
% higher-level named identifier, internally (directly from the current process):
% returns any corresponding named one.
%
% Looks up either, for stock identifiers, first the button table, then the menu
% item one or, for MyriadGUI ones, the specified name allocation table.
%
-spec maybe_resolve_backend_id_internal( backend_id(),
						id_name_alloc_table() ) -> maybe( name_id() ).
% If a standard/stock identifier:
maybe_resolve_backend_id_internal( BackendId, _NameTable )
								when BackendId < ?min_allocated_id ->

	%trace_utils:debug_fmt( "Resolving backend identifier #~B.",
	%                       [ BackendId ] ),

	case gui_generated:get_maybe_first_for_button_id( BackendId ) of

		undefined ->
			gui_generated:get_maybe_first_for_menu_item_id( BackendId );

		ButtonNameId ->
			ButtonNameId

	end;

% If a dynamically-allocated one:
maybe_resolve_backend_id_internal( BackendId, NameTable ) ->

	%trace_utils:debug_fmt( "Searching name identifier for #~B from ~ts.",
	%   [ BackendId, bijective_table:to_string( NameTable ) ] ),

	bijective_table:get_maybe_first_for( BackendId, NameTable ).



% @doc Returns an unconstrained identifier, whose value is left free.
-spec get_any_id() -> id().
get_any_id() ->
	% Not wanting the other MyriadGUI module to depend on wx includes.

	% Using directly that constant rather than calling this function can be
	% preferred:
	%
	?gui_any_id.


% @doc Returns the best (highest-level) identifier for the specified backend
% one, from the MyriadGUI identifier allocator (if any): if able to determine a
% name identifier for it, returns it, otherwise returns said backend identifier
% as it is.
%
-spec get_best_id( backend_id() ) -> id().
get_best_id( BackendId ) ->
	get_best_id( BackendId, _IdAllocRef=gui:get_id_allocator_pid() ).



% @doc Returns the best (highest-level) identifier for the specified backend
% one, from the specified identifier allocator: if able to determine a name
% identifier for it, returns it, otherwise returns said backend identifier as it
% is.
%
-spec get_best_id( backend_id(), id_allocator_ref() ) -> id().
get_best_id( BackendId, IdAllocRef ) ->
	IdAllocRef ! { getBestId, BackendId, self() },
	receive

		{ notifyResolvedAnyIdentifier, ResolvedAnyId } ->
			ResolvedAnyId

	end.



% @doc Tries to resolve the specified lower-level, backend-specific identifier,
% internally (directly from the current process): returns any corresponding
% named one, otherwise returns that backend identifier as it is.
%
-spec get_best_id_internal( backend_id(), id_name_alloc_table() ) -> id().
get_best_id_internal( NameId, _NameTable ) when is_atom( NameId ) ->
	NameId;

get_best_id_internal( BackendId, NameTable ) ->
	case maybe_resolve_backend_id_internal( BackendId, NameTable ) of

		undefined ->
			BackendId;

		NameId ->
			NameId

	end.


% @doc Tries to resolve the specified lower-level, backend-specific identifier
% supposedly of a menu item, internally (directly from the current process):
% returns any corresponding named one, otherwise returns that backend identifier
% as it is.
%
-spec get_best_menu_item_id_internal( backend_id(), id_name_alloc_table() ) ->
											id().
get_best_menu_item_id_internal( BackendId, _NameTable )
									when BackendId < ?min_allocated_id ->
	case gui_generated:get_maybe_first_for_menu_item_id( BackendId ) of

		undefined ->
			BackendId;

		MenuItemNameId ->
			MenuItemNameId

	end;

get_best_menu_item_id_internal( BackendId, NameTable ) ->
	case bijective_table:get_maybe_first_for( BackendId, NameTable ) of

		undefined ->
			BackendId;

		MenuItemNameId ->
			MenuItemNameId

	end.



% @doc Tries to resolve the specified lower-level, backend-specific identifier
% supposedly of a button internally (directly from the current process): returns
% any corresponding named one, otherwise returns that backend identifier as it
% is.
%
-spec get_best_button_id_internal( backend_id(), id_name_alloc_table() ) ->
											id().
get_best_button_id_internal( BackendId, _NameTable )
									when BackendId < ?min_allocated_id ->
	case gui_generated:get_maybe_first_for_button_id( BackendId ) of

		undefined ->
			BackendId;

		ButtonNameId ->
			ButtonNameId

	end;

get_best_button_id_internal( BackendId, NameTable ) ->
	case bijective_table:get_maybe_first_for( BackendId, NameTable ) of

		undefined ->
			BackendId;

		ButtonNameId ->
			ButtonNameId

	end.



% @doc Returns any MyriadGUI name identifier associated to the specified
% backend-specific widget identifier.
%
% Not exactly the reciprocal of resolve_any_id/1.
%
-spec get_maybe_name_id( backend_id() ) -> maybe( name_id() ).
get_maybe_name_id( BackendId ) when is_integer( BackendId ) ->
	% Relies on the fact that the MyriadGUI main process now impersonates a
	% standalone gui_id server:
	%
	maybe_resolve_backend_id_internal( BackendId,
									   _IdAllocRef=gui:get_id_allocator_pid() ).



% @doc Returns a textual representation of the specified object identifier.
-spec id_to_string( id() ) -> ustring().
id_to_string( BackendId ) when is_integer( BackendId ) ->
	text_utils:format( "#~B", [ BackendId ] );

id_to_string( NameId ) when is_atom( NameId ) ->
	text_utils:atom_to_string( NameId ).
