% Copyright (C) 2022-2022 Olivier Boudeville
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
% User-defined identifiers can be introduced in order to simplify the
% GUI-related processings.
%
% They can be managed thanks to a separate server (process), or directly through
% an embedded allocation table.
%
-module(gui_id).


% Management of widget identifiers.
-export([ get_first_allocatable_id/0, get_initial_allocation_table/0,
		  get_id_allocator_pid/0,
		  allocate_id/0, allocate_id/1, allocate_ids/1, allocate_ids/2,
		  declare_id/1, declare_id/2, declare_ids/1, declare_ids/2,
		  resolve_id/1, resolve_id/2, resolve_ids/1, resolve_ids/2,
		  try_resolve_id/1, try_resolve_id/2 ]).


% Internals:
-export([ create_id_allocator/0 ]).


-type name_id() :: atom().
% A higher-level, user-defined identifier of a widget (ex: 'my_file_menu_id'),
% as a name (an atom internally translated transparently to a relevant wx_id()).
%
% The 'undefined' atom is reserved.


-type backend_id() :: wx_id().
% Lowest-level, backend-specific identifier.


-type id() :: maybe( backend_id() ) | name_id().
% Backend-specific object identifier.
%
% Defined here so that no user-level datastructure (like the event_context
% public record) bears any trace of any GUI actual backend.
%
% 'undefined' means that any identifier may be used, as chosen by the backend
% rather than by the user.
%
% May not be defined if the actual event comes from MyriadGUI itself (and thus
% not wx).


-type id_allocator_pid() :: pid().
% The PID of a MyriadGUI allocator of unique object identifiers.


-type myriad_instance_id() :: count().
% Myriad-specific instance identifier, corresponding a reference in the internal
% MyriadGUI type table.
%
% This is a different identifier from id() or backend_id(): it is not a
% standalone, user-level symbol to be used to designate directly an instance,
% but a part of its internal technical reference (similar in spirit to the
% integer in wx object references, like {wx_ref,35,wxFrame,[]}).


-export_type([ name_id/0, id/0, id_allocator_pid/0, myriad_instance_id/0,
			   backend_id/0, wx_id/0, id_name_alloc_table/0 ]).



% For related, public defines like gui_id_alloc_reg_name:
-include("gui.hrl").

% For related, internal, wx-related defines, like wxID_HIGHEST:
-include("gui_internal_defines.hrl").


% Identifier allocation.
%
% The allocation server holds:
% - the next free numerical identifier, to ensure that no two new ones collide
% - a conversion table so that the user can only handle symbolic (atom, named)
% identifiers (name_id()) instead of direct, raw numerical identifiers
%
% Note that the standard name/id associations (typically the standard menu
% items, like 'undo_menu_item') are reserved and automatically registered.
%
-type id_name_alloc_table() :: bijective_table( name_id(), backend_id() ).
% A table to convert between name identifiers and backend ones.



% Shorthands:

-type count() :: basic_utils:count().

-type wx_id() :: gui_wx_backend:wx_id().
% Just an integer.

-type bijective_table( F, S ) :: bijective_table:bijective_table( F, S ).



% @doc Returns the first backend-specific widget identifier that may be
% allocated by MyriadGUI.
%
-spec get_first_allocatable_id() -> backend_id().
get_first_allocatable_id() ->
	% So that the first identifier to be returned will typically be 10000:
	?wxID_HIGHEST + 4001.



% @doc Returns the initial allocation table for named identifiers.
-spec get_initial_allocation_table() -> id_name_alloc_table().
get_initial_allocation_table() ->

	% Statically known name/id associations:
	InitialEntries = [ { N, gui_wx_backend:to_wx_menu_item_maybe_id( N ) }
								|| N <- gui:get_standard_item_names() ],

	bijective_table:new( InitialEntries ).



% @doc Uses the calling process so that it becomes the identifier allocator.
%
% At least currently, with wx, no service seems to be provided in order to
% generate unique user-level wx_id(), so we provide ours.
%
% Otherwise the user would have to hardcode its own identifiers (ex: for menu
% items), which is fragile and error-prone.
%
% Note that the standard name/id associations (typically the standard menu
% items, like 'undo_menu_item') are reserved and automatically registered.
%
-spec create_id_allocator() -> no_return().
create_id_allocator() ->

	naming_utils:register_as( ?gui_id_alloc_reg_name, _RegScope=local_only ),

	id_allocator_main_loop( get_first_allocatable_id(),
							get_initial_allocation_table() ).



% The main loop of the process serving widget identifiers.
-spec id_allocator_main_loop( backend_id(), id_name_alloc_table() ) ->
													no_return().
id_allocator_main_loop( NextId, NameTable ) ->

	% Still pseudo WOOPER-like conventions (list parameters not wrapped, not
	% empty list if no parameter):
	%
	receive

		% For non-name identifiers:

		{ allocateIdentifier, RequesterPid } ->
			RequesterPid ! { notifyAllocatedIdentifier, NextId },
			id_allocator_main_loop( NextId+1, NameTable );

		{ allocateIdentifiers, Count, RequesterPid } ->
			Ids = lists:seq( NextId, NextId+Count-1 ),
			RequesterPid ! { notifyAllocatedIdentifiers, Ids },
			NewNextId = NextId + Count,
			id_allocator_main_loop( NewNextId, NameTable );


		% For name identifiers:

		{ declareNamedIdentifier, NameId, RequesterPid } ->
			NewNameTable =
				bijective_table:add_new_entry( NameId, NextId, NameTable ),
			RequesterPid ! { notifyDeclaredIdentifier, NextId },
			id_allocator_main_loop( NextId+1, NewNameTable );

		{ declareNamedIdentifiers, NameIds, RequesterPid } ->
			Count = length( NameIds ),
			Ids = lists:seq( NextId, NextId+Count-1 ),
			RequesterPid ! { notifyDeclaredIdentifiers, Ids },
			NewEntries = lists:zip( NameIds, Ids ),
			NewNameTable =
				bijective_table:add_new_entries( NewEntries, NameTable ),
			id_allocator_main_loop( NextId+Count, NewNameTable );


		{ resolveIdentifier, NameId, RequesterPid } ->
			Id = bijective_table:get_second_for( NameId, NameTable ),
			RequesterPid ! { notifyResolvedIdentifier, Id },
			id_allocator_main_loop( NextId, NameTable );

		{ resolveIdentifiers, NameIds, RequesterPid } ->
			Ids = bijective_table:get_second_elements_for( NameIds, NameTable ),
			RequesterPid ! { notifyResolvedIdentifiers, Ids },
			id_allocator_main_loop( NextId, NameTable );

		{ tryResolveIdentifier, BackendId, RequesterPid } ->
			BestId = try_resolve_id( BackendId, NameTable ),
			RequesterPid ! { notifyResolvedAnyIdentifier, BestId },
			id_allocator_main_loop( NextId, NameTable );

		% No real interest in having the client know the actual numerical
		% identifiers:
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
			ok

	end.



% @doc Returns the PID of the unique backend identifier allocator.
-spec get_id_allocator_pid() -> id_allocator_pid().
get_id_allocator_pid() ->
	naming_utils:get_registered_pid_for( ?gui_id_alloc_reg_name, _Scope=local ).




% Allocation section.


% @doc Returns a new, original (never used) unnamed backend object identifier,
% obtained from the MyriadGUI identifier allocator.
%
-spec allocate_id() -> backend_id().
allocate_id() ->
	?gui_id_alloc_reg_name ! { allocateIdentifier, self() },
	receive

		{ notifyAllocatedIdentifier, AllocatedId } ->
			AllocatedId

	end.


% @doc Returns a new, original (never used) unnamed backend object identifier,
% obtained from the specified identifier allocator.
%
-spec allocate_id( id_allocator_pid() ) -> backend_id().
allocate_id( IdAllocPid ) ->
	IdAllocPid ! { allocateIdentifier, self() },
	receive

		{ notifyAllocatedIdentifier, AllocatedId } ->
			AllocatedId

	end.


% @doc Returns the specified number of new, original (never used) unnamed
% backend object identifiers, obtained from the MyriadGUI identifier allocator.
%
-spec allocate_ids( count() ) -> [ backend_id() ].
allocate_ids( Count ) ->
	?gui_id_alloc_reg_name ! { allocateIdentifiers, Count, self() },
	receive

		{ notifyAllocatedIdentifiers, AllocatedIds } ->
			AllocatedIds

	end.


% @doc the specified number of new, original (never used) unnamed backend object
% identifiers, obtained from the specified identifier allocator.
%
-spec allocate_ids( count(), id_allocator_pid() ) -> [ backend_id() ].
allocate_ids( Count, IdAllocPid ) ->
	IdAllocPid ! { allocateIdentifiers, Count, self() },
	receive

		{ notifyAllocatedIdentifiers, AllocatedIds } ->
			AllocatedIds

	end.



% Declaration section.


% @doc Declares the specified named identifier, so that it becomes registered by
% the MyriadGUI identifier allocator, and returns the associated backend object
% identifier.
%
% Note that the standard name/id association (typically the standard menu items
% like 'undo_menu_item') are reserved and already registered.
%
-spec declare_id( name_id() ) -> backend_id().
declare_id( NameId ) ->
	?gui_id_alloc_reg_name ! { declareNamedIdentifier, NameId, self() },
	receive

		{ notifyDeclaredIdentifier, AllocatedId } ->
			AllocatedId

	end.


% @doc Declares the specified named identifier, so that it becomes registered by
% the specified identifier allocator, and returns the associated backend object
% identifier.
%
% Note that the standard name/id associations (typically the standard menu
% items, like 'undo_menu_item') are reserved and automatically registered.
%
-spec declare_id( name_id(), id_allocator_pid() ) -> void().
declare_id( NameId, IdAllocPid ) ->
	IdAllocPid ! { declareNamedIdentifier, NameId, self() },
	receive

		{ notifyDeclaredIdentifier, AllocatedId } ->
			AllocatedId

	end.



% @doc Declares the specified named identifiers, so that they become registered
% by the MyriadGUI identifier allocator.
%
% Note that the standard name/id associations (typically the standard menu
% items, like 'undo_menu_item') are reserved and automatically registered.
%
%
-spec declare_ids( [ name_id() ] ) -> void().
declare_ids( NameIds ) ->
	?gui_id_alloc_reg_name ! { declareNamedIdentifiers, NameIds, self() },
	receive

		{ notifyDeclaredIdentifiers, AllocatedIds } ->
			AllocatedIds

	end.


% @doc Declares the specified named identifiers, so that they become registered
% by the specified identifier allocator.
%
% Note that the standard name/id associations (typically the standard menu
% items, like 'undo_menu_item') are reserved and automatically registered.
%
-spec declare_ids( [ name_id() ], id_allocator_pid() ) -> void().
declare_ids( NameIds, IdAllocPid ) ->
	IdAllocPid ! { declareNamedIdentifiers, NameIds, self() },
	receive

		{ notifyDeclaredIdentifiers, AllocatedIds } ->
			AllocatedIds

	end.



% Resolution section.


% @doc Returns the low-level backend object identifier corresponding to the
% specified named identifier, which is expected to be already registered by the
% MyriadGUI identifier allocator.
%
-spec resolve_id( name_id() ) -> backend_id().
resolve_id( NameId ) ->
	?gui_id_alloc_reg_name ! { resolveIdentifier, NameId, self() },
	receive

		{ notifyResolvedIdentifier, ResolvedId } ->
			ResolvedId

	end.


% @doc Returns the low-level backend object identifier corresponding to the
% specified named identifier, which is expected to be already registered by the
% specified identifier allocator.
%
-spec resolve_id( name_id(), id_allocator_pid() ) -> backend_id().
resolve_id( NameId, IdAllocPid ) ->
	IdAllocPid ! { resolveIdentifier, NameId, self() },
	receive

		{ notifyResolvedIdentifier, ResolvedId } ->
			ResolvedId

	end.



% @doc Returns the low-level backend object identifiers corresponding to the
% specified named identifiers, which are expected to be already registered by
% the MyriadGUI identifier allocator.
%
-spec resolve_ids( [ name_id() ] ) -> [ backend_id() ].
resolve_ids( NameIds ) ->
	?gui_id_alloc_reg_name ! { resolveIdentifiers, NameIds, self() },
	receive

		{ notifyResolvedIdentifiers, ResolvedIds } ->
			ResolvedIds

	end.


% @doc Returns the low-level backend object identifiers corresponding to the
% specified named identifiers, which are expected to be already registered by
% the specified identifier allocator.
%
-spec resolve_ids( [ name_id() ], id_allocator_pid() ) -> [ backend_id() ].
resolve_ids( NameIds, IdAllocPid ) ->
	IdAllocPid ! { resolveIdentifiers, NameIds, self() },
	receive

		{ notifyResolvedIdentifiers, ResolvedIds } ->
			ResolvedIds

	end.


% @doc Tries to resolve the specified lower-level, backend-specific identifier:
% returns any corresponding named one, otherwise returns that backend identifier
% as it is.
%
-spec try_resolve_id( backend_id() ) -> id().
try_resolve_id( BackendId ) ->
	?gui_id_alloc_reg_name ! { tryResolveIdentifier, BackendId, self() },
	receive

		{ notifyResolvedAnyIdentifier, ResolvedAnyId } ->
			ResolvedAnyId

	end.



% @doc Tries to resolve the specified lower-level, backend-specific identifier:
% returns any corresponding named one, otherwise returns that backend identifier
% as it is.
%
-spec try_resolve_id( backend_id(), id_name_alloc_table() ) -> id().
try_resolve_id( BackendId, NameTable ) ->
	case bijective_table:get_maybe_first_for( BackendId, NameTable ) of

		undefined ->
			BackendId;

		NameId ->
			NameId

	end.
