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
% Creation date: Sunday, April 3, 2022.


% @doc Gathering of facilities for the management of any kind of <b>data
% resources</b>, typically a content (e.g. image, sound) read from file as a
% binary, or generated as a term, and to be stored in a suitable <b>resource
% referential</b> (mostly a table of resources), possibly made available thanks
% to a <b>resource server</b> (mosty a process holding a table of resources).
%
% Specialised accessors are defined for some resource types (e.g. for bitmaps),
% so that they can be loaded specifically and conveniently.

% See also the (unrelated) environment module for the caching of application
% environments.
%
-module(resource).


-export([ % Local referential:
		  create_referential/0, create_referential/1,
		  get/2, get_multiple/2,
		  get_bitmap/2, get_bitmaps/2,

		  has/2, register/2, register/3,
		  remove/2, remove_multiple/2, flush/1,

		  locate_data/2, locate_multiple_data/2, get_path/2,
		  referential_to_string/1, resource_type_to_string/1,

		  % Server-side:

		  create_server/0, create_server/1, create_server/2,

		  create_linked_server/0, create_linked_server/1,
		  create_linked_server/2 ]).



% Usage notes:
%
% While a resource referential acts only at the level of a given process, a
% resource server allows to share resources between processes. Thanks to the
% sharing of (large-enough) binaries, this operation makes sense.
%
% The API exposed by this module (e.g. get/2) can be used for any resource
% holder, i.e. a local referential or a server one. In this last case, one may
% also directly perform the message sending and retrieving, in order to favor
% process interleaving.
%
% For graphical resources:
%
% For graphical operations to be able to happen (e.g. with get_bitmap/2,
% get_bitmaps/2), the calling process shall already have its GUI backend
% environment set (see gui:{g,s}et_backend_environment/1), otherwise it will
% exit (for example with a {wx,unknown_env} error).
%
% Bitmaps currently do not seem to be clonable (no cheap copy or increase of
% their reference counter); therefore, if one is used in a frame (typically for
% a static display thereof) that gets closed, this bitmap will be silently
% deallocated, and any reference thereof (e.g. {wx_ref,67, wxBitmap,[]}} held in
% referential will actually become a stale reference (any operation on it
% resulting in {{badarg,"This"}, ...}). A copy constructor available from wx
% would help relying on the underlying reference counter of wxWidgets.
%
% As a result, a specialised resource (like a bitmap) shall not be fetched as a
% basic resource (e.g. with get/2 or get_multiple/2; use get_bitmap/2,
% get_bitmaps/2, etc. instead), as the extra provisions undertaken for
% specialised resources would not be performed.


-type resource() :: term().
% Any (unspecialised) resource, often as a binary term (e.g. if loaded from
% file).


-type bitmap_resource() :: bitmap().
% A bitmap resource.


-type resource_file_id() :: bin_file_path().
% An identifier of a file resource, that is a resource that can be read directly
% from a filesystem, as a path either absolute or relative to an (implicit)
% resource root directory. For example: `<<"images/hello.png">>'.


-type resource_file_id_string() :: file_path().
% A (plain) string version of a file resource identifier, possibly more
% convenient for the user.


-type any_resource_file_id() :: resource_file_id() | resource_file_id_string().
% Any kind of file resource identifier.


-type resource_logical_id() :: any(). % Precisely: non-list().
% A logical, user-introduced resource identifier (e.g. 'my_splash_content', or
% 147), which does not correspond (at least directly) to a resource that can be
% read directly from a filesystem (otherwise any_resource_file_id() should be
% used); it is thus opaque, explicitly registered by the user, and refer to any
% term (e.g. a generated bitmap).
%
% Using often a symbol (an atom), so that it cannot be mixed up with an actual
% path.


-type resource_id() :: any_resource_file_id() | resource_logical_id().
% Any type of resource identifier.

-type bitmap_resource_id() :: resource_id().
% Any type of bitmap resource identifier.



-type resource_table( I ) :: table( I, resource() ).
% A table referencing resources, based on keys of the specified type, except
% lists (that are prohibited in order to avoid ambiguity with plain strings).
%
% Generally I is resource_id(), see resource_table/0.


-type resource_table() :: resource_table( resource_id() ).
% A table referencing resources, based on any kind of resource keys.


% For the resource_referential record:
-include("resource.hrl").

-type resource_referential() :: #resource_referential{}.
% A resource referential, storing resources based on their identifier.
%
% It is a term (a record mostly holding a table of resources) in the current
% process.


-type resource_server_pid() :: pid().
% The PID of a resource server, which is basically a process holding a resource
% referential.


-type resource_holder() :: resource_referential() | resource_server_pid().
% Any container of resources.


-export_type([ resource/0, bitmap_resource/0,
			   resource_file_id/0, resource_file_id_string/0,
			   any_resource_file_id/0,
			   resource_logical_id/0,
			   resource_id/0, bitmap_resource_id/0,
			   resource_table/0, resource_table/1,
			   resource_referential/0,
			   resource_holder/0 ]).



% Implementation notes:
%
% At least most traces conditioned by the myriad_debug_resources token includes
% a PID to discriminate between the caller and any resource server.
%
% Next versions could define a resource_entry record, tracking resource sizes
% and creation/last use timestamps.


% Shorthands:

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().
-type any_file_path() :: file_utils:any_file_path().
-type bin_directory_path() :: file_utils:bin_directory_path().
-type any_directory_path() :: file_utils:any_directory_path().

-type ustring() :: text_utils:ustring().

% No clear enough: -type backend_environment() :: gui:backend_environment().
-type bitmap() :: gui_bitmap:bitmap().


-compile( { no_auto_import, [ register/2 ] } ).



% Resource Referential section.


% @doc Returns an empty referential, not anchored to a specific root directory.
-spec create_referential() -> resource_referential().
create_referential() ->
	#resource_referential{ root_directory=undefined,
						   table=table:new() }.



% @doc Returns an empty referential that is to operate from the specified root
% directory. File-based resources can then be declared implicitly relatively to
% this directory.
%
-spec create_referential( any_directory_path() ) -> resource_referential().
create_referential( AnyRootDir ) ->

	AbsRootDir = file_utils:ensure_path_is_absolute( AnyRootDir ),

	file_utils:is_existing_directory_or_link( AbsRootDir )
		orelse throw( { non_existing_resource_directory, AbsRootDir } ),

	#resource_referential{
		root_directory=text_utils:ensure_binary( AbsRootDir ),
		table=table:new() }.




% Resource Server section.


% @doc Creates a resource server, and returns its PID.
%
% For graphical operations to be able to happen (e.g. with get_bitmap/2,
% get_bitmaps/2), the calling process shall already have its GUI backend
% environment set (see gui:{g,s}et_backend_environment/1).
%
-spec create_server() -> resource_server_pid().
create_server() ->
	spawn( fun() -> server_init() end ).


% @doc Creates a resource server anchored at the specified root directory, and
% returns its PID.
%
% For graphical operations to be able to happen (e.g. with get_bitmap/2,
% get_bitmaps/2), the calling process shall already have its GUI backend
% environment set (see gui:{g,s}et_backend_environment/1).
%
-spec create_server( any_directory_path() ) -> resource_server_pid().
create_server( AnyRootDir ) ->
	spawn( fun() -> server_init( text_utils:ensure_binary( AnyRootDir ) ) end ).


% @doc Creates a resource server anchored at the specified root directory and
% using the specified GUI backend environment (useful to perform for example
% bitmap loading), and returns its PID.
%
% For graphical operations to be able to happen (e.g. with get_bitmap/2,
% get_bitmaps/2), the calling process shall already have its GUI backend
% environment set (see gui:{g,s}et_backend_environment/1).
%
-spec create_server( any_directory_path(), gui:backend_environment() ) ->
											resource_server_pid().
create_server( AnyRootDir, GUIBackendEnv ) ->
	spawn( fun() ->
			server_init( text_utils:ensure_binary( AnyRootDir ), GUIBackendEnv )
		   end ).



% @doc Creates a linked resource server, and returns its PID.
%
% For graphical operations to be able to happen (e.g. with get_bitmap/2,
% get_bitmaps/2), the calling process shall already have its GUI backend
% environment set (see gui:{g,s}et_backend_environment/1).
%
-spec create_linked_server() -> resource_server_pid().
create_linked_server() ->
	spawn_link( fun() -> server_init() end ).


% @doc Creates a linked resource server anchored at the specified root
% directory, and returns its PID.
%
% For graphical operations to be able to happen (e.g. with get_bitmap/2,
% get_bitmaps/2), the calling process shall already have its GUI backend
% environment set (see gui:{g,s}et_backend_environment/1).
%
-spec create_linked_server( any_directory_path() ) -> resource_server_pid().
create_linked_server( AnyRootDir ) ->
	spawn_link( fun() ->
					server_init( text_utils:ensure_binary( AnyRootDir ) )
				end ).


% @doc Creates a linked resource server anchored at the specified root directory
% and using the specified GUI backend environment (useful to perform for example
% bitmap loading), and returns its PID.
%
% For graphical operations to be able to happen (e.g. with get_bitmap/2,
% get_bitmaps/2), the calling process shall already have its GUI backend
% environment set (see gui:{g,s}et_backend_environment/1).
%
-spec create_linked_server( any_directory_path(), gui:backend_environment() ) ->
											resource_server_pid().
create_linked_server( AnyRootDir, GUIBackendEnv ) ->
	spawn_link( fun() ->
					server_init( text_utils:ensure_binary( AnyRootDir ),
								 GUIBackendEnv )
				end ).


% Resource holder API: applies both to referentials and servers.


% @doc Returns the resource (unspecialised) content corresponding to the
% specified (file or logical) identifier, based on the specified resource
% holder.
%
-spec get( resource_id(), resource_referential() ) ->
				{ resource(), resource_referential() };
		 ( resource_id(), resource_server_pid() ) -> resource().
get( RscIdStr, RscHolder ) when is_list( RscIdStr ) ->
	get( text_utils:string_to_binary( RscIdStr ), RscHolder );

% First, clauses for referential (local) side:
%
% Here, RscId is not a list/string (generally it is either a binary string or an
% atom), for which no resource is ever loaded: either the resource is already
% registered and thus returned, or this operation fails. Referential is thus
% const.
%
get( RscId, RscRef=#resource_referential{ table=RscTable } ) ->

	case table:lookup_entry( RscId, RscTable ) of

		{ value, Rsc } ->
			% Resource logical or not:
			cond_utils:if_defined( myriad_debug_resources,
				trace_utils:debug_fmt( "[~w] Returning already-available "
									   "resource '~p'.", [ self(), RscId ] ) ),
			{ Rsc, RscRef };

		key_not_found ->
			% Then hopefully it is a binary string to allow for a loading from
			% file:
			%
			case is_binary( RscId ) of

				true ->
					BinRscPath =
						case RscRef#resource_referential.root_directory of

							undefined ->
								RscId;

							BinRootDir ->
								file_utils:bin_join( BinRootDir, RscId )

						end,

					file_utils:is_existing_file_or_link( BinRscPath ) orelse
						throw( { resource_not_found,
								 text_utils:binary_to_string( BinRscPath ) } ),

					BinRsc = file_utils:read_whole( BinRscPath ),

					NewRscTable =
						table:add_entry( RscId, BinRsc, RscTable ),

					NewRscRef =
						RscRef#resource_referential{ table=NewRscTable },

					cond_utils:if_defined( myriad_debug_resources,
						trace_utils:debug_fmt(
							"[~w] Returning just-loaded resource '~p'.",
							[ self(), RscId ] ) ),

					{ BinRsc, NewRscRef };

				_False ->
					trace_bridge:error_fmt( "No (logical) resource of "
						"identifier '~p', in the referential, "
						"and it is not loadable.", [ RscId ] ),
					throw( { cannot_load_resource_from, RscId } )

			end

	end;

% Second, server-side.
%
% RscId is by design not a list/string (hence is a logical identifier):
get( RscId, RscSrvPid ) when is_pid( RscSrvPid ) ->
	RscSrvPid ! { get, RscId, self() },
	receive

		{ notifyResource, Rsc } ->
			cond_utils:if_defined( myriad_debug_resources,
				trace_utils:debug_fmt( "[~w] Returning received "
					"resource for '~p'.", [ self(), RscId ] ) ),

			Rsc

	end.



% @doc Returns the resource (unspecialised) contents corresponding to the
% specified (file or logical) identifiers, based on the specified resource
% holder.
%
-spec get_multiple( [ resource_id() ], resource_referential() ) ->
				{ [ resource() ], resource_referential() };
				  ( [ resource_id() ], resource_server_pid() ) ->
											[ resource() ].
% Server-side:
get_multiple( RscIds, RscSrvPid ) when is_pid( RscSrvPid ) ->
	BestRscIds = get_best_identifiers( RscIds ),

	% Wrapping list useless:
	RscSrvPid ! { getMultiple, BestRscIds, self() },
	receive

		{ notifyResources, Rscs } ->
			cond_utils:if_defined( myriad_debug_resources,
				trace_utils:debug_fmt( "[~w] Returning received "
					"resources for '~p'.", [ self(), RscIds ] ) ),

			Rscs

	end;

% Local side:
get_multiple( RscIds, RscRef ) -> % Implicit: RscRef=#resource_referential{}
	% (resource identifiers being processed in reverse order, resources are
	% returned in the correct order):
	%
	lists:foldr( fun( RscId, _Acc={ AccRscs, AccRscRef } ) ->
					{ Rsc, NewRscRef } = get( RscId, AccRscRef ),
					{ [ Rsc | AccRscs ], NewRscRef }
				 end,
				 _Acc0={ _AccRscs=[], RscRef },
				 _List=RscIds ).



% @doc Returns the bitmap resource content corresponding to the specified (file
% or logical) identifier, based on the specified resource holder.
%
-spec get_bitmap( bitmap_resource_id(), resource_referential() ) ->
				{ bitmap(), resource_referential() };
				( bitmap_resource_id(), resource_server_pid() ) -> bitmap().
get_bitmap( BmpIdStr, RscHolder ) when is_list( BmpIdStr ) ->
	get_bitmap( text_utils:string_to_binary( BmpIdStr ), RscHolder );

% First, clauses for referential (local) side:
%
% Here, BmpId is not a list/string (generally it is either a binary string or an
% atom), for which no resource is ever loaded: either the resource is already
% registered and thus returned, or this operation fails. Referential is thus
% const.
%
get_bitmap( BmpId, BmpRef=#resource_referential{ table=RscTable } ) ->

	case table:lookup_entry( BmpId, RscTable ) of

		{ value, Bmp } ->

			cond_utils:if_defined( myriad_check_resources,
				begin

					% Not expected to happen:
					wx:is_null( Bmp ) andalso
						throw( { null_resource_bitmap, Bmp, BmpId } ),

					try

						% wxBitmap:isOk/1 likely to crash with EXIT:
						% {badarg,"This"} rather than returning false:
						%
						true = wxBitmap:isOk( Bmp )

					catch _:E ->
						% Already deallocated (gui_bitmap_destruct/1), probably
						% by mistake:
						%
						throw( { stale_resource_bitmap, Bmp, BmpId, E } )

					end

				end ),

			% Resource logical or not:
			cond_utils:if_defined( myriad_debug_resources,
				trace_utils:debug_fmt( "[~w] Returning already-available "
					"bitmap resource ~w for '~p'.",
				   [ self(), Bmp, BmpId ] ) ),

			{ Bmp, BmpRef };


		key_not_found ->
			% Then hopefully it is a binary string to allow for a loading from
			% file:
			%
			case is_binary( BmpId ) of

				true ->
					BmpPath = get_path_from_referential( BmpId, BmpRef ),

					file_utils:is_existing_file_or_link( BmpPath ) orelse
						throw( { bitmap_resource_not_found,
								 text_utils:ensure_string( BmpPath ) } ),

					Bitmap = gui_bitmap:create_from( BmpPath ),

					NewRscTable = table:add_entry( BmpId, Bitmap, RscTable ),

					% The following workaround, implemented to circumvent an
					% early deallocation, was actually useless (see
					% https://erlangforums.com/t/reference-counting-wx-object-instances/3226/):;
					% when using such a resource holder, the caller shall not
					% destruct its instances by itself (for example with
					% gui_bitmap:destruct/1):

					% Ensures that a dummy frame is available:
					% DummyFrame =
					%		case BmpRef#resource_referential.dummy_frame of

					%	undefined ->
					%		% Parentless gui_window is not a legit one, so:
					%		gui_frame:create( "MyriadGUI dummy frame" );

					%	DFrame ->
					%		DFrame

					% end,

					% We created a dummy static bitmap whose parent is / which
					% is owned by the dummy frame, to prevent that its bitmap is
					% deallocated whereas that bitmap is still held by the
					% cache:
					%
					%_StaticBmp = gui_bitmap:create_static_display( Bitmap,
					%	_Parent=DummyFrame ),

					NewBmpRef = BmpRef#resource_referential{
						table=NewRscTable },
						%dummy_frame=DummyFrame },

					cond_utils:if_defined( myriad_debug_resources,
						trace_utils:debug_fmt(
							"[~w] Returning just-loaded bitmap resource ~w "
							"for '~p'.", [ self(), Bitmap, BmpId ] ) ),

					{ Bitmap, NewBmpRef };

				_False ->
					trace_bridge:error_fmt( "No (logical) bitmap resource of "
						"identifier '~p', in the referential, "
						"and it is not loadable.", [ BmpId ] ),
					throw( { cannot_load_bitmap_resource_from, BmpId } )

			end

	end;

% Second, server-side.
%
% BmpId is by design not a list/string (hence is a logical identifier):
get_bitmap( BmpId, BmpSrvPid ) when is_pid( BmpSrvPid ) ->
	BmpSrvPid ! { getBitmap, BmpId, self() },
	receive

		{ notifyBitmapResource, Bitmap } ->
			cond_utils:if_defined( myriad_debug_resources,
				trace_utils:debug_fmt( "[~w] Returning received "
					"bitmap resource ~w for '~p'.",
					[ self(), Bitmap, BmpId ] ) ),

			Bitmap

	end.



% @doc Returns the bitmap resource contents corresponding to the specified (file
% or logical) identifiers, based on the specified resource holder.
%
% Like get_multiple/2, but for bitmaps.
%
-spec get_bitmaps( [ bitmap_resource_id() ], resource_referential() ) ->
							{ [ bitmap() ], resource_referential() };
				 ( [ bitmap_resource_id() ], resource_server_pid() ) ->
							[ bitmap() ].
% Server-side:
get_bitmaps( BitmapRscIds, RscSrvPid ) when is_pid( RscSrvPid ) ->
	BestBmpRscIds = get_best_identifiers( BitmapRscIds ),

	% Wrapping list useless:
	RscSrvPid ! { getBitmaps, BestBmpRscIds, self() },
	receive

		{ notifyBitmapResources, BmpRscs } ->
			cond_utils:if_defined( myriad_debug_resources,
				trace_utils:debug_fmt( "[~w] Returning received "
					"bitmap resources ~w for ~p.",
					[ self(), BmpRscs, BestBmpRscIds ] ) ),

			BmpRscs

	end;

% Local side; implicit: RscRef=#resource_referential{}:
get_bitmaps( BitmapRscIds, RscRef ) ->

	%trace_utils:debug_fmt( "Resource referential: ~p.", [ RscRef ] ),

	% (bitmap identifiers being processed in reverse order, bitmaps are returned
	% in the correct order):
	%
	lists:foldr( fun( BRId, _Acc={ AccBitmaps, AccRscRef } ) ->
					{ Bitmap, NewRscRef } = get_bitmap( BRId, AccRscRef ),
					{ [ Bitmap | AccBitmaps ], NewRscRef }
				 end,
				 _Acc0={ _AccBitmaps=[], RscRef },
				 _List=BitmapRscIds ).





% @doc Returns whether the resource content corresponding to the specified
% identifier is registered in the specified resource holder.
%
-spec has( resource_id(), resource_holder() ) -> boolean().
% As plain strings are tolerated:
has( RscIdStr, RscHolder ) when is_list( RscIdStr )->
	has( text_utils:string_to_binary( RscIdStr ), RscHolder );

% Non-plain string from here:
has( RscId, #resource_referential{ table=RscTable } ) ->
	table:has_entry( RscId, RscTable );

has( RscId, RscSrvPid ) ->
	RscSrvPid ! { has, RscId, self() },
	receive

		{ notifyResourceAvailability, Bool } ->
			Bool

	end.



% @doc Registers explicitly the specified logical resource(s) in the specified
% resource holder, based on either a pair made of a logical (non-plain string)
% identifier and the resource itself, or a list of such pairs.
%
% Useful for resources that cannot be loaded directly from a filesystem
% (e.g. because they have to be processed first, or are obtained through other
% means - for example through a network or if being generated as a whole).
%
% Any resource previously registered under the same identifier will be replaced.
%
-spec register( maybe_list( { resource_logical_id(), resource() } ),
				resource_referential() ) -> resource_referential();
			  ( maybe_list( { resource_logical_id(), resource() } ),
				resource_server_pid() ) -> void().
% As maybe_list:
register( RscPair={ _Id, _RSc }, RscRef ) ->
	register( [ RscPair ], RscRef );

% Referential (local) side:
register( RscPairs, RscRef=#resource_referential{ table=RscTable } )
											when is_list( RscPairs ) ->

	cond_utils:if_defined( myriad_debug_resources,
		trace_utils:debug_fmt( "[~w] Registering logical resources ~ts.",
			[ self(), text_utils:terms_to_string(
				[ I || { I, _R } <- RscPairs ] ) ] ) ),

	check_logical_ids( RscPairs ),

	NewRscTable = table:add_entries( RscPairs, RscTable ),
	RscRef#resource_referential{ table=NewRscTable };

% Server-side:
register( RscPairs, RscSrvPid ) ->
	check_logical_ids( RscPairs ),
	RscSrvPid ! { register, [ RscPairs ] }.



% @doc Checks that the resource identifiers in the specified pairs are legit
% logical ones.
%
check_logical_ids( RscPairs ) ->
	[ is_list( I ) andalso throw( { invalid_logical_resource_identifier, I } )
		|| { I, _R } <- RscPairs ].



% @doc Registers explicitly the specified (single, logical) resource, based on
% the specified logical (non-plain list) identifier, in the specified resource
% holder.
%
% Useful for resources that cannot be loaded directly from a filesystem
% (e.g. because they have to be processed first, or are obtained through other
% means - for example through a network or if being generated as a whole).
%
% Any resource previously registered under the same identifier will be replaced.
%
-spec register( resource_logical_id(), resource(), resource_referential() ) ->
											resource_referential();
			  ( resource_logical_id(), resource(), resource_server_pid() ) ->
											void().
% Referential (local) side; RscLogId must not be a plain list:
register( RscLogId, _Rsc, _RscRef ) when is_list( RscLogId ) ->
	throw( { invalid_logical_resource_identifier, RscLogId } );

% Local side:
register( RscLogId, Rsc, RscRef=#resource_referential{ table=RscTable } ) ->

	cond_utils:if_defined( myriad_debug_resources,
		trace_utils:debug_fmt( "[~w] Registering logical resource '~p', "
			"whose content is ~w.", [ self(), RscLogId, Rsc ] ) ),

	NewRscTable = table:add_entry( RscLogId, Rsc, RscTable ),
	RscRef#resource_referential{ table=NewRscTable };

% Server-side:
register( RscLogId, Rsc, RscSrvPid ) ->
	RscSrvPid ! { register, [ RscLogId, Rsc ] }.




% @doc Removes the specified resource from the specified resource holder.
%
% This resource is just unregistered; for example file-based ones are not
% deleted from the filesystem.
%
-spec remove( any_resource_file_id(), resource_referential() ) ->
										resource_referential();
			( any_resource_file_id(), resource_server_pid() ) -> void().
% As strings are tolerated:
remove( RscIdStr, RscHolder ) when is_list( RscIdStr ) ->
	remove( text_utils:string_to_binary( RscIdStr ), RscHolder );

% Non-plain list from here; local side:
remove( RscId, Ref=#resource_referential{ table=RscTable } ) ->
	case table:has_entry( RscId, RscTable ) of

		true ->
			NewRscTable = table:remove_entry( RscId, RscTable ),
			Ref#resource_referential{ table=NewRscTable };

		false ->
			throw( { non_existing_resource, RscId } )

	end;

% Server-side:
remove( RscId, RscSrvPid ) ->
	RscSrvPid ! { remove, RscId }.



% @doc Removes the specified resources from the specified resource holder.
%
% These resource are just unregistered; for example file-based ones are not
% deleted from the filesystem.
%
-spec remove_multiple( [ any_resource_file_id() ], resource_referential() ) ->
										resource_referential();
					 ( [ any_resource_file_id() ], resource_server_pid() ) ->
										void().
% Server-side (mostly like get_multiple/2):
remove_multiple( RscIds, RscSrvPid ) when is_pid( RscSrvPid ) ->
	BestRscIds = get_best_identifiers( RscIds ),

	% Wrapping list useless:
	RscSrvPid ! { removeMultiple, BestRscIds };

% Local side:
remove_multiple( RscIds, RscRef=#resource_referential{ table=RscTable } ) ->

	BestRscIds = get_best_identifiers( RscIds ),

	% No order matters; existence checked for reliability:
	ShrunkRscTable = table:remove_existing_entries( BestRscIds, RscTable ),

	%trace_utils:debug_fmt( "Before: ~ts~nAfter: ~ts",
	%  [ table:to_string( RscTable ),
	%    table:to_string( ShrunkRscTable ) ] ),

	RscRef#resource_referential{ table=ShrunkRscTable }.



% @doc Fully flushes the specified resource holder.
flush( RscSrvPid ) when is_pid( RscSrvPid ) ->
	RscSrvPid ! flush;

% No bitmap held:
flush( RscRef=#resource_referential{ table=_RscTable } ) ->
									 %dummy_frame=undefined } ) ->
	RscRef#resource_referential{ table=table:new() }.

%flush( RscRef=#resource_referential{ table=_RscTable,
%									  dummy_frame=DummyFrame } ) ->

	% Should trigger in turn the deallocation of its static bitmaps, then of
	% their bitmap resources.
	%
	% Not gui_widget:destruct(DummyFrame) as we do not need synchronisation
	% here:
	%
%	gui_widget:destruct_direct( DummyFrame ),

%	RscRef#resource_referential{ table=table:new(),
%								 dummy_frame=undefined }.





% @doc Returns the full, absolute path (not a resource per se) to the (single)
% file-based specified data whose relative path to the resource directory is
% specified; ensures that the returned file exists indeed (as a regular file or
% a symbolic link).
%
% Especially useful with register/3, in order to locate data files through the
% resource root directory, so that a logical resource is first obtained from
% them, and then registered as such.
%
-spec locate_data( any_file_path(), resource_holder() ) -> bin_file_path().
% As strings are tolerated:
locate_data( DataRelPathStr, AnyRscHolder ) when is_list( DataRelPathStr ) ->
	locate_data( text_utils:string_to_binary( DataRelPathStr ), AnyRscHolder );

% Binary string expected from here; server-side:
locate_data( BinDataRelPath, RscSrvPid ) when is_pid( RscSrvPid ) ->
	RscSrvPid ! { locateData, BinDataRelPath, self() },
	receive

		{ notifyDataLocation, BinDataAbsPath } ->
			BinDataAbsPath

	end;

% Local side:
locate_data( BinDataPath, RscRef ) ->
	locate_data_from_ref( BinDataPath, RscRef ).



% (helper for local side)
%
% Whereas no root directory set:
locate_data_from_ref( BinDataPath,
					  #resource_referential{ root_directory=undefined } ) ->
	case file_utils:is_existing_file_or_link( BinDataPath ) of

		true ->
			% Involves the current directory:
			file_utils:ensure_path_is_absolute( BinDataPath );

		_False ->
			throw( { data_file_not_found, BinDataPath,
					 file_utils:get_current_directory() } )

	end;

% Whereas a root directory was set:
locate_data_from_ref( BinDataPath,
					  #resource_referential{ root_directory=BinRootDir } ) ->

	AbsBinDataPath =
		file_utils:ensure_path_is_absolute( BinDataPath, _BasePath=BinRootDir ),

	case file_utils:is_existing_file_or_link( AbsBinDataPath ) of

		true ->
			AbsBinDataPath;

		_False ->
			throw( { data_file_not_found, BinDataPath,
					 text_utils:binary_to_string( BinRootDir ),
					 file_utils:get_current_directory() } )

	end.



% @doc Returns the full, absolute paths (not resources per se) to each of the
% file-based specified data elements whose relative path to the resource
% directory is specified; ensures that the returned files exist indeed (as
% regular files or symbolic links).
%
% Especially useful with register/3, in order to locate data files through the
% resource root directory, so that a logical resource is first obtained from
% them, and then registered as such.
%
-spec locate_multiple_data( [ any_file_path() ], resource_holder() ) ->
										[ bin_file_path() ].
locate_multiple_data( DataRelPaths, RscSrvPid ) when is_pid( RscSrvPid ) ->

	BinDataRelPaths = text_utils:ensure_binaries( DataRelPaths ),

	RscSrvPid ! { locateMultipleData, BinDataRelPaths, self() },
	receive

		{ notifyMultipleDataLocation, BinDataAbsPaths } ->
			BinDataAbsPaths

	end;

locate_multiple_data( BinDataPaths, RscRef ) ->
	locate_multiple_data_from_ref( BinDataPaths, RscRef ).



% (shared; like locate_data_from_ref/2 but context-optimised)
locate_multiple_data_from_ref( BinDataPaths, #resource_referential{
										root_directory=undefined } ) ->
	[ case file_utils:is_existing_file_or_link( P ) of

		true ->
			% Involves the current directory:
			file_utils:ensure_path_is_absolute( P );

		_False ->
			throw( { data_file_not_found, P,
					 file_utils:get_current_directory() } )

	end || P <- BinDataPaths ];


locate_multiple_data_from_ref( BinDataPaths, #resource_referential{
										root_directory=BinRootDir } ) ->

	[ begin

		  AbsBinDataPath =
			  file_utils:ensure_path_is_absolute( P, _BasePath=BinRootDir ),

		  case file_utils:is_existing_file_or_link( AbsBinDataPath ) of

			  true ->
				  AbsBinDataPath;

			  _False ->
				  throw( { data_file_not_found, P,
						   text_utils:binary_to_string( BinRootDir ),
						   file_utils:get_current_directory() } )
		  end

	end || P <- BinDataPaths ].



% @doc Returns the full, absolute path to the file-based, already-registered
% resource specified through its identifier in the specified resource holder.
%
-spec get_path( any_resource_file_id(), resource_holder() ) -> bin_file_path().
% As strings are tolerated:
get_path( RscFileIdStr, RscRef ) when is_list( RscFileIdStr ) ->
	get_path( text_utils:string_to_binary( RscFileIdStr ), RscRef );

% Binary string expected:
get_path( RscFileId, #resource_referential{ root_directory=MaybeBinRootDir,
											table=RscTable } )
								when is_binary( RscFileId ) ->

	case table:has_entry( RscFileId, RscTable ) of

		true ->
			case MaybeBinRootDir of

				undefined ->
					RscFileId;

				BinRootDir ->
					file_utils:bin_join( BinRootDir, RscFileId )

			end;

		false ->
			cond_utils:if_defined( myriad_debug_resources,
				trace_utils:error_fmt( "[~w] Resource '~ts' not known; "
					"referenced ones are: ~ts", [ self(), RscFileId,
						text_utils:terms_to_string(
							[ I || I <- table:keys( RscTable ) ] ) ] ) ),

			throw( { resource_not_known, RscFileId } )

	end;

get_path( RscFileId, RscSrvPid ) when is_binary( RscFileId ) ->
	RscSrvPid ! { getPath, RscFileId, self() },
	receive

		{ notifyResourcePath, BinPath } ->
			BinPath

	end.



% @doc Returns the (unchecked) path corresponding to the specified (file)
% resource, in the context of the specified referential.
%
% (helper)
%
-spec get_path_from_referential( any_resource_file_id(),
				resource_referential() ) -> any_resource_file_id().
get_path_from_referential( RscFileId, #resource_referential{
										root_directory=undefined } ) ->
	% Relative to current directory:
	RscFileId;

get_path_from_referential( RscFileId, #resource_referential{
										root_directory=BinRootDir } ) ->
	file_utils:bin_join( BinRootDir, RscFileId ).



% @doc Returns the best identifiers in order to communicate with a resource
% server: transforms the plain string ones into binary ones (others left
% unchanged).

% (helper)
%
-spec get_best_identifiers( [ resource_id() ] ) -> [ resource_id() ].
get_best_identifiers( RscIds ) ->
	[ case is_list( RI ) of
		true ->
			text_utils:string_to_binary( RI );

		_False ->
			RI

	  end || RI <- RscIds ].



% @doc Returns a textual description of the specified referential.
-spec referential_to_string( resource_referential() ) -> ustring().
referential_to_string( #resource_referential{ root_directory=undefined,
											  table=Rsctable } ) ->
	text_utils:format( "resource referential, not anchored to a specific "
		"root directory, storing ~ts",
		[ resource_table_to_string( Rsctable ) ] );

referential_to_string( #resource_referential{ root_directory=BinRootDir,
											  table=Rsctable } ) ->
	text_utils:format( "resource referential whose root directory is '~ts', "
		"storing ~ts", [ BinRootDir, resource_table_to_string( Rsctable ) ] ).



% @doc Returns a textual description of the specified resource table.
-spec resource_table_to_string( resource_table() ) -> ustring().
resource_table_to_string( Rsctable ) ->
	case table:keys( Rsctable ) of

		[] ->
			"no resource";

		[ RscId ] ->
			text_utils:format( "a single ~ts resource, '~p'",
							   [ resource_type_to_string( RscId ), RscId ] );

		RscIds ->
			text_utils:format( "~B resources: ~ts", [ length( RscIds ),
				text_utils:strings_to_string( [ text_utils:format(
					"~ts resource '~p'",
					[ resource_type_to_string( RI ), RI ] )
						|| RI <- RscIds ] ) ] )

	end.



% @doc Returns a textual description of the type of the specified resource.
-spec resource_type_to_string( resource_id() ) -> ustring().
resource_type_to_string( RscId )
							when is_list( RscId ) orelse is_binary( RscId ) ->
	"file";

resource_type_to_string( _RscId ) ->
	"logical".






% Resource server implementation.

% Initialises the resource server.
-spec server_init() -> no_return().
server_init() ->
	InitialRef = create_referential(),
	server_main_loop( InitialRef ).


% Initialises the resource server with specified root directory.
-spec server_init( bin_directory_path() ) -> no_return().
server_init( BinRootDir ) ->
	server_init( BinRootDir, _MaybeGUIBackendEnv=undefined ).


% Initialises the resource server with specified root directory and maybe-GUI
% environment.
%
-spec server_init( bin_directory_path(), maybe( gui:backend_environment() ) ) ->
										no_return().
server_init( BinRootDir, _MaybeGUIBackendEnv=undefined ) ->
	InitialRef = create_referential( BinRootDir ),
	server_main_loop( InitialRef );

server_init( BinRootDir, GUIBackendEnv ) ->
	gui:set_backend_environment( GUIBackendEnv ),
	server_init( BinRootDir, _MaybeGUIBackendEnv=undefined ).



% Message-based interactions, made according to conventions similar to the
% Ceylan-WOOPER ones.
%
% When an operation fails, no answer is sent to the caller, and the resource
% server fails. As a result, it is better to create linked instances thereof.
%
% (helper)
-spec server_main_loop( resource_referential() ) -> no_return().
server_main_loop( RscRef ) ->

	receive

		% Requests:
		{ get, RscId, SenderPid } ->
			{ Rsc, NewRscRef } = get( RscId, RscRef ),
			SenderPid ! { notifyResource, Rsc },
			server_main_loop( NewRscRef );

		{ getMultiple, RscIds, SenderPid } ->
			{ Rscs, NewRscRef } = get_multiple( RscIds, RscRef ),
			SenderPid ! { notifyResources, Rscs },
			server_main_loop( NewRscRef );


		% Requests:
		{ getBitmap, BitmapRscId, SenderPid } ->
			{ Bitmap, NewRscRef } = get_bitmap( BitmapRscId, RscRef ),
			SenderPid ! { notifyBitmapResource, Bitmap },
			server_main_loop( NewRscRef );

		{ getBitmaps, BitmapRscIds, SenderPid } ->
			{ Bitmaps, NewRscRef } = get_bitmaps( BitmapRscIds, RscRef ),
			SenderPid ! { notifyBitmapResources, Bitmaps },
			server_main_loop( NewRscRef );


		% Requests:
		{ has, RscId, SenderPid } ->
			Bool = has( RscId, RscRef ),
			SenderPid ! { notifyResourceAvailability, Bool },
			server_main_loop( RscRef );

		% Oneways:
		{ register, [ RscPairs ] } ->
			NewRscRef = register( RscPairs, RscRef ),
			server_main_loop( NewRscRef );

		{ register, [ RscLogId, Rsc ] } ->
			NewRscRef = register( RscLogId, Rsc, RscRef ),
			server_main_loop( NewRscRef );


		% Oneways:
		{ remove, RscId } ->
			ShrunkRscRef = remove( RscId, RscRef ),
			server_main_loop( ShrunkRscRef );

		{ removeMultiple, RscIds } ->
			ShrunkRscRef = remove_multiple( RscIds, RscRef ),
			server_main_loop( ShrunkRscRef );


		% Request:
		{ locateData, RelFilePathBin, SenderPid } ->
			BinDataAbsPath = locate_data_from_ref( RelFilePathBin, RscRef ),
			SenderPid ! { notifyDataLocation, BinDataAbsPath },
			server_main_loop( RscRef );

		% Request:
		{ locateMultipleData, RelFilePathBins, SenderPid } ->
			BinDataAbsPaths =
				locate_multiple_data_from_ref( RelFilePathBins, RscRef ),
			SenderPid ! { notifyMultipleDataLocation, BinDataAbsPaths },
			server_main_loop( RscRef );

		% Request:
		{ getPath, RscFileId, SenderPid } ->
			BinRscPath = get_path( RscFileId, RscRef ),
			SenderPid ! { notifyResourcePath, BinRscPath },
			server_main_loop( RscRef );

		flush ->
			FlushedRscRef = flush( RscRef ),
			server_main_loop( FlushedRscRef );

		terminate ->
			ok;

	Other ->
		trace_utils:warning_fmt( "Resource server ~w ignoring the following "
			"message:~n  ~p.", [ self(), Other ] ),
		server_main_loop( RscRef )

	end.
