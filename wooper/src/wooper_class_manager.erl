% Copyright (C) 2007-2026 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER library.
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
% Creation date: Friday, July 12, 2007.

-module(wooper_class_manager).

-moduledoc """
Module corresponding to the WOOPER **class manager singleton**.

The purpose of this process is, on a per-node basis, to create and notably to
serve to instances the virtual table corresponding to the actual class they are
corresponding to.

This way each virtual table is computed only once per node, and no significant
per-instance memory is used for the virtual table: all the instances of a given
class just refer to a common virtual table stored by this manager, and each
virtual table and the table of virtual tables itself are optimised, with regard
to their respective load factor.

Local registration only of this manager, as we want the WOOPER instances to
benefit from a local direct reference to the same method table (theoretically),
rather to waste memory with one copy of the table per instance (which is now not
anymore the case in practice, since using persistent_term).

In a distributed context, there should be exactly one class manager per node.

The class manager may be launched:

- either after an explicit, OTP-compliant initialisation phase (supervisor,
gen_server, `init/1` callback, etc.)

- or implicitly, as done before the OTP integration: then done on-demand (as
soon as a first WOOPER instance is created, hence with no a priori explicit
initialisation)
""".


% See documentation at http://wooper.esperide.org.


% Implementation notes:

% Each WOOPER instance is expected to request (through a getClassKey
% "request") this class manager for its virtual table key.
%
% When integrating OTP, this class manager became a gen_server (of course it
% could not be a WOOPER instance itself), rather than as a supervisor_bridge.
%
% As such, a getClassKey inquiry could have been implemented as an handle_call
% or an handle_info. We preferred the former, so that it could be triggered
% thanks to our get_class_key/1 function below in a classical OTP way.


% So we retrofitted the class manager into a gen_server for (optional) OTP
% compliance:
%
% (see wooper_enable_otp_integration in wooper_defines_exports.hrl)
%
-behaviour(gen_server).


% Service API:
-export([ start/0, start_link/0, start/1, start_link/1,
          get_existing_manager/0, get_class_key/1, get_virtual_table/1,
          display/0, stop/0 ]).


% Non-OTP API:
-export([ get_manager/0, stop_automatic/0, ping/1 ]).



% gen_server callbacks:
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).


% Local type:

-type manager_pid() :: pid().

-export_type([ manager_pid/0 ]).



% For wooper_class_manager_name:
-include("wooper_class_manager.hrl").


% For wooper_table_type:
-include("wooper_defines_exports.hrl").


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").


-doc "A table keeping track, for each class, of its virtual table.".
-type class_table() ::
    ?wooper_table_type:?wooper_table_type( classname(), virtual_table() ).


-doc "State kept by the WOOPER class manager.".
-type manager_state() :: class_table(). % Just all known virtual tables.



% Approximate average method count for a given class, including inherited ones.
%
% (ideally should be slightly above the maximum number of actual methods)
%
-define( wooper_method_count_upper_bound, 32 ).



% Approximate average class count for the program.
%
% (ideally should be slightly above the maximum number of actual classes being
% instantiated)
%
-define( wooper_class_count_upper_bound, 128 ).


% Comment/uncomment to respectively disable and enable debug mode:
%-define(debug,).


-define( log_prefix, "[WOOPER class manager] " ).


% In seconds:
-define( registration_time_out, 5 ).



% naming_utils:wait_for_local_registration_of/2 used:
-define( manager_registration_scope, local_only ).

% Better local, as more efficient and any per-node duplication is not a problem:
-define( manager_lookup_scope, local ).



% To avoid warnings (note: display/1 is apparently a BIF, renamed to
% display_msg/1):
%
-export([ display_state/1, display_table_creation/1,
          display_msg/1, display_msg/2 ]).



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type classname() :: wooper:classname().
-type class_key() :: wooper:class_key().
-type virtual_table() :: wooper:virtual_table().


% Uncomment to activate debug mode:
%-define(wooper_debug_class_manager,).


-spec display_state( manager_state() ) -> void().
-spec display_table_creation( basic_utils:module_name() ) -> void().

-spec display_msg( ustring() ) -> void().
-spec display_msg( text_utils:format_string(), text_utils:format_values() ) ->
                                            void().


-ifdef(wooper_debug_class_manager).


% doc: Displays the manager state.
display_state( _State=ClassTable ) ->
    wooper:log_info( ?log_prefix "Storing now ~B table(s).~n",
                     [ ?wooper_table_type:size( ClassTable ) ] ).


% doc: Displays virtual table information.
display_table_creation( Classname ) ->
    wooper:log_info( ?log_prefix "Creating a virtual table "
                     "for class ~ts.~n", [ Classname ] ).


% doc: Displays the specified log message.
display_msg( String ) ->
    Message = text_utils:format( ?log_prefix "~ts~n", [ String ] ),
    wooper:log_info( Message ).


% doc: Displays the specified formatted log message.
display_msg( FormatString, Values ) ->
    Message = text_utils:format( ?log_prefix ++ FormatString ++ "~n", Values ),
    wooper:log_info( Message ).


-else. % wooper_debug_class_manager


-doc "Displays the manager state.".
display_state( _State=_ClassTable ) ->
    ok.

-doc "Displays virtual table information.".
display_table_creation( _Classname ) ->
    ok.

-doc "Displays the specified log message.".
display_msg( _String ) ->
    ok.

-doc "Displays the specified formatted log message.".
display_msg( _FormatString, _Values ) ->
    ok.


-endif. % wooper_debug_class_manager




-doc "Typically returned after a launching described in `wooper_sup:init/1`.".
-type start_return() :: { 'ok', Child :: manager_pid() }.


% OTP-related section: first, the user-level API.


%% Explicit, OTP-based initialisation.


-doc """
Starts a new, blank, class manager, with no listening client, and returns its
PID.
""".
-spec start() -> start_return().
start() ->
    start( _MaybeClientPid=undefined ).



-doc """
Starts and links a new, blank, class manager, with no listening client, and
returns its PID.
""".
-spec start_link() -> start_return().
start_link() ->
    start_link( _MaybeClientPid=undefined ).



-doc """
Starts a new, blank, class manager, with possibly a listening client, and
returns its PID.
""".
-spec start( option( pid() ) ) -> start_return().
start( MaybeClientPid ) ->

    % A client process might be useful for testing.

    case gen_server:start( { local, ?wooper_class_manager_name },
                           ?MODULE, _Args=[], _Opts=[] ) of

        Success={ ok, ManagerPid } ->
            MaybeClientPid =:= undefined orelse
                ( MaybeClientPid ! { wooper_class_manager_pid, ManagerPid } ),
            Success;

        % Typically {error,Reason} or ignore:
        Unexpected ->
            throw( { wooper_class_manager_creation_failed, Unexpected } )

    end.



-doc """
Starts and links a new, blank, class manager, with possibly a listening client,
and returns its PID.

(same as `start/1` except for the link)
""".
-spec start_link( option( pid() ) ) -> start_return().
start_link( MaybeClientPid ) ->

    %trace_utils:debug( "Starting and linking the WOOPER class manager." ),

    % A client process might be useful for testing.

    case gen_server:start_link( { local, ?wooper_class_manager_name },
                                ?MODULE, _Args=[], _Opts=[] ) of

        Success={ ok, ManagerPid } ->
            %trace_utils:debug_fmt( "WOOPER class manager created, as ~w.",
            %                       [ ManagerPid ] ),

            MaybeClientPid =:= undefined orelse
                ( MaybeClientPid ! { wooper_class_manager_pid, ManagerPid } ),

            Success;

        % Typically {error,Reason} or ignore:
        Unexpected ->
            trace_bridge:error_fmt( "The WOOPER class manager could not be "
                                    "created: ~w.", [ Unexpected ] ),
            throw( { wooper_class_manager_creation_failed, Unexpected } )

    end.



-doc """
Returns (possibly after some waiting) a supposedly already-existing WOOPER class
manager.
""".
-spec get_existing_manager() -> manager_pid().
get_existing_manager() ->
    naming_utils:wait_for_local_registration_of( ?wooper_class_manager_name,
                                                 ?registration_time_out ).



-doc """
Returns the key corresponding to the virtual table associated to the specified
classname.

To be called when knowing that the corresponding class is not already
registered; otherwise prefer `wooper_utils:get_virtual_table_for/1`.
""".
-spec get_class_key( classname() ) -> class_key().
get_class_key( Classname ) ->

    % Rather than specifying the registered name of the class manager
    % (?wooper_class_manager_name) in the call, we attempt to fetch directly its
    % PID: this will not change anything for the OTP case (where this manager
    % shall be already available, as being launched as soon as WOOPER is
    % started), but it allows to support the non-OTP case (where this manager
    % may be spawned automatically, only the first time it becomes necessary)

    % Not done, as would create and put the manager in its non-OTP loop:
    %ClassManagerPid = get_manager(),

    ClassManagerPid = get_manager_through_otp(),

    % Still respect the OTP conventions:
    % Previously the virtual table was fetched as a message:
    % { ok, Table } = ...
    %
    % Then we just fetch the corresponding key in persistent_term:
    { ok, ClassPersistentKey } =
        gen_server:call( ClassManagerPid, { getClassKey, Classname } ),

    % Directly evaluated in the instance process:
    %persistent_term:get( ClassPersistentKey ).

    % Just returning the key now:
    ClassPersistentKey.



-doc """
Returns the virtual table (created if needed) corresponding to the specified
classname.
""".
-spec get_virtual_table( classname() ) -> virtual_table().
get_virtual_table( Classname ) ->

    % As a side effect, ensures that the corresponding virtual table exists:
    ClassKey = get_class_key( Classname ),

    % So shall be found by design:
    persistent_term:get( ClassKey ).



-doc """
Returns the PID of the WOOPER class manager.

We do the same as `get_manager/0` (including w.r.t. to multiple, simultaneous
launches), but with a gen_server-based procedure.

(note that performing, if possible, an explicit start is better, as it prevents
simultaneous, clashing launches)
""".
-spec get_manager_through_otp() -> manager_pid().
get_manager_through_otp() ->

    case naming_utils:is_registered( ?wooper_class_manager_name,
                                     ?manager_lookup_scope ) of

        not_registered ->
            % We have to launch, hopefully with no clash with other launchings:
            try

                %trace_utils:debug( "OTP start of the WOOPER class manager." ),

                { ok, ManagerPid } = start( _MaybeClientPid=undefined ),
                ManagerPid

            catch _:_E ->

                %trace_utils:debug_fmt( "OTP start of the WOOPER class "
                %   "manager failed (~p), assuming simultaneous launches, "
                %   "waiting for one to succeed.", [ E ] ),

                % Returning the PID of the winner, whichever it is:
                get_existing_manager()

            end;

        ManagerPid ->
            ManagerPid

    end.



-doc "Displays runtime information about the class manager.".
-spec display() -> void().
display() ->
    gen_server:cast( ?wooper_class_manager_name, display ).



-doc "Stops (the OTP-way) the class manager.".
-spec stop() -> void().
stop() ->
    trace_bridge:debug_fmt( "Stopping (OTP) the WOOPER class manager ~p.",
                            [ self() ] ),
    gen_server:cast( ?wooper_class_manager_name, stop ).






% See loop/1 for the counterpart to gen_server callbacks.


-doc """
Inits the manager (gen_server callback).

(also used by the non-OTP mode of operation)
""".
-spec init( any() ) -> { 'ok', manager_state() }.
init( _Args=[] ) ->

    display_msg( "Starting (init) WOOPER class manager on node ~ts (PID: ~w).",
                 [ node(), self() ] ),

    % With this creation procedure, we are supposed to start from scratch
    % through an initialisation phase (as opposed to uncoordinated creations
    % from instances that may overlap), so by design no concurrent launch is
    % expected to happen here.

    % Registering already done by gen_server:start*.

    % Should this manager be overwhelmed by instance creations:
    erlang:process_flag( message_queue_data, off_heap ),

    % Infinite time-out:
    { ok, get_initial_manager_state() }.



-doc """
Handles OTP-based call requests (gen_server callback, triggered by
`get_class_key/1` in this module).

Also, as a side effect, ensures that the specified class has its virtual table
registered.
""".
handle_call( { getClassKey, Classname }, _From, _State=ClassTable ) ->

    display_msg( "handle_call: getClassKey for ~ts.", [ Classname ] ),

    { NewClassTable, TargetClassKey } =
        get_virtual_table_key_for( Classname, ClassTable ),

    { reply, { ok, TargetClassKey }, _NewState=NewClassTable }.



-doc "Handles OTP-based oneways (gen_server callback).".
handle_cast( stop, State ) ->

    display_msg( "handle_cast: stop." ),

    stop_common(),

    { stop, normal, State };

handle_cast( display, State=ClassTable ) ->

    wooper:log_info( ?log_prefix "Internal state is: ~ts~n",
                     [ ?wooper_table_type:to_string( ClassTable ) ] ),

    % Const:
    { noreply, State }.



-doc "Handles OTP-based info requests (gen_server callback).".
handle_info( { getClassKey, _Classname, _FromPid }, _State=_ClassTable ) ->

    trace_bridge:error( "WOOPER class manager called according to the "
        "non-OTP conventions, whereas is running as an (OTP) gen_server." ),

    % (for the caller, see wooper:retrieve_virtual_table_key/1)

    throw( otp_integration_mismatch );

handle_info( Info, State ) ->

    trace_bridge:warning_fmt( "The WOOPER class manager received an "
        "unexpected, hence ignored, handle_info message:~n  ~p.", [ Info ] ),

    { noreply, State }.



-doc """
Handles the `terminate` optional callback.
""".
% Catch any normal terminate beforehand, if needed.
terminate( Reason, _State ) ->
    trace_bridge:error_fmt( "WOOPER class manager terminated; reason:~n ~p",
                           [ Reason ] ).



-doc """
Handles the optional `code change` callback.
""".
code_change( OldVsn, State, Extra ) ->

    trace_bridge:info_fmt( "Code change for the WOOPER class manager; "
        "old version is ~w, and extra information is ~w", [ OldVsn, Extra ] ),

    { ok, State }.





% Non-OTP section: WOOPER's base/historic mode of operation.



%% Implicit, non-OTP initialisation.


-doc """
Returns the PID of the WOOPER class manager.

If it is already running, finds it and returns its PID, otherwise launches it
(in an ad hoc, non-OTP way), and returns the relevant PID as well (even if
multiple instances thereof were concurrently spawned).
""".
-spec get_manager() -> manager_pid().
get_manager() ->

    % Could have been named start_automatic/0 as well.

    case naming_utils:is_registered( ?wooper_class_manager_name,
                                     ?manager_lookup_scope ) of

        not_registered ->

            %trace_utils:debug(
            %  "WOOPER class manager not available, starting it." ),

            % Not linking, at least for consistency with the case where it is
            % already launched:
            %
            % (init_automatic/0 not exported)
            %
            ?myriad_spawn( fun() -> init_automatic() end ),

            % We do not return readily the PID of the just-spawned process, as
            % its launch might be concurrent with other, quasi-simultaneous
            % launches. We report only the one that is for sure the winner (the
            % others areto terminate immediately):
            %
            naming_utils:wait_for_local_registration_of(
                ?wooper_class_manager_name, ?registration_time_out );


        ManagerPid ->

            %trace_utils:debug(
            %  "WOOPER class manager available, returning it." ),

            ManagerPid

    end.



-doc "Initialises the class manager, when launched in a non-OTP way.".
-spec init_automatic() -> no_return().
init_automatic() ->

    % Two first WOOPER instances being created nearly at the same time might
    % trigger the creation of two class managers, should the second instance
    % detect that no manager is registered, whereas the first manager is created
    % but not registered yet. That would result in superfluous class
    % managers. Up to one should exist.
    %
    % Note: as the register call cannot be performed at once (not an atomic
    % operation), there must remain a tiny window for a race condition to
    % happen).
    %
    try naming_utils:register_as( self(), ?wooper_class_manager_name,
                                  ?manager_registration_scope ) of

        % Registration success, we are the one, and we use our custom main loop:
        _ ->
            { ok, InitialState } = init( _Args=[] ),
            loop( InitialState )

    catch

        % Catches only the case where a manager was already registered; let it
        % be the only one and, stop the current one:
        %
        throw:{ local_registration_failed, _Name, already_registered,
                _OtherPid } ->

            % The client instances should use the first manager only.
            %
            % (no looping performed, hence terminating this second, extraneous
            % manager)
            ok

    end.



-doc "Stops the class manager, when not using the OTP way.".
-spec stop_automatic() -> void().
stop_automatic() ->

    case naming_utils:is_registered( ?wooper_class_manager_name, local ) of

        not_registered ->
            trace_bridge:warning( "No WOOPER class manager to stop." );

        ManagerPid ->
            display_msg( "Stopping WOOPER class manager." ),
            ManagerPid ! stop

    end.




% Section common to all kinds of modes of operation (OTP or not).


-doc """
Returns the initial state of this manager, that is an (initially empty) table of
per-class virtual tables.
""".
-spec get_initial_manager_state() -> manager_state().
get_initial_manager_state() ->
    % Empty class table:
    ?wooper_table_type:new( ?wooper_class_count_upper_bound ).



-doc """
Stops the class manager.

Used by all modes of operation (OTP or not).
""".
-spec stop_common() -> void().
stop_common() ->
    display_msg( "Stopping the WOOPER class manager." ).



-doc """
Handles the manager main loop; serves virtual tables on request (mostly on
instances creation).
""".
-spec loop( class_table() ) -> no_return() | 'ok' .
loop( ClassTable ) ->

    display_state( ClassTable ),

    receive

        { getClassKey, Classname, Pid } ->

            { NewClassTable, TargetClassKey } =
                get_virtual_table_key_for( Classname, ClassTable ),

            Pid ! { wooper_class_key, TargetClassKey },

            loop( NewClassTable );


        display ->
            wooper:log_info( ?log_prefix "Internal state is: ~ts~n",
                             [ ?wooper_table_type:to_string( ClassTable ) ] ),
            loop( ClassTable );


        stop ->
            unregister( ?wooper_class_manager_name ),
            stop_common();


        Unexpected ->

            trace_bridge:error_fmt( "The WOOPER class manager received an "
                "unexpected, thus ignored, message: ~w", [ Unexpected ] ),

            loop( ClassTable )

    end.



-doc """
Looks-up the specified classname in the specified class table: secures that such
an entry exists (creating it if needed), and returns its corresponding key.

If found, returns its key immediately, otherwise constructs it, stores the
result and returns its key in the `persistent_term` registry.

Allows to synchronise the instances so that they can know for sure that their
virtual tables are available.

Returns a pair formed of the new table of virtual tables and of the requested
table key.
""".
-spec get_virtual_table_key_for( classname(), class_table() ) ->
                                            { class_table(), class_key() }.
get_virtual_table_key_for( Classname, ClassTable ) ->

    ClassKey = wooper_utils:get_persistent_key_for( Classname ),

    case ?wooper_table_type:has_entry( Classname, ClassTable ) of

        true ->
            % Cache hit, no change in internal data:
            { ClassTable, ClassKey };

        false ->

            % Time to create this virtual table and to store it:
            display_table_creation( Classname ),

            ClassVirtualTable = create_virtual_table_for( Classname ),

            %trace_utils:debug_fmt( "Persistent registry before addition "
            %   "of ~ts: ~p", [ Classname, persistent_term:info() ] ),

            % Apparently sufficient to handle from now on a reference:
            persistent_term:put( ClassKey, ClassVirtualTable ),

            %trace_utils:debug_fmt( "Persistent registry after addition "
            %   "of ~ts: ~p", [ Classname, persistent_term:info() ] ),

            % Not using ClassTable anymore, switching to the following
            % reference instead (value still being virtual_table()):
            %
            ClassTableRef = persistent_term:get( ClassKey ),

            % Uncomment to report some information about this virtual table:
            %TableSize = system_utils:get_size( ClassTableRef ),

            %trace_utils:debug_fmt( "For class '~ts', returning a table whose "
            %   "size is ~ts (~B bytes): ~ts",
            %   [ Classname, system_utils:interpret_byte_size( TableSize ),
            %     TableSize, table:to_string( ClassTableRef ) ] ),

            %trace_utils:debug_fmt( "Virtual table for ~ts: ~ts",
            %   [ Classname, ?wooper_table_type:to_string( ClassTableRef ) ] ),

            % No need anymore to optimise any table.

            % Here the table could be patched with destruct/1, if defined.
            NewClassTable = ?wooper_table_type:add_entry( _K=Classname,
                _V=ClassTableRef, ClassTable ),

            % And the table of virtual tables was itself optimised each time a
            % new class was introduced:
            %
            { NewClassTable, ClassKey }

    end.


-doc """
Creates recursively (indirectly thanks to `update_virtual_table_with/2`) the
virtual table corresponding to the specified class.
""".
-spec create_virtual_table_for( classname() ) -> virtual_table().
create_virtual_table_for( TargetClassname ) ->
    lists:foldl(
        fun( Classname, VTable ) ->
            update_virtual_table_with( Classname, VTable )
        end,
        create_local_virtual_table_for( TargetClassname ),
        TargetClassname:get_superclasses() ).



-doc """
Updates the specified virtual table with the methods of the specified class
(that is: precomputes the virtual table for the related class).

In case of key collision, the values specified in `VirtualTable` have priority
over the ones relative to `tClassname`. Hence methods redefined in child classes
are selected, rather than the ones of the mother class.
""".
-spec update_virtual_table_with( classname(), virtual_table() ) ->
                                                virtual_table().
update_virtual_table_with( Classname, VirtualTable ) ->
    ?wooper_table_type:merge( VirtualTable,
                              create_virtual_table_for( Classname ) ).



-doc """
Tells whether a function `Name/Arity` should be registered into a method virtual
table.
""".
select_function( _Name, _Arity=0 )                                    -> false ;

select_function( new,_ )                                              -> false ;
select_function( new_link,_ )                                         -> false ;
select_function( new_passive,_ )                                      -> false ;

select_function( synchronous_new,_ )                                  -> false ;
select_function( synchronous_new_link,_ )                             -> false ;
select_function( synchronous_timed_new,_ )                            -> false ;
select_function( synchronous_timed_new_link,_ )                       -> false ;

select_function( remote_synchronisable_new,_ )                        -> false ;
select_function( remote_synchronisable_new_link,_ )                   -> false ;

select_function( remote_new,_ )                                       -> false ;
select_function( remote_new_link,_ )                                  -> false ;
select_function( remote_synchronous_new,_ )                           -> false ;
select_function( remote_synchronous_new_link,_ )                      -> false ;
select_function( remote_synchronous_timed_new,_ )                     -> false ;
select_function( remote_synchronous_timed_new_link,_ )                -> false ;

select_function( construct,_ )                                        -> false ;
select_function( destruct,1 )                                         -> false ;
select_function( delete_any_instance_referenced_in,_ )                -> false ;
select_function( delete_synchronously_any_instance_referenced_in,_ )  -> false ;
select_function( delete_synchronously_instances,_ )                   -> false ;

select_function( wooper_construct_and_run,_ )                         -> false ;
select_function( wooper_construct_and_run_synchronous,_ )             -> false ;
select_function( wooper_debug_listen,_ )                              -> false ;
select_function( wooper_destruct,_ )                                  -> false ;
select_function( wooper_display_instance,_ )                          -> false ;
select_function( wooper_display_loop_state,_ )                        -> false ;
select_function( wooper_display_state,_ )                             -> false ;
select_function( wooper_display_virtual_table,_ )                     -> false ;
select_function( wooper_get_all_attributes,_ )                        -> false ;
select_function( wooper_get_state_description,_ )                     -> false ;
select_function( wooper_get_virtual_table_description,_ )             -> false ;
select_function( wooper_pop_from_attribute,_ )                        -> false ;
select_function( wooper_effective_method_execution,4 )                -> false ;
select_function( wooper_execute_method,3 )                            -> false ;
select_function( wooper_execute_method_as,4 )                         -> false ;

% Might be useful, currently still enabled:
%select_function( wooper_get_instance_description,1 )                 -> false ;

select_function( wooper_handle_local_oneway_execution,3 )             -> false ;
select_function( wooper_handle_local_request_execution,3 )            -> false ;
select_function( wooper_handle_remote_oneway_execution,3 )            -> false ;
select_function( wooper_handle_remote_request_execution,4 )           -> false ;
select_function( wooper_main_loop,1 )                                 -> false ;

select_function( addKeyValueToAttribute, 4 )                          -> false ;
select_function( addToAttribute, 3 )                                  -> false ;
select_function( appendToAttribute, 3 )                               -> false ;
select_function( concatToAttribute, 3  )                              -> false ;
select_function( decrementAttribute, 2 )                              -> false ;
select_function( deleteFromAttribute, 3 )                             -> false ;
select_function( getAttribute, 2 )                                    -> false ;
select_function( getAttributes, 2 )                                   -> false ;
select_function( hasAttribute, 2 )                                    -> false ;
select_function( incrementAttribute, 2 )                              -> false ;
select_function( popFromAttribute, 2 )                                -> false ;
select_function( removeAttribute, 2 )                                 -> false ;
select_function( setAttribute, 3 )                                    -> false ;
select_function( setAttributes, 2 )                                   -> false ;
select_function( swapInAttribute, 3 )                                 -> false ;
select_function( subtractFromAttribute, 3 )                           -> false ;
select_function( toggleAttribute, 2 )                                 -> false ;

select_function( post_deserialise_hook, 1 )                           -> false ;
select_function( post_serialise_hook, 3 )                             -> false ;
select_function( pre_deserialise_hook, 2 )                            -> false ;
select_function( pre_serialise_hook, 1 )                              -> false ;

select_function( executeOneway,_ )                                    -> false ;
select_function( executeConstOneway,_ )                               -> false ;
select_function( executeOnewayAs,_ )                                  -> false ;
select_function( executeConstOnewayAs,_ )                             -> false ;

select_function( executeRequest,_ )                                   -> false ;
select_function( executeConstRequest,_ )                              -> false ;
select_function( executeRequestAs,_ )                                 -> false ;
select_function( executeConstRequestAs,_ )                            -> false ;

select_function( module_info,1)                                       -> false ;
% Includes 'wooper_get_instance_description/1', which could be useful to debug:
select_function( _, _ )                                               -> true.



-doc """
Returns a table appropriate for method look-up, for the specified class.
""".
-spec create_local_virtual_table_for( classname() ) -> virtual_table().
create_local_virtual_table_for( Classname ) ->

    % Typically if Classname is misspelled:
    Exports = try
                Classname:module_info( exports )
              catch

                error:undef ->
                    trace_bridge:error_fmt( "Unable to find a module "
                        "corresponding to '~ts', knowing that ~ts~n"
                        "Hint: check the BEAM_DIRS make variable and any "
                        "application-level setting that specifies code that "
                        "shall be used or deployed.",
                        [ Classname, code_utils:get_code_path_as_string() ] ),
                    throw( { class_not_found, Classname } )

              end,

    % Filter-out functions that should not be callable via RMI (Remote Method
    % Invocation):
    %
    lists:foldl(
        fun( { Name, Arity }, VTable ) ->
            case select_function( Name, Arity ) of

                true ->
                    ?wooper_table_type:add_entry( { Name, Arity }, Classname,
                                                  VTable );

                false ->
                    VTable

            end
        end,
        _InitVTable=?wooper_table_type:new( ?wooper_method_count_upper_bound ),
        _List=Exports ).



-doc """
Pings the specified WOOPER instance, designated by its PID or registered name
(locally, otherwise, if not found, globally).

Returns `pong` if it could be successfully ping'ed, otherwise returns `pang`.
""".
-spec ping( naming_utils:registration_name() | wooper:instance_pid() ) ->
                                                    'pong' | 'pang'.
ping( Target ) when is_pid( Target ) ->

    Target ! { ping, self() },
    receive

        { pong, Target } ->
            pong

        after 500 ->
            pang

    end;


ping( Target ) when is_atom( Target ) ->

    case naming_utils:is_registered( Target ) of

        not_registered ->
            pang;

        Pid ->
            ping( Pid )

    end.
