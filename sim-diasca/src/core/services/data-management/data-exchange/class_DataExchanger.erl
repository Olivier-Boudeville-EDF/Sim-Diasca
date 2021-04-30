% Copyright (C) 2011-2021 EDF R&D

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


-module(class_DataExchanger).


-define( class_description,
		 "The data exchanger offers extra communication patterns within the "
		 "simulation (in addition to pure actor oneways). "
		 "For all general documentation regarding the data-exchanging "
		 "service, please refer to the 'Data Exchange' section of the "
		 "Sim-Diasca Technical Manual." ).


% Determines what are the direct mother classes of this class (if any):
% (not an actor!)
%
-define( superclasses, [ class_EngineBaseObject ] ).


% Attributes that are specific to a data-exchanger instance are:
-define( class_attributes, [

	{ simulation_running, boolean(), "tells whether the simulation is running "
	  "(and thus inter-diasca commits and updates have to be performed) or "
	  "not (then direct commits and updates)" },

	{ root_exchanger_pid, root_manager_pid(),
	  "PID of the root data exchanger (possibly self())" },

	{ parent_exchanger_pid, union( data_exchanger_pid(), 'none' ),
	  "either the PID of the parent exchanger (if any), or the 'none' atom, "
	  "if being the root exchanger" },

	{ child_exchangers, [ data_exchanger_pid() ],
	  "a list of the PID of the direct child exchangers, if any" },

	{ json_parser_state, maybe( json_utils:parser_state() ),
	  "stores the current state of the JSON parser (if any); incidently "
	  "tells whether the reading of JSON files is enabled (note that "
	  "*all* nodes are expected to have this support enabled for this feature "
	  "to be deemed available)" },

	{ feeder_files, [ file_utils:bin_file_name() ], "a list of the paths "
	  "(as binary strings) relative to the root of the simulation archive, "
	  "corresponding to the static information stored in deployed files, "
	  "that were read by all exchangers" },

	{ data_table, data_table(), "a table whose "
	  "keys are the ones of the data entry, and whose values are { Value, "
	  "Qualifier } pairs, where Value is the data associated to the key and "
	  "Qualifier is either 'const' or 'mutable'" },

	{ root_time_manager_pid, time_manager_pid(), "holds the PID of the root "
	  "time manager, with whom the root data exchanger interacts" },

	{ pending_commits, qualified_entries(), "a list of current pending "
	  "{ Key, Value, Qualifier } commit elements waiting for the current "
	  "diasca to be over so that they can be applied in the whole exchanger "
	  "hierarchy" },

	{ interdiasca_requested, boolean(), "tells whether this (root) exchanger "
	  "already requested the (root) time manager to trigger an inter-diasca "
	  "commit propagation phase" } ] ).


-type qualifier() :: 'const' | 'mutable'.


-type key() :: table:key().

-type value() :: table:value().

-type qualified_value() :: { value(), qualifier() }.


-type entry() :: { key(), value() }.

-type entries() :: [ entry() ].


-type qualified_entry() :: { key(), value(), qualifier() }.

-type qualified_entries() :: [ qualified_entry() ].


-type qualified_basic_entry() :: { key(), qualified_value() }.

-type qualified_basic_entries() :: [ qualified_basic_entry() ].


-type mixed_entry() :: entry() | qualified_entry().

-type mixed_entries() :: [ mixed_entry() ].

-type data_table() :: table( key(), qualified_value() ).


% Designates the PID of a data-exchanger:
-type data_exchanger_pid() :: sim_diasca:agent_pid().

% Could be changed later in, say, a record:
-opaque exchange_settings() :: { data_exchanger_pid(), data_exchanger_pid() }.


-export_type([ qualifier/0, key/0, value/0, qualified_value/0,
			   entry/0, entries/0, qualified_entry/0, qualified_entries/0,
			   qualified_basic_entry/0, qualified_basic_entries/0,
			   mixed_entry/0, mixed_entries/0, data_exchanger_pid/0,
			   exchange_settings/0 ]).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_EngineBaseObject header:
-define( trace_emitter_categorization, "Core.DataExchange" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").



% The (local) registration name of a data exchanger:
-define( data_exchanger_name, sim_diasca_data_exchanger ).


% The (global) registration name of the root data exchanger:
-define( root_data_exchanger_name, sim_diasca_root_data_exchanger ).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().

-type json_parser_state() :: json_utils:parser_state().

-type node_type() :: class_DeploymentManager:node_type().

-type time_manager_pid() :: class_TimeManager:time_manager_pid().



% TO-DO:
% - add remove/delete operations
% - perform immediate propagations



% Implementation notes.


% There is one data exchanger agent per computing node, and they form a tree
% (the exchanger hierarchy), whose root is the root data-exchanger, very much
% like the hierarchy of time managers.

% As a result, the commit consistency is verified by the root data exchanger,
% which holds the sole reference copy of the data repository, which is
% replicated on all other (thus non-root, i.e. local) data exchangers. As
% updates are made only in-between diascas, during a diasca the data is stable
% (immutable), and each actor will be able to read them locally (directly), and
% at will (as many times as needed).

% Data definitions and modifications will be recorded during a diasca, and
% applied only once that diasca is over, i.e. just before the next scheduled
% one.
%
% Data can also be defined and modified before the simulation is
% started. Definitions can be done through requests or through the reading of
% any number of configuration files (either in Erlang term format, i.e. the one
% of file:consult/1, or in JSON, if the JSON support has been enabled *on all
% nodes* and if the extension of the filename is ".json" - like in
% "foobar.baz.json").

% For actors, data readings are just plain, direct, non-reordered, WOOPER
% requests that are made locally (on the node of the reading actor), thanks to
% the local exchanger that was automatically deployed there. Therefore readings
% should be considered as being by design fast and inexpensive.

% From the simulation case, the readings are managed a bit differently
% internally, depending on whether the user host is included or not in the
% simulation.

% If included, then on that same user host there is by design a computing node
% with its own local data-exchanger. As a consequence, to avoid a data
% duplication of the (potentially large) exchange repository on the user host,
% the user node will interact with the data-exchanger that is local to the
% computing node on the same host.

% If the user node is not included in the simulation, then there is no computing
% host to rely upon, and a local data-exchanger dedicated to the user node is
% simply created and integrated in the data-exchange hierarchy.

% Data exchangers store data, either static (const) or not (mutable), either
% read once for all at start-up from a deployed configuration file, or specified
% at any time in the course of the simulation.

% Data is here made of sets of entries, each of which is a {Key,Value} pair
% where Key is an atom and Value is any Erlang term.

% Data can be defined (whereas not existing), or modified (thanks to commits).



% A reading is easy to implement: static information is either read from a
% deployed configuration file (thus all exchangers start with the same content)
% or from synchronous (blocking) declarations of newer static data. If the
% simulation is started, the information can be read only starting from the next
% diasca.
%
% The exchanger is not a specialised actor, it just interacts with data writers
% and readers, and with the root time manager.

% Settings can be defined from the code (ex: sending messages from the
% simulation case to the exchanger), but they can be read also from file(s). In
% this case there would be two possibilities: either we can read the content
% from the root exchanger, and send data from it as messages, or we include this
% file content in the deployment archive and have each exchanger read that same
% data on its own.
%
% As the files are potentially large, transferring the data in a compressed
% binary form through the archive rather than thanks to marshalled Erlang terms
% would be probably more efficient. However, when creating a child exchanger, an
% entry read from a (static) deployed file must not have been overridden in the
% meantime at the level of its parent, otherwise the targeted content is not the
% one that could be read from files. As a consequence, at deployment-time, all
% data exchangers are created and synchronised with the root one first, before
% all subsequent operations apply to all of them, so that they remain in sync.


% Exchangers could maintain a somewhat lazy copy of shared data: if a given
% local exchanger was unable to fulfill a read request, it could ask its
% parent. As a consequence, the root exchanger would be the only official data
% reference (single authoritative source) and any data modification may have to
% climb up to the root, but may not go down directly through the exchanger
% hierarchy. However a non-root exchanger would never be able to tell if a
% key/value pair it holds is up-to-date, unless it is const. So the best,
% safest, simplest solution is to always propagate any modification down the
% whole hierarchy. So exchangers basically behave like perfect clones, rather
% than mere (incomplete) caches, knowing that any update happens for them
% between overall simulation diascas.

% The general convention is to preferably associate the 'undefined' atom to a
% key rather than defining or not that key (like, for instance, state
% attributes).


% The root data exchanger is created on the same node as the root time manager,
% so that they can communicate within the smallest possible latency (as any
% update of the former lies in the critical path of the simulation).
%
% A local data exchanger is created on all the other computing nodes, and by
% default a very simple data-exchanger tree is created: all local managers are
% direct children of the root one, like in the case of time managers.
%
% No data exchanger is created on the user node, as initial set-up directly
% involves the root data exchanger.
%
% Thus one data exchanger exists per computing node, and only there; all are
% registered locally.




% Constructs a data exchanger:
%
% - ExchangerName is the name of this exchanger, as a string
%
% - ExchangerInfos is either:
%
%   - {ParentExchangerPid, NodeType} where:
%
%      - the PID of the parent exchanger of this exchanger, in which case we are
%      creating a child exchanger, which will retrieve information from its
%      parent
%
%      - NodeType is either 'computing_node' or 'user_node'; the latter case
%      happens only when a data-exchanger local to the user node has to be
%      created; it allows to update the paths accordingly
%
%   - or {ConfigurationFiles, RootTimeManagerPid} where:
%
%      - ConfigurationFiles is a list of plain strings, corresponding to the
%      paths, relative to the root of the deployment archive, of the
%      configuration files that shall be read (we then create here a root
%      exchanger)
%
%      - RootTimeManagerPid is the PID of the root time manager, to insert
%      updates between diascas
%
-spec construct( wooper:state(), ustring(),
				 { data_exchanger_pid(), node_type() }
				 | { [ file_path() ], time_manager_pid() } ) -> wooper:state().
construct( State, ExchangerName, { ConfigurationFiles, RootTimeManagerPid } )
  when is_list( ConfigurationFiles ) andalso is_pid( RootTimeManagerPid ) ->

	% We construct the root exchanger here:

	RootTimeManagerPid ! { declareDataExchanger, self() },

	CommonState = common_construct( ExchangerName, State ),

	% We register globally (so that direct communication to this root exchanger
	% can be done) and locally (as there might actors local to the node of the
	% root data exchanger):
	%
	naming_utils:register_as( ?root_data_exchanger_name, global_only ),
	naming_utils:register_as( ?data_exchanger_name, local_only ),

	class_InstanceTracker:register_agent( State ),

	%trace_utils:debug_fmt( "Registered root data exchanger ~w globally "
	%    "(as ~ts) and locally (as ~ts) on ~p.",
	%    [ self(), ?root_data_exchanger_name, ?data_exchanger_name, node() ] ),

	{ Message, ReadState } = case ConfigurationFiles of

			[] ->
				{ "with no configuration file to read.", CommonState };

			_ ->
				Mes = text_utils:format(
				   "that will read following configuration file(s): ~ts",
				   [ text_utils:strings_to_string( ConfigurationFiles ) ] ),

				ParseState = parse_files( ConfigurationFiles,
						_NodeType=computing_node, CommonState ),

				{ Mes, ParseState }

	end,

	JSONStatus = case getAttribute( ReadState, json_parser_state ) of

		undefined ->
			"disabled";

		_ ->
			"enabled"

	end,

	?send_info( ReadState,
				text_utils:format( "Creating a root data exchanger ~ts~n"
					"(with JSON support ~ts)", [ Message, JSONStatus ] ) ),

	% Returns an updated state:
	setAttributes( ReadState, [
		{ root_exchanger_pid, self() },
		{ parent_exchanger_pid, none },
		{ feeder_files,
		  text_utils:strings_to_binaries( ConfigurationFiles ) },
		{ root_time_manager_pid, RootTimeManagerPid } ] );


construct( State, ExchangerName, { ParentExchangerPid, NodeType } )
  when is_pid( ParentExchangerPid ) andalso is_atom( NodeType ) ->

	% We construct a child exchanger here, requests answer early:

	ParentExchangerPid ! { synchronise, [], self() },

	CommonState = common_construct( ExchangerName, State ),

	% We register only locally (all non-root exchangers with the same, local,
	% name), as any actor will only look it up directly, on its current
	% (computing) node:
	%
	naming_utils:register_as( ?data_exchanger_name,
							  _RegistrationType=local_only ),

	class_InstanceTracker:register_agent( State ),

	?send_info_fmt( CommonState, "Creating a child data exchanger, "
					"whose parent exchanger is ~w.", [ ParentExchangerPid ] ),

	% Answer to the 'synchronise' call:
	{ NewState, BinFeederFileList } = receive

		{ wooper_result, { FeederFileListAsBins, RootExchangerPid } } ->

			ChildState = setAttribute( CommonState, root_exchanger_pid,
									   RootExchangerPid ),

			Feeders = text_utils:binaries_to_strings( FeederFileListAsBins ),

			%trace_utils:debug_fmt( "Feeders = ~p.", [ Feeders ] ),

			{ parse_files( Feeders, NodeType, ChildState ),
			  FeederFileListAsBins }

	end,

	?send_debug_fmt( NewState, "Data table after construction: ~ts",
			  [ table:to_string( getAttribute( NewState, data_table ) ) ] ),

	setAttributes( NewState, [
			% To be kept for possible future own child exchangers:
			{ feeder_files, BinFeederFileList },
			{ parent_exchanger_pid, ParentExchangerPid } ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?info( "Deleting data exchanger." ),

	class_InstanceTracker:unregister_agent(),

	naming_utils:unregister( ?data_exchanger_name, local_only ),

	% Recursive descending deletion:
	wooper:delete_synchronously_instances( ?getAttr(child_exchangers) ),

	case ?getAttr(json_parser_state) of

		undefined ->
			ok;

		JsonParserState ->
			json_utils:stop_parser( JsonParserState )

	end,

	% Then allows chaining:
	State.




% Member methods section.


% Requests this exchanger to send back synchronisation information to the caller
% one, which becomes a (direct) additional child exchanger thereof.
%
% Returns {FeederFiles, RootExchangerPid}.
%
-spec synchronise( wooper:state() ) ->
		request_return( { [ bin_file_path() ], data_exchanger_pid() } ).
synchronise( State ) ->

	ChildExchangerPid = ?getSender() ,

	BinFeederFiles = ?getAttr(feeder_files),

	wooper:return_state_result(
		_State=appendToAttribute( State, child_exchangers, ChildExchangerPid ),
		_Res={ BinFeederFiles, ?getAttr(root_exchanger_pid) } ).



% Requests to parse specified list of files (to be read from the deployment
% archive) and register the corresponding data.
%
% Used by non-root data exchangers.
%
-spec parse( wooper:state(), [ file_path() ] ) -> oneway_return().
parse( State, FileList ) ->

	BasePath = class_Actor:get_deployed_root_directory( State ),

	AbsolutePathList =
		[ file_utils:join( BasePath, Path ) || Path <- FileList ],

	JsonParserState = ?getAttr(json_parser_state),

	NewDataTable = lists:foldl(
				 fun( Filename, AccTable ) ->
						parse_file( Filename, AccTable, JsonParserState )
				 end,
				 ?getAttr(data_table),
				 AbsolutePathList ),

	wooper:return_state( setAttribute( State, data_table, NewDataTable ) ).




% Simulation listening callbacks.



% Called by the time-manager on the root data-exchanger (as the latter is a
% listener of the former), so that commits can be managed with regard to diascas
% once the simulation is running.
%
-spec simulation_started( wooper:state() ) -> oneway_return().
simulation_started( State ) ->

	% Checkings:
	false = ?getAttr(simulation_running),
	[] = ?getAttr(pending_commits),
	none = ?getAttr(parent_exchanger_pid),

	wooper:return_state(
		setAttribute( State, simulation_running, true ) ).



% Not used here.
-spec simulation_suspended( wooper:state() ) -> const_oneway_return().
simulation_suspended( State ) ->
	none = ?getAttr(parent_exchanger_pid),
	wooper:const_return().



% Not used here.
-spec simulation_resumed( wooper:state() ) -> const_oneway_return().
simulation_resumed( State ) ->
	none = ?getAttr(parent_exchanger_pid),
	wooper:const_return().



% Not used here.
-spec simulation_succeeded( wooper:state() ) -> const_oneway_return().
simulation_succeeded( State ) ->
	none = ?getAttr(parent_exchanger_pid),
	wooper:const_return().



% Not used here.
-spec simulation_stopped( wooper:state() ) -> oneway_return().
simulation_stopped( State ) ->
	none = ?getAttr(parent_exchanger_pid),
	wooper:return_state(
	   setAttribute( State, simulation_running, false ) ).



% Called by the root time manager when the current diasca is finished, waiting
% for the next one to start, only when this root data-exchanger will have
% achieved its commit propagation and then notified it.
%
% (request, for synchronization purposes)
%
-spec onInterDiascaBegin( wooper:state() ) ->
								request_return( 'interdiasca_ended' ).
onInterDiascaBegin( State ) ->

	PendingCommits = ?getAttr(pending_commits),

	% Checkings:
	true  = ?getAttr(interdiasca_requested),
	false = ( PendingCommits =:= [] ),
	none  = ?getAttr(parent_exchanger_pid),

	% Returned result is a list of initial_data_defined atoms, not interesting
	% as such here, thus ignored:
	%
	{ NewState, _Res } = executeRequestInTree( State,
			_RequestName=commitDataHelper, _Params=[ PendingCommits ] ),

	FinalState = setAttributes( NewState, [
			{ interdiasca_requested, false },
			{ pending_commits, [] } ] ),

	wooper:return_state_result( FinalState, interdiasca_ended ).




% Section for initial data manipulation.
%
% All initial operations are synchronously performed: when they return, the
% whole data-exchange tree will already have been updated accordingly.
%
% We could either first check the data, then propagate it (slower but easier to
% debug) or trigger as soon as possible the propagation and then only check (on
% all nodes, or only on the root one) - which would be faster.
%
% We preferred this latter solution: we propagate first, then check, and we do
% this on all exchangers, not on the root exchanger only (despite the fact that
% all exchangers are expected to maintain exact clones); anyway, in terms of
% wallclock-time, this will lead to very little difference, as all nodes will
% work roughly in parallel, doing the same as the root one, thus with no real
% delay added).



% Subsection for initial definition.


% Defines specified (non-already existing) initial data, returns
% initial_data_defined for synchronisation purposes, or throws an exception.
%
% Data defined with no qualifier will use the default one.
%
% Only to be called on the root data exchanger, and while the simulation is not
% running.
%
-spec defineInitialData( wooper:state(), entries() ) ->
								request_return( 'initial_data_defined' ).
defineInitialData( State, EntryList ) ->

	% Optional checkings: must be initial, and deal with root exchanger.
	false = ?getAttr(simulation_running),
	none = ?getAttr(parent_exchanger_pid),

	Table = ?getAttr(data_table),

	DefinitionList = prepare_entries_to_define( EntryList, Table ),

	NewState = define_data_recursive( DefinitionList, State ),

	wooper:return_state_result( NewState, initial_data_defined ).



% Defines specified (non-already existing) initial data, returns
% initial_data_defined for synchronisation purposes, or throws an exception.
%
% Note: the default qualifier is implied here.
%
% Only to be called on the root data exchanger, and while the simulation is not
% running.
%
-spec defineInitialData( wooper:state(), key(), value() ) ->
								request_return( 'initial_data_defined' ).
defineInitialData( State, Key, Value ) ->

	% Optional checkings: must be initial, and deal with root exchanger.
	false = ?getAttr(simulation_running),
	none = ?getAttr(parent_exchanger_pid),

	Table = ?getAttr(data_table),

	DefinitionEntry = get_entry_to_define( { Key, Value }, Table ),

	NewState = define_data_recursive( DefinitionEntry, State ),

	wooper:return_state_result( NewState, initial_data_defined ).



% Defines specified (non-already existing) initial data with a qualifier,
% returns initial_data_defined for synchronisation purposes, or throws an
% exception.
%
% Only to be called on the root data exchanger, and while the simulation is not
% running.
%
-spec defineInitialData( wooper:state(), key(), value(), qualifier() ) ->
								request_return( 'initial_data_defined' ).
defineInitialData( State, Key, Value, Qualifier ) ->

	% Optional checkings: must be initial, and deal with root exchanger.
	false = ?getAttr(simulation_running),
	none = ?getAttr(parent_exchanger_pid),

	Table = ?getAttr(data_table),

	DefinitionEntry = get_entry_to_define( { Key, Value, Qualifier }, Table ),

	NewState = define_data_recursive( DefinitionEntry, State ),

	wooper:return_state_result( NewState, initial_data_defined ).




% Subsection for initial modification.


% Sets specified (already defined) initial data, returns initial_data_modified
% for synchronisation purposes, or throws an exception.
%
% Only to be called on the root data exchanger, and while the simulation is not
% running.
%
-spec modifyInitialData( wooper:state(), entries() )  ->
								request_return( 'initial_data_modified' ).
modifyInitialData( State, EntryList ) ->

	% Optional checkings: must be initial, and deal with root exchanger.
	false = ?getAttr(simulation_running),
	none = ?getAttr(parent_exchanger_pid),

	Table = ?getAttr(data_table),

	ModificationList = prepare_entries_to_modify( EntryList, Table ),

	NewState = modify_data_recursive( ModificationList, State ),

	wooper:return_state_result( NewState, initial_data_modified ).



% Sets specified (already defined) initial data, returns
% initial_data_modified for synchronisation purposes, or throws an exception.
%
% Note: the already-defined qualifier will be kept there.
%
% Only to be called on the root data exchanger, and while the simulation is not
% running.
%
-spec modifyInitialData( wooper:state(), key(), value() ) ->
								request_return( 'initial_data_modified' ).
modifyInitialData( State, Key, Value ) ->

	% Optional checkings: must be initial, and deal with root exchanger.
	false = ?getAttr(simulation_running),
	none = ?getAttr(parent_exchanger_pid),

	Table = ?getAttr(data_table),

	ModificationEntry = get_entry_to_modify( { Key, Value }, Table ),

	NewState = modify_data_recursive( ModificationEntry, State ),

	wooper:return_state_result( NewState, initial_data_modified ).



% Sets specified (already defined) initial data, returns
% initial_data_modified for synchronisation purposes, or throws an exception.
%
% Only to be called on the root data exchanger, and while the simulation is not
% running.
%
-spec modifyInitialData( wooper:state(), key(), value(), qualifier() ) ->
							request_return( 'initial_data_modified' ).
modifyInitialData( State, Key, Value, Qualifier ) ->

	% Optional checkings: must be initial, and deal with root exchanger.
	false = ?getAttr(simulation_running),
	none = ?getAttr(parent_exchanger_pid),

	Table = ?getAttr(data_table),

	ModificationEntry = get_entry_to_modify( { Key, Value, Qualifier }, Table ),

	NewState = modify_data_recursive( ModificationEntry, State ),

	wooper:return_state_result( NewState, initial_data_modified ).




% Subsection for initial reading.


% Returns the values (without the qualifiers) associated to the specified keys
% as a list of key/value pairs (ex: [ {K1,V1}, {K2,V2} ]), or throws an
% exception.
%
% Preferably to be called on a local data exchanger; the simulation must not be
% started yet.
%
-spec readInitialData( wooper:state(), [ key() ] ) ->
							const_request_return( entries() );
					 ( wooper:state(), key() ) ->
							const_request_return( value() ).
readInitialData( State, Keys ) when is_list( Keys ) ->

	% Optional checkings: must be initial.
	false = ?getAttr(simulation_running),

	ReadPairs = try

					table:select_entries( Keys, ?getAttr(data_table) )

				catch

					_Type:Exception ->
						throw( { initial_key_to_read_not_found,
								 Exception, Keys, ?getSender() } )

				end,

	UnQualifiedPairs = [ { K, V } || { K, { V, _Q } } <- ReadPairs ],

	wooper:const_return_result( UnQualifiedPairs );


% Returns the value (without the qualifier) associated to the specified key, or
% throws an exception.
%
% Preferably to be called on a local data exchanger; the simulation must not be
% started yet.
%
readInitialData( State, Key ) when is_atom( Key ) ->

	% Optional checkings: must be initial.
	false = ?getAttr(simulation_running),

	case table:lookup_entry( Key, ?getAttr(data_table) ) of

		{ value, { Value, _Qualifier } } ->
			wooper:const_return_result( Value );

		key_not_found ->
			throw( { initial_key_to_read_not_found, Key, ?getSender() } )

	end.



% Returns the values associated to the specified keys as a list of
% key/value/qualifier triplets (ex: [ {K1,V1,Q1}, {K2,V2,Q2} ]), or throws an
% exception.
%
% Preferably to be called on a local data exchanger; the simulation must not be
% started yet.
%
-spec readQualifiedInitialData( wooper:state(), key() ) ->
									const_request_return( qualified_value() );
							  ( wooper:state(), [ key() ] ) ->
									const_request_return( qualified_entries() ).
readQualifiedInitialData( State, Keys ) when is_list( Keys ) ->

	% Optional checkings: must be initial.
	false = ?getAttr(simulation_running),

	ReadPairs = try

					table:select_entries( Keys, ?getAttr(data_table) )

				catch

					_Type:Exception ->
						throw( { initial_key_to_read_not_found, Exception,
								 Keys, ?getSender() } )

				end,

	QualifiedTriplets = [ { K, V, Q } || { K, { V, Q } } <- ReadPairs ],

	wooper:const_return_result( QualifiedTriplets );


% Returns the value and qualifier associated to the specified key as a {Value,
% Qualifier} pair, or throws an exception.
%
% Preferably to be called on a local data exchanger; the simulation must not be
% started yet.
%
readQualifiedInitialData( State, Key ) when is_atom( Key ) ->

	% Optional checkings: must be initial.
	false = ?getAttr(simulation_running),

	case table:lookup_entry( Key, ?getAttr(data_table) ) of

		{ value, P } ->
			wooper:const_return_result( P );

		key_not_found ->
			throw( { initial_key_to_read_not_found, Key, ?getSender() } )

	end.





% Section for data manipulation from actors, i.e. while the simulation may or
% may not be running (in the case of initial actors).
%
% Commits (data definition/modification) are done directly at the level of the
% root data-exchanger, and are propagated only between the diascas (and iff
% needed).
%
% A principle is to perform as many checkings as possible directly, when the
% commit is issued, for a better diagnosis of errors, and to better spread the
% computing load (avoid operations in the inter-diasca critical path).



% Defines specified (non-already existing) data, returns data_defined for
% synchronisation purposes, or throws an exception.
%
% Only to be called on the root data exchanger, from actors; the simulation
% might be already running or not.
%
-spec defineData( wooper:state(), entries() ) ->
						request_return( 'data_defined' ).
defineData( State, _EntryList=[] ) ->

	% Optional checkings: must deal with root exchanger.
	none = ?getAttr(parent_exchanger_pid),

	% Not wanting to trigger inter-diasca notifications if not having data:
	wooper:const_return_result( data_defined );


defineData( State, EntryList ) ->

	% Optional checkings: must deal with root exchanger.
	none = ?getAttr(parent_exchanger_pid),

	Table = ?getAttr(data_table),

	% EntryList is a list of {K,V} pairs and {K,V,Q} triplets.
	% DefinitionList will be a list of {K,{V,Q}} pairs.

	% Conditions to respect:

	% 1. No key must already be defined in the table. Once returned, all of them
	% will have a qualifier (all entries must then be {K,{V,Q}} pairs):
	%
	DefinitionList = prepare_entries_to_define( EntryList, Table ),

	% Note that the previous list may still have more than one entry referring
	% to the same key; this will be checked in both of the next branches:
	%
	NewState = case ?getAttr(simulation_running) of

		false ->
				% No pending commits to check these entries against, we just
				% have to ensure that there are no duplicates in that list:
				%
				check_no_duplicated_key( DefinitionList ),

				% Update to be made immediately and without anymore checking:
				define_data_recursive( DefinitionList, State );

		true ->
				% 2. No other already recorded commit should exist for that key:
				% (note: this deals with the fact that the same key might appear
				% more than once in the specified entry list)
				%
				PendingCommits = ?getAttr(pending_commits),

				% Here, we must ensure that no new entry will collide either
				% with already-existing commits, or with the other entries:
				%
				NewPendingCommits =
							add_commits( DefinitionList, PendingCommits ),

				NotifiedState = manage_inter_diasca_notification( State ),

				setAttribute( NotifiedState, pending_commits,
							  NewPendingCommits )

	end,

	wooper:return_state_result( NewState, data_defined ).





% Defines specified (non-already existing) data, returns data_defined for
% synchronisation purposes, or throws an exception.
%
% Note: the default qualifier is implied here.
%
% Only to be called on the root data exchanger, from actors; the simulation
% might be already running or not.
%
-spec defineData( wooper:state(), key(), value() ) ->
						request_return( 'data_defined' ).
defineData( State, Key, Value ) ->

	{ NewState, Result } = defineData( State, Key, Value,
									   _Qualifier=get_default_qualifier() ),

	wooper:return_state_result( NewState, Result ).



% Defines specified (non-already existing) data, returns data_defined for
% synchronisation purposes, or throws an exception.
%
% Only to be called on the root data exchanger, from actors; the simulation
% might be already running, or not.
%
-spec defineData( wooper:state(), key(), value(), qualifier() ) ->
						request_return( 'data_defined' ).
defineData( State, Key, Value, Qualifier ) ->

	% Optional checkings: must deal with root exchanger.
	none = ?getAttr(parent_exchanger_pid),

	Table = ?getAttr(data_table),

	% Conditions to respect:
	DefinitionEntry = get_entry_to_define( { Key, Value, Qualifier }, Table ),

	NewState = case ?getAttr(simulation_running) of

		false ->
				% Update to be made immediately:
				define_data_recursive( DefinitionEntry, State );

		true ->

				% No other already recorded commit should exist for that key:
				PendingCommits = ?getAttr(pending_commits),

				NewPendingCommits =
						   add_commit( DefinitionEntry, PendingCommits ),

				NotifiedState = manage_inter_diasca_notification( State ),

				setAttribute( NotifiedState, pending_commits,
							  NewPendingCommits )

	end,

	wooper:return_state_result( NewState, data_defined ).




% Sets specified (already defined) data, returns data_modified for
% synchronisation purposes, or throws an exception.
%
% If a qualifier is not specified, the default one (thus not necessarily the
% previous one) will be used.
%
% Only to be called on the root data exchanger, from actors; the simulation
% might be already running or not.
%
-spec modifyData( wooper:state(), entries() ) ->
						request_return( 'data_modified' ).
modifyData( State, _EntryList=[] ) ->

	% Optional checkings: must deal with root exchanger.
	none = ?getAttr(parent_exchanger_pid),

	% Not wanting to trigger inter-diasca notifications if not having data:
	wooper:const_return_result( data_modified );


modifyData( State, EntryList ) ->

	% Optional checkings: must deal with root exchanger.
	none = ?getAttr(parent_exchanger_pid),

	Table = ?getAttr(data_table),

	% EntryList is a list of {K,V} pairs and {K,V,Q} triplets.
	% DefinitionList will be a list of {K,{V,Q}} pairs.

	% Conditions to respect:

	% 1. All keys must already be defined in the table, and mutable. Once
	% returned, they all have a qualifier (all entries must then be {K,{V,Q}}
	% pairs):
	%
	DefinitionList = prepare_entries_to_modify( EntryList, Table ),

	% Note that the previous list may still have more than one entry referring
	% to the same key; this will be checked in both of the next branches:
	%
	NewState = case ?getAttr(simulation_running) of

		false ->
				% No pending commits to check these entries against, we just
				% have to ensure there are no duplicates in that list:
				%
				check_no_duplicated_key( DefinitionList ),

				% Update to be made immediately and without anymore checking:
				modify_data_recursive( DefinitionList, State );

		true ->
				% 2. No other already recorded commit should exist for that key:
				% (note: this deals with the fact that the same key might appear
				% more than once in the specified entry list)
				%
				PendingCommits = ?getAttr(pending_commits),

				% Here, we must ensure that no new triplet will collide either
				% with already-existing commits, or with the other triplets:
				%
				NewPendingCommits =
							add_commits( DefinitionList, PendingCommits ),

				NotifiedState = manage_inter_diasca_notification( State ),

				setAttribute( NotifiedState, pending_commits,
							  NewPendingCommits )

	end,

	wooper:return_state_result( NewState, data_modified ).



% Sets specified (already defined) data, returns data_modified for
% synchronisation purposes, or throws an exception.
%
% If a qualifier is not specified, the default one (thus not necessarily the
% previous one) will be used.
%
% Only to be called on the root data exchanger, from actors; the simulation
% might be already running or not.
%
-spec modifyData( wooper:state(), key(), value() ) ->
						request_return( 'data_modified' ).
modifyData( State, Key, Value ) ->

	% Optional checkings: must deal with root exchanger.
	none = ?getAttr(parent_exchanger_pid),

	Table = ?getAttr(data_table),

	% Conditions to respect:

	% 1. All keys must already be defined in the table, and mutable. Once
	% returned, they all have a qualifier (all entries must then be {K,{V,Q}}
	% pairs):
	%
	DefinitionEntry = get_entry_to_modify( { Key, Value }, Table ),

	NewState = case ?getAttr(simulation_running) of

		false ->
			% Update to be made immediately:
			modify_data_recursive( DefinitionEntry, State );

		true ->

			% No other already recorded commit should exist for that key:
			PendingCommits = ?getAttr(pending_commits),

			NewPendingCommits = add_commit( DefinitionEntry,
											PendingCommits ),

			NotifiedState = manage_inter_diasca_notification( State ),

			setAttribute( NotifiedState, pending_commits, NewPendingCommits )

	end,

	wooper:return_state_result( NewState, data_modified ).




% Sets specified (already defined) data, returns data_modified for
% synchronisation purposes, or throws an exception.
%
% Only to be called on the root data exchanger, from actors; the simulation
% might be already running or not.
%
-spec modifyData( wooper:state(), key(), value(), qualifier() ) ->
						request_return( 'data_modified' ).
modifyData( State, Key, Value, Qualifier ) ->

	% Optional checkings: must deal with root exchanger.
	none = ?getAttr(parent_exchanger_pid),

	Table = ?getAttr(data_table),

	% Conditions to respect:

	% 1. All keys must already be defined in the table, and mutable. Once
	% returned, they all have a qualifier (all entries must then be {K,{V,Q}}
	% pairs):
	%
	DefinitionEntry = get_entry_to_modify( { Key, Value, Qualifier }, Table ),

	NewState = case ?getAttr(simulation_running) of

		false ->
				% Update to be made immediately:
				modify_data_recursive( DefinitionEntry, State );

		true ->

				% No other already recorded commit should exist for that key:
				PendingCommits = ?getAttr(pending_commits),

				NewPendingCommits =
						   add_commit( DefinitionEntry, PendingCommits ),

				NotifiedState = manage_inter_diasca_notification( State ),

				setAttribute( NotifiedState, pending_commits,
							  NewPendingCommits )

	end,

	wooper:return_state_result( NewState, data_modified ).




% Returns the values associated to the specified keys as a list of key/value
% pairs (ex: [ {K1,V1}, {K2,V2} ]), or throws an exception.
%
% Preferably to be called on a local data exchanger; the simulation must not be
% started yet.
%
-spec readData( wooper:state(), key() ) ->
						const_request_return( value() );
			  ( wooper:state(), [ key() ] ) ->
						const_request_return( entries() ).
readData( State, Keys ) when is_list( Keys ) ->

	ReadPairs = try

		table:select_entries( Keys, ?getAttr(data_table) )

	catch

		_Type:Exception ->
			throw( { key_to_read_not_found, Exception, Keys, ?getSender() } )

	end,

	UnQualifiedPairs = [ { K, V } || { K, { V, _Q } } <- ReadPairs ],

	wooper:const_return_result( UnQualifiedPairs );


% Returns the value (without the qualifier) associated to the specified key, or
% throws an exception.
%
% Preferably to be called on a local data exchanger; the simulation must not be
% started yet.
%
readData( State, Key ) when is_atom( Key ) ->

	case table:lookup_entry( Key, ?getAttr(data_table) ) of

		{ value, { Value, _Qualifier } } ->
			wooper:const_return_result( Value );

		key_not_found ->
			throw( { key_to_read_not_found, Key, ?getSender() } )

	end.



% Returns the values associated to the specified keys as a list of
% key/value/qualifier triplets (ex: [ {K1,V1,Q1}, {K2,V2,Q2} ]), or throws an
% exception.
%
% Preferably to be called on a local data exchanger; the simulation must not be
% started yet.
%
-spec readQualifiedData( wooper:state(), key() ) ->
							   const_request_return( qualified_value() );
					   ( wooper:state(), [ key() ] ) ->
							   const_request_return( qualified_entries() ).
readQualifiedData( State, Keys ) when is_list( Keys ) ->

	ReadPairs = try table:select_entries( Keys, ?getAttr(data_table) )

	catch

		_Type:Exception ->
			throw( { key_to_read_not_found, Exception, Keys, ?getSender() } )

	end,

	wooper:const_return_result( ReadPairs );



% Returns the value and qualifier associated to the specified key as a
% {Value,Qualifier} pair, or throws an exception.
%
% Preferably to be called on a local data exchanger; the simulation must not be
% started yet.
%
readQualifiedData( State, Key ) when is_atom( Key ) ->

	case table:lookup_entry( Key, ?getAttr(data_table) ) of

		{ value, P } ->
			wooper:const_return_result( P );

		key_not_found ->
			throw( { key_to_read_not_found, Key, ?getSender() } )

	end.





% Section for recursive data-management helpers through the exchanger hierarchy.


% Helper request to recurse in the data-exchange tree with an entry.
%
% Entry is {Key, {Value,Qualifier}}.
%
-spec defineDataHelper( wooper:state(), entry() | entries() ) ->
								request_return( 'data_defined' ).
defineDataHelper( State, _Entry={ Key, V } ) ->

	% This entry list must have been already checked, thus is to be stored
	% without further ado:
	%
	NewTable = table:add_entry( Key, V, ?getAttr(data_table) ),

	wooper:return_state_result( setAttribute( State, data_table, NewTable ),
								data_defined );


% Helper request to recurse in the data-exchange tree with an entry list.
%
% EntryList is a list of {Key, {Value, Qualifier}} elements.
%
defineDataHelper( State, EntryList ) -> % when is_list(EntryList) ->

	NewTable = table:add_entries( EntryList, ?getAttr(data_table) ),

	wooper:return_state_result( setAttribute( State, data_table, NewTable ),
								data_defined ).





% Helper request to recurse in the data-exchange tree with an entry, a qualifier
% being specified.
%
% Here V={Value,Qualifier}
%
-spec modifyDataHelper( wooper:state(), entry() | entries() ) ->
							request_return( 'data_modified' ).
modifyDataHelper( State, _Entry={ Key, V } ) ->

	% This entry must have been already checked, thus is to be stored without
	% further ado:
	%
	NewTable = table:add_entry( Key, V, ?getAttr(data_table) ),

	wooper:return_state_result( setAttribute( State, data_table, NewTable ),
								data_modified );


% Helper request to recurse in the data-exchange tree with an entry list.
%
% EntryList is a list of {Key, {Value, Qualifier}} elements.
%
modifyDataHelper( State, EntryList ) -> % when is_list(Entries) ->

	% This entry list must have been already checked, thus is to be stored
	% without further ado:
	%
	NewTable = table:add_entries( EntryList, ?getAttr(data_table) ),

	wooper:return_state_result( setAttribute( State, data_table, NewTable ),
								data_modified ).




% Helper request to recurse in the data-exchange tree with an entry list.
%
% EntryList is a list of {Key, {Value, Qualifier}} elements.
%
-spec commitDataHelper( wooper:state(), qualified_basic_entries() ) ->
								request_return( 'data_committed' ).
commitDataHelper( State, EntryList ) ->

	% This list of entries must have been already checked, thus is to be stored
	% without further ado:
	%
	NewTable = table:add_entries( EntryList, ?getAttr(data_table) ),

	wooper:return_state_result( setAttribute( State, data_table, NewTable ),
								data_committed ).





% Returns all the data stored by this data exchanger, as a unordered list of
% {Key,Value} pairs.
%
% Note: mostly for debugging purpose.
%
-spec getAllData( wooper:state() ) -> const_request_return( entries() ).
getAllData( State ) ->
	wooper:const_return_result( table:enumerate( ?getAttr(data_table) ) ).



% Outputs, as a debug trace, the current data held by this data exchanger, as a
% unordered list of {Key,Value} pairs.
%
% Note: mostly for debugging purpose.
%
-spec traceData( wooper:state() ) -> const_request_return( 'data_traced' ).
traceData( State ) ->

	DataPairs = table:enumerate( ?getAttr(data_table) ),

	PairStrings = [ text_utils:format( "~ts: ~p", [ K, V ] )
					|| { K, V } <- DataPairs ],

	?debug_fmt( "Current data table on ~p (~p): ~ts~n~n",
		[ self(), node(), text_utils:strings_to_string( PairStrings ) ] ),

	wooper:const_return_result( data_traced ).



% Outputs, as debug traces, the current data held by all data exchangers, as a
% unordered list of {Key,Value} pairs.
%
% Note: mostly for debugging purpose.
%
-spec traceDistributedData( wooper:state() ) ->
							const_request_return( 'distributed_data_traced' ).
traceDistributedData( State ) ->

	executeRequestInTree( State, _RequestName=traceData, _Params=[] ),

	wooper:const_return_result( distributed_data_traced ).





% Static methods section.



% Returns the name under which a data-exchanger will be locally registered.
-spec get_local_exchanger_name() ->
						static_return( naming_utils:registration_name() ).
get_local_exchanger_name() ->
	wooper:return_static( ?data_exchanger_name ).



% Returns the name under which the data-exchanger to be used by the simulation
% case is to be registered globally.
%
% Indeed, if the user host is included in the simulation, then it will use the
% data-exchanger local to the computing node created on that same user host, in
% order to avoid to replicate it.
%
-spec get_global_name_of_exchanger_for_case() ->
						static_return( naming_utils:registration_name() ).
get_global_name_of_exchanger_for_case() ->
	wooper:return_static( sim_diasca_data_exchanger_for_case ).



% Returns the PID of the data exchanger instantiated on the node of the caller.
-spec get_local_exchanger() -> static_return( data_exchanger_pid() ).
get_local_exchanger() ->

	Pid = naming_utils:get_registered_pid_for( ?data_exchanger_name,
											   _RegistrationType=local ),

	wooper:return_static( Pid ).



% Returns the PID of the data exchanger instantiated on the node of the caller.
-spec get_root_exchanger() -> static_return( data_exchanger_pid() ).
get_root_exchanger() ->

	Pid = naming_utils:get_registered_pid_for( ?root_data_exchanger_name,
										 _RegistrationType=global ),

	wooper:return_static( Pid ).



% Returns an opaque datatype that allows to make use of the data-exchange
% service from a simulation case (ex: a test case).
%
-spec get_case_exchange_settings() -> static_return( exchange_settings() ).
get_case_exchange_settings() ->

	% Some operations are made directly with the root exchangers, others not.

	% In this context (i.e. from a simulation case), we run on the user node,
	% thus we may either use:
	%
	% - if the user host was not included in the simulation (which is not very
	% common), a locally-registered PID of a node-local, private,
	% data-exchanger, that was especially created for the user node
	%
	% - otherwise, either a WOOPER instance proxy (which is then local) pointing
	% to another data-exchanger on the same user host (this mode of operation is
	% now disabled) or the remote PID of the data-exchanger that is nevertheless
	% host-local (i.e. local to the computing node running on the same host as
	% this user node)

	% If wanting to use the proxy:
	% PseudoLocalExchangerPid = get_local_exchanger(),

	% If preferring to use directly the pointed exchanger:

	% In all cases, we try to look-up the global name for the 'data-exchanger
	% dedicated to the simulation case', which is expected to point either to a
	% node-local data-exchanger (local host not included in the simulation) or
	% to the host-local data-exchanger running on the local computing host
	% (local host included in the simulation).
	%
	% However the look-up of this name may fail, as the root data-exchanger may
	% have been placed on the user host, globally registered as such. As the
	% same PID cannot be registered globally more than once, we cannot register
	% it globally also as 'data-exchanger dedicated to the simulation case'. So:

	PseudoLocalExchangerPid = case naming_utils:is_registered(
		  get_global_name_of_exchanger_for_case(), _RegistrationType=global ) of

		not_registered ->
			% The simulation-case specific name could not be registered, as it
			% must have been already registered globally as the root
			% data-exchanger (we must be in the single-host case), thus
			% looking up this root exchanger, bound to be local:
			%
			% (if ever the root exchanger was used by mistake instead of a more
			% local one, the only risk incurred would be decreased performances)
			%
			get_root_exchanger();

		Pid ->
			Pid

	end,

	wooper:return_static( { get_root_exchanger(), PseudoLocalExchangerPid } ).



% Returns an opaque datatype that allows an actor to make use then of the
% data-exchange service.
%
-spec get_actor_exchange_settings() -> static_return( exchange_settings() ).
get_actor_exchange_settings() ->

	% Some operations are made directly with the root exchanger, others not;
	% anyway, for an actor (thus on a computing node), everything is readily
	% available:
	%
	wooper:return_static( { get_root_exchanger(), get_local_exchanger() } ).




% Section for initial data manipulation.
%
% A data can be:
%
% - defined if it was not already defined yet; omitting a qualifier will then
% select the default one
%
% - set if it was already defined; omitting a qualifier will then keep the
% current one
%
% - read if it was already defined
%
% Write operations will be made directly with the root data-exchanger, whereas
% read operations will remain purely local (with the local data-exchanger).





% Section about data definition.


% Registers into the root data exchanger (based on the specified PID) the
% specified initial data.
%
% An exception will be thrown if the data was already defined.
%
% This method is synchronous, to avoid race conditions.
%
-spec define_initial_data( key(), value(), qualifier(), exchange_settings() ) ->
									static_void_return().
define_initial_data( Key, Value, Qualifier,
			_ExchangeSettings={ RootExchangerPid, _LocalExchangerPid } ) ->

	RootExchangerPid ! { defineInitialData, [ Key, Value, Qualifier ], self() },

	receive

		{ wooper_result, initial_data_defined } ->
			wooper:return_static_void()

	end.



% Registers into the root data exchanger (based on the specified PID) the
% specified initial data. No qualifier was specified here, the default one will
% be used instead.
%
% An exception will be thrown if the data was already defined.
%
% This method is synchronous, to avoid race conditions.
%
-spec define_initial_data( key(), value(),
				exchange_settings() | qualifier() ) -> static_void_return().
define_initial_data( Key, Value,
					_ExchangeSettings={ RootExchangerPid, _LocalExchangerPid } )
  when is_pid( RootExchangerPid ) ->

	% No qualifier here.

	RootExchangerPid ! { defineInitialData, [ Key, Value ], self() },

	receive

		{ wooper_result, initial_data_defined } ->
			wooper:return_static_void()

	end;

% Registers into the root data exchanger (whose PID will be determined by a
% specific look-up) the specified data.
%
% An exception will be thrown if the data was already defined.
%
% This method is synchronous, to avoid race conditions.
%
% Note: for efficiency reasons, the counterpart version relying on exchange
% settings being obtained as parameters is to be preferred (call then
% get_case_exchange_settings/0 at the beginning of the simulation case and
% specify its result to next data-exchange calls).
%
define_initial_data( Key, Value, Qualifier ) ->
	define_initial_data( Key, Value, Qualifier, get_case_exchange_settings() ),
	wooper:return_static_void().



% Registers into the root data exchanger (based on the specified PID) the
% specified initial data, which is a list of data entries, each entry being
% either {Key, Value} or {Key, Value, Qualifier}.
%
% An exception will be thrown if the data was already defined.
%
% This method is synchronous, to avoid race conditions.
%
-spec define_initial_data( mixed_entries(), exchange_settings() ) ->
									static_void_return();
						 ( key(), value() ) -> static_void_return().
define_initial_data( EntryList,
			_ExchangeSettings={ RootExchangerPid, _LocalExchangerPid } )
  when is_list( EntryList ) andalso is_pid( RootExchangerPid ) ->

	RootExchangerPid ! { defineInitialData, [ EntryList ], self() },

	receive

		{ wooper_result, initial_data_defined } ->
			wooper:return_static_void()

	end;


% Registers into the root data exchanger (whose PID will be determined by a
% specific look-up) the specified data.
%
% An exception will be thrown if the data was already defined.
%
% This method is synchronous, to avoid race conditions.
%
% Note: for efficiency reasons, the counterpart version relying on exchange
% settings being obtained as parameters is to be preferred (call then
% get_case_exchange_settings/0 at the beginning of the simulation case and
% specify its result to next data-exchange calls).
%
define_initial_data( Key, Value ) when is_atom( Key ) ->
	define_initial_data( Key, Value, get_case_exchange_settings() ),
	wooper:return_static_void().



% Registers into the root data exchanger (whose PID will be determined by a
% specific look-up) the specified data, which is a list of data entries, each
% entry being either {Key, Value} or {Key, Value, Qualifier}.
%
% This method is synchronous, to avoid race conditions.
%
% Note: for efficiency reasons, the counterpart version relying on exchange
% settings being obtained as parameters is to be preferred (call then
% get_case_exchange_settings/0 at the beginning of the simulation case and
% specify its result to next data-exchange calls).
%
-spec define_initial_data( mixed_entries() ) -> static_void_return().
define_initial_data( EntryList ) when is_list( EntryList ) ->
	define_initial_data( EntryList, get_case_exchange_settings() ),
	wooper:return_static_void().




% Section about data setting.



% Registers into the root data exchanger (based on the specified PID) the
% specified initial data, which must have been already defined.
%
% An exception will be thrown if the data was already set.
%
% This method is synchronous, to avoid race conditions.
%
-spec modify_initial_data( key(), value(), qualifier(), exchange_settings() ) ->
			static_void_return().
modify_initial_data( Key, Value, Qualifier,
			_ExchangeSettings={ RootExchangerPid, _LocalExchangerPid } ) ->

	RootExchangerPid ! { modifyInitialData, [ Key, Value, Qualifier ], self() },

	receive

		{ wooper_result, initial_data_modified } ->
			wooper:return_static_void()

	end.



% Registers into the root data exchanger (based on the specified PID) the
% specified initial data, which must have been already defined.
%
% No qualifier is specified here, the default one will be used instead (not
% necessarily the current qualifier).
%
% An exception will be thrown if the data was already set.
%
% This method is synchronous, to avoid race conditions.
%
-spec modify_initial_data( key(), value(),
				exchange_settings() | qualifier() ) -> static_void_return().
modify_initial_data( Key, Value,
					_ExchangeSettings={ RootExchangerPid, _LocalExchangerPid } )
  when is_pid( RootExchangerPid ) ->

	% No qualifier here.

	RootExchangerPid ! { modifyInitialData, [ Key, Value ], self() },

	receive

		{ wooper_result, initial_data_modified } ->
			wooper:return_static_void()

	end;


% Registers into the root data exchanger (whose PID will be determined by a
% specific look-up) the specified data.
%
% An exception will be thrown if the data was already set.
%
% This method is synchronous, to avoid race conditions.
%
% Note: for efficiency reasons, the counterpart version relying on exchange
% settings being obtained as parameters is to be preferred (call then
% get_case_exchange_settings/0 at the beginning of the simulation case and
% specify its result to next data-exchange calls).
%
modify_initial_data( Key, Value, Qualifier ) ->
	modify_initial_data( Key, Value, Qualifier, get_case_exchange_settings() ),
	wooper:return_static_void().



% Registers into the root data exchanger (based on the specified PID) the
% specified initial data, which is a list of data entries, each entry being
% either {Key,Value} (in this case the default qualifier will be implied) or
% {Key,Value,Qualifier}.
%
% An exception will be thrown if the data was already set.
%
% This method is synchronous, to avoid race conditions.
%
-spec modify_initial_data( mixed_entries(), exchange_settings() ) ->
								static_void_return();
						 ( key(), value() ) ->
								static_void_return().
modify_initial_data( EntryList,
			_ExchangeSettings={ RootExchangerPid, _LocalExchangerPid } )
  when is_list( EntryList ) andalso is_pid( RootExchangerPid ) ->

	RootExchangerPid ! { modifyInitialData, [ EntryList ], self() },

	receive

		{ wooper_result, initial_data_modified } ->
			wooper:return_static_void()

	end;


% Registers into the root data exchanger (whose PID will be determined by a
% specific look-up) the specified data, which must have already been defined.
%
% No qualifier is specified here, the default one will be used instead (not
% necessarily the current qualifier).
%
% An exception will be thrown if the data was already set.
%
% This method is synchronous, to avoid race conditions.
%
% Note: for efficiency reasons, the counterpart version relying on exchange
% settings being obtained as parameters is to be preferred (call then
% get_case_exchange_settings/0 at the beginning of the simulation case and
% specify its result to next data-exchange calls).
%
modify_initial_data( Key, Value ) when is_atom( Key ) ->
	modify_initial_data( Key, Value, get_case_exchange_settings() ),
	wooper:return_static_void().




% Registers into the root data exchanger (whose PID will be determined by a
% specific look-up) the specified data, which is a list of data entries, each
% entry being either {Key,Value} or {Key,Value,Qualifier}. This data have
% already been defined.
%
% This method is synchronous, to avoid race conditions.
%
% Note: for efficiency reasons, the counterpart version relying on exchange
% settings being obtained as parameters is to be preferred (call then
% get_case_exchange_settings/0 at the beginning of the simulation case and
% specify its result to next data-exchange calls).
%
-spec modify_initial_data( mixed_entries() ) -> static_void_return().
modify_initial_data( EntryList ) when is_list( EntryList ) ->
	modify_initial_data( EntryList, get_case_exchange_settings() ),
	wooper:return_static_void().





% Section about data reading.


% Returns the value associated to specified key in the data-exchange service.
%
% Note: for efficiency reasons, the counterpart version relying on exchange
% settings being obtained as parameters is to be preferred (call then
% get_case_exchange_settings/0 at the beginning of the simulation case and
% specify its result to next data-exchange calls).
%
-spec read_initial_data( key() ) -> static_return( value() ).
read_initial_data( Key ) ->
	Value = read_initial_data( Key, get_case_exchange_settings() ),
	wooper:return_static( Value ).




% Returns the value associated to specified key(s) in the data-exchange service:
% if Key is an atom, only the corresponding value will be returned, whereas if
% Key is a list of keys [K1, K2, ...] (as atoms) then the corresponding list
% of key/value pairs will be returned: [{K1,V1}, {K2,V2}, ...].
%
-spec read_initial_data( key(), exchange_settings() ) ->
							   static_return( value() ).
read_initial_data( Key,
		_ExchangeSettings={ _RootExchangerPid, LocalExchangerPid } ) ->

	LocalExchangerPid ! { readInitialData, Key, self() },
	receive

		{ wooper_result, Value } ->
			wooper:return_static( Value )

	end.




% Returns the value and qualifier (as a {Value,Qualifier} pair) associated to
% specified key in the data-exchange service.
%
% Note: for efficiency reasons, the counterpart version relying on exchange
% settings being obtained as parameters is to be preferred (call then
% get_case_exchange_settings/0 at the beginning of the simulation case and
% specify its result to next data-exchange calls).
%
-spec read_qualified_initial_data( key() ) ->
										 static_return( qualified_value() ).
read_qualified_initial_data( Key ) ->
	Value = read_qualified_initial_data( Key, get_case_exchange_settings() ),
	wooper:return_static( Value ).



% Returns the value and qualifier associated to specified key(s) in the
% data-exchange service: if Key is an atom, only the corresponding
% {Value,Qualifier} pair will be returned, whereas if Key is a list of keys [
% K1, K2, ... ] (as atoms) then the corresponding list of key/value triplets
% will be returned: [ {K1,V1,Q1}, {K2,V2,Q2}, ... ].
%
-spec read_qualified_initial_data( key(), exchange_settings() ) ->
										static_return( qualified_value() );
								 ( [ key() ], exchange_settings() ) ->
										static_return( qualified_entries() ).
read_qualified_initial_data( Key,
				_ExchangeSettings={ _RootExchangerPid, LocalExchangerPid } ) ->

	LocalExchangerPid ! { readQualifiedInitialData, Key, self() },
	receive

		{ wooper_result, R } ->
			wooper:return_static( R )

	end.




% Helper functions.



% Checks that, in the specified list of tuples, the first element of each tuple
% is unique.
%
-spec check_no_duplicated_key( [ tuple() ] ) -> void().
check_no_duplicated_key( TupleList ) ->
	check_no_duplicated_key( TupleList, _Acc=[] ).


% (helper)
check_no_duplicated_key( _TupleList=[], Acc ) ->
	Acc;

check_no_duplicated_key( [ H | T ], Acc ) ->

	% For each element, we have just to check the remainder of the list (not the
	% full list), as by design the prior elements have already been checked:
	%
	% (done that way instead of using a pattern-matching H={K,_V,_Q} in the
	% function head, so that this function can be used with tuples of any size)
	%
	FirstElement = erlang:element( _Pos=1, H ),

	case lists:keysearch( FirstElement, _Position=1, T ) of

		false ->
			check_no_duplicated_key( T, [ H | Acc ] );

		{ value, Duplicate } ->
			throw( { duplicate_entry, H, Duplicate } )

	end.




% Definition section.


% Checks that no entry was already defined. Whether or not a qualifier was
% omitted, the returned triplet list is ready for a commit definition
% (i.e. qualifiers are already the ones to write directly).
%
prepare_entries_to_define( Entries, Table ) ->
	% Each entry is either a pair or a triplet, and will be managed accordingly:
	[ get_entry_to_define( E, Table ) || E <- Entries ].



% Returns the { K, { V, Q } } final entry corresponding to the qualifier-less
% specified one.
%
get_entry_to_define( _Entry={ K, V }, Table ) when is_atom( K ) ->

	% No qualifier specified here:
	case table:lookup_entry( K, Table ) of

		key_not_found ->
			% No qualifier specified:
			{ K, { V, get_default_qualifier() } };

		{ value, _PreviousValue={ Vp, Qp } } ->
			Previous = { K, Vp, Qp },
			New = { K, V, get_default_qualifier() },
			throw( { data_already_defined, Previous, New } )

	end;

% Returns the {K, {V, Q}} final entry corresponding to qualifier-including
% specified one.
%
get_entry_to_define( _Entry={ K, V, Q }, Table ) when is_atom( K ) ->

	case table:lookup_entry( K, Table ) of

		key_not_found ->
			check_qualifier( Q ),
			{ K, { V, Q } };

		{ value, _PreviousValue={ Vp, Qp } } ->
			Previous = { K, Vp, Qp },
			New = { K, V, Q },
			throw( { data_already_defined, Previous, New } )

	end;

get_entry_to_define( E={ K, _V }, _Table) ->
	throw( { data_definition_failed, { key_must_be_atom, K }, E } );

get_entry_to_define( E={ K, _V, _Q }, _Table) ->
	throw( { data_definition_failed, { key_must_be_atom, K }, E } );

get_entry_to_define( Other, _Table ) ->
	throw( { data_definition_failed, invalid_entry, Other } ).





% Modification section.


% Checks that all entries were already defined and that their qualifier was
% mutable indeed. Whether or not a qualifier was omitted, the returned triplet
% list is ready for a commit modification (i.e. qualifiers are already the ones
% to write directly).
%
prepare_entries_to_modify( Entries, Table ) ->
	% Each entry is either a pair or a triplet, and will be managed accordingly:
	[ get_entry_to_modify( E, Table ) || E <- Entries ].




% Returns the {K, {V,Q} } final entry corresponding to qualifier-less specified
% one.
%
get_entry_to_modify( E={ K, V }, Table ) when is_atom( K ) ->

	% No qualifier specified here:
	case table:lookup_entry( K, Table ) of

		{ value, PreviousEntry={ _PreviousValue, _PreviousQualifier=const } } ->
			throw( { const_data_cannot_be_modified, E, PreviousEntry } );

		key_not_found ->
			throw( { non_already_defined_data, E } );

		% We do not really have to check previous qualifier, we know it is
		% mutable:
		%
		% {value,PreviousEntry={_PreviousValue,_PreviousQualifier=mutable}} ->
		_ ->
			% No qualifier specified, thus data is to remain mutable:
			{ K, { V, mutable } }

	end;

% Returns the {K, {V, Q}} final entry corresponding to qualifier-including
% specified one.
%
get_entry_to_modify( E={ K, V, Q }, Table ) when is_atom( K ) ->

	% No qualifier specified here:
	case table:lookup_entry( K, Table ) of

		{ value, PreviousEntry={ _PreviousValue, _PreviousQualifier=const } } ->
			throw( { const_data_cannot_be_modified, E, PreviousEntry } );

		key_not_found ->
			throw( { non_already_defined_data, E } );

		% We do not really have to check previous qualifier, we know it is
		% mutable:
		%
		% {value,PreviousEntry={ _PreviousValue,
		%                        _PreviousQualifier=mutable } } ->
		_ ->
			check_qualifier( Q ),
			{ K, { V, Q } }

	end;

get_entry_to_modify( E={ K, _V }, _Table ) ->
	throw( { data_modification_failed, { key_must_be_atom, K }, E } );

get_entry_to_modify( E={ K, _V, _Q }, _Table ) ->
	throw( { data_modification_failed, { key_must_be_atom, K }, E } );

get_entry_to_modify( Other, _Table ) ->
	throw( { data_modification_failed, invalid_entry, Other } ).



% Adds specified entries to the pending commits.
%
% We check each of these entries against the already-pending ones, but also
% against the other entries of the input list.
%
add_commits( _EntryList=[], PendingCommits ) ->
	PendingCommits;

add_commits( _EntryList=[ E | T ], PendingCommits ) ->
	NewPendingCommits = add_commit( E, PendingCommits ),
	add_commits( T, NewPendingCommits ).



% Adds specified entry in the pending commits.
%
% Returns a new commit list.
%
% We check this entry against the already-pending ones.
%
add_commit( Entry={ Key, _P }, PendingCommits ) ->

	case lists:keysearch( Key, _Index=1, PendingCommits ) of

		false ->
			% OK, so here we will add this commit "as is":
			[ Entry | PendingCommits ];

		{ value, PastEntry } ->
			% A key must be specified up to once per diasca:
			throw( { commit_collision, PastEntry, Entry } )

	end.



% Helper function for the actual data initial definition, from simulation case
% or from initial actors.
%
% Returns an updated state.
%
define_data_recursive( EntryList, State ) when is_list( EntryList ) ->

	none = ?getAttr(parent_exchanger_pid),

	% Returned result is a list of initial_data_defined atoms, not interesting
	% as such here, thus ignored:
	%
	{ NewState, _Res } = executeRequestInTree( State,
			_RequestName=defineDataHelper, _Params=[ EntryList ] ),

	NewState;


% Helper function for the actual data initial definition, from simulation case
% or from initial actors.
%
% Entry is { K, {V,Q} }.
%
% Returns an updated state.
%
define_data_recursive( Entry, State ) ->

	none = ?getAttr(parent_exchanger_pid),

	% Returned result is a list of initial_data_defined atoms, not interesting
	% as such here, thus ignored:
	%
	{ NewState, _Res } = executeRequestInTree( State,
			_RequestName=defineDataHelper, _Params=Entry ),

	NewState.




% Helper function for the actual data initial modification, from simulation case
% or from initial actors.
%
% Returns an updated state.
%
modify_data_recursive( EntryList, State ) when is_list( EntryList ) ->

	none = ?getAttr(parent_exchanger_pid),

	% Returned result is a list of initial_data_modified atoms, not interesting
	% as such here, thus ignored:
	%
	{ NewState, _Res } = executeRequestInTree( State,
			_RequestName=modifyDataHelper, _Params=[ EntryList ] ),

	NewState;


% Helper function for the actual data initial modification, from simulation case
% or from initial actors.
%
% Returns an updated state.
%
modify_data_recursive( Entry, State ) ->

	none = ?getAttr(parent_exchanger_pid),

	% Returned result is a list of initial_data_modified atoms, not interesting
	% as such here, thus ignored:
	%
	{ NewState, _Res } = executeRequestInTree( State,
			_RequestName=modifyDataHelper, _Params=[ Entry ] ),

	NewState.





% Requests (up to once per diasca) the root time manager to trigger an
% inter-diasca notification for the current diasca, if needed. Will trigger in
% turn a onInterDiascaBegin/1 call.
%
% Returns an updated state.
%
% (helper function)
%
-spec manage_inter_diasca_notification( wooper:state() ) -> wooper:state().
manage_inter_diasca_notification( State ) ->

	case ?getAttr(interdiasca_requested) of

		true ->
			% Already done:
			State;

		false ->

			% Must be synchronous, to avoid race conditions:
			?getAttr(root_time_manager_pid) !
				{ requestInterDiascaNotification, [], self() },

			receive

				{ wooper_result, interdiasca_tracked } ->
					ok

			end,

			setAttribute( State, interdiasca_requested, true )

	end.



% Returns the qualifier that shall apply if the data definition did not specify
% it.
%
-spec get_default_qualifier() -> qualifier().
get_default_qualifier() ->
	const.


% Checks that specified qualifier is a known supported one.
-spec check_qualifier( basic_utils:user_data() ) -> void().
check_qualifier( const ) ->
	ok;

check_qualifier( mutable ) ->
	ok;

check_qualifier( Other ) ->
	throw( { invalid_data_qualifier, Other } ).



% Performs operations common to all variations of constructors.
%
% Returns an updated state.
%
% (helper function)
%
-spec common_construct( ustring(), wooper:state() ) -> wooper:state().
common_construct( ExchangerName, State ) ->

	% First the direct mother classes:
	TraceState = class_EngineBaseObject:construct( State,
									   ?trace_categorize(ExchangerName) ),

	JSONState = case json_utils:get_parser_backend_name() of

		undefined ->
			setAttribute( TraceState, json_parser_state, undefined );

		BackendName ->
			JSonParserState = json_utils:start_parser( BackendName ),
			setAttribute( TraceState, json_parser_state, JSonParserState )

	end,

	setAttributes( JSONState, [
		{ simulation_running, false },
		{ root_exchanger_pid, undefined },
		{ parent_exchanger_pid, undefined },
		{ child_exchangers, [] },
		{ feeder_files, [] },
		{ data_table, table:new() },
		{ root_time_manager_pid, undefined },
		{ pending_commits, [] },
		{ interdiasca_requested, false } ] ).



% Parses specified configuration files, updates the data table accordingly, and
% returns a new state.
%
% FileList must be a list of plain strings.
%
% NodeType allows to tell whether we are a (normal) computing-node based
% data-exchanger or a user-node based data-exchanger (created ad-hoc as no other
% was available on the user host).
%
% Returns an updated state.
%
% (helper function)
%
-spec parse_files( [ file_path() ], node_type(), wooper:state() ) ->
							wooper:state().
parse_files( FileList, NodeType, State ) ->

	%?notice_fmt( "Parsing following files: ~ts",
	%			 [ text_utils:strings_to_string( FileList ) ] ),

	DataTable = ?getAttr(data_table),

	BasePath = case NodeType of

		computing_node ->
			% Normal case:
			class_Actor:get_deployed_root_directory( State );

		user_node ->
			% Here we have to supply an adequate path for the ad-hoc user-node
			% local exchanger, as unlike computing nodes relying on the
			% directory for temporary data (by default '/tmp') it is still in
			% the user current directory:
			%
		   class_DeploymentManager:determine_root_directory()

	end,

	%trace_utils:debug_fmt( "BasePath = ~ts.", [ BasePath ] ),

	AbsolutePathList = [ file_utils:join( BasePath, Path )
						 || Path <- FileList ],

	JsonParserState = ?getAttr(json_parser_state),

	NewDataTable = lists:foldl(
		fun( Filename, AccTable ) ->
			parse_file( Filename, AccTable, JsonParserState )
		end,
		DataTable,
		AbsolutePathList ),

	% Optimising the load factor may enhance the table look-up performances:
	setAttribute( State, data_table, table:optimise( NewDataTable ) ).



% Parses specified file, updates the specified data table accordingly, and
% returns it.
%
% (helper function)
%
-spec parse_file( file_path(), data_table(), maybe( json_parser_state() ) ) ->
						data_table().
parse_file( Filename, DataTable, MaybeJsonParserState ) ->

	FileEntries = case file_utils:is_existing_file( Filename ) of

		true ->

			% We currently consider that such a file is either specified in JSON
			% format, or as Erlang terms (i.e. in file:consult/1 format):

			case file_utils:get_extension( Filename ) of

				no_extension ->
					manage_term_file( Filename );

				Ext ->

					case text_utils:to_lowercase( Ext ) of

						"json" ->
							case MaybeJsonParserState of

								undefined ->
									trace_utils:error_fmt(
									  "A JSON file ('~ts') was specified for "
									  "the data-exchanger, whereas no JSON "
									  "support is available.", [ Filename ] ),

									throw( { json_support_lacking_to_exchange,
											 Filename } );

								JsonParserState ->
									manage_json_file( Filename,
													  JsonParserState )

							end;

						_ ->
							manage_term_file( Filename )

					end

			end;

		false ->
			throw( { file_to_exchange_not_found, Filename, node() } )

	end,

	try

		DefinitionList = prepare_entries_to_define( FileEntries, DataTable ),

		table:add_entries( DefinitionList, DataTable )

	catch

		throw:Exception ->
			throw( { invalid_file_to_exchange, Filename, Exception } )

	end.



% Returns the table entries corresponding to the content of specified file
% (containing Erlang terms).
%
-spec manage_term_file( file_path() ) -> data_table().
manage_term_file( Filename ) ->

	case file:consult( Filename ) of

		% AnyError is ErrorTuple={Line,Mod,Term} or AtomError:
		{ error, AnyError } ->
			throw( { data_file_parsing_failed, Filename,
					 lists:flatten( file:format_error( AnyError ) ) } );

		{ ok, EntryList } ->
			%trace_utils:debug_fmt( "Parsed entries from '~ts':~n~p",
			%                       [ Filename, Entries ] ),
			EntryList

	end.



% Returns the table entries corresponding to the content of specified JSON file.
-spec manage_json_file( file_path(), maybe( json_utils:parser_state() ) ) ->
								data_table().
manage_json_file( Filename, JsonParserState ) ->

	JsonContent = file_utils:read_whole( Filename ),

	% We expect to obtain here a proplist (a list of pairs, whose first element
	% is an atom):
	%
	EntryList = json_utils:from_json( JsonContent, JsonParserState ),

	%trace_utils:debug_fmt( "Parsed JSON entries from ~ts:~n~p",
	%                       [ Filename, EntryList ] ),

	EntryList.



% Helper functions to manage operation in the full data-exchanger tree.
% Somewhat similar to a map/reduce.


% Executes specified request (atom) with specified list of parameters
% recursively through the whole data-exchanger hierarchy.
%
% Returns a list of the results.
%
-spec executeRequestInTree( wooper:state(), method_name(),
				method_arguments() ) -> request_return( [ term() ] ).
executeRequestInTree( State, MethodName, Parameters ) ->

	%trace_utils:debug_fmt( "executeRequestInTree in for ~ts with "
	%                       "parameters ~p.", [ MethodName, Parameters ] ),

	% The only goal of the code here is to return Res, not {self(),Res}.

	% We use executeRequest here, as we cannot know whether the
	% executeRequestInTreeHelper method will return a message containing
	% wooper_result or not, depending on the WOOPER debug mode:
	% (note: this reason does not apply anymore, {NewState,Result} is always
	% returned)
	{ NewState, { _Self, Res } } = executeRequest( State,
					executeRequestInTreeHelper, [ MethodName, Parameters ] ),

	%trace_utils:debug_fmt( "executeRequestInTree out for ~ts with "
	%                       "parameters ~p.", [ MethodName, Parameters ] ),

	wooper:return_state_result( NewState, Res ).



% Recursive helper for executeRequestInTree/3.
-spec executeRequestInTreeHelper( wooper:state(), method_name(),
		method_arguments() ) ->
					request_return( { data_exchanger_pid(), [ term() ] } ).
executeRequestInTreeHelper( State, MethodName, Parameters ) ->

	% Depth-first, as we want to parallelise as much as possible:

	RequestMessage = { executeRequestInTreeHelper, [ MethodName, Parameters ],
					   self() },

	Children = ?getAttr(child_exchangers),

	[ C ! RequestMessage || C <- Children ],

	% Before waiting for the answers, perform our own work in the meantime:
	{ ExecutedState, LocalRes } =
		executeRequest( State, MethodName, Parameters ),

	% Now wait and collect answers:

	%trace_utils:debug_fmt( "Will wait on ~p for following children: ~p.",
	%		  [ node(), Children ] ),

	ChildRes = wait_for_tree( Children, _Acc=[] ),

	%trace_utils:debug_fmt( "Children ~p answered.~n", [ Children ] ),

	% We must return the PID of this process, to know which are not to be waited
	% anymore:
	%
	wooper:return_state_result( ExecutedState,
								 { self(), [ LocalRes | ChildRes ] } ).



% Waits for specified list of PIDs, and aggregates their result.
%
% Will block forever if at least one child does not answer.
%
% Returns a list of {ChildPid, ChildRes} pairs.
%
wait_for_tree( _WaitedPidList=[], Acc ) ->
	Acc;

wait_for_tree( WaitedPidList, Acc ) ->

	receive

		{ wooper_result, { Pid, ResList } } ->

			case lists:member( Pid, WaitedPidList ) of

				true ->
					NewWaitedPidList = lists:delete( Pid, WaitedPidList ),
					%trace_utils:debug_fmt( "wait_for_tree received an "
					%  answer from ~p, waited list is now ~p.",
					% [ Pid, NewWaitedPidList ] ),
					wait_for_tree( NewWaitedPidList, ResList ++ Acc );

				false ->
					throw( { unexpected_tree_child, Pid } )

			end

	after 2000 ->

			trace_utils:warning_fmt( "No answer from ~p.", [ WaitedPidList ] ),
			wait_for_tree( WaitedPidList, Acc )

	end.
