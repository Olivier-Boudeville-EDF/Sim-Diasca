% Copyright (C) 2014-2021 EDF R&D

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


-module(class_PluginManager).


-define( class_description,
		 "Manager of the Sim-Diasca plugins, which allows third-party tools "
		 "to interface% to the engine." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).


% The class-specific attributes of a plugin manager:
-define( class_attributes, [

	{ plugin_table, table( basic_utils:module_name(), maybe( term() ) ),
	  "an associative table whose keys are the module name of each plugin "
	  "(as an atom) and whose values are any state information returned by a "
	  "given plugin" } ] ).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.PluginManagement" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% For plugin_manager_name:
-include("class_PluginManager.hrl").


% For the configuration_changes record:
-include("sim_diasca_plugin.hrl").



% Where the plugin manager should be registered.
% Could be local_and_global or global_only as well:
%
%-define( registration_scope, local_only ).
-define( registration_scope, local_and_global ).


% How the plugin manager shall be looked-up internally:
-define( look_up_scope, global ).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type directory_path() :: file_utils:directory_path().
-type file_name() :: file_utils:file_name().

-type plugin_event() :: sim_diasca_plugin:plugin_event().
-type event_data() :: sim_diasca_plugin:event_data().
-type configuration_changes() :: sim_diasca_plugin:configuration_changes().



% Constructs a new plugin manager, from following parameter: PluginDirectories
% :: [ directory_path() ], a list of the paths that should be
% searched into, in order to look-up for plugins
%
-spec construct( wooper:state(), [ directory_path() ] ) -> wooper:state().
construct( State, PluginDirectories ) ->

	TraceState = class_EngineBaseObject:construct( State,
									?trace_categorize("Plugin Manager") ),

	% Then the class-specific actions:

	% Ensures also it is a singleton indeed:
	naming_utils:register_as( ?plugin_manager_name, ?registration_scope ),

	DirMessage = case PluginDirectories of

		[] ->
			"no plugin directory specified.";

		_ ->
			text_utils:format( "following ~B plugin directories specified: ~s",
				[ length( PluginDirectories ),
				  text_utils:strings_to_string( PluginDirectories ) ] )

	end,

	?send_info( TraceState, "Initialising plugin manager, with "
				++ DirMessage ),


	% List of absolute paths, extension-less BEAMs:
	Plugins = get_plugins_from( PluginDirectories, TraceState ),

	% Keys are the plugin module names (as atoms), associated values start at
	% 'undefined':
	%
	PluginTable = create_initial_plugin_table( Plugins, TraceState ),

	ModuleState = setAttribute( TraceState, plugin_table, PluginTable ),

	load_plugins( Plugins, ModuleState ),

	ModuleState.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?info( "Deleting plugin manager." ),

	class_InstanceTracker:unregister_agent(),

	naming_utils:unregister( ?plugin_manager_name, ?registration_scope ),

	?debug( "Plugin manager deleted." ),

	% Then allow chaining:
	State.




% Methods section.


% Requests all plugins to be notified of following standard event.
-spec notifyEvent( wooper:state(), plugin_event() ) ->
						request_return( 'event_notified' ).
notifyEvent( State, Event ) ->

	NewState = notify_event( Event, State ),

	wooper:return_state_result( NewState, event_notified ).



% Requests all plugins to be notified of the start of the simulatr, and gives
% them a chance of updating the requested configuration changes.
%
% (request, for synchronicity)
%
-spec notifySimulatorStart( wooper:state() ) ->
		request_return( configuration_changes() ).
notifySimulatorStart( State ) ->

	?info( "Notifying all plugins that the simulator starts, "
		   "giving them a chance of updating the requested "
		   "configuration changes." ),

	BlankConfChanges = #configuration_changes{},

	InitialPluginTable = ?getAttr(plugin_table),

	{ FinalChanges, FinalTable } = lists:foldl(
		fun( { Mod, PlugState }, { Changes, Table } ) ->

				{ NewChanges, NewPlugState } =
					Mod:on_simulator_start( Changes, PlugState ),

				% Update state:
				NewTable = table:add_entry( _K=Mod, _V=NewPlugState, Table ),

				{ NewChanges, NewTable }

		end,
		_Acc0={ BlankConfChanges, InitialPluginTable },
		_List=table:enumerate( InitialPluginTable ) ),

	?info_fmt( "Final requested configuration changes: ~p.", [ FinalChanges ] ),

	FinalState = setAttribute( State, plugin_table, FinalTable ),

	wooper:return_state_result( FinalState, FinalChanges ).



% Requests all plugins to be notified of following parametrised event.
%
% (request, for synchronicity)
%
-spec notifyParametrisedEvent( wooper:state(), plugin_event(), event_data() ) ->
				 request_return( 'parametrised_event_notified' ).
notifyParametrisedEvent( State, Event, Parameters ) ->

	NewState = notify_parametrised_event( Event, Parameters, State ),

	wooper:return_state_result( NewState, parametrised_event_notified ).



% Requests all plugins to be notified of following case-specific event, with
% its associated parameter.
%
% (request, for synchronicity)
%
-spec notifyCaseSpecificEvent( wooper:state(),
		sim_diasca_plugin:case_specific_event(), event_data() ) ->
				request_return( 'case_specific_event_notified' ).
notifyCaseSpecificEvent( State, CaseSpecificEvent, EventParameter ) ->

	NewState =
		notify_case_specific_event( CaseSpecificEvent, EventParameter, State ),

	wooper:return_state_result( NewState, case_specific_event_notified ).




% Helper functions.


% Returns a list of plugins found, as a list of the extension-less absolute
% paths of the plugin modules.
%
-spec get_plugins_from( [ directory_path() ], wooper:state() ) ->
							[ ustring() ].
get_plugins_from( PluginDirectories, State ) ->
	get_plugins_from( PluginDirectories, State, _AccPlugins=[] ).



% (helper)
get_plugins_from( _PluginDirectories=[], _State, AccPlugins ) ->
	AccPlugins;

get_plugins_from( _PluginDirectories=[ Dir | T ], State, AccPlugins ) ->

	AbsDir = file_utils:ensure_path_is_absolute( Dir ),

	case file_utils:is_existing_directory( AbsDir ) of

		true ->
			NewPlugins = case get_plugins_from_dir( AbsDir ) of

				[] ->
					?debug_fmt( "No plugin found in directory '~s'.",
								[ AbsDir ] ),
					[];

				Plugins ->
					?debug_fmt( "~B plugin(s) found in directory '~s': ~p.",
								[ length( Plugins ), AbsDir, Plugins ] ),
					Plugins

			end,

			get_plugins_from( T, State, NewPlugins ++ AccPlugins );

		false ->
			?debug_fmt( "Plugin directory '~s' does not exist.", [ AbsDir ] ),
			get_plugins_from( T, State, AccPlugins )

	end.



% Returns a list of the BEAM files (absolute paths, but with their extension
% removed) found in the specified directory.
%
% (helper)
%
-spec get_plugins_from_dir( directory_path() ) -> [ file_name() ].
get_plugins_from_dir( DirectoryName ) ->

	% First, select all BEAM regular files:
	{ Files, _Symlinks, _Dirs, _Others, _Devs } =
		file_utils:list_dir_elements( DirectoryName ),

	Beams = file_utils:filter_by_extension( Files, ".beam" ),

	% Then remove their extension (to specify moduels) and make them absolute
	% paths:
	%
	Modules = [ file_utils:replace_extension( _Filename=B,
		   _SourceExtension=".beam", _TargetExtension="" ) || B <- Beams ],

	% Full paths needed:
	[ file_utils:join( DirectoryName, M ) || M <- Modules ].



% Initialises and returns the plugin table.
%
% (helper)
%
create_initial_plugin_table( Plugins, State ) ->

	case Plugins of

		[] ->
			?info( "Plugin manager started, but no plugin found." ),
			table:new();

		_ ->

			StringModules = [ filename:basename( P ) || P <- Plugins ],

			Count = length( Plugins ),

			?info_fmt( "Plugin manager started, with ~B plugin(s): ~s",
					   [ Count,
						 text_utils:strings_to_string( StringModules ) ] ),

			Modules = [ text_utils:string_to_atom( S ) || S <- StringModules ],

			EmptyTable = table:new(),

			lists:foldl( fun( Mod, Table ) ->
							% Initial plugin state is undefined:
							table:add_entry( _K=Mod, _V='undefined', Table )
						 end,
						 _Acc0=EmptyTable,
						 _List=Modules )

	end.



% Loads specified plugins.
load_plugins( Plugins, State ) ->
	[ load_plugin( P, State ) || P <- Plugins ].


% Loads specified plugin.
load_plugin( Plugin, State ) ->

	% No need to tweak the code paths:
	case code:load_abs( Plugin ) of

		{ error, Reason } ->
			?error_fmt( "Loading of plugin '~s' failed: ~s.",
						[ Plugin, Reason ] ),
			throw( { plugin_loading_failed, Plugin, Reason } );

		{ module, Module } ->
			?debug_fmt( "Plugin '~s' successfully loaded from '~s'.",
						[ Module, filename:dirname( Plugin ) ] )

	end.



% Notifies known plugins of specified event; returns an updated state.
%
% (helper)
%
notify_event( Event, State ) ->

	PluginTable = ?getAttr(plugin_table),

	?info_fmt( "Notifying all plugins of event '~s'.", [ Event ] ),

	NewTable = lists:foldl( fun( { Mod, PlugState }, Table ) ->

		NewPlugState = Mod:Event( PlugState ),

		% Update state:
		table:add_entry( _K=Mod, _V=NewPlugState, Table )

							end,
							_Acc0=PluginTable,
							_List=table:enumerate( PluginTable ) ),

	setAttribute( State, plugin_table, NewTable ).



% Notifies known plugins of specified parametrised event; returns an updated
% state.
%
% (helper)
%
notify_parametrised_event( Event, Parameters, State ) ->

	PluginTable = ?getAttr(plugin_table),

	?info_fmt( "Notifying all plugins of event '~s' "
			   "parametrised with '~p'.", [ Event, Parameters ] ),

	NewTable = lists:foldl( fun( { Mod, PlugState }, Table ) ->

		NewPlugState = Mod:Event( Parameters, PlugState ),

		% Update state:
		table:add_entry( _K=Mod, _V=NewPlugState, Table )

							end,
							_Acc0=PluginTable,
							_List=table:enumerate( PluginTable ) ),

	setAttribute( State, plugin_table, NewTable ).



% Notifies known plugins of specified case-specific event; returns an updated
% state.
%
% (helper)
%
notify_case_specific_event( CaseSpecificEvent, EventParameter, State ) ->

	PluginTable = ?getAttr(plugin_table),

	?info_fmt( "Notifying all plugins of case-specific event '~s' "
			   "with parameter '~p'.", [ CaseSpecificEvent, EventParameter ] ),

	NewTable = lists:foldl( fun( { Mod, PlugState }, Table ) ->

		NewPlugState = Mod:on_case_specific_event( CaseSpecificEvent,
												   EventParameter, PlugState ),

		% Update state:
		table:add_entry( _K=Mod, _V=NewPlugState, Table )

							end,
							_Acc0=PluginTable,
							_List=table:enumerate( PluginTable ) ),

	setAttribute( State, plugin_table, NewTable ).




% Static section.


% To notify from any place the plugin manager of an event.
-spec notify( plugin_event() ) -> static_void_return().
notify( Event ) ->

	PluginManagerPid = naming_utils:get_registered_pid_for(
						 ?plugin_manager_name, _Scope=?look_up_scope ),


	PluginManagerPid ! { notifyEvent, [ Event ], self() },

	receive

		{ wooper_result, event_notified } ->
			wooper:return_static_void()

	end.



% To notify from any place the plugin manager - provided it is registered - of
% an event.
%
-spec notify_if_registered( plugin_event() ) -> static_void_return().
notify_if_registered( Event ) ->

	case naming_utils:is_registered( ?plugin_manager_name,
									 _Scope=?look_up_scope ) of

		not_registered ->
			wooper:return_static_void();

		PluginManagerPid ->
			PluginManagerPid ! { notifyEvent, [ Event ], self() },

			receive

				{ wooper_result, event_notified } ->
					wooper:return_static_void()

			end

	end.



% Notifies that the simulator started, in order to allow plugins to return
% requests for configuration changes.
%
-spec notify_simulator_start() -> static_return( configuration_changes() ).
notify_simulator_start() ->

	PluginManagerPid = naming_utils:get_registered_pid_for(
							 ?plugin_manager_name, _Scope=?look_up_scope ),

	% Starts with blank changes:
	PluginManagerPid ! { notifySimulatorStart, [], self() },

	receive

		{ wooper_result, ConfigurationChanges } ->
			wooper:return_static( ConfigurationChanges )

	end.



% To notify from any place the plugin manager of a parametrised event.
-spec notify( plugin_event(), event_data() ) -> static_void_return().
notify( Event, Parameters ) ->

	PluginManagerPid = naming_utils:get_registered_pid_for(
						?plugin_manager_name, _Scope=?look_up_scope ),


	PluginManagerPid ! { notifyParametrisedEvent, [ Event, Parameters ],
						 self() },

	receive

		{ wooper_result, parametrised_event_notified } ->
			wooper:return_static_void()

	end.



% To notify from any place the plugin manager of a case-specific event.
-spec notify_case_specific( sim_diasca_plugin:case_specific_event(),
							event_data() ) -> static_void_return().
notify_case_specific( Event, EventParameter ) ->

	PluginManagerPid = naming_utils:get_registered_pid_for(
						?plugin_manager_name, _Scope=?look_up_scope ),

	PluginManagerPid ! { notifyCaseSpecificEvent, [ Event, EventParameter ],
						 self() },

	receive

		{ wooper_result, case_specific_event_notified } ->
			wooper:return_static_void()

	end.
