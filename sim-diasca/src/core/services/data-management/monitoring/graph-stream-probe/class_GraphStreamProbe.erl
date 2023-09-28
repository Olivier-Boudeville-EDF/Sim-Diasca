% Copyright (C) 2022-2023 EDF R&D
%
% This file is part of Sim-Diasca.
%
% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.
%
% Authors: Jérôme Cantenot    [jerome (dot) cantenot (at) edf (dot) fr]
%          Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
%
% Creation date: Tuesday, June 21, 2022.


% @doc Probe able to process and display <b>streams of graphs</b>, that is
% graphs that may change over (simulation) time.
%
-module(class_GraphStreamProbe).



% For Gephi-related troubleshooting:
%
% - use the Window -> Output menu item in order to display a log window
%
% - apparently in some cases a Gephi server can become unresponsive; use
% 'killall java' to clear any prior instance thereof
%
% - command-line testing can be done for example with:
%    $ wget --post-data "XXX" "localhost:8090/myproject?operation=updateGraph"



% Implementation notes:
%
% A graph stream probe will fail if its target Gephi server is not listening at
% the target TCP port.
%
% A Gephi server could be detected and waited for, by polling its expected TCP
% port (especially useful to wait for the launch of the tool).




-define( class_description,
		 "Probe able to process and display streams of graphs.").


-define( superclasses, [ class_ResultProducer ] ).


-define( class_attributes, [

	{ gephi_host, bin_fqdn(),
	  "the host (possibly localhost) on which a suitable Gephi server is "
	  "expected to run" },

	{ gephi_port, tcp_port(),
	  "the TCP port on which a suitable Gephi server is expected to run" },

	{ update_url, bin_url(),
	  "the preprocessed base URL used to trigger update calls to Gephi" },

	{ timestamp_table,
	  table( app_service_pid(), { ins(), [ { timestamp(), time_factor() } ] } ),
	  "a table associating to the PID of each service its INS and time-related "
	  "information, in reverse chronological order" } ] ).


% Exported helpers:
-export([]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Result management.GraphStreamProbe" ).


-type project_path() :: file_path().
% A user-specified path to a (Gephi) project file, from which a project name may
% be deduced.
%
% For example the '/tmp/foo/my_project.gephi' path is to correspond to the
% 'my_project' project.
%
% The project path is typically defined in the deployment settings (rather than
% for example when creating a graph stream probe).


-type project_name() :: ustring().
% The name of a (Gephi) project, typically one that shall be loaded. For
% example, a "Foobar" project name would refer to a "Foobar.gephi" project file.


-type bin_project_name() :: bin_string().
% The name of a (Gephi) project, typically one that shall be loaded. For
% example, a <<"Foobar">> project name would refer to a "Foobar.gephi" project
% file.


-type any_project_name() :: project_name() | bin_project_name().
% The name of a (Gephi) project, typically one that shall be loaded.


-type graph_stream_probe_pid() :: class_ResultProducer:producer_pid().


-type graph_stream_probe_ref() :: 'non_wanted_probe' | graph_stream_probe_pid().
% A probe may not be a wanted result producer.


-type element_id() :: any_string().
% The identifier of a graph element (e.g. node, edge).


-type node_id() :: element_id().
% The identifier of a graph node.

-type edge_id() :: element_id().
% The identifier of a graph edge.

-type property_id() :: element_id().


-type element_label() :: any_string().
% A label that can be associated to a graph element (e.g. node, edge).


-type graph_value() :: float().
% A value associated to a given timestamp in a graph.
%
% It would be interesting to determine whether other datatypes can be accepted
% (e.g. booleans, integers, non-scalar types).


-type time_factor() :: float().
% Typically in [0.0, 1.0].


-export_type([ project_path/0, project_name/0, bin_project_name/0,
			   graph_stream_probe_pid/0, graph_stream_probe_ref/0,
			   element_id/0, node_id/0, edge_id/0, property_id/0,
			   element_label/0,
			   graph_value/0, time_factor/0 ]).




% The default (non-priviledged) TCP port on which Gephi is to run:
-define( gephi_default_tcp_port, 8090 ).


% The file extension of a Gephi project:
-define( gephi_project_extension, "gephi" ).



% For getAttr/1, etc.:
-include_lib("wooper/include/wooper.hrl").

% For app_info*:
-include_lib("traces/include/traces.hrl").



% Gephi installation:
%
% Our convention is to have Gephi installed in GEPHI_ROOT=~/Software/gephi, in
% which a 'gephi-current-install' symbolic link is to point to an actual sibling
% installation directory with a version, like: 'gephi-0.9.7'; for example:
%
% $ tree -d -L 1 ~/Software/gephi/
% ~/Software/gephi/
% ├── gephi-0.9.5
% ├── gephi-0.9.6
% ├── gephi-0.9.7
% └── gephi-current-install -> gephi-0.9.7
%
% Then, gephi can be run in all cases as
% '~/Software/gephi/gephi-current-install/bin/gephi'.
%
% So the installation boils down to selecting the latest stable version of Gephi
% from https://gephi.org/users/download/ (we recommend to avoid the 0.9.6
% version), to download a corresponding gephi-x.y.z-linux-x64.tar.gz archive in
% ~/Software/gephi/, to extract it (e.g. 'tar xvf
% gephi-x.y.z-linux-x64.tar.gz'), and to create a sibling gephi-current-install
% symbolic link pointing to it.
%
% The ~/Software/gephi/gephi-current-install/bin directory shall then preferably
% be set in the PATH environment variable for good (e.g. in one's .bashrc).


% Gephi configuration:
%
% The following steps are needed:
%  - installing the 'Graph Streaming' plugin (listed in Tools -> Plugins ->
%  'Available Plugins'), for example in version 1.0.3
%  - configuring a correct TCP port to be listened to by the Gephi server (see
%  the comments of run_gephi/0 for that)
%  - defining and loading a suitable project file (e.g. 'foobar.gephi')


% Gephi use:
%
% Refer to the comments in run_gephi/0.
%
% Launching a suitable Gephi server requires an host (by default the local one)
% and a port (see the gephi_default_tcp_port define for defaults).
%
% At least currently, Gephi cannot be fully launched only from the command-line,
% as a Gephi server must be started and a project file (typically bearing a
% *.gephi extension) must be selected.
%
% The run_gephi/{0,1,2} static methods automate the launching as much as
% currently possible.
%
% If a Gephi instance is already running, launching another one just sets the
% focus on that initial instance; as a consequence, on a given host, the risk of
% having conflicting instances is low (especially if they are to operate on the
% same TCP port, as up to one can be started).
%
% See also: executable_utils:get_default_graph_stream_tool_{name,path}/0.



% Data files
%
% We recommend the use of the GraphML graph format, and relying on the
% '.graphml' file extension for that.


% Feeding such probes:
%
% Most operations are available either as oneways (most efficient, "fire and
% forget" operations) or as requests (more demanding yet safer, as synchronised
% by a returned message, whether or not a specific information is to be sent
% back).
%
% The goal is to avoid any race condition, for example if having triggered
% operations that are still pending whereas shutting down. Such operations may
% indeed not be processed yet, or may accumulate in memory (example of a
% simulation faster to progress than such a probe) and exhaust it.
%
% To prevent such issues, each batch of operations (preferably, for efficiency
% reasons, to each single operation) shall be synchronised; so each of the calls
% in the series may be synchronous, or, better, only the last one (possibly
% itself being a mere call to sync/1) can be synchronous (as they are guaranteed
% to be evaluated in order). Then operations that are both safe and efficient
% are implemented.


% Method naming: alter* (e.g. alter{Node,Edge}) has been preferred to change*
% (as clearer, cannot be taken for switch*).



% Implementation notes:
%
% This probe is based on a third-party graph stream tool, namely Gephi
% (https://gephi.org/).
%
% This probe is to communicate with a Gephi server.
%
% This communication may be asynchronous (fastest, yet prone to race conditions)
% or not.
%
% Synchronous operations do not return for example the identifiers involved, as
% a single calling process is generally involved.



% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type file_path() :: file_utils:file_path().

-type tcp_port() :: net_utils:tcp_port().
-type possibly_local_bin_hostname() :: net_utils:possibly_local_bin_hostname().
-type string_host_name() :: net_utils:string_host_name().

-type bin_json() :: json_utils:bin_json().

-type status_code() :: rest_utils:status_code().

-type probe_name_init() :: class_Probe:probe_name_init().
-type probe_name() :: class_Probe:probe_name().

-type producer_options() :: class_ResultProducer:producer_options().
-type producer_result() :: class_ResultProducer:producer_result().



% TODO:
%
% - should we launch a Gephi server in batch mode?
%
% - create check_gephi_server( tcp_port() ) -> boolean() based on a no-op JSON
% call testing the availability of that server
%
% - add a 'sync/1' request to force synchronisation of such a probe with its
% target server
%
% - define synchronous counterparts to most/all operations
%
% - manage gracefully the ending timestamp



% @doc Constructs a graph stream probe of the specified name, using a local
% server running on a default TCP port, and that will send its samples based on
% the specified project name.
%
-spec construct( wooper:state(), probe_name_init(), project_name() ) ->
									wooper:state().
construct( State, NameInit, ProjectName ) ->
	construct( State, NameInit, _Hostname="localhost", ProjectName ).



% @doc Constructs a graph stream probe of the specified name, using a server
% running on the specified host, on a default TCP port, and that will send its
% samples based on the specified project name.
%
-spec construct( wooper:state(), probe_name_init(), string_host_name(),
				 project_name() ) -> wooper:state().
construct( State, NameInit, Hostname, ProjectName ) ->
	construct( State, NameInit, Hostname, ?gephi_default_tcp_port,
			   ProjectName ).



% @doc Constructs a graph stream probe of the specified name, using a server
% running on the specified host and TCP port, and that will send its samples
% based on the specified project name.
%
% Most complete constructor.
%
-spec construct( wooper:state(), probe_name_init(),
		possibly_local_bin_hostname(), tcp_port(), any_project_name() ) ->
											wooper:state().
construct( State, NameInit, _PossiblyHostname=localhost, TCPPort,
		   AnyProjectName ) ->
	construct( State, NameInit, <<"localhost">>, TCPPort, AnyProjectName );


construct( State, NameInit, BinHostname, TCPPort, AnyProjectName )
								when is_integer( TCPPort ) ->

	ProbeName = class_Probe:get_actual_probe_name( NameInit ),

	ProducerState = class_ResultProducer:construct( State, ProbeName ),

	% For an increased interleaving:
	getAttribute( ProducerState, result_manager_pid ) !
		{ declareGraphStreamProbe,
		  [ text_utils:string_to_binary( ProbeName ),
			_IsTrackedProducer=true ], self() },

	?send_info_fmt( ProducerState, "Creating a Graph Stream probe, "
		"expecting a Gephi server to run on ~ts, TCP port #~B, and to have "
		"a relevant project already loaded.", [ BinHostname, TCPPort ] ),

	BinHostname =:= <<"localhost">> orelse
		begin
			net_utils:ping( BinHostname ) orelse
				?send_warning_fmt( ProducerState,
					"Unable to ping Gephi host '~ts'.", [ BinHostname ] )
		end,

	% Runtime resolution of path:
	{ JSONParserName, _JSONResolvablePath, JSONResolvedPath } =
		json_utils:get_parser_name_paths(),

	code_utils:declare_beam_directory( JSONResolvedPath ),

	json_utils:start_parser( JSONParserName ),

	web_utils:start(),

	BinProjectName = text_utils:ensure_binary( AnyProjectName ),

	% Determined once for all:
	BinUpdateUrl = text_utils:bin_format(
		"http://~ts:~B/~ts?operation=updateGraph",
		[ BinHostname, TCPPort, BinProjectName ] ),

	?send_debug_fmt( ProducerState, "Graph Stream probe created, "
		"to perform updates with '~ts', and using the '~ts' JSON parser.",
		[ BinUpdateUrl, JSONParserName ] ),

	ReadyState = setAttributes( ProducerState, [
		{ gephi_host, BinHostname },
		{ gephi_port, TCPPort },
		{ update_url, BinUpdateUrl },

		% At least currently, as a result producer, no specific element will be
		% to report:
		%
		{ result_produced, true } ] ),

	% After call to the declareGraphStreamProbe/4 request:
	wait_result_declaration_outcome( ProbeName, ReadyState ).




% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?info( "Deleting graph Stream probe." ),

	web_utils:stop(),
	json_utils:stop_parser(),

	% No unregistering from the Gephi server needed.

	State.




% @doc Adds (asynchronously) the specified node to the graph.
-spec addNode( wooper:state(), node_id(), element_label() ) ->
									const_oneway_return().
addNode( State, NodeId, Label ) ->

	Json = json_utils:to_json( #{ an => #{ NodeId => #{ label => Label } } } ),

	_StatusCode = post_gephi( Json, State ),

	wooper:const_return().



% @doc Adds (asynchronously) the specified directed edge between the two
% specified nodes of the graph.
%
-spec addDirectedEdge( wooper:state(), edge_id(), node_id(), node_id() ) ->
									const_oneway_return().
addDirectedEdge( State, EdgeId, SourceNodeId, TargetNodeId ) ->

	Json = json_utils:to_json( #{ ae => #{ EdgeId=> #{ source => SourceNodeId,
													   target => TargetNodeId,
													   directed => true } } } ),

	_StatusCode = post_gephi( Json, State ),

	wooper:const_return().



% @doc Modifies (asynchronously) the state of the specified node, by setting its
% specified property to the specified JSON value.
%
-spec alterNode( wooper:state(), node_id(), property_id(), bin_json() ) ->
							const_oneway_return().
alterNode( State, NodeId, PropertyId, PropertyValue ) ->

	_StatusCode = alter_node( NodeId, PropertyId, PropertyValue, State ),

	wooper:const_return().



% @doc Modifies synchronously the state of the specified node, by setting its
% specified property to the specified JSON value.
%
-spec alterNodeSync( wooper:state(), node_id(), property_id(), bin_json() ) ->
							const_request_return( 'node_altered' ).
alterNodeSync( State, NodeId, PropertyId, PropertyValue ) ->

	_StatusCode = alter_node( NodeId, PropertyId, PropertyValue, State ),

	wooper:const_return_result( node_altered ).


% (helper)
-spec alter_node( node_id(), property_id(), bin_json(), wooper:state() ) ->
											status_code().
alter_node( NodeId, PropertyId, PropertyValue, State ) ->

	%trace_utils:debug_fmt( "alter_node: PropertyValue=~p", [ PropertyValue ] ),

	Json = json_utils:to_json(
		#{ cn => #{ NodeId => #{ PropertyId => PropertyValue } } } ),

	post_gephi( Json, State ).




% @doc Modifies (asynchronously) the state of the specified edge, by setting its
% specified property to the specified JSON value.
%
-spec alterEdge( wooper:state(), edge_id(), property_id(), bin_json() ) ->
									const_oneway_return().
alterEdge( State, EdgeId, PropertyId, PropertyValue ) ->

	_StatusCode = alter_edge( EdgeId, PropertyId, PropertyValue, State ),

	wooper:const_return().



% @doc Modifies synchronously the state of the specified edge, by setting its
% specified property to the specified JSON value.
%
-spec alterEdgeSync( wooper:state(), edge_id(), property_id(), bin_json() ) ->
							const_request_return( 'edge_altered' ).
alterEdgeSync( State, EdgeId, PropertyId, PropertyValue ) ->

	_StatusCode = alter_edge( EdgeId, PropertyId, PropertyValue, State ),

	wooper:const_return_result( edge_altered ).



% (helper)
-spec alter_edge( edge_id(), property_id(), bin_json(), wooper:state() ) ->
											status_code().
alter_edge( EdgeId, PropertyId, PropertyValue, State ) ->

	Json = json_utils:to_json(
		#{ ce=> #{ EdgeId => #{ PropertyId => PropertyValue } } } ),

	post_gephi( Json, State ).



% @doc Synchronises this probe to the graph: ensures that all previous
% operations that are potentially still pending have been processed for good.
%
% Typically useful to close a series of asynchronous operations, in order to
% avoid any race condition.
%
-spec sync( wooper:state() ) -> const_request_return( 'graph_ready' ).
sync( State ) ->
	wooper:const_return_result( graph_ready ).



% ResultProducer-related section.


% @doc Sends the specified results to the caller (generally the result manager).
%
% At least currently, such a probe has no specific result to report.
%
% (const request, for synchronous yet concurrent operations)
%
-spec sendResults( wooper:state(), producer_options() ) ->
						request_return( producer_result() ).
sendResults( State, _Options ) ->
	ResState = setAttribute( State, result_collected, true ),
	wooper:return_state_result( ResState, { self(), no_result } ).



% Static section.


% @doc Declares (synchronously) a new graph stream probe, to be seen as a result
% producer, and to be created either from an actor or from a test case.
%
% Returns:
%
% - either the PID of this newly created result probe, if graph streaming was
% enabled in the deployment settings (refer to its 'enable_graph_streaming'
% field) and also if the name of that probe is acknowledged as a wanted
% result by the result manager
%
% - or the 'non_wanted_probe' atom
%
-spec declare_result_probe( probe_name_init() ) ->
								static_return( graph_stream_probe_ref() ).
declare_result_probe( NameInit ) ->

	ProbeName = class_Probe:get_actual_probe_name( NameInit ),

	ActualBinName = text_utils:string_to_binary( ProbeName ),

	ResultManagerPid = class_ResultManager:get_result_manager(),

	ResultManagerPid ! { isResultProducerWanted,
		[ ActualBinName, _Nature=graph_stream_probe ], self() },

	Res = receive

		{ wooper_result, { true, _Metadata } } ->

			DeployManPid = class_DeploymentManager:get_deployment_manager(),

			% In the future we could imagine using multiple, possibly per-probe,
			% project names, on a same Gephi instance (still controlled by the
			% deployment manager).
			%
			DeployManPid ! { getGraphStreamInformation, [], self() },

			% Disabled, as they are always metadata (e.g. layer versions, tick
			% duration, etc.):
			%
			%Metadata =:= [] orelse
			%   trace_utils:warning_fmt( "Ignoring, for graph stream probe "
			%       "'~ts', the following metadata: ~n ~p.",
			%       [ ProbeName, Metadata ] ),

			receive

				{ wooper_result, _GraphInfos={ GephiPossiblyHostname,
						GephiServerTCPPort, _GephiProjectPath,
						GephiProjectName } } ->

					synchronous_new_link( NameInit, GephiPossiblyHostname,
						GephiServerTCPPort, GephiProjectName );

				{ wooper_result, _MaybeGraphInfos=undefined } ->
					non_wanted_probe

			end;


		{ wooper_result, false } ->
			non_wanted_probe

	end,

	wooper:return_static( Res ).



% @doc Returns the default TCP port expected to be used by a Gephi server.
-spec get_gephi_default_tcp_port() -> static_return( tcp_port() ).
get_gephi_default_tcp_port() ->
	wooper:return_static( ?gephi_default_tcp_port ).



% @doc Returns the file extension that Gephi projects are expected to use.
-spec get_gephi_extension() -> static_return( file_utils:extension() ).
get_gephi_extension() ->
	wooper:return_static( ?gephi_project_extension ).



% @doc Runs Gephi on the local host; the Gephi graphical interface will be run,
% yet no Gephi project will be loaded, and no corresponding server will be
% started (thus no TCP port applies here).
%
% Typically called by the deployment manager whenever the support of graph
% streaming is enabled.
%
% To take care of these steps, the user may first select a project, thanks to
% the File -> 'Open' or 'Open Recent' (a *.gephi file) menu item.
%
% Then the corresponding Gephi server must be started.
%
% Currently its TCP port cannot be set programatically. It can be assigned by
% selecting first Window -> Streaming, then, on the 'Streaming' tab, by clicking
% the 'Settings' button and, regarding the 'HTTP Server Settings' panel, setting
% the 'Port' to the desired value (the gephi_default_tcp_port define is
% recommended for that).
%
% To start a Gephi server, select in the 'Streaming' tab the 'Master' -> 'Master
% Server' entry, and right-click on it to select 'Start'.
%
% The red point shall become green, and the corresponding TCP port shall be
% listened to; for example: 'ss -an | grep 8090' ('ss' superseding 'netstat')
% may now return information like: 'tcp6 0 0 :::8090 :::* LISTEN 1028/java'.
%
% Throws an exception on error.
%
-spec run_gephi() -> static_void_return().
run_gephi() ->
	run_gephi( _MaybeGephiProjectPath=undefined ),
	wooper:return_static_void().



% @doc Runs Gephi on the local host, on the specified TCP port, loading the
% specified project.
%
% Note: currently the project path and the TCP port are not taken into account,
% and the corresponding server is not started automatically.
%
% For example, a "Foobar.gephi" project path would refer to a corresponding file
% in the local directory, to be opened by the user.
%
% Refer to run_gephi/0 for further details.
%
-spec run_gephi( maybe( project_path() ) ) -> static_void_return().
run_gephi( MaybeGephiProjectPath ) ->
	run_gephi( MaybeGephiProjectPath, ?gephi_default_tcp_port ),
	wooper:return_static_void().



% @doc Runs Gephi on the local host, on the specified TCP port, loading the
% specified project.
%
% Note: currently the project path and the TCP port are not taken into account,
% and the corresponding server is not started automatically.
%
% For example, a "Foobar.gephi" project path would refer to a corresponding file
% in the local directory, to be opened by the user.
%
% Refer to run_gephi/0 for further details.
%
-spec run_gephi( maybe( project_path() ), tcp_port() ) -> static_void_return().
run_gephi( MaybeGephiProjectPath, TCPPort ) ->

	case executable_utils:get_default_graph_stream_tool_name() of

		"gephi" ->
			ok;

		Other ->
			throw( { graph_stream_tool_not_gephi, Other } )

	end,

	GephiExecPath =
			case executable_utils:get_default_graph_stream_tool_path() of

		false ->
			throw( no_gephi_executable_found );

		Exec ->
			Exec

	end,


	{ ProjOpt, MaybeProjPath } = case MaybeGephiProjectPath of

		undefined ->
			{ [], undefined };

		ProjPath ->

			AbsProjPath = file_utils:ensure_path_is_absolute( ProjPath ),

			file_utils:is_existing_file_or_link( AbsProjPath ) orelse
				begin

					trace_utils:error_fmt( "The specified Gephi project path, "
						"'~ts' (resolved as '~ts'), does not exist.",
						[ ProjPath, AbsProjPath ] ),

					throw( { gephi_project_file_not_found, ProjPath } )

				end,

			% To uncomment when ready:
			%{ [ "--load-project-file", AbsProjPath ], AbsProjPath }
			{ [], AbsProjPath }

	end,

	% To uncomment when ready:
	%PortOpt = [ "--tcp-port", text_utils:integer_to_string( TCPPort ) ],
	PortOpt = [],

	%Args = [ "--nosplash", "--autostart-server" ] ++ ProjOpt ++ PortOpt,
	Args = [ "--nosplash" ] ++ ProjOpt ++ PortOpt,

	system_utils:run_background_executable( GephiExecPath, Args ),

	% To comment when ready:
	ProjStr = case MaybeProjPath of

		undefined ->
			"project file of interest (*.gephi)";

		GephiProjPath ->
			text_utils:format( "'~ts' project file", [ GephiProjPath ] )

	end,

	% For the user, directly on the console:
	trace_utils:notice_fmt( "Supposing that Gephi is appropriately configured "
		"(it is notably expected to use, on the local host, the TCP port #~B), "
		"a relevant project must now be opened; "
		"for that, in the 'File' menu, choose 'Open' or 'Open Recent' "
		"and select the ~ts.~n"
		"Then, in the 'Streaming' tab (if necessary obtained by selecting "
		"the corresponding entry in the 'Window' menu, and providing that "
		"the 'Graph Streaming' plugin has already been installed), "
		"right-click on 'Master Server', and select 'Start'. "
		"The point on its left shall then switch from red to green.",
		[ TCPPort, ProjStr ] ),

	wooper:return_static_void().



% @doc Returns a JSON binary string aggregating the specified terms.
-spec encode_terms_to_json( [ term() ] ) -> static_return( bin_json() ).
encode_terms_to_json( Terms ) ->

	% Typically each term is [Timestamp :: iso8601_bin_string(), graph_term()]
	% 2-element list.

	TermsAsJson = [ json_utils:to_json( T ) || T <- Terms ],

	Bin = text_utils:bin_join( _Sep=$;, TermsAsJson ),

	wooper:return_static( Bin ).



% @doc Returns a JSON binary string appending to the end of the specified one
% the specified term.
%
-spec append_term_to_json( bin_json(), term() ) -> static_return( bin_json() ).
append_term_to_json( BaseBinJson, Term ) ->
	TermAsBinJson = json_utils:to_json( Term ),
	Bin = text_utils:bin_concatenate( BaseBinJson, TermAsBinJson ),
	wooper:return_static( Bin ).




% @doc Deduces the Gephi project name from the path of the specified project
% file.
%
-spec get_project_name_from_path( project_path() ) ->
											static_return( project_name() ).
get_project_name_from_path( ProjectPath ) ->

	ProjectFilename = file_utils:get_last_path_element( ProjectPath ),

	ProjName = file_utils:remove_extension( ProjectFilename,
											?gephi_project_extension ),

	wooper:return_static( ProjName ).




% Section for helper functions (not methods).


% @doc Waits for the feedback of the result manager, after this probe declared
% itself to it (after a call to its declareGraphStreamProbe/3 request).
%
-spec wait_result_declaration_outcome( probe_name(), wooper:state() ) ->
												wooper:state().
wait_result_declaration_outcome( ProbeName, State ) ->

	%trace_utils:info_fmt( "Waiting for the result declaration outcome for "
	%                      "graph probe '~ts'.", [ ProbeName ] ),

	% Finally waits the answer from the result manager to a prior
	% declareGraphStreamProbe/3 call or similar:
	%
	% (in terms of result selection, what is true now may not be true anymore
	% later, if the state of the result manager changes)
	%
	receive

		{ wooper_result, output_not_requested } ->
			?info_fmt( "The graph stream probe '~ts' would not produce "
					   "an expected result.", [ ProbeName ] ),

			setAttribute( State, enabled_producer, false );


		{ wooper_result, output_requested } ->

			% Default is enabled_producer set to true:
			?info_fmt( "The graph stream probe '~ts' will produce an expected "
					   "result.", [ ProbeName ] ),

			State

	end.



% @doc Posts the specified JSON to the target Gephi server.
-spec post_gephi( bin_json(), wooper:state() ) -> web_utils:http_status_code().
post_gephi( Json, State ) ->

	Url = ?getAttr(update_url),

	%trace_utils:debug_fmt( "Posting to '~ts' following JSON:~n ~p",
	%                       [ Url, Json ] ),

	case web_utils:post( Url, _Headers=[], _HttpOptions=[], Json ) of

		{ StatusCode, _HeaderMap, _Body } ->
			StatusCode;


		{ error, { failed_connect, [ {to_address, { _Host="localhost", Port } },
				{ inet, [ inet ], econnrefused } ] } } ->

			?error_fmt( "Unable to connect to a local Gephi server "
				"supposed to listen on TCP port #~B; none found.", [ Port ] ),

			throw( { local_gephi_connection_failed, Port } );


		{ error, { failed_connect, [ {to_address, { Host, Port } },
				{ inet, [ inet ], econnrefused } ] } } ->

			?error_fmt( "Unable to connect to host ~ts on TCP port #~B: "
				"no Gephi server seems to be listening.", [ Host, Port ] ),

			throw( { gephi_connection_failed, Host, Port } );


		{ error, Reason } ->
			throw( { gephi_post_failed, Reason } )

	end.
