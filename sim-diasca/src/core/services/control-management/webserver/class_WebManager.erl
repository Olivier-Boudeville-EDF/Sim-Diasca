% Copyright (C) 2019-2026 EDF R&D
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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: Friday, June 7, 2019.

-module(class_WebManager).

-moduledoc """
Overall (singleton) manager of **web-based interactions**, notably in terms of
result producers (probes), possibly based on client-side, otherwise server-side
(Node.js) Javascript, and to be perused by the user thanks to a web browser.

This manager is meant to run on the user node, as for example it may have to
launch a web browser.
""".


-define( class_description, "Overall (singleton) manager of web-based
         interactions, notably in terms of result producers (probes), possibly
         based on client-side, otherwise server-side (Node.js) Javascript, and
         to be perused by the user thanks to a web browser.
         This manager is meant to run on the user node, as for example it
         may have to launch a web browser."  ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).



-type manager_pid() :: sim_diasca:agent_pid().

-type probe_info() :: class_ResultManager:probe_info().


-doc "Table storing the known (web) probes.".
-type probe_table() :: table( probe_pid(), probe_info() ).


-export_type([ manager_pid/0, probe_table/0 ]).

% Silencing:
-export_type([ support_table/0 ]).


-export([ check_no_pending_webserver/2, start_webserver/3, stop_webserver/1 ]).


-include_lib("wooper/include/wooper.hrl").


% For its registering name:
-include("class_WebManager.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Control.Web" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% The default TCP port at which any corresponding webserver is to run:
-define( default_webserver_tcp_port, 8080 ).


-doc """
Table allowing to associate class-level information about support facilities.

For example, a given type (i.e. class) of web probe might need one-time
operations to enable its support (e.g. a relevant CSS shall be copied once for
all in a target content directory). This operation shall be done exactly once,
iff at least one instance of such probe is needed (hence either not at all
should no instance of it be created, or once, at the creation of the first
instance).

So the boolean associated to the classname tells whether the corresponding
support has already been initialised (or terminated since then).
""".
-type support_table() :: table( wooper:classname(), boolean() ).



% The class-specific attributes of an instance of a web manager are:
-define( class_attributes, [

    %{ webserver_install_root, option( bin_directory_path() ),
    %  "the root directory of any webserver runtime install" },

    % The overall web root to be served by our webserver.
    %
    % At least currently, this corresponds to the base case-specific directory
    % (the one containing 'simulation-results'), as the simulation results have
    % to be referenced by the result web pages, so the web content root must
    % encompass both of them (otherwise they will not be taken into account by
    % our integrated webserver, which does not allow escaping from its web
    % root). So at least index.html must sit directly at the web root and, as it
    % is better that other web elements are gathered separately, a
    % "web-elements" direct subdirectory of the web root is defined and used.
    %
    % So the web root corresponds to the root of the simulation case tree
    % (e.g. "My_Case-on-2026-6-25-at-11h-09m-14s-by-xxx-40243151"), whose (main)
    % subdirectories are "simulation-results" and "web-elements".
    %
    { webserver_content_root, bin_directory_path(),
      "the root directory of the web content to be served (i.e. the web "
      "root)" },

    { webserver_id, option( server_id() ),
      "the identifier of any Myriad webserver launched" },

    % Not used anymore now that we rely on our own webserver:
    %{ local_access_required, boolean(),
    %  "tells whether at least one web probe requires that extra, local content"
    %  " (e.g. Javascript scripts) can be loaded by the browser from "
    %  "the local filesystem" },

    { result_manager_pid, class_ResultManager:manager_pid(),
      "PID of the result manager" },

    { result_dir, option( bin_directory_path() ),
      "the directory (if any) in which the (web) results to aggregate will be "
      "found" },

    { tcp_port, tcp_port(),
      "the TCP port at which the local webserver is to run" },

    { support_table, support_table(), "a table telling whether the support "
      "brought by a given classname has already been initialised (currently, "
      "if a classname entry exists, it is necessarily associated to 'true',
      until it has been terminated)" },

    { probe_table, probe_table(), "known web probes, associating to the PID of "
      "a probe its name and possibly base directory" },

    { interactivity_mode, interactivity_mode(),
      "tells whether we run in batch or interactive mode" },

    { sii, sim_diasca:sii(), "the identifier of the current simulation" },

    { engine_root_dir, bin_directory_path(),
      "the root directory of the engine" } ] ).



% Scope at which this web manager should register:
-define( registration_scope, global_only ).


-define( web_elements_dir, "web-elements" ).

-define( css_filename, "sim-diasca.css" ).
-define( logo_filename, "sim-diasca-on-transparent-background.png" ).




% Implementation notes:
%
% Some probes are web-based, i.e. the generate HTML content; most of them will
% rely, among other elements, on client-side Javascript components. Some others
% may rely on server-side components, and as such rely on a suitable local
% webserver (for the aforementioned probes, possibly any kind of webserver,
% possibly hosting multiple instances of such probes), running on a (generally
% unprivileged) TCP port.
%
% Of course all these prerequisites have to be available beforehand, for the web
% support to be available (at least some basic checks are performed when
% creating such a manager).
%
% As a result, by default, any webserver-based interaction with the engine is
% typically to be done at 'http://localhost:8080/'. Of course a browser (such as
% Firefox) will be needed.
%
% For at least most web probes, as Javascript lies on the client only, no
% specific server-side support is needed, and not even a webserver was
% required.
%
% Previously the web content was directly displayed locally, without involving
% any webserver, just thanks to any browser, on the user host.
%
% However most if not all recent browsers (e.g. Firefox since the version 68)
% will block CORS requests about resources like remote *.js files (see
% https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS/Errors/CORSRequestNotHttp).
%
% For example a now insufficient solution was, in the "about:config" tag, to set
% the "privacy.file_unique_origin" key to false
% (cf. https://support.mozilla.org/bm/questions/1264312).
%
% So we reinstated the former webserver-based approach, except that we switched
% for the best from a Node.js webserver to the Myriad minimalist one.

% Note also that all remote resources (e.g. see planner-header.html) have been
% cached locally, so that a simulation can be run with no Internet access at
% all.




% Type shorthands:

-type ustring() :: text_utils:ustring().

-type directory_path() :: file_utils:directory_path().
-type file_path() :: file_utils:file_path().
-type bin_directory_path() :: file_utils:bin_directory_path().
%-type bin_file_path() :: file_utils:bin_file_path().
-type file() :: file_utils:file().

-type command() :: system_utils:command().

-type human_language() :: language_utils:human_language().

-type tcp_port() :: net_utils:tcp_port().

-type server_id() :: web_utils:server_id().

-type interactivity_mode() :: class_TimeManager:simulation_interactivity_mode().

-type bin_probe_name() :: class_ResultProducer:bin_producer_name().

-type probe_pid() :: class_WebProbe:probe_pid().
-type facility_probe_pid() :: class_WebProbe:facility_probe_pid().




-doc """
Creates the web manager.

Construction parameters are:

- SII is the identifier of this simulation run

- EngineRootDir is the root directory of the engine

- InteractivityMode tells whether we run in batch or interactive mode

- WebRoot designates the root directory of the web content to serve; it belongs
  to the directory created on the user host for the current simulation case
  (which is expected to exist already); often, to enable all elements (including
  web ones, and results), the web root is directly that directory (not a
  subdirectory thereof)

- MaybeTCPPort is, if defined, the TCP port at which the local webserver is to
run
""".
-spec construct( wooper:state(), sim_diasca:sii(), bin_directory_path(),
    interactivity_mode(), directory_path(), class_ResultManager:manager_pid(),
    option( tcp_port() ) ) -> wooper:state().
construct( State, SII, EngineRootDir, InteractivityMode, WebRoot,
           ResultManagerPid, MaybeTCPPort ) ->
    construct( State, SII, EngineRootDir, InteractivityMode, WebRoot,
               ResultManagerPid, _ServerInstallRoot=undefined, MaybeTCPPort ).



-doc """
Creates the web manager.

Construction parameters are:

- SII is the identifier of this simulation run

- EngineRootDir is the root directory of the engine

- InteractivityMode tells whether we run in batch or interactive mode

- WebRoot designates the root directory of the web content to serve; it belongs
  to the directory created on the user host for the current simulation case
  (which is expected to exist already); often, to enable all elements (including
  web ones, and results), the web root is directly that directory (not a
  subdirectory thereof)

- MaybeServerInstallRoot designates, if defined, the root directory of the
webserver installation

- MaybeTCPPort is, if defined, the TCP port at which the local webserver is to
run
""".
-spec construct( wooper:state(), sim_diasca:sii(), bin_directory_path(),
    interactivity_mode(), directory_path(), class_ResultManager:manager_pid(),
    option( directory_path() ), option( tcp_port() ) ) -> wooper:state().
construct( State, SII, EngineRootDir, InteractivityMode, WebRoot,
           ResultManagerPid, MaybeServerInstallRoot, MaybeTCPPort ) ->

    % Oneway should be safe here:
    ResultManagerPid ! { addResultListener, self() },

    % First the direct mother classes:
    BaseState = class_EngineBaseObject:construct( State,
        ?trace_categorize("WebManager") ),

    %{ NewMaybeBinServerInstallRoot, NewBinWebRoot, TCPPort } =
    { NewBinWebRoot, TCPPort } = check_settings( WebRoot,
        MaybeServerInstallRoot, MaybeTCPPort, BaseState ),

    % Start this potentially longer check early:
    check_no_pending_webserver( TCPPort, BaseState ),

    RegistrationName = get_registration_name(),

    % Web probes rely on it:
    naming_utils:register_as( RegistrationName, ?registration_scope ),

    class_InstanceTracker:register_agent( RegistrationName ),

    EmptyTable = table:new(),

    CreatedState = setAttributes( BaseState, [
        %{ webserver_install_root, NewMaybeBinServerInstallRoot },
        { webserver_content_root, NewBinWebRoot },
        { webserver_id, undefined },
        %{ local_access_required, false },
        { result_manager_pid, ResultManagerPid },
        { result_dir, undefined },
        { tcp_port, TCPPort },
        { support_table, EmptyTable },
        { probe_table, EmptyTable },
        { interactivity_mode, InteractivityMode },
        { sii, SII },
        { engine_root_dir, text_utils:string_to_binary( EngineRootDir ) } ] ),

    % Webserver reinstated again, as CORS directives prevent now browsers to
    % perform most if not all local accesses:

    %_CfgFilename = generate_webserver_configuration_file( SII,
    %    NewServerInstallRoot, NewWebRoot, TCPPort, CreatedState ),

    %start_webserver( NewServerInstallRoot, NewWebRoot,
    %                 CfgFilename, CreatedState ),

    WebState = start_webserver( TCPPort, NewBinWebRoot,
                                CreatedState ),

    ?send_debug_fmt( CreatedState, "Created ~ts",
                     [ to_string( WebState ) ] ),

    WebState.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

    % Class-specific actions:
    ?info( "Deleting web manager." ),

    ?getAttr(result_manager_pid) ! { removeResultListener, self() },

    %stop_webserver( State ),

    % Web probes not owned, hence not deallocated here.

    State.




% Methods section.



-doc "Declares a new web probe.".
-spec declareWebProbe( wooper:state(), bin_probe_name(), wooper:classname(),
                       %option( bin_directory_path() ), boolean() ) ->
                       option( bin_directory_path() ) ) ->
        request_return( 'web_probe_declared' ).
declareWebProbe( State, BinProbeName, WebProbeClassname, MaybeBinProbeDir ) ->

    ?debug_fmt( "Declaring web probe '~ts' of class '~ts'.",
                [ BinProbeName, WebProbeClassname ] ),

    ProbeTable = ?getAttr(probe_table),

    ProbePid = ?getSender(),

    NewProbeTable = case table:lookup_entry( ProbePid, ProbeTable ) of

        { value, { BinClashingName, _Dir } } ->
            throw( { web_probe_already_registered, ProbePid,
                        { BinClashingName, BinProbeName } } );

        key_not_found ->
            table:add_entry( _K=ProbePid, _V={ BinProbeName, MaybeBinProbeDir },
                             ProbeTable )

    end,

    %WasLocalAccessRequired = ?getAttr(local_access_required),
    %
    % Early check, if needed and not done already:
    % IsLocalAccessRequired andalso not WasLocalAccessRequired andalso
    %     %case executable_utils:get_any_web_browser_info_for_local_access() of
    %     case executable_utils:get_any_web_browser_info() of
    %
    %         { ok, { BrowserFamily, BrowserPath, _Opts } } ->
    %             ?debug_fmt( "A web probe requested local filesystem "
    %                 "access, it will be obtained with a ~ts browser "
    %                 "(whose path is '~ts').",
    %                 [ BrowserFamily, BrowserPath ] );
    %
    %         error ->
    %             ?error_fmt( "The '~ts' web probe requested a local filesystem"
    %                 " access, but no suitable browser could be found.",
    %                 [ BinProbeName ] ),
    %             throw( { no_suitable_web_browser,
    %                      no_local_filesystem_access } )
    %
    %    end,
    %
    %FinalLocalAccessRequired = WasLocalAccessRequired or IsLocalAccessRequired,

    %DeclaredState = setAttributes( State, [
    %    { probe_table, NewProbeTable },
    %    { local_access_required, FinalLocalAccessRequired } ] ),

    DeclaredState = setAttribute( State, probe_table, NewProbeTable ),

    % We should not return to the probe the engine root dir or the webserver
    % installation root, as the probe may be running on a different host.

    SupportTable = ?getAttr(support_table),


    %trace_utils:debug_fmt(
    %    "Looking up the ~ts:initialise_support/2 static method.",
    %    [ WebProbeClassname ] ),

    SupportState = case table:lookup_entry( WebProbeClassname, SupportTable ) of

        % Not initialised yet, maybe a class-specific initialisation callback
        % has been defined:
        %
        key_not_found ->

            Mod = WebProbeClassname,
            Fun = initialise_support,

            SupportValue = case meta_utils:is_function_exported( Mod, Fun,
                                                                 _Arity=2 ) of

                true ->
                    Mod:Fun( ?getAttr(engine_root_dir),
                             ?getAttr(webserver_content_root) );

                false ->
                    ?debug_fmt( "(no ~ts:initialise_support/2 static method "
                                "available)", [ WebProbeClassname ] ),
                    true

            end,

            NewSupportTable = table:add_entry( WebProbeClassname,
                                               SupportValue, SupportTable ),

            setAttribute( DeclaredState, support_table, NewSupportTable );


        % Already initialised, nothing more to do:
        { value, _IsInit=true } ->
            DeclaredState

    end,

    wooper:return_state_result( SupportState, web_probe_declared ).



-doc """
Callback triggered by the result manager, as the web manager is a listener
thereof.
""".
-spec results_collected( wooper:state(), bin_directory_path() ) ->
                                oneway_return().
results_collected( State, ResultBaseDirName ) ->

    % The web manager already knows the web-related information, but other probe
    % information (including basic probes and metadata) are also of interest
    % here:
    %
    ?getAttr(result_manager_pid) ! { getBaseProbeInfos, [], self() },

    ?debug_fmt( "Notified that results were collected, in '~ts'.",
                [ ResultBaseDirName ] ),

    WebProbePairs = table:values( ?getAttr(probe_table) ),

    % Answer from the getBaseProbeInfos/1 request:
    { LandingFilename, GenState } = receive

        % Virtual probes currently not specifically managed here:
        { wooper_result, { BasicProbePairs, _VirtualProbePairs, Metadata } } ->
            generate_landing_page( BasicProbePairs, WebProbePairs, Metadata,
                                   State )

    end,

    case ?getAttr(interactivity_mode) of

        batch ->
            ?debug( "In batch mode, hence no browser launched." );

        interactive ->

            Cmd = get_browser_command_for( LandingFilename, State ),

            ?debug_fmt( "In interactive mode, thus launching a browser pointing"
                " to the landing page (with command: '~ts').", [ Cmd ] ),

            trace_utils:notice(
                "The web results are displayed in a browser tab." ),

            system_utils:run_background_command( Cmd )

    end,

    wooper:return_state( GenState ).



-doc "Returns a shell command suitable to browse the specified landing page.".
-spec get_browser_command_for( file_path(), wooper:state() ) -> command().
get_browser_command_for( LandingFilePath, State ) ->

    %LocalAccessRequired = ?getAttr(local_access_required),

    % FallibleBrowserInfo = case LocalAccessRequired of

    %     true ->
    %         executable_utils:get_any_web_browser_info_for_local_access();

    %     false ->
    %         executable_utils:get_any_web_browser_info()

    % end,

    FallibleBrowserInfo = executable_utils:get_any_web_browser_info(),

    { _BrowserFamily, BrowserPath, Opts } = case FallibleBrowserInfo of

        error ->
            %throw( { no_suitable_browser, LocalAccessRequired } );
            throw( no_suitable_browser );

        { ok, BrowserInfo } ->
            BrowserInfo

    end,

    OptStr = text_utils:join( _Sep=" ", Opts ),

   % Redirections at least useful with chromium to avoid uninteresting
   % warnings/messages:
   %
   %text_utils:format( "~ts ~ts file://~ts 1>/dev/null 2>&1",
   text_utils:format( "~ts ~ts http://localhost:~B/~ts 1>/dev/null 2>&1",
       [ BrowserPath, OptStr, ?getAttr(tcp_port), LandingFilePath ] ).



% Static methods.


-doc """
Initialises, synchronously (typically from the simulation case) and on the
current node, the web management service, using the specified SII and
directories, respectively as the root directory of the engine, as webserver
content root and as webserver installation root, and the specified TCP port.
""".
-spec create_manager( sim_diasca:sii(), bin_directory_path(),
    interactivity_mode(), bin_directory_path(),
    class_ResultManager:manager_pid(), option( bin_directory_path() ),
    option( tcp_port() ) ) -> static_return( manager_pid() ).
create_manager( SII, EngineRootDir, InteractivityMode, WebserverContentRoot,
                ResultManagerPid, MaybeWebserverInstallRoot, MaybeTCPPort ) ->
    wooper:return_static( synchronous_timed_new_link( SII, EngineRootDir,
        InteractivityMode, WebserverContentRoot, ResultManagerPid,
        MaybeWebserverInstallRoot, MaybeTCPPort ) ).



-doc """
Returns the atom corresponding to the name the web manager should be registered
as.
""".
-spec get_registration_name() -> static_return( net_utils:atom_node_name() ).
get_registration_name() ->
    wooper:return_static( ?web_manager_name ).




-doc """
Returns the web root of interest, based on the specified root of the simulation
run.
""".
-spec get_web_content_root( directory_path() ) ->
                                    static_return( directory_path() ).
get_web_content_root( BaseRunDir ) ->

    % Directly here at the root of the case-specific tree (e.g. not in any
    % 'web-content' subdirectory), as the webserver requires a full visibility
    % onto simulation results and web elements (see the comments on the
    % webserver_content_root attribute for more details):
    %
    wooper:return_static( BaseRunDir ).



-doc """
Returns, based on the specified web root (see `get_web_content_root/1`), the
directory into which most web elements (except `index.html`) are to be found.
""".
-spec get_web_elements_root( directory_path() ) ->
                                    static_return( directory_path() ).
get_web_elements_root( WebRoot ) ->

    WebElemsRoot = file_utils:join( WebRoot, ?web_elements_dir ),

    wooper:return_static( WebElemsRoot ).




% Helper functions.


-doc """
Checks the settings assigned to this manager (and performs some side effects
like the creation of directories).
""".
-spec check_settings( directory_path(), option( directory_path() ),
                      option( tcp_port() ), wooper:state() ) ->
        { bin_directory_path(), tcp_port() }.
check_settings( WebRoot, _MaybeServerInstallRoot, MaybeTCPPort,
                State ) ->

    ServerTCPPort = case MaybeTCPPort of

        undefined ->
            TCPPort = ?default_webserver_tcp_port,
            ?notice_fmt( "No user-defined TCP port for webserver specified, "
                         "using the default one, #~B.", [ TCPPort ] ),
            TCPPort;

        TCPPort when is_integer( TCPPort ) andalso TCPPort > 0 ->
            ?notice_fmt( "User-defined TCP port for webserver is #~B.",
                         [ TCPPort ] ),
            TCPPort;

        InvalidTCPPort ->
            ?error_fmt( "Invalid TCP port specified: '~p'.",
                        [ InvalidTCPPort ] ),
            throw( { invalid_tcp_port, InvalidTCPPort } )

    end,

    % A Node.js or any other webserver (even purely local) is not used anymore:
    %ServerInstallRoot = case MaybeServerInstallRoot of
    %
    %   undefined ->
    %       InstDir = get_default_webserver_installation_root(),
    %       ?notice_fmt( "No user-defined webserver root directory specified, "
    %                    "using the default one, '~ts'.", [ InstDir ] ),
    %       InstDir;
    %
    %   InstRoot ->
    %       ?notice_fmt( "User-specified webserver root specified, "
    %                    "using the default one, '~ts'.", [ InstRoot ] ),
    %       file_utils:ensure_path_is_absolute( InstRoot )
    %
    %end,

    %CheckedServerInstallRoot = case file_utils:is_existing_directory_or_link(
    %       ServerInstallRoot ) of
    %
    %   true ->
    %       text_utils:string_to_binary( ServerInstallRoot );
    %
    %   false ->
    %       ?error_fmt( "The specified root directory of the webserver "
    %               "install, '~ts', does not exist.", [ ServerInstallRoot ] ),
    %       throw( { non_existing_webserver_install_root, ServerInstallRoot } )
    %
    %end,
    %CheckedServerInstallRoot = undefined,

    CanonWebRoot =
        file_utils:ensure_path_is_absolute( WebRoot ),

    %% CheckedWebRoot = case
    %%         file_utils:is_existing_directory_or_link(
    %%             CanonWebRoot ) of

    %%     true ->
    %%         ?error_fmt( "The root directory of the webserver content, "
    %%             "'~ts', already exists (transformed from ~ts).",
    %%             [ CanonWebRoot, WebRoot ] ),

    %%         throw( { already_existing_webserver_content_root,
    %%                  CanonWebRoot } );

    %%     false ->
    %%         % Will not create any lacking parent directory:
    %%         file_utils:create_directory( CanonWebRoot ),
    %%         text_utils:string_to_binary( CanonWebRoot )

    %% end,

    CheckedWebRoot = case file_utils:is_existing_directory_or_link(
                            CanonWebRoot ) of

        true ->
            text_utils:string_to_binary( CanonWebRoot );

        false ->
            ?error_fmt( "The root directory of the webserver content, "
                "'~ts', doest not exist already exist (transformed from ~ts).",
                [ CanonWebRoot, WebRoot ] ),

            throw( { non_existing_webserver_content_root,
                     CanonWebRoot } )

    end,

    %{ CheckedServerInstallRoot, CheckedWebRoot, ServerTCPPort }.
    { CheckedWebRoot, ServerTCPPort }.



-doc """
Checks that no pending local (web)server is lingering at the TCP port we target,
to ensure that a next launch is possible.
""".
-spec check_no_pending_webserver( tcp_port(), wooper:state() ) -> void().
check_no_pending_webserver( TCPPort, State ) ->

    case net_utils:is_local_service_running_at( TCPPort ) of

        true ->
            % Not a blocking error anymore, as reuseaddr is used now when
            % starting our webservers, so that this instance will attempt any
            % reuse if needed:

            %?error_fmt( "A service (presumably a webserver) is already "
            %    "running on local TCP port #~B, it shall be "
            %    "stopped first.", [ TCPPort ] ),

            %throw( { server_already_running_at_port, TCPPort } );

            ?warning_fmt( "A service (presumably a webserver) is already "
                "running on local TCP port #~B apparently; "
                "trying to re-use this port nevertheless.", [ TCPPort ] );


        false ->
            ?debug_fmt( "No local server found at port #~B.", [ TCPPort ] )

    end.



% -doc "Returns the default root directory of the webserver installation.".
% -spec get_default_webserver_installation_root() -> directory_path().
% get_default_webserver_installation_root() ->

%     % Possibly a symlink:
%     file_utils:join( [ system_utils:get_user_home_directory(), "Software",
%                        "sim_diasca_webserver_install_root" ] ).



% -doc """
% Generates a new, suitable Node.js configuration file.

% As mentioned, depending on the probes, running such a node may or may not be
% necessary.

% Now relying on the Myriad minimalist webserver.
% """.
% -spec generate_webserver_configuration_file( sim_diasca:sii(),
%         bin_directory_path(), bin_directory_path(),
%         tcp_port(), wooper:state() ) -> bin_file_path().
% generate_webserver_configuration_file( SII, BinServerInstallRoot,
%                                    BinWebRoot, TCPPort, State ) ->

%     TargetFilename = text_utils:format( "sim-diasca-~ts-server.js", [ SII ] ),

%     TargetFilePath = file_utils:join( BinServerInstallRoot, TargetFilename ),

%     % Made not to fail, knowing a clash in filenames is very unlikely:
%     file_utils:is_existing_file_or_link( TargetFilePath ) andalso
%         begin
%             BackupName = text_utils:format( "~ts-~ts", [ TargetFilePath,
%                 time_utils:get_textual_timestamp_for_path() ] ),

%             ?warning_fmt( "A Node.js configuration file has been found "
%                 "already existing, '~ts'; moving it out of the way by "
%                 "renaming it to '~ts'.", [ TargetFilePath, BackupName ] ),

%             file_utils:rename( TargetFilePath, BackupName )
%         end,

%     % Now not existing by design:
%     NewCfgFile = file_utils:open( TargetFilePath, [ write, raw ] ),

%     file_utils:write_ustring( NewCfgFile,
%         "// Generated by Sim-Diasca on ~ts.~n"
%         "const express = require('express');~n"
%         "const app = new express();~n"
%         "app.use(express.static('~ts'));~n"
%         "app.listen(~B);~n"
%         "// End of generated file.~n",
%         [ time_utils:get_textual_timestamp(), BinWebRoot,
%           TCPPort ] ),

%     file_utils:close( NewCfgFile ),

%     ?notice_fmt( "New configuration file '~ts' written.",
%                  [ TargetFilePath ] ),

%     TargetFilePath.



-doc """
Generates a landing page (`index.html`) at the root of the specified web content
directory, and returns its path.

Any pre-existing version of that page will be removed first.
""".
-spec generate_landing_page( [ probe_info() ], [ probe_info() ],
    class_ResultManager:meta_data(), wooper:state() ) -> file_path().
generate_landing_page( BasicProbeInfos, WebProbeInfos, Metadata, State ) ->

    BinWebRoot = ?getAttr(webserver_content_root),

    ?debug_fmt( "Generating, in webserver content root '~ts', "
        "the landing page for:~n  - basic probes: ~p~n"
        "  - web probes: ~p~n(metadata: ~p)",
        [ BinWebRoot, BasicProbeInfos, WebProbeInfos, Metadata ] ),


    CommonDir = file_utils:join( [ ?getAttr(engine_root_dir), "sim-diasca",
                                   "doc", "common-elements" ] ),

    CssFilePath = file_utils:join( [ CommonDir, "css", ?css_filename ] ),


    WebElemsDir = get_web_elements_root( BinWebRoot ),

    file_utils:create_directory_if_not_existing( WebElemsDir ),

    file_utils:copy_file_in( CssFilePath, WebElemsDir ),

    LogoFilePath =
        file_utils:join( [ CommonDir, "edf-related", ?logo_filename ] ),

    file_utils:copy_file_in( LogoFilePath, WebElemsDir ),

    LandingFilename = "index.html",

    LandingFilePath = file_utils:join( BinWebRoot, LandingFilename ),

    file_utils:is_existing_file_or_link( LandingFilePath ) andalso
        begin

            % Quite surprising:
            ?warning_fmt( "Removing a former version of '~ts'.",
                          [ LandingFilePath ] ),

            file_utils:remove_file( LandingFilePath )

        end,

    ?debug_fmt( "Generating following landing page: '~ts'.",
                [ LandingFilePath ] ),

    % Do not *ever* add 'raw' here, otherwise encoding might be screwed up:
    PageFile = file_utils:open( LandingFilePath, [ write ] ),

    Lang = language_utils:get_user_language(),

    write_header( ?getAttr(sii), Metadata, Lang, PageFile ),

    HasBasicProbe = BasicProbeInfos =/= [],

    BrowsableWebProbeInfos =
        select_browsable_probes( BinWebRoot, WebProbeInfos ),

    %trace_utils:debug_fmt( "For WPIs ~p and root ~ts, selected:~n ~p",
    %    [ WebProbeInfos, BinWebRoot, BrowsableWebProbeInfos ] ),

    HasBrowsableWebProbe = BrowsableWebProbeInfos =/= [],

    case { HasBasicProbe, HasBrowsableWebProbe } of

        { true, true } ->
            write_toc( BasicProbeInfos, BrowsableWebProbeInfos, PageFile ),

            % Basic probes better listed last (generally gather final results):
            write_web_probes( BrowsableWebProbeInfos, BinWebRoot, Lang,
                              PageFile ),

            write_basic_probes( BasicProbeInfos, BinWebRoot, Lang, PageFile );

        { true, false } ->
            write_basic_probes( BasicProbeInfos, BinWebRoot, Lang, PageFile );

        { false, true } ->
            write_web_probes( BrowsableWebProbeInfos, BinWebRoot, Lang,
                              PageFile );

        { false, false } ->
            write_no_probe( Lang, PageFile )

    end,

    write_footer( PageFile ),

    file_utils:close( PageFile ),

    TermState = manage_support_termination( State ),

    { LandingFilename, TermState }.




-doc """
Selects only the probes whose directory is in the specified web content root.
As for example the facility web probes are by design out of the result web root.
""".
select_browsable_probes( BinWebRoot, WebProbeInfos ) ->
    % 'undefined' is accepted, as kept implicitly in the content root:
    [ WPI || WPI={ _BinProbeName, MaybeBinDirPath } <- WebProbeInfos,
             MaybeBinDirPath =:= undefined orelse file_utils:is_relative(
                 _Path=MaybeBinDirPath, _RefDir=BinWebRoot ) ].



-doc """
Manages the termination of every registered, initialised support class, by
calling any appropriate class-level callbacks.
""".
-spec manage_support_termination( wooper:state() ) -> wooper:state().
manage_support_termination( State ) ->

    SupportTable = ?getAttr(support_table),

    SupportClassnamesToTerminate = [ Classname
        || { Classname, _IsInit=true } <- table:enumerate( SupportTable ) ],

    %trace_utils:debug_fmt( "Support classes to terminate: ~p",
    %                       [ SupportClassnamesToTerminate ] ),

    Fun = terminate_support,

    [ meta_utils:is_function_exported( Classname, Fun, _Arity=2 )
        andalso
            begin

                true = Classname:Fun( ?getAttr(engine_root_dir),
                    ?getAttr(webserver_content_root) )

            end || Classname <- SupportClassnamesToTerminate ],


    NewSupportTable = table:add_entries( [ { Classname, _IsInit=false }
        || Classname <- SupportClassnamesToTerminate ], SupportTable ),

    setAttribute( State, support_table, NewSupportTable ).



-doc "Writes the header of the landing page.".
-spec write_header( sim_diasca:sii(), class_ResultManager:meta_data(),
                    human_language(), file() ) -> void().
write_header( SII, Metadata, Lang, File ) ->

    { SimFmtStr, RunFmtStr, TimeStr, HostFmtStr, TickFmtStr, HeaderFmtStr } =
            case Lang of

        french ->
            { "nom du cas de simulation : <b>~ts</b>",
              "identifiant de l'exécution : <code>~ts</code>",
              text_utils:format( "horodatage d'obtention des résultats : ~ts",
                [ time_utils:get_french_textual_timestamp() ] ),
              "hôte de lancement : <code>~ts</code>",
              "~ts", %"pas de temps : ~ts",
              get_header_format_string( french ) };

        % 'english' being the default:
        _ ->
            { "name of the simulation case: <b>~ts</b>",
              "run ID: <code>~ts</code>",
              text_utils:format( "collection timestamp: ~ts",
                                 [ time_utils:get_textual_timestamp() ] ),
              "user host: <code>~ts</code>",
              "~ts", %"tick: ~ts",
              get_header_format_string( english ) }

    end,


    SimStr = text_utils:format( SimFmtStr,
        [ list_table:get_value( simulation_name, Metadata ) ] ),

    RunStr = text_utils:format( RunFmtStr, [ SII ] ),


    HostStr = text_utils:format( HostFmtStr, [ net_utils:localhost() ] ),

    TickStr = text_utils:format( TickFmtStr,
        [ list_table:get_value( tick_duration, Metadata ) ] ),

    % Layer versions skipped:
    MetadataStr = web_utils:get_unordered_list(
        [ SimStr, RunStr, TimeStr, HostStr, TickStr ] ),

    HeaderStr = text_utils:format( HeaderFmtStr,
                                   [ SII, SII, MetadataStr ] ),

    file_utils:write_ustring( File, HeaderStr ).



get_header_format_string( _Lang=french ) ->
      "<!DOCTYPE html>~n"
      "<html>~n"
      "  <head>~n"
      "    <title>Résultats de simulation pour l'exécution ~ts</title>~n"
      "    <meta charset=\"UTF-8\">~n"
      "    <meta name=\"description\" content=\"Sim-Diasca Web Results\">~n"
      "    <meta name=\"keywords\" content=\"HTML,CSS,XML,JavaScript\">~n"
      "    <meta name=\"author\" content=\"Sim-Diasca\">~n"
      "    <link rel=\"stylesheet\" href=\""?web_elements_dir"/"?css_filename"\">~n"
      "    <meta name=\"viewport\" content=\"width=device-width,"
      "initial-scale=1.0\">~n"
      "  </head>~n"
      "  <body>~n"
      "    <h1>Bienvenue sur l'interface de navigation des résultats "
      % Already displayed as a logo at the bottom:
      %"de Sim-Diasca pour la simulation <code>#~ts</code></h1>~n"
      "pour la simulation <code>#~ts</code></h1>~n"
      "<blockquote><p><b>Information sur la simulation</b>:~n~ts</p>"
      "</blockquote>~n";


% Default being English:
get_header_format_string( _Lang ) ->

      "<!DOCTYPE html>~n"
      "<html>~n"
      "  <head>~n"
      "    <title>Simulation Results for run ~ts</title>~n"
      "    <meta charset=\"UTF-8\">~n"
      "    <meta name=\"description\" content=\"Sim-Diasca Web Results\">~n"
      "    <meta name=\"keywords\" content=\"HTML,CSS,XML,JavaScript\">~n"
      "    <meta name=\"author\" content=\"Sim-Diasca\">~n"
      "    <link rel=\"stylesheet\" href=\""?web_elements_dir"/"?css_filename"\">~n"
      "    <meta name=\"viewport\" content=\"width=device-width,"
      "initial-scale=1.0\">~n"
      "  </head>~n"
      "  <body>~n"
      % Shorter is better:
      %"    <h1>Welcome to the Sim-Diasca web result browser "
      "    <h1>Welcome to the result browser "
      "for simulation run <code>#~ts</code></h1>~n"
      "<blockquote><p><b>Simulation information</b>:~n~ts</p></blockquote>~n".



-doc "Writes a suitable mini-table of contents.".
-spec write_toc( [ bin_probe_name() ], [ bin_probe_name() ], file() ) -> void().
write_toc( _BasicProbeNames, _WebProbeNames, _File ) ->
    ok.


-doc """
Writes in the specified file the content relative to the specified web probes.
""".
-spec write_web_probes( [ probe_info() ], bin_directory_path(),
                        human_language(), file() ) -> void().
% Single (web) probe here:
write_web_probes( [ WebProbeInfo ], BinWebRoot, Lang, File ) ->

    MsgStr = case Lang of

        french ->
            "Une seule sonde web activée ";

        _ ->
            "A single web probe enabled"

    end,

    file_utils:write_ustring( File, "<a name=\"web_probes\"></a>~n"
        "<p>~ts: ~ts</p>~n",
        [ MsgStr, get_probe_link( WebProbeInfo, BinWebRoot ) ] );

% Multiple (web) probes here:
write_web_probes( WebProbeInfos, BinWebRoot, Lang, File ) ->

    MsgStr = case Lang of

        french ->
            "sondes web activées ";

        _ ->
            "web probes enabled"

    end,

    ProbeLinks =
        [ get_probe_link( I, BinWebRoot ) || I <- WebProbeInfos ],

    file_utils:write_ustring( File, "<a name=\"web_probes\"></a>~n"
        "<p>~B ~ts:~n~ts</p>~n",
        [ length( WebProbeInfos ), MsgStr,
          web_utils:get_unordered_list( ProbeLinks ) ] ).



-doc """
Returns an HTML link (if possible) for the specified probe.
""".
% Must be a tracked (non-facility) *web* probe, whose result location is a
% priori known:
%
get_probe_link( { BinProbeName, _MaybeBinProbeDir=undefined },
                _BinWebRoot ) ->
    % Directly in simulation-results:
    text_utils:format( "<a href=\"~ts\">~ts</a>", [
        file_utils:join( [ "simulation-results",
            class_WebProbe:get_generated_html_filename( BinProbeName ) ] ),
        BinProbeName ] );


% Any directory of its own, relative to the web root (facility probes cannot be
% managed here, as they are not even in the result web rooot):
%
get_probe_link( { BinProbeName, BinProbeDir }, _BinWebRoot ) ->
    text_utils:format( "<a href=\"~ts\">~ts</a>", [
        file_utils:join( [ "..", BinProbeDir,
            class_WebProbe:get_generated_html_filename( BinProbeName ) ] ),
        BinProbeName ] ).



-doc """
Writes in the specified file the content relative to the specified web probes.
""".
-spec write_basic_probes( [ probe_info() ], bin_directory_path(),
                          human_language(), file() ) -> void().
% Single (basic) probe here:
write_basic_probes( [ WebProbeInfo ], BinWebRoot, Lang, File ) ->

    MsgStr = case Lang of

        french ->
            "Une seule sonde basique activée ";

        _ ->
            "A single basic probe enabled"

    end,

    file_utils:write_ustring( File, "<a name=\"basic_probes\"></a>~n"
        "<p>~ts: ~ts</p>~n",
        [ MsgStr, get_probe_link( WebProbeInfo, BinWebRoot ) ] );


% Multiple (basic) probes here:
write_basic_probes( BasicProbeInfos, BinWebRoot, Lang, File ) ->

    MsgStr = case Lang of

        french ->
            "sondes basiques activées ";

        _ ->
            "basic probes enabled"

    end,

    ProbeLinks = [ get_html_link_for( PName, BinWebRoot, Lang )
        || { PName, _BinDirPath } <- BasicProbeInfos ],

    file_utils:write_ustring( File,
        "<a name=\"basic_probes\"></a>~n<p>~B ~ts: ~ts</p>~n",
        [ length( BasicProbeInfos ), MsgStr,
          web_utils:get_unordered_list( ProbeLinks ) ] ).



-doc "Returns an HTML link (if possible) for the specified basic probe.".
-spec get_html_link_for( bin_probe_name(), bin_directory_path(),
                         human_language() ) -> ustring().
get_html_link_for( BinProbeName, BinWebRoot, Lang ) ->

    % Relative to the web root:
    ContentRelPath = file_utils:join( [ "simulation-results",
        class_Probe:get_report_filename(
            text_utils:binary_to_string( BinProbeName ) ) ] ),

    ProbeContentFilePath = file_utils:join( BinWebRoot, ContentRelPath ),

    %trace_utils:debug_fmt( "Searching for '~ts' from '~ts'.",
    %   [ ProbeContentFilePath, file_utils:get_current_directory() ] ),

    % Should a basic probe not receive any sample, no rendering thereof will be
    % produced; so we do not want to generate an URL that is actually a dead
    % link:
    %
    case file_utils:is_existing_file( ProbeContentFilePath ) of

        true ->
            text_utils:format( "<a href=\"~ts\">~ts</a>",
                               [ ContentRelPath, BinProbeName ] );

        false ->
            AvailStr = case Lang of

                french ->
                    "aucun résultat disponible pour ";

                _ ->
                    "no available report for"

            end,

            text_utils:format( "(~ts <em>~ts</em>)",
                               [ AvailStr, BinProbeName ] )

    end.



-doc "Writes down the fact that there is no probe available.".
-spec write_no_probe( human_language(), file() ) -> void().
write_no_probe( _Lang=french, File ) ->
    file_utils:write_ustring( File,
        % "record":
        "<p>Aucune sonde de résultat activée (se référer au champ "
        "<code>result_specification</code> de l'enregistrement "
        "<code>simulation_settings</code> afin d'activer toute "
        "sonde (basique ou web) potentielle).</p>~n", [] );

write_no_probe( _Lang, File ) ->
    file_utils:write_ustring( File,
        "<p>No tracked probe available (see the "
        "<code>result_specification</code> field of the "
        "<code>simulation_settings</code> record to enable any "
        "potential, either basic or web, probe).</p>~n", [] ).


-doc "Writes the footer of the web page.".
write_footer( File ) ->
    file_utils:write_ustring( File,
        "   <hr>~n"
        "   <p><center><img src=\""?web_elements_dir"/"?logo_filename"\" width=10%></center></p>~n"
        " </body>~n"
        "</html>~n", [] ).



-doc "Starts the corresponding webserver, with the current user.".
-spec start_webserver( tcp_port(), directory_path(), wooper:state() ) ->
                                            wooper:state().
start_webserver( TCPPort, WebRoot, State ) ->

    % Check:
    undefined = ?getAttr(webserver_id),

    % Now relying for the best on a Myriad minimalist webserver:
    SrvId = web_utils:start_server( _SrvName="sim_diasca_result_webserver",
        _BindIPAddressSpec=localhost, TCPPort,
        _AnySrvRootDir=WebRoot,
        _AnyDocRootDir=WebRoot,
        _SrvProfile=sim_diasca_http_result ),

    setAttribute( State, webserver_id, SrvId ).



-doc "Stops any corresponding webserver.".
-spec stop_webserver( wooper:state() ) -> void().
stop_webserver( State ) ->
    case ?getAttr(webserver_id) of

        undefined ->
            ok;

        SrvId ->
            web_utils:stop_server( SrvId )

    end.



-doc """
Starts a suitable webserver (on a default port) so that the specified facility
probe can be browsed; returns its server identifier.
""".
-spec start_facility_webserver( facility_probe_pid() ) ->
                                        static_return( server_id() ).
start_facility_webserver( FacilityProbePid ) ->
    SrvId = start_facility_webserver( FacilityProbePid, _DefTCPPort=8081 ),
    wooper:return_static( SrvId ).


% TO-DO: add a means of launching a browser for that webserver, of pointing to
% the page of that probe and of waiting until thet server is closed.


-doc """
Starts a suitable webserver on the specified port, so that the specified
facility probe can be browsed; returns its server identifier.
""".
-spec start_facility_webserver( facility_probe_pid(), tcp_port() ) ->
                                        static_return( server_id() ).
start_facility_webserver( FacilityProbePid, TCPPort ) ->

    FacilityProbePid ! { getProbeDirectory, [], self() },

    BinProbeDir = receive

        { wooper_result, BinWebProbeDir } ->
            BinWebProbeDir

    end,

    SrvId = web_utils:start_server( _SrvName="sim_diasca_facility_webserver",
        _BindIPAddressSpec=localhost, TCPPort,
        _AnySrvRootDir=BinProbeDir,
        _AnyDocRootDir=BinProbeDir, _SrvProfile=sim_diasca_http_facility ),

    wooper:return_static( SrvId ).



-doc "Stops the facility webserver specified by its identifier.".
-spec stop_facility_webserver( server_id() ) -> static_void_return().
stop_facility_webserver( FacSrvId ) ->
    web_utils:stop_server( FacSrvId ),
    wooper:return_static_void().



%-doc """
%Starts the corresponding webserver, with the specified configuration file, with
%the current user.
%""".
%-spec start_webserver( directory_path(), directory_path(), file_path(),
%                       wooper:state() ) -> void().
%start_webserver( _ServerInstallRoot, WebRoot, ConfigFilename,
%                 State ) ->

    % Former version was based on Node.js:

    % % Better than npm:
    % WebExec = "node",

    % NodePath = case executable_utils:lookup_executable( WebExec ) of

    %     false ->
    %         ?error_fmt( "No executable '~ts' found (is Node.js installed?).",
    %                     [ WebExec ] ),
    %         throw( { webserver_executable_not_found, WebExec } );

    %     ExecPath ->
    %         ExecPath

    % end,

    % We should not redirect (hide) the standard error channel (which by default
    % % ends up in the user console), as it may return useful information,
    % % notably:
    % %
    % % events.js:170
    % %    throw er; // Unhandled 'error' event
    % %  ^
    % % Error: listen EADDRINUSE: address already in use :::8080
    % % at Server.setupListenHandle [as _listen2] (net.js:1259:14)
    % %
    % % This happens whenever a server is already listening to that port,
    % % typically due to a previous launch. As such an instance points to an
    % % obsolete content root, this error shall not be hidden.

    % %LogFilename = file_utils:join( NewWebRoot,
    % %                               "sim-diasca-web-launch.log" ),

    % %Command = text_utils:join( _Sep=" ", [ NodePath, ConfigFilename,
    % %               text_utils:format( " 1> ~ts 2>&1", [ LogFilename ] ) ),

    % % TO-DO: perform a direct HTTP test prior to a new launch.

    % By design we expect that a relevant configuration file is available there:
    % Command = text_utils:join( _Sep=" ", [ NodePath, ConfigFilename ] ),

    % % We have to be in the directory where server.js is located:
    % system_utils:run_background_command( Command, _Environment=[],
    %                                      _WorkingDir=ServerInstallRoot ).






-doc "Returns a textual representation of this instance.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

    SupportString = case table:enumerate( ?getAttr(support_table) ) of

        [] ->
            "not having initialised support classes";

        [ { SupportClassname, IsInit } ] ->
            text_utils:format(
                "having initialised (to ~w) a single support class: ~ts",
                [ IsInit, SupportClassname ] );

        SupportClasses ->
            text_utils:format( "having initialised ~B support classes: ~ts",
                [ length( SupportClasses ),
                  table:to_string( SupportClasses ) ] )

    end,

    ProbeString = case table:values( ?getAttr(probe_table) ) of

        [] ->
            "no web probe";

        ProbeInfos ->
            text_utils:format( "~B web probe(s): ~ts", [ length( ProbeInfos ),
                text_utils:strings_to_string(
                    [ Name || { Name, _Dir } <- ProbeInfos ] ) ] )

    end,

    text_utils:format( "web manager, whose content root is '~ts' "
        %"(webserver installation root is '~ts', TCP port is ~B), "
        "(TCP port is ~B, webserver identifier is ~w), "
        "~ts, referencing ~ts",
        [ ?getAttr(webserver_content_root), %?getAttr(webserver_install_root),
          ?getAttr(tcp_port), ?getAttr(webserver_id), SupportString,
          ProbeString ] ).
