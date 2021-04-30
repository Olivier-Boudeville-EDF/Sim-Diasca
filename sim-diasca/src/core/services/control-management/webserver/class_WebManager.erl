% Copyright (C) 2019-2021 EDF R&D

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
% Creation date: Friday, June 7, 2019.


-module(class_WebManager).


-define( class_description, "Overall (singleton) manager of web-based
		 interactions, notably in terms of result producers (probes), possibly
		 based on client-side, otherwise server-side (Node.js) Javascript, and
		 to be perused by the user thanks to a web browser."  ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).



-type manager_pid() :: sim_diasca:agent_pid().

-type probe_info() :: class_ResultManager:probe_info().


% Table storing the known (web) probes:
-type probe_table() :: table( probe_pid(), probe_info() ).


-export_type([ manager_pid/0, probe_table/0 ]).


% To silence unused warnings now that a webserver is not needed:
-export([ get_default_webserver_installation_root/0,
		  generate_webserver_configuration_file/5, check_no_pending_webserver/2,
		  start_webserver/4 ]).


-include_lib("wooper/include/wooper.hrl").


% For its registering name:
-include("class_WebManager.hrl").



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Control.Web" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% The default TCP port at which the corresponding webserver is to run:
-define( default_webserver_tcp_port, 8080 ).



% Table allowing to associate class-level information about support facilities.
%
% For example, a given type (i.e. class) of web probe might need one-time
% operations to enable its support (ex: a relevant CSS shall be copied once for
% all in a target content directory). This operation shall be done exactly once,
% iff at least one instance of such probe is needed (hence either not at all
% should no instance of it be created, or once, at the creation of the first
% instance).
%
% So the boolean associated to the classname tells whether the corresponding
% support has already been initialized (or terminated since then).
%
%-type support_table() :: table( wooper:classname(), boolean() ).


% Shorthands:
-type ustring() :: text_utils:ustring().

-type directory_path() :: file_utils:directory_path().
-type file_path() :: file_utils:file_path().

-type bin_directory_path() :: file_utils:bin_directory_path().
-type bin_file_path() :: file_utils:bin_file_path().

-type probe_pid() :: class_WebProbe:probe_pid().
-type bin_probe_name() :: class_ResultProducer:bin_producer_name().
-type interactivity_mode() :: class_TimeManager:simulation_interactivity_mode().



% The class-specific attributes of an instance of a web manager are:
-define( class_attributes, [

	{ webserver_install_root, maybe( bin_directory_path() ),
	  "the root directory of the webserver runtime install (if any)" },

	{ webserver_content_root, bin_directory_path(),
	  "the root directory of the web content to be served" },

	{ result_manager_pid, class_ResultManager:manager_pid(),
	  "PID of the result manager" },

	{ result_dir, maybe( bin_directory_path() ),
	  "the directory (if any) in which the (web) results to aggregate will be "
	  "found" },

	{ tcp_port, tcp_port(),
	  "the TCP port at which the local webserver is to run" },

	{ support_table, support_table(), "a table telling whether the support "
	  "brought by a given classname has already been initialized (currently, "
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

-define( css_filename, "sim-diasca.css" ).

-define( logo_filename, "sim-diasca.png" ).


% Shorthands:
-type tcp_port() :: net_utils:tcp_port().


% Implementation notes:
%
% Some probes are web-based; most of them will rely, among other elements, on
% client-side Javascript components. Some others may rely on server-side
% components, and as such rely on a suitable local webserver (for the
% aforementioned probes, typically a Node.js instance, possibly hosting multiple
% instances of such probes), running on a (generally unpriviledged) TCP port.
%
% Of course all these prerequisites have to be installed beforehand, for the web
% support to be available (at least some basic checks are performed when
% creating such a manager).
%
% As a result, by default, any web-based interaction with the engine is
% typically to be done at 'http://localhost:8080/'. Of course a browser (such as
% Firefox) will be needed.
%
% Recent browsers (ex: Firefox since the version 68) will block CORS requests
% about resources like remote *.js files (see
% https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS/Errors/CORSRequestNotHttp).
%
% A solution is, in the "about:config" tag, to set the
% "privacy.file_unique_origin" key to false
% (cf. https://support.mozilla.org/bm/questions/1264312).
%
% We realised over time that for at least most web probes, Javascript lies on
% the client only, no server-side support is needed, and not even a webserver is
% required. So, from now, a Node.js server is not started anymore (call to
% start_webserver/4 commented-out).
%
% Moreover all remote resources (ex: see planner-header.html) have been cached
% locally, so that a simulation can be run with no Internet access at all.



% Creates the web manager.
%
% Construction parameters are:
%
% - SII is the identifier of this simulation run
%
% - EngineRootDir is the root directory of the engine
%
% - InteractivityMode tells whether we run in batch or interactive mode
%
% - ServerContentRoot designates the root directory of the web content to serve;
% belonging to a  directory specific to the current simulaiton, it is
% supposed to be created by this manager (hence not to exist already, whereas
% its direct parent directory is expected to exist already)
%
% - MaybeTCPPort is, if defined, the TCP port at which the local webserver is to
% run
%
-spec construct( wooper:state(), sim_diasca:sii(), bin_directory_path(),
	interactivity_mode(), directory_path(), class_ResultManager:manager_pid(),
	maybe( tcp_port() ) ) -> wooper:state().
construct( State, SII, EngineRootDir, InteractivityMode, ServerContentRoot,
		   ResultManagerPid, MaybeTCPPort ) ->
	construct( State, SII, EngineRootDir, InteractivityMode, ServerContentRoot,
			   ResultManagerPid, _ServerInstallRoot=undefined, MaybeTCPPort ).



% Creates the web manager.
%
% Construction parameters are:
%
% - SII is the identifier of this simulation run
%
% - EngineRootDir is the root directory of the engine
%
% - InteractivityMode tells whether we run in batch or interactive mode
%
% - ServerContentRoot designates the root directory of the web content to serve;
% it is located in an (already-created) directory that is specific to the
% current simulation; it is supposed to be created by this manager (hence not to
% exist already)
%
% - MaybeServerInstallRoot designates, if defined, the root directory of the
% webserver installation
%
% - MaybeTCPPort is, if defined, the TCP port at which the local webserver is to
% run
%
-spec construct( wooper:state(), sim_diasca:sii(), bin_directory_path(),
	interactivity_mode(), directory_path(), class_ResultManager:manager_pid(),
	maybe( directory_path() ), maybe( tcp_port() ) ) -> wooper:state().
construct( State, SII, EngineRootDir, InteractivityMode, ServerContentRoot,
		   ResultManagerPid, MaybeServerInstallRoot, MaybeTCPPort ) ->

	% Oneway should be safe here:
	ResultManagerPid ! { addResultListener, self() },

	% First the direct mother classes:
	BaseState = class_EngineBaseObject:construct( State,
							?trace_categorize("WebManager") ),

	{ NewMaybeBinServerInstallRoot, NewBinServerContentRoot, TCPPort } =
		check_settings( ServerContentRoot, MaybeServerInstallRoot, MaybeTCPPort,
						BaseState ),

	% Start this potentially longer check early:
	%check_no_pending_webserver( TCPPort, BaseState ),

	RegistrationName = get_registration_name(),

	% Web probes rely on it:
	naming_utils:register_as( RegistrationName, ?registration_scope ),

	class_InstanceTracker:register_agent( RegistrationName ),

	EmptyTable = table:new(),

	CreatedState = setAttributes( BaseState, [
			{ webserver_install_root, NewMaybeBinServerInstallRoot },
			{ webserver_content_root, NewBinServerContentRoot },
			{ result_manager_pid, ResultManagerPid },
			{ result_dir, undefined },
			{ tcp_port, TCPPort },
			{ support_table, EmptyTable },
			{ probe_table, EmptyTable },
			{ interactivity_mode, InteractivityMode },
			{ sii, SII },
			{ engine_root_dir,
			  text_utils:string_to_binary( EngineRootDir ) } ] ),

	% No more webserver needed:

	%_CfgFilename = generate_webserver_configuration_file( SII,
	%	  NewServerInstallRoot, NewServerContentRoot, TCPPort, CreatedState ),

	%start_webserver( NewServerInstallRoot, NewServerContentRoot,
	%				 CfgFilename, CreatedState ),

	?send_debug_fmt( CreatedState, "Created ~s",
					 [ to_string( CreatedState ) ] ),

	CreatedState.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?info( "Deleting web manager." ),

	?getAttr(result_manager_pid) ! { removeResultListener, self() },

	% Web probes not owned, hence not deallocated here.

	State.




% Methods section.



% Declares a new web probe.
-spec declareWebProbe( wooper:state(), bin_probe_name(), wooper:classname(),
					   maybe( bin_directory_path() ) ) ->
										request_return( 'web_probe_declared' ).
declareWebProbe( State, BinProbeName, WebProbeClassname, MaybeBinProbeDir ) ->

	?debug_fmt( "Declaring web probe '~s' of class '~s'.",
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

	DeclaredState = setAttribute( State, probe_table, NewProbeTable ),

	% We should not return to the probe the engine root dir or the webserver
	% installation root, as the probe may be running on a different host.

	SupportTable = ?getAttr(support_table),

	SupportState = case table:lookup_entry( WebProbeClassname, SupportTable ) of

		% Not initialized yet, maybe a class-specific initialization callback
		% has been defined:
		%
		key_not_found ->
			SupportValue = case meta_utils:is_function_exported(
					Mod=WebProbeClassname, Fun=initialize_support, _Arity=3 ) of

				true ->
					Mod:Fun( ?getAttr(engine_root_dir),
							 ?getAttr(webserver_install_root),
							 ?getAttr(webserver_content_root) );

				false ->
					true

			end,

			NewSupportTable = table:add_entry( WebProbeClassname,
											   SupportValue, SupportTable ),

			setAttribute( DeclaredState, support_table, NewSupportTable );


		% Already initialized, nothing more to do:
		{ value, _IsInit=true } ->
			DeclaredState

	end,

	wooper:return_state_result( SupportState, web_probe_declared ).



% Callback triggered by the result manager, as the web manager is a listener
% thereof.
%
-spec results_collected( wooper:state(), bin_directory_path() ) ->
								oneway_return().
results_collected( State, ResultBaseDirName ) ->

	% The web manager already knows the web-related information, but other probe
	% information (including basic probes and metadata) are also of interest
	% here:
	%
	?getAttr(result_manager_pid) ! { getBaseProbeInfos, [], self() },

	?debug_fmt( "Notified that results were collected, in '~s'.",
				[ ResultBaseDirName ] ),

	BrowserPath = executable_utils:get_default_web_browser_path(),

	WebProbePairs = table:values( ?getAttr(probe_table) ),

	% Answer from the getBaseProbeInfos/1 request:
	{ LandingFilePath, GenState } = receive

		% Virtual probes currently not specifically managed here:
		{ wooper_result, { BasicProbePairs, _VirtualProbePairs, Metadata } } ->
			generate_landing_page( BasicProbePairs, WebProbePairs, Metadata,
								   State )

	end,

	case ?getAttr(interactivity_mode) of

		batch ->
			?debug( "In batch mode, hence no browser launched." ),
			ok;

		interactive ->
			Command = text_utils:format( "~s file://~s",
										 [ BrowserPath, LandingFilePath ] ),

			?debug_fmt( "In interactive mode, thus launching a browser pointing"
						" to the landing page: '~s'.", [ Command ] ),

			system_utils:run_background_command( Command )

	end,

	wooper:return_state( GenState ).



% Static methods.


% Initializes synchronously (typically from the simulation case) the web
% management service, using the specified SII and directories, respectively as
% the root directory of the engine, as webserver content root and as webserver
% installation root, and the specified TCP port.
%
-spec create_manager( sim_diasca:sii(), bin_directory_path(),
	interactivity_mode(), bin_directory_path(),
	class_ResultManager:manager_pid(), maybe( bin_directory_path() ),
	maybe( tcp_port() ) ) -> static_return( manager_pid() ).
create_manager( SII, EngineRootDir, InteractivityMode, WebserverContentRoot,
				ResultManagerPid, MaybeWebserverInstallRoot, MaybeTCPPort ) ->
	wooper:return_static( synchronous_timed_new_link( SII, EngineRootDir,
		InteractivityMode, WebserverContentRoot, ResultManagerPid,
		MaybeWebserverInstallRoot, MaybeTCPPort ) ).



% Returns the atom corresponding to the name the  web manager should be
% registered as.
%
-spec get_registration_name() -> static_return( net_utils:atom_node_name() ).
get_registration_name() ->
	wooper:return_static( ?web_manager_name ).



% Returns the web content root corresponding to specified base result directory.
-spec get_web_content_root( directory_path() ) ->
									static_return( directory_path() ).
get_web_content_root( ResultDir ) ->

	WebserverContentRoot = file_utils:join( ResultDir, "web-content" ),

	wooper:return_static( WebserverContentRoot ).



% Helper functions.


% Checks the settings assigned to this manager (and performs some side effects
% like the creation of directories).
%
-spec check_settings( directory_path(), maybe( directory_path() ),
					  maybe( tcp_port() ), wooper:state() ) ->
		{ bin_directory_path(), bin_directory_path(), tcp_port() }.
check_settings( ServerContentRoot, _MaybeServerInstallRoot, MaybeTCPPort,
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
	%	undefined ->
	%		InstDir = get_default_webserver_installation_root(),
	%		?notice_fmt( "No user-defined webserver root directory specified, "
	%				   "using the default one, '~s'.", [ InstDir ] ),
	%		InstDir;
	%
	%	InstRoot ->
	%		?notice_fmt( "User-specified webserver root specified, "
	%				   "using the default one, '~s'.", [ InstRoot ] ),
	%		file_utils:ensure_path_is_absolute( InstRoot )
	%
	%end,

	%CheckedServerInstallRoot = case file_utils:is_existing_directory_or_link(
	%							  ServerInstallRoot ) of
	%
	%	true ->
	%		text_utils:string_to_binary( ServerInstallRoot );
	%
	%	false ->
	%		?error_fmt( "The specified root directory of the webserver "
	%				"install, '~s', does not exist.", [ ServerInstallRoot ] ),
	%		throw( { non_existing_webserver_install_root, ServerInstallRoot } )
	%
	%end,
	CheckedServerInstallRoot = undefined,

	CanonServerContentRoot =
		file_utils:ensure_path_is_absolute( ServerContentRoot ),

	CheckedServerContentRoot = case
		  file_utils:is_existing_directory_or_link( CanonServerContentRoot ) of

		true ->
			?error_fmt( "The root directory of the webserver content, "
				"'~s', already exists (transformed from ~s).",
				[ CanonServerContentRoot, ServerContentRoot ] ),

			throw( { already_existing_webserver_content_root,
					 CanonServerContentRoot } );

		false ->
			% Will not create any lacking parent directory:
			file_utils:create_directory( CanonServerContentRoot ),
			text_utils:string_to_binary( CanonServerContentRoot )

	end,

	{ CheckedServerInstallRoot, CheckedServerContentRoot, ServerTCPPort }.



% Checks that no pending local (web)server is lingering at the TCP port we
% target, to ensure that a next launch is possible.
%
-spec check_no_pending_webserver( tcp_port(), wooper:state() ) -> void().
check_no_pending_webserver( TCPPort, State ) ->

	case net_utils:is_service_running_at( TCPPort ) of

		true ->
			?error_fmt( "A service (presumably a webserver) is already "
				"running on local TCP port #~B, it shall be "
				"stopped first.", [ TCPPort ] ),
			throw( { server_already_running_at_port, TCPPort } );

		false ->
			?debug_fmt( "No local server found at port #~B.", [ TCPPort ] )

	end.



% Returns the default root directory of the webserver installation.
-spec get_default_webserver_installation_root() -> directory_path().
get_default_webserver_installation_root() ->

	% Possibly a symlink:
	file_utils:join( [ system_utils:get_user_home_directory(), "Software",
					   "sim_diasca_webserver_install_root" ] ).



% Generates a new, suitable Node.js configuration file.
-spec generate_webserver_configuration_file( sim_diasca:sii(),
		bin_directory_path(), bin_directory_path(),
		tcp_port(), wooper:state() ) -> bin_file_path().
generate_webserver_configuration_file( SII, BinServerInstallRoot,
									   BinServerContentRoot, TCPPort, State ) ->

	TargetFilename = text_utils:format( "sim-diasca-~s-server.js", [ SII ] ),

	TargetFilePath = file_utils:join( BinServerInstallRoot, TargetFilename ),

	% Made not to fail, knowing a clash in filenames is very unlikely:
	case file_utils:is_existing_file_or_link( TargetFilePath ) of

		true ->

			BackupName = text_utils:format( "~s-~s", [ TargetFilePath,
							 time_utils:get_textual_timestamp_for_path() ] ),

			?warning_fmt( "A Node.js configuration file has been found already "
				"existing, '~s'; moving it out of the way by "
				"renaming it to '~s'.", [ TargetFilePath, BackupName ] ),

			file_utils:rename( TargetFilePath, BackupName );

		false ->
			ok

	end,

	% Now not existing by design:
	NewCfgFile = file_utils:open( TargetFilePath, [ write, raw ] ),

	file_utils:write_ustring( NewCfgFile,
		"// Generated by Sim-Diasca on ~s.~n"
		"const express = require('express');~n"
		"const app = new express();~n"
		"app.use(express.static('~s'));~n"
		"app.listen(~B);~n"
		"// End of generated file.~n",
		[ time_utils:get_textual_timestamp(), BinServerContentRoot, TCPPort ] ),

	file_utils:close( NewCfgFile ),

	?notice_fmt( "New configuration file '~s' written.", [ TargetFilePath ] ),

	TargetFilePath.





% Generates a landing page (index.html) at the root of the specified web content
% directory, and returns its path.
%
% Any pre-existing version of that page will be removed first.
%
-spec generate_landing_page( [ probe_info() ], [ probe_info() ],
	class_ResultManager:meta_data(), wooper:state() ) -> file_path().
generate_landing_page( BasicProbeInfos, WebProbeInfos, Metadata, State ) ->

	?debug_fmt( "Generating landing page for:~n  - basic probes: ~p~n"
				"  - web probes: ~p~n(metadata: ~p)",
				[ BasicProbeInfos, WebProbeInfos, Metadata ] ),

	BinServerContentRoot = ?getAttr(webserver_content_root),

	CommonDir = file_utils:join( [ ?getAttr(engine_root_dir), "sim-diasca",
								   "doc", "common-elements" ] ),

	CssFilename = file_utils:join( [ CommonDir, "css", ?css_filename ] ),

	file_utils:copy_file_in( CssFilename, BinServerContentRoot ),

	LogoFilename = file_utils:join(
					 [ CommonDir, "edf-related", ?logo_filename ] ),

	file_utils:copy_file_in( LogoFilename, BinServerContentRoot ),


	LandingFilename = file_utils:join( BinServerContentRoot, "index.html" ),

	case file_utils:is_existing_file_or_link( LandingFilename ) of

		true ->
			% Quite surprising:
			?warning_fmt( "Removing a former version of '~s'.",
						  [ LandingFilename ] ),
			file_utils:remove_file( LandingFilename );

		false ->
			ok

	end,

	?debug_fmt( "Generating following landing page: '~s'.",
				[ LandingFilename ] ),

	% Do not *ever* add 'raw' here, otherwise encoding might be screwed up:
	PageFile = file_utils:open( LandingFilename, [ write ] ),

	write_header( ?getAttr(sii), Metadata, PageFile ),

	HasBasicProbe = case BasicProbeInfos of

		[] ->
			false;

		_ ->
			true

	end,

	HasWebProbe = case WebProbeInfos of

		[] ->
			false;

		_ ->
			true

	end,

	case { HasBasicProbe, HasWebProbe } of

		{ true, true } ->
			write_toc( BasicProbeInfos, WebProbeInfos, PageFile ),

			% Basic probes better listed last (generally gather final results):
			write_web_probes( WebProbeInfos, BinServerContentRoot, PageFile ),

			write_basic_probes( BasicProbeInfos, BinServerContentRoot,
								PageFile );

		{ true, false } ->
			write_basic_probes( BasicProbeInfos, BinServerContentRoot,
								PageFile );

		{ false, true } ->
			write_web_probes( WebProbeInfos, BinServerContentRoot, PageFile );

		{ false, false } ->
			write_no_probe( PageFile )

	end,

	write_footer( PageFile ),

	file_utils:close( PageFile ),

	TermState = manage_support_termination( State ),

	{ LandingFilename, TermState }.



% Manages the termination of every registered, initialized support class, by
% calling any appropriate class-level callbacks.
%
-spec manage_support_termination( wooper:state() ) -> wooper:state().
manage_support_termination( State ) ->

	SupportTable = ?getAttr(support_table),

	SupportClassnamesToTerminate = [ Classname
		|| { Classname, _IsInit=true } <- table:enumerate( SupportTable ) ],

	%trace_utils:debug_fmt( "Support classes to terminate: ~p",
	%					   [ SupportClassnamesToTerminate ] ),

	[ case meta_utils:is_function_exported( Mod=Classname,
								Fun=terminate_support, _Arity=3 ) of

		  true ->
			  true = Mod:Fun( ?getAttr(engine_root_dir),
					   ?getAttr(webserver_install_root),
					   ?getAttr(webserver_content_root) );

		  false ->
			  ok

	  end || Classname <- SupportClassnamesToTerminate ],

	NewSupportTable = table:add_entries( [ { Classname, _IsInit=false }
		  || Classname <- SupportClassnamesToTerminate ], SupportTable ),

	setAttribute( State, support_table, NewSupportTable ).



% Writes the header of the landing page.
-spec write_header( sim_diasca:sii(), class_ResultManager:meta_data(),
					file_utils:file() ) -> void().
write_header( SII, Metadata, File ) ->

	SimString = list_table:get_value( simulation_name, Metadata ),

	RunString = text_utils:format( "run ID: ~s", [ SII ] ),

	TimeString = text_utils:format( "collection timestamp: ~s",
									[ time_utils:get_textual_timestamp() ] ),

	HostString = text_utils:format( "user host: ~s",
									[ net_utils:localhost() ] ),

	TickString = list_table:get_value( tick_duration, Metadata ),

	% Layer versions skipped:
	MetadataString = web_utils:get_unordered_list(
			[ SimString, RunString, TimeString, HostString, TickString ] ),

	file_utils:write_ustring( File,
	  "<!DOCTYPE html>~n"
	  "<html>~n"
	  "  <head>~n"
	  "    <title>Simulation Results for run ~s</title>~n"
	  "    <meta charset=\"UTF-8\">~n"
	  "    <meta name=\"description\" content=\"Sim-Diasca Web Results\">~n"
	  "    <meta name=\"keywords\" content=\"HTML,CSS,XML,JavaScript\">~n"
	  "    <meta name=\"author\" content=\"Sim-Diasca\">~n"
	  "    <link rel=\"stylesheet\" href=\""?css_filename"\">~n"
	  "    <meta name=\"viewport\" content=\"width=device-width,"
			   "initial-scale=1.0\">~n"
	  "  </head>~n"
	  "  <body>~n"
	  "    <h1>Welcome to the Sim-Diasca web results browser "
	  "for simulation run ~s</h1>~n"
	  "<blockquote><p><b>Simulation information</b>:~n~s</p></blockquote>~n",
	  [ SII, SII, MetadataString ] ).



% Writes a suitable mini-table of contents.
write_toc( _BasicProbeNames, _WebProbeNames, _File ) ->
	ok.



% Single probe here:
write_web_probes( [ WebProbeInfo ], BinServerContentRoot, File ) ->
	file_utils:write_ustring( File, "<a name=\"web_probes\"></a>~n"
		"<p>A single web probe enabled: ~s</p>~n",
		[ get_probe_link( WebProbeInfo, BinServerContentRoot ) ] );

write_web_probes( WebProbeInfos, BinServerContentRoot, File ) ->

	ProbeLinks = [ get_probe_link( I, BinServerContentRoot )
				   || I <- WebProbeInfos ],

	file_utils:write_ustring( File, "<a name=\"web_probes\"></a>~n"
		"<p>~B web probes enabled:~n~s</p>~n",
		[ length( WebProbeInfos ),
		  web_utils:get_unordered_list( ProbeLinks ) ] ).



% Must be a tracked (non-facility) *web* probe, whose result location is a
% priori known:
%
get_probe_link( { BinProbeName, _MaybeBinProbeDir=undefined },
				_BinServerContentRoot ) ->
	text_utils:format( "<a href=\"~s\">~s</a>", [
		file_utils:join( [ "..", "simulation-results",
					class_WebProbe:get_filename_for( BinProbeName ) ] ),
		BinProbeName ] );


% Must be a facility web probe, in a directory of its own:
get_probe_link( { BinProbeName, BinProbeDir }, BinServerContentRoot ) ->

	RelativeDir = file_utils:make_relative(
		text_utils:binary_to_string( BinProbeDir ),
		text_utils:binary_to_string( BinServerContentRoot ) ),

	% We do our best to rely on relative links:
	text_utils:format( "<a href=\"~s\">~s</a>", [
		file_utils:join( RelativeDir,
						 class_WebProbe:get_filename_for( BinProbeName ) ),
		BinProbeName ] ).



% At least one of them, possibly just one:
write_basic_probes( _BasicProbeInfos=[ { ProbeName, _BinDirPath } ],
					BinServerContentRoot, File ) ->

	ProbeLink = get_html_link_for( ProbeName, BinServerContentRoot ),

	file_utils:write_ustring( File, "<a name=\"basic_probes\"></a>~n<p>"
		"A single basic probe enabled: ~s</p>~n", [ ProbeLink ] );


% At least two of them:
write_basic_probes( BasicProbeInfos, BinServerContentRoot, File ) ->

	ProbeLinks = [ get_html_link_for( PName, BinServerContentRoot )
				   || { PName, _BinDirPath } <- BasicProbeInfos ],

	file_utils:write_ustring( File,
	  "<a name=\"basic_probes\"></a>~n<p>~B basic probes enabled: ~s</p>~n",
	  [ length( BasicProbeInfos ),
		web_utils:get_unordered_list( ProbeLinks ) ] ).



% Returns an HTML link (if possible) for specified basic probe.
-spec get_html_link_for( bin_probe_name(), bin_directory_path() ) -> ustring().
get_html_link_for( BinProbeName, BinServerContentRoot ) ->

	ProbeContentFilePath = file_utils:join( [ BinServerContentRoot, "..",
		"simulation-results", class_Probe:get_report_filename(
							text_utils:binary_to_string( BinProbeName ) ) ] ),

	%trace_utils:debug_fmt( "Searching for '~s' from '~s'.",
	%	[ ProbeContentFilePath, file_utils:get_current_directory() ] ),

	% Should a basic probe not receive any sample, no rendering thereof will be
	% produced; so we do not want to generate an URL that is actually a dead
	% link:
	%
	case file_utils:is_existing_file( ProbeContentFilePath ) of

		true ->
			text_utils:format( "<a href=\"~s\">~s</a>",
							   [ ProbeContentFilePath, BinProbeName ] );

		false ->
			text_utils:format( "(no available report for: <em>~s</em>)",
							   [ BinProbeName ] )

	end.



% Writes down the fact that there is no probe available.
write_no_probe( File ) ->
	file_utils:write_ustring( File,
	  "<p>No tracked probe available (see the <code>result_specification</code>"
	  " field of the <code>simulation_settings</code> record to enable any "
	  "potential, either basic or web, probe).</p>~n", [] ).


write_footer( File ) ->
	file_utils:write_ustring( File,
	  "     <hr>~n"
	  "     <p><center><img src=\""?logo_filename"\" width=15%></center></p>~n"
	  "  </body>~n"
	  "</html>~n", [] ).




% Starts the corresponding webserver, with specified configuration file, with
% the current user.
%
-spec start_webserver( directory_path(), directory_path(), file_path(),
					   wooper:state() ) -> void().
start_webserver( ServerInstallRoot, _ServerContentRoot, ConfigFilename,
				 State ) ->

	% Better than npm:
	WebExec = "node",

	NodePath = case executable_utils:lookup_executable( WebExec ) of

		false ->
			?error_fmt( "No executable '~s' found (is Node.js installed?).",
						[ WebExec ] ),
			throw( { webserver_executable_not_found, WebExec } );

		ExecPath ->
			ExecPath

	end,

	% We should not redirect (hide) the standard error channel (which by default
	% ends up in the user console), as it may return useful information,
	% notably:
	%
	% events.js:170
	%    throw er; // Unhandled 'error' event
	%  ^
	% Error: listen EADDRINUSE: address already in use :::8080
	% at Server.setupListenHandle [as _listen2] (net.js:1259:14)
	%
	% This happens whenever a server is already listening to that port,
	% typically due to a previous launch. As such an instance points to an
	% obsolete content root, this error shall not be hidden.

	%LogFilename = file_utils:join( NewServerContentRoot,
	%							   "sim-diasca-web-launch.log" ),

	%Command = text_utils:join( _Sep=" ", [ NodePath, ConfigFilename,
	%				text_utils:format( " 1> ~s 2>&1", [ LogFilename ] ) ),

	% TO-DO: perform a direct HTTP test prior to a new launch.

	% By design we expect that a relevant configuration file is available there:
	Command = text_utils:join( _Sep=" ", [ NodePath, ConfigFilename ] ),

	% We have to be in the directory where server.js is located:
	system_utils:run_background_command( Command, _Environment=[],
										 _WorkingDir=ServerInstallRoot ).



% Returns a textual representation of this instance.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	SupportString = case table:enumerate( ?getAttr(support_table) ) of

		[] ->
			"not having initialized support classes";

		[ { SupportClassname, IsInit } ] ->
			text_utils:format(
			  "having initialized (to ~w) a single support class: ~s",
			  [ IsInit, SupportClassname ] );

		SupportClasses ->
			text_utils:format( "having initialized ~B support classes: ~s",
			  [ length( SupportClasses ), table:to_string( SupportClasses ) ] )

	end,

	ProbeString = case table:values( ?getAttr(probe_table) ) of

		[] ->
			"no web probe";

		ProbeInfos ->
			text_utils:format( "~B web probe(s): ~s", [ length( ProbeInfos ),
				text_utils:strings_to_string(
						[ Name || { Name, _Dir } <- ProbeInfos ] ) ] )

	end,

	text_utils:format( "web manager, whose content root is '~s' "
		"(webserver installation root is '~s', TCP port is ~B), "
		"~s, referencing ~s",
		[ ?getAttr(webserver_content_root), ?getAttr(webserver_install_root),
		  ?getAttr(tcp_port), SupportString, ProbeString ] ).
