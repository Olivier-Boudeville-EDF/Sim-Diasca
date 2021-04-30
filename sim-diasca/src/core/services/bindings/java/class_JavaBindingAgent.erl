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


-module(class_JavaBindingAgent).


-define( class_description,
		 "Class managing locally, on a given node (generally a computing one), "
		 "the Java binding (i.e. an actual JVM), from Erlang. "
		 "An instance of this class is in charge of launching a corresponding "
		 "JVM, with a proper configuration, and ensuring that a full "
		 "end-to-end connectivity exists between the Erlang processes and "
		 "their Java counterparts (through the worker mailboxes)." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).


% The class-specific attributes:
-define( class_attributes, [

  { binding_manager_pid, class_JavaBindingManager:manager_pid(),
	"the PID of the overall manager of the Java binding (federating all "
	"Java binding agents)" },

  { controller_mbox, maybe( controller_mbox_pid() ),
	"PID of the controller mailbox (if any)" },

  { worker_mboxes, [ maybe( worker_mbox_pid() ) ],
	"a list of the PID of all (local) worker mailboxes (if any)" } ] ).


-type agent_pid() :: class_EngineBaseObject:object_pid().


% PID of a controller mailbox:
-type controller_mbox_pid() :: java_utils:java_mbox_pid().

% PID of a worker mailbox:
-type worker_mbox_pid() :: java_utils:java_mbox_pid().


-export_type([ agent_pid/0, controller_mbox_pid/0, worker_mbox_pid/0 ]).


% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


-include("engine_common_defines.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Deployment.JavaBinding.BindingAgents" ).


% For registration:
%
% (note that this name will be only locally registered, as the Java part will
% send its handshake message to a locally-named process; not relying on global
% names is convenient in order to avoid name clashes between the distributed
% binding agents)
%
-define( java_binding_agent_name, sim_diasca_java_binding_agent ).


% Allows to use macros for trace sending:
-include_lib("traces/include/traces.hrl").


% Shorthands:

-type code_path() :: code_utils:code_path().

-type directory_path() :: file_utils:directory_path().



% Design notes:
%
% For the communication between Erlang and Java, Jinterface is relied upon
% (http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html). Of course
% each OtpMailbox will have its own state.
%
% The binding agent shall specify to the JVM that it launches following
% settings:
%
% - the name that this JVM instance shall elect for its own node (thisNodeName,
% from a Java point of view)
%
% - the name of its counterpart Erlang node, so that it can reach it
% (peerNodeName)
%
% - the cookie to be used by this OTPNode
%
% - the number of cores to use, corresponding to the number of threads to create
% on the JVM side
%
% - the EPMD port number to rely on
%
% - the registered name of this agent
%
% See also: our jinterface-testing branch for a full, minimalistic yet
% autonomous Erlang/Java bridge.



% Implementation notes:
%
% We rely on a properly symlinked JInterface directory, so that its JAR can be
% found typically in lib/erlang/jinterface/priv/OtpErlang.jar.


% This binding relies on JInterface, one may refer to
% http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html.
%
% In practice, if the Java binding is enabled, then the deployment manager
% spawns an instance of the current class on each computing node.
%
% That instance will then execute (as a UNIX process), manage and interact with
% the Java program (SimDiascaJavaRuntimeContainer) in charge of hosting the
% simulation actors that are implemented in Java.
%
% This JVM will be seen as an (Erlang) node (as an instance of JInterface's
% OtpNode) defining OtpMailbox instances (each mailbox being roughly equivalent
% to an Erlang PID):
%
%  - one main, controller mailbox, in charge of general communication
%
%  - a number of worker mailboxes (typically as many of them as there are local
%  cores), each corresponding to a given thread of the JVM
%
% As a result, each thread of the JVM will maintain its own set of
% (Java-implemented) actors, and manage them accordingly. This worker pool is
% load-balanced thanks to a round-robin policy, and allows to avoid that too
% many Java threads are spawned (for example, one per hosted actor would be way
% too many for the JVM).


% Shorthands:

-type ustring() :: text_utils: ustring().



% Constructs a new binding agent managing, on a given node, the use of Java.
-spec construct( wooper:state(), net_utils:tcp_port(), code_path(),
				 class_JavaBindingManager:manager_pid() ) -> wooper:state().
construct( State, EpmdPort, ClassPath, JavaBindingManagerPid ) ->

	Localhost = net_utils:localhost( short ),

	AgentName = text_utils:format( "for ~ts", [ Localhost ] ),

	% First the direct mother class:
	LangState = class_EngineBaseObject:construct( State,
											?trace_categorize(AgentName) ),

	% Any language-specific binding agent might be registered that way:
	% (enforces local uniqueness, and can then be looked up)
	%
	naming_utils:register_as( ?java_binding_agent_name, local_only ),

	?send_notice_fmt( LangState, "Creating the Java binding agent "
		"for the '~ts' host (node: ~ts), using EPMD port ~B and "
		"following user-specified classpath: ~ts",
		[ Localhost, node(), EpmdPort,
		  code_utils:code_path_to_string( ClassPath ) ] ),

	% Triggers the launch, then the handshake will arrive through a oneway call:
	LaunchedState = launch_jvm( EpmdPort, ClassPath, LangState ),

	% Now expecting the JVM to trigger back a call to our handshakeRequest/3
	% oneway.

	setAttributes( LaunchedState, [
					{ binding_manager_pid, JavaBindingManagerPid },
					{ controller_mbox, undefined },
					{ worker_mboxes, undefined } ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	case ?getAttr(controller_mbox) of

		undefined ->
			ok;

		ContPid ->
			ContPid ! terminate

	end,

	State.



% Methods section.


% Notifies this agent of a handshake request, expected to come from the just
% locally-launched JVM, specifying the PID of its main, controller mailbox, and
% the ones of its workers.
%
-spec handshakeRequest( wooper:state(), controller_mbox_pid(),
						[ worker_mbox_pid() ] ) -> oneway_return().
handshakeRequest( State, MainMboxPid, WorkerMboxPids ) ->

	?notice_fmt( "Received a handshake requested from a child JVM, whose "
		"controller mailbox is ~w, and whose ~B worker mailboxes are ~w.",
		[ MainMboxPid, length( WorkerMboxPids ), WorkerMboxPids ] ),

	% Handshake considered over here:
	MainMboxPid ! { handshakeConfirmed, self() },

	% Allows the overall binding manager to complete its tables:
	?getAttr(binding_manager_pid) ! { notifyNodeMailboxes,
									  [ MainMboxPid, WorkerMboxPids, self() ] },

	ReadyState = setAttributes( State, [ { controller_mbox, MainMboxPid },
										 { worker_mboxes, WorkerMboxPids } ] ),

	wooper:return_state( ReadyState ).



% Notifies this agent that the Java side sent a debug message.
-spec onJavaDebugMessage( wooper:state(), ustring() ) ->
								const_oneway_return().
onJavaDebugMessage( State, DebugMessage ) ->

	?debug_fmt( "[Forwarded from Java runtime container] ~ts",
				[ DebugMessage ] ),

	wooper:const_return().



% Notifies this agent that the Java side sent a info message.
-spec onJavaInfoMessage( wooper:state(), ustring() ) ->
								const_oneway_return().
onJavaInfoMessage( State, InfoMessage ) ->

	?info_fmt( "[Forwarded from Java runtime container] ~ts",
			   [ InfoMessage ] ),

	wooper:const_return().



% Notifies this agent that the Java side sent a trace message.
-spec onJavaNoticeMessage( wooper:state(), ustring() ) ->
								const_oneway_return().
onJavaNoticeMessage( State, TraceMessage ) ->

	?notice_fmt( "[Forwarded from Java runtime container] ~ts",
				 [ TraceMessage ] ),

	wooper:const_return().


% Notifies this agent that the Java side sent a warning message.
-spec onJavaWarningMessage( wooper:state(), ustring() ) ->
								const_oneway_return().
onJavaWarningMessage( State, WarningMessage ) ->

	?warning_fmt( "[Forwarded from Java runtime container] ~ts",
				  [ WarningMessage ] ),

	wooper:const_return().



% Notifies this agent that the Java side sent a error message.
-spec onJavaErrorMessage( wooper:state(), ustring() ) ->
								const_oneway_return().
onJavaErrorMessage( State, ErrorMessage ) ->

	?error_fmt( "[Forwarded from Java runtime container] ~ts",
				[ ErrorMessage ] ),

	wooper:const_return().



% Notifies this agent that the Java side sent an critical message.
-spec onJavaCriticalMessage( wooper:state(), ustring() ) ->
								const_oneway_return().
onJavaCriticalMessage( State, CriticalMessage ) ->

	?critical_fmt( "[Forwarded from Java runtime container] ~ts",
				[ CriticalMessage ] ),

	wooper:const_return().



% Notifies this agent that the Java side sent an alert message.
-spec onJavaAlertMessage( wooper:state(), ustring() ) ->
								const_oneway_return().
onJavaAlertMessage( State, AlertMessage ) ->

	?alert_fmt( "[Forwarded from Java runtime container] ~ts",
				[ AlertMessage ] ),

	wooper:const_return().



% Notifies this agent that the Java side sent an emergency message.
-spec onJavaEmergencyMessage( wooper:state(), ustring() ) ->
								const_oneway_return().
onJavaEmergencyMessage( State, EmergencyMessage ) ->

	?emergency_fmt( "[Forwarded from Java runtime container] ~ts",
				[ EmergencyMessage ] ),

	wooper:const_return().




% Notifies this agent that an exception was thrown from the Java side.
-spec onJavaExceptionThrown( wooper:state(), ustring() ) ->
								const_oneway_return().
onJavaExceptionThrown( State, ExceptionString ) ->

	?error_fmt( "Java exception thrown: '~ts', terminating.",
				[ ExceptionString ] ),

	throw( { java_exception_thrown, ExceptionString } ),

	% Just to make WOOPER happy:
	wooper:const_return().



% Returns the Java Virtual Machine associated to the sender of this request.
%
% In practice the returned binding container is the Java VM running on the same
% node as the sender, to lighten the load induced by their exchanges.
%
-spec getAssociatedJavaMailbox( wooper:state() ) ->
			const_request_return( language_utils:java_vm_container_pid() ).
getAssociatedJavaMailbox( State ) ->

	SenderPid = ?getSender(),

	% This inherited method is just fine:
	{ State, InterpreterPid } =
		executeRequest( State, getAssociatedRuntimeContainer, [ SenderPid ] ),

	wooper:const_return_result( InterpreterPid ).




% Static section.


% Returns the atom corresponding to the name the Java binding agent should be
% registered as.
%
% Note: executed on the caller node.
%
-spec get_registration_name() ->
						 static_return( naming_utils:registration_name() ).
get_registration_name() ->
	% Ex: 'sim_diasca_java_binding_agent':
	wooper:return_static( ?java_binding_agent_name ).



% Returns the PID of the (unique) local Java binding agent.
%
% To be used by clients of the Java binding agent.
%
-spec get_registered_agent() -> static_return( 'none' | agent_pid() ).
get_registered_agent() ->

	case naming_utils:is_registered( ?java_binding_agent_name, local ) of

		not_registered ->
			wooper:return_static( none );

		Pid ->
			wooper:return_static( Pid )

	end.



% Returns the path to the binding support code, relatively to the engine root
% (either the build one, typically from the user node, or the deployment one,
% from the computing nodes).
%
-spec get_engine_relative_binding_path() -> static_return( directory_path() ).
get_engine_relative_binding_path() ->
	wooper:return_static( file_utils:join( [ "sim-diasca", "src", "core",
				"services", "dataflow", "bindings", "java", "api" ] ) ).


% Returns the name of the binding main class.
-spec get_binding_classname() -> static_return( java_utils:java_classname() ).
get_binding_classname() ->

	% Defined in SimDiascaJavaRuntimeContainer.java, expected to be available as
	% SimDiascaJavaRuntimeContainer.class and related (ex:
	% SimDiascaJavaRuntimeContainer$RequestCall.class):
	%
	wooper:return_static( "SimDiascaJavaRuntimeContainer" ).



% Returns the implementation file (*.class) corresponding to the binding main
% class.
%
-spec get_binding_class_filename() ->
					static_return( java_utils:java_bytecode_filename() ).
get_binding_class_filename() ->
	wooper:return_static(
	  java_utils:classname_to_bytecode_filename( get_binding_classname() ) ).



% Helpers section.


% Launches specified JVM (as a separate UNIX process), with proper settings for
% interconnection, so that the handshake can proceed.
%
-spec launch_jvm( net_utils:tcp_port(), code_path(), wooper:state() ) ->
						wooper:state().
launch_jvm( EpmdPort, UserClassPath, State ) ->

	% Establishing the proper launch command-line for the JVM.

	% Let's build first the full classpath:

	% Useful to find modules and packages from the case directory:
	WorkingDir = file_utils:get_current_directory(),

	RootDir = class_EngineBaseObject:get_deployment_root_directory(),

	% For the internal engine needs (SimDiascaJavaRuntimeContainer class and
	% related, and the 'myriad' and 'sim_diasca' packages - all of which that
	% are located in bindings/java/api):
	%
	JavaAPIPath = file_utils:join( RootDir,
								   get_engine_relative_binding_path()  ),

	InternalCodePath = [ WorkingDir, JavaAPIPath,
						 executable_utils:get_default_jinterface_path() ],

	% User classpath free to enrich and/or shadow any builtin one:
	ActualClassPath = UserClassPath ++ InternalCodePath,

	ClasspathOpt = "-classpath "
		++ text_utils:join( _PathSeparator=":", ActualClassPath ),

	BindingClassFilename = get_binding_class_filename(),

	BindingAbsFilename = file_utils:join( JavaAPIPath, BindingClassFilename ),

	case file_utils:is_existing_file( BindingAbsFilename ) of

		true ->
			ok;

		false ->
			?error_fmt( "Java binding class in '~ts' has not been deployed, "
				"since this file could not be found from '~ts'.",
				[ BindingClassFilename, JavaAPIPath ] ),

			throw( { no_java_binding_class_found, BindingAbsFilename } )

	end,

	CookieOpt = "--cookie '"
		++ text_utils:atom_to_string( net_utils:get_cookie() ) ++ "'",

	ThisNodeOpt = "--this-node-name 'sim_diasca_java_binding_node'",

	PeerNodeOpt = "--peer-node-name '"
		++ text_utils:atom_to_string( node() ) ++ "'",

	CoreOpt = text_utils:format( "--core-count ~B",
								 [ system_utils:get_core_count() ] ),

	EpmdOpt = "--epmd-port " ++ text_utils:integer_to_string( EpmdPort ),

	JavaExec = executable_utils:get_default_java_runtime(),

	BindingClassname = get_binding_classname(),

	JVMLaunchCommand = text_utils:join( _TokenSeparator=" ",
		[ JavaExec, ClasspathOpt, BindingClassname, CookieOpt, ThisNodeOpt,
		  PeerNodeOpt, CoreOpt, EpmdOpt ] ),

	?debug_fmt( "Launching now a JVM thanks to following command: '~ts'; "
		"this corresponds to following actual classpath: ~ts",
		[ JVMLaunchCommand,
		  code_utils:code_path_to_string( ActualClassPath ) ] ),

	% No possible feedback (ex; result) to collect, as is to run detached, and
	% in parallel of the engine:
	%
	system_utils:run_background_command( JVMLaunchCommand ),

	setAttributes( State, [ { controller_mbox, undefined },
							{ worker_mboxes, undefined } ] ).




% Returns a textual description of this binding agent.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	ControlString = case ?getAttr(controller_mbox) of

		undefined ->
			"not knowing a controller mbox";

		ContPid ->
			text_utils:format( "knowing controller mailbox ~w", [ ContPid ] )


	end,

	WorkerString = case ?getAttr(worker_mboxes) of

		undefined ->
			"not knowing worker mailboxes";

		WorkPids ->
			text_utils:format( "knowing ~B worker mailboxes, ~w",
							   [ length( WorkPids ), WorkPids ] )

	end,

	ManagerString = text_utils:format( "linked to the binding manager ~w",
									   [ ?getAttr(binding_manager_pid) ] ),

	text_utils:format( "Java binding agent for node '~ts', ~ts, ~ts, ~ts",
		[ node(), ControlString, WorkerString, ManagerString ] ).
