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


-module(class_JavaBindingManager).


-define( class_description,
		 "Class defining the overall, unique manager of all Java Virtual "
		 "Machines spawned by the engine, each of them being driven by its "
		 "node-local JavaBindingAgent."
		 "As all language binding managers, it is expected to run on the "
		 "user node, while each of its agents (instances of "
		 "class_JavaBindingAgent) is to run on the computing node that it "
		 "drives."
		 "More precisely each computing node is to host its own Jinterface "
		 "OtpNode, itself hosting OtpMailboxes (one controller, and a number "
		 "of worker ones), each of them being roughly equivalent to an "
		 "Erlang process." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_LanguageBindingManager ] ).


% The class-specific attributes:
-define( class_attributes, [

	{ controller_table, table( agent_pid(), controller_mbox_pid() ),
	  "allows to associate to a given binding agent PID the one of the "
	  "corresponding controller mailbox; this allows, from an actor PID, "
	  "to determine its node, then its Java binding agent, then the "
	  "associated controller mailbox (useful for example to co-allocated "
	  "actors)" },

	{ worker_table, table( agent_pid(), [ worker_mbox_pid() ] ),
	  "allows to associate to a given binding agent PID the ones of the "
	  "corresponding worker mailboxes; this allows to evaluate actors more "
	  "efficiently (locally to a current node and with some balancing)" },

	{ waited_agents, [ agent_pid() ],
	  "a list of the Java binding agents currently waited for" } ] ).



-type manager_pid() :: class_LanguageBindingManager:manager_pid().

-export_type([ manager_pid/0 ]).


% Helpers:
-export([ to_string/1 ]).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Deployment.JavaBinding.BindingManager" ).


% For registration:
-define( java_binding_manager_name, sim_diasca_java_binding_manager ).


% Allows to use macros for trace sending:
-include_lib("traces/include/traces.hrl").


% Shorthands:

-type controller_mbox_pid() :: class_JavaBindingAgent:controller_mbox_pid().
-type worker_mbox_pid() :: class_JavaBindingAgent:worker_mbox_pid().
-type agent_pid() :: class_JavaBindingAgent:agent_pid().

-type directory_path() :: file_utils:directory_path().

-type atom_node_name() :: net_utils:atom_node_name().
-type tcp_port() :: net_utils:tcp_port().

-type code_path() :: code_utils:code_path().



% Implementation notes:
%
% For the communication between Erlang and Java, Jinterface is relied upon
% (http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html). Of course
% each Java-based actor will have its own state, managed by its reference worker
% mailbox.
%
% Note that:
%
% - the EPMD daemon is expected to be already running
%
% - there is no way (and probably no need) to specify a TCP port range for Java
% nodes (direct pipe-like communication), unlike standard (Erlang) nodes
%
% For Java, the runtime containers stored in the node table are the
% JavaBindingAgent instances (ex: not the controller mailboxes, which are kept
% in the controller_table)




% Constructs a new manager of Java resources (resource manager), from:
%
% - ComputingNodes, a list of the computing nodes on each of which a JVM is to
% be created
%
% - EngineRootDir, the root directory in which the engine is located, on the
% user node
%
% - EpmdPort, the TCP port of the EPMD daemon to rely on (if any)
%
% - ClassPath, the (Java) classpath specified to each launched JVM in order to
% locate user-specific class files
%
% - DeploymentManagerPid, the PID of the deployment manager
%
-spec construct( wooper:state(), [ atom_node_name() ], directory_path(),
		maybe( tcp_port() ), code_path(),
		class_DeploymentManager:manager_pid() ) -> wooper:state().
construct( State, ComputingNodes, EngineRootDir, EpmdPort, ClassPath,
		   DeploymentManagerPid ) ->

	% First the direct mother class:
	LangState = class_LanguageBindingManager:construct( State,
		   ?trace_categorize("JavaBindingManager"), EngineRootDir, EpmdPort,
		   ClassPath, DeploymentManagerPid ),

	% Any language-specific binding manager might be registered that way:
	% (enforces uniqueness, and provides global access)
	%
	naming_utils:register_as( ?java_binding_manager_name, global_only ),

	?send_notice_fmt( LangState, "Creating the binding manager of ~B Java "
		"OtpNodes, running on the following computing nodes: ~ts using "
		"EPMD port ~B and following user-specified classpath: ~ts",
		[ length( ComputingNodes ),
		  text_utils:atoms_to_string( ComputingNodes ), EpmdPort,
		  code_utils:code_path_to_string( ClassPath ) ] ),

	% Rushing now the parallel, longer JVM creations, returning a state:
	initialise_java_nodes( ComputingNodes, EpmdPort, ClassPath, LangState ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	NodeTable = ?getAttr(node_table),

	% The Java binding agents associated to the computing nodes:
	BindingAgents = table:values( NodeTable ),

	% Asynchronous deletions, not done by the mother class; wipes as well the
	% controller mailboxes:
	%
	[ AgentPid ! delete || AgentPid <- BindingAgents ],

	setAttribute( State, node_table, undefined ).



% Methods section.


% Notifies this manager of the PID of the controller mailbox for the specified
% computing node.
%
-spec notifyNodeMailboxes( wooper:state(), controller_mbox_pid(),
			[ worker_mbox_pid() ], agent_pid() ) -> oneway_return().
notifyNodeMailboxes( State, ControllerMailboxPid, WorkerMailboxPids,
					 BindingAgentPid ) ->

	NewControlTable = table:add_new_entry( BindingAgentPid,
						ControllerMailboxPid, ?getAttr(controller_table) ),

	NewWorkerTable = table:add_new_entry( BindingAgentPid, WorkerMailboxPids,
										 ?getAttr(worker_table) ),

	NewWaited = list_utils:delete_existing( BindingAgentPid,
											?getAttr(waited_agents) ),

	case NewWaited of

		[] ->
			?getAttr(deployment_manager_pid) ! { notifyBindingManagerReady,
												 self() };

		StillOthersWaited ->
			StillOthersWaited

	end,

	NotifiedState = setAttributes( State, [
						{ controller_table, NewControlTable },
						{ worker_table, NewWorkerTable },
						{ waited_agents, NewWaited } ] ),

	wooper:return_state( NotifiedState ).



% Returns the PID of the controller mailbox running on the Java Virtual Machine
% associated to the specified sender of this request.
%
% In practice this controller mailbox is the one existing in the Java VM running
% on the same node as the request sender (not specifying any particular thread
% thereof), to lighten the load induced by their exchanges.
%
-spec getAssociatedControllerMailbox( wooper:state() ) ->
			const_request_return( controller_mbox_pid() ).
getAssociatedControllerMailbox( State ) ->

	SenderPid = ?getSender(),

	% Getting first from our node table the PID of the node-local Java binding
	% agent:
	%
	JavaAgentPid = executeConstRequest( getAssociatedRuntimeContainer,
										[ SenderPid ] ),

	% Then obtaining from it the PID of the controller mailbox of that node:
	ControllerMboxPid =
		table:get_value( JavaAgentPid, ?getAttr(controller_table) ),

	wooper:const_return_result( ControllerMboxPid ).



% Returns the PID corresponding to the mailbox of a worker (anyone of them)
% running on the (local) JVM, based on the specified Java binding manager.
%
% In practice this worker mailbox is one existing in the Java VM running on the
% same node as the sender (not specifying any particular thread thereof), to
% lighten the load induced by their exchanges.
%
-spec getAnyAssociatedWorkerMailbox( wooper:state() ) ->
			const_request_return( worker_mbox_pid() ).
getAnyAssociatedWorkerMailbox( State ) ->

	SenderPid = ?getSender(),

	% Getting first from our node table the PID of the node-local Java binding
	% agent:
	%
	JavaAgentPid = executeConstRequest( State,
							getAssociatedRuntimeContainer, [ SenderPid ] ),

	% Then obtaining from it the PID of all worker mailboxes of that node:
	AllWorkerMboxPids = table:get_value( JavaAgentPid,
										 ?getAttr(worker_table) ),

	% May not be reproducible, but should not matter:
	WorkerMboxPid = list_utils:draw_element( AllWorkerMboxPids ),

	?debug_fmt( "Associating worker mailbox ~w to requester ~w.",
				[ WorkerMboxPid, SenderPid ] ),

	wooper:const_return_result( WorkerMboxPid ).



% Static section.


% Returns the atom corresponding to the name the Java binding manager should be
% registered as.
%
% Note: executed on the caller node.
%
-spec get_registration_name() ->
							static_return( naming_utils:registration_name() ).
get_registration_name() ->
	% Ex: 'sim_diasca_java_binding_manager':
	wooper:return_static( ?java_binding_manager_name ).



% Returns the PID of the (unique) Java binding manager.
%
% To be used by clients of the Java binding manager.
%
-spec get_registered_manager() -> static_return( 'none' | manager_pid() ).
get_registered_manager() ->

	case naming_utils:is_registered( ?java_binding_manager_name, global ) of

		not_registered ->
			wooper:return_static( none );

		Pid ->
			wooper:return_static( Pid )

	end.



% Returns the PID corresponding to the controller mailbox of the (local) JVM,
% based on the specified Java binding manager.
%
% To be used by clients of this Java binding manager.
%
-spec get_controller_mbox( manager_pid() ) ->
				static_return( controller_mbox_pid() ).
get_controller_mbox( JavaBindingManagerPid ) ->

	JavaBindingManagerPid ! { getAssociatedControllerMailbox, [], self() },

	receive

		{ wooper_result, MboxPid } when is_pid( MboxPid ) ->
			wooper:return_static( MboxPid )

	end.



% Returns the PID corresponding to the mailbox of a worker (anyone of them)
% running on the (local) JVM, based on the specified Java binding manager.
%
% To be used by clients of this Java binding manager.
%
-spec get_any_worker_mailbox( manager_pid() ) ->
					static_return( worker_mbox_pid() ).
get_any_worker_mailbox( JavaBindingManagerPid ) ->

	JavaBindingManagerPid ! { getAnyAssociatedWorkerMailbox, [], self() },

	receive

		{ wooper_result, MboxPid } when is_pid( MboxPid ) ->
			wooper:return_static( MboxPid )

	end.




% Helpers section.


% Initialises the per-computing node Java runtime containers, i.e. JVMs,
% a.k.a. OtpNodes.
%
-spec initialise_java_nodes( [ atom_node_name() ], tcp_port(), code_path(),
							 wooper:state() ) -> wooper:state().
initialise_java_nodes( ComputingNodes, EpmdPort, ClassPath, State ) ->

	?debug_fmt( "Creating a Java binding agent on each of the ~B "
		"computing nodes: ~ts", [ length( ComputingNodes ),
					text_utils:atoms_to_string( ComputingNodes ) ] ),

	% Preliminary test that the Java launcher is available, to avoid a later
	% possible error:

	BindingPath = file_utils:join( ?getAttr(engine_root_dir),
		class_JavaBindingAgent:get_engine_relative_binding_path() ),

	BindingClassFilename = class_JavaBindingAgent:get_binding_class_filename(),

	BindingAbsFilename = file_utils:join( BindingPath, BindingClassFilename ),

	case file_utils:is_existing_file( BindingAbsFilename ) of

		true ->
			ok;

		false ->
			?error_fmt( "The implementation of the Java binding class '~ts' "
				"is not found, whereas it was expected to be available "
				"from '~ts'. Has the USE_JAVA_BINDING make variable been set "
				"to 'true' by your project?",
				[ class_JavaBindingAgent:get_binding_classname(),
				  BindingPath ] ),

			throw( { no_java_binding_class_found, BindingAbsFilename } )

	end,

	NodePidPairs = [ { Node,
					   class_JavaBindingAgent:remote_synchronous_timed_new_link(
						   Node, EpmdPort, ClassPath, self() ) }
					 || Node <- ComputingNodes ],

	% Storing, for each computing node, the PID of its Java binding agent,
	% knowing that controller mailboxes will be notified later, once their
	% handshake will be done:

	NodeTable = table:new( NodePidPairs ),

	% Each of these agents is expected to send back, when operational, a
	% notifyNodeMailboxes message:
	%
	WaitedAgents = [ AgentPid || { _Node, AgentPid } <- NodePidPairs ],

	EmptyTable = table:new(),

	setAttributes( State, [ { node_table, NodeTable },
							{ controller_table, EmptyTable },
							{ worker_table,EmptyTable },
							{ waited_agents, WaitedAgents } ] ).



% Returns a textual description of this manager.
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	MotherString = class_LanguageBindingManager:to_string( State ),

	ControlPairs = table:enumerate( ?getAttr(controller_table) ),

	ControlStrings = [ text_utils:format(
						 "for agent ~w, controller mailbox is ~w",
						 [ AgentPid, CtlMboxPid ] )
					   || { AgentPid, CtlMboxPid } <- ControlPairs ],

	WorkerPairs = table:enumerate( ?getAttr(worker_table) ),

	WorkerStrings = [ text_utils:format(
						"for agent ~w, worker mailboxes are ~w",
						 [ AgentPid, WorkerMboxPids ] )
					   || { AgentPid, WorkerMboxPids } <- WorkerPairs ],

	text_utils:format( "Java ~ts~nFollowing controller mailboxes are "
		"associated to their corresponding Java binding "
		"agent: ~ts~nAs for worker mailboxes: ~ts",
		[ MotherString,
		  text_utils:strings_to_string( ControlStrings ),
		  text_utils:strings_to_string( WorkerStrings ) ] ).
