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

% Author: Robin Huart (robin-externe.huart@edf.fr)


-module(class_PythonBindingManager).


-define( class_description,
		 "Class in charge of managing a set of Python interpreters, to be "
		 "used as binding containers for simulation actors that rely on "
		 "Python-based code. "
		 "Several interpreters may be used in order to dispatch processing "
		 "load and memory consumption in a distributed way. "
		 "This class is meant to be a simulation-level singleton, a service "
		 "running on the user node that creates one Python interpreter per "
		 "computing node and federates them all." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_LanguageBindingManager ] ).


% The class-specific attributes:
-define( class_attributes, [

			% None (node_table is inherited).

						   ] ).


-type manager_pid() :: class_LanguageBindingManager:manager_pid().

-export_type([ manager_pid/0 ]).


% Helpers:
-export([ to_string/1 ]).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Deployment.PythonBinding.BindingManager" ).


% For registration:
-define( python_binding_manager_name, sim_diasca_python_binding_manager ).


% Allows to use macros for trace sending:
-include_lib("traces/include/traces.hrl").


% Shorthands:

-type code_path() :: code_utils:code_path().
-type atom_node_name() :: net_utils:atom_node_name().


% Implementation notes:
%
% For the communication between Erlang and Python, ErlPort (http://erlport.org/;
% source in https://github.com/hdima/erlport) is relied upon. Of course each
% interpreter will have its own, private state, hosting a number of Python
% instances corresponding to actors.
%
% Even if this Python binding relies on a specific Erlang node to be created
% (ex: like for C or for Java), it does not specifically need to know the port
% of the EPMD daemon (direct pipe-like communication).


% Often, on a given host (typically a computing one), multiple generations of
% Python coexist (Python 2 or 3), as well as multiple versions thereof (ex:
% 3.5.2, 3.6.3, etc.).
%
% We leave the choice of the actual Python version to be used up to the
% developer, who can select it by creating a symbolic link named
% 'python-for-sim-diasca' that should point to the Python version of interest
% and be found from the default user PATH (ex: one may create such a symbolic
% link in a ~/Software/bin that would be declared in the PATH environment
% variable.
%
% Ex:
% $ mkdir -p ~/Software/bin
% $ cd ~/Software/bin
%
% $ ln -s ~/Software/Python/Python-3.6.3-current-install/bin/python3.6
%  python-for-sim-diasca
%   or
% $ ln -s /usr/bin/python3 python-for-sim-diasca
%
% Then update ~/.bashrc with:
% export PATH=~/Software/bin:${PATH}
%
% Should no such 'python-for-sim-diasca' executable be found, an error will be
% reported. Maybe in the future a warning will be issued and the default
% 'python' command will be used, hoping for the best.



% Constructs a new manager of a set of Python interpreters, from:
%
% - ComputingNodes, a list of the computing nodes on each of which a Python
% interpreter is to be created
%
% - EngineRootDir, the root directory in which the engine is located, on the
% user node
%
% - EpmdPort, the TCP port of the EPMD daemon to rely on (not used by this
% binding)
%
% - CodePath, the Python code path specified to each launched Python interpreter
% in order to locate user-specific modules
%
% - DeploymentManagerPid, the PID of the deployment manager
%
-spec construct( wooper:state(), [ atom_node_name() ],
		file_utils:directory_name(), maybe( net_utils:tcp_port() ),
		code_path(), class_DeploymentManager:manager_pid() ) -> wooper:state().
construct( State, ComputingNodes, EngineRootDir, EpmdPort, CodePath,
		   DeploymentManagerPid ) ->

	% First the direct mother class:
	LangState = class_LanguageBindingManager:construct( State,
		  ?trace_categorize("PythonBindingManager"), EngineRootDir,
		  EpmdPort, CodePath, DeploymentManagerPid ),

	% Any language-specific binding manager might be registered that way:
	% (enforces uniqueness, and provides global access)
	%
	naming_utils:register_as( ?python_binding_manager_name, global_only ),

	?send_notice_fmt( LangState, "Creating the binding manager of ~B "
		"Python interpreters, running on the following computing nodes: ~ts "
		"and using following user-defined Python code path: ~ts",
		[ length( ComputingNodes ),
		  text_utils:atoms_to_string( ComputingNodes ),
		  code_utils:code_path_to_string( CodePath ) ] ),

	% Rushing now the parallel, longer interpreter creations, returning a state:
	initialise_interpreters( ComputingNodes, CodePath, LangState ).





% Launches and initializes a Python interpreter on each of the specified nodes.
-spec initialise_interpreters( [ atom_node_name() ], code_path(),
			wooper:state() ) -> class_LanguageBindingManager:node_table().
initialise_interpreters( TargetNodes, CodePath, State ) ->

	% Following options allow to customize the Python interpreters and the way
	% ErlPort communicates with them:
	%  - compression level (for optimisation tests)
	%  - which 'python' executable is to be used (as Python 2 might be the
	%    default one found)
	%  - paths to append to Python's code path (in sys.path)

	RootDir = class_DeploymentManager:determine_root_directory(),

	% Internal engine needs:
	PythonAPIPath = file_utils:join( [ RootDir, "sim-diasca", "src", "core",
					"services", "dataflow", "bindings", "python", "api" ] ),

	% Useful to find modules and packages from the case directory:
	WorkingDir = file_utils:get_current_directory(),

	NodeCount = length( TargetNodes ),

	% User-defined paths first:
	PythonSpecifiedCodePath = CodePath ++ [ WorkingDir, PythonAPIPath ],

	ErlportStartOptions = [ { compressed, 0 },
							% Could be: /bin/env/python, python3, etc.:
							{ python, "python-for-sim-diasca" },
							{ python_path, PythonSpecifiedCodePath } ],

	?debug_fmt( "Starting ~B Python interpreters, with specified code path: ~ts"
		"~nFull options retained:~n~p",
		[ NodeCount, code_utils:code_path_to_string( PythonSpecifiedCodePath ),
		  ErlportStartOptions ] ),


	% Starts one interpreter per specified node, and populates the inherited
	% node table with them; launching an interpreter is long, it is thus done in
	% parallel:
	%
	% (for an unknown reason, python:start_link/1 shall not be used - at least
	% not with a rpc call, as this leads to a freeze and to an interpreter that
	% is crashed or not even launched)
	%
	{ Res, FailedNodes } = rpc:multicall( TargetNodes, python, start,
										  [ ErlportStartOptions ] ),

	case FailedNodes of

		[] ->
			?debug_fmt( "A Python interpreter has been started on all ~B nodes",
						[ NodeCount ] );

		_ ->
			?error_fmt( "Following ~B nodes (over ~B) failed during "
				"interpreter initializations: ~ts",
				[ length( FailedNodes ), NodeCount,
				  text_utils:strings_to_string( FailedNodes ) ] ),
			throw( { failed_nodes, python_initialization, FailedNodes } )

	end,

	% Res is a (supposedly ordered) list of per-node results, being the ones
	% of python:start/1:

	% We take advantage of this pass to link this manager to each interpreter
	% (as start_link could not be used above):
	%
	InterpreterPids = case lists:foldl(
						fun

						  ( { ok, IntPid }, _Acc={ AccPid, AccError } ) ->
							  % As early as possible, but later than hoped:
							  erlang:link( IntPid ),
							  { [ IntPid | AccPid ], AccError };

						  ( { error, Error }, _Acc={ AccPid, AccError } ) ->
								{ AccPid, [ Error | AccError ] };

						  ( Unexpected, _Acc ) ->
								throw( { unexpected_launch_outcome,
										 Unexpected } )

						end,
						_Acc0={ [], [] },
						_List=Res ) of

		{ PidList, _Errors=[] } ->
			PidList;


		{ _PidList, [ { invalid_option,
				{ python, LinkName="python-for-sim-diasca" }, not_found } ] } ->

			PATHVarName = "PATH",

			PATHVarValue = system_utils:get_environment_variable( PATHVarName ),

			?error_fmt( "Unable to select a proper version of Python: "
				"no '~ts' symbolic link found in the '~ts' environment "
				"variable, whose value is: '~ts'.~n"
				"Please refer to the installation instructions in the "
				"Sim-Diasca Technical Manual.",
				[ LinkName, PATHVarName, PATHVarValue ] ),

			throw( { python_selection_link_not_found, LinkName,
					 PATHVarValue } );


		{ _PidList, Errors } ->

			% When an error occurs, not sure we can relate it to a given node,
			% as it is unclear whether the rpc:multicall put outcomes in the
			% order of the specified target nodes (probably yes, though)

			ErrorStrings = [ text_utils:format( "~p", [ E ] ) || E <- Errors ],

			?error_fmt( "Following ~B error(s) occurred during the "
				"initialization of the Python interpreter(s): ~ts",
				[ length( Errors ),
				  text_utils:strings_to_string( ErrorStrings ) ] ),

			throw( { failed_interpreter_initializations, Errors } )

	end,

	InterpreterCount = length( InterpreterPids ),

	% Just a check:
	NodeCount = InterpreterCount,

	NodePairs = lists:zip( TargetNodes, InterpreterPids ),

	% Filling and checking the node table:
	FilledNodeTable = lists:foldl(
		fun( { Node, InterpreterPid }, AccTable ) ->
			% Checking:
			%Node = node( InterpreterPid ),
			table:add_new_entry( Node, InterpreterPid, AccTable )
		end,
		_FillAcc=table:new(),
		_FillList=NodePairs ),

	?debug_fmt( "~B interpreters (~w) successfully spawned.",
				[ InterpreterCount, InterpreterPids ] ),

	% Initializes (sequentially) the binding-induced states of all interpreters,
	% using our binding_input.py module for that, and obtains in the same
	% operation their Python version, current directory and code paths:
	%
	PythonVersionCodePaths = [ python:call( Pid, 'common.erlang_binding_entry',
											init_binding, [ self() ] )
							   || Pid <- InterpreterPids ],

	NodeAndVersionPathPairs = lists:zip( TargetNodes, PythonVersionCodePaths ),

	InterpreterStrings = [
	  interpret_python_settings( Node, Version, CurrentDir, ActualLocalPath )
						  || { Node,
		{ Version, CurrentDir, ActualLocalPath } } <- NodeAndVersionPathPairs ],

	% Check:
	InterpreterCount = length( InterpreterStrings ),

	?debug_fmt( "Information about the ~B Python interpreters used: ~ts",
		[ InterpreterCount,
		  text_utils:strings_to_string( InterpreterStrings ) ] ),

	?getAttr(deployment_manager_pid) ! { notifyBindingManagerReady, self() },

	% Final state returned:
	setAttribute( State, node_table, FilledNodeTable ).



% (helper)
interpret_python_settings( Node, Version, CurrentDir, CodePath ) ->

	% An empty string directs a Python interpreter to search modules in the
	% current directory first (see
	% https://docs.python.org/3.4/library/sys.html#sys.path), so we may comment
	% the first entry:
	%
	DescribedPath = case CodePath of

		[ "" | T ] ->
			[ text_utils:format( "search in current directory (~ts) first",
								 [ CurrentDir ] ) | T ];
		Other ->
			Other

	end,

	text_utils:format( "for computing node '~ts', using Python version ~ts, "
		"from current directory '~ts', with following code path: ~ts",
		[ Node, Version, CurrentDir, text_utils:strings_to_enumerated_string(
									   DescribedPath, _IndentationLevel=1 ) ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Stopping all interpreters:
	case ?getAttr(node_table) of

		undefined ->
			State;

		NodeTable ->

			case table:values( NodeTable ) of

				[] ->
					?info( "Manager destructed (no Python interpreter "
							"was registered)." ),
					State;

				InterpreterPids ->

					?info_fmt( "Stopping ~B Python interpreters "
						"(corresponding to following runtime containers: ~w).",
						[ length( InterpreterPids ), InterpreterPids ] ),

					% python:stop/1 returns 'ok' in all cases:
					[ python:stop( IntPid ) || IntPid <- InterpreterPids ],

					?info( "Manager destructed." ),

					setAttribute( State, node_table, undefined )

			end

	end.




% Methods section.


% Returns the Python interpreter associated to the sender of this request.
%
% In practice the returned binding container is the Python interpreter running
% on the same (computing) host as the request sender, to lighten the load
% induced by their exchanges.
%
-spec getAssociatedPythonInterpreter( wooper:state() ) ->
	const_request_return( language_utils:python_interpreter_container_pid() ).
getAssociatedPythonInterpreter( State ) ->

	SenderPid = ?getSender(),

	% This inherited method is just fine:
	InterpreterPid = executeConstRequest( State, getAssociatedRuntimeContainer,
										  [ SenderPid ] ),

	wooper:const_return_result( InterpreterPid ).





% Static section.



% Returns the atom corresponding to the name the Python binding manager should
% be registered as.
%
% Note: executed on the caller node.
%
-spec get_registration_name() ->
							static_return( naming_utils:registration_name() ).
get_registration_name() ->
	wooper:return_static( ?python_binding_manager_name ).



% Returns the PID of the (unique) Python binding manager.
%
% To be used by clients of the Python binding manager.
%
-spec get_registered_manager() -> static_return( manager_pid() ).
get_registered_manager() ->

	ManagerName = get_registration_name(),

	% No waiting performed, as expected to have been synchronously created:
	% try naming_utils:wait_for_global_registration_of( ManagerName )...

	case naming_utils:is_registered( ManagerName, _Scope=global ) of

		not_registered ->
			?notify_error( "No Python binding manager registered, whereas its "
				"availability has been requested; maybe it has not "
				"been listed in the 'enable_language_bindings' "
				"field of the deployment settings?" ),

			throw( python_binding_manager_not_registered );

		ManagerPid ->
			wooper:return_static( ManagerPid )

	end.




% Returns the PID corresponding to the (local) Python interpreter.
%
% To be used by clients of the Python binding manager.
%
-spec get_interpreter() -> static_return( python_utils:interpreter_pid() ).
get_interpreter() ->
	InterpreterPid = get_interpreter( get_registered_manager() ),
	wooper:return_static( InterpreterPid ).



% Returns the PID corresponding to the (local) Python interpreter, based on the
% specified Python binding manager.
%
% To be used by clients of the Python binding manager.
%
-spec get_interpreter( manager_pid() ) ->
							static_return( python_utils:interpreter_pid() ).
get_interpreter( PythonBindingManagerPid ) ->

	PythonBindingManagerPid ! { getAssociatedPythonInterpreter, [], self() },

	receive

		{ wooper_result, InterpreterPid } when is_pid( InterpreterPid ) ->
			wooper:return_static( InterpreterPid )

	end.




% Helpers section.


% Returns a textual description of this manager.
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	NodePairs = table:enumerate( ?getAttr(node_table) ),

	NodeStrings = [ text_utils:format( "interpreter ~w running on node '~ts'",
		   [ ContainerPid, Node ] ) || { Node, ContainerPid } <- NodePairs ],

	text_utils:format( "Python binding manager federating ~B interpreters: ~ts",
	   [ length( NodeStrings ), text_utils:strings_to_string( NodeStrings ) ] ).
