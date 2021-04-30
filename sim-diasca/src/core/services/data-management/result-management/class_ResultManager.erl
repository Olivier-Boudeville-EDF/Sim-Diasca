% Copyright (C) 2010-2021 EDF R&D

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


-module(class_ResultManager).


-define( class_description,
		 "This service allows to declare, keep track of, retrieve, make "
		 "available, possibly post-process (ex: generating appropriate "
		 "rendering) the results of the simulation. "
		 "These are the subset of the simulation outputs that the user wants "
		 "to obtain from the simulation. "
		 "See also: class_ResultProducer.erl." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).


% The class-specific attributes of a result manager:
-define( class_attributes, [

	{ result_spec, result_specification(),
	  "stores the result settings, after a first early "
	  "validation/transformation pass, for later use; it is either an atom "
	  "or a pair made of two lists (targeted/blacklisted); elements of the "
	  "targeted list are {BinaryPattern, PrecompiledMatchSpec, OptionList} "
	  "triplets, whereas elements of the blacklisted list are "
	  "{ BinaryPatttern, PrecompiledMatchSpec } pairs; for both lists we keep "
	  "the original pattern so that we can know easily which to remove (not "
	  "sure match specs can be reliably compared)" },

	{ result_dir, directory_path(), "the directory in which actual
	  results (such as basic probe outputs) will be stored" },

	{ basic_probe_table, table( bin_probe_name(), basic_probe_entry() ),
	  "table associating, to a basic probe name, its related record "
	  "information, as known of this manager" },

	{ basic_probe_default_options, [ probe_options() ],
	  "a list of default options for basic probes" },

	{ virtual_probe_table,
	  table( bin_probe_name(), virtual_probe_entry() ),
	  "table associating, to a virtual probe name, its related record "
	  "information, as known of this manager" },

	{ datalogger_default_options, [ probe_options() ],
	  "a list of default options for virtual probes" },

	{ web_probe_table, table( bin_probe_name(), web_probe_entry() ),
	  "table associating, to a web probe name, its related record "
	  "information, as known of this manager" },

	{ web_probe_default_options, [ probe_options() ],
	  "a list of default options for web probes" },

	{ pid_to_queue, table( producer_pid(), result_queue() ),
	  "table allowing to convert the PID of a tracked producer into the "
	  "result queue in charge of it" },

	{ result_queues, [ result_queue() ], "stores the queues of pending results "
	  "assigned to computing nodes, to control their load" },

	{ user_node_info, { atom_node_name(), atom_host_name() },
	  "allows to record information about the user node and host, notably so "
	  "that any load induced by result producers created directly from the "
	  "simulation case can be assigned to the relevant host" },

	{ result_collected, boolean(),
	  "tells whether result collection has already been performed" },

	{ result_found, boolean(),
	  "tells whether there was at least one actual collected result" },

	{ listeners, [ result_listener_pid() ], "a list of the PID of the result "
	  "listeners, to be notified of the main result-related events" },

	{ meta_data, meta_data(), "an ordered list of {Key,Value} pairs describing "
	  "result-related metadata, which for example will be sent to each created "
	  "probe (useful to have these information stored in their result files)" },

	{ root_time_manager_pid, time_manager_pid(),
	  "the PID of the root time manager" } ] ).


% Scope at which the result manager should be registered:
-define( registration_scope, global_only ).



% Exported helpers:
-export([ to_string/1, display_queues/1 ]).



% Type section.
%
% See class_TimeManager.hrl for a detailed description of their meaning.


% The name of a probe, as a binary:
-type bin_probe_name() :: bin_string().


% Typically an information transmitted to the web manager:
-type probe_info() :: { bin_probe_name(), maybe( bin_directory_path() ) }.


% Describes a Regex pattern to select results:
-type base_result_pattern() :: text_utils:regex_string().

-type target_pattern() :: base_result_pattern()
		| { base_result_pattern(), producer_options() }.


% Allows to whitelist specific results:
-type targeted_elements() :: { 'targeted_patterns', [ target_pattern() ] }.


% Allows to blacklist specific results:
-type blacklisted_elements() :: [ base_result_pattern() ].


-type selection_pattern() :: targeted_elements() | blacklisted_elements().

% PID of a result manager:
-type manager_pid() :: sim_diasca:agent_pid().


% User-specified result spec:
-type result_specification() :: 'all_outputs'
							  | 'no_output'
							  | 'all_basic_probes_only'
							  | 'all_virtual_probes_only'
							  | 'all_web_probes_only'
							  | [ selection_pattern() ].



% Additional information to be passed to result producers:
-type meta_data() :: option_list:option_list( atom(), bin_string() ).


% Records the result queue corresponding to a computing node, in order to
% control its load when generating the results, and not overload/crash it.
%
-record( result_queue, {

		% The identifier of this queue (a simple counter):
		id :: count(),

		% The name of the computing host this result queue applies to:
		host_name :: atom_host_name(),

		% The name of the corresponding computing node:
		node_name :: atom_node_name(),

		% An evaluation of the maximum number of simultaneous workers (depending
		% notably on the number of available cores) that can be used to generate
		% results on the corresponding node:
		%
		max_worker_count :: count(),

		% A list of the result producers that are currently working:
		waited_producers :: [ producer_pid() ],

		% A list of the names (as binaries) of the result producers whose
		% generation is still pending:
		%
		% (we list names rather than PIDs as the names are (and must be) the
		% keys of the table)
		%
		pending_results :: [ bin_string() ] } ).


-type result_queue() :: #result_queue{}.




% Describes a basic probe, as seen by the result manager.
%
% Such an entry is the value associated to the (binary) probe name, in the probe
% table.
%
-record( basic_probe_entry, {

	% The PID of the corresponding probe:
	probe_pid :: probe_pid(),

	% The generation options for that probe:
	probe_options :: producer_options(),

	% Tells whether this probe is to be tracked as a result:
	is_tracked :: boolean(),

	% The directory in which that probe is to produce content, if not tracked:
	probe_dir :: maybe( bin_directory_path() ) } ).

-type basic_probe_entry() :: #basic_probe_entry{}.



% Describes a web probe, as seen by the result manager.
%
% Such an entry is the value associated to the (binary) probe name, in the web
% probe table.
%
-record( web_probe_entry, {

	% The PID of the corresponding web probe:
	probe_pid :: class_WebProbe:probe_pid(),

	% The generation options for that probe:
	probe_options :: producer_options(),

	% Tells whether this probe is to be tracked as a result:
	is_tracked :: boolean() } ).

-type web_probe_entry() :: #web_probe_entry{}.



-export_type([ manager_pid/0, result_specification/0, meta_data/0,
			   result_queue/0, basic_probe_entry/0, web_probe_entry/0,
			   probe_info/0 ]).



% For result_manager_name:
-include("class_ResultManager.hrl").


% For instance_tracker_name:
-include("class_InstanceTracker.hrl").


-include("engine_common_defines.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.ResultManagement" ).


% For app_info*:
-include_lib("traces/include/traces.hrl").

% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").



% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type directory_path() :: file_utils:directory_path().
-type bin_directory_path() :: bin_directory_path().
-type directory_name() :: file_utils:directory_name().

-type atom_host_name() :: net_utils:atom_host_name().
-type atom_node_name() :: net_utils:atom_node_name().

-type bin_producer_name() :: class_ResultProducer:bin_producer_name().
-type producer_pid() :: class_ResultProducer:producer_pid().
-type producer_options() :: class_ResultProducer:producer_options().
-type producer_nature() :: class_ResultProducer:producer_nature().

-type result_listener_pid() :: sim_diasca:agent_pid().



% Implementation notes.


% The process to handle results is the following:
%
% 1. perform basic checks of the result specification given by the user,
% possibly precomputing match specifications (compiled regular expressions)
%
% 2. handle declarations of result producers as they arrive, resulting in
% referencing them or not, potentially disabling them (all depending on their
% matching against the result specification)
%
% 3. on simulation successful termination, retrieve all referenced outputs on
% the user's behalf


% Once patterns are changed, resulting lists could be uniquified.


% The name of an output is ambiguous: for example, it can be either the one of a
% basic probe result, or of a virtual probe result, or of a web probe result.
%
% As a consequence we have to look-up these names in tables maintained for
% different producer types, and ensure that no ambiguity remains.

% So there are tables per producer type (ex: basic_probe_table) that convert its
% name into its information (PID, settings, etc.).


% Flow control had to be added for the result generation, otherwise simulations
% encompassing a large number of probes would end up with thousands of plots to
% be generated at once, resulting in a huge slow-down, the exhaustion of file
% descriptors and/or RAM, or even the crash of the host.
%
% So currently each computing host is loaded as much as reasonably possible
% (depending on its number of cores), pending tasks being requested as current
% tasks are over.

% Note: we reason by hosts rather than nodes, as an host may have for example
% both a computing node and a user node: result producers created from the
% simulation cases should of course be taken into account in the overall load of
% their host.

% This flow control does apply to the datalogger as a whole, which still
% performs its result generation task in a sequential, non-distributed way.

% Finally, non-tracked producers (ex: the probes of the performance tracker) are
% not driven by the result manager (the load they induce should be negligible,
% and they have to be generated whether or not the simulation succeeds).

% There is also a pid_to_queue table, to associate back the PID of a producer to
% the result queue that is in charge of it.


% The basic or web probes that are tracked will be deleted by the result
% manager. The ones that are not tracked are to be specifically managed by their
% creator.





% Constructs a new result manager, from following parameters:
%
% - ResultSpecification allows to specify coarsely what are the outputs to be
% promoted to results
%
% - DataLoggerEnabled tells whether the data-logger is used
%
% - RootTimeManagerPid is the PID of the root time manager; this result manager
% will subscribe to it as a listener, so that it can know when/if the simulation
% ended on success
%
% - SimRunDir is the path (specified as a string) of the directory in which the
% current simulation runs
%
% - ResultBaseDirName is the name (specified as a plain string) of the base
% result directory for the current simulation
%
% - Metadata is an ordered list of meta-data key-value pairs, whose keys are
% atoms, and values are binary strings
%
% See class_TimeManager.erl for further details.
%
-spec construct( wooper:state(), result_specification(), boolean(),
				 time_manager_pid(), directory_path(), directory_name(),
				 meta_data() ) -> wooper:state().
construct( State, ResultSpecification, DataLoggerEnabled,
		   RootTimeManagerPid, SimRunDir, ResultBaseDirName, Metadata ) ->

	% First the direct mother classes:
	TraceState = class_EngineBaseObject:construct( State,
							?trace_categorize("Result Manager") ),

	ProcessedResultSpec =
		check_and_transform_result_specification( ResultSpecification ),

	RegistrationName = get_registration_name(),

	% Result producers rely on it:
	naming_utils:register_as( RegistrationName, ?registration_scope ),

	class_InstanceTracker:register_agent( RegistrationName ),

	% We want to know when/if the simulation terminates on success:
	RootTimeManagerPid ! { addSimulationListener, self() },

	SimResultDirName =
		file_utils:join( ResultBaseDirName, "simulation-results" ),

	EmptyTable = table:new(),

	FinalState = setAttributes( TraceState, [

		{ result_spec, ProcessedResultSpec },

		{ result_dir, SimResultDirName },

		{ simulation_run_dir, SimRunDir },

		{ basic_probe_table, EmptyTable },
		{ basic_probe_default_options, [ data_and_rendering ] },

		{ virtual_probe_table, EmptyTable },
		{ datalogger_default_options, [ rendering_only ] },

		{ web_probe_table, EmptyTable },
		{ web_probe_default_options, [] },

		{ pid_to_queue, EmptyTable },
		{ result_queues, undefined },
		{ user_node_info, undefined },
		{ result_collected, false },
		{ result_found, false },
		{ listeners, [] },
		{ data_logger_enabled, DataLoggerEnabled },
		{ meta_data, Metadata },
		{ root_time_manager_pid, RootTimeManagerPid } ] ),

	% Late sending, to have it rely on the emitter category:
	?send_info_fmt( FinalState, "Constructed, with following result "
		"specification: ~p and following meta-data: ~ts",
		[ ResultSpecification, get_metadata_string( Metadata ) ] ),

	FinalState.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?info( "Deleting result manager." ),

	% Deleting all basic and web probes that are tracked (others are to be
	% managed explicitly by their user):

	BasicProbePidList = [ Pid || #basic_probe_entry{ probe_pid=Pid,
													 is_tracked=true }
							  <- table:values( ?getAttr(basic_probe_table) ) ],

	case BasicProbePidList of

		[] ->
			?debug( "No tracked basic probe to delete." );

		_ ->
			?debug_fmt( "Deleting ~B tracked basic probe(s): ~w.",
						[ length( BasicProbePidList ), BasicProbePidList ] ),

			wooper:delete_synchronously_instances( BasicProbePidList )

	end,


	WebProbePidList = [ Pid || #web_probe_entry{ probe_pid=Pid,
												 is_tracked=true }
							  <- table:values( ?getAttr(web_probe_table) ) ],

	case WebProbePidList of

		[] ->
			?debug( "No tracked web probe to delete." );

		_ ->
			?debug_fmt( "Deleting ~B tracked web probes: ~w.",
						[ length( WebProbePidList ), WebProbePidList ] ),

			wooper:delete_synchronously_instances( WebProbePidList )

	end,

	?getAttr(root_time_manager_pid) ! { removeSimulationListener, self() },

	class_InstanceTracker:unregister_agent(),

	naming_utils:unregister( get_registration_name(), ?registration_scope ),

	% Then allow chaining:
	State.





% Member methods section.


% Notifies this manager of the information about nodes, hosts and their core
% count.
%
% The user node (and host) has to be specified as well, as the result producers
% directly created from the simulation case will end up being processed by the
% same host as a computing node (if any). So any result from the user node will
% have to be assigned to the local computing node instead, not too overload the
% corresponding host.
%
-spec setResourceMapping( wooper:state(),
		[ { atom_host_name(), atom_node_name(), count() } ],
		{ atom_node_name(), atom_host_name() } ) -> oneway_return().
setResourceMapping( State, HostCoreList, UserNodeInfos ) ->

	% Initially there is no pending result producer:
	InitialResultQueues = create_initial_queues( HostCoreList ),

	wooper:return_state( setAttributes( State, [
			{ result_queues, InitialResultQueues },
			{ user_node_info, UserNodeInfos } ] ) ).



% Creates the initial (empty) result queues.
%
% One objective is to ensure that the user host has a queue (and only one), as
% result producers may be created directly from the simulation case.
%
create_initial_queues( HostCoreList ) ->
	create_initial_queues( HostCoreList, _Count=1, _Acc=[] ).


create_initial_queues( _HostCoreList=[], _Count, Acc ) ->
	Acc;

create_initial_queues( _HostCoreList=[ { HostName, NodeName, CoreCount } | T ],
			 Count, Acc ) ->

	 NewQueue = #result_queue{ id=Count,
							   host_name=HostName,
							   node_name=NodeName,

							   % We ensure that each core will be busy enough,
							   % yet not too overloaded:
							   %
							   max_worker_count=2*CoreCount,

							   % No generation triggered yet:
							   waited_producers=[],

							   % No result known yet:
							   pending_results=[] },

	create_initial_queues( T, Count+1, [ NewQueue | Acc ] ).



% Processes the declaration of the specified (basic) probe, whose name is a
% binary here, and whose elements shall be written in specified directory (if
% any).
%
% IsToBeTracked tells whether this manager is to track this probe as the
% producer of actual simulation results (if true) or just as a producer of
% results that are not simulation results, like facility probes (if false).
%
% Returns, depending on the current state of this manager, either
% output_requested or output_not_requested. Note that this may change over time,
% should targeted/blacklisted patterns be used and changed.
%
-spec declareProbe( wooper:state(), bin_probe_name(), boolean(),
					maybe( bin_directory_path() ) ) ->
		request_return( 'output_not_requested' | 'output_requested' ).
declareProbe( State, BinProbeName, IsToBeTracked, MaybeProbeBinDir ) ->

	ProbeName = text_utils:binary_to_string( BinProbeName ),

	?info_fmt( "Declaration of basic probe '~ts' (tracked: ~ts).",
				[ ProbeName, IsToBeTracked ] ),

	case is_result_wanted( BinProbeName, basic_probe, State ) of

		false ->
			% Can thus be fully ignored:
			?debug_fmt( "Basic probe '~ts' is not wanted.", [ ProbeName ] ),
			wooper:const_return_result( output_not_requested );


		{ true, ProbeOptions } ->

			% First, check that the same probe will not be registered twice:
			ProbeTable = ?getAttr(web_probe_table),

			case table:has_entry( _Key=BinProbeName, ProbeTable ) of

				true ->
					throw( { basic_probe_declared_more_than_once, ProbeName } );

				false ->
					ok

			end,

			ActualProbeOptions = case ProbeOptions of

				undefined ->
					?getAttr(basic_probe_default_options);

				Other ->
					Other

			end,

			RegisteredState = register_basic_probe( BinProbeName,
				ActualProbeOptions, IsToBeTracked, MaybeProbeBinDir,
			   _ProbePid=?getSender(), State ),

			wooper:return_state_result( RegisteredState, output_requested )

	end.



% Processes the declaration of the specified web probe, whose name is a binary
% here.
%
% IsToBeTracked tells whether this manager is to track this (web) probe as the
% producer of actual simulation results (if true) or just as a producer of
% results that are not simulation results, like facility (web) probes (if
% false).
%
% Returns, depending on the current state of this manager, either
% output_requested or output_not_requested. Note that this may change over time,
% should targeted/blacklisted patterns be used and changed.
%
-spec declareWebProbe( wooper:state(), bin_probe_name(), boolean() ) ->
		request_return( 'output_not_requested' | 'output_requested' ).
declareWebProbe( State, BinProbeName, IsToBeTracked ) ->

	ProbeName = text_utils:binary_to_string( BinProbeName ),

	?info_fmt( "Declaration of web probe '~ts' (tracked: ~ts).",
				[ ProbeName, IsToBeTracked ] ),

	case is_result_wanted( BinProbeName, web_probe, State ) of

		false ->
			% Can thus be fully ignored:
			wooper:const_return_result( output_not_requested );


		{ true, ProbeOptions } ->

			% First, check that the same probe will not be registered twice:
			ProbeTable = ?getAttr(web_probe_table),

			case table:has_entry( _Key=BinProbeName, ProbeTable ) of

				true ->
					throw( { web_probe_declared_more_than_once, ProbeName } );

				false ->
					ok

			end,

			ActualProbeOptions = case ProbeOptions of

				undefined ->
					?getAttr(basic_probe_default_options);

				Other ->
					Other

			end,

			RegisteredState = register_web_probe( BinProbeName,
				ActualProbeOptions, IsToBeTracked, _ProbePid=?getSender(),
				State ),

			wooper:return_state_result( RegisteredState, output_requested )

	end.



% Registers specified (basic) probe, returns an updated state.
%
% (helper)
%
register_basic_probe( BinProbeName, ProbeOptions, IsToBeTracked,
					  MaybeProbeBinDir, ProbePid, State ) ->

	?debug_fmt( "Registering basic probe '~ts' (~w); to be tracked: ~w.",
				[ BinProbeName, ProbePid, IsToBeTracked ] ),

	ProbeEntry = #basic_probe_entry{ probe_pid=ProbePid,
									 probe_options=ProbeOptions,
									 is_tracked=IsToBeTracked,
									 probe_dir=MaybeProbeBinDir },

	NewProbeTable = table:add_entry( _K=BinProbeName, _V=ProbeEntry,
									 ?getAttr(basic_probe_table) ),

	% Now, takes care of the update of the result queues, if this probe is
	% tracked:
	%
	case IsToBeTracked of

		false ->
			setAttribute( State, basic_probe_table, NewProbeTable );

		true ->
			{ UserNodeName, UserHostAtom } = ?getAttr(user_node_info),

			{ ResultQueue, OtherQueues } = case node( ProbePid ) of

			   UserNodeName ->

					% This probe runs on the user node, hence we must register
					% it in the right result queue (based on the appropriate
					% computing host):
					%
					extract_queue_by_host( UserHostAtom,
										   ?getAttr(result_queues) );

				AComputingNode ->

					extract_queue_by_node( AComputingNode,
										   ?getAttr(result_queues) )

			end,

			% Adds this probe to the pending ones:
			NewResultQueue = ResultQueue#result_queue{

				 pending_results= [ BinProbeName
						  | ResultQueue#result_queue.pending_results ] },

			% Updates also the PID-to-queue table:
			NewPidToQueueTable = table:add_entry( _Key=ProbePid,
				_Value=NewResultQueue#result_queue.id, ?getAttr(pid_to_queue) ),

			setAttributes( State, [
						{ basic_probe_table, NewProbeTable },
						{ result_queues, [ NewResultQueue | OtherQueues ] },
						{ pid_to_queue, NewPidToQueueTable } ] )

	end.



% Registers specified (web) probe, returns an updated state.
%
% (helper)
%
register_web_probe( BinProbeName, ProbeOptions, IsToBeTracked, ProbePid,
					State ) ->

	% Mostly the same as register_basic_probe/5:

	%trace_utils:debug_fmt( "Adding web probe '~ts' (~w); "
	% "is to be tracked: ~ts.", [ BinProbeName, ProbePid, IsToBeTracked ] ),

	ProbeEntry = #web_probe_entry{ probe_pid=ProbePid,
								   probe_options=ProbeOptions,
								   is_tracked=IsToBeTracked },

	NewProbeTable = table:add_entry( _K=BinProbeName, _V=ProbeEntry,
									 ?getAttr(web_probe_table) ),

	% Now, takes care of the update of the result queues, if this probe is
	% tracked:
	%
	case IsToBeTracked of

		false ->
			setAttribute( State, web_probe_table, NewProbeTable );

		true ->
			{ UserNodeName, UserHostAtom } = ?getAttr(user_node_info),

			{ ResultQueue, OtherQueues } = case node( ProbePid ) of

			   UserNodeName ->

					% This probe runs on the user node, hence we must register
					% it in the right result queue (based on the appropriate
					% computing host):
					%
					extract_queue_by_host( UserHostAtom,
										   ?getAttr(result_queues) );

				AComputingNode ->

					extract_queue_by_node( AComputingNode,
										   ?getAttr(result_queues) )

			end,

			% Adds this probe to the pending ones:
			NewResultQueue = ResultQueue#result_queue{

				 pending_results= [ BinProbeName
						  | ResultQueue#result_queue.pending_results ] },

			% Updates also the PID-to-queue table:
			NewPidToQueueTable = table:add_entry( _Key=ProbePid,
				_Value=NewResultQueue#result_queue.id, ?getAttr(pid_to_queue) ),

			setAttributes( State, [
				{ web_probe_table, NewProbeTable },
				{ result_queues, [ NewResultQueue | OtherQueues ] },
				{ pid_to_queue, NewPidToQueueTable } ] )

	end.






% Section for targeted patterns.


% Tells whether the results of the specified producer, specified as a binary
% string, are wanted, i.e. whether they match the result specification.
%
% If not, then they will not be retrieved, thus there is no point in producing
% them anyway, and the corresponding producer should preferably not even be
% created.
%
-spec isResultProducerWanted( wooper:state(), bin_producer_name() ) ->
			const_request_return( 'false' | { 'true', meta_data() } ).
isResultProducerWanted( State, ProducerName ) ->

	% When we do not know the nature (ex: basic, virtual or web probe) of a
	% producer, we consider it is wanted, knowing its results will be correctly
	% managed on simulation success (only drawback: we may allow some producers
	% to unnecessarily exist).

	Res = isResultProducerWanted( State, ProducerName, _Nature=undefined ),

	wooper:const_return_result( Res ).



% Tells whether the outputs of the specified producer, whose name is specified
% as a binary string, are wanted, i.e. whether the name matches the result
% specification.
%
% If not, then these outputs will not be retrieved, thus there is no point in
% producing them anyway, and the corresponding producer should preferably not
% even be created.
%
% (nature is either 'basic_probe', 'virtual_probe', 'web_probe' or 'undefined')
%
-spec isResultProducerWanted( wooper:state(), bin_producer_name(),
							  producer_nature() ) ->
			const_request_return( 'false' | { 'true', meta_data() } ).
isResultProducerWanted( State, ProducerName, Nature ) ->

	%trace_utils:debug_fmt( "isResultProducerWanted for producer '~ts' "
	%  "of nature ~p.", [ ProducerName, Nature ] ),

	Res = case is_result_wanted( ProducerName, Nature, State ) of

		false ->
			false;

		{ true, _Opts } ->
			{ true, ?getAttr(meta_data) }

	end,

	wooper:const_return_result( Res ).



% Tells whether the results of the specified producer, specified as a binary
% string, are wanted, i.e. whether they match the result specification.
%
% If not, then they will not be retrieved, thus there is no point in producing
% them anyway, and the corresponding producer should preferably not even be
% created.
%
% (nature is either 'basic_probe', 'virtual_probe', 'web_probe' or 'undefined').
%
% Returns either false or {true, Metadata, Opts}.
%
-spec isResultProducerWantedWithOptions( wooper:state(), bin_producer_name(),
										 producer_nature() ) ->
			const_request_return( 'false' | { 'true', producer_options() } ).
isResultProducerWantedWithOptions( State, ProducerName, Nature ) ->

	Res = is_result_wanted( ProducerName, Nature, State ),

	%trace_utils:debug_fmt( "isResultProducerWantedWithOptions "
	%   "for producer '~ts' of nature ~p: answer is '~p'.",
	%   [ ProducerName, Nature, Res ] ),

	wooper:const_return_result( Res ).



% Adds specified targeted result pattern, expressed as a plain string, to the
% current result specification, which must be already using targeted/blacklisted
% patterns (and not shortcut atoms).
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
-spec addTargetedPattern( wooper:state(), base_result_pattern() ) ->
								oneway_return().
addTargetedPattern( State, Pattern ) ->

	NewState = case text_utils:is_string( Pattern ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targets, BlackLists } ->
					BinTarget = text_utils:string_to_binary( Pattern ),
					setAttribute( State, result_spec,
								 { [ BinTarget | Targets ], BlackLists } );

				Other ->
					?error_fmt( "Error, no targeted pattern can be added "
						"when relying on an incompatible result "
						"specification (trying to add '~p' to '~p').",
						[ Pattern, Other ] ),
					throw( { target_cannot_be_added, Pattern, Other } )

			end;

		false ->
			throw( { added_target_not_a_string, Pattern } )

	end,

	wooper:return_state( NewState ).



% Adds specified targeted result patterns, expressed as plain strings, to the
% current result specification, which must be already using targeted/blacklisted
% patterns (and not shortcut atoms).
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
-spec addTargetedPatterns( wooper:state(), [ base_result_pattern() ] ) ->
								oneway_return().
addTargetedPatterns( State, Patterns ) ->

	NewState = case text_utils:are_strings( Patterns ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targets, BlackLists } ->
					BinTargets = text_utils:strings_to_binaries( Patterns ),
					setAttribute( State, result_spec,
								 { BinTargets ++ Targets, BlackLists } );

				Other ->
					?error_fmt( "Error, no targeted pattern can be added "
						"when relying on an incompatible result "
						"specification (trying to add '~p' to '~p').",
						[ Patterns, Other ] ),
					throw( { targets_cannot_be_added, Patterns, Other } )

			end;

		false ->
			throw( { added_targets_not_all_strings, Patterns } )

	end,

	wooper:return_state( NewState ).



% Removes specified targeted result pattern, expressed as a plain string, from
% the current result specification, which must be already using
% targeted/blacklisted patterns (and not shortcut atoms).
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
-spec removeTargetedPattern( wooper:state(), base_result_pattern() ) ->
									oneway_return().
removeTargetedPattern( State, Pattern ) ->

	NewState = case text_utils:is_string( Pattern ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targets, BlackLists } ->
					BinTarget = text_utils:string_to_binary( Pattern ),
					% Supposed to be there only once:
					NewTargets = lists:delete( BinTarget, Targets ),
					setAttribute( State, result_spec,
								  { NewTargets, BlackLists } );

				Other ->
					?error_fmt( "Error, no targeted pattern can be removed "
						"when relying on an incompatible result specification "
						"specification (trying to remove '~p' from '~p').",
						[ Pattern, Other ] ),
					throw( { target_cannot_be_removed, Pattern, Other } )

			end;

		false ->
			throw( { removed_target_not_a_string, Pattern } )

	end,

	wooper:return_state( NewState ).



% Removes specified targeted result patterns, expressed as plain strings, from
% the current result specification, which must be already using
% targeted/blacklisted patterns (and not shortcut atoms).
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
-spec removeTargetedPatterns( wooper:state(), [ base_result_pattern() ] ) ->
									oneway_return().
removeTargetedPatterns( State, Patterns ) ->

	NewState = case text_utils:are_strings( Patterns ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targets, BlackLists } ->
					BinTargets = text_utils:strings_to_binaries( Patterns ),
					NewTargets = lists:subtract( Targets, BinTargets ),
					setAttribute( State, result_spec,
								  { NewTargets, BlackLists } );

				Other ->
					?error_fmt( "Error, no targeted pattern can be added "
						"when relying on an incompatible result "
						"specification (trying to add '~p' to '~p').",
						[ Patterns, Other ] ),
					throw( { targets_cannot_be_removed, Patterns, Other } )

			end;

		false ->
			throw( { removed_targets_not_all_strings, Patterns } )

	end,

	wooper:return_state( NewState ).



% Replaces the current targeted result patterns by the specified ones, expressed
% as plain strings, in the current result specification, which must be already
% using targeted/blacklisted patterns (and not shortcut atoms).
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
-spec setTargetedPatterns( wooper:state(), [ base_result_pattern() ] ) ->
									oneway_return().
setTargetedPatterns( State, NewPatterns ) ->

	NewState = case text_utils:are_strings( NewPatterns ) of

		true ->

			case ?getAttr(result_spec) of

				{ _Targets, BlackLists } ->
					BinTargets = text_utils:strings_to_binaries( NewPatterns ),
					setAttribute( State, result_spec,
								  { BinTargets, BlackLists } );

				Other ->
					?error_fmt( "Error, targeted patterns cannot be set "
						"when relying on an incompatible result "
						"specification (trying to set '~p' in '~p').",
						[ NewPatterns, Other ] ),
					throw( { targets_cannot_be_set, NewPatterns, Other } )

			end;

		false ->
			throw( { removed_targets_not_all_strings, NewPatterns } )

	end,

	wooper:return_state( NewState ).




% Section for blacklisted patterns.


% Adds specified blacklisted result pattern, expressed as a plain string, to the
% current result specification, which must be already using targeted/blacklisted
% patterns (and not shortcut atoms).
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
-spec addBlacklistedPattern( wooper:state(), base_result_pattern() ) ->
									oneway_return().
addBlacklistedPattern( State, Pattern ) ->

	NewState = case text_utils:is_string( Pattern ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targetlists, BlackLists } ->
					BinBlacklist = text_utils:string_to_binary( Pattern ),
					setAttribute( State, result_spec,
							{ Targetlists, [ BinBlacklist | BlackLists ] } );

				Other ->
					?error_fmt( "Error, no blacklisted pattern can be added "
						"when relying on an incompatible result "
						"specification (trying to add '~p' to '~p').",
						[ Pattern, Other ] ),
					throw( { blacklist_cannot_be_added, Pattern, Other } )

			end;

		false ->
			throw( { added_blacklist_not_a_string, Pattern } )

	end,

	wooper:return_state( NewState ).



% Adds specified blacklisted result patterns, expressed as plain strings, to the
% current result specification, which must be already using targeted/blacklisted
% patterns (and not shortcut atoms).
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
-spec addBlacklistedPatterns( wooper:state(), [ base_result_pattern() ] ) ->
									oneway_return().
addBlacklistedPatterns( State, Patterns ) ->

	NewState = case text_utils:are_strings( Patterns ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targetlists, BlackLists } ->
					BinBlacklists = text_utils:strings_to_binaries( Patterns ),
					setAttribute( State, result_spec,
						 { Targetlists, BinBlacklists ++ BlackLists } );

				Other ->
					?error_fmt( "Error, no blacklisted pattern can be added "
						"when relying on an incompatible result "
						"specification (trying to add '~p' to '~p').",
						[ Patterns, Other ] ),
					throw( { blacklists_cannot_be_added, Patterns, Other } )

			end;

		false ->
			throw( { added_blacklists_not_all_strings, Patterns } )

	end,

	wooper:return_state( NewState ).



% Removes specified blacklisted result pattern, expressed as a plain string,
% from the current result specification, which must be already using
% targeted/blacklisted patterns (and not shortcut atoms).
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
-spec removeBlacklistedPattern( wooper:state(), base_result_pattern() ) ->
										oneway_return().
removeBlacklistedPattern( State, Pattern ) ->

	NewState = case text_utils:is_string( Pattern ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targetlists, BlackLists } ->
					BinBlacklist = text_utils:string_to_binary( Pattern ),
					% Supposed to be there only once:
					NewBlacklists = lists:delete( BinBlacklist, BlackLists ),
					setAttribute( State, result_spec,
								  { Targetlists, NewBlacklists } );

				Other ->
					?error_fmt( "Error, no blacklisted pattern can be removed "
						"when relying on an incompatible result "
						"specification (trying to remove "
						"'~p' from '~p').", [ Pattern, Other ] ),
					throw( { blacklist_cannot_be_removed, Pattern, Other } )

			end;

		false ->
			throw( { removed_blacklist_not_a_string, Pattern } )

	end,

	wooper:return_state( NewState ).



% Removes specified blacklisted result patterns, expressed as plain strings,
% from the current result specification, which must be already using
% targeted/blacklisted patterns (and not shortcut atoms).
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
-spec removeBlacklistedPatterns( wooper:state(), [ base_result_pattern() ] ) ->
										oneway_return().
removeBlacklistedPatterns( State, Patterns ) ->

	NewState = case text_utils:are_strings( Patterns ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targetlists, BlackLists } ->
					BinBlacklists = text_utils:strings_to_binaries( Patterns ),
					NewBlacklists = lists:subtract( BlackLists, BinBlacklists ),
					setAttribute( State, result_spec,
								  { Targetlists, NewBlacklists } );

				Other ->
					?error_fmt( "Error, no blacklisted pattern can be added "
						"when relying on an incompatible result "
						"specification (trying to add '~p' to '~p').",
						[ Patterns, Other ] ),
					throw( { blacklists_cannot_be_removed, Patterns, Other } )

			end;

		false ->
			throw( { removed_blacklists_not_all_strings, Patterns } )

	end,

	wooper:return_state( NewState ).



% Replaces the current blacklisted result patterns by the specified ones,
% expressed as plain strings, in the current result specification, which must be
% already using targeted/blacklisted patterns (and not shortcut atoms).
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
-spec setBlacklistedPatterns( wooper:state(), [ base_result_pattern()] ) ->
									oneway_return().
setBlacklistedPatterns( State, NewPatterns ) ->

	NewState = case text_utils:are_strings( NewPatterns ) of

		true ->
			case ?getAttr(result_spec) of

				{ TargetLists, _BlackLists } ->
					BinBlacklists =
						text_utils:strings_to_binaries( NewPatterns ),
					setAttribute( State, result_spec,
								  { TargetLists, BinBlacklists } );

				Other ->
					?error_fmt( "Error, blacklisted patterns cannot be set "
						"when relying on an incompatible result "
						"specification (trying to set '~p' in '~p').",
						[ NewPatterns, Other ] ),
					throw( { blacklists_cannot_be_set, NewPatterns, Other } )

			end;

		false ->
			throw( { removed_blacklists_not_all_strings, NewPatterns } )

	end,

	wooper:return_state( NewState ).



% Updates (adds any non-existing entry, overwrites it otherwise) the current
% meta-data with the specified (possibly, user-originating) one.
%
% (request, for synchronicity)
%
-spec updateMetaData( wooper:state(), meta_data() ) ->
							request_return( 'meta_data_added' ).
updateMetaData( State, UpdatingMetaData ) ->

	NewMetaData = lists:foldl( fun( Pair, Acc ) ->
								   option_list:set( Pair, Acc )
							   end,
							  _AccInit=?getAttr(meta_data),
							  _List=option_list:enumerate( UpdatingMetaData ) ),

	NewState = setAttribute( State, meta_data, NewMetaData ),

	wooper:return_state_result( NewState, meta_data_added ).



% Tells each resilience agent what are its (local) probes that it must manage.
%
% Typically called by the resilience manager, when a serialisation must take
% place.
%
% (request, for synchronicity)
%
-spec notifyResilienceAgentsOfProbes( wooper:state(),
			[ resilience_agent_pid() ] ) ->
							const_request_return( 'probes_notified' ).
notifyResilienceAgentsOfProbes( State, NodeAgents ) ->

	% Retrieving first the PID of all basic probes:
	ProbePids = [ ProbePid || { _NameKey,
			_Value=#basic_probe_entry{ probe_pid=ProbePid } }
				 <- table:enumerate( ?getAttr(basic_probe_table) ) ],

	% Note: virtual and web probes to be managed.

	% Creating then an empty table where the keys are the node names, and the
	% associated values are a pair made of the PID of the resilience agent
	% corresponding to that node and of the list of the PIDs of the
	% corresponding (local) probes (initially an empty list):
	%
	EmptyNodeTable = lists:foldl(
			fun( AgentPid, TableAcc ) ->
				   table:add_entry( _K=node( AgentPid ),
									_V={ AgentPid, _Probes=[] }, TableAcc )
			end,
			_EmptyInitialAcc=table:new(),
			_EmptyList=NodeAgents ),

	% Now let's map the probes onto the agents, using nodes as intermediary:
	FilledNodeTable = lists:foldl(

			fun( ProbePid, TableAcc ) ->
				% Simply adds this probe to the list for the right node:
				NodeKey = node( ProbePid ),
				{ AgentPid, ProbeList } = table:get_value( NodeKey, TableAcc ),
					table:add_entry( NodeKey,
						{ AgentPid, [ ProbePid | ProbeList ] }, TableAcc )

			end,
			_FilledInitialAcc=EmptyNodeTable,
			_FilledList=ProbePids ),

	% Now we can notify each resilience agent of its probes:
	[ AgentPid ! { notifyOfLocalProbes, [ AgentProbeList ], self() }
	  || { AgentPid, AgentProbeList } <- table:values( FilledNodeTable ) ],

	wooper:wait_for_request_answers( _RequestedList=NodeAgents,
									 _AckAtom=probes_recorded ),

	wooper:const_return_result( probes_notified ).



% Returns probe-related base information.
%
% Typically called by the web manager.
%
-spec getBaseProbeInfos( wooper:state() ) ->
	const_request_return( { [ probe_info() ], [ probe_info() ], meta_data() } ).
getBaseProbeInfos( State ) ->

	% The only probes whose directory cannot be automatically determined are the
	% facility ones:

	Res = { get_basic_probe_infos( State ), get_virtual_probe_infos( State ),
			?getAttr(meta_data) },

	wooper:const_return_result( Res ).



get_basic_probe_infos( State ) ->
	[ get_basic_probe_info( Name, Entry )
	  || { Name, Entry } <- table:enumerate( ?getAttr(basic_probe_table) ) ].



get_basic_probe_info( Name, #basic_probe_entry{ probe_dir=MaybeBinDir } ) ->
	{ Name, MaybeBinDir }.



% Not implemented currently:
get_virtual_probe_infos( _State ) ->
	undefined.



% Simulation listener section.


% Optimises result tables and lists all known registered results.
-spec simulation_started( wooper:state() ) -> oneway_return().
simulation_started( State ) ->

	BasicProbeTable = ?getAttr(basic_probe_table),

	OptimisedBasicProbeTable = table:optimise( BasicProbeTable ),
	%table:display( "Basic probe table", OptimisedBasicProbeTable ),

	% Note: we do not distinguish here between probes that are tracked or not:
	BasicProbeBinNames = table:keys( OptimisedBasicProbeTable ),
	BasicProbeString = text_utils:binaries_to_string( BasicProbeBinNames ),
	BasicProbeCount = length( BasicProbeBinNames ),

	VirtualProbeTable = ?getAttr(virtual_probe_table),
	OptimisedVirtualProbeTable = table:optimise( VirtualProbeTable ),
	%table:display( "Virtual probe table", OptimisedVirtualProbeTable ),

	VirtualProbeBinNames = table:keys( OptimisedVirtualProbeTable ),

	VirtualProbeString = text_utils:binaries_to_string( VirtualProbeBinNames ),

	VirtualProbeCount = length( VirtualProbeBinNames ),


	WebProbeTable = ?getAttr(web_probe_table),
	OptimisedWebProbeTable = table:optimise( WebProbeTable ),
	%table:display( "Web probe table", OptimisedWebProbeTable ),

	WebProbeBinNames = table:keys( OptimisedWebProbeTable ),

	WebProbeString = text_utils:binaries_to_string( WebProbeBinNames ),

	WebProbeCount = length( WebProbeBinNames ),


	ResultCount = BasicProbeCount + VirtualProbeCount + WebProbeCount,

	case ResultCount of

		0 ->
			% Results *may* also be declared at simulation-time (dynamically):
			?notice( "At simulation start, no expected result is identified." );

		_ ->

			BasicString = case BasicProbeCount of

				0 ->
					"no basic probe declared";

				_ ->
					text_utils:format( "~B basic probe(s) declared: ~ts",
									   [ BasicProbeCount, BasicProbeString ] )

			end,


			VirtualString = case VirtualProbeCount of

				0 ->
					"no virtual probe declared";

				_ ->
					text_utils:format( "~B virtual probe(s) declared: ~ts",
						[ VirtualProbeCount, VirtualProbeString ] )

			end,

			WebString = case WebProbeCount of

				0 ->
					"no web probe declared";

				_ ->
					text_utils:format( "~B web probe(s) declared: ~ts",
									   [ WebProbeCount, WebProbeString ] )

			end,

			?notice_fmt( "At simulation start, ~ts, ~ts and ~ts",
						 [ BasicString, VirtualString, WebString ] )

	end,

	wooper:return_state( setAttributes( State, [
			{ basic_probe_table, OptimisedBasicProbeTable },
			{ virtual_probe_table, OptimisedVirtualProbeTable },
			{ web_probe_table, OptimisedWebProbeTable } ] ) ).



% Notification ignored.
-spec simulation_suspended( wooper:state() ) -> const_oneway_return().
simulation_suspended( State ) ->
	wooper:const_return().


% Notification ignored.
-spec simulation_resumed( wooper:state() ) -> const_oneway_return().
simulation_resumed( State ) ->
	wooper:const_return().


% This corresponds to a message, interpreted as a oneway call, being sent by the
% root time manager whenever the simulation succeeded, knowing this result
% manager subscribed to it as a simulation listener.
%
-spec simulation_succeeded( wooper:state() ) -> oneway_return().
simulation_succeeded( State ) ->

	?info( "Simulation succeeded, collecting results now." ),

	class_PluginManager:notify( on_result_gathering_start ),

	% We used to change the (overall) current directory, not done anymore, for
	% the best (changing the overall VM current directory whereas plenty of
	% processes are potentially relying on a stable one is not recommended)
	%
	%RunDir = ?getAttr(simulation_run_dir),
	%
	%?debug_fmt( "Switching from '~ts' to '~ts'.",
	%			[ file_utils:get_current_directory(), RunDir ] ),
	%
	%file_utils:set_current_directory( RunDir ),

	% Let's create the result directory for that simulation:
	ResultBaseDirName = ?getAttr(result_dir),

	case file_utils:is_existing_directory( ResultBaseDirName ) of

		% Normal case:
		false ->
			file_utils:create_directory( ResultBaseDirName );

		true ->
			% At least the SII may have been used more than once:
			?error_fmt( "Result directory ('~ts') is already existing, "
				"which is both unlikely and abnormal.", [ ResultBaseDirName ] ),

			throw( { already_existing_result_directory, ResultBaseDirName } )

	end,

	% Note that this would have side-effects, as the current working directory
	% of the whole VM (the one of the simulation case - i.e. the user one) would
	% then change, which as detailed below is not wanted:
	%
	% ok = file:set_cwd( ResultBaseDirName ),

	% There would be no problem for all producers that were created from models,
	% as they are running on different VMs with different working directories
	% (by default, under '/tmp').
	%
	% But producers created from the simulation case (ex: a probe created from a
	% test) are in the same VM as the result manager, and thus share the same
	% current working directory.
	%
	% If we went to the output directory, then the .dat file could have been
	% produced in the initial directory (if written immediately), and thus would
	% not be found. The solution is to stay in the initial directory, and to
	% write/extract results in the output one.

	% We request results to be produced and sent in parallel, but we have to
	% ensure that some flow control is enforced, otherwise a given computing
	% host might be overwhelmed by the number of parallel reports requested, and
	% may even crash because of it.

	?info_fmt( "Simulation succeeded, collecting results now from "
		"producers in result directory '~ts', while current one is '~ts'.",
		[ ResultBaseDirName, file_utils:get_current_directory() ] ),

	% First, triggers the possibly most loaded producer:
	VirtualState = case ?getAttr(data_logger_enabled) of

		true ->
			trigger_virtual_probe_results( State );

		false ->
			State

	end,

	% Then, the other producers, and wait for them all:
	ProbeState = manage_all_producers( VirtualState ),

	Listeners = ?getAttr(listeners),

	?info_fmt( "All results successfully gathered, "
			   "notifying all listeners (~p).", [ Listeners ] ),

	ResultOneway= { results_collected,
					text_utils:string_to_binary( ResultBaseDirName ) },

	% Notifies that the result collection is over, but does not imply there were
	% actual results:
	%
	[ Pid ! ResultOneway || Pid <- Listeners ],

	class_PluginManager:notify( on_result_gathering_stop ),

	wooper:return_state( ProbeState ).




% Requests the data-logger for any result needed.
%
% Returns an updated state, with updated queues.
%
% (helper)
%
trigger_virtual_probe_results( State ) ->

	case ?getAttr(result_spec) of

		no_output ->
		   State;

		all_basic_probes_only ->
		   State;

		% Includes: all_outputs, all_virtual_probes_only, hence always involves
		% results from the data-logger:
		%
		_Other ->
			% First, triggers the result generation:

			DataloggerPid = class_DataLogger:get_main_datalogger(),

			DataloggerPid ! { sendResults,
							 [ ?getAttr(datalogger_default_options) ], self() },


			% Then registers this generation:

			DataloggerNode = node( DataloggerPid ),

			% We add only now the datalogger to the pending result producers;
			% this is fortunate, as it will thus be the first one to be picked
			% and thus the first to be triggered (a good thing, knowing it might
			% induce a lot of processing).

			{ DataloggerQueue, OtherQueues } = extract_queue_by_node(
					DataloggerNode, ?getAttr(result_queues) ),

			NewPidToQueueTable = table:add_entry( _K=DataloggerPid,
				_V=DataloggerQueue#result_queue.id, ?getAttr(pid_to_queue) ),

			% Waited list was presumably empty:
			NewWaited = [ DataloggerPid
						 | DataloggerQueue#result_queue.waited_producers ],

			NewDataloggerQueue =
				DataloggerQueue#result_queue{ waited_producers=NewWaited },

			setAttributes( State, [
				{ result_queues, [ NewDataloggerQueue | OtherQueues ] },
				{ pid_to_queue, NewPidToQueueTable }
				% result_collected set to true upon acknowledgement that all
				% result producers finished.
								  ] )

	end.



% Requests by chunks (whose size depends on the core count of the target host)
% all relevant probes to send their result(s).
%
% Returns an updated state.
%
manage_all_producers( State ) ->

	ManagedState = case ?getAttr(result_spec) of

		no_output ->
			State;


		% Includes: all_outputs, all_virtual_probes_only, all_basic_probes_only;
		% basically, we wait and trigger producers until all of them are over:
		%
		_Other ->
			Queues = ?getAttr(result_queues),

			%trace_utils:debug( "Managing result queues." ),
			%display_queues( Queues ),

			TotalProducerCount = compute_producer_count( Queues, _Sum=0 ),

			?debug_fmt( "Requesting results from ~B producers.",
						[ TotalProducerCount ] ),

			ResultFound = ( TotalProducerCount > 0 ),

			BasicProbeTable = ?getAttr(basic_probe_table),

			WebTable = ?getAttr(web_probe_table),

			% All concrete (non-virtual) probes (unclashing merges expected):
			MergedTable = table:merge( BasicProbeTable, WebTable ),

			% Triggers a first set of result production:
			{ TriggeredQueues, UpdatedPidToQueueTable } =
			  load_result_queues( Queues, ?getAttr(pid_to_queue), MergedTable ),

			ProducerTimeout = get_producer_time_out(),

			% Now waits until all producers have been processed:
			DepletedQueues = wait_and_exhaust_queues( TriggeredQueues,
				UpdatedPidToQueueTable, TotalProducerCount, MergedTable,
				ProducerTimeout, State ),

			setAttributes( State, [ { result_queues, DepletedQueues },
									{ result_found, ResultFound } ] )

	end,

	setAttribute( ManagedState, result_collected, true ).



compute_producer_count( _Queues=[], Sum ) ->
	Sum;

compute_producer_count( _Queues=[ #result_queue{ pending_results=Pending,
							waited_producers=Waited } | T ], Sum ) ->

	% We may have producers already launched and waited:
	compute_producer_count( T, Sum + length( Pending ) + length( Waited ) ).



% Ensures that each result queue has all possible workers working.
%
% Returns a { TriggeredQueues, PidToQueueTable } pair, where TriggeredQueues is
% a list of updated queues and PidToQueueTable is an updated 'PID to queue ID'
% translation table.
%
load_result_queues( Queues, PidToQueueTable, ProbeTable ) ->
	load_result_queues( Queues, PidToQueueTable, ProbeTable, _AccQueues=[] ).


load_result_queues( _Queues=[], PidToQueueTable, _ProbeTable, AccQueues ) ->
	{ AccQueues, PidToQueueTable } ;

load_result_queues( _Queues=[ Q | T ], PidToQueueTable, ProbeTable,
					AccQueues ) ->

	{ UpdatedQueue, ProducerPidList } = load_result_queue( Q, ProbeTable ),

	QueueId = UpdatedQueue#result_queue.id,

	% Adds these key/value pairs:
	NewPidEntries = [ { Pid, QueueId } || Pid <- ProducerPidList ],

	UpdatedPidToQueueTable =
		table:add_entries( NewPidEntries, PidToQueueTable ),

	load_result_queues( T, UpdatedPidToQueueTable, ProbeTable,
						[ UpdatedQueue | AccQueues ] ).



% Loads as much as possible specified queue.
%
% Returns {UpdatedQueue, ProducerPidList}.
%
load_result_queue( Queue=#result_queue{ pending_results=[] }, _ProbeTable ) ->

	% Here, no pending result, nothing to do, thus nothing to change:
	{ Queue, _ProducerPidList=[] };


load_result_queue( Queue=#result_queue{ max_worker_count=MaxCount,
		waited_producers=Waited, pending_results=Pending }, ProbeTable ) ->

	% Here, there are results to be generated; how many worker slots are free?
	SpareSlotCount = MaxCount - length( Waited ),

	% Selects the SpareSlots first producers:
	{ FirstProducers, OtherProducers } =
		list_utils:split_at( SpareSlotCount, Pending ),

	ProducerPidList = trigger_producers( FirstProducers, ProbeTable ),

	NewQueue = Queue#result_queue{ waited_producers= ProducerPidList ++ Waited,
								   pending_results=OtherProducers },

	{ NewQueue, ProducerPidList }.



% Triggers specified producers.
%
% Returns a list of their PIDs.
%
trigger_producers( Producers, ProbeTable ) ->
	trigger_producers( Producers, ProbeTable, _Acc=[] ).


trigger_producers( _Producers=[], _ProbeTable, Acc ) ->
	Acc;

trigger_producers( _Producers=[ ProducerName | T ], ProbeTable, Acc ) ->

	ProducerPid = trigger_producer( ProducerName, ProbeTable ),

	trigger_producers( T, ProbeTable, [ ProducerPid | Acc ] ).



% Triggers specified result producer, and returns its PID.
%
% Defined for reusability.
%
trigger_producer( ProducerName, ProbeTable ) ->

	{ ProducerPid, Options } =
		case table:get_value( ProducerName, ProbeTable ) of

			#basic_probe_entry{ probe_pid=Pid, probe_options=Opts } ->
				{ Pid, Opts };

			#web_probe_entry{ probe_pid=Pid, probe_options=Opts } ->
				{ Pid, Opts }

	end,

	%trace_utils:debug_fmt( "Triggering producer ~ts (~w).",
	%					   [ ProducerName, ProducerPid ] ),

	ProducerPid ! { sendResults, [ Options ], self() },

	ProducerPid.



% Waits for current triggered producers to finish, and replenish workers for all
% queues, until no result is pending.
%
% Returns the updated queues.
%
wait_and_exhaust_queues( Queues, _PidToQueueTable, _TotalProducerCount=0,
						 _ProbeTable, _ProducerTimeout, _State ) ->

	%trace_utils:debug( "No producer left, finished!" ),

	check_queues_empty( Queues ),

	Queues;


wait_and_exhaust_queues( Queues, PidToQueueTable, TotalProducerCount,
						 ProbeTable, ProducerTimeout, State ) ->

	%trace_utils:debug_fmt( "Still ~B producers waited.",
	%					   [ TotalProducerCount ] ),

	% Hijacks the WOOPER main loop to better manage time-outs.
	%
	% We consider that, on a previous step, an initial request in order to
	% produce the first results was sent; let's wait for answers and replenish
	% waited lists:
	%

	ActualProducerPid = receive


		{ wooper_result, { ProducerPid, archive, BinArchive } } ->

			Filenames = file_utils:zipped_term_to_unzipped_files( BinArchive,
												?getAttr(result_dir) ),

			?info_fmt( "Received an archive from producer ~w, following "
				"files were extracted (while in directory '~ts'): ~ts",
				[ ProducerPid, file_utils:get_current_directory(),
				  text_utils:strings_to_string( Filenames ) ] ),

			ProducerPid;


		{ wooper_result, { ProducerPid, raw, { BinFilename, BinContent } } } ->


			Filename = text_utils:binary_to_string( BinFilename ),
			TargetFilename = file_utils:join( ?getAttr(result_dir), Filename ),

			file_utils:write_whole( TargetFilename, BinContent ),

			?info_fmt( "Received a raw file from producer ~w: '~ts', "
				"written (while in directory '~ts') as '~ts'.",
				[ ProducerPid, BinFilename, file_utils:get_current_directory(),
				  TargetFilename ] ),

			ProducerPid;


		{ wooper_result, { ProducerPid, no_result } } ->

			% A producer may have nothing to report (ex: the data-logger):
			?info_fmt( "Producer ~w notified that it had no result "
					   "to provide.", [ ProducerPid ] ),

			ProducerPid


	% Time-out renewed at each producer answer:
	after ProducerTimeout ->

		throw( { result_producer_time_out, TotalProducerCount, Queues } )

	end,

	{ NewQueues, NewPidToQueueTable } = update_queues_after_result(
			 ActualProducerPid, PidToQueueTable, ProbeTable, Queues ),

	wait_and_exhaust_queues( NewQueues, NewPidToQueueTable,
		TotalProducerCount - 1, ProbeTable, ProducerTimeout, State ).



% Updates the queues after the reception of the result from specified producer.
%
% Returns { NewQueues, NewPidToQueueTable }.
%
update_queues_after_result( ProducerPid, PidToQueueTable, ProbeTable,
							Queues ) ->

	QueueId = table:get_value( ProducerPid, PidToQueueTable ),

	{ Queue, OtherQueues } = extract_queue_by_id( QueueId, Queues ),

	NewPidToQueueTable = table:remove_entry( ProducerPid, PidToQueueTable ),

	DelWaited = lists:delete( ProducerPid,
							  Queue#result_queue.waited_producers ),

	% One less producer online, hence one more to trigger:
	{ NewPending, NewWaited } = case Queue#result_queue.pending_results of

		[] ->
			{ [], DelWaited };

		[ ProducerName | T ] ->

			%trace_utils:debug_fmt( "Replenishing queue #~B with '~ts'.",
			%						[ QueueId, ProducerName ] ),

			NewProducerPid = trigger_producer( ProducerName, ProbeTable ),

			{ T, [ NewProducerPid | DelWaited ] }

	end,

	NewQueue = Queue#result_queue{ waited_producers=NewWaited,
								   pending_results=NewPending },

	NewQueues = [ NewQueue | OtherQueues ],

	{ NewQueues, NewPidToQueueTable }.



% Ensures that all result queues have been fully processed indeed.
check_queues_empty( _Queues=[] ) ->
	ok;

check_queues_empty( _Queues=[
		#result_queue{ waited_producers=[], pending_results=[] } | T ] ) ->
	check_queues_empty( T ).


% Notification ignored.
-spec simulation_stopped( wooper:state() ) -> const_oneway_return().
simulation_stopped( State ) ->
	wooper:const_return().



% Returns the current result directory in use.
-spec getResultDirectory( wooper:state() ) ->
							const_request_return( directory_path() ).
getResultDirectory( State ) ->
	wooper:const_return_result( ?getAttr(result_dir) ).


% Requires the result reports to be browsed.
-spec browseResultReports( wooper:state() ) ->
								 const_request_return( 'results_browsed' ).
browseResultReports( State ) ->

	% Checking:
	true = ?getAttr(result_collected),

	case ?getAttr(result_found) of

		true ->

			ResultDir = ?getAttr(result_dir),

			% As all results may be non-graphical (ex: only *.dat):
			{ Files, _Symlinks, _Directories, _OtherFiles, _Devices } =
				file_utils:list_dir_elements( ResultDir ),

			case file_utils:filter_by_extensions( Files,
							file_utils:get_image_extensions() ) of

				[] ->
					trace_utils:notice( "(results matched the specification, "
						"but none was graphical, thus nothing to be browsed "
						"here)" );

				[ _ ] ->
					trace_utils:notice(
					  "(displaying a single graphical result)" ),

					executable_utils:browse_images_in( ResultDir );

				L ->
					trace_utils:notice_fmt( "(displaying ~B graphical results)",
											[ length( L ) ] ),

					executable_utils:browse_images_in( ResultDir )

			end;


		false ->
			trace_utils:notice( "(no result matched the specification, "
								"thus nothing to be browsed here)" )

	end,

	wooper:const_return_result( results_browsed ).



% Adds specified process as a result listener, that will be notified of any
% event it missed.
%
-spec addResultListener( wooper:state(), result_listener_pid() ) ->
								oneway_return().
addResultListener( State, ListenerPid ) ->

	% If results were already collected, send past notification:
	case ?getAttr(result_collected) of

		true ->
			ResultBinDir = text_utils:string_to_binary( ?getAttr(result_dir) ),
			ListenerPid ! { results_collected, ResultBinDir };

		false ->
			ok

	end,

	wooper:return_state( appendToAttribute( State, listeners, ListenerPid ) ).



% Removes specified process from the known result listeners.
-spec removeResultListener( wooper:state(), result_listener_pid() ) ->
								oneway_return().
removeResultListener( State, ListenerPid ) ->

	?debug_fmt( "Remove result listener ~p.", [ ListenerPid ] ),

	wooper:return_state( deleteFromAttribute( State, listeners, ListenerPid ) ).




% Static methods section.


% Returns the atom corresponding to the name the result manager should be
% registered as.
%
-spec get_registration_name() -> static_return( atom_node_name() ).
get_registration_name() ->
	wooper:return_static( sim_diasca_result_manager ).



% Returns the PID of the (unique) result manager.
%
% (static method, to be used by clients of the result manager, notably result
% producers).
%
-spec get_result_manager() -> static_return( manager_pid() ).
get_result_manager() ->

	ManagerPid = naming_utils:wait_for_global_registration_of(
					get_registration_name() ),

	wooper:return_static( ManagerPid ).



% Returns the path to the current result directory.
-spec get_result_directory() -> static_return( directory_path() ).
get_result_directory() ->

	ManagerPid = get_result_manager(),

	ManagerPid ! { getResultDirectory, [], self() },

	receive

		{ wooper_result, Dir } when is_list( Dir ) ->
			wooper:return_static( Dir )

	end.



% Browse reports.
%
% Allows to request the automatic displaying of graphical reports (ex: plots
% from plots) depending on the batch mode being enabled or not, and to wait for
% it.
%
% To be used from simulation cases.
%
% Replaces a less reliable macro.
%
-spec browse_reports() -> static_void_return().
browse_reports() ->
	browse_reports( _TriggerBasicDisplay=true ),
	wooper:return_static_void().



% Browse reports.
%
% Allows to request the automatic displaying of graphical reports (ex: plots
% from plots) depending on the batch mode being enabled or not, and to wait for
% it.
%
% To be used from simulation cases.
%
% Replaces a less reliable macro.
%
-spec browse_reports( boolean() ) -> static_void_return().
browse_reports( TriggerBasicDisplay ) ->

	% This function is stateless, which is very convenient as, since the start,
	% most services may have been redeployed in the meantime, due to resilience
	% mechanisms having to kick in.

	% Generating performance monitoring results, if any, and in parallel to
	% simulation results:
	%
	WaitForPerformanceTracker = case class_PerformanceTracker:get_tracker() of

		not_registered ->
			%trace_utils:debug( "No performance tracker was enabled." ),
			false;


		TrackerPid ->
			?notify_debug_fmt_cat( "Requesting reports from the "
				"performance tracker ~w.", [ TrackerPid ],
				?trace_emitter_categorization ),

			TrackerPid ! { generateMonitoringReports, [], self() },

			true

	end,

	ResultManagerPid = class_ResultManager:get_result_manager(),

	% All simulation test cases are expected to register themselves as
	% simulation listeners (ex: at simulation start), thus they should be
	% notified also about the simulation success (if any).

	case executable_utils:is_batch() of

		true ->

			% In batch mode here.

			receive

				% We *must* wait for that, otherwise results will not be copied
				% on time before the VM halts (race condition with case
				% termination, we must wait synchronously):
				%
				simulation_succeeded ->

					% We nevertheless have to wait for them:
					ResultManagerPid ! { addResultListener, self() },

					trace_utils:notice(
					  "In batch mode, no browsing of results performed. "
					  "Waiting for their processing and retrieval." ),


					receive

						{ results_collected, ResultBinDir } ->
							trace_utils:notice_fmt( "Results are available "
							  "now, in the '~ts' directory.", [ ResultBinDir ] )

					end

			end;


		false ->

			% In interactive mode here.

			receive

				% All simulation test cases are expected to register themselves
				% as simulation listeners (ex: at simulation start), thus they
				% should be notified also about the simulation success (if any).
				%
				simulation_succeeded ->

					% We need to wait for the results:
					ResultManagerPid ! { addResultListener, self() },

					trace_utils:notice( "Simulation success, result reports to "
						"be processed, collected then browsed now." ),

					receive

						{ results_collected, ResultBinDir } ->
							trace_utils:notice_fmt( "Results are available now,"
								" in the '~ts' directory.", [ ResultBinDir ] )

					end,

					case TriggerBasicDisplay of

						true ->
							ResultManagerPid !
								{ browseResultReports, [], self() },

							receive

								{ wooper_result, results_browsed } ->
									?notify_info_cat( "Result reports have "
										"been successfully browsed.",
										?trace_emitter_categorization )

							after 5000 ->

								trace_utils:notice(
									"Simulation failed, no result gathered." )

							end;

						false ->
							%trace_utils:info( "No basic display requested." )
							ok

					end

			end

	end,

	case WaitForPerformanceTracker of

		true ->

			%trace_utils:debug( "Waiting for the performance tracker." ),

			receive

				{ wooper_result, report_generated } ->

					?notify_info_cat( "Monitoring reports have been "
						"successfully generated.",
						?trace_emitter_categorization ),

					trace_utils:notice( "Monitoring reports have been "
										"successfully generated." )

			end;

		false ->
			ok

	end,

	wooper:return_static_void().



% Returns a plain string describing the specified result-related meta-data.
-spec get_metadata_string( meta_data() ) -> static_return( ustring() ).
get_metadata_string( Metadata ) ->

	MetadataAllStrings = [ text_utils:binary_to_string( BinText )
						   || { _Key, BinText } <- Metadata ],

	Desc = text_utils:strings_to_string( MetadataAllStrings ),

	wooper:return_static( Desc ).




% Creates a mock-up environment suitable for the test of result producers in
% isolation (i.e. without creating all the simulation services).
%
% See also: class_InstanceTracker:create_mockup_environment/0.
%
-spec create_mockup_environment() -> static_return( pid() ).
create_mockup_environment() ->

	Pid = ?myriad_spawn_link( fun() ->

		naming_utils:register_as( ?instance_tracker_name, local_only ),
		naming_utils:register_as( ?result_manager_name, global_only ),

		create_mockup_environment_loop()

							  end ),

	wooper:return_static( Pid ).



% Forces the executing process to linger (will never terminate); otherwise for
% example a probe destructor would not find its expected instance tracker as a
% still registered process.
%
% (helper)
%
create_mockup_environment_loop() ->

	% Mimics a result manager (and the existence of a local instance tracker):

	% Fakes a local instance tracker and a result manager:
	receive

		{ registerResultProducer, _BinName, ProducerPid } ->
			ProducerPid ! { wooper_result, result_producer_registered };

		{ registerAgent, _AgentRef, AgentPid } ->
			AgentPid ! { wooper_result, agent_registered };

		{ declareProbe, [ _Name, _IsTrackedProducer, _MaybeProbeBinDir ],
		  ProbePid } ->
			ProbePid ! { wooper_result, output_requested };

		{ unregisterResultProducer, _ProducerPid } ->
			ok;

		{ unregisterAgent, _AgentPid } ->
			ok;

		Unexpected ->
			trace_utils:error_fmt(
			  "Mock-up result environment received an unexpected (hence "
			  "ignored) message:~n  ~p", [ Unexpected ] )

	end,

	create_mockup_environment_loop().



% Helper functions.


% We want to defer as much as possible the fine analysis of the result
% matchings, however we ensure first that the user did not rely on
% specifications that are obviously incorrect (mostly syntax checkings).
%
% Result is either an atom of a pair of two lists.
%
check_and_transform_result_specification( all_outputs ) ->
	all_outputs;

check_and_transform_result_specification( no_output) ->
	no_output;

check_and_transform_result_specification( all_basic_probes_only ) ->
	all_basic_probes_only;

check_and_transform_result_specification( all_virtual_probes_only ) ->
	all_virtual_probes_only;

check_and_transform_result_specification( all_web_probes_only ) ->
	all_web_probes_only;

% Returns { Targets, Blacklists }:
check_and_transform_result_specification( Specs ) when is_list( Specs ) ->
	manage_patterns( Specs, _Targets=[], _Blacklists=[] );

check_and_transform_result_specification( NonMatching ) ->
	throw( { invalid_result_specification, NonMatching } ).


% (helper)
manage_patterns( _Specs=[], Targets, Blacklists ) ->
	{ Targets, Blacklists };

manage_patterns( [ { targeted_patterns, L } | T ], Targets, Blacklists )
  when is_list( L ) ->
   manage_patterns( T, manage_targets( L ) ++ Targets, Blacklists );

manage_patterns( [ { targeted_patterns, L } | _T ], _Targets, _Blacklists ) ->
	throw( { invalid_result_target_pattern, L } );

manage_patterns( [ { blacklisted_patterns, L } | T ], Targets, Blacklists )
  when is_list( L ) ->
	manage_patterns( T, Targets, manage_blacklists( L ) ++ Blacklists );

manage_patterns( [ { blacklisted_patterns, L } | _T ], _Targets,
				_Blacklists ) ->
	throw( { invalid_result_blacklist_pattern, L } );

manage_patterns( [ Any | _T ], _Targets, _Blacklists ) ->
	throw( { invalid_result_pattern, Any } ).



% In the Targets list, elements are {BinaryPatttern, PrecompiledMatchSpec,
% OptionList} triplets (default option list is empty).
%
manage_targets( Targets ) ->
	manage_targets( Targets, _Acc=[] ).


manage_targets( _Targets=[], Acc ) ->
	Acc;

% Wanting a list of options, if it is not the 'undefined' atom:
manage_targets( [ { Target, Option } | T ], Acc ) when is_atom( Option )
			  andalso Option =/= undefined ->
	manage_targets( [ { Target, [ Option ] } | T ], Acc );

% We have a list of options here:
manage_targets( [ { Target, Options } | T ], Acc ) ->

	case text_utils:is_string( Target ) of

		true ->
			check_target_option( Options ),
			NewAcc = [ { text_utils:string_to_binary( Target ),
						 compile( Target ), Options } | Acc ],
			manage_targets( T, NewAcc );

		false ->
			throw( { invalid_result_target, Target } )

	end;

% No option here:
manage_targets( [ Target | T ], Acc ) when is_list( Target ) ->
	manage_targets( [ { Target, undefined } | T ], Acc );

manage_targets( Other, _Acc ) ->
	throw( { invalid_result_target_specification, Other } ).



% In the blacklisted list, elements are {BinaryPatttern, PrecompiledMatchSpec}
% pairs.
%
manage_blacklists( Blacklists ) ->
	manage_blacklists( Blacklists, _Acc=[] ).


manage_blacklists( _Blacklists=[], Acc ) ->
	Acc;

manage_blacklists( [ BlackList | T ], Acc ) ->

	case text_utils:is_string( BlackList ) of

		true ->
			NewAcc = [ { text_utils:string_to_binary( BlackList ),
						compile( BlackList ) } | Acc ],
			manage_blacklists( T, NewAcc );

		false ->
			throw( { invalid_result_blacklist, BlackList } )

	end.


% Checks that the option(s) specified with the targeted patterns are valid.
check_target_option( _Options=undefined ) ->
	ok;

check_target_option( _Options=[] ) ->
	ok;

check_target_option( _Options=[ H | T ] ) ->

	case lists:member( H, [ data_only, rendering_only, data_and_rendering ] ) of

		true ->
			check_target_option( T );

		false ->
			throw( { invalid_targeted_pattern_option, H } )

	end.



% Returns a precompiled regular expression.
compile( Pattern ) ->

	%trace_utils:debug_fmt( "Compiling pattern '~ts'.", [ Pattern ] ),

	case re:compile( Pattern ) of

		{ ok, MatchSpec } ->
			MatchSpec;

		{ error, Error } ->
			throw( { result_pattern_precompilation_failed, Pattern, Error } )

	end.



% Returns either 'false' or a {'true', Options} pair, where Options is a list.
%
% ProducerName must be a binary string.
%
is_result_wanted( ProducerName, Nature, State ) ->

	WantedInfos = case ?getAttr(result_spec) of

		all_outputs ->
			{ true, undefined };

		no_output ->
			false;

		all_basic_probes_only ->

			case Nature of

				undefined ->
					{ true, undefined };

				basic_probe ->
					{ true, undefined };

				virtual_probe ->
					false;

				web_probe ->
					false

			end;

		all_virtual_probes_only ->
			case Nature of

				undefined ->
					{ true, undefined };

				basic_probe ->
					false;

				virtual_probe ->
					{ true, undefined };

				web_probe ->
					false

			end;

		all_web_probes_only ->
			case Nature of

				undefined ->
					{ true, undefined };

				basic_probe ->
					false;

				virtual_probe ->
					false;

				web_probe ->
					{ true, undefined }

			end;


		{ TargetPatterns, BlacklistPatterns } ->
			is_selected_with_options( ProducerName, TargetPatterns,
									  BlacklistPatterns )

	end,

	%trace_utils:debug_fmt( "Is result producer '~ts' wanted? '~p'.",
	%		  [ ProducerName, WantedInfos ] ),

	%?info_fmt( "Is result producer '~ts' wanted? '~p'.",
	%		  [ ProducerName, WantedInfos ] ),

	WantedInfos.



% Tells whether the specified producer name (a binary string) is targeted and
% non-blacklisted. If yes, returns also its associated options (if any).
%
% Return false or {true,Options}
%
is_selected_with_options( BinProducerName, TargetPatterns,
						  BlacklistPatterns ) ->

	ProducerName = text_utils:binary_to_string( BinProducerName ),

	case check_targeted( ProducerName, TargetPatterns ) of

		false ->
			%trace_utils:debug_fmt( "Producer ~p was not selected, "
			%                       "as not targeted.", [ ProducerName ] ),
			false;

		Res -> % = { true, Options } ->
			case check_blacklisted( ProducerName, BlacklistPatterns ) of

				true ->
					%trace_utils:debug_fmt( "Producer ~p was not selected, "
					%     "as targeted but blacklisted.", [ ProducerName ] ),
					false;

				false ->
					%trace_utils:debug_fmt( "Producer ~p was selected, "
					%  "as targeted and not blacklisted.", [ ProducerName ] ),
					Res

			end

	end.



% (helper)
check_targeted( _ProducerName, _TargetPatterns=[] ) ->
	false;

check_targeted( ProducerName, [ { _BinPattern, MatchSpec, Opts } | T ] ) ->

	%trace_utils:debug_fmt( "Checking '~ts' against targeted pattern ~p.",
	%						[ ProducerName, BinPattern ] ),

	case re:run( ProducerName, MatchSpec ) of

		nomatch ->
			check_targeted( ProducerName, T );

		{ match, _ } = _Match ->
			%trace_utils:debug_fmt( "Targeted match found: '~p'.", [ Match ] ),
			{ true, Opts }

	end.


% (helper)
check_blacklisted( _ProducerName, _BlacklistPatterns=[] ) ->
	false;

check_blacklisted( ProducerName, [ { _BinPattern, MatchSpec } | T ] ) ->

	%trace_utils:debug_fmt( "Checking '~ts' against blacklisted pattern ~p.",
	%						[ ProducerName, BinPattern ] ),

	case re:run( ProducerName, MatchSpec ) of

		nomatch ->
			check_blacklisted( ProducerName, T );

		{ match, _ } = _Match ->
			%trace_utils:debug_fmt( "Blacklisted match found: '~p'.",
			%                       [ Match ] ),
			true

	end.



% Extracts from the specified result queues the one whose node name is the
% specified one. Returns a pair made of this queue and a list of the other
% queues.
%
% (helper)
%
extract_queue_by_node( AtomNodeName, ResultQueues ) ->
	extract_queue_by_node( AtomNodeName, ResultQueues, _Acc=[] ).



extract_queue_by_node( AtomNodeName,
  _ResultQueues=[ Queue=#result_queue{ node_name=AtomNodeName } | T ], Acc ) ->
	% Found!
	{ Queue, T ++ Acc };

extract_queue_by_node( AtomNodeName, _ResultQueues=[ H | T ], Acc ) ->
	extract_queue_by_node( AtomNodeName, T, [ H | Acc ] ).



% Extracts from the specified result queues the one whose host name is the
% specified one. Returns a pair made of this queue and a list of the other
% queues.
%
% (helper)
%
extract_queue_by_host( AtomHostName, ResultQueues ) ->
	extract_queue_by_host( AtomHostName, ResultQueues, _Acc=[] ).



extract_queue_by_host( AtomHostName,
  _ResultQueues=[ Queue=#result_queue{ host_name=AtomHostName } | T ], Acc ) ->
	% Found!
	{ Queue, T ++ Acc };

extract_queue_by_host( AtomHostName, _ResultQueues=[ H | T ], Acc ) ->
	extract_queue_by_host( AtomHostName, T, [ H | Acc ] ).



% Extracts from the specified result queues the one whose identifier is the
% specified one. Returns a pair made of this queue and a list of the other
% queues.
%
% (helper)
%
extract_queue_by_id( Id, ResultQueues ) ->
	extract_queue_by_id( Id, ResultQueues, _Acc=[] ).



extract_queue_by_id( Id,
  _ResultQueues=[ Queue=#result_queue{ id=Id } | T ], Acc ) ->
	% Found!
	{ Queue, T ++ Acc };

extract_queue_by_id( Id, _ResultQueues=[ H | T ], Acc ) ->
	extract_queue_by_id( Id, T, [ H | Acc ] ).



% Returns a textual description of the specified queue.
to_string( #result_queue{ id=Id,
						  host_name=Hostname,
						  node_name=Nodename,
						  max_worker_count=MaxWorkerCount,
						  waited_producers=WaitedProducers,
						  pending_results=PendingResults } ) ->

	PendingString = case PendingResults of

		[] ->
			"no pending result";

		_ ->
			text_utils:format( "~B pending results (~ts)",
							   [ length( PendingResults ), PendingResults ] )

	end,

	text_utils:format(
	  "Result queue whose ID is #~B, on host '~ts' (node: '~ts') "
	  "with up to ~B workers allowed, currently waiting for "
	  "producers ~w, having ~ts",
	  [ Id, Hostname, Nodename, MaxWorkerCount, WaitedProducers,
		PendingString ] ).



% Displays on the console specified queues.
display_queues( Queues ) ->

	% Sorts by ID:
	QueueStrings = [ to_string( Q )
					 || Q <- lists:keysort( _IndexId=2, Queues ) ],

	trace_utils:debug_fmt( "Result queues:~n ~ts",
			[ text_utils:strings_to_string( QueueStrings ) ] ).



-spec get_producer_time_out() -> time_utils:time_out().


-ifdef(exec_target_is_production).


% Returns the producer time-out, depending on the execution target: the number
% of milliseconds waited before a result producer is supposed having failed.

% In production mode:
get_producer_time_out() ->
	% 4 hours here (yes, this is quite a lot):
	4 * 60 * 60 * 1000.


-else. % exec_target_is_production


% In development mode, we can rely on the default value:
get_producer_time_out() ->
	% 5 minutes here:
	5 * 60 * 1000.


-endif. % exec_target_is_production
