% Copyright (C) 2008-2021 EDF R&D

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

% Authors:
%   Jingxuan Ma (jingxuan.ma@edf.fr)
%   Olivier Boudeville (olivier.boudeville@edf.fr)


-module(class_PerformanceTracker).


-define( class_description,
		 "Agent in charge of tracking the overall runtime resource consumption "
		 "of the simulation." ).


% Its roles are to trace:
%
% 1. the memory consumptions on all nodes (i.e. on the user one and on each
% computing nodes)
%
% Thus there will be one (basic) facility probe per node, showing the following
% memory consumptions:
%
% - the available RAM memory (which is: free memory + buffers + cache)
%
% - the memory allocated to the Erlang virtual machine (i.e. the memory
% currently used by the simulation, plus any other Erlang program being executed
% at the same time)
%
% - the memory used by the other applications (i.e. all non-Erlang applications)
%
% - the used swap
%
% 2. the number of Erlang processes and instances (overall and on each node)
%
% 3. the total and per-class instance count (aggregated, i.e. regardless of
% their dispatching on computing nodes)
%
%
% All these metrics are tracked over time twice, based on simulation time
% (i.e. tick) and on wall-clock time (i.e. real time).


% Data collected in wall-clock time is sent periodically, but, depending on the
% computer load, the sampling might be non-uniform, time-wise. However, the
% values correspond to the relevant time (i.e. the time at which they were
% measured).


% The performance tracker is mostly expected to run on the user node, so that,
% in case of simulation crash, post-mortem analysis of files left over is
% easier.


% See also: class_PerformanceTracker.hrl and class_PerformanceTracker_test.erl.


% Implementation notes.

% All probes used by this tracker are facility probes: they are not simulation
% results.
%
% The command file for each of these probes is created as soon as possible, so
% that, should this tracker crash (ex: should the simulation as a whole crash),
% they remain nevertheless available.



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).


-type node_static_info() :: class_InstanceTracker:node_static_info().


% To keep track of per-node resources:
%
% (probes track resources respectively over ticks and wallclock time)
%
-type node_entry() :: { atom_node_name(), node_static_info(),
						probe_pid(), probe_pid() }.


-type tracker_pid() :: agent_pid().


% To silence compiler:
-export_type([ node_entry/0 ]).

-export_type([ tracker_pid/0 ]).


% Attributes that are specific to a data-logger instance are:
-define( class_attributes, [

		{ started, boolean(),
		  "tells whether the performance tracking started" },

		{ tracker_result_dir, file_utils:directory_path(),
		  "where performance results will be written" },

		{ root_time_manager_pid, maybe( time_manager_pid() ),
		  "PID of the root time manager" },

		{ current_tick_offset, class_TimeManager:tick_offset(),
		  "allows this tracker to keep track of simulation ticks, as a "
		  "listener of the root time manager" },

		{ load_balancer_pid, maybe( load_balancer_pid() ),
		  "PID of the load balancer" },

		{ instance_trackers, [ instance_tracker_pid() ],
		  "a list of the PIDs of the instance tracker on all nodes (computing "
		  "ones and user one - we want to monitor it as well, resource-wise)" },

		{ resources_per_node_entries, [ node_entry() ],
		  "the ordered list of nodes with their associated information, "
		  "so that we can keep track of the order in which curves are "
		  "listed in samples for all probes aggregating node information, "
		  "and so that we can associate to each node its static settings and "
		  "resource probes; note that the first entry corresponds to the "
		  "user node, while the others are for the computing nodes" },

		{ nodes_in_tick_probe, probe_pid(),
		  "PID of the probe tracking the Erlang process and instance count on "
		  "each node, and also their total (simulation-wide) number, over "
		  "simulation ticks; initially, its value is undefined, and then is "
		  "set when computing nodes are known" },

		{ nodes_in_time_probe, probe_pid(),
		  "PID of the probe tracking the Erlang process and instance count on "
		  "each node, and also their total (simulation-wide) number, over "
		  "wallclock time; initially, its value is undefined, and then is "
		  "set when computing nodes are known"  },

		{ ordered_classnames, [ wooper:classname() ],
		  "ordered list of all classnames involved, so that we can keep track "
		  "of the order in which curves are listed in samples for the two "
		  "instance probes (over ticks and over time); note that there is an "
		  "implicit first entry (in display order) which corresponds to the "
		  "overall, total instance count (regardless of any class)" },

		{ classes_in_tick_probe, probe_pid(),
		  "PID of the probe tracking the number of instances for each WOOPER "
		  "class and the total one (in first position), over simulation "
		  "ticks; curves are added as classes are discovered" },

		{ classes_in_time_probe, probe_pid(),
		  "PID of the probe tracking the number of instances for each WOOPER "
		  "class and the total one (in first position), over wallclock "
		  "time; curves are added as classes are discovered" },

		{ ticker_pid, pid(),
		  "PID of the ticker process, whicht triggers information updates" },

		{ ticker_period, milliseconds(),
		  "Duration between two performance measurements (i.e. sending of  a "
		  "new data sample to its probes" } ] ).



% We need serialisation hooks to take care of internal helper processes:
-define( wooper_serialisation_hooks,).


% For the probe_option record:
-include("class_Probe.hrl").


% For performance tracker name, registration type and performance tracker
% settings:
%
-include("class_PerformanceTracker.hrl").


% For host_{static,dynamic}_info:
-include_lib("myriad/include/system_utils.hrl").

% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").

-define( trace_emitter_categorization, "Core.Tracker.Performance" ).


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").


% A general, multi-purpose time-out (in milliseconds):
-define( general_timeout_duration, 8000 ).


% Shorthands:

-type directory_path() :: file_utils:directory_path().

-type milliseconds() :: unit_utils:milliseconds().

-type tick_offset() :: class_TimeManager:tick_offset().
-type diasca() :: class_TimeManager:diasca().

-type atom_node_name() :: net_utils:atom_node_name().

-type host_dynamic_info() :: system_utils:host_dynamic_info().

-type term_serialisation() :: wooper_serialisation:term_serialisation().



% Constructs a new performance tracker, from following parameters:
%
% - PerformanceTrackerName is the atom under which this instance will be
% registered
%
% - RegistrationScope describes what kind of registration is requested
%
% - ResultDirInfo allows to set in with directory results should be created, and
% whether this directory shall be created
%
-spec construct( wooper:state(), naming_utils:registration_name(),
		naming_utils:registration_scope(),
		directory_path() | { directory_path(), 'do_not_create' } ) ->
						wooper:state().
construct( State, PerformanceTrackerName, RegistrationScope, ResultDirInfo ) ->

	%trace_utils:debug_fmt( "Constructing performance tracker ~w.",
	%					   [ self() ] ),

	% Increases the chances that this tracker does not lag too much compared to
	% wall-clock time:
	%
	erlang:process_flag( priority, _Level=high ),

	EmitterName = atom_to_list( PerformanceTrackerName ),

	TraceState = class_EngineBaseObject:construct( State,
										   ?trace_categorize(EmitterName) ),

	RegistrationName = get_registration_name( PerformanceTrackerName ),

	naming_utils:register_as( RegistrationName, RegistrationScope ),

	class_InstanceTracker:register_agent( RegistrationName ),

	ActualResultDirName = case ResultDirInfo of

		{ ResultDirName, do_not_create } ->
			ResultDirName;

		ResultDirName ->
			ResultDirName

	end,

	% Creating the result directory for the performance tracker:
	TrackerResultDir = file_utils:join( ActualResultDirName,
										"performance-monitoring" ),

	case ResultDirInfo of

		{ _ResultDirName, do_not_create } ->
			ok;

		_ ->
			file_utils:create_directory( TrackerResultDir )

	end,

	% The probes to monitor the resources per node cannot be created here, as we
	% do not know yet the nodes of interest.

	{ InstancesPerNodeInTickProbe, InstancesPerNodeInTimeProbe } =
		create_node_probes( TrackerResultDir ),

	{ InstancesPerClassInTickProbe, InstancesPerClassInTimeProbe } =
		create_class_probes( TrackerResultDir ),


	%trace_utils:debug( "Performance tracker ready." ),

	% All memory-related and process-related probes will be created when the
	% this performance tracker will be started:
	%
	FinalState = setAttributes( TraceState, [
		{ started, false },
		{ tracker_result_dir, TrackerResultDir },
		{ root_time_manager_pid, undefined },
		{ current_tick_offset, undefined },
		{ load_balancer_pid, undefined },
		{ instance_trackers, [] },
		{ resources_per_node_entries, undefined },
		{ nodes_in_tick_probe, InstancesPerNodeInTickProbe },
		{ nodes_in_time_probe, InstancesPerNodeInTimeProbe },
		{ ordered_classnames, [] },
		{ classes_in_tick_probe, InstancesPerClassInTickProbe },
		{ classes_in_time_probe, InstancesPerClassInTimeProbe },
		{ simulation_status, undefined },
		{ ticker_pid, undefined },
		{ ticker_period, _Milliseconds=100 } ] ),

	?send_info( FinalState, "Performance tracker created." ),

	FinalState.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%trace_utils:debug_fmt( "Deleting performance tracker ~w.", [ self() ] ),

	% Class-specific actions:
	?notice( "Deleting performance tracker." ),

	TickerState = stop_ticker( State ),

	% Deletes synchronously all probes:
	AllProbes = get_all_probes( TickerState ),

	[ class_Probe:delete_facility_probe( P ) || P <- AllProbes ],

	StoppedState = case ?getAttr(started) of

		true ->
			executeOneway( TickerState, stop );

		false ->
			State

	end,

	class_InstanceTracker:unregister_agent(),

	% No unregistering needed.

	?notice( "Performance tracker is deleted." ),

	%trace_utils:debug_fmt( "Performance tracker ~w deleted.", [ self() ] ),

	% Then call the direct mother class counterparts and allow chaining:
	StoppedState.





% Method method section.


% Starts the performance tracker:
%
% - RootTimeManagerPid is the PID of the root time manager, needed to monitor
% the main simulation events
%
% - Nodes is a list of atom node names, starting with the user node
%
% - {LoadBalancerPid, LoadBalancerNode} allows to interact with the load
% balancer
%
-spec start( wooper:state(), time_manager_pid(), [ atom_node_name() ],
		[ instance_tracker_pid() ], load_balancer_pid() ) -> oneway_return().
start( State, RootTimeManagerPid, Nodes=[ UserNode | ComputingNodes ],
	   InstanceTrackers, LoadBalancerPid )->

	% A performance tracker is a simulation listener in order to be notified (by
	% the root time manager) of the end of the simulation; otherwise the tracker
	% would keep on requesting statistics from the load balancer - which is an
	% actor, and thus may as such be already destructed, resulting in a
	% performance tracker time-out.

	RootTimeManagerPid ! { addSimulationListener, [ self() ] },

	% Requests early all remote information:
	Message = { getStaticResourceInformation, [], self() },

	[ I ! Message || I <- InstanceTrackers ],

	% We need to timestamp probe samples:
	RootTimeManagerPid ! { addTimeListener, self() },

	% Creates blanck node entries to preserve the intended node order:
	ResourcesPerNodeEntries = [ { N, undefined, undefined, undefined }
								|| N <- Nodes ],

	% Declares a curve corresponding to each computing node on all relevant
	% probes:
	%
	MultiNodesProbes =  [ ?getAttr(nodes_in_tick_probe),
						  ?getAttr(nodes_in_time_probe) ],

	[ [ begin

			NodeCurveName = text_utils:format( "Computing Node ~ts", [ N ] ),
			P ! { addCurve, [ NodeCurveName ] }

		end || N <- ComputingNodes ]

	  || P <- MultiNodesProbes ],

	% The probe creation requires the static information to be already
	% available (waited after the getStaticResourceInformation/2 calls):
	%
	UpdatedResourcesPerNodeEntries = wait_static_node_info(
		   ResourcesPerNodeEntries, InstanceTrackers, UserNode,
		   ?getAttr(tracker_result_dir) ),

	% Triggers the first update, before any instance is created yet, as the
	% ticker will wait for a full period before issuing its first request:
	%
	self() ! record_new_sample,

	TickerState = launch_ticker( ?getAttr(ticker_period), State ),

	StartedState = setAttributes( TickerState, [
			{ started, true },
			{ instance_trackers, InstanceTrackers },
			{ resources_per_node_entries, UpdatedResourcesPerNodeEntries },
			{ root_time_manager_pid, RootTimeManagerPid },
			{ load_balancer_pid, LoadBalancerPid } ] ),

	wooper:return_state( StartedState ).



% Waits for the static node information to be received, and updates accordingly
% the node entries (with static information to probes).
%
wait_static_node_info( ResourcesPerNodeEntries, _InstanceTrackers=[],
					   _UserNode, _TrackerDir ) ->
	ResourcesPerNodeEntries;

wait_static_node_info( ResourcesPerNodeEntries, InstanceTrackers, UserNode,
					   TrackerDir ) ->

	receive

		{ wooper_result, { NodeName, NodeStaticInfo, InstanceTrackerPid } } ->

			{ TickProbe, TimeProbe } = create_node_resource_probes(
						NodeName, NodeStaticInfo, UserNode, TrackerDir ),

			UpdatedEntry = { NodeName, NodeStaticInfo, TickProbe, TimeProbe },

			% We update in-place a blanck entry:
			UpdatedResourcesPerNodeEntries = lists:keyreplace( _Key=NodeName,
					   _Index=1, ResourcesPerNodeEntries, UpdatedEntry ),

			RemainingInstanceTrackers = list_utils:delete_existing(
									  InstanceTrackerPid, InstanceTrackers ),

			wait_static_node_info( UpdatedResourcesPerNodeEntries,
				RemainingInstanceTrackers, UserNode, TrackerDir )

	after ?general_timeout_duration ->

			throw( { time_out, instance_trackers, InstanceTrackers } )

	end.



% To be called directly to stop the performance tracker, supposedly already
% started.
%
-spec stop( wooper:state() ) -> oneway_return().
stop( State ) ->

	case ?getAttr(root_time_manager_pid) of

		undefined ->
			ok;

		RootTimeManagerPid ->
			[ RootTimeManagerPid ! { RemoveOneway, self() }
			  || RemoveOneway <- [ removeTimeListener,
								   removeSimulationListener ] ]

	end,

	UpdatedState = stop_ticker( State ),

	wooper:return_state( UpdatedState ).



% Called to stop (synchronously) the tracker sending of data to its probes.
%
% (helper)
%
-spec stop_ticker( wooper:state() ) -> wooper:state().
stop_ticker( State ) ->

	case ?getAttr(ticker_pid) of

		undefined ->
			%?warning( "Stop ticker request ignored: was not running." ),
			State;

		TickerPid ->

			?notice( "Stopping the ticker." ),

			TickerPid ! { stop, self() },

			receive

				stopped ->
					ok

			after ?general_timeout_duration ->

					throw( { time_out, ticker, TickerPid } )

			end,

			% However there might be already 'record_new_sample' messages
			% sitting in the mailbox, let's flush them:
			%
			basic_utils:flush_pending_messages( record_new_sample ),

			setAttribute( State, ticker_pid, undefined )

	end.



% Sets the ticker period, in milliseconds.
%
% The performance tracker will send data to its probes at this pace.
%
% Default period is 1s (1000 milliseconds).
%
-spec setTickerPeriod( wooper:state(), milliseconds() ) -> oneway_return().
setTickerPeriod( State, TickerPeriod ) ->

	% Changes dynamically the ticker frequency:
	StoppedState = stop_ticker( State ),

	TickerState = launch_ticker( TickerPeriod, StoppedState ),

	wooper:return_state( TickerState ).



% Notifies this tracker (as a time listener) that a new tick is being scheduled.
-spec onNewTick( wooper:state(), tick_offset() ) -> oneway_return().
onNewTick( State, NewTickOffset ) ->
	wooper:return_state(
				 setAttribute( State, current_tick_offset, NewTickOffset ) ).



% Notifies this tracker (as a time listener) that a new diasca is being
% scheduled.
%
-spec onNewDiasca( wooper:state(), tick_offset(), diasca() ) ->
							const_oneway_return().
onNewDiasca( State, _TickOffset, _NewDiasca ) ->
	% No-op, the performance tracker does not track diascas currently:
	wooper:const_return().




% Called to generate all performance monitoring reports.
-spec generateMonitoringReports( wooper:state() ) ->
										request_return( 'report_generated' ).
generateMonitoringReports( State ) ->

	?debug( "Generating performance monitoring reports." ),

	% Checking we are the one (a bit useless):
	% true = (get_tracker() =:= self()),

	% Done synchronously:
	StoppedState = stop_ticker( State ),

	% Generated in parallel:
	Message = { generateReport, _DisplayWanted=false, self() },

	ProbeList = send_to_all_probes( Message, StoppedState ),

	%trace_utils:debug_fmt(
	%  "Requesting the generation of report for ~B probe(s): ~p.",
	%  [ length( ProbeList ), ProbeList ] ),

	basic_utils:wait_for( _Message={ wooper_result, probe_report_generated },
		 _Count=length( ProbeList ), _Duration=2000,
		 "Still waiting for ~B probe()s to complete their report generation." ),

	?notice( "Performance report correctly generated." ),

	case executable_utils:is_batch() of

		true ->
			ok;

		false ->
			executable_utils:browse_images_in( ?getAttr(tracker_result_dir) )

	end,

	%trace_utils:debug( "Performance monitoring reports generated." ),

	wooper:return_state_result( StoppedState, report_generated ).




% Section for static methods.


% Returns the PID of the performance tracker (if any).
-spec get_tracker() -> static_return( tracker_pid() | 'not_registered' ).
get_tracker() ->

	RegisteredName = get_registration_name( ?performance_tracker_name ),

	wooper:return_static( naming_utils:is_registered( RegisteredName ) ).



% Helper functions.


-spec create_node_resource_probes( atom_node_name(),
		system_utils:host_static_info(), atom_node_name(), directory_path() ) ->
										 { probe_pid(), probe_pid() }.
create_node_resource_probes( NodeName, NodeStaticInfo, UserNode,
							 TrackerResultDir ) ->

	TotalRam = system_utils:interpret_byte_size_with_unit(
				 NodeStaticInfo#host_static_info.total_ram ),

	TotalSwap = system_utils:interpret_byte_size_with_unit(
				  NodeStaticInfo#host_static_info.total_swap ),

	TotalUsed  = text_utils:format( "Total Memory Used (over a total of ~ts)",
									[ TotalRam ] ),


	ErlangVersion = NodeStaticInfo#host_static_info.erlang_version,

	ErlangUsed = text_utils:format(
		"Memory Used by the Erlang VM (version ~ts)", [ ErlangVersion ] ),

	SwapUsed = text_utils:format( "Swap Used (in GiB, over a total of ~ts)",
								  [ TotalSwap ] ),

	CPUUtilization = "Percentage of CPU Used (Non-idle)",

	ProcessCount = text_utils:format(
						"Erlang Process Count (spread over ~B cores)",
						[ NodeStaticInfo#host_static_info.core_count ] ),

	CurveNames = [ TotalUsed, ErlangUsed, SwapUsed, CPUUtilization,
				   ProcessCount ],

	Zones = [ { "Total available memory (free+buffers+cache)",
				{ 'abscissa_top', TotalUsed } },

			  { "Memory used by all other programs",
				{ ErlangUsed, TotalUsed } },

			  { text_utils:format(
				  "Memory used for the simulation (over a total of ~ts)",
				  [ TotalRam ] ),

				{ 'abscissa_bottom', ErlangUsed } } ],


	% Do not suppose anything about this tracker location:
	NodeDescription = case UserNode of

		NodeName ->
			"user";

		_ ->
			"computing"

	end,

	ProbeOptions = #probe_options{ create_command_file_initially=false },

	ResourceOverTickProbe = class_Probe:create_facility_probe(

		 { _TickProbeName=text_utils:format( "Resource Consumption Probe Over "
				"Simulation Time for ~ts node ~ts",
				[ NodeDescription, NodeName ] ),
		   ProbeOptions },
		 CurveNames,
		 Zones,
		 _TickTitle=text_utils:format( "Monitoring memory and swap "
				"consumptions on ~ts node ~ts over wall-clock time",
				[ NodeDescription, NodeName ] ),
		 _TickXLabel="Simulation duration, in simulation ticks",
		 _TickYLabel="Percentages of Memory Consumption, "
					 "Swap and CPU Use and Process Count",
		TrackerResultDir ),

	[ ResourceOverTickProbe ! M || M <- [
			{ setKeyOptions, [ "bmargin center" ] },
			{ setOrdinateRange, [ 0, 100 ] } ,
			{ setPlotStyle, [ "linespoints" ] },
			setRotatedTickLabels,
			{ setPointSize, [ 2, _GenerateFile=true ] } ] ],


	ResourceOverTimeProbe = class_Probe:create_facility_probe(

		 { _TimeProbeName=text_utils:format( "Resource Consumption Probe Over "
			"Wallclock Time for ~ts node ~ts",
			[ NodeDescription, NodeName ] ),
		   ProbeOptions },
		 CurveNames,
		 Zones,
		 _TimeTitle=text_utils:format( "Monitoring memory and swap "
				"consumptions on ~ts node ~ts  over wall-clock time",
				[ NodeDescription, NodeName ] ),
		 _TimeXLabel="Wall-clock duration, in milliseconds",
		 _TimeYLabel="Percentages of Memory Consumption, "
					 "Swap and CPU Use and Process Count",
		TrackerResultDir ),

	[ ResourceOverTimeProbe ! M || M <- [
			{ setKeyOptions, [ "bmargin center" ] },
			{ setOrdinateRange, [ 0, 100 ] } ,
			{ setPlotStyle, [ "linespoints" ] },
			setRotatedTickLabels,
			{ setPointSize, [ 2, _GenerateFile=true ] } ] ],

	{ ResourceOverTickProbe, ResourceOverTimeProbe }.





% Creates the two probes used to monitor per-node process and instance
% creations, over simulation ticks and over wallclock time.
%
% Each of these two probes will monitor all nodes at once (in a single plot),
% regarding instance count.
%
% Once the computing nodes will be known, the corresponding curves will be added
% to these probes.
%
-spec create_node_probes( directory_path() ) -> { probe_pid(), probe_pid() }.
create_node_probes( TrackerResultDir ) ->

	ProbeOptions = #probe_options{ create_command_file_initially=true },

	InstancesPerNodeInTickProbe = class_Probe:create_facility_probe(
			{ _TickProbeName="Per Node Instance Count Over Tick Probe",
			  ProbeOptions },
			_TickCurveNames=[ "Overall Instance Count" ],
			_TickZones=[],
			_TickTitle="Monitoring the Overall and Per-Node Instance Count "
					   "Over Simulation Ticks",
			_TickXLabel="Simulation time, in ticks",
			_TickYLabel="Instance Count",
			TrackerResultDir ),


	InstancesPerNodeInTimeProbe = class_Probe:create_facility_probe(
			 { _TimeProbeName="Per Node Instance Count Over Time Probe",
			   ProbeOptions },
			_TimeCurveNames=[ "Overall Instance Count" ],
			_TimeZones=[],
			_TimeTitle="Monitoring the Overall and Per-Node Instance Count "
					   "Over Wall-clock Time",
			_TimeXLabel="Wall-clock duration, in milliseconds",
			_TimeYLabel="Instance Count",
			TrackerResultDir ),

	{ InstancesPerNodeInTickProbe, InstancesPerNodeInTimeProbe }.



% Creates the two probes used to monitor per-class instance creations over
% simulation ticks and over wallclock time.
%
-spec create_class_probes( directory_path() ) -> { probe_pid(), probe_pid() }.
create_class_probes( TrackerResultDir ) ->

	ProbeOptions = #probe_options{ create_command_file_initially=false },

	InstancesPerClassInTickProbe = class_Probe:create_facility_probe(
			{ _TickProbeName="Per Class Instance Count Over Tick Probe",
			  ProbeOptions },
			_TickCurveNames=[ "Overall Instance Count" ],
			_TickZones=[],
			_TickTitle="Monitoring the Overall and Per-Class Instance Count "
					   "Over Simulation Ticks",
			_TickXLabel="Simulation time, in ticks",
			_TickYLabel="Instance Count",
			TrackerResultDir ),

	InstancesPerClassInTimeProbe = class_Probe:create_facility_probe(

			{ _TimeProbeName="Per Class Instance Count Over Time Probe",
			  ProbeOptions },
			_TimeCurveNames=[ "Overall Instance Count" ],
			_TimeZones=[],
			_TimeTitle="Monitoring the Overall and Per-Class Instance Count "
					   "Over Wall-clock Time",
			_TimeXLabel="Wall-clock duration, in milliseconds",
			_TimeYLabel="Instance Count",
			TrackerResultDir ),

	[ P ! { setRotatedTickLabels, _GenerateFile=true } ||
		P <- [ InstancesPerClassInTickProbe, InstancesPerClassInTimeProbe ] ],

	{ InstancesPerClassInTickProbe, InstancesPerClassInTimeProbe }.



% Main loop of the ticker process.
ticker_main_loop( PerformanceTrackerPid, TickerPeriod ) ->

	receive

		{ stop, CallerPid } ->
			%trace_utils:debug_fmt( "The performance ticker received a stop "
			%                       "request." ),
			CallerPid ! stopped

	after TickerPeriod ->

			%trace_utils:debug_fmt( "Tick!" ),

			% Requests, based on wall-clock time, the tracker to update its
			% statistics:
			%
			PerformanceTrackerPid ! record_new_sample,

			ticker_main_loop( PerformanceTrackerPid, TickerPeriod )

	end.



% Returns the registering name of the performance tracker.
get_registration_name( PerformanceName ) when is_atom( PerformanceName ) ->
	PerformanceName.




% The following methods are called to set curves data and to send the data to
% the performance probe, at each ticker period (in wall-clock time).



% Called to send updated data to performance probes.
-spec record_new_sample( wooper:state() ) -> oneway_return().
record_new_sample( State ) ->

	% First, requests the information from the load balancer, early as we need
	% the current tick offset first for all curves:
	%
	LoadBalancerPid = ?getAttr(load_balancer_pid),

	% Note that a race condition may exist with the ending of the simulation:
	% the load balancer, as all actors, may then be removed whereas the
	% notification (simulation_stopped) has not reached yet this tracker; this
	% is why a time-out happening while waiting for this call is legit.
	%
	LoadBalancerPid ! { getInstanceCounts, [], self() },

	% Same thing (in parallel) for node-related information:

	Request = { getDynamicResourceInformation, [], self() },

	InstanceTrackers = ?getAttr(instance_trackers),

	[ I ! Request || I <- InstanceTrackers ],

	{ CurrentWallclockTime, _T } = statistics( wall_clock ),

	% First, wait for the information from the load balancer, notably the tick
	% offset (set to zero if not defined):
	%
	% (may wait indefinitively if we are at the end of the simulation, whereas
	% the load-balancer has been destroyed, as all actors)
	%
	receive

		{ wooper_result,
		  { instance_counts, InstancesPerClass, InstancesPerNode } } ->

			CurrentTickOffset = case ?getAttr(current_tick_offset) of

				undefined ->
					0;

				Tick ->
					Tick

			end,

			%trace_utils:debug_fmt( "Tick offset #~p: InstancesPerClass=~p, "
			% "InstancesPerNode=~p.", [ CurrentTickOffset,
			% InstancesPerClass, InstancesPerNode ] ),

			{ ClassState, Count } = manage_class_monitoring( CurrentTickOffset,
				  CurrentWallclockTime, InstancesPerClass, State ),

			% Note the pattern-matching on Count, to check correctness of
			% instance count:
			%
			{ NodeState, Count } = manage_node_monitoring( CurrentTickOffset,
					CurrentWallclockTime, InstancesPerNode, ClassState ),

			% Unsorted list:
			NodeDynInfos = wait_dynamic_info_from_trackers( InstanceTrackers,
										NodeState, _Acc=[] ),

			LocalState = manage_node_dynamic_info( CurrentTickOffset,
					CurrentWallclockTime, NodeDynInfos, NodeState ),

			wooper:return_state( LocalState )


		after 1000 ->

			% This time-out is not an error case: probably that the load
			% balancer has already been removed as all actors at simulation end;
			% this call is simply ignored then, yet we have to flush the answers
			% from the instance trackers:
			%
			_NodeDynInfos = wait_dynamic_info_from_trackers( InstanceTrackers,
															 State, _Acc=[] ),

			% However there might be already other 'record_new_sample' messages
			% sitting in the mailbox, let's flush them as well:
			%
			basic_utils:flush_pending_messages( record_new_sample ),

			wooper:const_return()

	end.




% Helper functions in charge of recording new samples:


% Manages the per-class instance monitoring.
%
% Returns an updated state and the total instance count.
%
manage_class_monitoring( CurrentTickOffset, CurrentWallclockTime,
						 InstancesPerClass, State ) ->

	OrderedClasses = ?getAttr(ordered_classnames),


	% Computes the data sample in the class order as was discovered:
	%
	% InstancesPerClass is a list of {Classname, {CreationCount,DeletionCount}}
	% entries.
	%
	% We want to sort it in the order of the classes in OrderedClasses, and
	% replace the two counters by one, the current number of instances of that
	% class, i.e. CreationCounter - DeletionCounter.
	%
	% Note that new classes may appear in InstancesPerClass (while not being
	% known of OrderedClasses yet) and that the first element of OrderedClasses
	% is (implicitly - it does not exist there) the total number of instances.

	% Sort classes:
	{ TotalCount, SortedExistingClasses, NewClasses } =
		sort_classes( InstancesPerClass, OrderedClasses ),


	TickProbe = ?getAttr(classes_in_tick_probe),

	TimeProbe = ?getAttr(classes_in_time_probe),

	Probes = [ TickProbe, TimeProbe ],

	% Let's now declare the new classes:
	[ begin

		 Classname = text_utils:atom_to_string( C ),
		 [ P ! { addCurve, [ [ Classname ] ] } || P <- Probes ]

	  end || { C, _Count } <- NewClasses ],

	% And finally feed the probes with a full corresponding sample:
	AllSortedPairs = SortedExistingClasses ++ NewClasses,

	ExtractedValues = [ V || { _C, V } <- AllSortedPairs ],

	SampleValues = list_to_tuple( [ TotalCount | ExtractedValues ] ),

	TickProbe ! { setData, [ CurrentTickOffset, SampleValues ] },

	TimeProbe ! { setData, [ CurrentWallclockTime, SampleValues ] },

	{ setAttribute( State, ordered_classnames,
					[ C || { C, _V } <- AllSortedPairs ] ),
	  TotalCount }.



% Sorts InstancesPerClass in the order in OrderedClasses, and determines the
% list of classes that were not known.
%
% Returns {TotalCount, SortedExistingClasses, NewClasses}, an overall instance
% count and the two lists (one ordered, one listing the new classes), each made
% of {Classname,InstanceCount} entries.
%
sort_classes( InstancesPerClass, OrderedClasses ) ->

	% First, computes the current instance count for each class, and sums it
	% also:
	%
	{ NewInstancesPerClass, OverallCount } = lists:foldl(

			fun( { Classname, { CreationCount, DeletionCount } },
				 _Acc={ ClassList, TotalCount } ) ->
					ExistingCount = CreationCount - DeletionCount,
					NewClassList = [ { Classname, ExistingCount } | ClassList ],
					NewTotalCount = TotalCount + ExistingCount,
					_NewAcc={ NewClassList, NewTotalCount }

			end,
			_FirstAcc={ _ClassList=[], _TotalCount=0 },
			_List=InstancesPerClass ),

	% Now re-order and split:
	{ SortedExistingClasses, NewClasses } = reorder_classes(
					NewInstancesPerClass, OrderedClasses, _ExistingAcc=[] ),

	{ OverallCount, SortedExistingClasses, NewClasses }.



% We iterate here over the ordered classes, moving the found classes from
% InstancesPerClass to ExistingAcc; the remaining ones are by design the new
% ones.
%
% Here we exhausted the known classes:
reorder_classes( InstancesPerClass, _OrderedClasses=[], ExistingAcc ) ->

	% Existing classes will then be correctly reordered:
	SortedExistingClasses = lists:reverse( ExistingAcc ),

	% InstancesPerClass is then a list corresponding to new classes:
	{ SortedExistingClasses, _NewClasses=InstancesPerClass };


reorder_classes( InstancesPerClass, _OrderedClasses=[ Class | T ],
				 ExistingAcc ) ->

	% Finds and removes 'Class':
	case lists:keytake( _Key=Class, _Index=1, InstancesPerClass ) of

		{ value, ClassEntry, NewInstancesPerClass } ->
			reorder_classes( NewInstancesPerClass, T,
							 [ ClassEntry | ExistingAcc ] );

		false ->
			throw( { class_not_found, Class, InstancesPerClass } )

	end.



% Waits for all instance trackers to answer, returns the list of corresponding
% node information records.
%
-spec wait_dynamic_info_from_trackers( [ instance_tracker_pid() ],
		wooper:state(), [ host_dynamic_info() ] ) -> [ host_dynamic_info() ].
wait_dynamic_info_from_trackers( _InstanceTrackers=[], _State, Acc ) ->
	Acc;

wait_dynamic_info_from_trackers( InstanceTrackers, State, Acc ) ->

	receive

		{ wooper_result, { NodeDynInfo, TrackerPid } } ->

			RemainingTrackers =
				list_utils:delete_existing( TrackerPid, InstanceTrackers ),

			wait_dynamic_info_from_trackers( RemainingTrackers, State,
											 [ NodeDynInfo | Acc ] )

	after ?general_timeout_duration ->

			?warning_fmt( "Still waiting for the dynamic information "
						  "from instance trackers ~p.", [ InstanceTrackers ] ),

			wait_dynamic_info_from_trackers( InstanceTrackers, State, Acc )

	end.



% Manages the per-node instance monitoring.
%
% Returns an updated state and the total instance count.
%
manage_node_monitoring( CurrentTickOffset, CurrentWallclockTime,
						InstancesPerNode, State ) ->

	% Otherwise would include the user node (in first position):
	[ { _UserNode, _Static, _P1, _P2 } | OrderedNodes ] =
		?getAttr(resources_per_node_entries),

	%trace_utils:debug_fmt( "manage_node_monitoring: OrderedNodes=~p",
	%						[ OrderedNodes ] ),

	% By design there is no instance on the user node:
	NewInstancesPerNode =
		reorder_nodes( InstancesPerNode, OrderedNodes, _Acc=[] ),

	ExtractedValues = [ V || { _C, V } <- NewInstancesPerNode ],

	TotalCount = lists:sum( ExtractedValues ),

	SampleValues = list_to_tuple( [ TotalCount | ExtractedValues ] ),

	%trace_utils:debug_fmt( "manage_node_monitoring: SampleValues = ~p.",
	%						[ SampleValues ] ),

	TickProbe = ?getAttr(nodes_in_tick_probe),

	TimeProbe = ?getAttr(nodes_in_time_probe),

	TickProbe ! { setData, [ CurrentTickOffset, SampleValues ] },

	TimeProbe ! { setData, [ CurrentWallclockTime, SampleValues ] },

	{ State, TotalCount }.



% We iterate here over the ordered nodes, moving the found nodes from
% InstancesPerNode to ExistingAcc.
%
% Here we exhausted the known nodes:
% (note that the two lists should be exhausted simultaneously)
%
reorder_nodes( _InstancesPerNode=[], _OrderedNodes=[], Acc ) ->
	% Returns a properly reordered {Node,InstanceCount} list:
	lists:reverse( Acc );

reorder_nodes( InstancesPerNode,
	   _OrderedNodes=[ { NodeName, _NodeStaticInfo, _Probe1, _Probe2 } | T ],
	   Acc ) ->

	% Finds and removes 'Node':
	case lists:keytake( _Key=NodeName, _Index=1, InstancesPerNode ) of

		{ value, NodeEntry, NewInstancesPerNode } ->
			reorder_nodes( NewInstancesPerNode, T, [ NodeEntry | Acc ] );

		false ->
			throw( { node_not_found, NodeName, InstancesPerNode } )

	end.



% Manages the per-node resource monitoring (RAM and swap).
%
% Returns an updated state.
%
manage_node_dynamic_info( CurrentTickOffset, CurrentWallclockTime,
						  NodeDynInfos, State ) ->

	% Iterates on the list of node entries and feeds appropriately the
	% corresponding probes:

	% trace_utils:debug_fmt( "manage_node_dynamic_info at tick #~p: "
	%						"NodeDynInfos=~p, NodeEntries = ~p.~n",
	%			 [ CurrentTickOffset, NodeDynInfos,
	%			   ?getAttr(resources_per_node_entries) ] ),

	manage_dyn_nodes( ?getAttr(resources_per_node_entries), NodeDynInfos,
					  CurrentTickOffset, CurrentWallclockTime ),

	State.


% We iterate on the records as we want to use 'keytake' for the node list:
manage_dyn_nodes( _NodeEntries=[], _NodeDynInfos=[], _CurrentTickOffset,
				  _CurrentWallclockTime ) ->
	ok;

manage_dyn_nodes( NodeEntries,
				  _NodeDynInfos=[ #host_dynamic_info{
			node_name=NodeName,
			swap_used=Swap,
			cpu_usage=CPUPercentages,
			ram_use={ PercentRamUsedBySimulation, PercentRamUsedByOthers },
			process_count=ProcessCount } | T ],
				  CurrentTickOffset,
				  CurrentWallclockTime ) ->

	TotalUsed = PercentRamUsedBySimulation + PercentRamUsedByOthers,

	% For the moment we just focus on an aggregated non-idle use:
	% (undefined might be returned, the probe manages this natively)
	%
	CPUPercent = system_utils:compute_cpu_usage_for( CPUPercentages ),

	SampleValues = { TotalUsed, PercentRamUsedBySimulation,
					 Swap, CPUPercent, ProcessCount },

	% Now retrieves the corresponding probes:
	{ value,  { NodeName, _NodeStaticInfo, TickProbePid, TimeProbePid },
	  NewNodeEntries } = lists:keytake( _K=NodeName, _Index=1, NodeEntries ),

	TickProbePid ! { setData, [ CurrentTickOffset, SampleValues ] },

	TimeProbePid ! { setData, [ CurrentWallclockTime, SampleValues ] },

	manage_dyn_nodes( NewNodeEntries, T, CurrentTickOffset,
					  CurrentWallclockTime );

% Catch-all crash clause:
manage_dyn_nodes( NodeEntries, NodeDynInfos, CurrentTickOffset,
				  CurrentWallclockTime ) ->

	throw( { node_inconsistency, NodeEntries, NodeDynInfos, CurrentTickOffset,
			 CurrentWallclockTime } ).



% Sends the specified message to all probes used by this tracker, returns the
% list of their PIDs.
%
% (helper)
%
-spec send_to_all_probes( any(), wooper:state() ) -> [ probe_pid() ].
send_to_all_probes( Message, State ) ->

	SingleProbeList = get_all_probes( State ),

	% More parallel by chunks:
	[ P ! Message || P <- SingleProbeList ],

	% Returns the full PID list:
	lists:foldl(
			fun( { _NodeName, _StaticInfos, TickProbe, TimeProbe }, Acc ) ->
				TickProbe ! Message, TimeProbe ! Message,
				[ TickProbe, TimeProbe | Acc ]
			end,
			_InitialAcc=SingleProbeList,
			?getAttr(resources_per_node_entries) ).



% Launches the ticker process.
%
% Returns an updated state.
%
% (helper)
%
launch_ticker( TickerPeriod, State ) ->

	% Allows to track the wall-clock time:
	% (closure used to avoid exporting the function)

	%trace_utils:debug_fmt( "Creating a ticker process whose period "
	%                       "is ~B ms.", [ TickerPeriod ] ),

	% Beware of closures!
	TrackerPid = self(),

	% Creating a ticker process, to trigger regular data updates:
	TickerPid = ?myriad_spawn_link( fun() ->
								ticker_main_loop( TrackerPid, TickerPeriod )
									end ),

	setAttributes( State, [ { ticker_pid, TickerPid },
							{ ticker_period, TickerPeriod } ] ).



% Message callbacks sent by the root time manager and interpreted as oneways,
% knowing that a performance tracker is a simulation listener:


% (not of interest here)
-spec simulation_started( wooper:state() ) -> const_oneway_return().
simulation_started( State ) ->
	wooper:const_return().


% (not of interest here)
-spec simulation_suspended( wooper:state() ) -> const_oneway_return().
simulation_suspended( State ) ->
	wooper:const_return().


% (not of interest here)
-spec simulation_resumed( wooper:state() ) -> const_oneway_return().
simulation_resumed( State ) ->
	wooper:const_return().


% (not of interest here)
-spec simulation_succeeded( wooper:state() ) -> const_oneway_return().
simulation_succeeded( State ) ->
	wooper:const_return().


-spec simulation_stopped( wooper:state() ) -> oneway_return().
simulation_stopped( State ) ->

	% The simulation is stopped, hence we must not interact anymore with actors
	% (for example with the load balancer)
	%
	StopState = stop_ticker( State ),

	wooper:return_state( StopState ).




% Returns a list of the PIDs of all probes.
-spec get_all_probes( wooper:state() ) -> [ probe_pid() ].
get_all_probes( State ) ->
	[ ?getAttr(nodes_in_tick_probe), ?getAttr(nodes_in_time_probe),
	  ?getAttr(classes_in_tick_probe), ?getAttr(classes_in_time_probe) ].



% Hooks for serialisation/deserialisation.

% We do not want the serialiser function to see the PIDs of the local worker
% process (the ticker one), as of course they are not meant to be resolved by
% the instance tracker (this would fail).



% Triggered just before serialisation.
%
% The state used here is dedicated to serialisation (i.e. it is not the actual
% state).
%
-spec pre_serialise_hook( wooper:state() ) -> wooper:state().
pre_serialise_hook( State ) ->

	% Just one here, the ticker:
	PrivateProcesses = [ ticker_pid ],

	% In this state, private processes have been replaced by restoration
	% markers:
	%
	wooper_serialisation:handle_private_processes( PrivateProcesses, State ).



% Triggered just after serialisation, based on the selected entries.
%
% The value returned by this hook will be converted "as is" into a binary, that
% will be written.
%
-spec post_serialise_hook( classname(), term_serialisation(),
						   wooper:state() ) -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	{ Classname, Entries }.



% Triggered just before deserialisation.
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
									term_serialisation().
pre_deserialise_hook( _SerialisationTerm={ _Classname, Entries }, _UserData ) ->
	Entries.



% Triggered just after deserialisation.
-spec post_deserialise_hook( wooper:state() ) -> wooper:state().
post_deserialise_hook( State ) ->

	% We have to recreate all private helper processes that were running,
	% i.e. just the ticker one:
	%
	case ?getAttr(ticker_pid) of

		undefined ->
			State;

		?process_restoration_marker ->
			% We restore the ticker (of course the wallclock time will register
			% a sudden delay):
			%
			launch_ticker( ?getAttr(ticker_period), State )

	end.
