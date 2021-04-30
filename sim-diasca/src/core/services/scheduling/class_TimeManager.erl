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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


-module(class_TimeManager).


-define( class_description,
		 "Distributed management of simulation time. "
		 "The time manager is expected to be a singleton on each computing "
		 "node. "
		 "Its process is registered locally under the name returned by the "
		 "get_registration_name/0 static method. "
		 "This is a time-driven simulation, actors are expected to be "
		 "synchronous. "
		 "See class_TimeManager_batch_test.erl and "
		 "class_TimeManager_interactive_test.erl." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).



% Helper functions:
-export([ get_current_tick_offset/1, get_current_tick/1,
		  get_simulation_time_and_date/1,
		  timestamp_to_ticks/2, ticks_to_timestamp/2,
		  display_waiting_reason/1, merge_local_with/1 ]).


% Useful for testing directly from the shell:
-export([ test_spontaneous_lists/0, test_min_max_timestamps/0 ]).



% Design notes.
%
% A given tick will be scheduled exactly once.
%
% During a tick, a given actor may be spontaneously scheduled up to once.



% Simulation absolute tick (thus in virtual time):
%
% (their origin is year #0, at 00:00:00 of the Gregorian calendar, i.e. the
% Western calendar a.k.a. the Christian calendar - internationally the most
% widely used civil calendar)
%
% In the future, simulation-specific types of ticks are to be supported. Ticks
% could then be floats or other datastructures, provided that the same
% operations done on current ticks can be done, namely, mostly:
%
% - converting a duration expressed in wall-clock seconds into a difference of
% ticks
%
% - adding such a difference of ticks to a tick
%
-type tick() :: integer().



% Simulation tick offset (difference between two ticks).
%
% On a 32-bit computer, Erlang will automatically switch from 32-bit integers to
% arbitrary-sized integers after roughly one year of simulation time, when at
% 50Hz (see 'math:pow(2,32)/50/3600/24/365', divided by 2 if signed).
%
-type tick_offset() :: integer().


% A (signed) duration expressed in ticks (as a difference of them):
-type tick_duration() :: integer().


% Agenda associating to tick offsets a set (as unordered and with no duplicates)
% of actors to schedule spontaneously at the specified tick.
%
% Pairs are sorted according to increasing tick offset order.
%
% An agenda is a set in logical terms, but in practice it is actually a plain
% list (key operations are: updating one entry or popping the one with the
% smallest tick offset), which is managed so that it remains ordered and with up
% to one entry per tick offset.
%
% More precisely, it is an ordered list of pairs, whose first element is a tick
% offset, and second element is a set of actors, like (the set being represented
% here as a list) in: [{4,[Pid1,Pid2]}, {7,[Pid3]}, {8,[Pid1]}]; these actor
% sets are not basic lists, even if one of the most usual actions performed on
% them will be to iterate over them, as there will be nevertheless other
% random-access operations performed on them (notably testing for membership -
% since, at any tick, a given actor (PID) should not be specified more than
% once, or when withdrawing spontaneous actions, etc.)
%
-type agenda() :: [ { tick_offset(), set_utils:set( actor_pid() ) } ].



% A diasca is the count of logical steps gone through a given tick.
%
% At each tick, it starts at zero, and is incremented as many times as needed to
% resolve all causal exchanges that are to take place during this tick.
%
% As such, a diasca does not imply any specific duration in virtual time: we
% just know that, during a given tick T, all events generated during a diasca
% (i.e. all actor messages sent during this diasca) happened logically before
% all events generated during the next diasca (still on the same tick T)
%
% So when a diasca elapses, the overall logical clock does not progress at all.
%
% In the future, we plan to support actors actually accounting for sub-actors
% ("recursive, imbricated/nested actors") and consequently a support for time
% refinement (a diasca will be a id_utils:sortable_id/0).
%
% In this context, a diasca is either an usual diasca (ex: D=17, 18,
% etc.) or a tuple of positive integers allowing to introduce any number of
% diascas between any two of them.
%
% For example, should a diasca be needed between diascas 17 and 18, a logical
% moment labelled {17,1} would be introduced. Next one would be {17,2} and so
% on. If diascas had to be inserted in turn between these last two diascas, then
% {17,1,1}, {17,1,2}, etc. could be introduced.
%
%-type diasca() :: non_neg_integer() | tuple( non_neg_integer() ).
-type diasca() :: non_neg_integer() | tuple().



% A logical timestamp is the current time in the simulation (expressed thank to
% a tick offset) and current the number of diasca that were needed to resolve
% it (at least one if the tick is scheduled at all).
%
-type logical_timestamp() :: { tick_offset(), diasca() }.


% Units of virtual seconds, i.e. seconds in simulation time.
%
% Note: these are strictly positive floating-point values; ex: 0.001 seconds
% (1ms).
%
-type virtual_seconds() :: float().



% Used by time managers to report the next action they plan (their scheduling
% subtree included) to their parent time manager: either no action planned at
% all, or a new diasca (if at least one actor in their subtree sent an actor
% message on the current diasca), or a new (possibly distant) tick to schedule
% (the smaller they have in their subtree).
%
-type next_manager_action() :: 'no_planned_action'
							 | 'new_diasca_needed'
							 | tick_offset().


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type timestamp() :: time_utils:timestamp().

-type seconds() :: unit_utils:seconds().
-type any_seconds() :: unit_utils:any_seconds().
-type milliseconds() :: unit_utils:milliseconds().

-type actor_pid() :: class_Actor:actor_pid().
-type actor_count() :: class_Actor:actor_count().




% The tracking information sent by a time manager to its parent once a diasca is
% over.
%
% Respectively:
%
% - number of schedulings done (spontaneous actions if at diasca 0, otherwise
% triggered actors)
%
% - number of local processes
%
-type diasca_tracking_info() :: { schedule_count(), count() }.



% The main simulation events that may be listened to:
-type simulation_events() :: 'simulation_started'
						   | 'simulation_suspended'
						   | 'simulation_resumed'
						   | 'simulation_succeeded'
						   | 'simulation_stopped'.



% Describes whether the simulation is paced according to real time (thus
% interactive), possibly with a scale factor, or runs as fast as possible (thus
% batch):
%
-type simulation_interactivity_mode() :: 'interactive' | 'batch'.


% Number of schedulings:
-type schedule_count() :: non_neg_integer().


% Designates the PID of a time manager:
-type time_manager_pid() :: sim_diasca:agent_pid().

% Corresponds to a process listening to simulation-level events.
%
% Any process can register itself as a simulation listener. It will be then
% notified of all main simulation events by the sending of appropriate messages
% (see the simulation_events() type).
%
% Of course the listener can interpret these messages as oneway calls.
%
-type simulation_listener_pid() :: pid().


% Corresponds to a process listening to events related to logical scheduling
% (i.e. ticks and diascas); for example, a "tick listener".
%
% Logical time listener API:
%
% Such a time listener should implement the two oneway methods below:
%
% - onNewTick/2, i.e.:
%
%-spec onNewTick( wooper:state(), class_TimeManager:tick_offset() ) ->
%          oneway_return().
% onNewTick( State, NewTickOffset ) -> ...
%
%
% - onNewDiasca/3, i.e.:
%
%-spec onNewDiasca( wooper:state(), class_TimeManager:tick_offset(),
%          class_TimeManager:diasca() ) -> oneway_return().
% onNewDiasca( State, TickOffset, NewDiasca ) -> ...
%
-type logical_time_listener_pid() :: pid().


% Corresponds to a process listening to wallclock-timing events:
-type wallclock_time_listener_pid() :: pid().


-export_type([ tick/0, tick_offset/0, tick_duration/0, diasca/0,
			   logical_timestamp/0, virtual_seconds/0, simulation_events/0,
			   simulation_interactivity_mode/0, schedule_count/0,
			   time_manager_pid/0,
			   simulation_listener_pid/0, logical_time_listener_pid/0,
			   wallclock_time_listener_pid/0 ]).


% The time, in milliseconds, to wait between two nominal console outputs of the
% time tracker:
%
%-define( nominal_wait_time, 1000 ).
% Smoother, even though with more notifications:
-define( nominal_wait_time, 800 ).


% The time, in milliseconds, to wait between two console outputs of the
% time tracker when still evaluating the same diasca:
%
-define( same_diasca_wait_time, 5000 ).



% Local (non-exported) types:


% To diagnose causes of any low performance:
-type diagnosis() :: 'none_waited' | [ binary() ].


% To display relevant time information on the console:
%
% (respectively: simulation date and time, tick and diasca, wallclock date and
% time)
%
-type timing_info() :: { ustring(), ustring(), tick_offset(), diasca(),
						 ustring(), ustring() }.


% To display relevant time information on the console:
%
% (respectively: total actor count, total scheduled count, total process count)
%
-type count_info() :: { actor_count(), count(), count() }.


% The attributes that are specific to a time-manager instance are:
-define( class_attributes, [

	{ parent_manager_pid, maybe( time_manager_pid() ),
	  "the PID of the parent time manager of this manager (if any, otherwise "
	  "set to 'undefined', in which case the current time manager is the root "
	  "one" },

	{ child_managers, set_utils:set( time_manager_pid() ),
	  "the set (unordered, with no duplicates) of the direct child managers "
	  "of this manager" },

	{ known_local_actors, set_utils:set( actor_pid() ),
	  "the set of the PIDs of all known actors that are directly managed by "
	  "this time manager; useful to notify them for example that the "
	  "simulation starts (it is a set, as for example we have to ensure that "
	  "an actor is not already subscribed before subscribing it)" },

	{ load_balancer_pid, load_balancer_pid(), "the PID of the load balancer "
	  "(useful to keep track of actor overall count for example)" },

	{ started, boolean(), "tells whether the time manager is running" },

	{ initial_tick, tick(), "the (absolute) simulation tick at which the time "
	  "manager will start, whenever asked to start; this corresponds to the "
	  "actual beginning of the simulation" },

	{ initial_timestamp, timestamp(), "the wallclock timestamp "
	  "corresponding to the moment the time manager was requested to start" },

	{ current_tick_offset, tick_offset(), "the offset, expressed as a number "
	  "of ticks, between the initial tick and the current one; we use mostly "
	  "offsets rather than absolute ticks for efficiency reasons" },

	{ current_diasca, diasca(),
	  "corresponds to the current diasca being evaluated" },

	{ spontaneous_agenda, agenda(), "the agenda  of the next simulation tick "
	  "offsets during which this time manager will have to send spontaneous "
	  "tops, to at least one actor" },

	{ previous_timestamp, logical_timestamp(), "the previous diasca "
	  "that was scheduled, so that tracking information can be correctly "
	  "associated to the right simulation moment" },

	{ next_timestamp, maybe( logical_timestamp() ),
	  "the expected new simulation timestamp, from the point of view of this "
	  "time manager; when being in {Talpha,Dalpha} it can be either "
	  "'undefined', or {Talpha,Dalpha+1} (if a new diasca is already known to "
	  "be needed) or {Tbeta,0} with Tbeta > Talpha otherwise" },

	{ next_action, next_manager_action(), "the soonest deadline that is known "
	  "of this manager (based on its scheduling subtree), equal to "
	  "'no_planned_action', to 'new_diasca_needed' or to an actual tick offset "
	  "to keep track of its soonest known deadline" },

	{ actors_to_trigger_in_one_diasca, set_utils:set( actor_pid() ),
	  "the set of actors that should be triggered on the next scheduled "
	  "diasca (stored in next_timestamp, whichever it is, whether or not this "
	  "time manager has already received its 'new diasca' message) because "
	  "they have received an actor message the current overall diasca; a local "
	  "actor is thus listed up to once, even if it received more than one "
	  "actor message on the corresponding diasca (this is ensured actor-side, "
	  "see schedule_trigger_already_sent, and also because the time managers "
	  "store them in a set, not a list)" },

	{ actors_to_trigger_in_two_diascas, set_utils:set( actor_pid() ),
	  "the set of actors that should be triggered on the diasca *after* the "
	  "next scheduled one, due to the intrinsic race condition described in "
	  "the implementation notes; a local actor must be listed up to once, "
	  "even if it received more than one actor message on the corresponding "
	  "diasca (this is ensured actor-side, see schedule_trigger_already_sent); "
	  "this is also a set, as it will be assigned to "
	  "actors_to_trigger_in_one_diasca afterwards" },

	{ watchdog_pid, maybe( pid() ),
	  "PID of the watchdog, if any, i.e. iff being the root time manager" },

	{ stop_tick_offset, maybe( tick_offset() ), "the tick offset at which the "
	  "simulation should end (if being in the root time manager and if "
	  "termination is based on a fixed timestamp in simulation time), "
	  "otherwise 'undefined'; only the root time manager may have a stop tick "
	  "offset defined" },

	{ simulation_listeners, [ simulation_listener_pid() ],
	  "a (plain) list of the PIDs of the processes which are to keep track of "
	  "the main simulation events, like start, stop, suspend, resume "
	  "transitions" },

	{ time_listeners, [ logical_time_listener_pid() ],
	  "a (plain) list of the PIDs of the processes that are to keep track of "
	  "the logical scheduling (i.e. ticks and diascas), while not being actors "
	  "(their onNewTick/2 and onNewDiasca/3 oneways are triggered "
	  "appropriately)" },

	{ wallclock_milestone_period, milliseconds(),
	  "the actual (wall-clock) duration between two wallclock milestones" },

	{ wallclock_milestone_listeners, [ wallclock_time_listener_pid() ],
	  "a (plain) list of the PIDs of the processes that are to be notified "
	  "whenever wallclock-based milestones are met (their "
	  "onWallclockMilestone/2 oneways are triggered appropriately), i.e. when "
	  "a sufficient time in the real world elapsed" },

	{ tick_milestone_period, tick_offset(), "the virtual (in simulation-time) "
	  "duration between two tick milestones" },

	{ tick_milestone_listeners, [ logical_time_listener_pid() ],
	  "a (plain) list of the PIDs of the processes that are to be notified "
	  "whenever tick-based milestones are met (their onTickMilestone/2 oneways "
	  "are triggered appropriately), i.e. when a sufficient time in the "
	  "simulated world elapsed" },

	{ suspended, boolean(),
	  "tells whether the simulation is currently suspended" },

	{ simulation_tick_duration, virtual_seconds(),
	  "tells the actual duration, in seconds (in virtual time) between two "
	  "simulation ticks (as a floating-point value)" },

	{ simulation_tick_waiting, milliseconds(),
	  "tells, when in interactive mode (with regard to simulation), "
	  "the real (i.e. wall-clock, user) duration, in milliseconds, which is "
	  "expected between two simulation ticks (possibly once scaled and "
	  "rounded)" },

	{ simulation_interactivity_mode, simulation_interactivity_mode(),
	  "discriminates between the 'interactive' and 'batch' simulation modes" },

	{ diasca_count, maybe( count() ), "keeps track of the number of all "
	  "diascas evaluated up to now, for example to measure concurrency" },

	{ schedule_count, maybe( count() ), "keeps track of the total number of "
	  "schedulings across all diascas, notably to be able to compute the "
	  "average concurrency at the end of a given case" },

	{ terminated_actors, [ actor_pid() ], "a plain list of actors that have "
	  "terminated (they have completed their termination procedure and can "
	  "thus be deleted at any time; actually their deletion is triggered at "
	  "the beginning of the next tick); does not seem to be actually used "
	  "currently (never set)" },

	{ terminating_actors, [ actor_pid() ], "a plain list (not a set: just "
	  "needing to add elements or iterate on them all) of actors that are "
	  "actively terminating (i.e. within a bounded number of diascas) this "
	  "diasca, but have not terminated yet; the list is established (reset "
	  "and rebuilt) at each diasca" },

	{ actors_to_delete_at_next_tick, [ actor_pid() ],
	  "a list of the PIDs of all actors that are terminating and whose "
	  "(deferred) deletion is to happen at next tick" },

	{ waited_child_managers, set_utils:set( time_manager_pid() ),
	  "the set of child managers that are still waited, for the current diasca "
	  "to finish; it is a set as well, since a simulation distributed over a "
	  "cluster may involve, say, 300+ nodes, if not 65000 nodes on a "
	  "Bluegene/Q?" },

	{ waited_spontaneous_actors, set_utils:set( actor_pid() ),
	  "the set of the actors whose spontaneous behaviour has been scheduled "
	  "this tick (as diasca 0) and that are still waited, for the current "
	  "diasca to finish" },

	{ waited_triggered_actors, set_utils:set( actor_pid() ),
	  "the set of the actors whose triggered behaviour has been scheduled this "
	  "diasca and that are still waited, for the current diasca to finish" },

	{ watchdog_waited, boolean(),
	  "tells whether the watchdog is still waited on this diasca" },

	{ waited_count, count(), "the number of 'done' notifications, coming "
	  "from scheduled actors, child managers and the watchdog, that are "
	  "waited this diasca (this cached count spares the need of polling "
	  "numerous sets each time a new 'done' message is received)" },

	{ interactive_tick_triggered, boolean(), "allows, in interactive mode, to "
	  "schedule a new tick only once, even if the timer sent multiple "
	  "timerTickFinished messages" },

	{ timer_pid, maybe( pid() ), "the PID of the timer process (if any), used "
	  "to keep track of real time when running in interactive mode; is set "
	  "iff being the root time manager and being in interactive mode" },

	{ wallclock_tracker_pid, maybe( pid() ), "the PID of the wallclock "
	  "tracker (only used for the root time manager)" },

	{ time_tracker_pid, maybe( pid() ), "the PID of the time tracker (if any; "
	  "only used for the root time manager)" },

	{ overall_actor_count, actor_count(), "the current total number of actors "
	  "in the simulation, as notified by the load balancer (only useful in the "
	  "root time manager)" },

	{ scheduled_tracking, actor_count(), "the number of actors to be "
	  "spontaneously scheduled (if being at diasca 0), or triggered (in "
	  "later diascas); it is the sum for all the scheduling subtree that "
	  "corresponds to this time manager (including itself), and it is updated "
	  "at each scheduled diasca" },

	{ process_tracking, count(), "the current count in terms of Erlang "
	  "processes, for all the scheduling subtree that corresponds to this "
	  "time manager (including itself); is updated at each scheduled tick" },

	{ root_data_exchanger_pid, maybe( data_exchanger_pid() ), "the PID of the "
	  "root data-exchanger (if any), with whom the root time manager may have "
	  "to interact" },

	{ root_instance_tracker_pid, maybe( instance_tracker_pid() ),
	  "the PID of the root instance tracker (if any), to resolve "
	  "instance-level issues" },

	{ local_instance_tracker_pid, instance_tracker_pid(),
	  "the PID of the local instance tracker (if any), i.e. the one running "
	  "on the same node as this time manager), to resolve instance-level "
	  "issues" },

	{ interdiasca_listeners, [ logical_time_listener_pid() ],
	  "a list containing the PIDs of all registered inter-diasca listeners "
	  "(ex: typically including the root data-exchanger)" },

	{ resilience_manager_pid, maybe( resilence_manager_pid() ),
	  "the PID of the resilience manager" },

	{ serialisation_requested, boolean(), "tells whether a serialisation is "
	  "to occur once the current diasca is over" } ] ).



% We need serialisation hooks to take care of internal helper processes:
-define( wooper_serialisation_hooks,).


% For silencing conditionally-unused functions:
-compile({ nowarn_unused_function, [ get_trace_timestamp/3,
				check_tick_consistency/2, check_diasca_consistency/3 ] }).


-include("engine_common_defines.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").




% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.TimeManagement" ).


% Allows to use macros for trace sending from static methods:
-include_lib("traces/include/traces.hrl").

% For the actor_info record:
-include("class_InstanceTracker.hrl").


% For talkative settings:
-include("simulation_settings.hrl").


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").



% To display additional (debug) information on the console:

% To disable as a whole:
-define( display_console( FormatString, Values ), ok ).

% To have the PID information about the emitter:
%-define( display_console( FormatString, Values ),
%		 trace_utils:debug_fmt( "[~w] " ++ FormatString,
%								[ self() | Values ] ) ).

% To additionally discriminate between PIDs having the same middle number, based
% on node:
%
%-define( display_console( FormatString, Values ),
%		 trace_utils:debug_fmt( "[~w on ~ts] " ++ FormatString,
%								[ self(), node() | Values ] ) ).

% To additionally discriminate between PIDs having the same middle number, based
% on host:
%
%-define( display_console( FormatString, Values ),
%		 trace_utils:debug_fmt( "[~w on ~ts] " ++ FormatString,
%								[ self(), net_adm:localhost() | Values ] ) ).



% For time_manager_name, virtual_seconds, etc.:
-include("class_TimeManager.hrl").



% Tells how the name of a time manager should be registered.
%
% Could be local_and_global or global_only as well, but we want here exactly one
% local time manager per computing node:
%
-define( registration_scope, local_only ).




% Use this instead, on a loaded computer, to test stall detection:
% (and/or add a call to timer:sleep/1 in a well-chosen actor oneway)
%
%-define( watchdog_wait_duration, 1 ).



% Simulations will start by default at a base (fixed) date, on Saturday, January
% 1st, 2000 at midnight:
%
% (use setInitialTick/2 or setInitialSimulationTimestamp/3 to change it if
% necessary)
%
-define( initial_simulation_date, { { 2000, 1, 1 }, { 0, 0, 0 } } ).



% Defines the threshold from which we deem that a list has too many entries to
% be displayed element per element.
%
-define( too_many_entries, 20 ).



% Implementation notes:
%
% - the general architecture of the time manager relies on the fact that:
%
%   - the communication from a time manager to an actor is fully asynchronous
%   (non-blocking; ex: triggerNameNotification/2 oneway)
%
%   - the communication from an actor to its time manager can (and usually
%   should) be synchronous (once again: not true the other way round)
%
% Otherwise deadlocks could happen: if each other sent a request to the other at
% roughly the same time, each could be blocked waiting for the other, until the
% end of time or until any declared time-out fires. Therefore the time manager
% should never block for an actor to answer to one of its logical requests
%
% - as much as possible, timings (simulation timestamps) are expressed as
% offsets to the simulation initial tick, rather than as absolute ticks (that
% are relative to year #0 of the Gregorian calendar), in order to preserve the
% performances; timing considerations in models should be expressed as actual
% durations (in virtual time), so that they do not depend on the chosen overall
% simulation frequency
%
% - spontaneous_agenda is actually an ordered list with no duplicates; we
% considered using an ordered_set or a gb_sets, but a plain list seems more
% appropriate, as only two operations are needed: insert or modify an entry, and
% pop first (leftmost, soonest) entry; so we use a custom, plain, ordered list,
% knowing that in most cases the list should not be very long (the agenda is
% indexed by future tick offsets, not by actors); inside this (plain) list of
% spontaneous entries, each element is a {Tick,ActorSet} pair, where ActorSet is
% a set (unordered, with no duplicates) of PIDs (see set_utils); we may have to
% search for a specific PID in it and also to iterate on its elements
%
% - on some other lists we just have to iterate; this reason still holds for the
% list of actors to be triggered on the next diasca
% (actors_to_trigger_in_{one,two}_diasca(s)), which are sets (as no duplicate
% shall exist, and terminating actors have to be picked from them)
%
% - all other actor-related lists (ex: waiting lists) are expected to hold
% possibly a very large number of elements (as usually one element corresponds
% to one actor), and some random access in them is required (ex: to remove a
% particular actor from a waiting list); moreover they are actually sets (not
% ordered, not having duplicates: generally we do not want to send a message
% more than once to an actor), therefore we preferred to use more advanced
% data-structures than simple lists for that, i.e. set_utils:set()

% We could also imagine recording, for each actor, each spontaneous tick
% planned, so that for example they can be removed when it is itself removed
% (instead of searching them) - but this would not scale well.



% Most if not all scheduling messages are timestamped (with the tick offset and,
% if appropriate, the current diasca), even though in some cases it is not
% strictly necessary. This is nevertheless a way of adding stronger runtime
% checkings, and the overhead is most probably tiny (as a message has to be sent
% anyway).

% For example there is a potential race condition, from the point of view of a
% non-root time manager, between the receiving of a 'beginTimeManagerDiasca'
% message (sent by its parent manager) and a 'scheduleTrigger' message (sent by
% a local actor having received a message, possibly from a non-local
% actor).
%
% Indeed, the time manager at diasca D may receive a scheduleTrigger message
% targeting the next diasca D+1 (as usual when sent from an actor at diasca D
% and when the manager already received its 'begin diasca' notification) or the
% scheduleTrigger might come from an early remote actor already at diasca D+1
% (thus targeting D+2), received before the 'beginTimeManagerDiasca' message.
%
% As a consequence two different sets of actors to be triggered must be
% maintained by a time manager: the one for next diasca, and the one for the
% diasca after.



% When running in simulation interactive mode, the root time manager is the only
% manager that makes use of a timer, all other managers are paced according to
% this same (possibly scaled) time base. Ticks are then scheduled on par with
% the real time, regardless of their need of actually being scheduled (no jump
% allowed over known idle ticks).


% A terminating actor must not terminate while being still to be scheduled later
% by the time manager (either due to an already planned spontaneous behaviour or
% because another actor is trying to interact with it): it will be scheduled
% (with a beginTermination call), directly at the next diasca after having
% declared its termination, and then only be safely removed from the simulation.



% Regarding stochastic support.

% Time managers used to assign random seeds to actors. This had for notable
% drawback that reproducibility was guaranteed only if using a constant number
% of computing hosts (as otherwise the random series driven by the corresponding
% time managers would differ); instead, now the (centralised) load-balancer
% takes care of actor seeding, with no real overhead as it has to send other
% information (ex: AAI) to them anyway.



% Resilience section.
%
% When a resilience rollback is performed, Sim-Diasca is redeployed from scratch
% on the surviving nodes. Blank time managers (root or not) are then created,
% and the agents of the other services link to them later during this rollback;
% this requires state merges to be operated (current newly deployed managers
% being already updated by other recreated agents - notably by the load
% balancer, which is an actor, which are merged with serialised managers). Hence
% in the general cases multiple merges per node are to be performed.

% This merging design is possible now that there is no more dependency of the
% random seeds onto the number of time managers: actor seeds will be the same,
% regardless of the number of computing nodes and time managers involved, as
% only actors request seeds (from the load balancer).


% How we measure concurrency:
%
% To compute the average concurrency, we simply sum all schedulings and divide
% the result by the number of diascas elapsed.
%
% More precisely, for each tick and scheduled diasca, we first add the number of
% local actors scheduled (respectively for spontaneous or scheduled actions) and
% then, when a subtree reports the end of its diasca, we add its corresponding
% count to the global one.



% TO-DO:
%
% - tune the beginTimeManagerTick method to further minimize its duration, as
% most of the useless simulation latency comes from that critical section
% (actors are all waiting in the meantime)
%
% - maintain, thanks to the (distributed) instance tracker, a list of {
% TickOffset, TerminatedActorList } pairs, so that wrong operations due to
% life-cycle (typically trying to interact with deleted actors) can be better
% diagnosed


% In terms of helper processes, we may have:
%
% - timer_main_loop/2, for the interactive timing internal process
% - watchdog_main_loop/4, for the watchdog internal process
% - wallclock_tracker_main_loop/3, to schedule wall-clock milestones
% - time_tracker_main_loop/5, for the console tick tracker internal process



-compile({ inline, [ get_trace_timestamp/3 ] } ).


% Returns a rather detailed (hence more expensive) timestamp to be included in
% traces.
%
% Meant to be inlined as much as possible to lessen the cost of such traces.
%
% Note: directly deriving from class_Actor counterpart system.
%
get_trace_timestamp( TickOffset, Diasca, State ) ->

	CurrentTick = ?getAttr(initial_tick) + TickOffset,

	% Only relies on the simulation_tick_duration attribute:
	CurrentSecond = convert_ticks_to_seconds( CurrentTick, State ),

	Timestamp = calendar:gregorian_seconds_to_datetime(
					round( CurrentSecond ) ),

	% Cannot include a newline as it would break the trace format:
	text_utils:format( "~ts {~B,~B}",
		[ time_utils:get_textual_timestamp( Timestamp ), TickOffset, Diasca ] ).




% Constructs a new time manager:
%
% - SimulationTickDuration designates the duration (as a strictly positive
% floating-point value), expressed in virtual seconds, of each simulation tick;
% for example, if the specified duration is 0.02s, then each simulation step
% will last for 20ms (in simulation time) and the simulation frequency will be
% 50Hz; models can be scheduled at a sub-multiple of this fundamental frequency
% if needed (ex: 16.7Hz, i.e. every 60ms, hence here every 3 ticks)
%
% - SimInteractivityMode, among:
%
%  - interactive: then the simulation will try to run on par with the user time
%
%  - batch: then the simulation will run as fast as possible, regardless of user
%  time
%
% - ParentManagerInformation, which can be either 'none' if this manager is the
% root one, otherwise the PID of the direct parent time manager of this one
%
% - RootInstanceTrackerPid is the PID of the root instance tracker, to help
% troubleshooting the actors
%
% - TroubleshootingMode tells whether the troubleshooting mode is enabled
%
% - Context tells here whether we are at simulation start-up or redeploying
% during a rollback
%
% Note: the initial simulation date is to be given only when this time manager
% is started.
%
-spec construct( wooper:state(), virtual_seconds(),
	simulation_interactivity_mode(),
	RootTimeManager :: 'none' | time_manager_pid(),
	RootInstanceTracker :: instance_tracker_pid(), boolean(),
	class_DeploymentManager:simulation_context() ) -> wooper:state().
construct( State, SimulationTickDuration, SimInteractivityMode,
		   ParentManagerInformation, RootInstanceTrackerPid,
		   TroubleshootingMode, _Context ) ->

	% This manager will terminate if ever an exit signal is received, instead of
	% transforming the signal into an 'EXIT' message:
	% (a monitor could be used instead)
	%
	%erlang:process_flag( trap_exit, true ),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_EngineBaseObject:construct( State,
										?trace_categorize("Time Manager") ),

	% We raise here the priority of all time managers, not specifically for
	% their processings, but because they are to send a large number of messages
	% (to scheduled actors), and, as a consequence, the Erlang scheduler might
	% want to retaliate:
	%
	erlang:process_flag( priority, _Level=high ),

	% Instance trackers are created before time managers:
	LocalInstanceTrackerPid =
		class_InstanceTracker:register_agent( ?time_manager_name ),

	% The child_managers are declared here, as the declareChildManager call will
	% need them to be already set:
	%
	CategorizedState =
		setAttribute( TraceState, child_managers, set_utils:new() ),

	ActualSimulationTickDuration =
		check_tick_duration( SimulationTickDuration ),

	case SimInteractivityMode of

		interactive ->
			ok;

		batch ->
			ok;

		_ ->
			throw( { invalid_simulation_interactivity_mode,
					 SimInteractivityMode } )

	end,

	RegistrationName = get_registration_name(),

	{ ParentManager, Description, InitialWaitedCount } =
		case ParentManagerInformation of

		none ->

			% Only one to register globally as well (even if re-deploying after
			% a recovered crash):
			%
			naming_utils:register_as( RegistrationName, local_and_global ),

			?send_debug_fmt( CategorizedState, "Root time manager "
				"registered as '~ts', locally and globally.",
				[ RegistrationName ] ),

			% The root manager is directly spawned as a process linked to the
			% deployment manager.

			% We wait until the actual start to set the initial waited count (as
			% the root time manager may be restarted, so each start should be
			% done with a right initialisation):
			%
			{ undefined, "root time manager", _InitialWaitedCount=undefined };


		ParentPid when is_pid( ParentPid ) ->

			% We are here a local (non-root) time manager, the children have to
			% declare themselves to their direct parent:
			%
			% (request for synchronisation purposes)
			%
			ParentPid ! { declareChildManager, [], self() },

			% A bit of interleaving:

			% Linking to the parent time manager so that transitively the root
			% one is linked to all actors:
			%
			erlang:link( ParentPid ),

			?send_debug_fmt( CategorizedState, "Child time manager "
				"registered, locally, as '~ts'.", [ RegistrationName ] ),

			naming_utils:register_as( RegistrationName, local_only ),

			receive

				{ wooper_result, child_manager_registered } ->

					Desc = text_utils:format(
						"child time manager, whose parent manager is ~w,",
						[ ParentPid ] ),

					% A child manager is not specifically started, hence it will
					% not go through init/0, so we initialise its waited count
					% here:
					%
					{ ParentPid, Desc, _InitialWaitedCount=0 }

			end

	 end,

	SimulationFrequency = 1 / ActualSimulationTickDuration,

	TickDurationString = time_utils:duration_to_string(
					   round( 1000 * ActualSimulationTickDuration ) ),

	?send_info_fmt( CategorizedState,
		"Creating a " ++ Description ++ " in ~w mode with an actual "
		"fundamental simulation frequency of approximately ~.2fHz; "
		"in virtual time, each simulation tick will last exactly for ~fs "
		"(i.e. ~ts).",
		[ SimInteractivityMode, SimulationFrequency,
		  ActualSimulationTickDuration, TickDurationString ] ),


	TickWaitingDuration = case SimInteractivityMode of

		interactive ->

			% In milliseconds:
			case round( ?scale_factor_for_interactive_time * 1000
						* ActualSimulationTickDuration ) of

				0 ->
					?send_warning_fmt( CategorizedState,
						"Running in simulation interactive mode, "
						"with a inter-tick duration that has been forced "
						"to exactly 1 ms, "
						"knowing that applying the interactive scale factor "
						"of x~f to the simulation tick duration "
						"(of ~fs, i.e. ~ts) would have led to a null "
						"inter-tick duration.",
						[ ?scale_factor_for_interactive_time,
						  ActualSimulationTickDuration, TickDurationString ] ),
					1;

				RoundDuration ->
					?send_info_fmt( CategorizedState,
						"Running in simulation interactive mode, "
						"with an actual inter-tick duration of ~B ms "
						"(i.e. ~ts), resulting from a simulation tick of ~f ms "
						"(i.e. ~ts) to which the interactive scale factor of "
						"x~p applies.",
						[ RoundDuration, time_utils:duration_to_string(
								round( RoundDuration ) ),
						  ActualSimulationTickDuration, TickDurationString,
						  ?scale_factor_for_interactive_time ] ),
					RoundDuration

			end;

		_ ->
			undefined

	end,

	% Useful to inspect the code path of each computing node:
	%send_debug_fmt( CategorizedState, "Code path for node '~ts' is: ~ts",
	%				 [ node(), text_utils:strings_to_string(
	%							 code_utils:get_code_path() ) ] ),

	EmptySet = set_utils:new(),

	StartState = setAttributes( CategorizedState, [

		{ parent_manager_pid, ParentManager },

		% child_managers already defined.

		% As local actors may register before the simulation starts, their list
		% must be created here:
		%
		{ known_local_actors, EmptySet },

		{ load_balancer_pid, undefined },
		{ started, false },

		% initial_tick set later in this method.
		{ initial_timestamp, time_utils:get_precise_timestamp() },

		% Will be set at simulation start:
		{ current_tick_offset, undefined },
		{ current_diasca, undefined },

		% As local actors may register and be scheduled before the simulation
		% starts, must be created here:
		%
		{ spontaneous_agenda, [] },

		{ actors_to_trigger_in_one_diasca, EmptySet },
		{ actors_to_trigger_in_two_diascas, EmptySet },

		{ next_action, undefined },
		{ previous_timestamp, undefined },
		{ next_timestamp, undefined },
		{ watchdog_pid, undefined },
		{ stop_tick_offset, undefined },
		{ simulation_listeners, [] },
		{ time_listeners, [] },

		% By default once every 4 real minutes:
		{ wallclock_milestone_period, 4*60*1000 },
		{ wallclock_milestone_listeners, [] },

		% By default every 1000 ticks:
		{ tick_milestone_period, 1000 },
		{ tick_milestone_listeners, [] },

		{ suspended, false },
		{ simulation_tick_duration, ActualSimulationTickDuration },
		{ simulation_tick_waiting, TickWaitingDuration },
		{ simulation_interactivity_mode, SimInteractivityMode },

		{ diasca_count, undefined },
		{ schedule_count, undefined },

		{ terminated_actors, undefined },
		{ terminating_actors, undefined },
		{ actors_to_delete_at_next_tick, [] },
		{ waited_child_managers, undefined },
		{ waited_spontaneous_actors, undefined },
		{ waited_triggered_actors, undefined },
		{ watchdog_waited, false },
		{ waited_count, InitialWaitedCount },
		{ interactive_tick_triggered, undefined },
		{ timer_pid, undefined },
		{ wallclock_tracker_pid, undefined },
		{ time_tracker_pid, undefined },
		{ overall_actor_count, 0 },
		{ scheduled_tracking, 0 },
		{ process_tracking, system_utils:get_process_count() },
		{ troubleshooting_mode, TroubleshootingMode },
		{ root_data_exchanger_pid, undefined },
		{ root_instance_tracker_pid, RootInstanceTrackerPid },
		{ local_instance_tracker_pid, LocalInstanceTrackerPid },
		{ interdiasca_listeners, [] },
		{ resilience_manager_pid, undefined },
		{ serialisation_requested, false } ] ),

	% Not starting at user time anymore, as it would break reproducibility:
	%{Date, Time} = {date(), time()},

	% Starting by default at a base (common) date instead:
	%
	% (StartState needed, as depends on simulation_tick_duration)
	%
	DefaultInitialTick =
		timestamp_to_ticks( ?initial_simulation_date, StartState ),

	?send_debug_fmt( StartState,
					 "Time manager created, default initial tick is ~B.",
					 [ DefaultInitialTick ] ),

	setAttribute( StartState, initial_tick, DefaultInitialTick ).




% Overridden destructor.
%
% All still-subscribed listeners will be warned of the manager deletion by a
% timeManagerShutdown message.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?info( "Deleting the time manager." ),

	StoppedState = case ?getAttr(started) of

		true ->
			{ NewState, stopped } = executeRequest( State, stop ),
			NewState;

		false ->
			State

	end,

	% Recurses down the scheduling tree:
	wooper:delete_synchronously_instances(
			  set_utils:to_list( ?getAttr(child_managers) ) ),

	basic_utils:send_to_pid_set( timeManagerShutdown,
								 ?getAttr(known_local_actors) ),

	naming_utils:unregister( get_registration_name(), ?registration_scope ),

	class_InstanceTracker:unregister_agent(),

	?info( "Time manager deleted." ),

	% Then allows chaining:
	StoppedState.






% Methods section.



% Called by all local managers that are direct children of this time manager
% (which thus is their parent), when they are created.
%
% To be called before the simulation is started.
%
% (request, for synchronisation purposes)
%
-spec declareChildManager( wooper:state() ) ->
								request_return( 'child_manager_registered' ).
declareChildManager( State ) ->

	NewManagers = set_utils:add( ?getSender(), ?getAttr(child_managers) ),

	NewState = setAttribute( State, child_managers, NewManagers ),

	% The simulation interactivity mode has already been specified at
	% construction:
	%
	wooper:return_state_result( NewState, child_manager_registered ).




% Called to notify the (root) time manager of the PID of the load balancer.
%
% (request, for synchronisation reasons)
%
-spec setLoadBalancerPid( wooper:state(), load_balancer_pid() ) ->
								request_return( 'load_balancer_set' ).
setLoadBalancerPid( State, LoadBalancerPid ) ->

	wooper:check_undefined( load_balancer_pid, State ),

	SetState = setAttribute ( State, load_balancer_pid, LoadBalancerPid  ),

	wooper:return_state_result( SetState, load_balancer_set ).




% Registers the fact that the specified load balancer is to be managed by this
% time manager, hence its bootstrap scheduling must be accounted for.
%
-spec registerBootstrapScheduling( wooper:state(), load_balancer_pid() ) ->
										request_return( 'load_balancer_set' ).
registerBootstrapScheduling( State, LoadBalancerPid ) ->

	% load_balancer_pid may (if this manager is the root one) or may not be
	% already set.

	[] = ?getAttr(spontaneous_agenda),

	% We want to schedule the load balancer for a spontaneous behaviour at the
	% first diasca of the first scheduled tick, so that it can itself trigger
	% the 'onFirstDiasca/2' actor oneway on all initial actors:
	%
	BalancerEntry = { _TickOffset=0, set_utils:singleton( LoadBalancerPid ) },

	InitialAgenda = [ BalancerEntry ],

	SetState = setAttributes( State, [
			{ load_balancer_pid, LoadBalancerPid },
			{ spontaneous_agenda, InitialAgenda } ] ),

	wooper:return_state_result( SetState, load_balancer_set ).



% Returns the initial tick for that simulation, if it has been already set,
% otherwise the atom 'undefined'.
%
-spec getInitialTick( wooper:state() ) ->
							const_request_return( maybe( tick() ) ).
getInitialTick( State ) ->
	wooper:const_return_result( ?getAttr(initial_tick) ).



% Sets the initial tick to be used by the simulation.
%
% Must not be called while the simulation is running.
%
% See also: setFinalTick/2.
%
-spec setInitialTick( wooper:state(), tick() ) -> oneway_return().
setInitialTick( State, InitialTick ) ->

	false = ?getAttr(started),

	wooper:return_state(
	   setAttribute( State, initial_tick, InitialTick ) ).



% Sets the initial tick to be used by the simulation, based on the specified
% timestamp, expressed in virtual time.
%
% Must not be called while the simulation is running.
%
% See also: setFinalSimulationTimestamp/2.
%
-spec setInitialSimulationTimestamp( wooper:state(), timestamp() ) ->
											oneway_return().
setInitialSimulationTimestamp( State, InitialTimestamp ) ->

	SetState = set_initial_simulation_timestamp( InitialTimestamp, State ),

	wooper:return_state( SetState ).




% Sets the initial tick to be used by the simulation, based on the specified
% time and date, expressed in virtual time.
%
% Must not be called while the simulation is running.
%
% See also: setFinalSimulationTimestamp/3.
%
-spec setInitialSimulationTimestamp( wooper:state(), time_utils:date(),
									 time_utils:time() ) -> oneway_return().
setInitialSimulationTimestamp( State, Date, Time ) ->

	SetState =
		set_initial_simulation_timestamp( _Timestamp={ Date, Time }, State ),

	wooper:return_state( SetState ).



% (helper)
-spec set_initial_simulation_timestamp( timestamp(), wooper:state() ) ->
												wooper:state().
set_initial_simulation_timestamp( InitialTimestamp, State ) ->

	false = ?getAttr(started),

	NewInitialTick = timestamp_to_ticks( InitialTimestamp, State ),

	?notice_fmt( "Initial timestamp set to ~ts (corresponding to "
		"absolute tick ~B, i.e. by design tick offset #0).",
		[ time_utils:get_textual_timestamp( InitialTimestamp ),
		  NewInitialTick ] ),

	setAttribute( State, initial_tick, NewInitialTick ).




% Returns the final (absolute) tick for that simulation, or the 'undefined' atom
% if no final tick was defined.
%
-spec getFinalTick( wooper:state() ) ->
						const_request_return( maybe( tick() ) ).
getFinalTick( State ) ->

	Res = case ?getAttr(stop_tick_offset) of

		undefined ->
			undefined;

		StopOffset ->
			?getAttr(initial_tick) + StopOffset

	end,

	wooper:const_return_result( Res ).



% Sets the final (absolute) tick to be used by the simulation (the first one not
% to be scheduled).
%
% Must not be called while the simulation is running.
%
% Note: depends on the current initial tick, as is stored as a tick offset.
%
% See also: setInitialTick/2.
%
-spec setFinalTick( wooper:state(), tick() ) -> oneway_return().
setFinalTick( State, FinalTick ) ->

	false = ?getAttr(started),

	InitialTick = ?getAttr(initial_tick),

	DurationInTicks = FinalTick - InitialTick,

	case DurationInTicks > 0 of

		true ->
		   ?notice_fmt( "Final (absolute) tick set to ~B (corresponding to "
				"tick offset #~B, i.e ~ts).",
				[ FinalTick, DurationInTicks, time_utils:get_textual_timestamp(
					  ticks_to_timestamp( FinalTick, State ) ) ] ),

			wooper:return_state(
				   setAttribute( State, stop_tick_offset, DurationInTicks ) );

		false ->
			throw( { final_tick_on_the_past, FinalTick, InitialTick } )

	end.



% Sets the final tick (the first one not to be scheduled) to be used by the
% simulation, based on the specified time and date, expressed in virtual time.
%
% Must not be called while the simulation is running.
%
% Note: depends on the current initial tick, as is stored as a tick offset.
%
% See also: setInitialSimulationTimestamp/2.
%
-spec setFinalSimulationTimestamp( wooper:state(), timestamp() ) ->
										oneway_return().
setFinalSimulationTimestamp( State, FinalTimestamp ) ->

	SetState = set_final_simulation_timestamp( FinalTimestamp, State ),

	wooper:return_state( SetState ).




% Sets the final tick (the first one not to be scheduled) to be used by the
% simulation, based on the specified time and date, expressed in virtual time.
%
% Must not be called while the simulation is running.
%
% Note: depends on the current initial tick, as is stored as a tick offset.
%
% See also: setInitialSimulationTimestamp/3.
%
-spec setFinalSimulationTimestamp( wooper:state(), time_utils:date(),
								   time_utils:time() ) -> oneway_return().
setFinalSimulationTimestamp( State, FinalDate, FinalTime ) ->

	FinalTimestamp = { FinalDate, FinalTime },

	SetState = set_final_simulation_timestamp( FinalTimestamp, State ),

	wooper:return_state( SetState ).



% (helper)
-spec set_final_simulation_timestamp( timestamp(), wooper:state() ) ->
											wooper:state().
set_final_simulation_timestamp( FinalTimestamp, State ) ->

	false = ?getAttr(started),

	FinalTick = timestamp_to_ticks( FinalTimestamp, State ),

	InitialTick = ?getAttr(initial_tick),

	DurationInTicks = FinalTick - InitialTick,

	case DurationInTicks > 0 of

		true ->
		   ?notice_fmt( "Final timestamp set to ~ts (corresponding "
				"to absolute tick ~B, i.e. tick offset #~B).",
				[ time_utils:get_textual_timestamp( FinalTimestamp ),
				  FinalTick, DurationInTicks ] ),

			setAttribute( State, stop_tick_offset, DurationInTicks );

		false ->
			throw( { final_timestamp_in_the_past, FinalTimestamp,
					 { FinalTick, InitialTick }, DurationInTicks } )

	end.



% Sets the bounds in virtual time of the simulation, i.e. the initial timestamp
% at which it shall start and the final, latest timestamp it should not go
% beyond, knowing that other termination causes may trigger before it.
%
% Must not be called while the simulation is running.
%
% See also: setInitialSimulationTimestamp/2, setFinalSimulationTimestamp/2.
%
-spec setSimulationTimeframe( wooper:state(), timestamp(), timestamp() ) ->
									oneway_return().
setSimulationTimeframe( State, InitialTimestamp, FinalTimestamp ) ->

	% Order matters:

	InitialState = set_initial_simulation_timestamp( InitialTimestamp, State ),
	FinalState = set_final_simulation_timestamp( FinalTimestamp, InitialState ),

	wooper:return_state( FinalState ).




% Management section of the time manager.



% Starts this (root) time manager with no specific termination tick defined.
-spec start( wooper:state() ) -> oneway_return().
start( State ) ->
	% Here no termination tick is set:
	wooper:return_state( init( State ) ).



% Starts this (root) time manager with a specific termination tick defined,
% expressed as an offset to the initial tick.
%
-spec start( wooper:state(), tick_offset() | simulation_listener_pid() ) ->
					oneway_return().
start( State, TerminationOffset ) when is_integer( TerminationOffset ) ->

	% This is an offset, no checking needed here:
	InitState =
		init( setAttribute( State, stop_tick_offset, TerminationOffset ) ),

	StopTick = getAttribute( InitState, initial_tick ) + TerminationOffset,

	?notice_fmt( "The simulation will stop no later than tick ~B "
		"(termination tick offset is #~B).", [ StopTick, TerminationOffset ] ),

	wooper:return_state( InitState );


% Starts this (root) time manager with a specific termination tick defined,
% expressed as an offset to the initial tick.
%
% (second clause of the oneway)
%
start( State, SimulationListenerPID ) when is_pid( SimulationListenerPID ) ->

	StartState = start(
	  appendToAttribute( State, simulation_listeners, SimulationListenerPID ) ),

	wooper:return_state( StartState ).



% Starts this (root) time manager with a specific termination tick and specified
% registered simulation listener.
%
-spec start( wooper:state(), tick_offset(), simulation_listener_pid() ) ->
				oneway_return().
start( State, TerminationOffset, SimulationListenerPID )
  when is_integer( TerminationOffset )
	   andalso is_pid( SimulationListenerPID ) ->

	StartState = start( appendToAttribute( State, simulation_listeners,
										  SimulationListenerPID ),
					   TerminationOffset ),

	wooper:return_state( StartState ).



% Starts this (root) time manager for a specified duration, expressed in
% simulation time (as a floating-point number of virtual seconds).
%
-spec startFor( wooper:state(), any_seconds() ) -> oneway_return().
startFor( State, Duration ) when is_integer( Duration ) ->
	StartState = startFor( State, erlang:float( Duration ) ),
	wooper:return_state( StartState );


startFor( State, Duration ) when is_float( Duration ) andalso Duration > 0.0 ->

	TerminationOffset = convert_seconds_to_ticks( Duration, State ),

	StartState = init(
				  setAttribute( State, stop_tick_offset, TerminationOffset ) ),

	StopTick = getAttribute( StartState, initial_tick ) + TerminationOffset,

	?notice_fmt( "The simulation will stop no later than tick ~B "
		"(termination tick offset is #~B), corresponding to "
		"a user-specified duration, in simulation time, of ~ts.",
		[ StopTick, TerminationOffset,
		  time_utils:duration_to_string( round( 1000 * Duration ) ) ] ),

	wooper:return_state( StartState );


startFor( _State, ErrorDuration ) ->
	throw( { invalid_simulation_duration_specified, ErrorDuration } ).



% Starts this (root) time manager for a specified duration, expressed in
% simulation time (as a number of virtual seconds), with a registered stop
% listener.
%
-spec startFor( wooper:state(), any_seconds(), simulation_listener_pid() ) ->
					oneway_return().
startFor( State, Duration, SimulationListenerPID )
  when is_pid( SimulationListenerPID ) ->

	StartState = startFor( appendToAttribute( State, simulation_listeners,
											  SimulationListenerPID ),
						   Duration ),

	wooper:return_state( StartState ).



% Stops this time manager.
-spec stop( wooper:state() ) ->
				request_return( { 'stopped', time_manager_pid() } ).
stop( State ) ->

	StoppedState = case ?getAttr(started) of

		false ->
			?error( "Stop request ignored: simulation clock not running." ),
			State;


		true ->

			Timings = get_textual_timings( State ),

			?notice_fmt( "Stopping simulation clock at ~ts.", [ Timings ] ),

			% The engine may be stopped at any time.


			?debug( "Taking care of pending already terminated actors." ),

			ActorToDeleteAtNextTick = ?getAttr(actors_to_delete_at_next_tick),

			?display_console( "Stopping at #~p, hence deleting actors ~p.",
				[ ?getAttr(current_tick_offset), ActorToDeleteAtNextTick ] ),

			wooper:delete_synchronously_instances( ActorToDeleteAtNextTick ),


			?debug( "Taking care of already terminating actors." ),

			% Having no more active actors is only one of the causes for the
			% time manager to stop, therefore there could be
			% already-terminating, or still running actors, which must be
			% removed:

			wooper:delete_synchronously_instances(
			  ?getAttr(terminated_actors) ),

			?debug( "Taking care now of still running actors." ),

			TerminatedState = terminate_running_actors(
			   _ActorsToSkip=ActorToDeleteAtNextTick, State ),

			?debug( "Stopping watchdog, trackers and child managers." ),

			TimeTrackerState = stop_time_tracker( TerminatedState ),

			WallclockTrackerState = stop_wallclock_tracker( TimeTrackerState ),

			WatchdogState = stop_watchdog( WallclockTrackerState ),

			TimerState = stop_timer( WatchdogState ),

			stop_child_managers( TimerState ),

			[ L ! simulation_stopped || L <- ?getAttr(simulation_listeners) ],

			flush_scheduling_messages(),


			?debug( "Stop successful." ),

			% Only one display wanted, the one of the root time manager:
			case is_root_manager( State ) of

				true ->
					class_PluginManager:notify( on_simulation_stop ),
					display_timing_information( Timings, TimerState ),
					display_concurrency_information( TimerState );

				false ->
					ok

			end,

			% Prepares back a blank state, should a new simulation be started:
			setAttributes( TimerState, [
				{ initial_timestamp, undefined },
				{ started, false },
				{ actors_to_delete_at_next_tick, [] },
				{ known_local_actors, set_utils:new() },
				{ spontaneous_agenda, [] },
				{ overall_actor_count, 0 } ] )

	end,

	% Returning the time manager PID allows to wait in parallel for multiple
	% stopping child managers:
	%
	wooper:return_state_result( StoppedState, { stopped, self() } ).



% Defined so that a time manager can stop itself asynchronously.
-spec selfStop( wooper:state() ) -> oneway_return().
selfStop( State ) ->

	{ StoppedState, _Result } = executeRequest( State, stop, [] ),

	wooper:return_state( StoppedState ).



% Suspends the simulation until a resume request is received.
-spec suspend( wooper:state() ) -> oneway_return().
suspend( State ) ->

	?notice( "Simulation requested to suspend." ),

	[ L ! simulation_suspended || L <- ?getAttr(simulation_listeners) ],

	wooper:return_state( setAttribute( State, suspended, true ) ).



% Resumes the simulation once it has been suspended.
-spec resume( wooper:state() ) -> const_oneway_return().
resume( State ) ->

	?error( "Resume request received, whereas should have been intercepted "
			"by suspend code. Ignored." ),

	wooper:const_return().



% Listener section.


% Registers a new simulation listener, which will be notified of all main
% simulation events (like start, stop, resume, etc.).
%
-spec addSimulationListener( wooper:state(), simulation_listener_pid() ) ->
								oneway_return().
addSimulationListener( State, ListenerPid ) ->

	%?debug_fmt( "Simulation listener ~w added.", [ ListenerPid ] ),

	wooper:return_state(
		appendToAttribute( State, simulation_listeners, ListenerPid ) ).



% Unregisters a simulation listener, expected to be already registered.
-spec removeSimulationListener( wooper:state(), simulation_listener_pid() ) ->
									oneway_return().
removeSimulationListener( State, ListenerPid ) ->

	%?debug_fmt( "Simulation listener ~w removed.", [ ListenerPid ] ),

	wooper:return_state(
		deleteFromAttribute( State, simulation_listeners, ListenerPid ) ).



% Registers a new time listener, which will be notified of all scheduled ticks
% and diascas.
%
% Note: contrary to an actor, no synchronisation will be performed, and the
% listener will not be able to trigger the scheduling of any tick.
%
-spec addTimeListener( wooper:state(), logical_time_listener_pid() ) ->
							oneway_return().
addTimeListener( State, ListenerPid ) ->

	%?debug_fmt( "Time listener ~w added.", [ ListenerPid ] ),

	wooper:return_state(
		appendToAttribute( State, time_listeners, ListenerPid ) ).



% Unregisters a time listener, expected to be already registered.
-spec removeTimeListener( wooper:state(), logical_time_listener_pid() ) ->
								oneway_return().
removeTimeListener( State, ListenerPid ) ->

	%?debug_fmt( "Time listener ~w removed.", [ ListenerPid ] ),

	wooper:return_state(
		deleteFromAttribute( State, time_listeners, ListenerPid ) ).



% Registers the resilience manager, so that it can be notified when a requested
% serialisation can be performed (and be also notified of the main simulation
% events, like all other simulation listeners).
%
% (request, for synchronicity)
%
-spec registerResilienceManager( wooper:state() ) ->
									request_return( 'registered' ).
registerResilienceManager( State ) ->

	ResilienceManagerPid = ?getSender(),

	Listeners = ?getAttr(simulation_listeners),

	wooper:return_state_result( setAttributes( State, [
			{ simulation_listeners, [ ResilienceManagerPid | Listeners ] },
			{ resilience_manager_pid, ResilienceManagerPid } ] ),
								registered ).



% Unregisters the resilience manager.
-spec unregisterResilienceManager( wooper:state() ) -> oneway_return().
unregisterResilienceManager( State ) ->
	wooper:return_state(
	  setAttribute( State, resilience_manager_pid, undefined ) ).



% Tells this (supposedly root) time manager that a serialisation is to happen
% once the current diasca is over (typically called by the resilience manager).
%
-spec serialisationRequested( wooper:state() ) -> oneway_return().
serialisationRequested( State ) ->
	wooper:return_state(
	   setAttribute( State, serialisation_requested, true ) ).



% Returns a list of all the "real" actors (the ones that are not simulation
% agents) and that are directly managed by this time manager.
%
-spec getAllLocalActors( wooper:state() ) ->
								const_request_return( [ actor_pid() ] ).
getAllLocalActors( State ) ->

	% We filter out the load balancer (if ever it was managed by this time
	% manager), as we want only actual, "real", actors:

	% Efficient search, no error if not found:
	NonAgentActors = set_utils:delete( ?getAttr(load_balancer_pid),
									   ?getAttr(known_local_actors) ),

	% More compact sending:
	Actors = set_utils:to_list( NonAgentActors ),

	wooper:const_return_result( Actors ).



% Merges the specified serialisation entries into the state of this time
% manager.
%
-spec mergeWith( wooper:state(), wooper_serialisation:term_serialisation() ) ->
					request_return( 'merged' ).
mergeWith( State, Entries ) ->

	ToMergeAgenda = option_list:get( spontaneous_agenda, Entries ),

	% Supposedly the current is the smallest:
	MergedAgenda = merge_agendas( ?getAttr(spontaneous_agenda),
								  ToMergeAgenda ),

	% Default pair order is *not* fine:
	MergedPreviousTimestamp = max_timestamp( ?getAttr(previous_timestamp),
			option_list:get( previous_timestamp, Entries ) ),

	MergedNextTimestamp = min_timestamp( ?getAttr(next_timestamp),
			option_list:get( next_timestamp, Entries ) ),

	MergedNextAction = merge_next_action( ?getAttr(next_action),
			option_list:get( next_action, Entries ), MergedAgenda ),

	MergedKnownActors = set_utils:union( ?getAttr(known_local_actors),
			option_list:get( known_local_actors, Entries ) ),

	MergedToDelete = ?getAttr(actors_to_delete_at_next_tick)
		++ option_list:get( actors_to_delete_at_next_tick, Entries ),

	MergedState = setAttributes( State, [
		{ spontaneous_agenda, MergedAgenda },
		{ previous_timestamp, MergedPreviousTimestamp },
		{ next_timestamp, MergedNextTimestamp },
		{ next_action, MergedNextAction },
		{ known_local_actors, MergedKnownActors },
		{ initial_timestamp, option_list:get( initial_timestamp, Entries ) },
		{ initial_tick, option_list:get( initial_tick, Entries ) },
		{ current_tick_offset,
			option_list:get( current_tick_offset, Entries ) },
		{ current_diasca, option_list:get( current_diasca, Entries ) },
		{ actors_to_delete_at_next_tick, MergedToDelete } ] ),

	?debug_fmt( "The merge of current state: ~ts~nwith specified entries: ~ts~n"
				"resulted in: ~ts", [ wooper:state_to_string( State ),
									 option_list:to_string( Entries ),
									 wooper:state_to_string( MergedState ) ] ),

	wooper:return_state_result( MergedState, merged ).



% Determines the next action after the merge.
merge_next_action( _FirstAction=no_planned_action,
				   _SecondAction=no_planned_action,
				   _MergedAgenda=[] ) ->
	no_planned_action;

merge_next_action( _FirstAction=no_planned_action,
				   _SecondAction=no_planned_action,
				   _MergedAgenda=[ { Offset, _ActorSet } | _T ] ) ->
	Offset;

merge_next_action( FirstAction, _SecondAction=no_planned_action,
				   _MergedAgenda ) ->
	FirstAction;

merge_next_action( _FirstAction=no_planned_action, SecondAction,
				   _MergedAgenda ) ->
	SecondAction;

merge_next_action( FirstAction, SecondAction, _MergedAgenda )
  when FirstAction < SecondAction ->
	FirstAction;

merge_next_action( _FirstAction, SecondAction, _MergedAgenda ) ->
	SecondAction.



% Relinks this time manager, supposing it was just deserialised.
%
% (request, for synchronicity)
%
-spec relink( wooper:state() ) ->
					request_return( { 'relinked', time_manager_pid() } ).
relink( State ) ->

	% Let's ensure we have all proper PIDs, starting from class-specific
	% updates:

	% Otherwise the next functions could not send traces:
	InitState = class_EngineBaseObject:init( State ),

	UpdatedState = InitState,

	% We have to recreate all private helper processes that were running, see
	% get_private_processes_attribute_names/0:
	%
	WatchdogState = case ?getAttr(watchdog_pid) of

		undefined ->
			UpdatedState;

		?term_restoration_marker ->
			% We restore the watchdog then (reset):
			launch_watchdog( UpdatedState )

	end,

	TimerState = case ?getAttr(timer_pid) of

		undefined ->
			WatchdogState;

		?term_restoration_marker ->
			% We restore the timer then (reset):
			launch_timer( WatchdogState )

	end,

	WallclockState = case ?getAttr(wallclock_tracker_pid) of

		undefined ->
			TimerState;

		?term_restoration_marker ->
			% We restore the wallclcok time tracker then (reset):
			launch_wallclock_tracker( TimerState )

	end,

	TimeState = case ?getAttr(time_tracker_pid) of

		undefined ->
			WallclockState;

		?term_restoration_marker ->
			% We restore the time tracker then (reset):
			launch_time_tracker( WallclockState )

	end,

	% Then performing generic relinking, based on an entry transformer
	% requesting the instance tracking service to convert back PIDs:

	wooper:return_state_result( TimeState, { relinked, self() } ).



% Requests this (supposedly root) time manager to restart, after a rollback
% (hence based on a just deserialised state).
%
-spec restartAfterRollback( wooper:state() ) -> oneway_return().
restartAfterRollback( State ) ->

	% Of course we do not want an infinite rollback loop:
	RestartedState = manage_end_of_diasca_as_root_manager(
				setAttribute( State, serialisation_requested, false ) ),

	wooper:return_state( RestartedState ).




% Scheduling section.


% Subsection for the scheduling of spontaneous actions (each first diasca of a
% tick).


% Notifies this time manager that a new tick must begin (thus at diasca 0); to
% be sent by the direct parent time manager (if any).
%
% Takes care of the beginning of the specified new overall simulation tick
% (starting at diasca 0), which implies sending this message recursively through
% the whole scheduling tree, so that all relevant actors can be reached.
%
-spec beginTimeManagerTick( wooper:state(), tick_offset() ) -> oneway_return().
beginTimeManagerTick( State, NewTickOffset ) ->

	?display_console( "beginTimeManagerTick: new tick offset is #~B, "
		"next action ~p.", [ NewTickOffset, ?getAttr(next_action) ] ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_tick_consistency( NewTickOffset, State ),
			check_waited_count_consistency( State ),

			ToTriggerNextDiasca = ?getAttr(actors_to_trigger_in_one_diasca),

			% We cannot have messages waiting for diasca 0, by design:
			case set_utils:is_empty( ToTriggerNextDiasca ) of

				true ->
					ok;

				false ->
					throw( { new_tick_whereas_actors_still_to_trigger,
							 NewTickOffset,
							 set_utils:to_list( ToTriggerNextDiasca ) } )

			end

		end ),

	% There might be early actors which already targeted diasca 1 of this new
	% tick:
	% true = set_utils:is_empty( ?getAttr(actors_to_trigger_in_two_diascas) ),


	% In the rest of that method, we will be using for example:
	% 'class_TraceEmitter:send( info, [ NewState | Args ] )' instead of
	% '?notice(..)' so that the traces are output with a timestamp set to the
	% new current tick rather than the previous one still in
	% ?getAttr(current_tick_offset): we want the next traces to be the first of
	% this new tick, not the last of the previous one, thus the use of
	% 'NewState' instead of the usual (implicit) 'State'.

	% Here we go for this next tick:

	StopTickOffset = ?getAttr(stop_tick_offset),

	FinalState = case NewTickOffset of

		% The stop tick (if defined) may happen to be a skipped tick:
		%
		% (relies also on the fact that any integer is considered as smaller
		% than the 'undefined' atom, according to Erlang term order: number <
		% atom)
		%
		LateOffset when LateOffset >= StopTickOffset ->

			% We never enter this clause if StopTickOffset == 'undefined'.

			?display_console( "beginTimeManagerTick: stopping at tick #~B, "
				"as stop tick was #~B.", [ NewTickOffset, StopTickOffset ] ),

			% Can only happen if being the root time manager.

			% Not using this new tick (which is past the requested end),
			% specifying the user-defined stop tick instead:
			%
			NewState = setAttributes( State, [
						{ current_tick_offset, StopTickOffset },
						{ current_diasca, 0 } ,
						{ next_action, no_planned_action } ] ),

			ActualCurrentTick = ?getAttr(initial_tick) + StopTickOffset,

			?send_notice_fmt( NewState, "Reached termination tick ~B: "
				"~B ticks, corresponding to ~ts, have elapsed since the "
				"simulation started, stopping now.",
				[ ActualCurrentTick, StopTickOffset,
				  time_utils:duration_to_string( 1000 *
					convert_ticks_to_seconds( StopTickOffset, State ) ) ] ),

			case ?getAttr(spontaneous_agenda) of

				[] ->
					ok;

				Agenda ->
					?send_warning_fmt( NewState, "At the termination tick "
						"offset (#~B), some actors had still to be "
						"scheduled; the simulation had a spontaneous ~ts",
						[ StopTickOffset, agenda_to_string( Agenda ) ] )

			end,

			on_simulation_success( NewState ),

			{ StoppedState, { stopped, _SelfPid } } =
							 executeRequest( NewState, stop ),
			StoppedState;


		% Most common case, business as usual:
		_NonTerminalOrUndefinedStopTickOffset ->

			?display_console( "beginTimeManagerTick: continuing at tick #~B.",
							  [ NewTickOffset ] ),

			NewTimestamp = { NewTickOffset, 0 },

			NewState = setAttributes( State, [
				{ current_tick_offset, NewTickOffset },
				{ current_diasca, 0 },
				{ trace_timestamp, NewTimestamp },

				cond_utils:if_defined( exec_target_is_production,
					{ trace_timestamp, { NewTickOffset, 0 } },
					{ trace_timestamp, get_trace_timestamp( NewTickOffset,
													 _Diasca=0, State ) } ),

				{ next_action, no_planned_action } ] ),

			% Let's run this new tick, and possibly wait for answers:
			ManagedState = manage_new_tick( NewTickOffset, NewState ),

			?display_console( "beginTimeManagerTick: new tick managed.", [] ),

			% Deferred deletion of terminated actors in the course of the
			% simulation:

			KnownActors = ?getAttr(known_local_actors),

			NewKnownActors = case ?getAttr(actors_to_delete_at_next_tick) of

				[] ->
					KnownActors;

				ActorsToDeleteNow ->
					wooper:delete_synchronously_instances( ActorsToDeleteNow ),
					% Now remove these deleted actors from the known ones:
					DeletedSet = set_utils:from_list( ActorsToDeleteNow ),
					set_utils:difference( KnownActors, DeletedSet )

			end,

			setAttributes( ManagedState, [
					{ previous_timestamp, NewTimestamp },
					{ known_local_actors, NewKnownActors },
					{ actors_to_delete_at_next_tick, [] } ] )


	end,

	wooper:return_state( FinalState ).


% Notifies this time manager that the subtree corresponding to its direct child
% time manager that sent this message finished its spontaneous scheduling (first
% diasca) for the current tick.
%
-spec notifySpontaneousSubtreeCompletion( wooper:state(), tick_offset(),
		time_manager_pid(), next_manager_action(), diasca_tracking_info() ) ->
												oneway_return().
notifySpontaneousSubtreeCompletion( State, SubtreeTickOffset, ChildManagerPid,
	  NextActionInSubtree,
	  _TrackingInfo={ ChildScheduleCount, ChildProcessCount } ) ->

	?display_console( "Notified of the completion of subtree managed by ~w.",
					  [ ChildManagerPid ] ),

	WaitedManagers = ?getAttr(waited_child_managers),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received "
			"notifySpontaneousSubtreeCompletion from ~w at #~B, reporting for "
			"next action '~p'; was still waiting for ~w.",
			[ self(), ChildManagerPid, SubtreeTickOffset,
			  NextActionInSubtree, set_utils:to_list( WaitedManagers ) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			% First, some (optional) checkings:
			check_waited_count_consistency( State ),
			true = set_utils:member( ChildManagerPid, WaitedManagers ),
			SubtreeTickOffset = ?getAttr(current_tick_offset),
			0 = ?getAttr(current_diasca),
			true = set_utils:is_empty( ?getAttr(waited_triggered_actors) )
		end ),

	SoonestState = update_next_action_with( NextActionInSubtree, State ),

	% Already checked for membership:
	RemainingWaitedManagers =
		set_utils:delete( ChildManagerPid, WaitedManagers ),

	CurrentScheduleCount = ?getAttr(scheduled_tracking) + ChildScheduleCount,

	CurrentProcessCount = ?getAttr(process_tracking) + ChildProcessCount,

	CurrentOverallScheduleCount = ?getAttr(schedule_count) + ChildScheduleCount,

	WaitedState = setAttributes( SoonestState, [
			{ waited_child_managers, RemainingWaitedManagers },
			{ scheduled_tracking, CurrentScheduleCount },
			{ process_tracking, CurrentProcessCount },
			{ schedule_count, CurrentOverallScheduleCount },
			{ waited_count, ?getAttr(waited_count) - 1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( WaitedState ) ).




% Notifies this time manager that the specified local actor finished its
% spontaneous actions (first diasca) for the current tick, and tells what it
% expects next in terms of scheduling.
%
% Clauses are sorted by decreasing probability.
%
-spec notifySpontaneousActionsCompleted( wooper:state(), tick_offset(),
		actor_pid(), class_Actor:next_reported_action(), [ tick_offset() ],
		[ tick_offset() ] ) -> oneway_return().
notifySpontaneousActionsCompleted( State, ActorTickOffset, ActorPid,
		  _NextActorAction=no_diasca_requested, AddedSpontaneousTicks,
		  WithdrawnSpontaneousTicks ) ->

	% Here this actor does not request any diasca, so it must not have sent any
	% actor message this diasca.

	WaitedActors = ?getAttr(waited_spontaneous_actors),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received "
			"notifySpontaneousActionsCompleted from ~w at #~B, reporting no "
			"need for a new diasca; "
			"was still waiting for ~B spontaneous actors.",
			[ self(), ActorPid, ActorTickOffset,
			  set_utils:size( WaitedActors ) ] ) ),

	%trace_utils:debug_fmt(
	%   "Adding for actor ~w following spontaneous ticks: ~w.",
	%   [ ActorPid, AddedSpontaneousTicks ] ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = set_utils:member( ActorPid, WaitedActors ),
			ActorTickOffset = ?getAttr(current_tick_offset),
			0 = ?getAttr(current_diasca),
			true = set_utils:is_empty( ?getAttr(waited_triggered_actors) )
		end ),

	% Note: the agenda is updated, but not the next action:
	AgendaState = update_agenda( AddedSpontaneousTicks,
			WithdrawnSpontaneousTicks, ActorTickOffset, ActorPid, State ),

	% Already checked for membership:
	NewWaitedSpontaneousActors = set_utils:delete( ActorPid, WaitedActors ),

	% next_timestamp, next_action, etc. not to be changed here.

	UpdatedState = setAttributes( AgendaState, [
			{ waited_spontaneous_actors, NewWaitedSpontaneousActors },
			{ waited_count, ?getAttr(waited_count) - 1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( UpdatedState ) );



notifySpontaneousActionsCompleted( State, ActorTickOffset, ActorPid,
		  _NextActorAction=new_diasca_needed, AddedSpontaneousTicks,
		  WithdrawnSpontaneousTicks ) ->

	% Here the actor requests a new diasca, most probably because it sent an
	% actor message this diasca. This means the next diasca should be scheduled,
	% but not necessarily that this sending actor is to be scheduled (the
	% receiving actor must have notified its own time manager about that).

	WaitedActors = ?getAttr(waited_spontaneous_actors),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received "
			"notifySpontaneousActionsCompleted from ~w at #~B, reporting the "
			"need for a new diasca; "
			"was still waiting for ~B spontaneous actors.",
			[ self(), ActorPid, ActorTickOffset,
			  set_utils:size( WaitedActors ) ] ) ),

	%trace_utils:debug_fmt(
	%   "Adding for actor ~w following spontaneous ticks: ~w.",
	%   [ ActorPid, AddedSpontaneousTicks ] ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = set_utils:member( ActorPid, WaitedActors ),
			ActorTickOffset = ?getAttr(current_tick_offset),
			0 = ?getAttr(current_diasca),
			true = set_utils:is_empty( ?getAttr(waited_triggered_actors) )
		end ),

	AgendaState = update_agenda( AddedSpontaneousTicks,
			 WithdrawnSpontaneousTicks, ActorTickOffset, ActorPid, State ),

	% Already checked for membership:
	NewWaitedSpontaneousActors = set_utils:delete( ActorPid, WaitedActors ),

	UpdatedState = setAttributes( AgendaState, [
			   { next_timestamp, { ActorTickOffset, 1 } },
			   { next_action, new_diasca_needed },
			   { waited_spontaneous_actors, NewWaitedSpontaneousActors } ,
			   { waited_count, ?getAttr(waited_count) - 1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( UpdatedState ) );



notifySpontaneousActionsCompleted( State, ActorTickOffset, ActorPid,
		_NextActorAction=terminating, _AddedSpontaneousTicks=[],
		_WithdrawnSpontaneousTicks=[] ) ->

	% Here the actor is (actively) terminating, has specified a number of
	% intercalary diascas and has not exhausted them.
	%
	% This implies that the next diasca must be scheduled (regardless whether it
	% sent an actor message or not), and that this actor will have to be
	% scheduled during it.

	WaitedActors = ?getAttr(waited_spontaneous_actors),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received "
			"notifySpontaneousActionsCompleted from ~w at #~B, reporting the "
			"actor termination; "
			"was still waiting for ~B spontaneous actors.",
			[ self(), ActorPid, ActorTickOffset,
			  set_utils:size( WaitedActors ) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = set_utils:member( ActorPid, WaitedActors ),
			ActorTickOffset = ?getAttr(current_tick_offset),
			0 = ?getAttr(current_diasca),
			true = set_utils:is_empty( ?getAttr(waited_triggered_actors) )
		end ),

	% List reset at each diasca:
	NewTerminating = [ ActorPid | ?getAttr(terminating_actors) ],

	% Already checked for membership:
	NewWaitedSpontaneousActors = set_utils:delete( ActorPid, WaitedActors ),

	UpdatedState = setAttributes( State, [
			{ next_timestamp, { ActorTickOffset, 1 } },
			{ next_action, new_diasca_needed },
			{ terminating_actors, NewTerminating },
			{ waited_spontaneous_actors, NewWaitedSpontaneousActors },
			{ waited_count, ?getAttr(waited_count) - 1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( UpdatedState ) );



notifySpontaneousActionsCompleted( State, ActorTickOffset, ActorPid,
		_NextActorAction={ terminating_unlimited, DiascaRequest },
		_AddedSpontaneousTicks=[], _WithdrawnSpontaneousTicks=[] ) ->

	% Here the actor has for next action { terminating, unlimited }; hence it is
	% passively terminating, we do not specifically schedule it anymore, and
	% simply will delete it at the next tick, regardless of the number of diasca
	% elapsed.

	WaitedActors = ?getAttr(waited_spontaneous_actors),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received "
			"notifySpontaneousActionsCompleted from ~w at #~B, reporting this "
			"actor is passively terminating;"
			"was still waiting for ~B spontaneous actors.",
			[ self(), ActorPid, ActorTickOffset,
			  set_utils:size( WaitedActors ) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = set_utils:member( ActorPid, WaitedActors ),
			ActorTickOffset = ?getAttr(current_tick_offset),
			0 = ?getAttr(current_diasca),
			true = set_utils:is_empty( ?getAttr(waited_triggered_actors) )
		end ),

	% This actor will be left as is during the current tick, then will be
	% deleted.
	%
	ActorsToDelete = [ ActorPid | ?getAttr(actors_to_delete_at_next_tick) ],

	% Already checked for membership:
	NewWaitedSpontaneousActors = set_utils:delete( ActorPid, WaitedActors ),

	{ NewNextTimestamp, NewNextAction } = case DiascaRequest of

		no_diasca_requested ->
			{ ?getAttr(next_timestamp), ?getAttr(next_action) };

		new_diasca_needed ->
			{ { ActorTickOffset, 1 }, new_diasca_needed }

	end,

	UpdatedState = setAttributes( State, [
				{ next_timestamp, NewNextTimestamp },
				{ next_action, NewNextAction },
				{ actors_to_delete_at_next_tick, ActorsToDelete },
				{ waited_spontaneous_actors, NewWaitedSpontaneousActors },
				{ waited_count, ?getAttr(waited_count) - 1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( UpdatedState ) );



notifySpontaneousActionsCompleted( State, ActorTickOffset, ActorPid,
		_NextActorAction={ terminated, DiascaRequest },
		_AddedSpontaneousTicks=[], _WithdrawnSpontaneousTicks=[] ) ->

	% This actor has terminated now, it could be deleted on any next
	% diasca/tick, but we leave it live until the next tick (not depending on
	% the number of intermediary diascas), to be able to catch any unexpected
	% late actor message directed at it.

	WaitedActors = ?getAttr(waited_spontaneous_actors),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received "
			"notifySpontaneousActionsCompleted from ~w at #~B, reporting the "
			"actual actor termination; "
			"was still waiting for ~B triggered actors.",
			[ self(), ActorPid, ActorTickOffset,
			  set_utils:size( WaitedActors ) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = set_utils:member( ActorPid, WaitedActors ),
			ActorTickOffset = ?getAttr(current_tick_offset),
			0 = ?getAttr(current_diasca),
			true = set_utils:is_empty( ?getAttr(waited_triggered_actors) )
		end ),

	% Already checked for membership:
	NewWaitedSpontaneousActors = set_utils:delete( ActorPid, WaitedActors ),

	% Postponing a bit the actual deletion:
	ActorsToDelete = [ ActorPid | ?getAttr(actors_to_delete_at_next_tick) ],

	{ NewNextTimestamp, NewNextAction } = case DiascaRequest of

		no_diasca_requested ->
			{ ?getAttr(next_timestamp), ?getAttr(next_action) };

		new_diasca_needed ->
			{ { ActorTickOffset, 1 }, new_diasca_needed }

	end,

	UpdatedState = setAttributes( State, [
				{ next_timestamp, NewNextTimestamp },
				{ next_action, NewNextAction },
				{ actors_to_delete_at_next_tick, ActorsToDelete },
				{ waited_spontaneous_actors, NewWaitedSpontaneousActors },
				{ waited_count, ?getAttr(waited_count) - 1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( UpdatedState ) ).




% Notifies this (root) time manager that the watchdog finished its spontaneous
% actions (first diasca) for the current tick.
%
-spec notifySpontaneousWatchdogCompleted( wooper:state(), tick_offset() ) ->
												oneway_return().
notifySpontaneousWatchdogCompleted( State, WatchdogTickOffset ) ->

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received "
			"notifySpontaneousWatchdogCompleted from watchdog at #~B, while "
			"still waiting for ~B spontaneous actor(s).",
			[ self(), WatchdogTickOffset,
			  set_utils:size( ?getAttr(waited_spontaneous_actors) ) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = ?getAttr(watchdog_waited),
			WatchdogTickOffset = ?getAttr(current_tick_offset),
			0 = ?getAttr(current_diasca),
			true = set_utils:is_empty( ?getAttr(waited_triggered_actors) )
		end ),

	% Nothing to do. Although it is not necessary, we prefer to force the
	% keeping in sync with the watchdog as well.

	AcknowledgedState = setAttributes( State, [
		{ waited_count, ?getAttr(waited_count) - 1 },
		{ watchdog_waited, false } ] ),

	% Yes, indeed, it may actually happen that the watchdog is the last to
	% answer, even in a distributed context!
	%
	wooper:return_state(
				manage_possible_end_of_diasca( AcknowledgedState ) ).





% Subsection for the scheduling of all diascas that may be created after the
% first one (which is dedicated to spontaneous actions).



% Notifies this time manager that a new (strictly positive) diasca tick is to
% begin now (thus after diasca 0, which is dedicated to spontaneous actions); to
% be sent by the direct parent time manager (if any).
%
% Takes care of the beginning of the specified new diasca, which implies sending
% this message recursively through the whole scheduling tree, so that all
% relevant actors can be notified.
%
-spec beginTimeManagerDiasca( wooper:state(), tick_offset(), diasca() ) ->
									oneway_return().
beginTimeManagerDiasca( State, TickOffset, NewDiasca ) ->

	?display_console( "beginTimeManagerDiasca: at tick offset #~B, "
					  "new diasca is ~B.", [ TickOffset, NewDiasca ] ),

	cond_utils:if_defined( simdiasca_check_time_management,
		check_diasca_consistency( TickOffset, NewDiasca, State ) ),

	% actors_to_trigger_in_one_diasca and/or actors_to_trigger_in_two_diascas
	% might be non-empty here.

	NewTimestamp = { TickOffset, NewDiasca },

	NewState = setAttributes( State, [
		{ current_diasca, NewDiasca },

		cond_utils:if_defined( exec_target_is_production,
			{ trace_timestamp, { TickOffset, NewDiasca } },
			{ trace_timestamp,
			  get_trace_timestamp( TickOffset, NewDiasca, State ) } ),

		{ trace_timestamp, NewTimestamp },
		{ next_action, no_planned_action } ] ),

	% Let's run this new diasca, and possibly wait for answers:
	ManagedState = manage_new_diasca( TickOffset, NewDiasca, NewState ),

	FinalState = setAttribute( ManagedState, previous_timestamp,
							   NewTimestamp ),

	wooper:return_state( FinalState ).




% Notifies this time manager that the subtree corresponding to its direct child
% time manager that sent this message finished the specified (strictly positive)
% diasca for the current tick.
%
-spec notifyTriggerSubtreeCompletion( wooper:state(), tick_offset(),
		 diasca(), time_manager_pid(), next_manager_action(),
		 diasca_tracking_info() ) -> oneway_return().
notifyTriggerSubtreeCompletion( State, SubtreeTickOffset, SubtreeDiasca,
			  ChildManagerPid, NextActionInSubtree,
			  _TrackingInfo={ ChildScheduleCount, ChildProcessCount } ) ->

	WaitedManagers = ?getAttr(waited_child_managers),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received notifyTriggerSubtreeCompletion "
			"from ~w at #~B diasca ~B, reporting for next action '~p'; "
			"was still waiting for ~p.",
			[ self(), ChildManagerPid, SubtreeTickOffset, SubtreeDiasca,
			  NextActionInSubtree, set_utils:to_list( WaitedManagers ) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = set_utils:member( ChildManagerPid, WaitedManagers ),
			SubtreeTickOffset = ?getAttr(current_tick_offset),
			SubtreeDiasca = ?getAttr(current_diasca),
			true = ( is_integer( SubtreeDiasca ) andalso SubtreeDiasca =/= 0 ),
			true = set_utils:is_empty( ?getAttr(waited_spontaneous_actors) )
		end ),

	SoonestState = update_next_action_with( NextActionInSubtree, State ),

	RemainingWaitedManagers = set_utils:delete( ChildManagerPid,
												WaitedManagers ),

	CurrentScheduleCount = ?getAttr(scheduled_tracking) + ChildScheduleCount,

	CurrentProcessCount = ?getAttr(process_tracking) + ChildProcessCount,

	CurrentOverallScheduleCount = ?getAttr(schedule_count) + ChildScheduleCount,

	WaitedState = setAttributes( SoonestState, [
			{ waited_child_managers, RemainingWaitedManagers },
			{ scheduled_tracking, CurrentScheduleCount },
			{ process_tracking, CurrentProcessCount },

			% Never reset:
			{ schedule_count, CurrentOverallScheduleCount },
			{ waited_count, ?getAttr(waited_count) -1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( WaitedState ) ).




% Notifies this time manager that the specified local actor finished its current
% (non-first) diasca for the current tick, and tells what it expects next in
% terms of scheduling.
%
% Clauses are sorted by decreasing probability.
%
-spec notifyTriggeredActionsCompleted( wooper:state(), tick_offset(), diasca(),
		actor_pid(), class_Actor:next_reported_action(),
		[ tick_offset() ], [ tick_offset() ] ) -> oneway_return().
notifyTriggeredActionsCompleted( State, ActorTickOffset, ActorDiasca, ActorPid,
	   _NextActorAction=no_diasca_requested, AddedSpontaneousTicks,
	   WithdrawnSpontaneousTicks ) ->

	cond_utils:if_defined( simdiasca_debug_time_management,
		trace_utils:debug_fmt(
			"Adding at ~w for actor ~w following spontaneous ticks: ~w "
			"(no_diasca_requested).",
			[ { ActorTickOffset, ActorDiasca }, ActorPid,
			  AddedSpontaneousTicks ] ) ),

	% Here this actor does not request any diasca, so it must not have sent any
	% actor message this diasca.

	WaitedActors = ?getAttr(waited_triggered_actors),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received notifyTriggeredActionsCompleted "
			"from ~w at #~B diasca ~B, reporting no need for a new diasca;"
			" was still waiting for ~B triggered actors.",
			[ self(), ActorPid, ActorTickOffset, ActorDiasca,
			  set_utils:size( WaitedActors ) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = set_utils:member( ActorPid, WaitedActors ),
			ActorTickOffset = ?getAttr(current_tick_offset),
			ActorDiasca = ?getAttr(current_diasca),
			true = ( is_integer( ActorDiasca ) andalso ActorDiasca =/= 0 ),
			true = set_utils:is_empty( ?getAttr(waited_spontaneous_actors) )
		end ),

	% Note: the agenda is updated, but not the next action:
	AgendaState = update_agenda( AddedSpontaneousTicks,
		WithdrawnSpontaneousTicks, ActorTickOffset, ActorPid, State ),

	% Already checked for membership:
	NewWaitedTriggeredActors = set_utils:delete( ActorPid, WaitedActors ),

	% next_timestamp, next_action, etc. should not to be changed here (will be
	% determined just when needed, when reporting the end of this diasca).

	UpdatedState = setAttributes( AgendaState, [
				{ waited_triggered_actors, NewWaitedTriggeredActors },
				{ waited_count, ?getAttr(waited_count) - 1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( UpdatedState ) );



notifyTriggeredActionsCompleted( State, ActorTickOffset, ActorDiasca, ActorPid,
	   _NextActorAction=new_diasca_needed, AddedSpontaneousTicks,
	   WithdrawnSpontaneousTicks ) ->

	%trace_utils:debug_fmt(
	%  "Adding at ~w for actor ~w following spontaneous ticks: ~w "
	%  "(new_diasca_needed).",
	%  [ { ActorTickOffset, ActorDiasca }, ActorPid, AddedSpontaneousTicks ] ),

	% Here the actor requests a new diasca, most probably because it sent an
	% actor message this diasca. This means the next diasca should be scheduled,
	% but not necessarily that this sending actor is to be scheduled (the
	% receiving actor must have notified its own time manager about that).

	WaitedActors = ?getAttr(waited_triggered_actors),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received notifyTriggeredActionsCompleted "
			"from ~w at #~B diasca ~B, reporting the need for a new diasca;"
			" was still waiting for ~B triggered actors.",
			[ self(), ActorPid, ActorTickOffset, ActorDiasca,
			  set_utils:size( WaitedActors ) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin

			check_waited_count_consistency( State ),

			% If this match fails, it may be the sign that at least one of your
			% classes sent an actor message from its constructor, whereas it is
			% not allowed (it should be done from its onFirstDiasca/2 actor
			% oneway onward).
			%
			true = set_utils:member( ActorPid, WaitedActors ),
			ActorTickOffset = ?getAttr(current_tick_offset),
			ActorDiasca = ?getAttr(current_diasca),
			true = ( is_integer( ActorDiasca ) andalso ActorDiasca =/= 0 ),
			true = set_utils:is_empty( ?getAttr(waited_spontaneous_actors) )
		end ),

	% Note: the agenda is updated, but not the next action:
	AgendaState = update_agenda( AddedSpontaneousTicks,
			 WithdrawnSpontaneousTicks, ActorTickOffset, ActorPid, State ),

	% Already checked for membership:
	NewWaitedTriggeredActors = set_utils:delete( ActorPid, WaitedActors ),

	UpdatedState = setAttributes( AgendaState, [
				{ next_timestamp, { ActorTickOffset, ActorDiasca+1 } },
				{ next_action, new_diasca_needed },
				{ waited_triggered_actors, NewWaitedTriggeredActors },
				{ waited_count, ?getAttr(waited_count) - 1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( UpdatedState ) );



notifyTriggeredActionsCompleted( State, ActorTickOffset, ActorDiasca, ActorPid,
		_NextActorAction=terminating, _AddedSpontaneousTicks=[],
		_WithdrawnSpontaneousTicks=[] ) ->

	% Here the actor is (actively) terminating, has specified a number of
	% intercalary diascas and has not exhausted them.
	%
	% This implies that the next diasca must be scheduled, and that this actor
	% will have to be scheduled during it.

	WaitedActors = ?getAttr(waited_triggered_actors),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received notifyTriggeredActionsCompleted "
			"from ~w at #~B diasca ~B, reporting this actor is actively "
			"terminating; was still waiting for ~B triggered actors.",
			[ self(), ActorPid, ActorTickOffset, ActorDiasca,
			  set_utils:size( WaitedActors ) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = set_utils:member( ActorPid, WaitedActors ),
			ActorTickOffset = ?getAttr(current_tick_offset),
			ActorDiasca = ?getAttr(current_diasca),
			true = ( is_integer( ActorDiasca ) andalso ActorDiasca =/= 0 ),
			true = set_utils:is_empty( ?getAttr(waited_spontaneous_actors) )
		end ),

	NewTerminating = [ ActorPid | ?getAttr(terminating_actors) ],

	% Already checked for membership:
	NewWaitedTriggeredActors = set_utils:delete( ActorPid, WaitedActors ),

	UpdatedState = setAttributes( State, [
			{ next_timestamp, { ActorTickOffset, ActorDiasca + 1 } },
			{ next_action, new_diasca_needed },
			{ terminating_actors, NewTerminating },
			{ waited_triggered_actors, NewWaitedTriggeredActors },
			{ waited_count, ?getAttr(waited_count) - 1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( UpdatedState ) );



notifyTriggeredActionsCompleted( State, ActorTickOffset, ActorDiasca, ActorPid,
		_NextActorAction={ terminating_unlimited, DiascaRequest },
		_AddedSpontaneousTicks=[], _WithdrawnSpontaneousTicks=[] ) ->

	% Here the actor has for next action { terminating, unlimited }; hence it is
	% passively terminating, we do not specifically schedule it anymore, and
	% simply will delete it at the next tick, regardless of the number of diasca
	% elapsed.

	WaitedActors = ?getAttr(waited_triggered_actors),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received notifyTriggeredActionsCompleted "
			"from ~w at #~B diasca ~B, reporting this actor is passively "
			"terminating; was still waiting for ~B triggered actors; "
			"next action is ~p.",
			[ self(), ActorPid, ActorTickOffset, ActorDiasca,
			  set_utils:size( WaitedActors ), ?getAttr(next_action) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = set_utils:member( ActorPid, WaitedActors ),
			ActorTickOffset = ?getAttr(current_tick_offset),
			ActorDiasca = ?getAttr(current_diasca),
			true = ( is_integer( ActorDiasca ) andalso ActorDiasca =/= 0 ),
			true = set_utils:is_empty( ?getAttr(waited_spontaneous_actors) )
		end ),

	% This actor will be left as is during the current tick, then will be
	% deleted.
	%
	ActorsToDelete = [ ActorPid | ?getAttr(actors_to_delete_at_next_tick) ],

	{ NewNextTimestamp, NewNextAction } = case DiascaRequest of

		no_diasca_requested ->
			{ ?getAttr(next_timestamp), ?getAttr(next_action) };

		new_diasca_needed ->
			{ { ActorTickOffset, ActorDiasca + 1 }, new_diasca_needed }

	end,

	% Already checked for membership:
	NewWaitedTriggeredActors = set_utils:delete( ActorPid, WaitedActors ),

	UpdatedState = setAttributes( State, [
				{ next_timestamp, NewNextTimestamp },
				{ next_action, NewNextAction },
				{ actors_to_delete_at_next_tick, ActorsToDelete },
				{ waited_triggered_actors, NewWaitedTriggeredActors },
				{ waited_count, ?getAttr(waited_count) - 1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( UpdatedState ) );



notifyTriggeredActionsCompleted( State, ActorTickOffset, ActorDiasca, ActorPid,
		_NextActorAction={ terminated, DiascaRequest },
		_AddedSpontaneousTicks=[], _WithdrawnSpontaneousTicks=[] ) ->

	% This actor has terminated now, it could be deleted on any next
	% diasca/tick, but we leave it live until the next tick (not depending on
	% the number of intermediary diascas), to be able to catch any unexpected
	% late actor message directed at it.

	WaitedActors = ?getAttr(waited_triggered_actors),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received notifyTriggeredActionsCompleted "
			"from ~w at #~B diasca ~B, reporting the actual actor "
			"termination; was still waiting for ~B triggered actors.",
			[ self(), ActorPid, ActorTickOffset, ActorDiasca,
			  set_utils:size( WaitedActors ) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = set_utils:member( ActorPid, WaitedActors ),
			ActorTickOffset = ?getAttr(current_tick_offset),
			ActorDiasca = ?getAttr(current_diasca),
			true = ( is_integer( ActorDiasca ) andalso ActorDiasca =/= 0 ),
			true = set_utils:is_empty( ?getAttr(waited_spontaneous_actors) )
		end ),

	% Already checked for membership:
	NewWaitedTriggeredActors = set_utils:delete( ActorPid, WaitedActors ),

	% Postponing a bit the actual deletion:
	ActorsToDelete = [ ActorPid | ?getAttr(actors_to_delete_at_next_tick) ],

	{ NewNextTimestamp, NewNextAction } = case DiascaRequest of

		no_diasca_requested ->
			{ ?getAttr(next_timestamp), ?getAttr(next_action) };

		new_diasca_needed ->
			{ { ActorTickOffset, ActorDiasca + 1 }, new_diasca_needed }

	end,

	UpdatedState = setAttributes( State, [
					{ next_timestamp, NewNextTimestamp },
					{ next_action, NewNextAction },
					{ actors_to_delete_at_next_tick, ActorsToDelete },
					{ waited_triggered_actors, NewWaitedTriggeredActors },
					{ waited_count, ?getAttr(waited_count) - 1 } ] ),

	wooper:return_state( manage_possible_end_of_diasca( UpdatedState ) ).




% Notifies this (root) time manager that the watchdog finished its current
% (non-first) diasca for the current tick.
%
-spec notifyTriggeredWatchdogCompleted( wooper:state(),
							tick_offset(), diasca() ) -> oneway_return().
notifyTriggeredWatchdogCompleted( State, WatchdogTickOffset, WatchdogDiasca ) ->

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "Time manager ~w received notifyTriggeredWatchdogCompleted "
			"from watchdog at #~B diasca ~B, while still waiting "
			"for ~B triggered actor(s).",
			[ self(), WatchdogTickOffset, WatchdogDiasca,
			  set_utils:size( ?getAttr(waited_spontaneous_actors) ) ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_waited_count_consistency( State ),
			true = ?getAttr(watchdog_waited),
			WatchdogTickOffset = ?getAttr(current_tick_offset),
			WatchdogDiasca = ?getAttr(current_diasca),
			true = set_utils:is_empty( ?getAttr(waited_spontaneous_actors) )
		end ),

	% Nothing to do. Although it is not necessary, we prefer to force the
	% keeping in sync with the watchdog as well.

	WaitedCount = ?getAttr(waited_count),

	AcknowledgedState = setAttributes( State, [
		{ waited_count, WaitedCount - 1 },
		{ watchdog_waited, false } ] ),

	% Yes, indeed, it may actually happen that the watchdog is the last to
	% answer, even in a distributed context!
	%
	wooper:return_state(
				 manage_possible_end_of_diasca( AcknowledgedState ) ).




% Notifies this time manager that the tick timer determined that a new tick
% should occur (based on wallclock time); if ready, the manager will increment
% the tick of the simulation clock and triggers its processing.
%
% To be called by the internal timer, only if being the root time manager and
% being in simulation interactive mode.
%
% Note: we do not enter into spontaneous actions/diasca considerations, as the
% tick timer exists only for the simulation interactive mode, only interested
% into ticks, i.e. in the synchronisation of wallclock and simulated time.
%
% We do not even record the actual, current values of ticks (we just notify that
% one elapsed), as we do not want to maintain here the actual tick offsets in
% spite of delays, drops and other events.
%
-spec timerTickFinished( wooper:state() ) -> oneway_return().
timerTickFinished( State ) ->

	%?info( "Received a timerTickFinished notification." ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			interactive = ?getAttr(simulation_interactivity_mode),
			check_waited_count_consistency( State )
		end ),

	% Checks whether this (root) time manager has indeed finished the evaluation
	% of its previous tick and hence is ready on time:
	%
	case ?getAttr(waited_count) of

		0 ->

			% Yes, all agents reported the tick could be ended, so we are on
			% time indeed, as we suppose here that this manager is not
			% overloaded enough to process this oneway with significant delay.
			%
			% So we just suppose this call corresponds to the real time and just
			% check that actors are on time.
			%
			% A more robust approach would be to base at least the timer
			% on the real time, not on receive time-outs.

			CurrentTickOffset = ?getAttr(current_tick_offset),

			?debug_fmt( "The simulation is on time for tick offset #~B.",
						[ CurrentTickOffset ] ),

			% Ready for next tick... if not suspended:
			SuspendedState = manage_suspension( CurrentTickOffset, State ),

			% Still supposed to be running?
			TimerState = case getAttribute( SuspendedState, started ) of

				true ->
					% Yes, so continue with the next tick (using a message to
					% ensure constant space), provided it has not already been
					% triggered (as timerTickFinished messages may accumulate):
					case getAttribute( SuspendedState,
									   interactive_tick_triggered ) of

						true ->
							% Tick already triggered, nothing more to do, except
							% that we do not want this timer message to be
							% silently eaten, so:
							%
							self() ! timerTickFinished,
							SuspendedState;

						false ->
							NewTickOffset = CurrentTickOffset + 1,
							self() ! { beginTimeManagerTick, NewTickOffset },
							setAttribute( SuspendedState,
								 interactive_tick_triggered, true )

					end;

				false ->
					% No new tick should be begun if stopped in the meantime: a
					% stop request might have been issued whereas a
					% timerTickFinished message was still in the mailbox of this
					% manager.
					%
					% If it was the case, just do nothing, to avoid beginning a
					% new tick whereas stopped:
					%
					SuspendedState

			end,
			wooper:return_state( TimerState );


		undefined ->
			throw( faulty_interactive_waited_count );


		_NonZeroCount ->

			%?warning_fmt( "Cannot keep up with the interactive pace, still "
			%			  "waiting for ~B agents, loosing sync with hard "
			%			  "real time, continuing in a best-effort basis.",
			%			  [ NonZeroCount ] ),

			% Tick not changed, still waiting:
			wooper:const_return()

	end.




% Section for time synchronisation of operations.


% Returns the current simulation time, as an absolute tick.
-spec getSimulationTick( wooper:state() ) -> const_request_return( tick() ).
getSimulationTick( State ) ->
	wooper:const_return_result( get_current_tick( State ) ).



% Returns the current simulation tick offset, if the simulation has already been
% started, otherwise the atom 'undefined'.
%
-spec getSimulationTickOffset( wooper:state() ) ->
									const_request_return( tick_offset() ).
getSimulationTickOffset( State ) ->
	wooper:const_return_result( ?getAttr(current_tick_offset) ).



% Returns the current simulation diasca, if the simulation has already been
% started, otherwise the atom 'undefined'.
%
-spec getSimulationDiasca( wooper:state() ) ->
								const_request_return( maybe( diasca() ) ).
getSimulationDiasca( State ) ->
	wooper:const_return_result( ?getAttr(current_diasca) ).



% Returns the current simulation timestamp (tick offset and diasca), if the
% simulation has already been started, otherwise the atom 'undefined'.
%
-spec getSimulationLogicalTimestamp( wooper:state() ) ->
					const_request_return( maybe( logical_timestamp() ) ).
getSimulationLogicalTimestamp( State ) ->

	Res = case ?getAttr(current_tick_offset) of

		undefined ->
			undefined;

		TickOffset ->
			{ TickOffset, ?getAttr(current_diasca) }

	end,

	wooper:const_return_result( Res ).




% Returns the current simulation time, structured as follows:
% { {SimYear,SimMonth,SimDay}, {SimHour,SimMinute,SimSecond} }.
%
-spec getSimulationDate( wooper:state() ) ->
							const_request_return( timestamp() ).
getSimulationDate( State ) ->

	Seconds = convert_ticks_to_rounded_seconds( get_current_tick( State ),
												State ),

	wooper:const_return_result(
	  calendar:gregorian_seconds_to_datetime( Seconds ) ).



% Returns a textual description of the simulation and real time.
-spec getTextualTimings( wooper:state() ) -> const_request_return( ustring() ).
getTextualTimings( State ) ->
	wooper:const_return_result( get_textual_timings( State ) ).



% Converts the specified number of seconds (expressed as an integer or a
% floating-point value, i.e. any granularity below the second can be specified)
% into an integer (rounded, non-negative) number of ticks, using the target time
% manager.
%
% Ex: TimeManager ! { convertSecondsToTicks, 0.02, self() }.
% Returns the appropriate (integer) number of ticks.
%
% Note that, due to rounding, depending on the current frequency of the time
% manager and on the specified duration, the returned duration might be zero
% tick, i.e. a null duration.
%
% Note also that models are expected to call the counterpart
% class_Actor:convert_seconds_to_ticks/{2,3} helper functions, rather than
% interacting with their time manager.
%
-spec convertSecondsToTicks( wooper:state(), any_seconds() ) ->
								const_request_return( tick_offset() ).
convertSecondsToTicks( State, Seconds ) ->
	wooper:const_return_result( convert_seconds_to_ticks( Seconds, State ) ).



% Converts the specified number of seconds (expressed as an integer or a
% floating-point value, i.e. any granularity below the second can be specified)
% into an integer (rounded, strictly positive) number of ticks, using the target
% time manager and ensuring that, if ever the rounding would have led to a
% zero-tick duration, a duration of one tick is returned instead.
%
% Ex: TimeManager ! {convertSecondsToNonNullTickDuration, 0.02, self()}.
% Returns the appropriate (integer, strictly positive) number of ticks.
%
% Useful to ensure that under no circumstances a duration can be null, in order
% that planned actions will always happen in a strict future.
%
-spec convertSecondsToNonNullTickDuration( wooper:state(), any_seconds() ) ->
										const_request_return( tick_offset() ).
convertSecondsToNonNullTickDuration( State, Seconds ) ->

	Count = case convert_seconds_to_ticks( Seconds, State ) of

		0 ->
			1 ;

		NonNullCount ->
			NonNullCount

	end,

	wooper:const_return_result( Count ).



% Converts the specified tick count into an integer (rounded) number of seconds,
% using the specified time manager.
%
% Returns the appropriate number of seconds.
%
-spec convertTicksToSeconds( wooper:state(), tick_offset() ) ->
								const_request_return( seconds() ).
convertTicksToSeconds( State, Ticks ) ->
	wooper:const_return_result(
		convert_ticks_to_rounded_seconds( Ticks, State ) ).


% Converts the specified tick count into a floating-point number of seconds,
% using the specified time manager.
%
% Returns the appropriate number of seconds.
%
-spec convertTicksToPreciseDuration( wooper:state(), tick_offset() ) ->
				const_request_return( virtual_seconds() ).
convertTicksToPreciseDuration( State, Ticks ) ->
	wooper:const_return_result(
		convert_ticks_to_seconds( Ticks, State ) ).



% Section dedicated to the subscribing/unsubscribing of the simulated actors.



% Requests the manager to subscribe the calling actor:
%
% - AbstractActorIdentifier is the AAI of the actor to subscribe
%
% - ActorBinName is the name of this actor (as a binary)
%
% - Classname is the class of this actor (as an atom)
%
% Any just subscribed actor will be notified of the next simulation diasca,
% which, if the simulation is running, will be the current one plus one,
% regardless of any skip of ticks which could be already planned.
%
% Returns either:
%
% - time_subscribed if the operation succeeded (subscribed, and was not
% subscribed yet); the actor will receive the begin notification for the next
% scheduled diasca
%
% - already_time_subscribed if the caller was already subscribed (it is still
% subscribed only once)
%
% Note: this method could not be named 'register', as it is a reserved word.
%
% Ex: MyTimeManager ! { subscribe, [ MyAAI, MyBinName, MyClassname ], self() }
%
% The current timestamp of manager cannot be returned, as it may not be started
% yet (in which case there is not even a current time).
%
% Note: in the future AAI, subscription will be a oneway, and the load-balancer
% will supply all relevant information (in addition to AAI and seeding).
%
-spec subscribe( wooper:state(), class_Actor:aai(), text_utils:bin_string(),
				 classname() ) -> request_return(
	'already_time_subscribed'
	| { 'time_subscribed', virtual_seconds(),
		'not_started_yet' | { tick(), logical_timestamp() } } ).
subscribe( State, AbstractActorIdentifier, ActorBinName, Classname ) ->

	% PID retrieved from request:
	CallerPid = ?getSender(),

	case ?getAttr(troubleshooting_mode) of

		true ->

			% Updates directly the right instance tracker: this time manager,
			% the subscribing actor and the instance tracker that shall be
			% targeted are by design on the same node, thus a local look-up is
			% the best approach.
			%
			LocalInstanceTracker = class_InstanceTracker:get_local_tracker(),

			LocalInstanceTracker ! { registerActor, [ AbstractActorIdentifier,
						ActorBinName, CallerPid, Classname ] };

		false ->
			ok

	end,

	% Links together this manager and the calling listener (usually an actor),
	% so that the termination of one will result in the other receiving an exit
	% signal:
	%
	% (this is better to link actors to their own local time manager, in a
	% distributed way, rather than for example linking all actors to the often
	% remote load balancer; linking to a single manager moreover would not be
	% scalable in terms of actor population)
	%
	erlang:link( CallerPid ),

	LocalActors = ?getAttr(known_local_actors),

	case set_utils:member( CallerPid, LocalActors ) of

		true ->

			?warning_fmt( "Subscribing requested, whereas actor ~w "
						  "was already time subscribed.", [ CallerPid ] ),

			wooper:const_return_result( already_time_subscribed ) ;

		false ->

			%?debug_fmt( "Subscribing actor ~w.", [ CallerPid ] ),

			AddedState = setAttribute( State, known_local_actors,
							set_utils:add( CallerPid, LocalActors ) ),

			% If the simulation is already running, notifies directly the actor,
			% otherwise does nothing, as it will be done when starting:
			%
			% (the actor is currently blocked in its constructor, waiting for a
			% wooper result; therefore its simulationStarted/3 request could not
			% be called until it received the result of this subscribe method,
			% which would in turn create a deadlock)
			%
			% We now have to be able to:
			%
			% - subscribe an actor
			%
			% - notify it that the simulation is already running (if it is the
			% case)
			%
			% both as one atomic operation, as otherwise there could be a race
			% condition between two spawned actors, the first receiving the
			% start notification and its first top then sending a message to the
			% second actor, whereas this latter actor is not even started yet.

			StartInformation = case ?getAttr(started) of

				true ->
					% Already running, notifying this actor about that.
					%
					% No need to specifically schedule this actor on the fly for
					% the very next diasca (based on the 'onFirstDiasca/2' actor
					% oneway) as it is done by the load balancer.
					%
					% We specify here the current diasca, not the next one at
					% which this actor will be scheduled, as it may receive an
					% actor message in-between (typically onFirstDiasca/2) and
					% this would trigger a 'message in the past' error:
					%
					NewTimestamp = { ?getAttr(current_tick_offset),
									 ?getAttr(current_diasca) },

					{ ?getAttr(initial_tick), NewTimestamp };

				false ->
					not_started_yet

			end,

			% Note: the created actor is not the one which must ensure the next
			% diasca is actually scheduled, since its time manager may already
			% have reported its end of diasca; the only time manager that should
			% be used is the only one that for sure has not finished its diasca
			% yet, i.e. the one of the creating actor. As a consequence here we
			% do nothing to ensure that the next diasca is scheduled.

			% StartInformation allows to combine 'subscribe' and
			% 'simulationStarted' into one atomic operation:
			%
			wooper:return_state_result( AddedState,
				{ time_subscribed, ?getAttr(simulation_tick_duration),
				  StartInformation } )

	end.



% Requests the manager to unsubscribe the caller from the list of time
% listeners.
%
% Returns time_unsubscribed if the operation succeeded (subscribed and was not
% subscribed yet).
%
% Ex: MyTimeManager ! { unsubscribe, [], self() }
%
-spec unsubscribe( wooper:state() ) -> request_return( 'time_unsubscribed' ).
unsubscribe( State ) ->
	UpdatedState = actual_unsubscribing( ?getSender(), State ),
	wooper:return_state_result( UpdatedState, time_unsubscribed ).





% Section dedicated to the exchanges performed in the course of a diasca.



% Called by an actor managed by this time manager and having received an actor
% message during this diasca for the first time: this call tells this time
% manager to schedule (trigger) this actor on the specified next diasca, so that
% it can process its actor message(s).
%
% Note that specifying the triggered diasca is necessary, due to a possible race
% condition for a local time manager between this message and its 'begin diasca'
% message sent by its parent time manager.
%
-spec scheduleTrigger( wooper:state(), tick_offset(), diasca() ) ->
							request_return( 'trigger_planned' ).
scheduleTrigger( State, TriggerTickOffset, TriggerDiasca ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),
	CurrentDiasca = ?getAttr(current_diasca),

	ActorToSchedule = ?getSender(),

	?display_console( "scheduleTrigger: ~w adding actor to trigger ~w "
		"at diasca ~B of tick offset #~B while being "
		"at diasca ~B of tick offset #~B.",
		[ self(), ActorToSchedule, TriggerDiasca,
		  TriggerTickOffset, CurrentDiasca, CurrentTickOffset ] ),


	% Checkings:

	% Most of the triggers should come from the same tick and diasca. However an
	% early non-local actor may already be in the next diasca (before this
	% manager receives its 'begin diasca' notification), or even in any
	% arbitrary future tick (after a jump over idle ones). So we can only test:
	%
	RecordedState = case TriggerTickOffset of


		CurrentTickOffset ->

			% Usual case: same tick; either sent from the same diasca (most
			% frequent), or being diasca-early:
			%
			UsuallyExpectedTriggerDiasca = CurrentDiasca + 1,
			EarlyBirdTriggerDiasca = CurrentDiasca + 2,

			case TriggerDiasca of

				UsuallyExpectedTriggerDiasca ->

					% Most common case, sender and manager in synch:

					CurrentDiascaActors =
						?getAttr(actors_to_trigger_in_one_diasca),

					% Expected not already registered:
					UpdatedDiascaActors = set_utils:add_as_new(
							 ActorToSchedule, CurrentDiascaActors ),

					setAttribute( State, actors_to_trigger_in_one_diasca,
								  UpdatedDiascaActors );


				EarlyBirdTriggerDiasca ->

					% Early sender, compared to this manager that must be
					% lagging behind:

					FutureDiascaActors =
						?getAttr(actors_to_trigger_in_two_diascas),

					% Expected not already registered:
					UpdatedDiascaActors = set_utils:add_as_new( ActorToSchedule,
													  FutureDiascaActors ),

					% We now know what is the next timestamp (hopefully):
					NextTimestamp = { CurrentTickOffset,
									  EarlyBirdTriggerDiasca },

					setAttributes( State, [
					  { actors_to_trigger_in_two_diascas, UpdatedDiascaActors },
					  { next_timestamp, NextTimestamp } ] );

				_ ->
					throw( { invalid_trigger_diasca, TriggerDiasca,
						CurrentDiasca, CurrentTickOffset, ActorToSchedule } )

			end;


		OtherTickOffset when OtherTickOffset > CurrentTickOffset ->

			% If already in the future (after a jump), cannot have gone past
			% diasca 0, thus the message must be targeting diasca 1:
			1 = TriggerDiasca,

			FutureDiascaActors = ?getAttr(actors_to_trigger_in_two_diascas),

			% This is another case of early actor, with a 2-diasca offset:
			UpdatedDiascaActors = set_utils:add_as_new( ActorToSchedule,
														FutureDiascaActors ),

			% We now know what is the next timestamp (hopefully):
			NextTimestamp = { OtherTickOffset, 1 },

			setAttributes( State, [
					{ actors_to_trigger_in_two_diascas, UpdatedDiascaActors },
					{ next_timestamp, NextTimestamp } ] )

	end,


	% Note: the sender of the actor message is expected to have notified its own
	% time manager (the only one which by design cannot have finished its
	% diasca) that the next diasca should be scheduled.

	% The purpose of having a request here (rather than a oneway) is not to
	% return a specific result, but to ensure that the actor that received the
	% actor message is blocked, itself blocking the calling actor, so that no
	% race condition can happen:
	%
	wooper:return_state_result( RecordedState, trigger_planned ).




% Called by an actor managed by this time manager whenever it is nudged
% (asynchronously) *and* when the answer from that actor came after the
% time-out: in this case the hijacked WOOPER loop at the level of
% onSimulationStallDetected/1 has already given up waiting for this answer, and
% this method is called instead. As a result, this later call shall be ignored.
%
-spec notifyNudged( wooper:state(), actor_pid(), tick_offset(),
					[ actor_pid() ] ) -> const_oneway_return().
notifyNudged( State, _NudgedActorPid, _TickOffset, _WaitedAcks ) ->

	% Ignored, as came too late (was meant to be intercepted before):
	wooper:const_return().




% Section about the management of abnormal situations.


% Overriding the WOOPER default EXIT handler, not interested in 'normal' EXIT
% messages.
%
-spec onWOOPERExitReceived( wooper:state(), pid(),
					basic_utils:exit_reason() ) -> const_oneway_return().
onWOOPERExitReceived( State, _Pid, normal ) ->
	wooper:const_return();

onWOOPERExitReceived( State, Pid, ExitType ) ->

	?warning_fmt( "Time manager EXIT handler ignored signal '~p' from ~w.",
				  [ ExitType, Pid ] ),

	wooper:const_return().




% Section about simulation milestones.



% This oneway is triggered periodically, in order to be able to perform
% operations like house-keeping on a regular basis (wallclock-wise).
%
% The duration, in milliseconds, is measured from the simulator start-up.
%
% Defined in order to be enriched at will.
%
% Note: each time manager (root or not) will call this method on its own, but it
% will triggered by the root one, to avoid non-synchronised waitings).
%
-spec onWallclockMilestone( wooper:state(), milliseconds() ) -> oneway_return().
onWallclockMilestone( State, CurrentMillisecond ) ->

	% First notifies ASAP the direct child managers:
	MilestoneMessage = { onWallclockMilestone, CurrentMillisecond },

	basic_utils:send_to_pid_set( MilestoneMessage, ?getAttr(child_managers) ),

	% Only the root manager will display it:
	case is_root_manager( State ) of

		true ->
			?info_fmt( "Wall-clock milestone triggered after an elapsed "
				"duration of ~ts; current wall-clock time is ~ts.",
				[ time_utils:duration_to_string( CurrentMillisecond ),
				  time_utils:get_textual_timestamp() ] );

		false ->
			ok

	end,

	?display_console( "Wall-clock milestone triggered on ~p (~p) after an "
		"elapsed duration of ~ts; current wall-clock time is ~ts.",
		[ self(), node(), time_utils:duration_to_string( CurrentMillisecond ),
		  time_utils:get_textual_timestamp() ] ),

	class_PluginManager:notify( _Event=on_simulation_wallclock_milestone_met,
								_Parameters=CurrentMillisecond ),

	% Then propagates the milestone to the registered local processes:
	[ A ! MilestoneMessage || A <- ?getAttr(wallclock_milestone_listeners) ],

	CleanedState = perform_house_keeping( State ),

	wooper:return_state( CleanedState ).



% This oneway is triggered one simulation tick out of N, in order to be able to
% perform operations like house-keeping on a regular basis (virtual-time wise).
%
% Defined in order to be enriched at will.
%
% Note: each time manager (root or not) will call this method on its own, with
% any specific synchronisation (as soon as they have finished the milestone
% tick), unlike wallclock-milestones.
%
-spec onTickMilestone( wooper:state(), tick_offset() ) -> oneway_return().
onTickMilestone( State, TickOffset ) ->

	% Only the root manager will display it:
	case is_root_manager( State ) of

		true ->
			?info_fmt( "Simulation-time milestone triggered at tick offset #~B,"
				" while current wall-clock time is ~ts.",
				[ TickOffset, time_utils:get_textual_timestamp() ] );

		false ->
			ok

	end,

	class_PluginManager:notify( _Event=on_simulation_tick_milestone_met,
								_Parameters=TickOffset ),

	?display_console( "Simulation-time milestone triggered on ~p (~p) "
		"at tick offset #~B, while current wall-clock time is ~ts.",
		[ self(), node(), TickOffset, time_utils:get_textual_timestamp() ] ),

	% Propagates the milestone to relevant simulation agents:
	[ A ! { onTickMilestone, TickOffset }
	  || A <- ?getAttr(tick_milestone_listeners) ],

	CleanedState = perform_house_keeping( State ),

	wooper:return_state( CleanedState ).



% Oneway called by the watchdog whenever it deems that the simulation is
% stalled.
%
% Only called for the root time manager, the only one using a watchdog.
%
% The upward propagation of a simulation stall does not need to be specifically
% managed, as the parent will have to wait for its child managers anyway.
%
-spec onSimulationStallDetected( wooper:state() ) -> oneway_return().
onSimulationStallDetected( State ) ->

	WaitedChildManagers = ?getAttr(waited_child_managers),

	WaitedChildManagerCount = set_utils:size( WaitedChildManagers ),

	% See whether the cause is local (a blocked actor) and/or indirect
	% (a blocked child manager):
	%
	ChildManagerMessage = case WaitedChildManagerCount of

		0 ->
			"not waiting for any child time manager";

		ChildManagerCount when ChildManagerCount > ?too_many_entries ->
			text_utils:format( "still waiting for ~B child time managers",
							   [ ChildManagerCount ] );

		ChildManagerCount ->

			ManagerDescriptions = [
				text_utils:format( "~w (on node ~ts)", [ M, node( M ) ] )
					  || M <- set_utils:to_list( WaitedChildManagers ) ],

			text_utils:format(
				"still waiting for following ~B child time manager(s): ~ts",
				[ ChildManagerCount,
				  text_utils:join( _Sep=", ", ManagerDescriptions ) ] )

	end,

	SpontaneousActors = ?getAttr(waited_spontaneous_actors),
	SpontaneousActorCount = set_utils:size( SpontaneousActors ),
	SpontaneousMessage = case SpontaneousActorCount of

		0 ->
			"not waiting for any actor spontaneously scheduled";

		SpontaneousCount when SpontaneousCount > ?too_many_entries ->
			text_utils:format(
				"still waiting for ~B actors spontaneously scheduled",
				[ SpontaneousCount ] );

		SpontaneousCount ->
			text_utils:format( "still waiting for following ~B actors "
				"spontaneously scheduled: ~w",
				[ SpontaneousCount, set_utils:to_list( SpontaneousActors ) ] )

	end,

	TriggeredActors = ?getAttr(waited_triggered_actors),
	TriggeredActorCount = set_utils:size( TriggeredActors ),
	TriggeredMessage = case TriggeredActorCount of

		0 ->
			"not waiting for any triggered actor";

		TriggeredCount when TriggeredCount > ?too_many_entries ->
			text_utils:format( "still waiting for ~B triggered actors",
							   [ TriggeredCount ] );

		TriggeredCount ->
			text_utils:format( "still waiting for following ~B "
				"triggered actors: ~w",
				[ TriggeredCount, set_utils:to_list( TriggeredActors ) ] )

	end,


	CurrentTickOffset = ?getAttr(current_tick_offset),

	WaitedCount = ?getAttr(waited_count),

	% Even the watchdog can be waited for (yes, this happens):
	WatchdogMessage = case ?getAttr(watchdog_waited) of

		false ->
			"not waiting for watchdog";

		true ->
			"still waiting for watchdog"

	end,

	Message = case WaitedCount of

		undefined ->
			"Simulation currently unable to start, "
			"or unusually long to do so..." ;

		_Defined ->
			text_utils:format(
			  "Simulation currently stalled at tick offset #~B (being at "
			  "diasca ~p) for time manager ~w, still waiting for a total of "
			  "~B notification(s) of end of diasca: ~ts",
			  [ CurrentTickOffset, ?getAttr(current_diasca), self(),
				WaitedCount, text_utils:strings_to_string(
					[ ChildManagerMessage, SpontaneousMessage, TriggeredMessage,
					  WatchdogMessage ] ) ] )

	end,

	% Mostly a const oneway, state-wise:
	FinalState = case WaitedCount of

		undefined ->

			?warning( Message ),
			?display_console( "~n### " ++ Message ++ "~n", [] ),
			State;

		WaitedChildManagerCount ->
			% Only stalled because of children, stay rather mute:
			?warning( Message ),
			% Maybe to disable:
			?display_console( "~n### Overall non-local stall detected "
					"at tick offset #~B.", [ CurrentTickOffset ] ),
			State;

		_Other ->

			% We are locally stalled (and a root time manager):
			{ DiagState, Diag } = executeRequest( State, getProgressDiagnosis ),

			FormattedDiag = format_nested_diagnoses( Diag ),

			ActorExplanation = text_utils:format( text_utils:join( "~n",
				[ "~nThe diagnosis about the simulation stall is: "
				  | FormattedDiag ] ) ++ "~n", [] ),

			FullMessage = Message ++ ActorExplanation,

			?warning( FullMessage ),

			?display_console( "~n### " ++ FullMessage ++ "~n", [] ),
			DiagState

	end,

	wooper:return_state( FinalState ).



% Returns a four-element tuple made of:
%
% - the PID of this time manager (for its parent to discriminate between the
% answers of its children)
%
% - the name of this node (as an atom)
%
% - a diagnosis about the local actors that are waited (as a list of binaries),
% or, if none, the 'none_waited' atom
%
% - a list of nested similar recursively-determined progress diagnoses, for the
% (direct) child managers
%
% That way we go through all the scheduling hierarchy.
%
% To be called on any time manager (either root or child, at any depth).
%
-spec getProgressDiagnosis( wooper:state() ) -> const_request_return(
		{ time_manager_pid(), net_utils:atom_node_name(), diagnosis(),
		  [ diagnosis() ] } ).
getProgressDiagnosis( State ) ->

	% First, triggers a parallel recursive progress request:
	Children = ?getAttr(child_managers),

	basic_utils:send_to_pid_set( { getProgressDiagnosis, [], self() },
								 Children ),

	LocalDiag = get_local_diagnosis( State ),

	ChildDiags = wait_for_diagnoses( Children, _Diagnoses=[] ),

	Res = { self(), net_utils:localnode(), LocalDiag, ChildDiags },

	wooper:const_return_result( Res ).



% Declares that a data exchange service will be used, and provides the PID of
% its root data exchanger, so that synchronisation of data updates with regard
% to tick can be done.
%
% The root data-exchanger becomes then a simulation listener.
%
-spec declareDataExchanger( wooper:state(), data_exchanger_pid() ) ->
								oneway_return().
declareDataExchanger( State, RootDataExchangerPid ) ->

	% Checking:
	undefined = ?getAttr(root_data_exchanger_pid),

	Listeners = ?getAttr(simulation_listeners),

	wooper:return_state( setAttributes( State, [
			{ simulation_listeners, [ RootDataExchangerPid | Listeners ] },
			{ root_data_exchanger_pid, RootDataExchangerPid } ] ) ).



% Requests this (root) time manager to notify the caller (e.g. the root data
% exchanger) when this tick will be over, so that operations (e.g. commit
% propagation) can be done.
%
% Note: the next overall tick does not need to be scheduled, information will
% just be ready for any next tick to come.
%
% (request, for synchronisation purposes)
%
-spec requestInterDiascaNotification( wooper:state() ) ->
				request_return( 'interdiasca_tracked' ).
requestInterDiascaNotification( State ) ->

	% Checking:
	undefined = ?getAttr(parent_manager_pid),
	[] = ?getAttr(interdiasca_listeners),

	wooper:return_state_result(
		appendToAttribute( State, interdiasca_listeners, ?getSender() ),
		interdiasca_tracked ).




% Helper section.



% Tells whether this time manager is the root one.
-spec is_root_manager( wooper:state() ) -> boolean().
is_root_manager( State ) ->

	case ?getAttr(parent_manager_pid) of

		undefined ->
			true;

		_ ->
			false

	end.



% Displays all relevant timing information, when the simulation is over.
-spec display_timing_information( ustring(), wooper:state() ) -> void().
display_timing_information( Timings, State ) ->

	ElapsedTicks = get_current_tick( State ) - ?getAttr(initial_tick),

	SimDuration = convert_ticks_to_milliseconds( ElapsedTicks, State ),

	SimDurationString = time_utils:duration_to_string( SimDuration ),

	RealDuration = time_utils:get_precise_duration_since(
						 ?getAttr(initial_timestamp) ),

	RealDurationString = time_utils:duration_to_string( RealDuration ),

	io:format( "Simulation terminated successfully at ~ts, "
		"after a duration of ~ts in simulation time (~B ticks), "
		"computed during a wall-clock duration of ~ts.~n",
		[ Timings, SimDurationString, ElapsedTicks, RealDurationString ] ),

	case RealDuration of

		0 ->
			?display_console( "Simulation did not last long enough to "
				"define a clock factor.", [] );

		_ ->

			AccFactor = SimDuration / RealDuration,

			case SimDuration > RealDuration of

				true ->
					io:format( "Simulation ran faster than the clock, "
						"with an acceleration factor of x~.3f.~n~n",
						[ AccFactor ] );

				false ->
					io:format( "Simulation ran slower than the clock, with an "
						"acceleration factor of x~.3f.~n~n", [ AccFactor ] )

			end

	end.



% Displays all relevant timing information, when the simulation is over.
%
% (helper)
%
-spec display_concurrency_information( wooper:state() ) -> void().
display_concurrency_information( State ) ->

	case ?getAttr(diasca_count) of

		0 ->
			?display_console( "No diasca evaluated, no average concurrency "
							  "can be computed.", [] );

		DiascaCount ->

			ScheduleCount = ?getAttr(schedule_count),

			AverageConcurrency = ScheduleCount / DiascaCount,

			io:format( "In the course of this run, a total of "
				"~B diascas were evaluated, corresponding "
				"to a total of ~B scheduled instances. "
				"This corresponds to a potential average "
				"concurrency of ~.1f instances evaluated per "
				"diasca (not counting any engine-level activity).~n~n",
				[ DiascaCount, ScheduleCount, AverageConcurrency ] )

	end.



% Returns a diagnosis, as a list of binaries or as the 'none_waited' atom, about
% the local actors (if any) managed by this time manager.
%
-spec get_local_diagnosis( wooper:state() ) -> diagnosis().
get_local_diagnosis( State ) ->

	SpontaneousActors = ?getAttr(waited_spontaneous_actors),
	TriggeredActors = ?getAttr(waited_triggered_actors),

	% Avoid creating a huge WaitedActors list if not necessary:
	WaitedActorCount = set_utils:size( SpontaneousActors )
		+ set_utils:size( TriggeredActors ),

	% Never perform blocking operations with actors from the time manager,
	% otherwise deadlocks could occur (ex: with scheduleTrigger/3, which can
	% happen approximately at any time).
	%
	case WaitedActorCount of

		0 ->
			none_waited;

		L when L < 20 ->
			WaitedActors = set_utils:union( SpontaneousActors,
											TriggeredActors ),
			get_wait_explanation( WaitedActors, L, State );

		TooLong ->
			[ text_utils:string_to_binary( text_utils:format(
					"is still waiting for ~B actors", [ TooLong ] ) ) ]

	end.


% (helper)
wait_for_diagnoses( Children, Diagnoses ) ->

	case set_utils:is_empty( Children ) of

		true ->
			Diagnoses;

		false ->

			receive

				{ wooper_result,
				  DiagTuple={ ChildPid, _NodeName, _LocalDiag, _SubDiags } } ->

					case set_utils:member( ChildPid, Children ) of

						true ->
							NewChildren = set_utils:delete( ChildPid,
															Children ),
							wait_for_diagnoses( NewChildren,
												[ DiagTuple | Diagnoses ] );

						false ->
							throw( { unexpected_diagnosis, DiagTuple } )

					end

			end

	end.





% Sends back explanations about the local actors that are still waited.
%
% Returns a list of binaries, for a more detailed view.
%
% (helper)
%
get_wait_explanation( WaitedActors, Count, State ) ->

	% They will answer back with 'notifyNudged':
	basic_utils:send_to_pid_set( { nudge, self() }, WaitedActors ),

	% These actors being local, we already know they are managed by the local
	% instance tracker:
	LocalTrackerPid = ?getAttr(local_instance_tracker_pid),

	% Needed for non-first level waited actors (they may not be local):
	RootTrackerPid = ?getAttr(root_instance_tracker_pid),

	case Count of

		L when L > 20 ->
			text_utils:format( "is still waiting for ~B actors (overloaded?)",
							   [ L ] );

		_ ->

			Header = case Count of

				% Frequent case:
				1 ->
					"is still waiting for following actor:";

				Many when Many > 1 ->
					text_utils:format( "is still waiting for "
									   "following ~B actors:", [ Count ] )

			end,

			WaitedActorList = set_utils:to_list( WaitedActors ),

			[ text_utils:string_to_binary( Header ) |
			  prepare_wait_explanation( WaitedActorList, LocalTrackerPid,
					RootTrackerPid, ?getAttr(current_tick_offset), _Acc=[] ) ]

	end.



% (helper)
prepare_wait_explanation( _WaitedActors=[], _LocalTrackerPid, _RootTrackerPid,
						  _TickOffset, Acc ) ->
	Acc;

prepare_wait_explanation( [ ActorPid | T ], LocalTrackerPid, RootTrackerPid,
						  TickOffset, Acc ) ->

	% This is by design an actor local to this time manager:
	ActorNaming = get_best_naming_for( ActorPid, LocalTrackerPid, local ),

	ActorInfo = receive

		% The next 'notifyNudged' message was sent by an actor in answer to the
		% 'nudge' one sent in get_wait_explanation/3.
		%
		% We only select the one that what we target; other notifyNudged
		% messages that will not be popped at the end will result in a call to
		% the corresponding oneway, which will do nothing.
		%
		{ notifyNudged, [ ActorPid, TickOffset, WaitedActors ] } ->

			case length( WaitedActors ) of

				L when L > 5 ->
					text_utils:format( "~ts, which itself is waiting for "
									   "~B other actors", [ ActorNaming, L ] );

				0 ->
					text_utils:format(
					  "~ts, not waiting for anyone (still busy?)",
					  [ ActorNaming ] );

				_ ->

					% The next actors are not necessarily local.
					%
					% Here we could either send a request to the root tracker
					% (and let it ask to the right local tracker), or find
					% ActorNode = node( Pid ), and then make a local name
					% look-up on this node for the (local) tracker.
					%
					% We preferred here the first solution:
					%
					ActorNamings = [
						get_best_naming_for( Pid, RootTrackerPid, global )
									|| Pid <- WaitedActors ],

					ListedActors = text_utils:join( _Sep=", and for ",
													ActorNamings ),

					text_utils:format( "~ts, which itself is waiting for ~ts",
									   [ ActorNaming, ListedActors ] )

			end

	after 100 ->

			text_utils:format( "~ts did not answer on time (busy?)",
							   [ ActorNaming ] )

	end,

	ActorInfoBin = text_utils:string_to_binary( ActorInfo ),

	prepare_wait_explanation( T, LocalTrackerPid, RootTrackerPid, TickOffset,
							  [ ActorInfoBin | Acc ] ).



% Returns the best textual description about specified actor, supposing that
% TrackerPid is the PID of a tracker able to answer for this actor (directly or
% not).
%
% LookUpType is either 'local' (we know that the target instance tracker should
% be able to answer) or 'global' (it may have to perform a global look-up in its
% hierarchy - generally to be used on the root instance tracker).
%
-spec get_best_naming_for( actor_pid(), instance_tracker_pid(),
						   'local' | 'global' ) -> ustring().
get_best_naming_for( ActorPid, TrackerPid, LookUpType ) ->

	Request = case LookUpType of

		local ->
			getActorInformationLocal;

		global ->
			getActorInformationGlobal

	end,

	TrackerPid ! { Request, ActorPid, self() },

	receive

		{ wooper_result, { Infos, Node } }
		  when is_record( Infos, actor_info ) ->

			AAI = Infos#actor_info.aai,

			ActorName = case Infos#actor_info.name of

				undefined ->

					% Sends a oneway to this actor, which will update the root
					% instance tracker later.
					%
					% (note: we do not specify self() here, we want the instance
					% tracking service to be updated instead; and as we do not
					% know which tracker shall be targeted, we specify the root
					% one instead):
					%
					ActorPid ! { triggerNameNotification, TrackerPid },

					% For this time, fall-back to a basic information:
					text_utils:format( "actor whose AAI is ~B (PID: ~w)",
								   [ AAI, ActorPid ] );

				Name ->
					% Already available, use it:
					text_utils:format(
					  "actor named '~ts' whose AAI is ~B (PID: ~w)",
					  [ Name, AAI, ActorPid ] )

			end,

			% These information are known to exist:
			ActorName ++ text_utils:format( " of class ~ts on node ~ts",
										[ Infos#actor_info.classname, Node ] )

	end.




% Returns a textual representation of the nested diagnoses: a list of plain
% strings.
%
format_nested_diagnoses( Diag ) ->
	% One-element list, to start:
	format_nested_diagnoses( [ Diag ], _Acc=[], _Level=0 ).


format_nested_diagnoses( _Diag=[], Acc, _Level ) ->
	Acc;

format_nested_diagnoses( _Diag=[ {TMPid,Node,LocalDiags,ChildDiags} | T ],
						 Acc, Level ) ->

	TMText = get_prefix_for( Level ) ++
		text_utils:format( "Time manager on ~ts (~w):", [ Node, TMPid ] ),

	PrefixBullet = get_prefix_for( Level+1 ),

	LocalTextList = case LocalDiags of

		none_waited ->
			[ PrefixBullet ++ "has no waited local actor" ];

		[ Header | ActorList ] ->
			PrefixSecondBullet = get_prefix_for( Level+2 ),
			TranslatedBullets = [ PrefixSecondBullet
					  ++ text_utils:binary_to_string( B ) || B <- ActorList ],
					[ PrefixBullet ++ text_utils:binary_to_string( Header )
					  | TranslatedBullets ]

	end,

	StartTextList = [ TMText | LocalTextList ],
	ChildTextList = case ChildDiags of

		[] ->
			[ PrefixBullet ++ "has no child time manager" ];

		DiagList ->
			% Recurses (depth-first)
			[ PrefixBullet ++ "has for child managers: "
			  | format_nested_diagnoses( DiagList, Acc, Level+3 ) ]

	end,

	% Continues on this "top-level" list:
	format_nested_diagnoses( T,  Acc ++ StartTextList ++ ChildTextList, Level ).



% Returns a suitable indentation prefix for specific nesting level, as a plain
% string.
%
get_prefix_for( _Level=0 ) ->
	" - ";

get_prefix_for( _Level=1 ) ->
	"   * ";

get_prefix_for( _Level=2 ) ->
	"     + ";

get_prefix_for( _Level=3 ) ->
	"       # ";

get_prefix_for( _Level=4 ) ->
	"         ~~ ";

get_prefix_for( _Level=5 ) ->
	"           + ";

get_prefix_for( Level ) ->
	lists:flatten( lists:duplicate( Level*2, " " ) ) ++ " - ".




% Generic interface.


% Called by the load balancer to update the overall actor count.
-spec notifyOverallActorCount( wooper:state(), actor_count() ) ->
									oneway_return().
notifyOverallActorCount( State, NewValue ) ->
	wooper:return_state(
		setAttribute( State, overall_actor_count, NewValue ) ).




% Static methods:


% Returns a textual description of specified simulation settings record.
-spec settings_to_string( #simulation_settings{} ) ->
								static_return( ustring() ).
settings_to_string( #simulation_settings{
					   simulation_name=Name,
					   tick_duration=SubmittedTickDuration,
					   simulation_interactivity_mode=SimInteractivity,
					   evaluation_mode=EvaluationMode }  ) ->

	NameString = text_utils:format( "simulation name is '~ts'.", [ Name ] ),

	TickDuration = float( SubmittedTickDuration ),

	Frequency = 1 / TickDuration,

	DurationString = text_utils:format( "simulation tick duration is exactly "
		"~f (virtual) seconds, which corresponds to a fundamental frequency of "
		"approximately ~.3f Hz.", [ TickDuration, Frequency ] ),

	InteractivityString = text_utils:format( "simulation will run in ~ts mode.",
											 [ SimInteractivity ] ),

	EvaluationModeString = case EvaluationMode of

		fastest->
			"evaluation will be done in fastest mode, "
			"with no message reordering, and using default seed." ;

		reproducible ->
			"evaluation will be totally reproducible, using default seed." ;

		{ reproducible, Seed } ->
			text_utils:format( "evaluation will be totally reproducible, "
							   "using user-specified seed ~p.", [ Seed ] ) ;

		ergodic ->
			"evaluation will be done in ergodic mode."

	end,

	Finalstring = text_utils:strings_to_string( [ NameString, DurationString,
		InteractivityString, EvaluationModeString ] ),

	wooper:return_static( Finalstring ).



% Returns the atom corresponding to the name this time manager should be
% registered as.
%
% Note: executed on the caller node.
%
-spec get_registration_name() ->
						static_return( naming_utils:registration_name() ).
get_registration_name() ->

	% We used to prefer using unique names (even if they actually remained,
	% each, node-local).
	%
	% Ex: sim_diasca_time_manager_for_testSimulation@myhost.mydomain.org
	%list_to_atom( atom_to_list( ?time_manager_name ) ++ "_for_"
	%	++ atom_to_list( node() ) ).

	% Simpler and sufficient in a purely local context:
	wooper:return_static( ?time_manager_name ).



% Returns the PID of the current time manager if it exists, otherwise the
% 'time_manager_not_available' atom.
%
% Waits a bit before giving up: useful when client and manager processes are
% launched almost simultaneously.
%
-spec get_any_manager() ->
		static_return( time_manager_pid() | 'time_manager_not_available' ).
get_any_manager() ->

	% Waits gracefully for the time manager to exist:
	try naming_utils:wait_for_global_registration_of( get_registration_name() )
			of

		TimeManagerPid ->
			wooper:return_static( TimeManagerPid )

	catch

		_Exception ->
			wooper:return_static( time_manager_not_available )

	end.





% Section for helper functions (not methods).


% Checks that specified simulation duration is correct, and returns a
% standardized version of it.
%
check_tick_duration( SpecifiedSimulationDuration )
  when is_integer( SpecifiedSimulationDuration ) ->
	check_tick_duration( erlang:float( SpecifiedSimulationDuration ) );

check_tick_duration( D ) when is_float( D ) andalso D > 0 ->
	D;

check_tick_duration( D ) ->
	throw( { invalid_specified_tick_duration, D } ).




% Updates the recorded next action for this manager, depending on incoming new
% information from a child manager.
%
% Returns an updated state.
%
-spec update_next_action_with( next_manager_action(), wooper:state() ) ->
									wooper:state().
update_next_action_with( no_planned_action, State ) ->
	% No change here, already set to the same value:
	State;

update_next_action_with( new_diasca_needed, State ) ->
	% Nothing sooner can exist, overriding blindly:
	setAttribute( State, next_action, new_diasca_needed );

update_next_action_with( NextTickOffset, State ) ->

	SoonestDeadline = case ?getAttr(next_action) of

		new_diasca_needed ->
			new_diasca_needed;

		undefined ->
			NextTickOffset;

		FirstNextOffset when FirstNextOffset > NextTickOffset ->
			NextTickOffset;

		SmallerOffset ->
			SmallerOffset

	end,
	setAttribute( State, next_action, SoonestDeadline ).




% Internal timer functions, for simulation interactive mode.


% Ticks at a regular pace (without maintaining any particular tick count), for
% simulation interactive mode:
%
-spec timer_main_loop( time_manager_pid(), milliseconds() ) -> no_return().
timer_main_loop( TimeManagerPid, TimeOut ) ->

	receive

		delete ->
			?display_console( "Timer: timer_main_loop requested to stop.",
							  [] ),
			TimeManagerPid ! timer_stopped
			% Ended.

	% After following real milliseconds, sends the timer top, and recurses:
	after TimeOut ->

		%trace_utils:debug_fmt( "Sending timerTickFinished (after ~B ms)",
		%					   [ TimeOut ] ),

		% No diasca shall be considered here:
		TimeManagerPid ! timerTickFinished,

		timer_main_loop( TimeManagerPid, TimeOut )

	end.



% Manages the stopping of the timer.
%
% Returns an updated state.
%
-spec stop_timer( wooper:state() ) -> wooper:state().
stop_timer( State ) ->

	UpdatedState = case ?getAttr(simulation_interactivity_mode) of

		interactive ->

			% In simulation interactive mode, the timer has to be stopped:
			case ?getAttr(timer_pid) of

				undefined ->
					% Nothing to stop:
					State;

				TimerPid ->

					?debug( "Waiting for the simulation timer to stop." ),
					TimerPid ! delete,

					receive

						timer_stopped ->
							?debug( "Timer stopped." ),
							setAttribute( State, timer_pid, undefined )

					end

			end;


		batch ->
			% In batch mode, no timer to stop:
			%
			% (note that a self-sent timer_top message might be still sitting in
			% the TimeManager mail box)
			%
			State


	end,

	% Flushes any remaining timer-top oneway calls/messages (Y-combinator):
	F = fun( Fun ) ->
		receive

			timerTickFinished ->
				Fun( Fun )

		after 0 ->

			ok

		end

	end,

	F( F ),

	UpdatedState.



% Ensures that watchdog-originating end of tick/diasca notifications are sent
% regularly, otherwise requests the specified time manager to display some
% information explaining the possible causes of the simulation stall.
%
% Note: apparently, loosing network connectivity (ex: putting the local network
% interface down in the course of the simulation) does not lead to having a
% non-suspended watchdog ever kicking in (stale VM?).
%
% This would just result into a 'noconnection' exception, quite later.
%
-spec watchdog_main_loop( time_manager_pid(), milliseconds(),
		milliseconds(), milliseconds(), milliseconds(), milliseconds() ) ->
								no_return().
watchdog_main_loop( TimeManagerPid, DelayBeforeFirstStall,
					DelayBetweenNextStalls, DelayBeforeFailed,NextPeriod,
					AccumulatedDuration ) ->

	?display_console( "Watchdog ~w running (first stall delay: ~B ms, then "
	  "~B ms; shutdown delay: ~B ms, next period: ~B ms, accumulated: ~B ms).",
	  [ self(), DelayBeforeFirstStall, DelayBetweenNextStalls,
		DelayBeforeFailed, NextPeriod, AccumulatedDuration ] ),

	receive

		{ beginWatchdogTick, _NewTickOffset=0 } ->

			?display_console( "Watchdog received a notification of tick begin "
							  "for tick offset #0.", [] ),

			% Acknowledges it immediately:
			TimeManagerPid ! { notifySpontaneousWatchdogCompleted, 0 },

			% Tactic #1: we start with an initial additional margin (actual
			% total delay: twice the stall one), so that all the initial actors,
			% even if they were quite numerous, can nevertheless complete their
			% first diasca (to overcome the typical initial burst load) with not
			% too many stall notifications.
			%
			% Tactic #2: to detect problems earlier, we start with an half
			% duration.
			%
			% Tactif #3: to find a good tradeoff between quickness of problem
			% spotting and not too many stall messages, we start the very first
			% tick with no specific margin.
			%
			% (unit: milliseconds)
			%

			CreationDurationMarginTacticOne = - DelayBeforeFirstStall,
			%CreationDurationMarginTacticTwo = DelayBeforeStalled div 2,
			%CreationDurationMarginTacticThree = 0,

			% Resets accordingly the idle timer:
			watchdog_main_loop( TimeManagerPid, DelayBeforeFirstStall,
						DelayBetweenNextStalls, DelayBeforeFailed,
						_NextPeriod=DelayBeforeFirstStall,
						_AccumulatedDuration=CreationDurationMarginTacticOne );


		{ beginWatchdogTick, NewTickOffset } ->

			?display_console( "Watchdog received a notification of tick begin "
							  "for tick offset #~B.", [ NewTickOffset ] ),

			% Acknowledges it immediately:
			TimeManagerPid ! { notifySpontaneousWatchdogCompleted,
							   NewTickOffset },

			% Resets the idle timer:
			watchdog_main_loop( TimeManagerPid, DelayBeforeFirstStall,
				DelayBetweenNextStalls, DelayBeforeFailed,
				_NextPeriod=DelayBeforeFirstStall, _AccumulatedDuration=0 );


		{ beginWatchdogDiasca, [ TickOffset, NewDiasca ] } ->

			?display_console( "Watchdog received a notification of diasca "
				"begin for tick offset #~B diasca ~B.",
				[ TickOffset, NewDiasca ] ),

			% Acknowledges it immediately:
			TimeManagerPid ! { notifyTriggeredWatchdogCompleted,
							   [ TickOffset, NewDiasca ] },

			% Resets the idle timer:
			watchdog_main_loop( TimeManagerPid, DelayBeforeFirstStall,
				DelayBetweenNextStalls, DelayBeforeFailed,
				_NextPeriod=DelayBeforeFirstStall, _AccumulatedDuration=0 );


		% Useful, for example to let a serialisation occur:
		suspendWatchdog ->

			trace_utils:notice( "Watchdog suspended." ),

			?notify_debug_cat( "Watchdog suspended.",
							   ?trace_emitter_categorization ),

			receive

				resumeWatchdog ->

					trace_utils:notice( "Watchdog resumed." ),

					?notify_debug_cat( "Watchdog resumed.",
									   ?trace_emitter_categorization ),

					watchdog_main_loop( TimeManagerPid, DelayBeforeFirstStall,
						DelayBetweenNextStalls, DelayBeforeFailed,
						_NextPeriod=DelayBeforeFirstStall,
						_AccumulatedDuration=0 )

			end;


		timeManagerShutdown  ->
			?display_console( "Watchdog shut down.", [] ),
			?notify_debug_cat( "Watchdog removed (shutdown).",
							   ?trace_emitter_categorization );


		delete ->
			?display_console( "Watchdog deleted.", [] ),
			?notify_debug_cat( "Watchdog removed (deletion).",
							   ?trace_emitter_categorization );


		{ synchronous_delete, ListenerPid } ->
			?display_console( "Watchdog synchronously deleted.", [] ),
			?notify_debug_cat( "Watchdog removed (synchronous deletion).",
							   ?trace_emitter_categorization ),
			ListenerPid ! watchdog_deleted;


		Other ->
			?notify_warning_fmt_cat(
				"Watchdog received an unexpected message (~p), ignored.",
				[ Other ], ?trace_emitter_categorization ),

			watchdog_main_loop( TimeManagerPid, DelayBeforeFirstStall,
				DelayBetweenNextStalls, DelayBeforeFailed,
				_NextPeriod=DelayBeforeFirstStall, AccumulatedDuration )


	% After following real (actual) milliseconds:
	after NextPeriod ->

		case AccumulatedDuration of

			% Deemed to be an orphaned node (thus needing to halt automatically)
			% if more than DelayBeforeFailed milliseconds elapsed since last
			% known event:

			TooLong when TooLong > DelayBeforeFailed ->

				DelayString =
					time_utils:duration_to_string( DelayBeforeFailed ),

				?notify_emergency_fmt_cat(
				   "Warning: watchdog on node '~ts' detected a too long "
				   "inter-diasca duration (more than ~ts), this node is thus "
				   "deemed orphaned, shutting it down now at ~ts.",
				   [ net_utils:localnode(), DelayString,
					 time_utils:get_textual_timestamp() ],
				   ?trace_emitter_categorization ),

				basic_utils:stop_on_failure( 95 );


			_OtherDuration ->

				% Output disabled, as anyway the onSimulationStallDetected
				% oneway will display a message both on the console and in the
				% traces:
				%
				?display_console( " ### Watchdog detected a stalled simulation "
					"at ~ts, see simulation traces for diagnosis.",
					[ time_utils:get_textual_timestamp() ] ),

				TimeManagerPid ! onSimulationStallDetected,

				watchdog_main_loop( TimeManagerPid, DelayBeforeFirstStall,
					DelayBetweenNextStalls, DelayBeforeFailed,
					_NextPeriod=DelayBetweenNextStalls,
					AccumulatedDuration + NextPeriod )

		end

	end.



% Manages the stopping of the watchdog.
%
% Returns an updated state.
%
-spec stop_watchdog( wooper:state() ) -> wooper:state().
stop_watchdog( State ) ->

	case ?getAttr(watchdog_pid) of

		undefined ->
			State;

		WatchdogPid ->

			WatchdogPid ! { synchronous_delete, self() },

			% Otherwise the watchdog could send 'done' messages after the next
			% flushing:
			receive

				watchdog_deleted ->
					ok

			end,

			?debug_fmt( "Flushing then any notification(s) "
				"sent by the watchdog (~w) in-between.", [ WatchdogPid ] ),

			% Flushes any remaining messages (Y-combinator):
			F = fun( Fun ) ->

				receive

					{ notifySpontaneousWatchdogCompleted, _TickOffset } ->
						Fun( Fun );

					{ watchdogDiascaFinished, _TickOffset, _Diasca } ->
						Fun( Fun )

				after 0 ->

					ok

				end

			end,

			F( F ),

			setAttribute( State, watchdog_pid, undefined )

	end.



% The wallclock tracker allows to monitor the simulation progress over real
% time, and to trigger wallclock milestones.
%
% One such tracker is used for a whole simulation (ex: not one per node), and is
% connected to the root time manager.
%
% Period is in wall-clock milliseconds.
%
-spec wallclock_tracker_main_loop( time_manager_pid(),
			milliseconds(), milliseconds() ) -> void().
wallclock_tracker_main_loop( RootTimeManagerPid, Period, TotalDuration ) ->

	receive

		delete ->
			ok

	after Period ->

		NewTotalDuration = TotalDuration + Period,

		RootTimeManagerPid ! { onWallclockMilestone, NewTotalDuration },

		wallclock_tracker_main_loop( RootTimeManagerPid, Period,
									 NewTotalDuration )

	end.



% The time tracker allows to output on the console the current time schedule
% information regularly, no more than once per wallclock second.
%
% This prevents very fast simulations from being slowed down by the mere console
% output of tick progress (flow control).
%
% Note: the tick tracker output (i.e. mainly the tick table) can be displayed so
% that it respects the RST syntax (see 'Uncomment for RST output').
%
% Therefore a PDF file can be generated from it, for example thanks to the
% generate-pdf-from-rst.sh script.
%
-spec time_tracker_start( load_balancer_pid(), time_manager_pid() ) ->
								no_return().
time_tracker_start( LoadBalancerPid, RootTimeManagerPid ) ->

	% Interactive, user-targeted processes matter, otherwise many seconds are
	% missed as soon as the local computer becomes loaded:

	% Built to force an initial display:
	time_tracker_main_loop( _PreviousDisplayTime=undefined,
		_PreviousSimTimestamp=undefined, LoadBalancerPid,
		RootTimeManagerPid, _WaitTime=?nominal_wait_time ).



% Main loop of the time tracker, typically fed by progress messages from the
% root time manager, each time a tick or a diasca has been evaluated.
%
% Note: PreviousSimTimestamp is useful to report the current timestamp at which
% any diasca may durably remain.
%
-spec time_tracker_main_loop( maybe( time_utils:time() ), logical_timestamp(),
	load_balancer_pid(), time_manager_pid(), milliseconds() ) -> no_return().
time_tracker_main_loop( PreviousDisplayTime, PreviousSimTimestamp,
						LoadBalancerPid, RootTimeManagerPid, WaitTime ) ->

	%?display_console( "Time tracker main loop.~n", [] ),

	% (actually most probably useless)
	%erlang:process_flag( priority, _Level=high ),

	% Most of the time-tracking messages are dropped (roughly only one per
	% second is kept for displaying, should more of them be received; depending
	% on the simulation, on the contrary ticks or even diascas may last for very
	% long), but messages nevertheless arrive mostly already (expensively)
	% pre-formatted.
	%
	% This is not necessarily a waste of resources, as these formattings and
	% conversions are anyway needed by the traces at each tick and diasca, see
	% record_progress_message/3.
	%
	% The case where a different second is reported with the same simulation
	% timestamp should never happen (a timestamp is sent only once).

	receive


		{ delete, CallerPid } ->

			% Not recursing anymore:

			% Comment for RST output:
			display_top_row(),

			io:format( "~n" ),

			?display_console( "(tick tracker stopped)~n", [] ),
			CallerPid ! { stopped, self() };


		{ RecordTime, _NewTopInfo={ Timings, Counts } } ->

			% We shall display a normal progress line iff :
			%   PreviousDisplayTime is not defined
			%     or ( RecordTime > PreviousDisplayTime
			%                and PreviousDisplayTime != CurrentTime )
			%
			% So we display the first progress report we receive for a given
			% second.

			CurrentTime = time(),

			ShallDisplay = case PreviousDisplayTime of

				CurrentTime ->
					false;

				undefined ->
					true;

				_ ->
					time_utils:get_intertime_duration( PreviousDisplayTime,
													   RecordTime ) > 0

			end,

			% In all cases (display or not), we update the latest known
			% simulation timestamp:
			%
			{ NewDisplayTime, NewSimTimestamp } = case ShallDisplay of

				true ->
					DisplayedTimestamp = display_normal_progress_line( Timings,
							   Counts, LoadBalancerPid, RootTimeManagerPid ),
					{ CurrentTime, DisplayedTimestamp };

				false ->
					{ _SimDateString, _SimTimeString, CurrentTickOffset,
					  CurrentDiasca, _RealDateString, _RealTimeString } =
															  Timings,

					NonDisplayedTimestamp = { CurrentTickOffset,
											  CurrentDiasca },

					{ PreviousDisplayTime, NonDisplayedTimestamp }

			end,

			time_tracker_main_loop( NewDisplayTime, NewSimTimestamp,
							LoadBalancerPid, RootTimeManagerPid,
							_WaitTime=?nominal_wait_time );


		Unexpected ->
			throw( { unexpected_time_tracker_message, Unexpected } )


	% A given tick/diasca may last for a lot more than a few seconds, we
	% nevertheless want to regularly give of sign of liveliness to the user:
	%
	after WaitTime ->

			%?display_console( "after triggered, at ~ts from ~B~n",
			% [ time_utils:get_textual_timestamp(), PreviousDisplayTime ] ),

			%io:format( "WaitTime timeout = ~p~n", [ WaitTime ] ),

			%NewDisplayTime = time(),

			display_diasca_in_progress( PreviousSimTimestamp ),

			time_tracker_main_loop( _PreviousDisplayTime=undefined,
				PreviousSimTimestamp, LoadBalancerPid, RootTimeManagerPid,
				_WaitTime=?same_diasca_wait_time )

	end.




% Displays the bar, above and below the per-tick lines, so that the whole looks
% like an array.
%
display_top_row() ->
	io:format( "+----------------------+----------------+--------+"
			   "----------------------+--------------+--------------+"
			   "----------------+~n" ).


% Displays a double bar, so that the whole looks like the header of an array.
%
display_top_row_heavy() ->
	io:format( "+======================+================+========+"
			   "======================+==============+==============+"
			   "================+~n" ).



% Displays the final progress line on the console.
%
% We defer as much as possible the string construction as very few messages are
% actually displayed, hence needed.
%
-spec display_console_line( timing_info(), count_info() ) ->
									logical_timestamp().
display_console_line(
  _Timings={ SimDateString, SimTimeString,
			 CurrentTickOffset, CurrentDiasca, RealDateString, RealTimeString },
  _Counts={ TotalActorCount, TotalScheduleCount, TotalProcessCount } ) ->

	% Ex: '|S:       (not started)|T:             0|D:     0|
	%  R:   3/9/2012 17:47:39|A:           0|S:           0|P:           34 |

	% Room for 999999999999 actors, i.e. 10^12 actors!

	TimestampString = text_utils:format(
		"|S: ~10.s ~8.s|T: ~13.B|D: ~5.B|R: ~10.s ~8.s|",
		[ SimDateString, SimTimeString, CurrentTickOffset, CurrentDiasca,
		  RealDateString, RealTimeString ] ),

	% We have to compensate for the load-balancer (if any), which is an actor
	% too:
	%
	ActualActorCount = case TotalActorCount of

		0 ->
			0;

		StrictlyPositive ->
			StrictlyPositive - 1

	end,

	% Total line width: 120 characters.

	io:format( "~tsA: ~11.B|S: ~11.B|P: ~12.B |~n",
			   [ TimestampString, ActualActorCount, TotalScheduleCount,
				 TotalProcessCount ] ),

	% Uncomment for RST output: display_top_row().

	{ CurrentTickOffset, CurrentDiasca }.




% Displays a normal (non-stuck at a diasca) progress line on the console.
display_normal_progress_line( Timings, Counts, LoadBalancerPid,
							  RootTimeManagerPid ) ->

	% We take advantage of this per-wallclock second scheduling to program a
	% lazy, asynchronous, informational only, update of the root time manager by
	% the load balancer (information will ultimately flow that way, not the
	% other way round, due to a callback):
	%
	LoadBalancerPid ! { getOverallInstanceCount, RootTimeManagerPid },

	display_console_line( Timings, Counts ).



% Displays that the current diasca is still in progress.
display_diasca_in_progress( SimTimestamp ) ->

	BaseText = text_utils:format( "diasca ~p still in progress at ~ts",
					[ SimTimestamp, time_utils:get_textual_timestamp() ] ),

	BaseTextWidth = length( BaseText ),

	% Full width less two borders:
	DoubleSpaceWidth = 120 - 2 - BaseTextWidth,

	SpaceWidth = DoubleSpaceWidth div 2,

	EndingString = case DoubleSpaceWidth rem 2 of

		0 ->
			"|";

		1 ->
			" |"

	end,

	FullText = "|" ++ text_utils:pad_string_right( "--- ", SpaceWidth )
		++ BaseText ++ text_utils:pad_string_left( " ---", SpaceWidth )
		++ EndingString,

	% Not basic_utils:display/1, to avoid any possible time-out:
	io:format( "~ts~n", [ FullText ] ).



% Manages the stopping of the wall-clock tracker.
%
% Returns an updated state.
%
% (helper)
%
-spec stop_wallclock_tracker( wooper:state() ) -> wooper:state().
stop_wallclock_tracker( State ) ->

	% Only the root manager has such a tracker:
	case ?getAttr(wallclock_tracker_pid) of

		undefined ->
			ok;

		Pid ->
			Pid ! delete

	end,

	% Not synchronous.

	setAttribute( State, wallclock_tracker_pid, undefined ).



% Manages the stopping of the tick tracker.
%
% Returns an updated state.
%
% (helper)
%
-spec stop_time_tracker( wooper:state() ) -> wooper:state().
stop_time_tracker( State ) ->

	case ?getAttr(time_tracker_pid) of

		undefined ->
			State;

		TrackerPid ->

			% Blocking to avoid having its last message be displayed after the
			% "simulation stopped" one:
			%
			TrackerPid ! { delete, self() },

			receive

				{ stopped, TrackerPid } ->
					ok

			end,

			setAttribute( State, time_tracker_pid, undefined )

	end.



% Returns the current (numerical) simulation tick.
%
% Note: the time manager must be started.
%
-spec get_current_tick_offset( wooper:state() ) -> maybe( tick_offset() ).
get_current_tick_offset( State ) ->
	?getAttr(current_tick_offset).



% Returns the current (numerical) simulation tick.
%
% Note: the time manager must be started.
%
-spec get_current_tick( wooper:state() ) -> tick().
get_current_tick( State ) ->
	%?display_console( "get_current_tick called." ),
	?getAttr(initial_tick) + ?getAttr(current_tick_offset).



% Returns the full date and time of the simulation, i.e.:
% {{SimYear,SimMonth,SimDay}, {SimHour,SimMinute,SimSecond,SimMicrosecond}}.
%
% (helper)
%
-spec get_simulation_time_and_date( wooper:state() ) -> timestamp().
get_simulation_time_and_date( State ) ->

	CurrentTick = get_current_tick( State ),

	RoundedSeconds = convert_ticks_to_rounded_seconds( CurrentTick, State ),

	calendar:gregorian_seconds_to_datetime( RoundedSeconds ).



% Converts specified timestamp (date and time, expressed in virtual time) into
% the corresponding absolute tick.
%
% (helper)
%
-spec timestamp_to_ticks( timestamp(), wooper:state() ) -> tick().
timestamp_to_ticks( Timestamp, State ) ->

	% Seconds since year #0:
	Secs = calendar:datetime_to_gregorian_seconds( Timestamp ),

	Ticks = convert_seconds_to_ticks( Secs, State ),

	%?debug_fmt( "Timestamp ~p converted to ~Bs, thus tick ~B.",
	%			[ Timestamp, Secs, Ticks ] ),

	Ticks.




% Converts specified absolute tick into the corresponding timestamp (date and
% time, expressed in virtual time).
%
% (helper)
%
-spec ticks_to_timestamp( tick(), wooper:state() ) -> timestamp().
ticks_to_timestamp( Tick, State ) ->

	% Seconds since year #0 (rounding necessary):
	Secs = convert_ticks_to_rounded_seconds( Tick, State ),

	Timestamp = calendar:gregorian_seconds_to_datetime( Secs ),

	%?debug_fmt( "Tick ~B converted to ~Bs, thus timestamp ~p.",
	%			[ Tick, Secs, Timestamp ] ),

	Timestamp.



% Returns a textual description of the real and simulated time.
-spec get_textual_timings( wooper:state() ) -> ustring().
get_textual_timings( State ) ->

	CurrentTick = get_current_tick( State ),

	{ {SimYear,SimMonth,SimDay}, {SimHour,SimMinute,SimSecond} } =
		calendar:gregorian_seconds_to_datetime(
			convert_ticks_to_rounded_seconds( CurrentTick, State ) ),

	{ {RealYear,RealMonth,RealDay}, {RealHour,RealMinute,RealSecond} } =
		{ date(), time() },

	text_utils:format( "simulation time: "
		"~B/~B/~B ~B:~2..0B:~2..0B (tick ~B), "
		"real time: ~B/~B/~B ~B:~2..0B:~2..0B",
		[ SimDay, SimMonth, SimYear, SimHour, SimMinute, SimSecond, CurrentTick,
		  RealDay, RealMonth, RealYear, RealHour, RealMinute, RealSecond ] ).




% Returns {DetailedDescription, CompactDescription, Second}, for the textual
% description of the specified timing information.
%
% DetailedDescription and CompactDescription are respectively a detailed and
% compact textual description of the real and simulated time, for example to be
% used respectively in traces and on the console (through the tick tracker).
%
-spec get_full_textual_timings( tick_offset(), diasca(), wooper:state() ) ->
				{ ustring(), timing_info(), time_utils:time() }.
get_full_textual_timings( TickOffset, Diasca, State ) ->

	Tick = ?getAttr(initial_tick) + TickOffset,

	% gregorian_seconds_to_datetime/1 is probably quite expensive:
	{ {SimYear,SimMonth,SimDay}, {SimHour,SimMinute,SimSecond} } =
		calendar:gregorian_seconds_to_datetime(
			convert_ticks_to_rounded_seconds( Tick, State ) ),

	{ RealDateString, RealTimeString, RealTime } = format_real_time_date(),

	Detailed = text_utils:format( "simulation time: "
		"~B/~B/~B ~B:~2..0B:~2..0B (tick ~B), real time: ~ts ~ts",
		[ SimDay, SimMonth, SimYear, SimHour, SimMinute, SimSecond, Tick,
		  RealDateString, RealTimeString ] ),

	SimDateString = text_utils:format( "~B/~B/~B",
									   [ SimDay, SimMonth, SimYear ] ),

	SimTimeString = text_utils:format( "~B:~2..0B:~2..0B",
									   [ SimHour, SimMinute, SimSecond ] ),


	% Invests as little as possible in string formatting, as anyway only one
	% compact message ("exactly") will be displayed per second (on the console):
	%
	Compact = { SimDateString, SimTimeString, TickOffset, Diasca,
				RealDateString, RealTimeString },

	{ Detailed, Compact, RealTime }.



% Returns a triplet describing the current wall-clock time.
-spec format_real_time_date() -> { ustring(), ustring(), time_utils:time() }.
format_real_time_date() ->

	{ RealYear, RealMonth, RealDay } = date(),
	RealDateString = text_utils:format( "~B/~B/~B",
										[ RealDay, RealMonth, RealYear ] ),

	{ RealHour, RealMinute, RealSecond } = Time = time(),
	RealTimeString = text_utils:format( "~B:~2..0B:~2..0B",
										[ RealHour, RealMinute, RealSecond ] ),

	{ RealDateString, RealTimeString, Time }.



% Converts the specified number of (floating-point or integer) seconds into an
% integer (rounded) number of ticks.
%
% (helper)
%
-spec convert_seconds_to_ticks( any_seconds(), wooper:state() ) ->
										tick_offset().
convert_seconds_to_ticks( Seconds, State ) ->
	% Less than 2% of relative error tolerated by default:
	convert_seconds_to_ticks( Seconds, _DefaultMaxRelativeError=0.02, State ).



% Converts the specified number of (floating-point or integer) seconds into an
% integer (rounded) number of ticks, checking that any rounding error stays
% within specified maximum relative error.
%
% For example, to limit the relative error to 5%, use MaxRelativeError=0.05.
%
% (helper)
%
-spec convert_seconds_to_ticks( any_seconds(), math_utils:percent(),
								wooper:state() ) -> tick_offset().
convert_seconds_to_ticks( Seconds, MaxRelativeError, State )
  when Seconds >= 0 ->

	TickDuration = ?getAttr(simulation_tick_duration),

	TickCount = erlang:round( Seconds / TickDuration ),

	% Converts back to measure error:
	CorrespondingSeconds = TickCount * TickDuration,

	case math_utils:are_relatively_close( Seconds, CorrespondingSeconds,
										  MaxRelativeError ) of

		true ->
			TickCount;

		false ->
			throw( { too_inaccurate_duration_conversion, TickCount, Seconds,
					 CorrespondingSeconds, TickDuration } )

	end.



% Converts the specified tick count into a (floating-point) number of virtual
% seconds.
%
% (helper)
%
-spec convert_ticks_to_rounded_seconds( tick_offset(), wooper:state() ) ->
											seconds().
convert_ticks_to_rounded_seconds( Ticks, State ) ->
	round( convert_ticks_to_seconds( Ticks, State ) ).


% Converts the specified tick count into a fractional (floating-point) number of
% seconds.
%
% (helper)
%
-spec convert_ticks_to_seconds( tick_offset(), wooper:state() ) ->
									virtual_seconds().
convert_ticks_to_seconds( Ticks, State ) ->
	Ticks * ?getAttr(simulation_tick_duration).



% Converts the specified tick count into an integer (rounded) number of
% milliseconds.
%
% Note: currently the most precise evaluation of simulated durations.
%
% (helper)
%
-spec convert_ticks_to_milliseconds( tick(), wooper:state() ) ->
										milliseconds().
convert_ticks_to_milliseconds( Ticks, State ) ->
	% We want (integer) milliseconds:
	erlang:round( Ticks * ?getAttr(simulation_tick_duration) * 1000 ).




% Init helper function, used by all start methods.
%
% Note: only the root time manager is started, thus we know that this time
% manager is not a child one.
%
% Returns an updated state.
%
-spec init( wooper:state() ) -> wooper:state().
init( State ) ->

	% Only first place when we are sure that the initialisation is over:
	class_PluginManager:notify( on_case_initialisation_stop ),

	% Initial checkings:
	case ?getAttr(started) of

		false ->
			ok;

		true ->
			throw( simulation_already_started )

	end,

	wooper:check_all_undefined( [ wallclock_tracker_pid, time_tracker_pid,
								  watchdog_pid ], State ),

	% We must be the root time manager:
	undefined = ?getAttr(parent_manager_pid),

	% Let's take care of the watchdog now:

	WatchdogState = launch_watchdog( State ),

	InitialTimestamp = { _FirstTickOffset=0, _FirstDiasca=0 },

	{ StartedState, { time_manager_started, _Self } } = executeRequest(
				WatchdogState, simulationStarted,
				[ ?getAttr(initial_tick), InitialTimestamp ] ),


	% stop_tick_offset left as is, might have been updated beforehand.
	PostStartedState = setAttributes( StartedState, [
		{ started, true },
		{ initial_timestamp, time_utils:get_precise_timestamp() },

		% Useless: { next_action, no_planned_action },

		% No '{ overall_actor_count, 0 }', as actors may be created before
		% simulation start.

		{ scheduled_tracking, 0 },
		{ diasca_count, 0 },
		{ schedule_count, 0 },

		% So that the check at the first scheduled tick/diasca does not fail in
		% batch mode, and since, in simulation interactive mode, the timer main
		% loop may send a timerTickFinished message even before the first
		% beginTimeManagerTick is triggered (whereas it is the one setting the
		% waited count at the first place):
		%
		{ waited_count, 0 } ] ),


	StopString = case ?getAttr(stop_tick_offset) of

		undefined ->
			"Warning: no specific simulation duration (stop tick) "
			"was specified.";

		StopOffset when StopOffset =< 0 ->
			throw( { unreachable_stop_tick_offset, StopOffset } );

		StopOffset ->

			AbsoluteStop = ?getAttr(initial_tick) + StopOffset,

			Duration = convert_ticks_to_seconds( StopOffset, PostStartedState ),

			% Expecting milliseconds:
			TextDuration = time_utils:duration_to_string(
							 round( Duration * 1000 ) ),

			text_utils:format( "Simulation will stop no later than tick ~B "
				"(i.e. after ~B ticks, which is a duration of ~ts in virtual "
				"time).", [ AbsoluteStop, StopOffset, TextDuration ] )

	end,

	TickDuration = ?getAttr(simulation_tick_duration),

	% duration_to_string/1 expects milliseconds:
	TickDurationString = time_utils:duration_to_string(
						   erlang:round( TickDuration * 1000 ) ),

	Frequency = 1 / TickDuration,

	% We must record current_tick_offset=InitialTick-1 for the engine, but we
	% want the start outputs to show InitialTick instead:
	%
	Timings = get_textual_timings(
				addToAttribute( PostStartedState, current_tick_offset, 1 ) ),

	?notice_fmt( "Simulation started at ~ts with a simulation frequency "
		"of approximately ~fHz (period of exactly ~ts). ~ts",
		[ Timings, Frequency, TickDurationString, StopString ] ),


	% The load balancer is indeed a technical component, but it is an actor,
	% thus counted as such.
	%
	?display_console( "Simulation started at ~ts with a simulation frequency "
		"of approximately ~fHz (period of exactly ~ts). ~ts~n~n"
		"Meaning of the console tracker columns:~n"
		" - S: overall [S]imulation time (full time and date)~n"
		" - T: overall simulation [T]ick (virtual time)~n"
		" - D: overall simulation [D]iasca "
		"(in-tick causality progress)~n"
		" - R: [R]eal (wall-clock) time~n"
		" - A: total (distributed) [A]ctor count~n"
		" - S: actor [S]chedulings on last diasca "
		"(spontaneous/triggered behaviours)~n"
		" - P: total (distributed) [P]rocess count~n",
		[ Timings, Frequency, TickDurationString, StopString ] ),

	display_top_row(),

	io:format( "| Simulation Time      | Tick Offset    | Diasca"
			   " | Real Time           "
			   " | Actor Count  | Schedulings  | Process Count  |~n" ),

	display_top_row_heavy(),

	% Launching the wallclock and tick trackers:

	WallclockTrackerState = launch_wallclock_tracker( PostStartedState ),

	TimeTrackerState = launch_time_tracker( WallclockTrackerState ),

	?debug( "Notifying time listeners." ),

	class_PluginManager:notify( on_simulation_start ),

	[ L ! simulation_started || L <- ?getAttr(simulation_listeners) ],

	case ?getAttr(simulation_interactivity_mode) of

		interactive ->

			case ?getAttr(timer_pid) of

				undefined ->
					launch_timer( TimeTrackerState );

				_TimerPid ->

					?warning( "Start request ignored: "
							  "simulation clock already running." ),
					% State, not TimeTrackerState:
					State

			end;


		batch ->

			?notify_by_speak( "Starting simulation clock in batch mode." ),

			?notify_mute_fmt( "Starting simulation clock in batch "
				"(non-interactive mode) at ~ts with a simulation frequency "
				"of ~fHz (period of exactly ~ts).",
				[ get_textual_timings( TimeTrackerState ), Frequency,
				  TickDurationString ] ),

			% Sends the very first top:
			self() ! { beginTimeManagerTick, _CurrentTickOffset=0 },

			TimeTrackerState

	end.



% Notifies this time manager that the simulation started.
%
% It will in turn recurse in its own child managers, if any.
%
% This message must be acknowledged (it is a request), to avoid a race
% condition: otherwise if two actors were spawned at the same tick/diasca or
% both before the overall simulation start, then the first could receive its
% first top and send an actor message to the second even before it was itself
% started, thus synchronized (thus not having a current tick/diasca yet).
%
% We specify a logical timestamp, whereas currently it can be only {T=0,D=0} (as
% simulation just starts), but later we could imagine that a time manager could
% join the simulation dynamically (i.e. whereas it is already running).
%
-spec simulationStarted( wooper:state(), tick(), logical_timestamp() ) ->
			request_return( { 'time_manager_started', time_manager_pid() } ).
simulationStarted( State, SimulationInitialTick,
				   InitialTimestamp={ InitialTick, InitialDiasca } ) ->

	?notice_fmt( "Time manager to start simulation at tick offset #~B "
		"diasca ~B, defined relatively to its initial tick, which was ~B.",
		[ InitialTick, InitialDiasca, SimulationInitialTick ] ),

	% Will be sent to the child managers then to all local actors:
	% (expressed as a oneway call)
	%
	StartMessage = { simulationStarted,
					[ SimulationInitialTick, InitialTimestamp ], self() },

	?debug( "Notifying all child managers that the simulation starts." ),

	ChildManagers = ?getAttr(child_managers),

	basic_utils:send_to_pid_set( StartMessage, ChildManagers ),

	InitialActors = ?getAttr(known_local_actors),

	?debug_fmt( "Notifying, based on chunks, all ~B initial actors "
		"that the simulation starts.", [ set_utils:size( InitialActors ) ] ),

	% We shall break larger lists into chunks, otherwise the select receive used
	% for the waiting of acknowlegments will be awfully long:
	%
	ActorCount = start_actors_by_chunks( InitialActors, StartMessage ),

	?debug_fmt( "All ~B actors start acknowledgements received.",
				[ ActorCount ] ),

	% To avoid a potential race condition:

	case set_utils:is_empty( ChildManagers ) of

		true ->
			?debug( "No start acknowledgement from child manager "
					"to be waited." ),
			ok;

		false ->
			?debug_fmt( "Waiting for the start acknowledgements of the ~B child"
						" managers.", [ set_utils:size( ChildManagers ) ] ),

			% We could imagine a large number of managers as well:
			wait_for_start_acknowlegments( time_manager_started, ChildManagers )

	end,

	?debug( "At this level the simulation is now ready to start." ),

	EmptySet = set_utils:new(),

	% Should already be set here: known_local_actors, spontaneous_agenda:
	StartedState = setAttributes( State, [
		{ started, true },
		{ actors_to_trigger_in_one_diasca, EmptySet },
		{ actors_to_trigger_in_two_diascas, EmptySet },
		{ terminating_actors, [] },
		{ terminated_actors, [] },
		{ waited_child_managers, EmptySet },
		{ waited_spontaneous_actors, EmptySet },
		{ waited_triggered_actors, EmptySet },
		{ initial_tick, SimulationInitialTick },

		% Next action is to begin a new tick at InitialTick, this means the
		% current tick is the one just before:

		% (otherwise checkings will fail):
		{ current_tick_offset, InitialTick - 1 },

		% We also need a valid diasca:
		{ current_diasca, InitialDiasca },

		{ scheduled_tracking, 0 },
		{ diasca_count, 0 },
		{ schedule_count, 0 } ] ),

	wooper:return_state_result( StartedState,
								{ time_manager_started, self() } ).



% Starts (synchronously) actors, based on chunks if they are to numerous.
%
% InitialActors is a set we will iterate through, rather than transforming it
% into a plain list.
%
% We build also a waited set, as we need random access in it, since answers will
% come unordered.
%
% Supposedly maintaining a separate count avoids many size/1 (potentiall
% expensive) computations.
%
% Returns the count of started actors.
%
-spec start_actors_by_chunks( set_utils:set( actor_pid() ),
							  basic_utils:message() ) -> actor_count().
start_actors_by_chunks( InitialActors, StartMessage ) ->

	ExpectedCount = set_utils:size( InitialActors ),

	?display_console( "Starting ~B actors by chunks of ~B.",
					  [ ExpectedCount, ?chunk_size ] ),

	Iterator = set_utils:iterator( InitialActors ),

	WaitedSet = set_utils:new(),

	% Yes, we *must* use next/1 even to get the first element (which is thus not
	% ditched here) of the set:
	%
	FinalCount = start_actors_by_chunks( set_utils:next( Iterator ), WaitedSet,
						_WaitedCount=0, StartMessage, _TotalCount=0 ),

	% Checking:
	ExpectedCount = FinalCount,

	FinalCount.



% (helper)
-spec start_actors_by_chunks( set_utils:iterator(),
		set_utils:set( actor_pid() ), actor_count(),
		basic_utils:message(), actor_count() ) -> actor_count().
% Finished, end of actor list reached:
start_actors_by_chunks( _InitialActorsIterator=none, WaitedSet, _WaitedCount,
						_StartMessage, TotalCount ) ->
	wait_for_start_acknowlegments( actor_started, WaitedSet ),
	TotalCount ;


% End of chunk reached (not wanting to wait for too many actors at once):
start_actors_by_chunks( InitialActorsIterator, WaitedSet,
		_MaxWaitedCount=?chunk_size, StartMessage, TotalCount ) ->

	wait_for_start_acknowlegments( actor_started, WaitedSet ),

	NewWaitedSet = set_utils:new(),

	% Then continue with the next chunk:
	start_actors_by_chunks( InitialActorsIterator, NewWaitedSet,
							_NewWaitedCount=0, StartMessage, TotalCount ) ;


% Still in chunk:
start_actors_by_chunks( _InitialActorsIterator={ ActorPid, NewIterator },
						WaitedSet, WaitedCount, StartMessage, TotalCount ) ->

	ActorPid ! StartMessage,

	NewWaitedSet = set_utils:add( ActorPid, WaitedSet ),

	start_actors_by_chunks( set_utils:next( NewIterator ), NewWaitedSet,
							WaitedCount+1, StartMessage, TotalCount+1 ).




% This can be used for actors and for child time managers.
%
% WaitedType is either actor_started or time_manager_started.
%
wait_for_start_acknowlegments( WaitedType, WaitedList ) ->

	?display_console( "Waiting for start acknowlegments for ~B processes of "
		"type ~p.", [ set_utils:size( WaitedList ), WaitedType ] ),

	case set_utils:is_empty( WaitedList ) of

		true ->
			ok;

		false ->
			receive

				{ wooper_result, { WaitedType, Pid } } ->

					DelList = set_utils:delete_existing( Pid, WaitedList ),

					wait_for_start_acknowlegments( WaitedType, DelList )

			end

	end.



% Returns the minimum of the two specified timestamps.
min_timestamp( _First=undefined, Second ) ->
	Second;

min_timestamp( First, _Second=undefined ) ->
	First;

min_timestamp( First={ F1, _ }, _Second={ S1, _} ) when F1 < S1 ->
	First;

min_timestamp( First={ F1, F2 }, _Second={ _S1=F1, S2 } ) when F2 < S2 ->
	First;

min_timestamp( _First, Second ) ->
	Second.



% Returns the maximum of the two specified timestamps.
max_timestamp( _First=undefined, Second ) ->
	Second;

max_timestamp( First, _Second=undefined ) ->
	First;

max_timestamp( _First={ F1, _ }, Second={ S1, _} ) when F1 < S1 ->
	Second;

max_timestamp( _First={ F1, F2 }, Second={ _S1=F1, S2 } ) when F2 < S2 ->
	Second;

max_timestamp( First, _Second ) ->
	First.



% Stops all child time managers.
%
% Tries to be parallel.
%
stop_child_managers( State ) ->

	ChildManagers = ?getAttr(child_managers),

	basic_utils:send_to_pid_set( { stop, [], self() }, ChildManagers ),

	wait_stop_of_child_managers( ChildManagers, State ).



% Waits for all child managers to stop.
wait_stop_of_child_managers( ChildManagers, State ) ->

	case set_utils:is_empty( ChildManagers ) of

		true ->
			ok;

		false ->

			WaitDuration = get_maximum_teardown_duration(),

			receive

				{ wooper_result, { stopped, ManagerPid } } ->

					RemainingManagers =
						set_utils:delete_existing( ManagerPid, ChildManagers ),

					wait_stop_of_child_managers( RemainingManagers, State )

			after WaitDuration ->

				?error_fmt( "Following child time managers failed to report "
					"on time (after ~B milliseconds) that they stopped: ~p.",
					[ WaitDuration, set_utils:to_list( ChildManagers ) ] )

			end

	end.



% Flushes all scheduling messages which could be already sitting in the process
% mailbox.
%
flush_scheduling_messages() ->

	receive

		{ beginTimeManagerTick, _TickOffset } ->
			flush_scheduling_messages();

		{ beginTimeManagerDiasca, [ _TickOffset, _Diasca ] } ->
			flush_scheduling_messages()

	after 0 ->
		ok

	end.



% Detects and takes care of any end of diasca.
%
% Returns an updated state.
%
manage_possible_end_of_diasca( State ) ->

	case is_current_diasca_over( State ) of

		true ->

			case ?getAttr(parent_manager_pid) of

				undefined ->
					% We are the root time manager here:
					manage_end_of_diasca_as_root_manager( State );


				_ParentManagerPid ->
					manage_end_of_diasca_as_child_manager( State )

			end;


		false ->

			% Nothing to do (diasca not finished), still having to wait:
			?display_console( "Time manager ~w still having to wait.",
							  [ self() ] ),

			State

	end.



% Manages an end of diasca, when being a root time manager, which leads to
% either a new diasca or a new tick.
%
% Returns an updated state.
%
manage_end_of_diasca_as_root_manager( State ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),

	CurrentDiasca = ?getAttr(current_diasca),

	% We insert here the resilience preparation as, if it was implemented among
	% the inter-diasca listeners above, it would be done on parallel with the
	% others, hence on a system whose state is still changing.
	%
	ResilienceState = manage_resilience( CurrentTickOffset, CurrentDiasca,
										 State ),

	% Moved after the resilience action, so that a rollback can restart more
	% easily:
	%
	InterState = manage_inter_diasca( ResilienceState ),

	SuspendState = manage_suspension( CurrentTickOffset, InterState ),

	% Either we have a new diasca to plan, or this tick is finished for good and
	% we can go to the next:
	%
	case ?getAttr(next_action) of

		new_diasca_needed ->

			% Next diasca to be scheduled, regardless of the simulation
			% interactivity mode:
			%
			% (a message is sent to ensure tail-recursiveness)
			%
			NewDiasca = CurrentDiasca + 1,

			?display_console( "Root time manager creates a new diasca, "
							  "{~B,~B}.", [ CurrentTickOffset, NewDiasca ] ),

			self() ! { beginTimeManagerDiasca,
					   [ CurrentTickOffset, NewDiasca ] },

			SuspendState;


		no_planned_action ->

			?display_console( "Root time manager does not have "
							  "a planned action.", [] ),

			% We may have to jump to next tick then:
			case ?getAttr(spontaneous_agenda) of


				[] ->

					% Even locally, no next event, time to stop:
					?notice_fmt( "At the global level, there is no actor to "
						"trigger anymore after this diasca (~B) nor spontaneous"
						" action recorded after this tick (offset #~B), "
						"therefore no future event could possibly occur. "
						"Stopping thus now the whole simulation.",
						[ ?getAttr(current_diasca), CurrentTickOffset ] ),

					on_simulation_success( SuspendState ),

					% However there could be actors terminating, stop will
					% manage them:
					%
					% (we cannot use the stop/1 request on ourselves, as the
					% result would itself be interpreted as a call)
					%
					self() ! selfStop,

					SuspendState;


				[ { NextOverallTick, _ActorSet } | _T ] ->

					% New state returned:
					schedule_new_tick( NextOverallTick, SuspendState )

			end;


		% Useless guard:
		TickOffset when TickOffset > CurrentTickOffset ->

			?display_console( "Root time manager selecting next "
							  "scheduling.", [] ),

			% As the end of diasca of local actors does not update the next
			% local action, let's take it into account now:
			%
			SoonestTickOffset = case ?getAttr(spontaneous_agenda) of

				[ { FirstAgendaTick, _ActorSet } | _T ]
								  when FirstAgendaTick < TickOffset ->
					FirstAgendaTick;

				% Either [] or the first agenda tick is in the future:
				_ ->
					TickOffset

			end,

			% Simple jump, new state returned:
			schedule_new_tick( SoonestTickOffset, SuspendState )

	end.




% Schedules the specified tick (root time manager only).
%
% Returns an updated state.
%
schedule_new_tick( NextTickOffset, State ) ->

	case ?getAttr(simulation_interactivity_mode) of

		interactive ->

			% Do nothing if in simulation interactive mode (as we will be
			% triggered by the timer, with a timerTickFinished call), except
			% resetting the safeguard against multi-tick scheduling:
			%
			% (next tick will be the incremented current one)
			%
			true = ?getAttr(interactive_tick_triggered),
			setAttribute( State, interactive_tick_triggered, false );


		batch ->

			?display_console( "Root time Manager ~w at tick offset #~B "
				"determined that the next tick should be #~p.",
				[ self(), ?getAttr(current_tick_offset), NextTickOffset ] ),

			% A message is sent to ensure tail-recursiveness:
			self() ! { beginTimeManagerTick, NextTickOffset },
			State

	end.



% Manages a possible end of diasca, when being a local, non-root time manager.
%
% Returns an updated state.
%
manage_end_of_diasca_as_child_manager( State ) ->

	Agenda = ?getAttr(spontaneous_agenda),

	% We have to update the next action with the information from the agenda:
	UpdatedNextAction = case ?getAttr(next_action) of

		new_diasca_needed ->
			% Cannot be beaten:
			new_diasca_needed;

		% We might have merged the two cases below (based on the Erlang term
		% ordering between integers and atoms), but it would have been a lot
		% less clear:
		%
		no_planned_action ->
			case Agenda of

				[] ->
					no_planned_action;

				[ { SoonestAgendaTickOffset, _ActorSet } | _T ] ->
					SoonestAgendaTickOffset

			end;

		TickOffset ->
			case Agenda of

				[] ->
					TickOffset;

				[ { SoonestAgendaTickOffset, _ActorSet } | _T ]
				  when SoonestAgendaTickOffset < TickOffset ->
					SoonestAgendaTickOffset;

				_ ->
					% Here we expect SoonestAgendaTickOffset >= TickOffset:
					TickOffset

			end

	end,

	?display_console(
	   "Child time manager reporting end of diasca ~B for tick offset #~B, "
	   "next action in subtree is ~p.",
	   [ ?getAttr(current_diasca), ?getAttr(current_tick_offset),
		 UpdatedNextAction ] ),

	ParentManagerPid = ?getAttr(parent_manager_pid),

	% We are a child time manager, we just report the end of diasca for this
	% subtree:
	%
	case ?getAttr(current_diasca) of

		0 ->
			% We have to declare an end of tick here:
			ParentManagerPid ! { notifySpontaneousSubtreeCompletion,
				[ ?getAttr(current_tick_offset), self(), UpdatedNextAction,
				  _TrackingInfo={ ?getAttr(scheduled_tracking),
								  ?getAttr(process_tracking) } ] };

		CurrentDiasca ->
			% We have to declare an end of tick here:
			ParentManagerPid ! { notifyTriggerSubtreeCompletion,
				[ ?getAttr(current_tick_offset), CurrentDiasca, self(),
				  UpdatedNextAction,
				  _TrackingInfo={ ?getAttr(scheduled_tracking),
								  ?getAttr(process_tracking) } ] }

	end,

	% No need to update next_action.
	State.



% Manages an inter-diasca transition.
%
% Returns an updated state.
%
manage_inter_diasca( State ) ->

	% We are the root time manager, deciding what to do next once having taken
	% care of inter-diasca listeners (if any):
	%
	InterDiascaListeners = ?getAttr(interdiasca_listeners),

	% This is the first time we know the current diasca is over; we need to
	% notify any inter-diasca listener (typically the root data-exchanger) as
	% soon as possible:
	%
	InterDiascaMessage = { onInterDiascaBegin, [], self() },

	[ L ! InterDiascaMessage || L <- InterDiascaListeners ],

	% Note: these requests are processed in parallel.

	basic_utils:wait_for( _Msg={ wooper_result, interdiasca_ended },
						  _MsgCount=length( InterDiascaListeners ) ),

	setAttribute( State, interdiasca_listeners, [] ).



% Manages the resilience mechanisms: meant to be called while the system state
% is stable and will not change until this function did its work.
%
% Returns an updated state.
%
manage_resilience( CurrentTickOffset, CurrentDiasca, State ) ->

	% Note: see the resilience manager to better understand the message
	% exchange.

	% We are the root time manager here and the whole simulation is
	% frozen. Let's act quickly!
	%
	case ?getAttr(serialisation_requested) of

		false ->
			State;

		true ->
			% We are just out of the latency-based critical path:

			% We do not want longer serialisations to trigger spurious
			% simulation stalls:
			%
			?getAttr(watchdog_pid) ! suspendWatchdog,

			?getAttr(resilience_manager_pid) ! { triggerSerialisation,
						[ CurrentTickOffset, CurrentDiasca ], self() },

			% Will resume the watchdog:
			wait_for_serialisation_end( _ActorsRequested=false,
										_Serialised=false, State )

	end.



% Waits for the serialisations to end.
%
% Returns an updated state.
%
wait_for_serialisation_end( _ActorsReturned=true, _Serialised=true, State ) ->

	% Just having to wait passively then:
	?display_console( "Waiting for serialisation_done.", [] ),

	receive

		{ wooper_result, serialisation_done } ->

			?display_console( "Serialisation over, resuming simulation.", [] ),

			% Simulation to continue now:
			?getAttr(watchdog_pid) ! resumeWatchdog,

			setAttribute( State, serialisation_requested, false )

	end;


% At least one request is still expected:
wait_for_serialisation_end( ActorsReturned, Serialised, State ) ->

	% Waiting fully idle for the serialisation to finish...
	%
	% However, all time managers (including this root one) must take part to the
	% serialisation action, for example by sending the list of all their local
	% actors; therefore we must be able to answer them, before waiting for the
	% serialisation to be performed. Not to mention that this time manager must
	% also be itself serialised, hence must answer to the corresponding request.
	%
	% For that, we hijack the WOOPER main loop of this root time manager:
	%
	receive

	   % Meant to be sent by the local instance tracker (first received):
	   { getAllLocalActors, [], CallerPid } ->

		   { NewState, Res } = executeRequest( State, getAllLocalActors ),

			CallerPid ! { wooper_result, Res },

			wait_for_serialisation_end( _ActorsReturned=true, Serialised,
										NewState );


		% Meant to be sent by the resilience agent (second received):
		{ serialise, [ EntryTransformer, UserData ], CallerPid } ->

			{ NewState, Res } = executeRequest( State, serialise,
											[ EntryTransformer, UserData ] ),

			CallerPid ! { wooper_result, Res },

			wait_for_serialisation_end( ActorsReturned, _Serialised=true,
										NewState )

	end.




% Manages simulation suspension: if the simulation must be suspended, waits
% until not suspended anymore.
%
% To be preferably called at the end of a tick.
%
% Returns an updated state.
%
manage_suspension( CurrentTickOffset, State ) ->

	% TO-DO: manage correctly the suspension, with regard to simulation
	% batch/interactive mode, suspending timer, time tracker, watchdog, etc. and
	% flushing their relevant messages.

	case ?getAttr(suspended) of

		false ->
			State;

		true ->

			?notice_fmt( "Simulation suspended at {~B,~B}, waiting for a "
				"resume request.",
				[ CurrentTickOffset, ?getAttr(current_diasca) ] ),

			% Blocks as long as necessary:
			receive

				resume ->
					?notice( "Simulation resumed." ),

					[ L ! simulation_resumed
					  || L <- ?getAttr(simulation_listeners) ],

					setAttribute( State, suspended, false )

			end

	end.



% Returns true iff this time manager can safely determine that its current
% diasca is over.
%
is_current_diasca_over( State ) ->

	check_waited_count_consistency( State ),

	% Precomputed, not to perform useless checkings of sets and all:
	case ?getAttr(waited_count) of

		0 ->
			true;

		% NonNullCount when NonNullCount < 20 ->

		%	%?debug_fmt( "(still waiting for a total of ~B "
		%	%	"end-of-tick notifications of all sorts)", [ NonNullCount ] ),

		%	?display_console( "(~w still waiting for a total of ~B "
		%	%		   "end-of-tick notifications of all sorts).",
		%	%		   [ self(), NonNullCount ] ),

		%	%display_waiting_reason( State ),

		%	false;

		_NonNullCount ->
			false

	end.



% Ensures that specified tick offset is compatible with the previous one, in the
% context of a new tick.
%
% Returns nothing useful, just throws an exception if an inconsistency is
% detected.
%
check_tick_consistency( NewTickOffset, State ) ->

	PreviousTickOffset = ?getAttr(current_tick_offset),

	JumpDuration = NewTickOffset - PreviousTickOffset,

	InitialTick = ?getAttr(initial_tick),

	PreviousTick   = InitialTick + PreviousTickOffset,
	NewCurrentTick = InitialTick + NewTickOffset,

	% Traces sent with the previous timestamp:
	case JumpDuration of

		PositiveOffset when PositiveOffset > 0 ->

			% Normal case, more checkings:
			%?notice_fmt( "Tick ~B (tick offset #~B, at diasca ~p) was over, "
			%	"having jumped forward of ~B tick(s) to reach "
			%	"the new current tick ~B, at diasca 0 "
			%	"(corresponding tick offset: #~B).",
			%	[ PreviousTick, PreviousTickOffset, ?getAttr(current_diasca),
			%		JumpDuration, NewCurrentTick, NewTickOffset ] );
			ok;

		NegativeOrNullOffset ->
			?emergency_fmt( "Tick ~B ended (tick offset: #~B, at diasca ~p), "
				"but specified new tick ~B (tick offset #~B) "
				"is not in its future (offset in the past of ~B tick(s)).",
				[ PreviousTick, PreviousTickOffset, ?getAttr(current_diasca),
				  NewCurrentTick, NewTickOffset, - NegativeOrNullOffset ] ),

			throw( { abnormal_tick_transition, PreviousTick, NewCurrentTick } )

	end.



% Ensures that specified tick offset and diasca are compatible with the previous
% ones, in the context of a new diasca.
%
% Returns nothing useful, just throws an exception if an inconsistency is
% detected.
%
check_diasca_consistency( TickOffset, NewDiasca, State ) ->

	PreviousDiasca = ?getAttr(current_diasca),

	% Checkings:
	TickOffset = ?getAttr(current_tick_offset),
	NewDiasca = PreviousDiasca + 1,
	{ TickOffset, PreviousDiasca } = ?getAttr(previous_timestamp).

	% We are on a non-zero diasca, thus there must be at least one child manager
	% with at least one actor to schedule (triggered or terminating), but it
	% cannot be checked here.



% Ensures that the current waiting count is accurate, or throws an exception.
check_waited_count_consistency( State ) ->

	WaitedSpontaneous = set_utils:size( ?getAttr(waited_spontaneous_actors) ),

	WaitedTriggered = set_utils:size( ?getAttr(waited_triggered_actors) ),

	% Either we are at diasca 0 or not:
	case WaitedSpontaneous =/= 0 andalso WaitedTriggered =/= 0 of

		true ->
			throw( { spontaneous_trigger_mismatch, WaitedSpontaneous,
					 WaitedTriggered } );

		false ->
			ok

	end,

	WaitedChildren = set_utils:size( ?getAttr(waited_child_managers) ),

	WaitedWatchdog = case ?getAttr(watchdog_waited) of

		true ->
			1;

		false ->
			0

	end,

	RealSum = WaitedSpontaneous + WaitedTriggered + WaitedChildren
		+ WaitedWatchdog,

	?display_console( "~w waiting for C=~B, S=~B, T=~B, W=~B.", [ self(),
		WaitedChildren, WaitedSpontaneous, WaitedTriggered, WaitedWatchdog ] ),

	case ?getAttr(waited_count) of

		RealSum->
			ok;

		WrongCount ->
			throw( { inconsistent_waited_count, WrongCount,
				{ RealSum, { WaitedChildren, WaitedSpontaneous, WaitedTriggered,
							 WaitedWatchdog } } } )

	end.



% Outputs in console the current status regarding waiting of this time manager.
-spec display_waiting_reason( wooper:state() ) -> void().
display_waiting_reason( State ) ->

	WaitedSpontaneous = set_utils:size( ?getAttr(waited_spontaneous_actors) ),

	WaitedTriggered = set_utils:size( ?getAttr(waited_triggered_actors) ),

	WaitedChildren = set_utils:size( ?getAttr(waited_child_managers) ),

	WaitedWatchdog = case ?getAttr(watchdog_waited) of

		true ->
			"the";

		false ->
			"no"

	end,

	io:format( "At tick offset #~B, time manager ~w waiting for "
		"~B spontaneous actor(s), ~B triggered actor(s), "
		"~B child manager(s) and for ~ts watchdog.",
		[ ?getAttr(current_tick_offset), self(), WaitedSpontaneous,
		  WaitedTriggered, WaitedChildren, WaitedWatchdog ] ),

	case WaitedSpontaneous of

		L when L > 0 andalso L < 5 ->

			?display_console( "Waiting for following spontaneous actors: ~p.",
				[ set_utils:to_list( ?getAttr(waited_spontaneous_actors) ) ] );

		_ ->
			ok

	end.




% Manages the new current tick.
%
% Note that a given time manager may have nothing to schedule at one tick, as
% all ticks are propagated downward the full scheduling hierarchy, regardless of
% whether a given time manager has something to schedule or not (this has to be
% that way, otherwise a race condition could occur; and currently a time manager
% cannot know whether there are actors to schedule in its whole subtree).
%
% We delay the console/trace output as much as possible, to reduce the critical
% path, as these operations take some time. However it may lead to some actors
% being scheduled and sending console/trace output before the 'new tick'
% notification.
%
% Returns an updated state.
%
-spec manage_new_tick( tick_offset(), wooper:state() ) -> wooper:state().
manage_new_tick( NewTickOffset, State ) ->

	% A bit of paranoid checking first:
	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			0 = ?getAttr(waited_count),
			false = ?getAttr(watchdog_waited),
			true = set_utils:is_empty( ?getAttr(waited_child_managers) ),
			true = set_utils:is_empty( ?getAttr(waited_spontaneous_actors) ),
			true = set_utils:is_empty( ?getAttr(waited_triggered_actors) )
		end ),

	?display_console( "Scheduling new tick, #~B (hence diasca 0), on ~w",
					  [ NewTickOffset, self() ] ),

	ChildManagers = ?getAttr(child_managers),

	% First of all, recurses in all the scheduling hierarchy, regardless of
	% whether a child manager has any actor to schedule for this new tick:
	%
	ChildManagerCount = notify_child_managers_of_tick( ChildManagers,
													   NewTickOffset ),

	{ SpontaneousActors, SpontaneousCount, NewAgenda } =
		notify_spontaneous_actors( NewTickOffset, State ),

	% Here we should have escaped from most of the critical path (the parallel
	% processing has already started).

	% Notifies the time listeners:
	[ P ! { onNewTick, NewTickOffset } || P <- ?getAttr(time_listeners) ],

	{ WatchedState, _TickHeader, WaitedWatchdog } =
			case is_root_manager( State ) of

		true ->
			% Only the root time manager has a watchdog:
			?getAttr(watchdog_pid) ! { beginWatchdogTick, NewTickOffset },

			% We now consider that only the root time manager is to report the
			% progress, and we have only the information for previous diasca:
			%
			record_progress_message( NewTickOffset, _NewDiasca=0, State ),

			{ setAttribute( State, watchdog_waited, true ),
			  "Root overall tick", _WatchWaited=1 };

		false ->
			{ State, "Child local tick", _WatchWaited=0 }

	end,

	% No actor triggered yet at this tick:
	WaitedCount = SpontaneousCount + ChildManagerCount + WaitedWatchdog,

	?display_console( "Total initial wait summary: ~B, i.e. S=~B, C=~B, W=~B.",
		[ WaitedCount, SpontaneousCount, ChildManagerCount, WaitedWatchdog ] ),

	?display_console( "~ts: ~ts is sent to ~B child time manager(s) and to ~B "
		"local actor(s) that are scheduled for their spontaneous behaviour "
		"(waited count, including any watchdog: ~B).",
		[ TickHeader, get_textual_timings( State ), ChildManagerCount,
		  SpontaneousCount, WaitedCount ] ),

	% Actor termination never occurs at diasca 0.

	% We are now at {NewTickOffset, 0}.
	% We came from {PastTickOffset, Dpast}.
	%
	% Some early non-local actors may already have sent actor messages to local
	% actors; in this case next_timestamp is already at {NewTickOffset,1} and
	% this should be left as is. Otherwise it must be either 'undefined' or the
	% previous timestamp, and no next timestamp is thus known. All other cases
	% mean a scheduling error.
	%
	PreviousTimestamp = ?getAttr(previous_timestamp),

	% Let's reset next_timestamp:
	NewNextTimestamp = case ?getAttr(next_timestamp) of

		PreviousTimestamp ->
			% Was not changed yet:
			undefined;

		undefined ->
			undefined;

		EarlyEntry={ NewTickOffset, 1 } ->
			% Was already set by an early actor, must be kept:
			EarlyEntry;

		WrongNextTimestamp ->
			throw( { scheduling_inconsistency_at_new_manager_tick,
					 NewTickOffset, PreviousTimestamp, WrongNextTimestamp } )

	end,

	EarlyActorsToTrigger = ?getAttr(actors_to_trigger_in_two_diascas),

	KnownNextAction = case set_utils:is_empty( EarlyActorsToTrigger ) of

		true ->

			% Checking:
			undefined = NewNextTimestamp,

			% Default:
			%
			% (we use the new agenda, otherwise we would pick the current tick)
			%
			case NewAgenda of

				[] ->
					no_planned_action;

				[ { TickOffset, _ActorSet } | _T ] ->
					TickOffset

			end;

		false ->

			% Actors already at diasca 0 managed to send a message before this
			% time manager even received the new tick notification (possibly
			% after a jump). Checking:
			%
			{ NewTickOffset, 1 } = NewNextTimestamp,

			new_diasca_needed

	end,

	NewDiascaCount = ?getAttr(diasca_count) + 1,

	NewScheduledCount = ?getAttr(schedule_count) + SpontaneousCount,

	% Newer tick offset and diasca already set:
	ResetState = setAttributes( WatchedState, [
		{ spontaneous_agenda, NewAgenda },
		% previous_timestamp already set.
		{ next_timestamp, NewNextTimestamp },
		{ next_action, KnownNextAction },
		% actors_to_trigger_in_one_diasca kept as is (may have already been
		% updated by early remote actors)

		% We refresh the list:
		{ waited_child_managers, ChildManagers },

		{ waited_spontaneous_actors, SpontaneousActors },
		{ waited_count, WaitedCount },

		% We initialize these two total values with the current local ones, as
		% values reported by child managers will be added during this diasca:
		%
		{ scheduled_tracking, SpontaneousCount },
		{ process_tracking, system_utils:get_process_count() },

		% Ready for next diasca (1):
		{ actors_to_trigger_in_one_diasca, EarlyActorsToTrigger },
		{ actors_to_trigger_in_two_diascas, set_utils:new() },
		{ diasca_count, NewDiascaCount },
		{ schedule_count, NewScheduledCount } ] ),

	TickPeriod = ?getAttr(tick_milestone_period),

	% Each 1000 simulated ticks (ex: after 20 seconds of simulated time at 50
	% Hz), let's trigger a simulation milestone:
	%
	% FIXME_WALLCLOCK: we can miss such deadlines if jumping over them...
	case NewTickOffset rem TickPeriod of

		0 ->
			self() ! { onTickMilestone, NewTickOffset };

		_ ->
			ok

	end,

	% From now, spontaneous actors, child managers and the watchdog are expected
	% to trigger 'notifySpontaneous*Complet*' methods... if there is at least
	% one of such agents:
	%
	case WaitedCount of

		0 ->
			% Nobody will ever answer, we must already report an end of tick:
			manage_possible_end_of_diasca( ResetState );


		_NonNull ->

			cond_utils:if_defined( simdiasca_check_time_management,
				check_waited_count_consistency( ResetState ) ),

			% Answers will trigger back an end of diasca when appropriate:
			%
			% (due to the watchdog, the root time manager will always go that
			% route)
			%
			ResetState

	end.



% Manages the new current (non-zero) diasca.
%
% Note that a given time manager may have nothing to schedule at one diasca, as
% all diascas are propagated downward the full scheduling hierarchy, regardless
% of whether a given time manager has something to schedule (this has to be that
% way, otherwise a race condition could occur; and currently a time manager
% cannot know whether or not there are actors to trigger in its subtree).
%
% We delay the console/trace output as much as possible, to reduce the critical
% path, as these operations take some time. However it may lead to some actors
% being triggered and sending console/trace output before the 'new diasca'
% notification.
%
% Returns an updated state.
%
-spec manage_new_diasca( tick_offset(), diasca(), wooper:state() ) ->
							wooper:state().
manage_new_diasca( TickOffset, NewDiasca, State ) ->

	TerminatingActorList = ?getAttr(terminating_actors),

	% A bit of paranoid checking first:
	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			0 = ?getAttr(waited_count),
			false = ?getAttr(watchdog_waited),
			true = set_utils:is_empty( ?getAttr(waited_child_managers) ),
			true = set_utils:is_empty( ?getAttr(waited_spontaneous_actors) ),
			true = set_utils:is_empty( ?getAttr(waited_triggered_actors) ),
			false = list_utils:has_duplicates( TerminatingActorList )
		end ),

	?display_console( "~n  + scheduling new diasca, tick offset #~B diasca ~B "
					  "on ~w~n", [ TickOffset, NewDiasca, self() ] ),

	ChildManagers = ?getAttr(child_managers),

	% First of all, recurses in all the scheduling hierarchy, regardless of
	% whether a child manager has any actor to schedule for this new tick:
	%
	ChildManagerCount = notify_child_managers_of_diasca( ChildManagers,
										TickOffset, NewDiasca ),

	ScheduledActors = ?getAttr(actors_to_trigger_in_one_diasca),


	% Smaller (plain) list on the left:
	%
	% A perfectly licit situation is to have a terminating (not terminated)
	% actor which, during its active termination, receives actor messages;
	% however we want to have it scheduled only once, so we have to avoid
	% duplicates, which is a side-effect of using a set:
	%
	UniqueTriggeredActors = set_utils:add_element_list( TerminatingActorList,
														ScheduledActors ),

	TriggeredCount = notify_triggered_actors( TickOffset, NewDiasca,
											  UniqueTriggeredActors ),

	% From there, we should at last have escaped from most of the critical path,
	% so we have less time pressure.

	% Notifies the time listeners:
	[ P ! { onNewDiasca, [ TickOffset, NewDiasca ] }
	  || P <- ?getAttr(time_listeners) ],

	{ WatchedState, _DiascaHeader, WaitedWatchdog } =
			case is_root_manager( State ) of

		true ->
			% Only the root time manager has a watchdog:
			?getAttr(watchdog_pid) ! { beginWatchdogDiasca,
									   [ TickOffset, NewDiasca ] },

			% We now consider that only the root time manager is to report the
			% progress, and we have only the information for previous diasca:
			%
			record_progress_message(TickOffset, NewDiasca, State ),

			{ setAttribute( State, watchdog_waited, true ),
			  text_utils:format( "Root overall diasca ~p", [ NewDiasca ] ),
			  _Waited=1 };

		false ->
			{ State,
			  text_utils:format( "Child local diasca ~p", [ NewDiasca ] ),
			  _Waited=0 }

	end,

	?display_console( "Triggered count at {~p,~p}: ~B~n",
					  [ TickOffset, NewDiasca, TriggeredCount ] ),

	% By design, no spontaneous actor waited:
	WaitedCount = TriggeredCount + ChildManagerCount + WaitedWatchdog,

	?display_console( "~n~ts for ~w: ~ts is sent to ~B child time manager(s) "
		"and to ~B local actor(s) that are triggered "
		"(waited count, including any watchdog: ~B).",
		[ DiascaHeader, self(), get_textual_timings( State ),
		  ChildManagerCount, TriggeredCount, WaitedCount ] ),

	TerminateState = terminate_actors( TickOffset, NewDiasca, WatchedState ),

	% We are now at {TickOffset, D}.
	% We came from {TickOffset, D-1}.
	%
	% Some early non-local actors may already have sent actor messages to local
	% actors; in this case next_timestamp is already at {TickOffset,D+1} and
	% this should be left as is. Otherwise it must be either 'undefined' or the
	% previous timestamp, and no next timestamp is thus known. All other cases
	% mean a scheduling error.

	CurrentTimestamp = { TickOffset, NewDiasca },

	EarlyTimestamp = { TickOffset, NewDiasca + 1 },

	% Let's reset next_timestamp:
	NewNextTimestamp = case ?getAttr(next_timestamp) of

		CurrentTimestamp ->
			% Was not changed yet:
			undefined;

		undefined ->
			undefined;

		EarlyTimestamp ->
			% Was already set by an early actor, must be kept:
			EarlyTimestamp;

		WrongNextTimestamp ->
			throw( { scheduling_inconsistency_at_new_manager_diasca,
					TickOffset, NewDiasca, ?getAttr(previous_timestamp),
					CurrentTimestamp, WrongNextTimestamp } )

	end,

	EarlyActors = ?getAttr(actors_to_trigger_in_two_diascas),

	% Either we already know that there will be a next diasca or not:
	KnownNextAction = case set_utils:is_empty( EarlyActors ) of

		true ->

			% Checking, cannot have been set by an early actor:
			undefined = NewNextTimestamp,

			% Updates the information if needed:
			%
			% (will be overwritten as soon as one actor message is sent
			% this diasca)
			%
			case ?getAttr(spontaneous_agenda) of

				[] ->
					no_planned_action;

				[ { NextTickOffset, _ActorSet } | _T ] ->
					NextTickOffset

			end;


		false ->

			?display_console( "Early actor detected, new diasca needed!~n",
							  [] ),

			% Checking, must have been set by an early actor:
			EarlyTimestamp = NewNextTimestamp,
			new_diasca_needed

	end,

	NewDiascaCount = ?getAttr(diasca_count) + 1,

	NewScheduleCount = ?getAttr(schedule_count) + TriggeredCount,

	% Newer tick offset and diasca already set:
	ResetState = setAttributes( TerminateState, [
		% previous_timestamp already set.
		{ next_timestamp, NewNextTimestamp },
		{ next_action, KnownNextAction },

		% This list is rebuilt each diasca:
		{ terminating_actors, [] },

		% The only place, once processed, where we can reset it without taking
		% the risk of forgetting early schedule-trigger notifications:
		%
		{ actors_to_trigger_in_one_diasca, EarlyActors },
		{ actors_to_trigger_in_two_diascas, set_utils:new() },

		% We refresh the list:
		{ waited_child_managers, ChildManagers },

		% We know we always wait for them all, using most efficient list:
		{ waited_triggered_actors, UniqueTriggeredActors },
		{ waited_count, WaitedCount },

		% We initialize these two total values with the current local ones, as
		% values reported by child managers will be added during this diasca:
		%
		{ scheduled_tracking, TriggeredCount },
		{ process_tracking, system_utils:get_process_count() },
		{ diasca_count, NewDiascaCount },

		% We add first the local actors:
		{ schedule_count, NewScheduleCount } ] ),


	% No 'onDiascaMilestone' event deemed useful yet.

	% From now, triggered actors, child managers and the watchdog are expected
	% to trigger 'notifyTrigger*Complet*' methods... if there is at least one
	% of such agents:
	%
	case WaitedCount of

		0 ->
			% Nobody will ever answer, we must already report an end of tick:
			manage_possible_end_of_diasca( ResetState );

		_NonNull ->
			check_waited_count_consistency( ResetState ),
			% Answers will trigger back an end of diasca when appropriate:
			ResetState

	end.



% Takes care of writing in the traces and to the time tracker the latest
% progress information.
%
% To be executed by all time managers.
%
% Note that this progress information is neither authoritative nor even
% consistent, as it gathers various information that may not be synchronised.
%
% We prefer reporting a new timestamp rather than the previous one, as the
% console output is deemed clearer for the user (and even if most of the
% displayed information actually relate to the previous timestamp).
%
% Does not return anything useful.
%
-spec record_progress_message( tick_offset(), diasca(), wooper:state() ) ->
										void().
record_progress_message( TickOffset, Diasca, State ) ->

	case ?getAttr(time_tracker_pid) of

		undefined ->
			ok;

		TimeTrackerPid ->
			report_progress_to( TickOffset, Diasca, TimeTrackerPid, State )

	end.



% (helper)
report_progress_to( TickOffset, Diasca, TimeTrackerPid, State ) ->

	% We have delayed these processings once the 'begin tick/diasca' message has
	% been sent (i.e. when we are no more on the critical path), while still
	% hoping to *display* this 'top' message (about previous diasca) first,
	% before any message sent in the context of current (next) diasca:
	%
	% (date conversion in messages might be a bit expensive, but, if the console
	% tracker only uses actually very few of them, all may be used in traces,
	% hence are useful)
	%
	{ _DetailedTopMessage, _CompactTopMessageElements=Timings, RealTime } =
		get_full_textual_timings( TickOffset, Diasca, State ),

	%trace_utils:debug_fmt( "Reporting progress for {~p,~p} at ~p.~n",
	%					   [ TickOffset, Diasca, RealTime ] ),

	% Here we use the information gathered on the last diasca, relative to the
	% scheduling subtree of this manager:
	%
	OverallSchedulingCount = ?getAttr(scheduled_tracking),
	OverallProcessCount = ?getAttr(process_tracking),

	OverallActorCount = ?getAttr(overall_actor_count),

	Counts = { OverallActorCount, OverallSchedulingCount, OverallProcessCount },

	NewTopInfo = { Timings, Counts },

	% We probably should give overall information like total actor count in the
	% root time manager only, as the child ones are not notified of that actual
	% value.

	%?notice_fmt(
	%	"~ts: sent at ~ts to ~B child time manager(s) and to ~B local "
	%   "actor(s). "
	%	"During last diasca, for this scheduling subtree, "
	%	"we had a total of ~B actors that were scheduled, "
	%	"with ~B processes alive.",
	%	[ Header, DetailedTopMessage, ChildManagerCount, ScheduledCount,
	%	  OverallSchedulingCount, OverallProcessCount ] ).

	TimeTrackerPid ! { RealTime, NewTopInfo }.




% Updates the specified spontaneous agenda accordingly.
%
% Returns an updated state.
%
-spec update_agenda( [ tick_offset() ], [ tick_offset() ], tick_offset(),
					 actor_pid(), wooper:state() ) -> wooper:state().
update_agenda( AddedSpontaneousTicks, WithdrawnSpontaneousTicks,
			   CurrentTickOffset, ActorPid, State ) ->

	Agenda = ?getAttr(spontaneous_agenda),

	% We withdraw before adding, hence if a never-specified tick is to be added
	% and withdrawn *at the same diasca*, the operation will fail (as considered
	% abnormal):
	%
	WithdrawAgenda = withdraw_from_agenda( ActorPid, WithdrawnSpontaneousTicks,
										   CurrentTickOffset, Agenda ),

	AddAgenda = add_to_agenda( ActorPid, AddedSpontaneousTicks,
							   CurrentTickOffset, WithdrawAgenda ),

	cond_utils:if_defined( simdiasca_debug_time_management,
		trace_utils:debug_fmt( "New agenda for ~w: ~ts.",
							   [ ActorPid, agenda_to_string( AddAgenda ) ] ) ),

	setAttribute( State, spontaneous_agenda, AddAgenda ).



% Withdraws specified tick offsets for the specified actor from specified
% agenda.
%
withdraw_from_agenda( _ActorPid, _WithdrawnSpontaneousTicks=[],
					  _CurrentTickOffset, Agenda ) ->
	Agenda;


% Must be in the future:
withdraw_from_agenda( ActorPid, _WithdrawnSpontaneousTicks=[ TickOffset | T ],
			CurrentTickOffset, Agenda ) when TickOffset > CurrentTickOffset ->

	NewAgenda = withdraw_from_agenda_helper( ActorPid, TickOffset, Agenda,
											 _Acc=[] ),

	withdraw_from_agenda( ActorPid, T, CurrentTickOffset, NewAgenda );


withdraw_from_agenda( ActorPid, _WithdrawnSpontaneousTicks=[ TickOffset | _T ],
					  CurrentTickOffset, _Agenda ) ->
	throw( { tick_to_withdraw_in_the_past, ActorPid, TickOffset,
			 CurrentTickOffset } ).



% Withdraws specified actor at specified offset from agenda.
%
% Returns an updated agenda.
%
withdraw_from_agenda_helper( ActorPid, TickOffset, _Agenda=[], _Acc ) ->
	% Agenda exhausted, tick not found:
	throw( { no_spontaneous_tick_to_withdraw, TickOffset, ActorPid } );

withdraw_from_agenda_helper( ActorPid, TickOffset,
							 _Agenda=[ { TickOffset, ActorSet } | T ], Acc ) ->

	% Tick found, was already declared:
	%
	% (search and remove could be done in one pass)
	%
	case set_utils:member( ActorPid, ActorSet ) of

		false ->
			throw( { no_spontaneous_tick_to_withdraw, TickOffset, ActorPid } );

		true ->
			% The set might end up being empty, let's remove it in this case:
			NewActorSet = set_utils:delete( ActorPid, ActorSet ),

			% Agenda is sorted by ascending offsets:
			case set_utils:is_empty( NewActorSet ) of

				true ->
					lists:reverse( Acc ) ++ T;

				false ->
					lists:reverse( Acc ) ++ [ { TickOffset, NewActorSet } | T ]

			end

	end;

withdraw_from_agenda_helper( ActorPid, TickOffset,
					  _Agenda=[ E={ TOffset, _ActorSet } | T ], Acc )
  when TOffset < TickOffset ->

	% Offset not reached yet, continue iterating:
	withdraw_from_agenda_helper( ActorPid, TickOffset, T, [ E | Acc ] );

withdraw_from_agenda_helper( ActorPid, TickOffset, _Agenda, _Acc ) ->
	% Tick not found (neither higher nor equal here):
	throw( { no_spontaneous_tick_to_withdraw, TickOffset, ActorPid } ).




% Adds specified tick offsets for the specified actor to the specified agenda.
add_to_agenda( _ActorPid, _AddedSpontaneousTicks=[], _CurrentTickOffset,
			   Agenda ) ->
	Agenda;

add_to_agenda( ActorPid, _AddedSpontaneousTicks=[ TickOffset | T ],
		   CurrentTickOffset, Agenda ) when TickOffset > CurrentTickOffset ->

	NewAgenda = add_to_agenda_helper( ActorPid, TickOffset, Agenda, _Acc=[] ),

	add_to_agenda( ActorPid, T, CurrentTickOffset, NewAgenda );

add_to_agenda( ActorPid, _AddedSpontaneousTicks=[ TickOffset | _T ],
			CurrentTickOffset, _Agenda ) when TickOffset > CurrentTickOffset ->
	throw( { tick_to_add_in_the_past, ActorPid, TickOffset,
			 CurrentTickOffset } ).



% Adds specified actor at specified offset in agenda.
%
% Returns an updated agenda.
%
add_to_agenda_helper( ActorPid, TickOffset, _Agenda=[], Acc ) ->
	% Agenda exhausted, tick to be added last:
	NewActorSet = set_utils:singleton( ActorPid ),
	lists:reverse( [ { TickOffset, NewActorSet } | Acc ] );

add_to_agenda_helper( ActorPid, TickOffset,
					  _Agenda=[ { TickOffset, ActorSet } | T ], Acc ) ->

	% Tick found, as was already declared: actor just to be added to the
	% pre-existing list here.
	%
	% We allow here an actor to declare the same spontaneous tick more than
	% once; in any case its PID will be listed only once (if any); we could have
	% also checked the presence of the PID before adding it, just to notify
	% (warning or error) that it was included multiple times, but we allow
	% multiple declarations (not considered as an error; more convenient for
	% model developers).
	%
	NewActorSet = set_utils:add( ActorPid, ActorSet ),
	lists:reverse( Acc ) ++ [ { TickOffset, NewActorSet } | T ];

add_to_agenda_helper( ActorPid, TickOffset,
					  _Agenda=[ E={ SmallerOffset, _ActorSet } | T ], Acc )
  when TickOffset > SmallerOffset ->
	% Still in smaller offsets here, let's continue iterating:
	add_to_agenda_helper( ActorPid, TickOffset, T, [ E | Acc ] );

add_to_agenda_helper( ActorPid, TickOffset,
		% Clearer: Agenda=[ { _HigherOffset, _ActorSet } | _T ], Acc ) ->
		Agenda, Acc ) ->

	% Implicitly here we went past the last smaller (i.e. HigherOffset >
	% TickOffset), so we have to add a new set, at the relevant place
	% (i.e. just before):
	%
	NewActorSet = set_utils:singleton( ActorPid ),
	lists:reverse( Acc ) ++ [ { TickOffset, NewActorSet } | Agenda ].



% Notifies all specified child managers that the specified tick is to begin.
%
% Returns the number of child managers.
%
notify_child_managers_of_tick( ChildManagers, NewTickOffset ) ->

	?display_console( "Notifying at tick offset #~B following "
		"child managers: ~p.",
		[ NewTickOffset, set_utils:to_list( ChildManagers ) ] ),

	% case ChildManagers of

	%	[] ->
	%		?display_console( "No child manager to notify of new tick "
	%                         "offset #~B.", [ NewTickOffset ] );

	%	_Children ->
	%		?display_console( "Notifying child managers ~w of new tick "
	%                         "offset #~B.", [ ChildManagers, NewTickOffset ] )

	% end,

	basic_utils:send_to_pid_set( { beginTimeManagerTick, NewTickOffset },
								 ChildManagers ).



% Notifies all specified child managers that the specified diasca is to begin.
notify_child_managers_of_diasca( ChildManagers, TickOffset, NewDiasca ) ->

	?display_console( "Notifying at diasca ~B in tick offset #~B "
		"following child managers: ~p.",
		[ NewDiasca, TickOffset, set_utils:to_list( ChildManagers ) ] ),

	% case ChildManagers of

	%	[] ->
	%		?display_console( "No child manager to notify of new diasca ~B "
	%					"at tick offset #~B.", [ NewDiasca, TickOffset ] );

	%	_Children ->
	%		?display_console( "Notifying child managers ~w of new diasca ~B "
	%					"at tick offset #~B.",
	%					[ ChildManagers, NewDiasca, TickOffset ] )

	% end,

	% Returns the number of child managers:
	basic_utils:send_to_pid_set(
		{ beginTimeManagerDiasca, [ TickOffset, NewDiasca ] }, ChildManagers ).



% Notifies all local actors that they are expected to develop their spontaneous
% behaviour that a new tick is to be scheduled now.
%
% Returns a {SpontaneousActors, ActorCount, NewAgenda} triplet made of the
% corresponding spontaneous actors (as a set), their count and of the new
% agenda.
%
notify_spontaneous_actors( NewTickOffset, State ) ->

	Agenda = ?getAttr(spontaneous_agenda),

	% Then only, manages the local actors that must be scheduled:
	case get_spontaneous_for( NewTickOffset, Agenda ) of


		none ->
			?display_console( "No spontaneous actor to notify at "
							  "tick offset #~B.", [ NewTickOffset ] ),
			{ set_utils:new(), 0, Agenda };


		{ ActorSet, NewSpontaneousAgenda } ->

			?display_console( "Notifying at tick offset #~B "
				"following spontaneous actors: ~p.",
				[ NewTickOffset, set_utils:to_list( ActorSet ) ] ),

			% Oneway:
			Count = basic_utils:send_to_pid_set( { beginTick, NewTickOffset },
												 ActorSet ),

			{ ActorSet, Count, NewSpontaneousAgenda }

	end.



% Notifies all local actors that received an actor message last diasca, or that
% are actively terminating, that a new diasca began.
%
% Returns the number of triggered actors.
%
notify_triggered_actors( TickOffset, NewDiasca, TriggeredActors ) ->
	basic_utils:send_to_pid_set(
	  { beginDiasca, [ TickOffset, NewDiasca ] }, TriggeredActors ).




% Called whenever the simulation terminates on success.
%
% Does not return anything useful.
%
on_simulation_success( State ) ->
	[ L ! simulation_succeeded || L <- ?getAttr(simulation_listeners) ].



% Terminates the actors which already notified this time manager on the previous
% diasca that they were terminating.
%
% Returns an updated state.
%
-spec terminate_actors( tick_offset(), diasca(), wooper:state() ) ->
							wooper:state().
terminate_actors( TickOffset, NewDiasca, State ) ->

	ActorList = ?getAttr(terminated_actors),

	?display_console( "Terminated actors: ~w.", [ ActorList ] ),

	% Rush for maximum parallelism:
	TerminationMessage = { beginTerminationDiasca, [ TickOffset, NewDiasca ] },
	[ Actor ! TerminationMessage || Actor <- ActorList ],

	% Then take care of the internal administrative details:
	TerminatedState = lists:foldl(
					fun( Actor, FoldedState ) ->
						actual_unsubscribing( Actor, FoldedState )
					end,
					_InitialAcc=State,
					ActorList ),

	% As a result, these terminated actors are not anymore among the known,
	% local ones.

	ActorsToDelete = ActorList ++ ?getAttr(actors_to_delete_at_next_tick),

	setAttributes( TerminatedState, [
			{ terminated_actors, [] },
			{ actors_to_delete_at_next_tick, ActorsToDelete } ] ).



% Terminates the actors that are still running, except the ones specified as to
% skip (probably because they have already been deleted).
%
% Typically used whenever the simulation (normally) ends for any reason whereas
% there are still non-terminating actors.
%
% Returns an updated state.
%
% (helper)
%
-spec terminate_running_actors( [ actor_pid() ], wooper:state() ) ->
									wooper:state().
terminate_running_actors( ActorsToSkip, State ) ->

	SkipActorSet = set_utils:from_list( ActorsToSkip ),

	% Not calling their simulationEnded/1 here, as we want to delete them
	% synchronously (ex: otherwise their could be a race condition in which the
	% local instance tracker would alreagy be deleted, hence unregistered, where
	% actors are still being deleted)

	BaseActorSet = ?getAttr(known_local_actors),

	% Eliminates the actors to skip from the local ones that will be deleted:
	ToDelSet = set_utils:difference( BaseActorSet, SkipActorSet ),

	TargetActors = set_utils:to_list( ToDelSet ),

	%trace_utils:debug_fmt( "Terminating ~B still running actors: ~w.",
	%					   [ length( TargetActors ), TargetActors ] ),

	% We used to rely on synchronous deletions, however, typically at simulation
	% tear-down, among all these actors they may exist some that own others in
	% that TargetActors list (ex: a planning owning plannable elements, since
	% they must be deallocated whenever their planning is itself deallocated);
	% as a result these owned actors would be deleted twice: one because they
	% belong to this TargetActors list, and one because their owner (also in
	% that list) is itself deallocated.
	%
	% So using a synchronous deletion here would result in synchronous deletion
	% time-outs ("Stopped waiting for the deletion of..."), triggered when
	% waiting for an already deleted owned actor, which is not satisfactory.
	%
	% We therefore rely here on asynchronous deletions now (this is not a too
	% serious problem as we are at simulation teardown, yet we loose the
	% certainty that all actors will be deallocated for sure; most probably that
	% the VM will halt whereas actors remain; a solution would be for a given
	% time manager to wait/poll for some time for the local instance tracker
	% (which is not an actor, and is the only one notified by an actor whenever
	% it is deleted) until, hopefully, it tells that it is not tracking actors
	% anymore.

	% wooper:safe_delete_synchronously_instances could also be an option:
	%wooper:delete_synchronously_instances( TargetActors ),
	[ APid ! delete || APid <- TargetActors ],

	setAttribute( State, known_local_actors, set_utils:new() ).



% Performs the actual unsubscription of the specified actor.
%
% Note: placed in a dedicated function, as used from more than one place.
%
% Returns an updated state.
%
% (helper)
%
actual_unsubscribing( ActorPid, State ) ->

	%?display_console( "############ Unsubscribing actor ~w.", [ ActorPid ] ),

	LocalActors = ?getAttr(known_local_actors),

	case set_utils:member( ActorPid, LocalActors ) of

		true ->

			?debug_fmt( "Unsubscribing actor ~w.", [ ActorPid ] ),

			PurgedState = ensure_actor_never_scheduled_anymore( ActorPid,
																State ),

			UpdatedLocalActors = set_utils:delete( ActorPid, LocalActors ),

			setAttribute( PurgedState, known_local_actors, UpdatedLocalActors );

		false ->

			throw( { unknown_actor_to_unsubscribe, ActorPid } )

	end.



% Ensures that specified actor may not be scheduled anymore.
%
% A mere (expensive) checking that can be disabled as a whole.
%
ensure_actor_never_scheduled_anymore( ActorPid, State ) ->

	% We go through all the agenda:
	%
	% (we could as well maintain a list of terminated actors to better spot
	% life-cycle errors, based on the instance tracker)
	%
	[ check_not_in_slot( ActorPid, S ) || S <- ?getAttr(spontaneous_agenda) ],

	State.



% Checks that specified actor is not in specified slot.
%
check_not_in_slot( ActorPid, { TickOffset, ActorSet } ) ->

	case set_utils:member( ActorPid, ActorSet ) of

		true ->
			throw( { future_schedule_for_terminating_actor, ActorPid,
					 TickOffset } );

		false ->
			ok

	end.



% Spontaneous tick management.
%
% The 'next spontaneous ticks' list is to be ordered according to the first
% member of its pairs, which is a simulation tick, from smallest to latest.



% Returns a pair made of any actor set registered for the specified tick in the
% specified agenda and of a corresponding new updated spontaneous agenda, or
% 'none' (should not actor be registered for that tick).
%
% Relies on the fact that the spontaneous list is ordered by increasing first
% element of the pair (i.e. by increasing ticks) and that therefore we always
% pop its head.
%
% Note: the third clause and the guard of the second could be removed.
%
get_spontaneous_for( TickOffset,
		_SpontaneousAgenda=[ { TickOffset, ActorSet } | T ] ) ->
	% Found, and returned popped:
	{ ActorSet, _NewSpontaneousAgenda=T };

get_spontaneous_for( _TickOffset, _SpontaneousAgenda=[] ) ->
	% Nothing is planned at all (local agenda exhausted), but it does not mean
	% the simulation will remain idle until the end of time, as other time
	% managers may have a non-empty agenda.
	%
	none;

get_spontaneous_for( TickOffset,
		_SpontaneousAgenda=[ { OtherTickOffset, _ActorSet } | _T ] )
			when OtherTickOffset > TickOffset ->
	% Other tick already in the future, thus nothing to do currently:
	none;

get_spontaneous_for( TickOffset, SpontaneousAgenda ) ->
	% here we must have found in the agenda a tick offset smaller than the
	% specified one: spontaneous entry in the past, abnormal!
	throw( { spontaneous_entry_in_the_past, TickOffset, SpontaneousAgenda } ).



% Inserts specified actor (PID) in the list of actors to be spontaneously
% scheduled on specified tick offset, if not already present.
%
% Returns an updated schedule agenda.
%
schedule_as_spontaneous_for( TickOffset, Actor, SpontaneousAgenda ) ->
	insert_as_spontaneous_for( TickOffset, Actor, SpontaneousAgenda,
							   _ReversedEndList=[] ).



% Here we deal actually with tick offsets, not (absolute) ticks.
%
% Example:
%    BeginList   ReversedEndList
% 1: [A,B,C,D,E] []
% 1: [B,C,D,E]   [A]
% 1: [C,D,E]     [B,A]
% 1: [D,E]       [C,B,A]
% so to rebuild the list we use: lists:reverse(ReversedEndList) ++ BeginList

% We arrived at the end of the list, not found, insert at last position:
insert_as_spontaneous_for( Tick, Actor, _BeginList=[], ReversedEndList ) ->
	lists:reverse( [ { Tick, set_utils:singleton( Actor ) }
					 | ReversedEndList ] );


% The tick has already an entry; adding this actor (in a set, hence this actor
% will be there only once, even if it was already present)
%
insert_as_spontaneous_for( Tick, Actor,
		_CurrentList=[ { Tick, ActorSet } | RemainderOfBeginList ],
		ReversedEndList ) ->

	NewActorSet = set_utils:add( Actor, ActorSet ),

	lists:reverse( [ { Tick, NewActorSet } | ReversedEndList ] )
		++ RemainderOfBeginList;


insert_as_spontaneous_for( Tick, Actor,
		BeginList=[ { CurrentTick, _ActorSet } | _RemainderOfBeginList ],
		ReversedEndList ) when CurrentTick > Tick ->

	% Here we just went past the correct tick, which had no actor list yet,
	% adding it:
	%
	lists:reverse( [ { Tick, set_utils:singleton( Actor ) }
					 | ReversedEndList ] ) ++ BeginList;

insert_as_spontaneous_for( Tick, Actor, [ Entry | BeginList ],
						   ReversedEndList ) ->

	% Here implicitly we have still CurrentTick < Tick, therefore just
	% recursing:
	%
	insert_as_spontaneous_for( Tick, Actor, BeginList,
							   [ Entry | ReversedEndList ] ).



% Merges specified agendas into a unique one.
%
% Preferably, the agenda having the smaller number of pairs should be the first
% specified one.
%
-spec merge_agendas( agenda(), agenda() ) -> agenda().
merge_agendas( _FirstAgenda=[], SecondAgenda ) ->
	SecondAgenda;

% We iterate over the first, and complement the second:
%
% Entries are { Tick, ActorSet } elements.
%
merge_agendas( _FirstAgenda=[ Entry | T ], SecondAgenda ) ->

	UpdatedSecondAgenda = insert_schedule_list_for( Entry, SecondAgenda ),

	merge_agendas( T, UpdatedSecondAgenda ).



% Adds the specified set of actors on specified agenda at specified tick, and
% returns the resulting agenda.
%
% Quite similar to insert_as_spontaneous_for/4.
%
insert_schedule_list_for( Entry, Agenda ) ->
	insert_schedule_list_for( Entry, Agenda, _ReversedEndAgenda=[] ).



% (helper)
% We arrived at the end of the list, not found, insert at last position:
insert_schedule_list_for( Entry, _Agenda=[], ReversedEndAgenda ) ->
	lists:reverse( [ Entry | ReversedEndAgenda ] );


% Tick already defined, just adding our entry there:
insert_schedule_list_for( _Entry={ Tick, ActorSet },
		_Agenda=[ { Tick, CurrentSet } | T ], ReversedEndAgenda ) ->
	% Complementing that tick with both sets:
	MergedSet = set_utils:union( ActorSet, CurrentSet ),
	lists:reverse( [ { Tick, MergedSet } | ReversedEndAgenda ] ) ++ T;


% Here we just went past the correct tick, which had no actor set yet, adding
% it:
%
insert_schedule_list_for( Entry={ ETick, _ActorSet },
		Agenda=[ { ATick, _CurrentSet } | _T ], ReversedEndAgenda )
  when ATick > ETick ->
	lists:reverse( [ Entry | ReversedEndAgenda ] ) ++ Agenda;

insert_schedule_list_for( Entry, _Agenda=[ AEntry | T ], ReversedEndAgenda ) ->
	% Here implicitly ATick < ETick, therefore just recursing:
	insert_schedule_list_for( Entry, T, [ AEntry | ReversedEndAgenda ] ).



% Launches the watchdog.
%
% Returns an updated state.
%
launch_watchdog( State ) ->

	%WatchdogDuration = ?watchdog_wait_duration,

	% Needed, as a self() in a closure would be evaluated by the spawned
	% process:
	%
	RootTimeManagerPid = self(),

	% They depend on the execution target:
	DelayBeforeFirstStall =
		get_max_inter_diasca_duration_until_first_stall_detected(),

	DelayBetweenNextStalls =
		get_max_inter_diasca_duration_until_next_stall_detected(),

	DelayBeforeFailed = get_max_inter_diasca_duration_until_failure_triggered(),

	% The watchdog ensures the manager does not get stuck:
	% (closure used to avoid exporting the function)
	%
	WatchdogPid = ?myriad_spawn_link(
		fun() ->
			watchdog_main_loop( RootTimeManagerPid, DelayBeforeFirstStall,
				DelayBetweenNextStalls, DelayBeforeFailed,
				_NextPeriod=DelayBeforeFirstStall, _AccumulatedDuration=0 )
		end ),

	?debug_fmt( "Watchdog ~w created and running, with a time-out duration "
		"of ~B ms (~ts) for any initial stall, then ~B ms (~ts) for next ones, "
		"and of ~B ms (~ts) for automatic shutdowns; "
		"notifying now initial actors that the simulation started.",
		[ WatchdogPid, DelayBeforeFirstStall,
		  time_utils:duration_to_string( DelayBeforeFirstStall ),
		  DelayBetweenNextStalls,
		  time_utils:duration_to_string( DelayBetweenNextStalls ),
		  DelayBeforeFailed,
		  time_utils:duration_to_string( DelayBeforeFailed ) ] ),

	setAttributes( State, [
			{ watchdog_pid, WatchdogPid },
			{ watchdog_waited, false } ] ).



% Launches the timer.
%
% Returns an updated state.
%
launch_timer( State ) ->

	?notify_by_speak( "Starting simulation clock in "
					  "simulation-interactive mode." ),

	TickDuration = ?getAttr(simulation_tick_duration),

	case TickDuration of

		D when is_float( D ) andalso D > 0 ->
			ok;

		_ ->
			throw( { invalid_tick_duration, TickDuration } )

	end,

	% duration_to_string/1 expects milliseconds:
	TickDurationString = time_utils:duration_to_string(
						   erlang:round( TickDuration * 1000 ) ),

	Frequency = 1 / TickDuration,

	TimerTimeOut = ?getAttr(simulation_tick_waiting),

	case TimerTimeOut of

		T when is_integer( T ) andalso T > 0 ->
			ok;

		_ ->
			throw( { invalid_tick_waiting, TimerTimeOut } )

	end,

	?notify_mute_fmt( "Starting global simulation clock in "
		"simulation-interactive mode at ~ts with a requested simulation "
		"frequency of approximately ~fHz (period of exactly ~ts), with "
		"~B ms expected between simulation ticks.",
		[ get_textual_timings( State ), Frequency, TickDurationString,
		  TimerTimeOut ] ),

	% Closure used to avoid exporting the function:

	RootTimeManagerPid = self(),

	TimerPid = ?myriad_spawn_link(
				 fun() ->
						 timer_main_loop( RootTimeManagerPid, TimerTimeOut )
				 end ),

	setAttributes( State, [ { interactive_tick_triggered, false },
							{ timer_pid, TimerPid } ] ).


% Launches the wallclock time tracker.
%
% Returns an updated state.
%
launch_wallclock_tracker( State ) ->

	RootTimeManagerPid = self(),

	WallclockTrakerPid = ?myriad_spawn_link(
		fun() ->
				wallclock_tracker_main_loop( RootTimeManagerPid,
					?getAttr(wallclock_milestone_period), _TotalDuration=0 )
		end ),

	setAttribute( State, wallclock_tracker_pid, WallclockTrakerPid ).


% (helper)
launch_time_tracker( State ) ->

	RootTimeManagerPid = self(),

	?debug( "Creating simulation-time tracker." ),

	TimeTrakerPid = ?myriad_spawn_link( fun() ->
		time_tracker_start( ?getAttr(load_balancer_pid), RootTimeManagerPid )
										end ),

	setAttribute( State, time_tracker_pid, TimeTrakerPid ).



-spec agenda_to_string( agenda() ) -> ustring().
agenda_to_string( _Agenda=[] ) ->
	"empty agenda";

agenda_to_string( Agenda ) ->
	text_utils:format( "agenda over ~B tick(s): ~ts",
		[ length( Agenda ), text_utils:strings_to_string(
			[ text_utils:format( "~B actor(s) for tick offset ~B: ~ts",
				begin
					Actors = set_utils:to_list( ActorSet ),
					[ length( Actors ), Tick,
					  text_utils:pids_to_short_string(Actors ) ]
				end ) || { Tick, ActorSet } <- Agenda ] ) ] ).



% Hooks for serialisation/deserialisation, used by WOOPER-specified serialise/3.

% We do not want the serialiser function to see the PIDs of the local worker
% processes, as of course they are not meant to be resolved by the instance
% tracker (this would fail). We only cherry-pick the relevant information.



% Triggered just before serialisation.
-spec pre_serialise_hook( wooper:state() ) -> wooper:state().
pre_serialise_hook( State ) ->

	% In this state forged for serialisation, we keep only the interesting bits
	% (for example private processes are silenced, as they could not be resolved
	% by instance trackers):
	%
	wooper_serialisation:mute_attributes( [ parent_manager_pid,
		time_tracker_pid, timer_pid, wallclock_tracker_pid,	watchdog_pid ],
		State ).



% Triggered just after serialisation, based on the selected entries.
%
% The value returned by this hook will be converted "as is" into a binary, that
% will be written.
%
-spec post_serialise_hook( classname(),
	   wooper_serialisation:term_serialisation(), wooper:state() ) -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	{ Classname, Entries }.



% The two hooks below are never used, as we do not deserialise time managers as
% they are, we merge them with redeployed, local ones.

% Triggered just before deserialisation.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
								wooper_serialisation:term_serialisation().
pre_deserialise_hook( _SerialisationTerm={ _Classname, Entries }, _UserData ) ->
	Entries.


% Triggered just after deserialisation.
-spec post_deserialise_hook( wooper:state() ) -> wooper:state().
post_deserialise_hook( State ) ->
	State.



% Merges specified entries coming from deserialisation into the local time
% manager.
%
-spec merge_local_with( wooper_serialisation:term_serialisation() ) -> void().
merge_local_with( SerialisedEntries ) ->

	RegistrationName = get_registration_name(),

	LocalManagerPid = naming_utils:get_registered_pid_for( RegistrationName,
														   local ),

	LocalManagerPid ! { mergeWith, [ SerialisedEntries ], self() },

	receive

		{ wooper_result, merged } ->
			ok

	end.



% Helper function to test the management of spontaneous lists:
%
% Using atoms instead of PID.
%
-spec test_spontaneous_lists() -> void().
test_spontaneous_lists() ->

	% Starts with an empty list:
	L1 = schedule_as_spontaneous_for( 5, first, [] ),
	?display_console( "Result is ~p.", [ L1 ] ),

	L2 = schedule_as_spontaneous_for( 4, second, L1 ),
	?display_console( "Result is ~p.", [ L2 ] ),

	L3 = schedule_as_spontaneous_for( 6, third, L2 ),
	?display_console( "Result is ~p.", [ L3 ] ),

	L4 = schedule_as_spontaneous_for( 5, fourth, L3 ),
	?display_console( "Result is ~p.", [ L4 ] ),

	L5 = schedule_as_spontaneous_for( 10, fifth, L4 ),
	?display_console( "Result is ~p.", [ L5 ] ),

	L6 = schedule_as_spontaneous_for( 0, sixth, L5 ),
	?display_console( "Result is ~p.", [ L6 ] ),

	% Check:
	L6 = [ { 0,  set_utils:from_list( [ sixth ] ) },
		   { 4,  set_utils:from_list( [ second ] ) },
		   { 5,  set_utils:from_list( [ fourth, first ] ) },
		   { 6,  set_utils:from_list( [ third ] ) },
		   { 10, set_utils:from_list( [ fifth ] ) } ].



% Helper function to test the minimum and maximum comparisons over timestamps.
-spec test_min_max_timestamps() -> void().
test_min_max_timestamps() ->

	Z = { 0, 0 },
	O = { 1, 1 },

	A = { 1, 0 },
	B = { 0, 1 },

	io:format( "Testing min." ),

	undefined = min_timestamp( undefined, undefined ),
	Z = min_timestamp( undefined, Z ),
	Z = min_timestamp( Z, undefined ),
	Z = min_timestamp( Z, Z ),
	Z = min_timestamp( Z, O ),
	Z = min_timestamp( O, Z ),
	Z = min_timestamp( Z, B ),
	B = min_timestamp( B, A ),
	A = min_timestamp( A, O ),

	io:format( "Testing max." ),

	undefined = max_timestamp( undefined, undefined ),
	Z = max_timestamp( undefined, Z ),
	Z = max_timestamp( Z, undefined ),
	Z = max_timestamp( Z, Z ),
	O = max_timestamp( Z, O),
	O = max_timestamp( O, Z ),
	B = max_timestamp( Z, B ),
	A = max_timestamp( B, A ),
	O = max_timestamp( O, A ).



% Performs some house-keeping, to enhance the mode of operation of this manager.
perform_house_keeping( State ) ->

	% Not much to be done here currently.

	% In no way necessary, but maybe useful:
	erlang:garbage_collect(),

	State.



% Returns the highest acceptable idle duration, in milliseconds, before deciding
% a diasca is lasting, for the first time, for too long and that a notification
% about a (first) simulation stall shall be send to the user.
%
-spec get_max_inter_diasca_duration_until_first_stall_detected() ->
													milliseconds().


% Returns the highest acceptable idle duration, in milliseconds, before deciding
% a diasca is lasting for too long whereas a first simulation stall has already
% been detected, and that new notifications about the simulation stall shall be
% send to the user.
%
-spec get_max_inter_diasca_duration_until_next_stall_detected() ->
													milliseconds().


% Returns the highest acceptable idle duration, in milliseconds, before deciding
% a diasca is lasting for too long and that the simulation shall be stopped on
% failure.
%
-spec get_max_inter_diasca_duration_until_failure_triggered() ->
													milliseconds().


% Returns the highest acceptable shutdown duration, in milliseconds, for child
% time managers.
%
-spec get_maximum_teardown_duration() -> milliseconds().



-ifdef(exec_target_is_production).

% In production mode here:

get_max_inter_diasca_duration_until_first_stall_detected() ->
	% 2 minutes ("early") before a first stall message is issued:
	2 * 60 * 1000.

get_max_inter_diasca_duration_until_next_stall_detected() ->
	% 8 minutes (relaxed) between next stall messages:
	8 * 60 * 1000.


get_max_inter_diasca_duration_until_failure_triggered() ->

	?display_console( "(in production mode, thus ~w will be using "
		"extended time-outs for simulation progress)~n", [ self() ] ),

	% Up to 12 hours for one single diasca (!) should be enough in general:

	% (note: such a huge duration might actually be useful, notably for
	% large-scale executions of instances whose first tick (tick offset #1)
	% involves a significantly lengthy initialization; we have seen a diasca of
	% this tick last for more than 4 hours on an HPC cluster)
	%
	12 * 60 * 60 * 1000.


get_maximum_teardown_duration() ->
	% 2 minutes is already quite a lot:
	2 * 60 * 1000.



-else. % exec_target_is_production


% In development mode here:

get_max_inter_diasca_duration_until_first_stall_detected() ->
	% 15 seconds ("early") before a first stall message is issued:
	15 * 1000.

get_max_inter_diasca_duration_until_next_stall_detected() ->
	% 1 minute between next stall messages may be quite tight, but it helps the
	% debugging:
	%
	60 * 1000.


get_max_inter_diasca_duration_until_failure_triggered() ->
	% 2 hours were already quite comfortable, yet did not suffice on all cases
	% (so, now: 12 hours):
	%
	12 * 60 * 60 * 1000.


get_maximum_teardown_duration() ->
	% 30 seconds is a lot:
	30 * 1000.


-endif. % exec_target_is_production
