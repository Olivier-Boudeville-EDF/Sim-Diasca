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


% @doc This is the mother class of <b>all actors</b>.
-module(class_Actor).


-define( class_description,
		 "Actor base class. "
		 "All elements of a simulation having to exchange messages with others "
		 "and/or having a spontaneous behaviour to develop should be instances "
		 "of this class (directly or not), as they need to be tightly "
		 "scheduled by a TimeManager. "
		 "See class_Actor_test.erl." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).

-type tick_offset() :: class_TimeManager:tick_offset().

-type tick_duration() :: class_TimeManager:tick_duration().
% A duration expressed as a difference of ticks.


% Shorthands:

-type ustring() :: text_utils:ustring().
-type title() :: text_utils:title().
-type label() :: text_utils:label().

-type timestamp() :: time_utils:timestamp().

-type any_seconds() :: unit_utils:any_seconds().
-type milliseconds() :: unit_utils:milliseconds().

-type logical_timestamp() :: class_TimeManager:logical_timestamp().
-type virtual_seconds() :: class_TimeManager:virtual_seconds().
-type tick() :: class_TimeManager:tick().
-type diasca() :: class_TimeManager:diasca().

-type key() :: class_DataExchanger:key().
-type value() :: class_DataExchanger:value().
-type qualified_value() :: class_DataExchanger:qualified_value().
-type qualifier() :: class_DataExchanger:qualifier().



% The attributes that are specific to an actor instance are:
-define( class_attributes, [

	{ actor_abstract_id, aai(), "the actor abstract identifier (AAI) of this "
	  "actor, as assigned by the load balancer" },

	{ load_balancer_pid, load_balancer_pid(), "the PID of the load balancer" },

	{ initial_tick, tick(),
	  "the (absolute) simulation tick at which the simulation began" },

	{ actor_creation_tick_offset, tick_offset(),
	  "the offset between the beginning of the simulation (initial_tick) and "
	  "the tick at which the actor was created; it will be set once that actor "
	  "is synchronized (when entering the simulation)" },

	{ waited_acks, [ actor_pid() ], "a list of the PID of the actors to which "
	  "this actor sent an actor message this diasca; it allows it to notify "
	  "adequately the time manager that its diasca is finished indeed" },

	{ previous_schedule, maybe( logical_timestamp() ),
	  "the logical timestamp of the latest scheduling (spontaneous or "
	  "triggered) of this actor (only used for scheduling checking, not "
	  "strictly necessary)" },

	{ added_spontaneous_ticks, [ tick_offset() ], "a list of the future tick "
	  "offsets (if any) at which this actor requests to develop additional "
	  "spontaneous actions; a tick might be declared multiple times by an "
	  "actor; it will then be scheduled only once" },

	{ withdrawn_spontaneous_ticks, [ tick_offset() ],
	  "a list of the future tick offsets (if any) that this actor requested "
	  "in the past, but wants to withdraw now; withdrawing a non-declared "
	  "spontaneous tick is not allowed and will be detected" },

	{ next_action, next_actor_action(), "describes the next action that this "
	  "actor requests (no diasca or new diasca, continuing, terminating or "
	  "terminated)" },

	{ expected_next_spontaneous_tick, tick_offset(), "the tick offset at which "
	  "this actor expects to be scheduled next for a spontaneous behaviour "
	  "(only used for scheduling checking, no strictly necessary)" },

	{ current_agenda, [ tick_offset() ], "the ordered list of tick offsets at "
	  "which this actor expects to develop a spontaneous behaviour (only used "
	  "for scheduling checking, not strictly necessary)" },

	{ last_sent_schedule_trigger, maybe( logical_timestamp() ),
	  "records whether this actor has already sent a schedule_trigger message "
	  "to its time manager, specifying timestamp {T,D} (corresponding to the "
	  "expected processing timestamp of the message, hence for its next "
	  "scheduling); this allows to avoid that an actor having received a large "
	  "number of actor messages sends consequently a large number of (local) "
	  "messages to its time manager for the same timestamp, and also spares "
	  "the need for the time manager to ensure that this actor is registered "
	  "up to once for any diasca" },

	{ pending_messages, [ actor_message() ], "the list of actor messages that "
	  "this actor will have to reorder and then to process on the next tick; "
	  "the list is initially built on receiving order (for what it is worth)" },

	{ current_tick_offset, tick_offset(), "the current tick offset of this "
	  "actor (actually this attribute is inherited from class_TraceEmitter)" },

	{ current_diasca, diasca(), "the current diasca of this actor" },

	{ exchange_settings, class_DataExchanger:exchange_settings(),
	  "an opaque term used to enable an actor making use of the data-exchange "
	  "service" },

	{ time_manager_pid, time_manager_pid(),
	  "the PID of the time manager that drives this actor directly" },

	{ random_seed, random_utils:seed(),
	  "the seed of this actor, as assigned by the time manager" },

	{ message_ordering_mode, message_ordering_mode(),
	  "dictates the reordering of messages that this actor is to perform" },

	{ simulation_tick_duration, virtual_seconds(),
	  "the actual duration, in floating-point seconds (in virtual time), "
	  "between two simulation ticks" } ] ).


-type actor_pid() :: pid().
% PID of an actor.


-type sending_actor_pid() :: actor_pid().
% PID of an actor having sent a message to the current actor.


-type created_actor_pid() :: actor_pid().
% PID of a just created actor.


-type actor_count() :: basic_utils:count().
% A number of actors.


-type diasca_count() :: basic_utils:count().
% A number of diascas (counting them generally leads to bad practices, as they
% account for the sorting out of causality - they do not represent durations).


% For #actor_settings:
-include("class_LoadBalancer.hrl").


% For time_manager_name, virtual_seconds, etc.:
-include("class_TimeManager.hrl").


% For actor_info() type:
-include("class_InstanceTracker.hrl").


-type actor_settings() :: #actor_settings{}.
% The record is defined in class_LoadBalancer.hrl:


-type actor_classname() :: wooper:classname().
% To designate more specialised classnames afterwards.


-type actor_oneway_return() :: wooper:state().
% Return types for the specification of non-const actor oneways.


-type const_actor_oneway_return() :: wooper:state().
% Return types for the specification of const actor oneways.


% Re-exporting some types from class_TimeManager, for a better model-level
% accessibility:
%
-export_type([ aai/0, name/0, internal_name/0,
			   next_actor_action/0, next_reported_action/0,
			   agenda/0, tick_offset/0, tick_duration/0,
			   logical_timestamp/0, virtual_seconds/0, tick/0, diasca/0,
			   actor_oneway_name/0, actor_oneway_argument/0,
			   actor_pid/0, sending_actor_pid/0, created_actor_pid/0,
			   actor_count/0, diasca_count/0,
			   actor_message/0, tag/0, instance_creation_spec/0,
			   message_ordering_mode/0, actor_settings/0,
			   actor_classname/0,
			   actor_oneway_return/0, const_actor_oneway_return/0 ]).


% Exported helpers:
-export([ get_abstract_identifier/1, get_deployed_root_directory/1,

		  % Time-related operations:
		  convert_seconds_to_ticks/2, convert_seconds_to_ticks/3,

		  convert_seconds_to_ticks_explicit/2,
		  convert_seconds_to_ticks_explicit/3,

		  convert_seconds_to_non_null_ticks/2,
		  convert_seconds_to_non_null_ticks/3,

		  convert_ticks_to_seconds/2, convert_ticks_to_seconds_explicit/2,

		  get_current_tick/1, get_current_tick_offset/1,
		  get_current_diasca/1, get_current_logical_timestamp/1,
		  get_current_timestamp/1, get_current_timestamp_as_string/1,
		  convert_absolute_tick_to_tick_offset/2,
		  convert_timestamp_to_tick_offset/2,

		  convert_tick_offset_to_timestamp/2,
		  convert_tick_offset_to_timestamp_as_string/2,

		  convert_tick_offset_to_timestamp_explicit/3,
		  convert_tick_offset_to_timestamp_as_string_explicit/3,


		  send_actor_message/3, send_actor_messages/3,

		  execute_actor_oneway/2, execute_actor_oneway/3,
		  execute_actor_oneway_as/3, execute_actor_oneway_as/4,

		  self_trigger_actor_message_in/3,


		  add_spontaneous_tick/2, add_spontaneous_ticks/2,
		  add_spontaneous_tick_in/2,
		  add_spontaneous_timestamp/2,

		  withdraw_spontaneous_tick/2, withdraw_spontaneous_ticks/2,

		  declare_termination/2, get_name/1, is_running/1,
		  create_actor/3, create_actor/4,
		  create_placed_actor/4, create_placed_actor/5,
		  create_actors/2,
		  declare_probe/6, declare_probe/7, declare_probe/8 ]).


-export([ enable_data_exchange/1,
		  define_data/2, define_data/3, define_data/4,
		  modify_data/2, modify_data/3, modify_data/4,
		  read_data/2, read_qualified_data/2 ]).


% Exported for specialised actors (ex: broadcasting ones):
-export([ check_spontaneous_tick_consistency/2,
		  check_diasca_consistency/3,
		  validate_scheduling_outcome/1,
		  process_last_diasca_messages/3,
		  update_agenda_with/3 ]).


% For silencing conditionally-unused functions:
-compile({ nowarn_unused_function, [ get_trace_timestamp/3,
			check_termination_time_consistency/3, check_future_messages/3 ] }).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor" ).



% For WOOPER, actor types, etc.:
-include_lib("wooper/include/wooper.hrl").


% A base used for cases as well:
-include("sim_diasca_base.hrl").



-type aai() :: non_neg_integer().
% Abstract Actor Identifier (AAI).
%
% Live actors have non-null AAI (starting at 1, the AAI of the load balancer); a
% null AAI designates a "zombi" actor, i.e. information about an actor that does
% not exist anymore.


-type name() :: class_TraceEmitter:emitter_init().
% Name of an actor, as supplied by the user:
%
% This is either a (plain or binary) string, or a pair made of two (plain or
% binary) strings, respectively the emitter name and categorization.
%
% Once fully constructed, both the name and categorization are binary strings.


-type internal_name() :: binary().
% Name of an actor, as stored internally.


-type termination_delay() :: diasca() | 'unlimited'.
% Specifies how an actor organises its termination process, from its declaration
% to its realisation: either a positive count of diascas (possibly zero) to
% wait, or 'unlimited', so that the actor is deleted only at the next tick.
%
% In most cases, the default, 'unlimited', is the best setting.


-type diasca_maybe() :: 'no_diasca_requested' | 'new_diasca_needed'.
% Tells whether a next diasca is requested (shorthand for clarity).


-type next_actor_action() ::
		diasca_maybe()
	  | { 'terminating', termination_delay(), diasca_maybe() }
	  | { 'terminated', diasca_maybe() }.
% Used by actors internally, to record the new next action that they plan (in
% their next_action attribute); this can be:
%
% - in a normal, ongoing mode of operation: either nothing, or needing a new
% diasca (either to process an actor message or to go through its termination
% procedure)
%
% - or when having decided to terminate (either being in the course of, or
% considering having terminated for good), in both cases with or without having
% sent at least one actor message (hence in that case needing a new diasca)



-type next_reported_action() ::

		diasca_maybe()

		% No need for diasca_maybe(), as implies new_diasca_needed:
	  | 'terminating'

	  | { 'terminating_unlimited', diasca_maybe() }
	  | { 'terminated', diasca_maybe() }.
% Used by actors in their dialog with their time manager, to notify it of the
% next action they plan: respectively, either nothing, or needing a new diasca
% to manage messages, or terminating with intermediary diascas, or terminating
% with no specific limit in terms of diascas, or having terminated for good.
%
% (difference with next_actor_action/0: the time manager does not have to know
% how many diascas, if any, remain until a planned termination)


-type agenda() :: [ tick_offset() ].
% Ordered list of tick offsets at which this actor expects to develop a
% spontaneous behaviour.


-type actor_oneway_name() :: oneway_name().

-type actor_oneway_argument() :: any().



-record( actor_message, {

		% The tick offset at which this actor message has been sent and is to be
		% processed:
		%
		tick_offset :: tick_offset(),

		% The target diasca at which this actor message is to be processed (the
		% next diasca after the one of the sending):
		%
		diasca :: diasca(),

		% The PID of the sending actor:
		sender_pid :: actor_pid(),

		% The AAI of the sending actor:
		sender_aai :: aai(),

		% The actual actor message, which is a oneway call:
		actual_message :: oneway_call() } ).


-type actor_message() :: #actor_message{}.
% Describes a pending actor message, as stored by an actor once having received
% it.


-type instance_creation_spec() :: { classname(), [ method_argument() ] }
  | { classname(), [ method_argument() ], class_LoadBalancer:placement_hint() }.
% Describes the information needed in order to create (without tag) an actor:


-type tag() :: any().
% A tag is a means of identifying the origin of a call on the client side
% (caller), i.e. to identify to which initial oneway an answer corresponds (can
% be freely defined by the user).


-type tagged_instance_creation_spec() ::
		{ classname(), [ method_argument() ], tag() }
	  | { classname(), [ method_argument() ], tag(),
		  class_LoadBalancer:placement_hint() }.
% Describes the information needed in order to create, thanks to a tag, an
% actor.



-type message_ordering_mode() :: 'unordered'
							   | 'constant_arbitrary_order'
							   | 'constant_permuted_order'.
% Tells what kind of reordering of actor messages is wanted, among:
%
% - 'unordered': the actor processes messages in their arrival order; smallest
% possible overhead, but the order is dictated by the technical context, and is
% not reproducible
%
% - 'constant_arbitrary_order': messages are ordered only according to their
% content and their sender; this order is totally reproducible, but will only
% exhibit one of the many possible trajectories of the simulated system
%
% - 'constant_permuted_order': messages are first reordered like in the case of
% 'constant_arbitrary_order', then they are uniformly permuted based on the
% random seed the actor received on creation, either coming from a reproducible
% or an ergodic context; all possible trajectories have then a fair probability
% of being chosen by this more advanced reordering
%
% Note: the overhead induced by this last, more advanced order is sufficiently
% low that it is used as the default order.



% Implementation notes:

% Note that an update to this class may need to be applied also in the
% class_BroadcastingActor one (especially if it deals with the waited_acks
% attribute).


% About stochastic management.

% The initial implementation for stochastic actors was complex and added some
% constraints; a fairly complex system where actors could wait for other actors
% and be waited by others had to be added then.
%
% The newer implementation for stochastic actors is better in all aspects, and
% does not need the waiting mechanism any more.
%
% Therefore the mechanism to manage wait graphs is not used anymore, and
% class_Actor is sufficient in all cases (no real reason to rely on
% class_StochasticActor anymore): refer to the static methods defined by
% class_RandomManager (preferably to using directly random_utils - although it
% is fully supported and does not jeopardise the simulation properties such as
% reproducibility) to generate the random values that a model needs.



% About timing specifications.

% In terms of timings, a simulation starts at an initial tick which is known of
% all schedule-related agents. Therefore only offsets relative to the beginning
% of the simulation time are used, not absolute ticks (that start at year #0 of
% our Gregorian, usual calendar), which would be far larger and would probably
% exceed the native integer type, therefore wasting resources with big integers.
%
% The actor class inherits from the trace emitter class (which itself must
% maintain the emitter time in order to timestamp traces), and the two
% attributes of the latter, 'initial_tick' and 'current_tick_offset',
% correspond, in the context of an actor, respectively to the initial tick of
% the simulation (thus this same value is shared by all actors) and the offset
% of the current tick of this actor relatively to this initial simulation tick.
%
% A new attribute is introduced for actors, 'actor_creation_tick_offset', which
% records when the actor is created, relatively to the beginning of the
% simulation (hence relatively to 'initial_tick'). This allows for example to
% keep track of the age of that actor. Therefore
% class_TraceEmitter:get_current_tick/1 (which returns
% initial_tick+current_tick_offset) could be used directly by actors. However
% the best practise is surely to call class_Actor:get_current_tick/1 or, still
% better, class_Actor:get_current_tick_offset/1 since usually offsets are more
% appropriate than absolute ticks.


% Note that a (most probably small) speed-up could be obtained by not performing
% further permutations in reproducible mode with a default seed: the basic,
% default arbitrary reordering is as good as one that is additionally uniformly
% permuted.


% Some attributes (namely: 'previous_schedule',
% 'expected_next_spontaneous_tick', 'current_agenda') are used only for checking
% purposes: they allow to verify whether the actual scheduling, as decided by
% the time manager, is indeed compliant with what could be forecast from the
% point of view of each actor.

% As a consequence, in release (non-debug) mode they could be removed as a
% whole, with no harm, with a certain resulting speed gain.



% About scheduling.

% An actor may not be scheduled at all during a tick (which itself may or may
% not be scheduled), or the actor may be scheduled, for a spontaneous action
% (then at diasca 0) and/or for any number of times to:
%
%  - perform triggered actions (then at strictly positive diascas), each time an
%  actor message was sent to it on the previous diasca
%
%  - or possibly to terminate.
%
% Once an actor finishes its scheduling, it notifies its time manager both of
% the additional ticks on which it is to perform spontaneous actions (if any)
% and of the ticks for which it had already declared such actions but wants now
% to withdraw (if any).
%
% The withdrawal of ticks is applied before the addition, so that it is not
% possible to add and remove the same tick in the same operation.


% About parallelism.

% We try to offset as much as possible processings so that they happen once the
% actor answered to the time manager (and at the level of the actor rather at
% the one of its time manager).


% About actor creation.

% When an actor is created (either initially, i.e. from the simulation case,
% direectly or not, or from another actor, i.e. in the course of the
% simulation), the actual spawn triggers its constructor that in turn will
% trigger the subscription of this actor. If the simulation is already running,
% it will result into the execution of the simulationStarted/3 engine-level
% request (otherwise it will happen right at the future simulation start).
%
% If the simulation is already running, we want to avoid simply scheduling first
% this newly created actor thanks to its actSpontaneous/1 actor oneway, as it
% would have then to wait for the next tick: creation chains would thus suffer
% from a potentially problematic engine-induced latency, which is not wanted.
%
% So we decided that when the request for an actor creation is made at diasca D
% (in the course of the simulation), not only the actual creation (spawn and
% synchronisation, done by the load balancer) will happen at D+1 and generally
% (not always, as for example a created actor may in turn create other actors)
% the *creator* will be notified of it at D+2 (by an onActorCreated/4 call), but
% also the created actor will also be triggered, at this already scheduled
% diasca (D+2 in many cases), by an onFirstDiasca/2 actor oneway call. This way,
% this actor may decide to send actor messages and/or define its next
% spontaneous ticks and/or create other actor(s), allowing for instantaneous
% chained creations.
%
% One may note that nested creations may happen, either initial or at runtime;
% in the former case, creations are instantaneous, while, on the latter case,
% the deeper the nesting, the more diascas will be needed until all actors are
% ready.
%
% If the simulation was not running yet, this is an initial creation and the
% onFirstDiasca/2 actor oneway will be triggered as soon as possible, i.e. at
% diasca 1 of the initial tick (thus once the simulation will be started). Of
% course no onActorCreated/4 actor oneway call will be made, as there is not
% even a creating actor in this case.
%
% We considered the possibility of introducing a unique primitive for actor
% creation that would cover both cases (creations that would be initial or not);
% however we deemed that their respective natures are too different to be
% transparently hidden (ex: done from any process/only from an actor,
% instantaneous/deferred (returning directly a PID/updating a state and
% triggering a later callback call), etc.; at worst, an actor may determine
% whether the simulation is already started (see is_running/1) or not, and then
% opt for a variation either of create_actor or create_initial_actor.



% About creation tags.

% At runtime (i.e. when the simulation is running), an actor may create multiple
% actors, at the same diasca or not. A means must be provided so that this
% creating actor is able to establish to which creation request a given
% 'onActorCreated' callback message corresponds (as the creation is done through
% an actor oneway, thus with no result possibly returned).
%
% This is done thanks to a creator-side tag, which is sent to the load-balancer
% and that this balancer will send back in turn to the creator in the context of
% the 'onActorCreated' notification.
%
% This tag is either user-defined (see create_actor/4 and create_placed_actor/5)
% or not (see create_actor/3 and and create_placed_actor/4) - in which case it
% will be chosen equal to a pair made of the classname and of the construction
% parameters (implementation detail: a non-tag version of the
% createRuntimeActor{4,5} functions has been defined in order to avoid sending
% in that case twice the classname and parameters to the load balancer; this
% default tag can be then directly determined by the load balancer).
%
% Specifying a tag may simplify the development of the creating model and also
% may avoid the (potentially large) construction parameters to be sent back to
% the creator by the load-balancer.
%
% Another option could have been to store the construction pair and send a
% creator-maintained counter. Then the pair would not have to be sent back, yet
% it still would have to be kept in the creator and sent at the first place to
% the load balancer of course (it would have therefore little interest).


% About actor synchronization.

% In a few words: whether or not the simulation is already started, on actor
% creation the time manager calls the simulationStarted/3 request on the actor
% for pure engine-level synchronisation, while the load-balancer sends to the
% actor the onFirstDiasca/2 actor oneway for pure model-level initialization /
% synchronisation.
%
% More precisely: created actors have to initialize themselves, and not all
% relevant information are always available at their creation (ex: simulation
% starting tick). For any late, model-specific, set-up, knowing that their
% simulationStarted/3 method is not meant to be overridden (it is used privately
% by the engine, notably to tell the actor about the simulation initial tick and
% the current timestamp), their onFirstDiasca/2 actor oneway (which is not used
% at all internally by the engine) is called as soon as they are synchronised
% with the simulation. They can then initialize themselves at will, with all
% necessary information. As this is an actor oneway, it must be called by
% another actor, and the load balancer is the most suitable for that.
%
% So, if we are performing an initial creation, the actor will be created, but
% not scheduled, as long as the simulation is not started; when this happens:
%
% - its simulationStarted/3 request is first called (transparently), by its time
% manager
%
% - then its possibly overridden onFirstDiasca/2 actor oneway is called (by the
% load balancer), through the usual process of inter-actor exchanges
%
% Otherwise, the actor is created in the course of the simulation, and then,
% quite similarly:
%
% - it calls by itself its simulationStarted/3 request, as a part of its
% construction phase (just having subscribed to its time manager)
%
% - then its possibly overridden onFirstDiasca/2 actor oneway is called (by the
% load balancer), through the usual process of inter-actor exchanges
%
% As a result, an actor may compute durations from this constructor (as calling
% the class_Actor constructor will take care of processing the actor settings),
% yet may access to the current time and/or the simulation start time only
% starting from its onFirstDiasca/2 oneway.


% About actor termination.

% The termination of an actor is solely decided by itself: an actor cannot
% trigger directly the deletion of another, as in the general case a termination
% involves an actor-specified coordinated teardown phase. For example, the actor
% might have to notify other actors that it is to terminate so that they "forget
% it", i.e. so that they know that they should not send actor messages to it
% anymore (lest the simulation is blocked).
%
% Termination is a three-step process, that we want, similarly to creations, to
% happen possibly during the same tick (thus leading to intermediary diascas
% being needed):
%
%  1. an actor declares at a given diasca Dt that, based on its own logic, it
%  now intends to terminate (see declareTermination/{1,2}), possibly specifying
%  the number of diascas it requests to elapse before its actual termination can
%  occur (otherwise it may decide to terminate immediately or only later, at the
%  end of the current tick, once no more diasca will be instantiated for that
%  tick); this may be of use for example when a notification of termination is
%  to propagate first into a chain (of undetermined length) of other actors
%  (which otherwise could keep on sending actor messages to this actor).

%  So this duration (in logical steps) before the termination can happen is
%  either a positive count of intermediary diascas to wait (possibly 0) or the
%  'unlimited' atom, in which case the termination is to occur no sooner than
%  the next tick, regardless of the number of diascas that happen to be needed
%  in-between (this is useful when not needing to assess that a termination is
%  to happen in a bounded duration in diascas, which is the general case); not
%  specifying a duration (see declareTermination/1) defaults to 'unlimited'
%
%  As a consequence, the next_action of this actor becomes 'terminated', if D=0,
%  or { terminating, D } if D is either a strictly positive number of diascas or
%  is the 'unlimited' atom.
%
%  Independently, that actor may or may not send an actor message in the current
%  diasca, which is covered by the diasca_maybe() element.
%
%  2. at the end of the diasca Dt (see notify_diasca_ended/1), if:
%
%   - D is 'unlimited', then the actor will stay in the 'terminating' state
%   until the end of the current tick (regardless of the number of diascas), and
%   then will be immediately deleted; in-between, the actor will not be
%   specifically scheduled anymore
%
%   - D is a strictly positive integer; then the actor stays in 'terminating'
%   state, D is decremented and a new diasca is requested, at which the actor
%   will be scheduled again (only to manage its termination phase)

%   - D is zero, then the actor goes in 'terminated' state, will not be
%   scheduled anymore, and the time manager will be allowed to delete it from
%   the next diasca; in practise the actual deletion will generally be deferred
%   to the next tick, so that detecting unexpected interactions with this
%   terminated actor is easier (unlike the D='unlimited' case, here the actor is
%   already in the 'terminated' case)
%
%  3. the actor is deleted for good by the time manager
%
% Note that, a priori, if no actor message is sent anymore, the tick could be
% ended directly, regardless of any remaining termination diasca (no need to
% wait for them).



% About serialisation.
%
% No corner case, we use the default WOOPER do-nothing hooks for the
% WOOPER-provided serialise/3 request, as each model instance is serialised
% properly thanks to the default mechanisms.


% About datastructures.
%
% Several attributes are semantically sets, but implemented as list for
% efficiency purposes.



-compile({ inline, [ get_trace_timestamp/3 ] } ).


% @doc Returns a rather detailed (hence more expensive) timestamp to be included
% in traces.
%
% Meant to be inlined as much as possible to lessen the cost of such traces.
%
-spec get_trace_timestamp( tick_offset(), diasca(), wooper:state() ) ->
			ustring().
get_trace_timestamp( TickOffset, Diasca, State ) ->

	CurrentTick = ?getAttr(initial_tick) + TickOffset,

	% Only relies on the simulation_tick_duration attribute:
	CurrentSecond = convert_ticks_to_seconds( CurrentTick, State ),

	Timestamp =
		calendar:gregorian_seconds_to_datetime( round( CurrentSecond ) ),

	% Cannot include a newline as it would break the trace format:
	text_utils:format( "~ts {~B,~B}",
		[ time_utils:get_textual_timestamp( Timestamp ), TickOffset, Diasca ] ).




% @doc Constructs a new simulation actor.
%
% Construction parameters:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - ActorInit is a human-readable name for that actor (as a plain string); it is
% preferably not too long, and without whitespaces
%
% Developer note: the engine, through child classes, may call this constructor
% not with a single name (ActorName), but with a ActorInit={ActorName,
% TraceCategorization} pair.
%
-spec construct( wooper:state(), actor_settings(), name() ) -> wooper:state().
construct( State,
		   #actor_settings{ aai=ActorAbstractIdentifier,
							seed=ActorSeed,
							message_ordering_mode=OrderingMode },
		   ActorInit ) ->

	% First the direct mother classes:
	TraceState = class_EngineBaseObject:construct( State,
										?trace_categorize(ActorInit) ),

	ActorName = case ActorInit of

		{ EmitterName, _EmitterCategorization } ->
			EmitterName;

		EmitterName ->
			EmitterName

	end,

	% Then the class-specific actions:

	% We do not keep that seed once initialised (no real interest); note that,
	% thanks to the engine-level reproducible seeding whence this seed should
	% come, no constraint on the random generation done by this actor applies
	% anymore (random_utils can then be used at will, either directly or,
	% preferably, through the static methods defined by class_RandomManager):
	%
	random_utils:start_random_source( ActorSeed ),

	%?send_info_fmt( TraceState, "Creating a new actor named '~ts' "
	%    "and bearing abstract identifier ~B.",
	%    [ ActorName, ActorAbstractIdentifier ] ),


	% The load balancer being itself an actor, by design it is not registered
	% yet, will use its reserved AAI (1) to detect and overcome that:
	%
	BalancerPid = case ActorAbstractIdentifier of

		1 ->
			self();

		_OtherAAI ->
			class_LoadBalancer:get_balancer()

	end,

	StartingState = setAttributes( TraceState, [

		{ initial_tick, undefined },

		{ actor_abstract_id, ActorAbstractIdentifier },
		{ load_balancer_pid, BalancerPid },
		{ actor_creation_tick_offset, undefined },
		{ waited_acks, [] },
		{ previous_schedule, undefined },
		{ added_spontaneous_ticks, [] },
		{ withdrawn_spontaneous_ticks, [] },
		% Set later in this constructor: {exchange_settings, undefined},
		{ next_action, no_diasca_requested },
		{ expected_next_spontaneous_tick, undefined },
		{ current_agenda, [] },
		{ last_sent_schedule_trigger, undefined },
		{ pending_messages, [] },
		{ current_tick_offset, undefined },
		{ current_diasca, undefined },
		% Set later in this constructor: {time_manager_pid, undefined},
		{ random_seed, ActorSeed },
		{ message_ordering_mode, OrderingMode }
		% Set later in this constructor:
		% {simulation_tick_duration, undefined}

												] ),

	% Find the time manager and subscribe:
	TimeManagerPid = try naming_utils:wait_for_local_registration_of(
			class_TimeManager:get_registration_name() ) of

		Pid ->
			Pid

	catch

		Exception ->

			?send_error_fmt( StartingState, "Actor constructor failed: "
				"unable to find time manager: ~w, stopping actor.",
				[ Exception ] ),

			throw( { actor_construction_failed, time_manager_not_found,
					 Exception } )

	end,

	%?send_debug( StartingState, "Time manager found, subscribing." ),

	TimeAwareState = setAttribute( StartingState, time_manager_pid,
								   TimeManagerPid ),

	BinActorName = text_utils:ensure_binary( ActorName ),

	% We want the actual class name of this actor instance, not, for example,
	% just 'class_Actor':
	%
	{ _SameState, Classname } = executeRequest( TimeAwareState, getClassname ),

	% Registers to the time manager, allows it to associate its instance and
	% class names, and AAI to its PID and get time information:
	%
	TimeManagerPid ! { subscribe,
				[ ActorAbstractIdentifier, BinActorName, Classname ], self() },

	% Note: in the future, subscription may be a single oneway call, and all
	% information (besides AAI and seeding) will be given to created actors by
	% the load-balancer.

	MaxWaitedDuration = get_maximum_subscription_duration(),

	% Hijacks the WOOPER main loop to force message selection:
	{ TickDuration, StartInformation } = receive

		{ wooper_result, { time_subscribed, TickDur, StartInfo } } ->
				 { TickDur, StartInfo };

		{ wooper_result, already_time_subscribed } ->
			?send_error( TimeAwareState, "Actor constructor failed: "
				"already subscribed to time manager, stopping actor." ),
			throw( { actor_construction_failed, already_time_subscribed } )

	after MaxWaitedDuration ->

		?send_error_fmt( TimeAwareState, "Actor constructor failed: "
			"time-out while waiting for time manager "
			"(after ~B ms, i.e. ~ts), stopping actor.",
			[ MaxWaitedDuration,
			  time_utils:duration_to_string( MaxWaitedDuration ) ] ),

		throw( { actor_construction_failed, time_subscription_timed_out } )

	end,

	TickState = setAttribute( TimeAwareState, simulation_tick_duration,
							  TickDuration ),

	case StartInformation of

		not_started_yet ->
			% Simulation not running, the simulationStarted message will be sent
			% later when appropriate:
			%
			TickState;

		{ ActualInitialTick, FirstScheduledOffset } ->
			% Created in the course of the simulation, calling
			% simulationStarted/3 atomically:
			%
			{ StartedState, { actor_started, _SelfPid } } = executeRequest(
					TickState, simulationStarted,
					[ ActualInitialTick, FirstScheduledOffset ] ),
			StartedState

	end.




% @doc Overridden destructor.
%
% Unsubscribing from the time manager supposed already done, thanks to a
% termination message.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	% Note: using the last known, possibly outdated timestamp of this actor:

	%?info( "Deleting actor." ),

	%trace_utils:debug_fmt( "Deleting actor ~w, while it was at ~p.",
	%    [ self(), get_current_logical_timestamp( State ) ] ),

	InstanceTrackerPid = class_InstanceTracker:get_local_tracker(),

	{ SameState, ActualClassname } = executeRequest( State, getClassname ),

	% Helps keeping track of current living actors:
	InstanceTrackerPid ! { unregisterActor, [ self(), ActualClassname ] },

	%?debug( "Actor deleted." ),

	% Then allow chaining:
	SameState.




% Methods section.


% Management section of the actor.


% @doc Notifies this initial actor that the simulation just started.
%
% Request called:
%
% - by the time manager when this actor enters the simulation while it was not
% running at the actor's subscription
%
% - directly by this actor itself if upon subscribing it is told that the
% simulation is already running
%
% Note: this simulationStarted/3 method belongs to the engine internals and
% should not be overridden. See the onFirstDiasca/2 actor oneway instead, whose
% sole purpose is to ease the implementation of model initialization (this
% method is a callback, intentionally left blank by the engine).
%
% Returns {started,Pid}, Pid being the PID of this actor (the atom is used as a
% security to discriminate among answers to requests).
%
-spec simulationStarted( wooper:state(), tick(), logical_timestamp() ) ->
				request_return( { 'actor_started', actor_pid() } ).
simulationStarted( State, SimulationInitialTick,
			 _CurrentTimestamp={ CurrentTickOffset, CurrentDiasca } ) ->

	%?info_fmt( "This initial actor is notified of simulation start, "
	%           "with a simulation initial tick of ~B, "
	%           "while current tick offset is #~B and diasca ~B.",
	%			[ SimulationInitialTick, CurrentTickOffset, CurrentDiasca ] ),

	%trace_utils:debug_fmt( "class_Actor:simulationStarted called for ~w, "
	%			"with simulation initial tick ~p, with current tick #~B "
	%			"and diasca ~B.",
	%			[ self(), SimulationInitialTick, CurrentTickOffset,
	%			CurrentDiasca ] ),

	wooper:check_all_undefined( [ initial_tick, current_tick_offset,
		current_diasca, actor_creation_tick_offset ], State ),

	false = is_running( State ),

	StartedState = setAttributes( State, [
		{ initial_tick, SimulationInitialTick },
		{ current_tick_offset, CurrentTickOffset },

		% Otherwise onFirstDiasca/2 will trigger some checking errors:
		{ current_diasca, CurrentDiasca },

		{ actor_creation_tick_offset, CurrentTickOffset }

		% current_agenda already set to [] for all actors except the
		% load-balancer (for bootstrapping purposes).
														] ),

	wooper:return_state_result( StartedState,
								_Result={ actor_started, self() } ).



% @doc Notifies this actor that the simulation ended.
%
% For the vast majority of actors (but unlike the load balancer for example),
% this means deletion (overridden for the load balancer, which has a different
% life cycle).
%
-spec simulationEnded( wooper:state() ) -> const_oneway_return().
simulationEnded( State ) ->

	% By default, means immediate deletion:
	self() ! delete,

	wooper:const_return().



% @doc This actor oneway is automatically called the next diasca after an actor
% is created or, if the simulation was not running, on diasca 1 (that is just
% after the spontaneous behaviours) of tick offset #0.
%
% This method is meant to be overridden, knowing that otherwise the created
% actor will be scheduled only once (this time), and never again.
%
% Its purpose is to gather one-time only operations, otherwise each call of
% actSpontaneous/1 would have to test whether it is the first one or not.
%
% A typical yet simplistic implementation can be, in order to trigger a first
% scheduling at the next tick:
%
%-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
%							actor_oneway_return().
% onFirstDiasca( State, _SendingActorPid ) ->
%	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),
%	wooper:return_state( ScheduledState ).
%
% or even, if wanting this actor to remain passive:
%
%-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
%							const_actor_oneway_return().
%onFirstDiasca( State, _SendingActorPid ) ->
%	actor:const_return().
%
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	{ _IgnoredState, ActualClassname } = executeRequest( State, getClassname ),

	% Made mandatory now (used to be a mere warning):
	?error_fmt( "The onFirstDiasca/2 actor oneway has not been overridden "
		"for class '~ts', whereas it is mandatory "
		"(even if its instances are to remain purely passive afterwards).",
		[ ActualClassname ] ),

	% Notably we could expect the following oneways (or their corresponding
	% helper functions) to be called here: scheduleNextSpontaneousTick/1,
	% addSpontaneousTick/2, addSpontaneousTicks/2, withdrawnSpontaneousTick/2,
	% withdrawnSpontaneousTicks/2, declareTermination/{1,2}, etc.

	% So the current option is simply to force the overriding of this method:
	throw( { not_overridden, { onFirstDiasca, 2 }, ActualClassname } ).

	% A last possible option would have been to have by default:
	%ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),
	%wooper:return_state( ScheduledState ).

	%wooper:const_return().



% @doc Called by the local time manager in order to schedule this actor for a
% new tick, starting with its spontaneous behaviour (diasca 0).
%
% Returns an updated state, and triggers back a notification to the
% corresponding time manager when the spontaneous action has been completed.
%
-spec beginTick( wooper:state(), tick_offset() ) -> oneway_return().
beginTick( State, NewTickOffset ) ->

	Agenda = ?getAttr(current_agenda),

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "beginTick for actor ~w at tick offset #~B, "
					"with agenda ~w.", [ self(), NewTickOffset, Agenda ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		begin
			check_spontaneous_tick_consistency( NewTickOffset, State ),
			[] = ?getAttr(added_spontaneous_ticks),
			[] = ?getAttr(withdrawn_spontaneous_ticks)
		end ),

	% Removes the first entry of this agenda, this new tick offset:
	[ NewTickOffset | NewAgenda ] = Agenda,

	% Other attributes set at the end of previous scheduling:
	PreState = setAttributes( State, [
		{ current_agenda, NewAgenda },
		{ current_tick_offset, NewTickOffset },
		{ current_diasca, 0 },

		cond_utils:if_defined( exec_target_is_production,
			{ trace_timestamp, { NewTickOffset, 0 } },
			{ trace_timestamp,
			  get_trace_timestamp( NewTickOffset, _Diasca=0, State ) } ) ] ),

	SpontaneousState = executeOneway( PreState, actSpontaneous ),

	% Note: we are not checking the correctness of the engine here, we ensure
	% that models are properly written.
	%
	cond_utils:if_defined( simdiasca_check_model_behaviours,
						   validate_scheduling_outcome( SpontaneousState ) ),

	% The 'actSpontaneous' method might have sent actor messages:
	AckState = case getAttribute( SpontaneousState, waited_acks ) of

		[] ->
			notify_diasca_ended( SpontaneousState );

		_StillWaiting ->
			% End of tick to be determined by acknowledgeMessage/2:
			SpontaneousState

	end,

	wooper:return_state( AckState ).



% @doc Performs checks, in the case of the beginning of a new tick.
check_spontaneous_tick_consistency( NewTickOffset, State ) ->

	% First, time must always progress:
	check_time_consistency_at_new_tick( NewTickOffset, State ),

	% Checks that actor messages are within possible past and future bounds:
	check_messages_at_new_tick( NewTickOffset, State ),

	% Then, this tick must be in the agenda, and be the first:
	check_spontaneous_indeed( NewTickOffset, State ).



% @doc Checks that this scheduling returned a sensible result.
validate_scheduling_outcome( State ) ->

	AddedTicks = ?getAttr(added_spontaneous_ticks),
	WithdrawnTicks = ?getAttr(withdrawn_spontaneous_ticks),

	case ?getAttr(next_action) of

		no_diasca_requested ->
			validate_new_ticks( AddedTicks, WithdrawnTicks,
								?getAttr(current_tick_offset) );


		new_diasca_needed ->
			validate_new_ticks( AddedTicks, WithdrawnTicks,
								?getAttr(current_tick_offset) );


		{ terminating, _Delay, _DiascaMaybe } ->

			case AddedTicks of

				[] ->
					ok;

				_ ->
					throw( { added_ticks_while_terminating, AddedTicks } )

			end,

			case WithdrawnTicks of

				[] ->
					ok;

				_ ->
					% Might be acceptable, though.
					throw( { withdrawn_ticks_while_terminating,
							 WithdrawnTicks } )

			end;


		{ terminated, _DiascaMaybe } ->

			case AddedTicks of

				[] ->
					ok;

				_ ->
					throw( { added_ticks_while_terminated, AddedTicks } )

			end,

			case WithdrawnTicks of

				[] ->
					ok;

				_ ->
					throw( { withdrawn_ticks_while_terminated,
							 WithdrawnTicks } )

			end

	end.



% @doc Ensures that all specified ticks are in the future and that none is added
% and withdrawn at the same time.
%
validate_new_ticks( AddedTicks, WithdrawnTicks, CurrentTickOffset ) ->

	case AddedTicks of

		[] ->
			ok;

		_ ->
			case lists:min( AddedTicks ) > CurrentTickOffset of

				true ->
					ok;

				false ->
					FaultyAddedTicks =
						[ T || T <- AddedTicks, T =< CurrentTickOffset ],
					throw( { added_spontaneous_ticks_must_be_in_future,
							 FaultyAddedTicks, CurrentTickOffset } )

			end

	end,

	case WithdrawnTicks of

		[] ->
			ok;

		_ ->
			case lists:min( WithdrawnTicks ) > CurrentTickOffset of

				true ->
					ok;

				false ->
					FaultyWithdrawnTicks =
						[ T || T <- WithdrawnTicks, T =< CurrentTickOffset ],
					throw( { withdrawn_spontaneous_ticks_must_be_in_future,
							 FaultyWithdrawnTicks, CurrentTickOffset } )

			end

	end,

	case list_utils:intersection( AddedTicks, WithdrawnTicks ) of

		[] ->
			ok;

		Intersection ->
			throw( { no_withdrawal_of_added_spontaneous_ticks,
					 AddedTicks, WithdrawnTicks, Intersection } )

	end.



% @doc Called by the local time manager in order to schedule this actor for a
% new non-null diasca, after its spontaneous behaviour.
%
% Returns an updated state, and triggers back a notification to the
% corresponding time manager when the triggered actions have been completed.
%
-spec beginDiasca( wooper:state(), tick_offset(), diasca() ) -> oneway_return().
beginDiasca( State, TickOffset, NewDiasca ) ->

	cond_utils:if_defined( simdiasca_debug_time_management,
		?debug_fmt( "beginDiasca for ~w at diasca ~B of tick offset #~B.",
					[ self(), NewDiasca, TickOffset ] ) ),

	cond_utils:if_defined( simdiasca_check_time_management,
		check_diasca_consistency( TickOffset, NewDiasca, State ) ),

	% Other attributes set at the end of previous scheduling:
	PreState = setAttributes( State, [

		% This is not superfluous, as we might have received an actor message
		% while being still lagging in a past tick:
		%
		{ current_tick_offset, TickOffset },

		{ current_diasca, NewDiasca },

		cond_utils:if_defined( exec_target_is_production,
			{ trace_timestamp, { TickOffset, 0 } },
			{ trace_timestamp,
			  get_trace_timestamp( TickOffset, NewDiasca, State ) } ) ] ),

	TriggerState =
		process_last_diasca_messages( TickOffset, NewDiasca, PreState ),

	% Note: we are not checking the correctness of the engine here, we ensure
	% that models are properly written.
	%
	cond_utils:if_defined( simdiasca_check_model_behaviours,
						   validate_scheduling_outcome( TriggerState ) ),

	% The triggered methods might have sent actor messages:
	AckState = case getAttribute( TriggerState, waited_acks ) of

		[] ->
			notify_diasca_ended( TriggerState );

		_StillWaiting ->
			% End of diasca to be determined by acknowledgeMessage/2:
			TriggerState

	end,

	wooper:return_state( AckState ).



% @doc Checks diasca consistency, when an actor begins a new diasca.
%
% (optional checkings)
%
check_diasca_consistency( TickOffset, NewDiasca, State ) ->

	check_trigger_tick_consistency( TickOffset, NewDiasca, State ),

	% We cannot enfore these constraints, as an otherwise idle actor might be
	% awoken in any late future by the receiving of an actor message:
	% (only possible checkings already done in check_trigger_tick_consistency/3.

	%TickOffset = ?getAttr(current_tick_offset),
	%NewDiasca = ?getAttr(current_diasca) + 1,

	[] = ?getAttr(added_spontaneous_ticks),
	[] = ?getAttr(withdrawn_spontaneous_ticks),

	% If we are scheduling a non-zero diasca, it must mean that either we have
	% at least an actor message to process and/or that we are terminating:
	%
	case ?getAttr(pending_messages) of

		[] ->

			% No actor message received, hence can only be scheduled as having
			% requested it:
			%
			case ?getAttr(next_action) of

				new_diasca_needed ->
					ok;

				% Unlimited or not;
				{ terminating, _Delay, new_diasca_needed } ->
					ok;

				% Implicitly no_diasca_requested:
				{ terminating, DiascaCount, no_diasca_requested }
				  when is_integer( DiascaCount ) ->
					ok;

				{ terminated, new_diasca_needed } ->
					ok;

				no_diasca_requested ->
					throw( unexpected_diasca_scheduling );

				UnexpectedAction ->
					throw( { unexpected_diasca_scheduling, UnexpectedAction } )

			end;

		_ ->

			% Receiving a message masks any local planification, as a result no
			% specified check can be performed here:
			%
			ok

	end.



% @doc Performs check, in the case of a new, triggered (i.e. non-spontaneous)
% diasca (D>0).
%
check_trigger_tick_consistency( TickOffset, NewDiasca, State ) ->

	% First, time must always progress:
	check_time_consistency_at_new_diasca( TickOffset, NewDiasca, State ),

	% Checks that actor messages are within possible past and future bounds:
	check_messages_at_new_diasca( TickOffset, NewDiasca, State ),

	% Finally, at least one actor message (for that diasca of that tick) must be
	% available:
	%
	check_triggered_indeed( TickOffset, NewDiasca, State ).



% @doc Helps checking whether the actor is scheduled for a new diasca as
% expected, by ensuring that the causality always progresses.
%
check_time_consistency_at_new_diasca( TickOffset, NewDiasca, State ) ->

	false = ( NewDiasca < 1 ),

	% A possible case is that simply this actor receives an actor message sent
	% at diasca 0 by another actor which was spontaneously scheduled, whereas
	% this actor was not.
	%
	% Hence the only checking we can do is to ensure that the previous schedule
	% is strictly in the past:
	%
	case ?getAttr(previous_schedule) of

		{ PreviousTickOffset, _AnyDiasca }
				when PreviousTickOffset < TickOffset ->
			ok;

		% Same tick:
		{ TickOffset, PreviousDiasca } when PreviousDiasca < NewDiasca ->
			ok;

		undefined ->
			% When an actor is just created, it shows up directly at a non-zero
			% diasca:
			%
			ok;

		% Same diasca:
		{ TickOffset, NewDiasca } ->
			throw( { duplicated_diasca, NewDiasca, TickOffset } );

		{ TickOffset, WrongDiasca } ->
			throw( { wrong_diasca, WrongDiasca, NewDiasca, TickOffset } );

		{ WrongTickOffset, NewDiasca } ->
			throw( { wrong_offset, WrongTickOffset, TickOffset, NewDiasca } );

		WrongTimestamp ->
			throw( { wrong_timestamp, WrongTimestamp,
						{ TickOffset, NewDiasca } } )

	end.



% @doc Called by the time manager in order to schedule this actor a last time,
% so that it can safely terminate.
%
-spec beginTerminationDiasca( wooper:state(), tick_offset(), diasca() ) ->
									const_oneway_return().
beginTerminationDiasca( State, TickOffset, NewDiasca ) ->

	% Actual termination now!

	cond_utils:if_defined( simdiasca_debug_life_cycles, [
		?debug( "Terminating now." ),
		trace_utils:debug_fmt( "Actor ~w terminating now, at {~p,~p}.",
							   [ self(), TickOffset, NewDiasca ] ) ] ),

	cond_utils:if_defined( simdiasca_check_life_cycles,
		begin
			{ terminated, no_diasca_requested } = ?getAttr(next_action),
			check_termination_time_consistency( TickOffset, NewDiasca, State ),
			[] = ?getAttr(current_agenda)
		end  ),

	cond_utils:if_defined( simdiasca_check_model_behaviours,
		case ?getAttr(pending_messages) of

			[] ->
				ok;

			Messages ->
				?warning_fmt( "Actor (AAI: ~B) is terminating at tick offset "
					"#~B diasca ~B, but it had following pending actor "
					"messages: ~p.",
					[ ?getAttr(actor_abstract_id), TickOffset, NewDiasca,
					  Messages ] )

		end ),

	cond_utils:if_defined( simdiasca_debug_model_behaviours,
		begin

			% (not counting terminating tick)
			LifespanInTicks = TickOffset - ?getAttr(actor_creation_tick_offset),

			LifespanInSeconds = convert_ticks_to_seconds( LifespanInTicks,
														  State ),

			?info_fmt( "Actual termination of actor (AAI: ~B) occurred at "
				"tick offset #~B diasca ~B, i.e. after an actual lifespan "
				"of ~B ticks (~ts).",
				[ ?getAttr(actor_abstract_id), TickOffset, NewDiasca,
				  LifespanInTicks, time_utils:duration_to_string(
									round( 1000 * LifespanInSeconds ) ) ] )

		end ),

	% Finally we do not delete this actor here, we defer until next tick and
	% trigger it from the time manager:
	%
	% self() ! delete,

	% Nothing more to send to the time manager:
	wooper:const_return().



% @doc Performs checks, in the case of a termination tick.
check_termination_time_consistency( TickOffset, NewDiasca, State ) ->

	% A termination can *only* happen just afterwards the previous diasca on the
	% same tick:
	%
	PreviousDiasca = NewDiasca - 1,

	{ TickOffset, PreviousDiasca } = ?getAttr(previous_schedule).




% @doc Helps checking whether the actor is scheduled for a new tick as expected,
% by ensuring time always progresses.
%
check_time_consistency_at_new_tick( NewTickOffset, State ) ->

	case ?getAttr(previous_schedule) of

		undefined when is_integer( NewTickOffset ) ->
			% Actor starting, with any tick offset (0 if the simulation was not
			% started, non-null otherwise):
			%
			ok;

		{ WrongTickOffset, _D } when WrongTickOffset > NewTickOffset ->
			throw( { backward_tick_scheduling, WrongTickOffset,
					 NewTickOffset } );

		{ NewTickOffset, _AnyDiasca } ->
			throw( { duplicated_scheduling, NewTickOffset } );

		_CompatibleTickOffset ->
			% Any tick offset strictly in the past is possible:
			ok

	end.



% @doc Checks that no pending actor message targets an impossible future for the
% specified new tick.
%
% To be called just once this actor was scheduled for a new tick.
%
% This check is useful as, when an actor receives a message, it cannot check
% whether it comes from a too distant (incorrect) future, as the actor may be
% still lingering in a remote past, short of having been scheduled recently.
%
% So the only point in time where it can check messages against future is once
% being just scheduled (whatever the reason - an actor may already have received
% messages once it starts its new tick, although it is not the most common
% case).
%
check_messages_at_new_tick( CurrentTickOffset, State ) ->
	check_messages_at_new_tick_helper( ?getAttr(pending_messages),
									   CurrentTickOffset ).



% Any pending actor message can only have a very specific timestamp here (these
% can only be early messages targeting diasca 1).
%
check_messages_at_new_tick_helper( _PendingMessages=[], _CurrentTickOffset ) ->
	ok;

% The only licit case (most common put first):
check_messages_at_new_tick_helper(
		% If having received an early message, can only occur at diasca 0 and
		% then target diasca 1:
		%
		[ #actor_message{ tick_offset=CurrentTickOffset, diasca=1 } | T ],
		CurrentTickOffset ) ->
	check_messages_at_new_tick_helper( T, CurrentTickOffset );

% Correct tick, but invalid diasca:
check_messages_at_new_tick_helper(
	  [ M=#actor_message{ tick_offset=CurrentTickOffset, diasca=MsgDiasca }
		| _T ],
	  CurrentTickOffset ) ->
	throw( { diasca_one_expected_in_message_at_new_tick, MsgDiasca,
			 CurrentTickOffset, M } );

% All other cases are errors as well:
check_messages_at_new_tick_helper( [ M | _T ], CurrentTickOffset ) ->
	throw( { invalid_logical_timestamp_in_message_at_new_tick,
			 CurrentTickOffset, M } ).




% @doc Checks that no pending actor message targets an impossible future for
% the specified diasca at the specified tick.
%
% To be called just once this actor was scheduled for a new diasca.
%
% This check is useful as, when an actor receives a message, it cannot check
% whether it comes from a too distant (incorrect) future, as the actor may be
% still lingering in a remote past, short of having been scheduled recently.
%
% So the only point in time where it can check messages against future is once
% being just scheduled (whatever the reason - an actor may already have received
% messages once it starts its new tick, although it is not the most common
% case).
%
check_messages_at_new_diasca( CurrentTickOffset, CurrentDiasca, State ) ->
	check_messages_at_new_diasca_helper( ?getAttr(pending_messages),
										 CurrentTickOffset, CurrentDiasca ).


% A pending actor message can only have a specific timestamp here.
check_messages_at_new_diasca_helper( _PendingMessages=[], _CurrentTickOffset,
									 _CurrentDiasca ) ->
	ok;

% The only licit cases are where the actor and message timestamps agree or a
% message, sent by an early actor, is one diasca in the future (most common put
% first):
%
check_messages_at_new_diasca_helper(
	  [ #actor_message{ tick_offset=CurrentTickOffset, diasca=CurrentDiasca }
	   | T ], CurrentTickOffset, CurrentDiasca ) ->
	% Same diasca, most common case:
	check_messages_at_new_diasca_helper( T, CurrentTickOffset, CurrentDiasca );

check_messages_at_new_diasca_helper(
  [ #actor_message{ tick_offset=CurrentTickOffset, diasca=NextDiasca } | T ],
  CurrentTickOffset, CurrentDiasca ) when NextDiasca == CurrentDiasca + 1 ->

	% Next diasca, message must have been sent by an early actor:
	check_messages_at_new_diasca_helper( T, CurrentTickOffset, CurrentDiasca );

% All other cases are errors:
check_messages_at_new_diasca_helper( [ M | _T ], CurrentTickOffset,
									 CurrentDiasca ) ->
	throw( { invalid_logical_timestamp_in_message_at_new_diasca,
			 { CurrentTickOffset, CurrentDiasca }, M } ).



% @doc Throws an exception iff the specified tick offset was actually not
% registered as a schedule tick for this actor, or if was registered, but not as
% the first deadline.
%
check_spontaneous_indeed( NewTickOffset, State ) ->

	% An actor may have received an early message from a remote actor before
	% receiving its own beginTick message, so the next checking would be wrong:
	%
	% [] = ?getAttr(pending_messages),

	case ?getAttr(next_action) of

		% Only licit case:
		no_diasca_requested ->
			ok;

		new_diasca_needed ->
			throw( no_diasca_possibly_already_requested );

		T={ terminating, _AnyDelay, _AnyDiascaRequest }  ->
			throw( { should_already_be_terminated, T } );

		T={ terminated, _AnyDiascaRequest }  ->
			throw( { should_already_be_terminated, T } )

		% Case clause otherwise.

	end,

	case ?getAttr(current_agenda) of

		[ NewTickOffset | _T ] ->
			ok;

		Agenda ->
			throw( { faulty_spontaneous_entry, NewTickOffset, Agenda } )

	end.



% @doc Throws an exception iff there is no pending actor messages for this
% diasca of the current tick, for this actor.
%
check_triggered_indeed( TickOffset, NewDiasca, State ) ->

	PendingMessages = ?getAttr(pending_messages),

	case find_message_for_timestamp( TickOffset, NewDiasca, PendingMessages ) of

		true ->
			ok;

		false ->

			% No message, hence only other reason to be triggered: active
			% termination in progress.
			%
			case ?getAttr(next_action) of

				{ terminating, DiascaCount, no_diasca_requested }
						when is_integer( DiascaCount ) ->
					ok;

				Unexpected ->
					throw( { triggered_unexpectedly, TickOffset, NewDiasca,
							 Unexpected, PendingMessages } )

			end

	end.



% @doc Determines whether there is at least one pending actor message for the
% specified timestamp.
%
% Intercepts as well actor messages in the past.
%
find_message_for_timestamp( _TickOffset, _Diasca, _Acc=[] ) ->
	false;

find_message_for_timestamp( TickOffset, Diasca,
	  _Acc=[ #actor_message{ tick_offset=TickOffset, diasca=Diasca } | _T ] ) ->
	true;

find_message_for_timestamp( TickOffset, Diasca,
	  [ M = #actor_message{ tick_offset=ATickOffset, diasca=ADiasca } | _T ] )
		 when ATickOffset =/= TickOffset orelse ADiasca < Diasca
			  orelse ADiasca > Diasca + 1 ->
	throw( { found_message_in_the_past, M, { TickOffset, Diasca } } );

find_message_for_timestamp( TickOffset, Diasca, [ _H | T ] ) ->
	find_message_for_timestamp( TickOffset, Diasca, T ).





% Scheduling section: adding or removing spontaneous ticks.



% @doc Requests the next tick to be added (if not already registered) to the
% future spontaneous ticks of this actor.
%
-spec scheduleNextSpontaneousTick( wooper:state() ) -> oneway_return().
scheduleNextSpontaneousTick( State ) ->

	NextTick = ?getAttr(current_tick_offset) + 1,

	wooper:return_state( add_spontaneous_tick( NextTick, State ) ).



% @doc Adds the specified spontaneous tick offset to the already registered
% ones.
%
% Note: adding a given tick more than once is allowed (and will result of course
% in a single spontaneous scheduling at that tick).
%
-spec addSpontaneousTick( wooper:state(), tick_offset() ) -> oneway_return().
addSpontaneousTick( State, SpontaneousTickToAdd ) ->

	NewState = add_spontaneous_tick( SpontaneousTickToAdd, State ),

	wooper:return_state( NewState ).



% @doc Adds the specified spontaneous tick offsets to the already registered
% ones.
%
% Note: adding a given tick more than once is allowed (and will result of course
% in a single spontaneous scheduling at that tick).
%
-spec addSpontaneousTicks( wooper:state(), [ tick_offset() ]  ) ->
									oneway_return().
addSpontaneousTicks( State, SpontaneousTicksToAdd ) ->

	NewState = add_spontaneous_ticks( SpontaneousTicksToAdd, State ),

	wooper:return_state( NewState ).



% @doc Adds the specified spontaneous tick offset to the already registered
% ones, and returns an updated state.
%
% Note: adding a given tick more than once is allowed (and will result of course
% for that actor in a single spontaneous scheduling at that tick).
%
-spec add_spontaneous_tick( tick_offset(), wooper:state() ) ->
									wooper:state().
add_spontaneous_tick( SpontaneousTickToAdd, State )
  when is_integer( SpontaneousTickToAdd ) ->

	% Uniquification done later, on sending:
	appendToAttribute( State, added_spontaneous_ticks, SpontaneousTickToAdd );

add_spontaneous_tick( Other, _State ) ->
	throw( { not_a_tick_offset, Other } ).



% @doc Adds the specified spontaneous tick offsets to the already registered
% ones, and returns an updated state.
%
% Note: adding a given tick more than once is allowed (and will result of course
% for that actor in a single spontaneous scheduling at that tick).
%
-spec add_spontaneous_ticks( [ tick_offset() ], wooper:state() ) ->
									wooper:state().
add_spontaneous_ticks( SpontaneousTicksToAdd, State ) ->

	cond_utils:if_defined( simdiasca_check_user_api_calls,
	  list_utils:check_integers( SpontaneousTicksToAdd ) ),

	PreviousTicks = ?getAttr(added_spontaneous_ticks),

	% We prefer simplifying as much as possible the future of work of the time
	% manager by removing duplicated ticks, but it will done later, on sending:
	%
	NewTicks = SpontaneousTicksToAdd ++ PreviousTicks,

	setAttribute( State, added_spontaneous_ticks, NewTicks ).



% @doc Adds the spontaneous tick offset determined from the current one plus the
% specified one to the already registered ones, and returns an updated state.
%
% Note: adding a given tick more than once is allowed (and will result of course
% for that actor in a single spontaneous scheduling at that tick).
%
-spec add_spontaneous_tick_in( tick_offset(), wooper:state() ) ->
										wooper:state().
add_spontaneous_tick_in( InSpontaneousDurationOffset, State )
  when is_integer( InSpontaneousDurationOffset )->

	SpontaneousTickToAdd = ?getAttr(current_tick_offset)
		+ InSpontaneousDurationOffset,

	% Uniquification done later, on sending:
	appendToAttribute( State, added_spontaneous_ticks, SpontaneousTickToAdd );

add_spontaneous_tick_in( Other, _State ) ->
	throw( { not_a_tick_offset, Other } ).



% @doc Adds the spontaneous tick offset corresponding to specified timestamp to
% the already registered ones, and returns an updated state.
%
% Note: adding a given tick more than once is allowed (and will result of course
% for that actor in a single spontaneous scheduling at that tick).
%
-spec add_spontaneous_timestamp( timestamp(), wooper:state() ) ->
										wooper:state().
add_spontaneous_timestamp( Timestamp, State ) ->
	Secs = calendar:datetime_to_gregorian_seconds( Timestamp ),
	TargetAbsTick = convert_seconds_to_ticks( Secs, State ),

	% Uniquification done later, on sending:
	appendToAttribute( State, added_spontaneous_ticks, TargetAbsTick ).



% @doc Withdraws the specified spontaneous tick offset from the already
% registered ones.
%
-spec withdrawSpontaneousTick( wooper:state(), tick_offset() ) ->
									oneway_return().
withdrawSpontaneousTick( State, SpontaneousTickToWithdraw ) ->

	NewState = withdraw_spontaneous_tick( SpontaneousTickToWithdraw, State ),

	wooper:return_state( NewState ).



% @doc Withdraws the specified spontaneous tick offsets from the already
% registered ones.
%
-spec withdrawSpontaneousTicks( wooper:state(), [ tick_offset() ] ) ->
										oneway_return().
withdrawSpontaneousTicks( State, SpontaneousTicksToWithdraw ) ->

	NewState = withdraw_spontaneous_ticks( SpontaneousTicksToWithdraw, State ),

	wooper:return_state( NewState ).



% @doc Withdraws the specified spontaneous tick offset from the already
% registered ones, and returns an updated state.
%
-spec withdraw_spontaneous_tick( tick_offset(), wooper:state() ) ->
										wooper:state().
withdraw_spontaneous_tick( SpontaneousTickToWithdraw, State )
  when is_integer( SpontaneousTickToWithdraw )->

	appendToAttribute( State, withdrawn_spontaneous_ticks,
					   SpontaneousTickToWithdraw );

withdraw_spontaneous_tick( Other, _State ) ->
	throw( { not_a_tick_offset, Other } ).



% @doc Withdraws the specified spontaneous tick offsets to the already
% registered ones, and returns an updated state.
%
-spec withdraw_spontaneous_ticks( [ tick_offset() ], wooper:state() ) ->
										wooper:state().
withdraw_spontaneous_ticks( SpontaneousTicksToWithdraw, State ) ->

	cond_utils:if_defined( simdiasca_check_user_api_calls,
		list_utils:check_integers( SpontaneousTicksToWithdraw ) ),

	PreviousTicks = ?getAttr(withdrawn_spontaneous_ticks),

	% We prefer simplifying as much as possible the future of work of the time
	% manager by removing duplicated ticks, but it will done later, on sending:
	%
	NewTicks = SpontaneousTicksToWithdraw ++ PreviousTicks,

	setAttribute( State, withdrawn_spontaneous_ticks, NewTicks ).



% @doc Called by an actor when it determines it is to be removed from the
% simulation and deleted: starts its termination procedure, with no specific
% upper bound in terms of diascas until actual termination (so this actor will
% be actually deleted at the next tick (diasca 0).
%
-spec declareTermination( wooper:state() ) -> oneway_return().
declareTermination( State ) ->

	cond_utils:if_defined( simdiasca_debug_life_cycles,
		trace_utils:debug_fmt( "Actor named '~ts' (~w) declaring termination.",
							   [ ?getAttr(name), self() ] ) ),

	% Defaults to 'unlimited':
	DeletionState = declare_termination( unlimited, State ),

	wooper:return_state( DeletionState ).



% @doc Called by an actor when it determines it is to be removed from the
% simulation and deleted: starts its termination procedure, waiting for exactly
% the specified number of diascas (supposedly sufficient so that all actors
% knowing it become aware of its termination) until performing its actual
% termination.
%
% Of course it is up to this actor to notify appropriately the relevant actors
% and to ensure they have a sufficient number of diascas to do so.
%
% Note: selecting a 'unlimited' termination delay is strongly recommended.
%
-spec declareTermination( wooper:state(), termination_delay() ) ->
								oneway_return().
declareTermination( State, TerminationDelay ) ->

	cond_utils:if_defined( simdiasca_debug_life_cycles,
		trace_utils:debug_fmt( "Actor named '~ts' (~w) declaring termination ",
			"with a delay of ~p.",
			[ ?getAttr(name), self(), TerminationDelay ] ) ),

	DeletionState = declare_termination( TerminationDelay, State ),

	wooper:return_state( DeletionState ).



% @doc Called by an actor when it determines it is to be removed from the
% simulation and deleted, which could be done on the very next diasca.
%
% (helper)
%
-spec declare_termination( termination_delay(), wooper:state() ) ->
									wooper:state().
declare_termination( _IntercalaryDiasca=0, State ) ->

	% Here we do not expect to receive any other actor message (even in this
	% diasca - beware to the unshuffling of messages, we never can be sure we
	% are processing the last message of a diasca), so we proceed directly to
	% the 'terminated' state, unless of course we already requested a new diasca
	% (typically we just sent at least one actor message during this diasca):

	NextAction = case ?getAttr(next_action) of

		new_diasca_needed ->
			{ terminated, new_diasca_needed };

		no_diasca_requested ->
			{ terminated, no_diasca_requested };

		T={ terminating, _Delay, _DiascaRequest } ->
			throw( { already_terminating, T } );

		T={ terminated, _DiascaRequest } ->
			throw( { already_terminated, T } )

	end,

	setAttribute( State, next_action, NextAction );


% Includes the 'unlimited' case (here we will wait the specified number of
% diascas before going in the 'terminated' state):
%
declare_termination( IntercalaryDiasca, State ) ->

	NextAction = case ?getAttr(next_action) of

		new_diasca_needed ->

			cond_utils:if_defined( simdiasca_debug_life_cycles,
				trace_utils:debug_fmt( "Actor ~w terminating, and "
					"requesting a new diasca.", [ self() ] ) ),

			{ terminating, IntercalaryDiasca, new_diasca_needed };


		no_diasca_requested ->

			cond_utils:if_defined( simdiasca_debug_life_cycles,
				trace_utils:debug_fmt( "Actor ~w terminating, not "
					"requesting any new diasca.", [ self() ] ) ),

			{ terminating, IntercalaryDiasca, no_diasca_requested };


		T={ terminating, _Delay, _DiascaRequest } ->
			throw( { already_terminating, T } );


		T={ terminated, _DiascaRequest } ->
			throw( { already_terminated, T } )


	end,

	setAttribute( State, next_action, NextAction ).



% @doc Reacts to a notification of time manager shutdown by deleting this actor.
-spec timeManagerShutdown( wooper:state() ) -> const_oneway_return().
timeManagerShutdown( State ) ->

	cond_utils:if_defined( simdiasca_debug_life_cycles,
		?debug( "Received a notification of time manager shutdown, "
				"requesting our own deletion." ) ),

	self() ! delete,

	wooper:const_return().



% @doc Defines the spontaneous behaviour of this actor.
%
% The actSpontaneous/1 oneway is expected to update the actor state, send any
% relevant actor message(s) and/or update the spontaneous agenda of this actor.
%
% Note that this is a (standard) oneway, not an actor oneway.
%
% Default implementation, made to be overridden.
%
-spec actSpontaneous( wooper:state() ) -> const_oneway_return().
actSpontaneous( State ) ->

	?info_fmt( "Acting spontaneously (blank default behaviour) "
			   "at tick offset #~B.", [ get_current_tick_offset( State ) ] ),

	% No future action defined, defaults to passive (none):
	wooper:const_return().



% @doc Oneway called by another actor (A) to send to this current actor
% (S=self()) a behaviour-specific message: this S actor stores this message for
% a later processing, but acknowleges it immediately to A.
%
-spec receiveActorMessage( wooper:state(), tick_offset(), diasca(),
					oneway_call(), actor_pid(), aai() ) -> oneway_return().
receiveActorMessage( State, MessageTickOffset, MessageTargetDiasca,
					 ActorOneway, SendingActorPid, SendingActorAai ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),
	CurrentDiasca = ?getAttr(current_diasca),

	cond_utils:if_defined( simdiasca_debug_time_management,
		trace_utils:debug_fmt( "receiveActorMessage '~p' for ~w: current tick "
		   "offset is #~p, current diasca is ~B, message tick offset is #~p, "
		   "message target diasca is ~B.",
		   [ ActorOneway, self(), CurrentTickOffset, CurrentDiasca,
		   MessageTickOffset, MessageTargetDiasca ] ) ),


	% First, some checkings.
	%
	% Note that this actor may not have been scheduled for a while, thus even
	% its current tick/diasca might be outdated.
	%
	% So the only impossible case is if the message targets the current local
	% stored current time or any time before; it must indeed be in a strict
	% future.
	%
	% As the engine has never be found faulty in this case, it is most probably
	% a model-level mistake leading to triggering this mechanism.
	%
	% A typical example of it is when implementing a model that may send actor
	% messages from its destructor. This shall never be done, as if for any
	% reason an instance thereof is deleted from a non-synchronised context (ex:
	% at simulation end, any lingering actor is deleted that way), this
	% destructed instance will send such actor messages with an outdated
	% timestamp corresponding to the last time this actor was synchronised, and
	% the following error will be raised on the side of the receiving actor:
	%
	case MessageTickOffset < CurrentTickOffset of

		true ->
			?error_fmt( "An actor message targeting a tick in the past was "
				"received by '~ts' (PID: ~w; AAI: ~B) at local tick offset #~B "
				"diasca ~B; sender: ~w (AAI: ~B), "
				"message tick offset #~B diasca ~B, actor oneway: '~p'.",
				[ ?getAttr(name), self(), ?getAttr(actor_abstract_id),
				  CurrentTickOffset, CurrentDiasca, SendingActorPid,
				  SendingActorAai, MessageTickOffset, MessageTargetDiasca,
				  ActorOneway ] ),

			FullMessage = { MessageTickOffset, MessageTargetDiasca, ActorOneway,
							SendingActorPid, SendingActorAai },

			throw( { message_tick_in_the_past, FullMessage, CurrentTickOffset,
					 CurrentDiasca, self() } );

		false ->

			case MessageTickOffset > CurrentTickOffset of

				true ->
					% Nothing more can be checked, can be licit:
					ok;

				false ->
					% Here MessageTickOffset == CurrentTickOffset:
					case MessageTargetDiasca =< CurrentDiasca of

						true ->

							% The message must be at least one diasca in the
							% future:

							?error_fmt( "An actor message targeting a diasca "
								"in the past was received at tick offset #~B, "
								"local diasca ~B; sender: ~w (AAI: ~B), "
								"message diasca ~B, oneway: ~p.",
								[ CurrentTickOffset, CurrentDiasca,
								  SendingActorPid, SendingActorAai,
								  MessageTargetDiasca, ActorOneway ] ),

							FullMessage = { MessageTickOffset,
											MessageTargetDiasca, ActorOneway,
											SendingActorPid, SendingActorAai },

							throw( { message_diasca_in_the_past, FullMessage,
									 CurrentTickOffset, CurrentDiasca,
									 self() } );

						false ->
							% In the future:
							ok

					 end

			end

	end,

	MessageTimestamp = { MessageTickOffset, MessageTargetDiasca },

	% Let's start by checking whether receiving a message is licit here:
	case ?getAttr(next_action) of

		{ terminated, _AnyDiascaRequest } ->

			?error_fmt( "An actor message has been received at diasca ~B "
				"whereas this receiving actor has already terminated; sender "
				"is ~w (AAI: ~B), message timestamp is {~p,~p}, and is '~p'.",
				[ CurrentDiasca, SendingActorPid, SendingActorAai,
				  MessageTickOffset, MessageTargetDiasca, ActorOneway ] ),

			throw( { message_received_while_terminated,
				{ CurrentTickOffset, CurrentDiasca },
				MessageTimestamp, { ?getAttr(actor_abstract_id), self() },
				{ SendingActorAai, SendingActorPid }, ActorOneway } ) ;

		_ ->
			% Correct, even if terminating:
			ok

	end,

	% Note that the message sent below is not enough to ensure that the next
	% diasca will be scheduled at all, as the time manager of this receiving
	% actor may already have reported its end of diasca; this is why this is up
	% to the *calling* actor to notify, when finishing its diasca, its own time
	% manager, which by design has not finished its diasca yet.
	%
	% We have here to specify the diasca at which this actor should be
	% triggered, as the time manager of this receiving actor may not have
	% already received the 'begin' message for the diasca from which the sending
	% actor acted (ex: if the sending actor is scheduled by the root time
	% manager whereas this receiving actor is on another node).
	%
	% Indeed, when the time manager of this receiving actor receives such a
	% scheduleTrigger message while still at diasca D (i.e. not having received
	% yet a 'begin diasca' for D+1), it has no way of knowing whether this is
	% due to another time manager still in D or already in D+1. The race
	% condition between its 'begin diasca' message and the scheduleTrigger
	% message results in the need for the target diasca to be specified.
	%
	% Of course this message is strictly necessary, let alone so that the local
	% time manager knows which actors shall be triggered during next diasca.
	%
	% However this scheduleTrigger message should not be sent more than once by
	% an actor to its local time manager during the same diasca (as an actor
	% should not be triggered more than once in a given diasca); instead of
	% having time managers remove duplicates in their
	% actors_to_trigger_next_diasca list, each actor just ensures that it sends
	% only up to one scheduleTrigger message per diasca:
	%
	ScheduledState = case ?getAttr(last_sent_schedule_trigger) of

		MessageTimestamp ->
			% Only case where nothing is to be done:
			State;

		% Either 'undefined' or a different timestamp (in the past):
		 _ ->

			cond_utils:if_defined( simdiasca_debug_time_management,
				trace_utils:debug_fmt(
					"~w sending scheduleTrigger to time manager ~w.",
					[ self(), ?getAttr(time_manager_pid) ] ) ),

			% A *key* point is to use the timestamp from the message, knowing
			% that this receiving actor may be lagging behind the sender one:
			%
			?getAttr(time_manager_pid) ! { scheduleTrigger,
						[ MessageTickOffset, MessageTargetDiasca ], self() },

			% Now wait for the scheduleTrigger answer:
			receive

				% A priori, having this operation be synchronous may not be
				% necessarily strictly mandatory (but it does not hurt and is
				% cheap - being purely local and happening only up to once per
				% diasca per actor):
				%
				{ wooper_result, trigger_planned } ->
					ok

			end,

			setAttribute( State, last_sent_schedule_trigger, MessageTimestamp )

	end,

	% Then acknowledges to the sender (no interleaving preferred):
	SendingActorPid ! { acknowledgeMessage, self() },


	% Stores this message for later processing:
	StoredMessage = #actor_message{ tick_offset=MessageTickOffset,
									diasca=MessageTargetDiasca,
									sender_pid=SendingActorPid,
									sender_aai=SendingActorAai,
									actual_message=ActorOneway },

	MessageState = appendToAttribute( ScheduledState, pending_messages,
									  StoredMessage ),

	wooper:return_state( MessageState ).




% @doc Callback triggered by the reception of an acknowledgement from an actor
% to which this actor sent a message.
%
-spec acknowledgeMessage( wooper:state(), actor_pid() ) -> oneway_return().
acknowledgeMessage( State, CalledActorPid ) ->

	Waited = ?getAttr(waited_acks),

	% Check we are indeed waiting for this ack, remove it from list, see if it
	% was the last waited one:
	%
	ShortenWaitedList =
			case list_utils:delete_if_existing( CalledActorPid, Waited ) of

		not_found ->
			?error_fmt( "An unexpected acknowledgement of actor message "
				"has been received, emanating from actor ~w "
				"(while the waited acks are: ~p). "
				"This usually happens whenever the current actor "
				"(~w) sent an (actor) message to an actor (here ~w), "
				"yet managed to forget this sending (by dropping "
				"the corresponding sender state).",
				[ CalledActorPid, Waited, self(), CalledActorPid ] ),

			throw( { unexpected_actor_message_ack, CalledActorPid } );

		L ->
			L

	end,

	ShortenState = setAttribute( State, waited_acks, ShortenWaitedList ),

	NewState = case ShortenWaitedList of

		[] ->
			% Last ack received, ready to declare this actor's end of diasca:
			notify_diasca_ended( ShortenState );

		_ ->
			% There is still at least one waited ack, still waiting:
			ShortenState

	end,

	wooper:return_state( NewState ).



% @doc Sends to the local time manager a notification that the current diasca
% ended.
%
% Returns an updated state.
%
% (helper)
%
notify_diasca_ended( State ) ->

	cond_utils:if_defined( simdiasca_check_time_management,
		[] = ?getAttr(waited_acks) ),

	% Let's try to ease as much as possible the work of the time manager:
	AddedTicks = list_utils:uniquify( ?getAttr(added_spontaneous_ticks) ),

	WithdrawnTicks =
		list_utils:uniquify( ?getAttr(withdrawn_spontaneous_ticks) ),

	CurrentTickOffset = ?getAttr(current_tick_offset),
	CurrentDiasca = ?getAttr(current_diasca),

	cond_utils:if_defined( simdiasca_debug_time_management,
		trace_utils:debug_fmt(
			"~w will report at ~w following added spontaneous ticks: ~w ",
			[ self(), { CurrentTickOffset, CurrentDiasca },
			  ?getAttr(added_spontaneous_ticks) ] ) ),

	{ NextRecordedAction, NextReportedAction } = case ?getAttr(next_action) of

		new_diasca_needed ->
			% We must reset the recorded next_action attribute:
			{ no_diasca_requested, new_diasca_needed };

		no_diasca_requested ->
			{ no_diasca_requested, no_diasca_requested };

		{ terminating, _DiascaCount=unlimited, DiascaRequest } ->
			cond_utils:if_defined( simdiasca_debug_life_cycles,
				trace_utils:debug_fmt( "Actor ~w unlimited-terminating at ~w, "
					"requesting ~w.", [ self(), DiascaRequest,
							{ CurrentTickOffset, CurrentDiasca } ] ) ),
			{
				% Diasca request reset internally:
				{ terminating, unlimited, no_diasca_requested },
				{ terminating_unlimited, DiascaRequest } };

		{ terminating, _DiascaCount=0, DiascaRequest } ->
			% Termination completed; an actor is expected to send a 'terminated'
			% notification once, as it is to be deallocated just afterwards (it
			% could be deleted at the next diasca, however we defer it to the
			% next tick):
			%
			NewTerminated = { terminated, DiascaRequest },
			{ NewTerminated, NewTerminated };

		{ terminating, NonNullDiascaCount, _DiascaRequest } ->
			% Termination still in progress:
			{
			   % Diasca request reset internally:
			   { terminating, NonNullDiascaCount-1, no_diasca_requested },
				 % Implies, manager-side, new_diasca_needed:
				 terminating };

		T={ terminated, _DiascaRequest } ->
			% Diasca request reset internally:
			{ { terminated, no_diasca_requested }, T }

	end,

	cond_utils:if_defined( simdiasca_debug_time_management,
		trace_utils:debug_fmt( "Actor ~w at {~p,~p}: next recorded action is "
			"~p, while next notified action is ~p.",
			[ self(), CurrentTickOffset, CurrentDiasca, NextRecordedAction,
			  NextReportedAction ] ) ),

	% No more actor message waited at this diasca:
	NotificationMessage = case CurrentDiasca of

		0 ->
			{ notifySpontaneousActionsCompleted, [ CurrentTickOffset, self(),
					NextReportedAction, AddedTicks, WithdrawnTicks ] };

		_ ->
			{ notifyTriggeredActionsCompleted, [ CurrentTickOffset,
					CurrentDiasca, self(), NextReportedAction, AddedTicks,
					WithdrawnTicks ] }

	end,

	% No more actor message waited this diasca:
	?getAttr(time_manager_pid) ! NotificationMessage,

	NewAgenda = update_agenda_with( AddedTicks, WithdrawnTicks,
									?getAttr(current_agenda) ),

	% Prepare for next diasca, reset relevant attributes:
	setAttributes( State, [
				{ previous_schedule, { CurrentTickOffset, CurrentDiasca } },
				{ added_spontaneous_ticks, [] },
				{ withdrawn_spontaneous_ticks, [] },
				{ next_action, NextRecordedAction },
				{ current_agenda, NewAgenda } ] ).



% @doc Returns the current simulation time of this actor, expressed as an offset
% of simulation ticks relative to the beginning of the simulation.
%
-spec getSimulationTickOffset( wooper:state() ) ->
									const_request_return( tick_offset() ).
getSimulationTickOffset( State ) ->
	wooper:const_return_result( ?getAttr(current_tick_offset) ).



% @doc Returns the current simulation time of this actor, expressed as an
% absolute number of simulation ticks.
%
-spec getSimulationTick( wooper:state() ) -> const_request_return( tick() ).
getSimulationTick( State ) ->
	wooper:const_return_result( get_current_tick( State ) ).



% @doc Returns the current simulation time of this actor, structured as follows:
% {{SimYear,SimMonth,SimDay}, {SimHour,SimMinute,SimSecond}}.
%
% This date might be less precise than the actual simulation tick, so the latter
% cannot be obtained from the former.
%
-spec getSimulationDate( wooper:state() ) ->
								const_request_return( timestamp() ).
getSimulationDate( State ) ->

	IntegerSeconds = round(
		convert_ticks_to_seconds( get_current_tick( State ), State ) ),

	wooper:const_return_result(
		calendar:gregorian_seconds_to_datetime( IntegerSeconds ) ).



% @doc Returns a textual description of the simulation and real time, for this
% actor.
%
-spec getTextualTimings( wooper:state() ) -> const_request_return( ustring() ).
getTextualTimings( State ) ->
	wooper:const_return_result( get_textual_timings( State ) ).



% @doc Returns an atom corresponding to the Erlang node on which this actor
% runs.
%
% Note: mostly useful for the test of the placement heuristics.
%
-spec getHostingNode( wooper:state() ) ->
							const_request_return( net_utils:node_name() ).
getHostingNode( State ) ->
	wooper:const_return_result( node() ).



% @doc Converts the specified duration in seconds (expressed as an integer or a
% floating point value) into an integer (rounded) number of simulation ticks,
% which is at least equal to one tick.
%
% Ex: convertSecondsToTicks( State, 0.001 )
% Note: the convert_seconds_to_ticks helper function can be used as well.
%
-spec convertSecondsToTicks( wooper:state(), unit_utils:any_seconds() ) ->
								const_request_return( tick_offset() ).
convertSecondsToTicks( State, Seconds ) ->
	wooper:const_return_result( convert_seconds_to_ticks( Seconds, State ) ).



% @doc Converts the specified tick count into a fractional (floating-point)
% number of seconds.
%
% Note: the convert_ticks_to_seconds helper function can be used as well.
%
-spec convertTicksToSeconds( wooper:state(), tick_offset() ) ->
						const_request_return( unit_utils:float_second() ).
convertTicksToSeconds( State, Ticks ) ->
	wooper:const_return_result( convert_ticks_to_seconds( Ticks, State ) ).



% @doc Returns (asynchronously, to avoid deadlocks) the current list of waited
% actors (if any) for that actor.
%
% Allows the TimeManager to know why this actor may be stalling the simulation,
% and who it is.
%
-spec nudge( wooper:state(), pid() ) -> const_oneway_return().
nudge( State, SenderPid ) ->

	SenderPid ! { notifyNudged, [ self(), get_current_tick_offset( State ),
								  ?getAttr(waited_acks) ] },

	wooper:const_return().



% @doc Returns the AAI of that instance.
-spec getAAI( wooper:state() ) -> const_request_return( aai() ).
getAAI( State ) ->
	wooper:const_return_result( ?getAttr(actor_abstract_id) ).



% @doc Returns an information record about this actor.
-spec getActorInfo( wooper:state() ) -> const_request_return( actor_info() ).
getActorInfo( State ) ->

	{ _SameState, ActualClassname } = executeRequest( State, getClassname ),

	ActorInfo = #actor_info{ classname=ActualClassname,
							 name=?getAttr(name),
							 aai=?getAttr(actor_abstract_id) },

	wooper:const_return_result( ActorInfo ).




% @doc Called automatically after (generally after two diascas) this actor
% called create_actor/{3,4} or create_placed_actor/{4,5}, to notify it the
% creation was done, resulting in a newly created actor.
%
% Parameters are:
%
% - CreatedActorPid the PID of the just created actor
%
% - CreatedActorTag the tag used for this actor creation so that it is able to
% discriminate among the multiple creations it might have requested; this is
% either a user-defined tag or { ActorClassname, ActorConstructionParameters },
% i.e. a pair made of the classname of that created actor and of the parameters
% which were specified for its creation
%
% Note: this is a default implementation, meant to be overridden.
%
% Typically called by the load balancer.
%
-spec onActorCreated( wooper:state(), actor_pid(), tag(),
					  sending_actor_pid() ) -> const_actor_oneway_return().
onActorCreated( State, CreatedActorPid, CreatedActorTag, _SendingActorPid ) ->

	%?debug_fmt(
	%    "Default non-overridden onActorCreated/4 method called: "
	%    "actor ~w was created from tag '~p'.",
	%    [ CreatedActorPid, CreatedActorTag ] ),

	trace_utils:debug_fmt( "Default non-overridden onActorCreated/4 method "
		"called: actor ~w was created from tag '~p'.",
		[ CreatedActorPid, CreatedActorTag ] ),

	actor:const_return().



% @doc Requires the caller (generally the time manager) to be notified
% (asynchronously) of the name (as a binary) of this actor.
%
-spec triggerNameNotification( wooper:state(), wooper:caller_pid() ) ->
										const_oneway_return().
triggerNameNotification( State, CallerPid ) ->

	CallerPid ! { notifyName, [ self(), ?getAttr(name) ] },

	wooper:const_return().



% @doc Relinks this actor, supposing it was just deserialised.
%
% (request, for synchronicity)
%
-spec relink( wooper:state() ) ->
					const_request_return( { 'relinked', actor_pid() } ).
relink( State ) ->
	wooper:const_return_result( { relinked, self() } ).




% Section for helper functions (not methods).


% @doc Returns the (supposed opaque) identifier (AAI) of that actor.
%
% During a given simulation, an actor bears a unique identifier, and this
% identifier will be the same from one simulation to another.
%
% (helper function)
%
-spec get_abstract_identifier( wooper:state() ) -> aai().
get_abstract_identifier( State ) ->
	?getAttr(actor_abstract_id).



% @doc Returns a path to the root directory of the deployed elements, as a plain
% string.
%
% Useful to be able to look-up and read third-party (simulation-specific)
% deployed data.
%
% Ex: "/tmp/sim-diasca-My_Case-boudevil-2012-12-7-at-13h-56m-03s-1f79"
% "3a6ba507/deployed-elements" may be returned.
%
% (helper function)
%
-spec get_deployed_root_directory( wooper:state() ) ->
										file_utils:directory_path().
get_deployed_root_directory( _State ) ->

	% Relies on the fact that the working directory must not have been changed,
	% neither by models nor by the simulation case (otherwise a path would have
	% to be recorded, possibly in each actor):
	%
	file_utils:join(
		[ file_utils:get_current_directory(), "..", "deployed-elements" ] ).




% This section for time conversion is directly inspired from the one offered by
% the time manager.


% @doc Converts the specified duration in virtual seconds (expressed as an
% integer or a floating-point value) into an integer (non-negative, rounded)
% number of simulation ticks.
%
% Note: this time conversion will be checked for accuracy based on the default
% threshold in terms of relative error, and thus may fail at runtime, should it
% be deemed too inaccurate.
%
% Ex: TickCount = class_Actor:convert_seconds_to_ticks(_Secs=0.001, State)
%
% This function can be called as soon as the class_Actor constructor has been
% executed.
%
% (helper function)
%
-spec convert_seconds_to_ticks( any_seconds(), wooper:state() ) ->
													tick_duration().
convert_seconds_to_ticks( Seconds, State ) ->
	% Less than 1.5% of relative error tolerated by default:
	convert_seconds_to_ticks( Seconds, _DefaultMaxRelativeError=0.015, State ).



% @doc Converts the specified duration in virtual seconds (expressed as an
% integer or a floating-point value) into an integer (non-negative, rounded)
% number of simulation ticks.
%
% Note: this time conversion will be checked for accuracy based on the default
% threshold in terms of relative error, and thus may fail, should it be deemed
% to inaccurate.
%
% Ex: TickCount = class_Actor:convert_seconds_to_ticks_explicit(_Secs=5,
%                                       _TickDur=0.01)
%
% This function can be called as soon as the class_Actor constructor has been
% executed.
%
% (helper function)
%
-spec convert_seconds_to_ticks_explicit( any_seconds(), virtual_seconds() ) ->
												tick_duration().
convert_seconds_to_ticks_explicit( Seconds, TickDuration )  ->
	convert_seconds_to_ticks_explicit( Seconds, _DefaultMaxRelativeError=0.015,
									   TickDuration ).



% @doc Converts the specified number of (floating-point) seconds into an integer
% (rounded) number of ticks, checking that any rounding error stays within
% specified maximum relative error.
%
% For example, to limit the relative error to 5%, use MaxRelativeError=0.05.
%
% (helper function)
%
-spec convert_seconds_to_ticks( any_seconds(), math_utils:percent(),
								wooper:state() ) -> tick_duration().
convert_seconds_to_ticks( Seconds, MaxRelativeError, State ) ->
	convert_seconds_to_ticks_explicit( Seconds, MaxRelativeError,
									   ?getAttr(simulation_tick_duration) ).



% @doc Converts the specified number of (floating-point) seconds into an integer
% (rounded) number of ticks, checking that any rounding error stays within
% specified maximum relative error.
%
% For example, to limit the relative error to 5%, use MaxRelativeError=0.05.
%
% Helper introduced to be exported, so that it may be used by non-actors as well
% (ex: WOOPER helper instances).
%
-spec convert_seconds_to_ticks_explicit( any_seconds(), math_utils:percent(),
										 virtual_seconds() ) -> tick_duration().
convert_seconds_to_ticks_explicit( Seconds, MaxRelativeError, TickDuration )
  when is_float( Seconds ) andalso Seconds >= 0 ->

	TickCount = erlang:round( Seconds / TickDuration ),

	% Converts back to measure error:
	CorrespondingSeconds = TickCount * TickDuration,

	case math_utils:are_relatively_close( Seconds, CorrespondingSeconds,
										  MaxRelativeError ) of

		true ->
			TickCount;

		false ->

			ActualError = math_utils:get_relative_difference( Seconds,
														CorrespondingSeconds ),

			throw( { too_inaccurate_duration_conversion, TickCount,
						{ Seconds, CorrespondingSeconds }, TickDuration,
						{ MaxRelativeError, ActualError } } )

	end;

convert_seconds_to_ticks_explicit( Seconds, MaxRelativeError, TickDuration )
  when is_integer( Seconds ) ->
	convert_seconds_to_ticks_explicit( erlang:float( Seconds ),
									   MaxRelativeError, TickDuration );

convert_seconds_to_ticks_explicit( Seconds, _MaxRelativeError,
								   _TickDuration ) ->
	throw( { invalid_duration, Seconds } ).



% @doc Converts the specified duration in seconds (expressed as an integer or a
% floating point value) into an integer (strictly positive, rounded) number of
% simulation ticks, which is at least equal to one tick.
%
% Ex: TickCount = convert_seconds_to_non_null_ticks( 0.001, State )
%
% (helper function)
%
-spec convert_seconds_to_non_null_ticks( any_seconds(), wooper:state() ) ->
												tick_offset().
convert_seconds_to_non_null_ticks( Seconds, State )
  when is_integer( Seconds ) ->
	convert_seconds_to_non_null_ticks( erlang:float( Seconds ), State );

convert_seconds_to_non_null_ticks( Seconds, State ) ->
	case convert_seconds_to_ticks( Seconds, State ) of

		0 ->
			1;

		TickCount ->
			TickCount

	end.



% @doc Converts the specified duration in seconds (expressed as an integer or a
% floating point value) into an integer (strictly positive, rounded) number of
% simulation ticks, checking that any rounding error stays within specified
% maximum relative error and then ensuring the returned duration is at least
% equal to one tick.
%
% Ex: TickCount = convert_seconds_to_non_null_ticks(0.001, 0.01, State)
%
% (helper function)
%
-spec convert_seconds_to_non_null_ticks( any_seconds(), math_utils:percent(),
										 wooper:state() ) -> tick_offset().
convert_seconds_to_non_null_ticks( Seconds, MaxRelativeError, State ) ->

	case convert_seconds_to_ticks( Seconds, MaxRelativeError, State ) of

		0 ->
			1;

		TickCount ->
			TickCount

	end.



% @doc Converts the specified tick count into a duration expressed as a
% fractional (floating-point) number of seconds.
%
% This function can be called as soon as the class_Actor constructor has been
% executed.
%
% (helper function)
%
-spec convert_ticks_to_seconds( tick_offset(), wooper:state() ) ->
										virtual_seconds().
convert_ticks_to_seconds( Ticks, State ) ->
	convert_ticks_to_seconds_explicit( Ticks,
									   ?getAttr(simulation_tick_duration) ).



% @doc Converts the specified tick count into a duration expressed as a
% fractional (floating-point) number of seconds.
%
% This function may be called from any context (possibly non-actor ones).
%
% (helper function)
%
-spec convert_ticks_to_seconds_explicit( tick_offset(), virtual_seconds() ) ->
												virtual_seconds().
convert_ticks_to_seconds_explicit( Ticks, TickDuration ) ->

	% Applies identically to (absolute) ticks or tick offsets:
	Res = Ticks * TickDuration,

	%trace_utils:debug_fmt( "Ticks: ~p, tick duration: ~p, secs: ~p.",
	%						[ Ticks, TickDuration, Res ] ),

	Res.




% @doc Returns a textual description of the real and simulated time.
%
% (helper function)
%
-spec get_textual_timings( wooper:state() ) -> ustring().
get_textual_timings( State ) ->

	CurrentTick = get_current_tick_offset( State ),

	CurrentSecond = convert_ticks_to_seconds( CurrentTick, State ),

	{ { SimYear, SimMonth, SimDay }, { SimHour, SimMinute, SimSecond } } =
		calendar:gregorian_seconds_to_datetime( CurrentSecond ),

	{ { RealYear, RealMonth, RealDay }, { RealHour, RealMinute, RealSecond } } =
		{ date(), time() },

	text_utils:format( "actor simulation time: "
		"~B/~B/~B ~B:~2..0B:~2..0B (tick ~B), "
		"real time: ~B/~B/~B ~B:~2..0B:~2..0B",
		[ SimDay, SimMonth, SimYear, SimHour, SimMinute, SimSecond, CurrentTick,
		  RealDay, RealMonth, RealYear, RealHour, RealMinute, RealSecond ] ).



% @doc Returns the current (numerical) simulation tick offset this actor is in,
% relatively to the simulation initial time (initial_tick), expressed in
% simulation ticks.
%
-spec get_current_tick_offset( wooper:state() ) -> tick_offset().
get_current_tick_offset( State ) ->
	?getAttr(current_tick_offset).



% @doc Returns the current diasca.
%
% Note: this function is defined for completeness, we see little reason for the
% user code to call it (except for debugging purposes).
%
-spec get_current_diasca( wooper:state() ) -> diasca().
get_current_diasca( State ) ->
	?getAttr(current_diasca).



% @doc Returns the current logical timestamp, that is a tick offset and diasca
% pair.
%
% Note: this function is defined for completeness, we see little reason for the
% user code to call it (except for debugging purposes), as the actual value of a
% diasca should remain opaque.
%
-spec get_current_logical_timestamp( wooper:state() ) -> logical_timestamp().
get_current_logical_timestamp( State ) ->
	{ ?getAttr(current_tick_offset), ?getAttr(current_diasca) }.



% @doc Returns the current simulation timestamp (that is absolute date and time,
% with a 1-second accuracy) at which this actor is.
%
% Note that, depending on the simulation frequency, the timestamp granularity
% might be finer or coarser than the one of ticks.
%
% (helper function)
%
-spec get_current_timestamp( wooper:state() ) -> timestamp().
get_current_timestamp( State ) ->
	CurrentTick = get_current_tick( State ),
	CurrentSecond = convert_ticks_to_seconds( CurrentTick, State ),
	calendar:gregorian_seconds_to_datetime( round( CurrentSecond ) ).



% @doc Returns, as the string, the current simulation timestamp (that is the
% absolute date and time, with a 1-second accuracy), at which this actor is.
%
% Note that, depending on the simulation frequency, the timestamp granularity
% might be finer or coarser than the one of ticks.
%
% (helper function)
%
-spec get_current_timestamp_as_string( wooper:state() ) -> ustring().
get_current_timestamp_as_string( State ) ->
	time_utils:get_textual_timestamp( get_current_timestamp( State ) ).



% @doc Converts the specified absolute tick into a tick offset.
%
% (helper function)
%
-spec convert_absolute_tick_to_tick_offset( tick(), wooper:state() ) ->
												tick_offset().
convert_absolute_tick_to_tick_offset( AbsoluteTick, State ) ->
	AbsoluteTick - ?getAttr(initial_tick).



% @doc Converts the specified timestamp (that is absolute date and time) into a
% tick offset.
%
% Note that, depending on the simulation frequency, the timestamp granularity
% might be finer or coarser than the one of ticks.
%
% (helper function)
%
-spec convert_timestamp_to_tick_offset( timestamp(), wooper:state() ) ->
												tick_offset().
convert_timestamp_to_tick_offset( Timestamp, State )
  when is_tuple( Timestamp ) ->

	%trace_utils:debug_fmt( "Converting to tick offset timestamp '~p'.",
	%						[ Timestamp ] ),

	Seconds = calendar:datetime_to_gregorian_seconds( Timestamp ),

	AbsoluteTick = convert_seconds_to_ticks( Seconds, State ),

	convert_absolute_tick_to_tick_offset( AbsoluteTick, State );

convert_timestamp_to_tick_offset( Timestamp, _State ) ->
	throw( { invalid_timestamp, Timestamp } ).



% @doc Converts the specified tick offset in a corresponding timestamp (that is
% absolute date and time), with an accuracy of one second.
%
% Note that, depending on the simulation frequency, the timestamp granularity
% might be finer or coarser than the one of ticks.
%
% (helper function)
%
-spec convert_tick_offset_to_timestamp( tick_offset(), wooper:state() ) ->
												timestamp().
convert_tick_offset_to_timestamp( TickOffset, State ) ->

	Tick = ?getAttr(initial_tick) + TickOffset,

	Second = convert_ticks_to_seconds( Tick, State ),

	calendar:gregorian_seconds_to_datetime( round( Second ) ).



% @doc Converts the specified tick offset in a corresponding textual timestamp
% (that is absolute date and time, as a string), with an accuracy of one second.
%
% Note that, depending on the simulation frequency, the timestamp granularity
% might be finer or coarser than the one of ticks.
%
% (helper function)
%
-spec convert_tick_offset_to_timestamp_as_string( tick_offset(),
												  wooper:state() ) -> ustring().
convert_tick_offset_to_timestamp_as_string( TickOffset, State ) ->

	Timestamp = convert_tick_offset_to_timestamp( TickOffset, State ),

	time_utils:get_textual_timestamp( Timestamp ).



% @doc Converts the specified tick offset in a corresponding timestamp (that is
% absolute date and time), with an accuracy of one second.
%
% Note that, depending on the simulation frequency, the timestamp granularity
% might be finer or coarser than the one of ticks.
%
% (helper function)
%
-spec convert_tick_offset_to_timestamp_explicit( tick_offset(), tick(),
											tick_duration() ) -> timestamp().
convert_tick_offset_to_timestamp_explicit( TickOffset, InitialTick,
										   TickDuration ) ->

	Tick = InitialTick + TickOffset,

	Second = convert_ticks_to_seconds_explicit( Tick, TickDuration ),

	calendar:gregorian_seconds_to_datetime( round( Second ) ).



% @doc Converts the specified tick offset in a corresponding textual timestamp
% (that is absolute date and time, as a string), with an accuracy of one second.
%
% Note that, depending on the simulation frequency, the timestamp granularity
% might be finer or coarser than the one of ticks.
%
% (helper function)
%
-spec convert_tick_offset_to_timestamp_as_string_explicit( tick_offset(),
			tick(), tick_duration() ) -> ustring().
convert_tick_offset_to_timestamp_as_string_explicit( TickOffset, InitialTick,
													 TickDuration ) ->

	Timestamp = convert_tick_offset_to_timestamp_explicit( TickOffset,
							InitialTick, TickDuration ),

	time_utils:get_textual_timestamp( Timestamp ).




% @doc Returns the current (numerical) absolute (that is relative to year #0 of
% the Gregorian calendar) simulation tick this actor is in.
%
% Note: class_TraceEmitter:get_current_tick/1 could be used as well. This
% version, to be called only when this actor is synchronized, must be slightly
% faster.
%
% It has been defined in class_Actor as well as in class_TraceEmitter, as the
% former is a child class of the latter, and, from the point of view of the
% user, relying on class_Actor:get_current_tick is more intuitive than using its
% class_TraceEmitter counterpart (inheritance, which is here a technical detail,
% is hidden).
%
-spec get_current_tick( wooper:state() ) -> tick().
get_current_tick( State ) ->
	?getAttr(initial_tick) + ?getAttr(current_tick_offset).




% @doc Sends specified (actor) message to the specified actor, records this
% sending to wait for its acknowledgement, and returns an updated state. These
% inter-actor messages exchanged during simulation are the only allowed way of
% communicating between actors.
%
% An actor message parameter describes the behaviour (actor oneway, translating
% to an Erlang function) to trigger when this message will be taken into account
% by the targeted actor, once messages will have been properly reordered.
%
% This sent message corresponds to a oneway, not a request, to avoid any
% blocking operation, as the time management service must be the only one to
% control the course of the simulation.
%
% The sender PID is automatically added to the message, thus it does not need to
% be specified explicitly here. The sender AAI is also automatically added as
% well, as the receiver will need it to reorder its incoming actor messages.
%
% The specified diasca is the one expected for the delivery, i.e. the next
% diasca, hence the +1.
%
% The actor message is a oneway call: it is described by the name of the actor
% oneway to trigger on the target actor (specified as an atom, ex: 'setColor')
% on the next diasca, and by a (possibly empty) list of the corresponding
% arguments; so the call is either 'my_oneway' or
% '{my_oneway,SingleNonListParameter}' or '{my_oneway,[Arg1,...]}'.
%
% As mentioned, in all cases, the actual call resulting from the sending of this
% actor message will be performed with on additional parameter, which is the PID
% of the sending actor (its AAI will be sent as well, yet will not be available
% to the users). These extra parameters are transparently added, so an actor
% oneway which looks like a call to a oneway with N parameters specified will
% trigger a call to a function whose arity is N+2: the state, then the N
% parameters, then the PID of the sending actor (i.e.: in that order).
%
% So a typical call made by an actor whose PID is P1 to an actor P2 can be made
% thanks to the following actor message:
% NewState = class_Actor:send_actor_message( P2, {setColor,[red,15]}, AState )
%
% This would trigger on the target actor, setColor/4 on the next tick, as the
% PID of the sending actor is automatically added as last parameter:
% 'setColor( State, red, 15, P1 ) ->'.
%
% Note that:
% - an actor is allowed to send an actor message to itself
% - actor messages shall not be sent from a constructor; the first legit moment
% to do so is the onFirstDiasca/2 actor oneway
%
% Returns an updated state, appropriate to wait automatically for this call to
% be acknowledged.
%
% (helper function)
%
-spec send_actor_message( actor_pid(), oneway_call(), wooper:state() ) ->
								wooper:state().
send_actor_message( ActorPid, ActorOneway, State ) when is_pid( ActorPid ) ->

	cond_utils:if_defined( simdiasca_debug_model_behaviours,
	  trace_utils:debug_fmt( "~w sending an actor message to ~w at {~p,~p}: ~p",
			[ self(), ActorPid, ?getAttr(current_tick_offset),
			  ?getAttr(current_diasca), ActorOneway ] ) ),

	% The simulation shall be already started:
	cond_utils:if_defined( simdiasca_check_model_behaviours,
						   true = is_running( State ) ),

	ActorPid ! { receiveActorMessage,
				[ ?getAttr(current_tick_offset), ?getAttr(current_diasca) + 1,
				  ActorOneway, self(), ?getAttr(actor_abstract_id) ] },

	NewAction = case ?getAttr(next_action) of

		new_diasca_needed ->
			% Already good:
			new_diasca_needed;

		no_diasca_requested ->
			new_diasca_needed;

		{ terminating, Delay, _DiascaRequest } ->
			{ terminating, Delay, new_diasca_needed };

		% No test really necessary against terminated: we should not even be
		% scheduled in this case.
		%
		{ terminated, _DiascaRequest } ->
			throw( { no_message_sending_when_terminated, ActorPid,
					 ActorOneway } )

	end,

	% Here we know for sure that the next diasca will have to be scheduled,
	% since the actor message will have to be processed by the recipient.
	%
	% At least one time manager must be notified of that. The one of the actor
	% that receives the actor message should be avoided, since it may have
	% already finished its tick and answered to its parent manager (if any).
	%
	% Conversely, we know for sure that the time manager of this sending actor
	% is still waiting for the end of its tick.
	%
	% Therefore it is up to that sending actor to trigger the scheduling of the
	% next tick, once it will finish its own scheduling for this diasca.
	%
	% This works, as all time managers are notified of all diascas, regardless
	% of what they are to schedule.
	%
	setAttributes( State, [
		{ waited_acks, [ ActorPid | ?getAttr(waited_acks) ] },
		{ next_action, NewAction } ] );

send_actor_message( Unexpected, _ActorOneway, _State ) ->
	throw( { not_a_pid, Unexpected } ).



% @doc Sends specified message to the specified listed actors, records these
% sendings to wait for the corresponding acknowledgements, and returns an
% updated state. These inter-actor messages exchanged during simulation are the
% only allowed way of communicating between actors.
%
% An actor message parameter describes the behaviour (actor oneway, translating
% to an Erlang function) to trigger when this message will be taken into account
% by the targeted actor, once messages will have been properly reordered.
%
% This sent message corresponds to a oneway, not a request, to avoid any
% blocking operation, as the time management service must be the only one to
% control the course of the simulation.
%
% The sender PID is automatically added, thus it does not need to be specified
% explicitly here. The sender AAI is also automatically added as well, as the
% receiver will need it to reorder its incoming actor messages.
%
% The specified diasca is the one expected for the delivery, i.e. the next
% diasca, hence the +1.
%
% The actor message is a oneway call: it is described by the name of the actor
% oneway to trigger on the target actor (specified as an atom, ex: 'setColor')
% on the next tick, and by a (possibly empty) list of the corresponding
% arguments; so the call is either 'my_oneway' or
% '{my_oneway, SingleNonListParameter}' or '{my_oneway, [ Arg1, ...]}'.
%
% In all cases, the actual call, in the case of an actor message, will be
% performed with an additional parameter, the PID of the sending actor. This
% extra parameter will be transparently added, so an actor oneway which looks
% like a call to a oneway with N parameters specified will trigger a call to a
% function whose arity is N+2: the state, then the N parameters, then the PID of
% the sending actor (i.e.: in that order).
%
% So a typical call made by an actor whose PID is P1 to actors P2 and P3 can be
% made thanks to the following actor message:
%
% NewState = class_Actor:send_actor_messages( [ P2, P3 ], {setColor,[red,15]},
% AState )
%
% This would trigger on the target actors, setColor/4 on the next tick, as the
% PID of the sending actor is automatically added as last parameter:
%
% setColor( State, red, 15, P1 ) ->
%
% Note that:
% - an actor is allowed to send an actor message to itself
% - actor messages shall not be sent from a constructor; the first legit moment
% to do so is the onFirstDiasca/2 actor oneway
%
% Returns an updated state, appropriate to wait automatically for this call to
% be acknowledged.
%
% (helper function)
%
-spec send_actor_messages( [ actor_pid() ], oneway_call(), wooper:state() ) ->
									wooper:state().
send_actor_messages( _ActorPidList=[], _ActorOneway, State ) ->
	% No target, no state change wanted:
	State;

send_actor_messages( ActorPidList, ActorOneway, State ) ->

	cond_utils:if_defined( simdiasca_debug_model_behaviours,
		trace_utils:debug_fmt( "  ~w sending an actor message to ~w "
			"at {~p,~p}: ~p",
			[ self(), ActorPidList, ?getAttr(current_tick_offset),
			  ?getAttr(current_diasca), ActorOneway ] ) ),

	% The simulation shall be already started:
	cond_utils:if_defined( simdiasca_check_model_behaviours,
						   true = is_running( State ) ),

	ActorMessage = { receiveActorMessage,
				[ ?getAttr(current_tick_offset), ?getAttr(current_diasca)+1,
				  ActorOneway, self(), ?getAttr(actor_abstract_id) ] },

	[ ActorPid ! ActorMessage || ActorPid <- ActorPidList ],

	NewAction = case ?getAttr(next_action) of

		new_diasca_needed ->
			% Already good:
			new_diasca_needed;

		no_diasca_requested ->
			new_diasca_needed;

		{ terminating, Delay, _DiascaRequest } ->
			{ terminating, Delay, new_diasca_needed };

		% No test really necessary against terminated: we should not even be
		% scheduled in this case.
		%
		{ terminated, _DiascaRequest } ->
			throw( { no_message_sending_when_terminated, ActorPidList,
					 ActorOneway } )

	end,


	% Here we know for sure that the next diasca will have to be scheduled,
	% since the actor message will have to be processed by the recipient.
	%
	% At least one time manager must be notified of that. The one of the actor
	% that receives the actor message should be avoided, since it may have
	% already finished its tick and answered to its parent manager (if any).
	%
	% Conversely, we know for sure that the time manager of this sending actor
	% is still waiting for the end of its tick.
	%
	% Therefore it is up to that sending actor to trigger the scheduling of the
	% next tick, once it will finish its own scheduling for this diasca.
	%
	% This works, as all time managers are notified of all diascas, regardless
	% of what they are to schedule.
	%
	setAttributes( State, [
		{ waited_acks, ActorPidList ++ ?getAttr(waited_acks) },
		{ next_action, NewAction } ] ).



% @doc Executes the specified actor oneway of this actor: executes a (often
% inherited) actor oneway from the current actor oneway.
%
-spec execute_actor_oneway( oneway_name(), wooper:state() ) -> wooper:state().
execute_actor_oneway( Onewayname, State ) ->
	% Just a WOOPER call:
	executeOneway( State, Onewayname ).



% @doc Executes the specified actor oneway of this actor: executes a (often
% inherited) actor oneway from the current actor oneway.
%
-spec execute_actor_oneway( oneway_name(), method_arguments(),
							wooper:state() ) -> wooper:state().
execute_actor_oneway( Onewayname, MethodArgs, State ) ->
	% Just a WOOPER call:
	executeOneway( State, Onewayname, MethodArgs ).



% @doc Executes the specified actor oneway of this actor: executes, as specified
% class, an actor oneway from the current actor oneway.
%
-spec execute_actor_oneway_as( classname(), oneway_name(), wooper:state() ) ->
										wooper:state().
execute_actor_oneway_as( Classname, Onewayname, State ) ->
	% Just a WOOPER call:
	executeOnewayAs( State, Classname, Onewayname ).



% @doc Executes the specified actor oneway of this actor: executes, as specified
% class, an actor oneway from the current actor oneway.
%
-spec execute_actor_oneway_as( classname(), oneway_name(), method_arguments(),
							   wooper:state() ) -> wooper:state().
execute_actor_oneway_as( Classname, Onewayname, MethodArgs, State ) ->
	% Just a WOOPER call:
	executeOnewayAs( State, Classname, Onewayname, MethodArgs ).




% @doc Self-triggers specified actor message in the specified number of diascas:
% allows for a deferred actor oneway execution by this actor, through diascas in
% the current tick.
%
% If an actor executes this helper at {T,D}, the actual (self) execution of the
% specified trigger is to happen at {T,D+DiascaOffset} (hence the corresponding
% actor oneway will be auto-sent at {T,D+DiascaOffset-1}).
%
% Note: relying on diasca counts is generally prohibited, as it is hackish and
% error prone (execution order to come from the causality reflected by actor
% messages); it is only useful when wanting to address very specific corner
% cases, for example when needing, at a given physical time, to act depending to
% actor messages being received *or not* in the prior diascas (no causality
% chain to build upon, and not wanting to suffer from offsets in physical time).
%
-spec self_trigger_actor_message_in( diasca_count(), oneway_call(),
									 wooper:state() ) -> wooper:state().
self_trigger_actor_message_in( _DiascaOffset=1, ActorOneway, State ) ->

	cond_utils:if_defined( simdiasca_debug_model_behaviours,
		?info_fmt( "Self-sending actor oneway '~p' now.", [ ActorOneway ] ) ),

	send_actor_message( self(), ActorOneway, State );


self_trigger_actor_message_in( DiascaOffset, ActorOneway, State )
  when DiascaOffset > 1 ->

	cond_utils:if_defined( simdiasca_debug_model_behaviours,
		?debug_fmt( "Will trigger in ~B diascas following actor oneway: ~p.",
					[ DiascaOffset, ActorOneway ] ) ),

	% Reserved to avoid name clashes:
	SelfActorOneway={ 'sd_reserved_selfTrigger',
					  [ DiascaOffset-1, ActorOneway ] },

	send_actor_message( self(), SelfActorOneway, State );

self_trigger_actor_message_in( DiascaOffset, ActorOneway, _State ) ->
	throw( { invalid_diasca_offset_trigger, DiascaOffset, ActorOneway } ).



% Internal, reserved actor oneway to implement deferred oneway triggers.
%
% See self_trigger_actor_message_in/3.
%
% Note: defining such actir oneway (not a mere helper) is necessary.
%
-spec sd_reserved_selfTrigger( wooper:state(), diasca_count(), oneway_call(),
							   sending_actor_pid() ) -> actor_oneway_return().
sd_reserved_selfTrigger( State, DiascaCount, ActorOneway,
						 _SelfSendingActorPid ) ->

	TrigState = self_trigger_actor_message_in( DiascaCount, ActorOneway,
											   State ),

	actor:return_state( TrigState ).



% @doc Makes this actor search for its messages associated to its current tick
% and diasca, sorts them in a particular order, and requests to process them.
%
% Returns an updated state.
%
process_last_diasca_messages( CurrentTickOffset, CurrentDiasca, State ) ->

	% Messages are checked at reception, but another checking is nevertheless
	% performed here, as here we know for sure at which global tick and diasca
	% we are (no possible race condition here between an actor message and the
	% 'begin diasca' message):
	%
	{ CurrentMessages, NextMessages, PastMessages } =
		split_messages_over_time( ?getAttr(pending_messages),
								  CurrentTickOffset, CurrentDiasca ),


	% Exactly as discussed in receiveActorMessage/6, messages in the very next
	% future (i.e. the diasca just after this new one) are licit, because they
	% might have been received before this 'begin diasca' message (that we are
	% processing) if the sending actor got its own 'begin diasca' message and
	% executed quick enough to have this actor message received before.

	cond_utils:if_defined( simdiasca_check_time_management,

		begin

			check_future_messages( NextMessages, CurrentTickOffset,
								   CurrentDiasca ),

			% They come from the past (abnormal):
			case PastMessages of

				[] ->
					ok;

				_ ->
					?error_fmt( "There is at least one actor message in "
						"the past: ~p.", [ PastMessages ] ),
					throw( { actor_message_in_the_past, PastMessages } )

			end

		end ),

	ReorderedMessages = apply_reordering( CurrentMessages,
										  ?getAttr(message_ordering_mode) ),

	% Executes the actor messages in the relevant order:
	ExecutedState = execute_reordered_oneways( ReorderedMessages, State ),

	% Flushes all messages except the ones in the future:
	setAttribute( ExecutedState, pending_messages, NextMessages ).



% @doc Checks that actor messages in the future are legitimate:
check_future_messages( _FutureMessages=[], _CurrentTickOffset,
					   _CurrentDiasca ) ->
	ok;

% Abnormal cases:
check_future_messages( _FutureMessages=[ M=#actor_message{
   tick_offset=MessageOffset, diasca=MessageDiasca } | _T ],
					   CurrentTickOffset, CurrentDiasca )
  when MessageOffset > CurrentTickOffset
	   orelse MessageDiasca > CurrentDiasca + 1->

	throw( { message_from_unexpected_future, { MessageOffset, MessageDiasca },
			 { CurrentTickOffset, CurrentDiasca }, M } );

% Only correct case is same offset, next diasca (same diasca not possible -
% these are all future messages):
%
check_future_messages( _FutureMessages=[ #actor_message{
   tick_offset=CurrentTickOffset, diasca=MessageDiasca } | T ],
					   CurrentTickOffset, CurrentDiasca )
  when MessageDiasca =:= CurrentDiasca + 1 ->

	check_future_messages( T, CurrentTickOffset, CurrentDiasca ).



% @doc Sends in turn the specified oneways (here no third parameter is
% specified, like for requests, however the sender PID is specified in the
% parameters nevertheless, as last argument) to this same process. Appending it
% at end (rather than at beginning) is a bit more expensive, but it is far
% clearer for the implementor of the methods corresponding to the received actor
% messages.
%
% (AAI is ignored here, as not useful anymore here)
%
% Basically a three-clause fold:
%
execute_reordered_oneways( _Messages=[], State ) ->
	State;

% List parameter here:
execute_reordered_oneways( _Messages=[
	   { SenderPid, SenderAAI, { OnewayName, OnewayArgList } }
										| MessageTuples ],
		State ) when is_list( OnewayArgList ) ->

	FullArgs = list_utils:append_at_end( SenderPid, OnewayArgList ),

	NewState = try

				   executeOneway( State, OnewayName, FullArgs )

			   catch ExceptionClass:Exception:FullStackTrace ->

					% Not relevant anymore since 21.0:
					%
					% Strangely enough, the call must not be synchronous, as if
					% placed later in this clause we might have a stack trace
					% corresponding to io_lib_{format,pretty}:
					%
					%FullStackTrace = erlang:get_stacktrace(),

					% We try, as early as possible, to get all relevant
					% information from (and about) the sending actor:
					%
					SenderPid ! { getActorInfo, [], self() },

					ExceptionString = case Exception of

						undef ->

							{ M, F, Args, _Loc } = hd( FullStackTrace ),

							Arity = length( Args ),

							DiagnoseString =
								code_utils:interpret_undef_exception( M, F,
																	  Arity ),

							% Call needed for newline:
							text_utils:format( "undef exception raised: ~ts "
								"(full information about current code path and "
								"available BEAM files is available in the "
								"traces).~n", [ DiagnoseString ] );

						_ ->
							text_utils:format( "The exception is:~n  ~p~n",
											   [ Exception ] )

					end,

					UsrStackTraceString = get_user_stacktrace( FullStackTrace ),


					% For State and SenderPid:
					OnewayArity = length( OnewayArgList ) + 2,

					ActorClassname = wooper:get_classname( State ),

					LocString = locate_error( hd( FullStackTrace ),
									ActorClassname, OnewayName, OnewayArity ),

					SimLogicalTimestamp = { CurrentTickOffset, _CurrentDiasca }
						   = get_current_logical_timestamp( State ),

					SimUserTimestamp = convert_tick_offset_to_timestamp(
											CurrentTickOffset, State ),

					SimUserTimestampString =
						time_utils:get_textual_timestamp( SimUserTimestamp ),

					StateString = state_to_string( State ),

					FullStackTraceString =
						   code_utils:interpret_stacktrace( FullStackTrace ),

					DictString =
						   list_table:to_string( list_table:new( get() ) ),

					SenderInfoString =

						   receive

							  % AAI matched:
							  { wooper_result, #actor_info{
												  classname=SenderActorClass,
												  name=SenderActorName,
												  aai=SenderAAI } } ->
									text_utils:format( ", corresponding to "
										"a ~ts instance named '~ts'",
									   [ SenderActorClass, SenderActorName ] )

							% Time-out, sending actor does not seem to answer:
							after 5000 ->
									""
					end,

					CoreParamString = case OnewayArity - 2 of

						% No core parameter here:
						0 ->
							"with no core parameter specified (beyond initial "
							"state and final sending actor PID).";

						1 ->
							% By design a one-element list:
							text_utils:format( "and the single core parameter "
								"of the actor oneway was (initial state and "
								"final sending actor PID parameters "
								"being omitted):~n  ~p", OnewayArgList );

						CoreParamCount ->
							ArgStrings = [ text_utils:format( "~p", [ Arg ] )
											|| Arg <- OnewayArgList ],

							ArgString = text_utils:strings_to_enumerated_string(
											ArgStrings ),

							text_utils:format( "and the ~B core parameters of "
								"the actor oneway were (initial state and "
								"final sending actor PID parameters "
								"being omitted):~n  ~ts",
								[ CoreParamCount, ArgString ] )

					end,

					% Information about the failing actor:
					ActorName = ?getAttr(name),
					ActorPid = self(),
					ActorAAI = ?getAttr(actor_abstract_id),

					ErrorMessage = text_utils:format(
						"Actor oneway ~ts/~B failed (~ts~ts) "
						%"on node '~ts' "
						"for ~ts instance named '~ts' of PID ~w (AAI: ~B).~n"
						"~n~ts~n"
						"This actor message was sent by actor ~w (AAI: ~B~ts) "
						"~ts~n"
						"The simulation timestamp was ~p, corresponding to ~ts "
						"(wallclock time: ~ts), and the corresponding "
						"user-level stack trace of the crashing actor "
						"(on node '~ts') was: ~ts~n"
						"Its actor-specific state was made of the "
						"following ~ts",
						[ OnewayName, OnewayArity, ExceptionClass, LocString,
						  %node(),
						  ActorClassname, ActorName, ActorPid, ActorAAI,
						  ExceptionString, SenderPid, SenderAAI,
						  SenderInfoString, CoreParamString,
						  SimLogicalTimestamp, SimUserTimestampString,
						  time_utils:get_textual_timestamp(),
						  node(), UsrStackTraceString, StateString ] ),

					logger:error( ErrorMessage ),

					Beams = code_utils:list_beams_in_path(),

					KnownBeamString =
						text_utils:atoms_to_sorted_string( Beams ),

					BaseString = "Here are the fully detailed, internal extra "
						"information about the exception reported "
						"in the error trace just sent. The purpose of the "
						"current trace is to help any complex troubleshooting "
						"that could even involve the engine itself.~n",

					DebugMessage = case Exception of

						undef ->
							% Internal state and process dictionary are not
							% needed:
							%
							text_utils:format( BaseString ++
							  "Full stack trace is: ~ts~n~nThe ~ts~n~n"
							  "Consequently, following ~B BEAM files are "
							  "available (listed in alphabetical order): ~ts~n",
							  [ FullStackTraceString,
								code_utils:get_code_path_as_string(),
								length( Beams ), KnownBeamString ] );

						_ ->
							text_utils:format( BaseString ++
							  "Full stack trace is: ~ts~n~n~ts~n"
							  "Process dictionary here shown as a ~ts",
							  [ FullStackTraceString,
								wooper:state_to_string( State ), DictString ] )

					end,

					logger:debug( DebugMessage ),

					% Wait a bit (also for past error reported), as logger (at
					% least former error_logger) seems asynchronous:
					%
					system_utils:await_output_completion( 500 ),

					% Commented out to avoid duplicated output, should it
					% propagate to beginDiasca/3.

					% Less detailed:
					%throw( { actor_oneway_failed, ExceptionClass,
					%			ActorClassname, ActorName,
					%			{ ActorPid, ActorAAI },
					%			{ OnewayName, OnewayArity }, Exception } )

					% The 'ERROR REPORT' does not add much information:
					%erlang:error( oneway_failed )

					basic_utils:stop( _StatusCode=15 )

	end,

	execute_reordered_oneways( MessageTuples, NewState );


% Standalone parameter here:
execute_reordered_oneways( _Messages=[
	   { SenderPid, SenderAAI, { OnewayName, OnewaySingleNonListArg } }
									| MessageTuples ], State ) ->

	% Parameter put in a list:
	Oneway = { OnewayName, [ OnewaySingleNonListArg ] },

	execute_reordered_oneways( [ { SenderPid, SenderAAI, Oneway }
								 | MessageTuples ], State );

% No parameter here:
execute_reordered_oneways( _Messages=[
	   { SenderPid, SenderAAI, OnewayName } | MessageTuples ], State ) ->

	% Parameter is an empty list:
	Oneway = { OnewayName, [] },

	execute_reordered_oneways( [ { SenderPid, SenderAAI, Oneway }
								 | MessageTuples ], State ).



% @doc Returns the best description of the location found for that error.
%
% (we want to avoid specifying multiple times the same MFA if matching)

% Canonicalised:
locate_error( _StackTraceItem={ AnyActorClassname, AnyOnewayName, OnewayArgs,
								LocInfos },
			  ActorClassname, OnewayName, OnewayArity )
  when is_list( OnewayArgs ) ->
	locate_error( { AnyActorClassname, AnyOnewayName, length( OnewayArgs ),
					LocInfos },
				  ActorClassname, OnewayName, OnewayArity );

% (from here, OnewayArity is supposed to be an integer)
% MFA matching here:
locate_error( _StackTraceItem={ ActorClassname, OnewayName, OnewayArity,
								LocInfos },
			  ActorClassname, OnewayName, OnewayArity ) ->
	text_utils:format( " directly in this actor oneway~ts",
					   [ location_to_string( LocInfos ) ] );

% M matching here, FA not:
locate_error( _StackTraceItem={ ActorClassname, OnewayName, OnewayArity,
								LocInfos },
			  ActorClassname, _OtherOnewayName, _OtherOnewayArity ) ->
	text_utils:format( " local to its class, in ~ts/~B~ts",
			[ OnewayName, OnewayArity, location_to_string( LocInfos ) ] );

% Otherwise other function:
locate_error( _StackTraceItem={ ActorClassname, OnewayName, OnewayArity,
								LocInfos },
			  _OtherActorClassname, _OtherOnewayName, _OtherOnewayArity ) ->
	text_utils:format( " regarding ~ts:~ts/~B~ts",
		[ ActorClassname, OnewayName, OnewayArity,
		  location_to_string( LocInfos ) ] ).



% (helper)
location_to_string( [ { file, FilePath }, { line, Line } ] ) ->
	text_utils:format( ", refer to line ~B of ~ts",
					   [ Line, filename:basename( FilePath ) ] );

location_to_string( [] ) ->
	"";

% Catch-all:
location_to_string( Other ) ->
	text_utils:format( ", refer to ~p", [ Other ] ).



% @doc Returns the best user-level stacktrace we can produce, that is a purely
% user-level one, stripped from the engine internals.
%
% (helper)
%
-spec get_user_stacktrace( code_utils:stack_trace() ) -> ustring().
get_user_stacktrace( StackTrace ) ->

	% Typically we have:
	% 1. class_Foo:bar/n
	% [...]
	% k: class_Actor:wooper_effective_method_execution/4
	% k+1: class_Actor:executeOneway/3
	% k+2: class_Actor:execute_reordered_oneways/2
	% k+3: class_Actor:process_last_diasca_messages/3
	% k+4: class_Actor:beginDiasca/3
	% k+5: class_Foo:wooper_effective_method_execution/4
	% k+6: class_Foo:wooper_main_loop/1

	% A problem that may arise is that the actual oneway triggered cannot be
	% seen in the stacktrace, whereas it should be listed just above the line
	% #k, most probably because it is an (optimised out) tail call (the culprit
	% is at least generally not the filtering below).
	%
	% Not much can be done at this level, except tracing...

	FilteredTrace = filter_stacktrace( StackTrace, _Acc=[] ),
	%FilteredTrace = StackTrace,

	code_utils:interpret_stacktrace( FilteredTrace ).



% @doc Removes all calls from the k level (if found):
filter_stacktrace( _Trace=[], Acc ) ->
	lists:reverse( Acc );

% We ensure that, if ever WOOPER recursive calls could happen, we strip only the
% last part (depth from k to k+6):
%
filter_stacktrace( _Trace=[ { _Module=class_Actor,
							  _Function=wooper_effective_method_execution,
							  _Arity=4, _Infos } | T ], Acc )
	  when length( T ) =:= 6 ->
	% From that point (k), we drop all lower calls:
	lists:reverse( Acc );

filter_stacktrace( _Trace=[ StackItem | T ], Acc ) ->
	filter_stacktrace( T, [ StackItem | Acc ] ).





% Actor-side management.


% @doc Updates this actor's agenda for checking with specified future action.
%
% Returns an updated agenda.
%
-spec update_agenda_with( [ tick_offset() ], [ tick_offset() ], agenda() ) ->
								agenda().
update_agenda_with( AddedTicks, WithdrawnTicks, Agenda ) ->

	% We withdraw before adding, hence if a never-specified tick is to be added
	% and withdrawn at the same diasca, the operation will fail:
	%
	WithdrawAgenda = lists:foldl( fun( Tick, AccAgenda ) ->
							list_utils:delete_existing( Tick, AccAgenda )
								  end,
					_WithdrawAcc0=Agenda,
					_WithdrawList=WithdrawnTicks ),

	%lists:sort( list_utils:uniquify( WithdrawAgenda ++ AddedTicks ) ),

	lists:foldl( fun( Tick, AccAgenda ) ->
						insert_in_agenda( Tick, AccAgenda )
				 end,
				 _AddAcc0=WithdrawAgenda,
				 _AddList=AddedTicks ).



% @doc Inserts the specified tick offset into specified agenda.
insert_in_agenda( TickOffset, Agenda ) ->
	insert_in_agenda( TickOffset, Agenda, _FirstAgendaEntries=[] ).


insert_in_agenda( TickOffset, _Agenda=[], FirstAgendaEntries ) ->
	% Agenda exhausted, insert at end:
	lists:reverse( [ TickOffset | FirstAgendaEntries ] );

insert_in_agenda( TickOffset, [ TickOffset | _T ]=Agenda,
				  FirstAgendaEntries ) ->

	% Previously a given actor could not register more than once to the same
	% tick; as some users felt the need for it, now an actor can specify the
	% same future tick multiple times; this will not raise an exception anymore,
	% and will trigger only one scheduling of that actor during the tick that
	% was specified multiple times:
	%
	%throw( { tick_already_in_agenda, TickOffset,
	%	lists:reverse( FirstAgendaEntries ) ++ Agenda } );

	% So now we just ignore repeated tick declarations and return the original
	% agenda:
	%
	lists:reverse( FirstAgendaEntries ) ++ Agenda;

insert_in_agenda( TickOffset, [ H | _T ]=Agenda, FirstAgendaEntries )
		when H > TickOffset ->
	% We went past the point where this offset should be specified:
	lists:reverse( FirstAgendaEntries ) ++ [ TickOffset | Agenda ];

% Here TickOffset > H:
insert_in_agenda( TickOffset, [ H | T ], FirstAgendaEntries ) ->
		% (implied: 'when H < TickOffset ->')
	% We shall continue, still in lower offsets:
	insert_in_agenda( TickOffset, T, [ H | FirstAgendaEntries ] ).



% @doc Sorts the specified list of messages into three lists (returned as a
% triplet): the messages corresponding to the specified tick, the ones in the
% future of that tick, and the ones in its past.
%
% For messages corresponding to the current tick, the tick information is
% removed: instead of a 4-element tuple, a {Pid,Aai,Message} triplet is
% returned.
%
split_messages_over_time( Messages, CurrentTickOffset, CurrentDiasca ) ->
	split_messages_over_time( Messages, CurrentTickOffset, CurrentDiasca,
							  _CurrentOnes=[], _FutureOnes=[], _PastOnes=[] ).


% (helper)
split_messages_over_time( _Messages=[], _CurrentTickOffset, _CurrentDiasca,
						  CurrentOnes, FutureOnes, PastOnes ) ->
	{ CurrentOnes, FutureOnes, PastOnes };


split_messages_over_time(
		[ H=#actor_message{ tick_offset=MessageTickOffset } | T ],
		CurrentTickOffset, CurrentDiasca, CurrentOnes, FutureOnes, PastOnes )
  when MessageTickOffset < CurrentTickOffset ->
	split_messages_over_time( T, CurrentTickOffset, CurrentDiasca, CurrentOnes,
							  FutureOnes, [ H | PastOnes ] );

split_messages_over_time(
		[ H=#actor_message{ tick_offset=MessageTickOffset } | T ],
		CurrentTickOffset, CurrentDiasca, CurrentOnes, FutureOnes, PastOnes )
  when MessageTickOffset > CurrentTickOffset ->
	split_messages_over_time( T, CurrentTickOffset, CurrentDiasca,
							  CurrentOnes, [ H | FutureOnes ], PastOnes );


% Here 'when MessageTickOffset =:= CurrentTickOffset' is implied by
% pattern-matching, now taking into account the diascas:
%
split_messages_over_time(
		[ H=#actor_message{ diasca=MessageDiasca } | T ],
		CurrentTickOffset, CurrentDiasca, CurrentOnes, FutureOnes, PastOnes )
  when MessageDiasca < CurrentDiasca ->
	split_messages_over_time( T, CurrentTickOffset, CurrentDiasca,
							  CurrentOnes, FutureOnes, [ H | PastOnes ] );

split_messages_over_time(
		[ H=#actor_message{ diasca=MessageDiasca } | T ],
		CurrentTickOffset, CurrentDiasca, CurrentOnes, FutureOnes, PastOnes )
  when MessageDiasca > CurrentDiasca ->
	split_messages_over_time( T, CurrentTickOffset, CurrentDiasca,
							  CurrentOnes, [ H | FutureOnes ], PastOnes );


% Here 'when MessageTickOffset =:= CurrentTickOffset andalso MessageDiasca =:=
% CurrentDiasca' is implied by pattern-matching:
%
% (could be refactored to be the first clause, as it must be by far the most
% commonly used one)
%
split_messages_over_time( [ #actor_message{ sender_pid=Pid, sender_aai=Aai,
			actual_message=Message} | T ], CurrentTickOffset, CurrentDiasca,
						 CurrentOnes, FutureOnes, PastOnes ) ->
	% No timestamp kept in the 'current list':
	split_messages_over_time( T, CurrentTickOffset, CurrentDiasca,
			[ { Pid, Aai, Message } | CurrentOnes ], FutureOnes, PastOnes ).





% Section dedicated to the creation of actors.



% @doc Creates synchronously a new initial actor, whereas the simulation has not
% been started yet.
%
% This must be called only directly from test/simulation cases, to create the
% initial situation before the simulation time starts progressing.
%
% This is a synchronous operation, to ensure that no race condition occurs
% between the creation of initial actors and the start of the simulation.
%
% The new actor will be placed according to the default placement heuristic of
% the engine.
%
% Use create_actor/{3,4} or create_placed_actor/{4,5} whenever needing to create
% an actor in the course of the simulation (in that case, actors must be created
% by other actors only).
%
% The single method parameter is ActorClassname, which is the classname of the
% actor to create (ex: 'class_TestActor'). No specific actor construction
% parameter is used here.
%
% Returns the PID of the newly created (initial) actor, or throws an exception.
%
-spec create_initial_actor( classname() ) -> static_return( actor_pid() ).
create_initial_actor( ActorClassname ) ->

	% No guard needed here, as will be applied in create_initial_actor/3:

	% No checking that the simulation is not started yet is needed, as it will
	% be done load-balancer-side.

	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	ActorPid = create_initial_actor( ActorClassname,
					_ActorConstructionParameters=[], LoadBalancerPid ),

	wooper:return_static( ActorPid ).



% @doc Creates synchronously a new initial actor, whereas the simulation has not
% been started yet.
%
% This must be called only directly from test/simulation cases, to create the
% initial situation before the simulation time starts progressing.
%
% This is a synchronous operation, to ensure that no race condition occurs
% between the creation of initial actors and the start of the simulation.
%
% The new actor will be placed according to the default placement heuristic of
% the engine.
%
% Use create_actor/{3,4} or create_placed_actor/{4,5} whenever needing to create
% an actor in the course of the simulation (in that case, actors must be created
% by other actors only).
%
% Method parameters are:
%
% - ActorClassname is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ]), knowing that the first two
% actual parameters (the initial state and the actor settings) are automatically
% added by the engine
%
% Returns the PID of the newly created (initial) actor, or throws an exception.
%
-spec create_initial_actor( classname(), [ method_argument() ] ) ->
									static_return( actor_pid() ).
create_initial_actor( ActorClassname, ActorConstructionParameters ) ->

	% No guard needed here, as will be applied in create_initial_actor/3:

	% No checking that the simulation is not started yet is needed, as it will
	% be done load-balancer-side.

	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	ActorPid = create_initial_actor( ActorClassname,
					ActorConstructionParameters, LoadBalancerPid ),

	wooper:return_static( ActorPid ).



% @doc Creates synchronously a new initial actor, whereas the simulation has not
% been started yet.
%
% Behaves exactly like the create_initial_actor/2 static method, except it
% relies on a user-specified PID for the load balancer.
%
% It is useful whenever having a large number of initial actors to create, as it
% allows to retrieve the load balancer PID only once for all, instead of having
% each create_initial_actor/2 performing a useless look-up for it.
%
% This static method is synchronous (blocking) to help the caller (typically the
% simulation case) avoiding race conditions with the start of the simulation;
% however creations are internally asynchronous (to support their nesting), and
% could be requested in a row and their acknowledgement waited later (provided
% the caller keeps track of the order of the requested creations).
%
% Returns the PID of the newly created (initial) actor, or throws an exception.
%
-spec create_initial_actor( classname(), [ method_argument() ],
						load_balancer_pid() ) -> static_return( actor_pid() ).
create_initial_actor( ActorClassname, ActorConstructionParameters,
					  LoadBalancerPid )
  when is_atom( ActorClassname )
	   andalso is_list( ActorConstructionParameters )
	   andalso is_pid( LoadBalancerPid ) ->

	% No checking that the simulation is not started yet is needed, as it will
	% be done load-balancer-side.

	cond_utils:if_defined( simdiasca_debug_initial_creations,
		trace_utils:debug_fmt( "~w requesting the creation of an initial actor "
			"named '~ts'.", [ self(), ActorClassname ] ) ),

	% Oneway call (to keep the exchanges asynchronous):

	LoadBalancerPid ! { createInitialActor,
		[ ActorClassname, ActorConstructionParameters, self() ] },

	cond_utils:if_defined( simdiasca_debug_initial_creations,
		trace_utils:debug_fmt( "~w waiting for the creation acknowledgment.",
							   [ self() ] ) ),

	receive

		{ onInitialActorCreated, ActorPid } -> % when is_pid( ActorPid ) ->

			cond_utils:if_defined( simdiasca_debug_initial_creations,
				trace_utils:debug_fmt( "~w received creation acknowledgment "
					"for initial actor ~w.", [ self(), ActorPid ] ) ),

			wooper:return_static( ActorPid )

	end.



% @doc Creates synchronously a new, placed, initial actor, whereas the
% simulation has not been started yet.
%
% This must be called only directly from test/simulation cases, to create the
% initial situation before the simulation time starts to progress.
%
% This is a synchronous operation, to ensure that no race condition occurs
% between the creation of initial actors and the start of the simulation.
%
% The new actor will be placed by the engine according to the specified
% placement hint: all actors created with a given placement hint are guaranteed
% to be placed on the same computing node, automatically selected approriately
% by the engine.
%
% This allows to ensure that the actors that are the most tightly linked are
% co-allocated and thus, being in the same node, interact with as little
% overhead as possible.
%
% Use create_actor/3 whenever needing to create an actor in the course of the
% simulation (in that case, actors must be created by other actors only).
%
% Method parameters are:
%
% - ActorClassname is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% - PlacementHint can be any Erlang term (ex: an atom); it allows to create all
% actors (both initial or simulation-time ones) for which the same placement
% hint was specified on the same computing node, for best performances
%
% Returns the PID of the newly created (initial) actor, or throws an exception.
%
-spec create_initial_placed_actor( classname(), [ method_argument() ],
		class_LoadBalancer:placement_hint() ) -> static_return( actor_pid() ).
create_initial_placed_actor( ActorClassname, ActorConstructionParameters,
							 PlacementHint ) ->

	% No guard needed here, as will be applied in create_initial_placed_actor/4:

	% No checking that the simulation is not started yet is needed, as it will
	% be done load-balancer-side.

	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	ActorPid = create_initial_placed_actor( ActorClassname,
				ActorConstructionParameters, LoadBalancerPid, PlacementHint ),

	wooper:return_static( ActorPid ).



% @doc Creates synchronously a new initial actor, whereas the simulation has not
% been started yet.
%
% Behaves exactly like the create_initial_placed_actor/3 static method, except
% it relies on a user-specified PID for the load balancer.
%
% It is useful whenever having a large number of initial actors to create, as it
% allows to retrieve the load balancer PID only once for all, instead of having
% each create_initial_actor/2 performing a useless look-up for it.
%
% This static method is specific to the Sim-Diasca distributed version (as the
% load balancer is involved).
%
% Returns the PID of the newly created (initial) actor, or throws an exception.
%
-spec create_initial_placed_actor( classname(), [ method_argument() ],
			load_balancer_pid(), class_LoadBalancer:placement_hint() ) ->
											static_return( actor_pid() ).
create_initial_placed_actor( ActorClassname, ActorConstructionParameters,
							 LoadBalancerPid, PlacementHint )
  when is_atom( ActorClassname ) andalso is_list( ActorConstructionParameters )
	   andalso is_pid( LoadBalancerPid ) ->

	% No checking that the simulation is not started yet is needed, as it will
	% be done load-balancer-side.

	cond_utils:if_defined( simdiasca_debug_initial_creations,
		trace_utils:debug_fmt( "~w requesting the creation of an initial "
			"placed actor named '~ts' with hint '~p'.",
			[ self(), ActorClassname, PlacementHint ] ) ),

	% Oneway call (to keep the exchanges asynchronous):

	LoadBalancerPid ! { createInitialPlacedActor, [ ActorClassname,
				ActorConstructionParameters, PlacementHint, self() ] },

	%trace_utils:debug_fmt(
	%     "~w waiting for the placed creation acknowledgment.", [ self() ] ),

	receive

		{ onInitialActorCreated, ActorPid } -> % when is_pid( ActorPid ) ->

			cond_utils:if_defined( simdiasca_debug_initial_creations,
				trace_utils:debug_fmt( "~w received creation acknowledgment "
					"for initial placed actor ~w.", [ self(), ActorPid ] ) ),

			wooper:return_static( ActorPid )

	end.



% @doc Creates synchronously - and in parallel - the specified list of new
% initial actors, whereas the simulation has not been started yet.
%
% Each actor is created based on the specified class name and construction
% parameters, possibly augmented of a placement hint. For example: {class_X,
% [P1, P2]} or {class_Y, [34], my_hint}.
%
% Returns the list of the PID of the newly created (initial) actors (in the
% order of their specification in the input list), or throws an exception.
%
-spec create_initial_actors( [ instance_creation_spec() ] ) ->
											static_return( [ actor_pid() ] ).
create_initial_actors( ActorConstructionList ) ->

	% No guard needed here, as will be applied in create_initial_actor/3:

	% No checking that the simulation is not started yet is needed, as it will
	% be done load-balancer-side.

	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	ActorPids = create_initial_actors( ActorConstructionList, LoadBalancerPid ),

	wooper:return_static( ActorPids ).



% @doc Creates synchronously - and in parallel - the specified list of new
% initial actors, whereas the simulation has not been started yet.
%
% Each actor is created based on the specified class name and construction
% parameters, possibly augmented of a placement hint. For example: {class_X,
% [P1, P2]} or {class_Y, [34], my_hint}.
%
% Returns the list of the PIDs of the newly created (initial) actors (in the
% order of their specification in the input list), or throws an exception.
%
% Behaves exactly like the create_initial_actors/1 static method, except it
% relies on a user-specified PID for the load balancer.
%
% It is useful whenever having a large number of initial actors to create, as it
% allows to retrieve the load balancer PID only once for all, instead of having
% each create_initial_actor/2 performing a useless look-up for it.
%
-spec create_initial_actors( [ instance_creation_spec() ],
				load_balancer_pid() ) -> static_return( [ actor_pid() ] ).
create_initial_actors( ActorConstructionList, LoadBalancerPid )
  when is_list( ActorConstructionList ) andalso is_pid( LoadBalancerPid ) ->

	% No checking that the simulation is not started yet is needed, as it will
	% be done load-balancer-side.

	cond_utils:if_defined( simdiasca_debug_initial_creations, [

	   ActorLines = [ text_utils:format( "~p", [ CP ] )
						|| CP <- ActorConstructionList ],

		trace_utils:debug_fmt( "~w requesting the creation of ~B initial "
			"actors, construction parameters being: ~ts",
			[ self(), length( ActorConstructionList ),
			  text_utils:strings_to_string( ActorLines ) ] ) ] ),

	LoadBalancerPid ! { createInitialActors,
						[ ActorConstructionList, self() ] },

	cond_utils:if_defined( simdiasca_debug_initial_creations,
		trace_utils:debug_fmt(
			"~w waiting for the acknowledgment of the ~B creations.",
			[ self(), length( ActorConstructionList ) ] ) ),

	receive

		{ onInitialActorsCreated, [ ActorPids ] } -> % when is_list( ActorPids )

			cond_utils:if_defined( simdiasca_debug_initial_creations,
				trace_utils:debug_fmt( "~w received creation acknowledgment "
					"for ~B actors: ~w.",
					[ self(), length( ActorPids ), ActorPids ] ) ),

			wooper:return_static( ActorPids )

	end.




% Helper functions.


% @doc Returns the name of this actor, as a binary.
%
% Note: is never and cannot be overridden.
%
% Allows to mask the inheritance from TraceEmitter.
%
% (const helper)
%
-spec get_name( wooper:state() ) -> class_Actor:internal_name().
get_name( State ) ->
	?getAttr(name).



% @doc Returns true iff this actor is running, that is if it has been
% synchronised to the simulation, that thus has already been started.
%
% Note: there are very few legitimate reasons for using this helper. Hint: do
% not try to abuse the engine, for example by special-casing initial actors
% (instead onFirstDiasca/2 shall be relied upon in all cases).

% (const helper)
%
-spec is_running( wooper:state() ) -> boolean().
is_running( State ) ->

	% simulationStarted/3 is the only one to set it:
	case ?getAttr(initial_tick) of

		undefined ->
			false;

		_Other ->
			true

	end.



% @doc Triggers the creation of a new actor, while the simulation is running,
% with no user tag specified.
%
% This is an actor-level helper function, which takes care automatically of the
% sending (at this tick T, diasca D) of the createActor actor message to the
% load balancer, which in turn, at the next diasca (still during T, at diasca
% D+1), will effectively create the targeted actor (with a remote synchronous
% timed new) until, at diasca D+2 of the same tick T, this creating actor has
% its onActorCreated/4 method called with the PID of the newly created actor;
% unlike with initial creations (which can return directly the PID of the newly
% created instance), the creation tag (by default a pair made of the classname
% and the list of construction parameters) is returned to the caller so that it
% can establish to which creation each callback corresponds.
%
% The load balancer is to create actors based on actor messages, for
% reproducibility reasons.
%
% The actual placement of the created actor is fully determined by the load
% balancer.
%
% Parameters are:
%
% - ActorClassname is the classname of the actor to create (ex: the
% 'class_TestActor' atom)
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% Note: if wanting to create an actor before the simulation is started, then
% create_initial_actor/{1,2,3} or create_initial_placed_actor/{2,3,4} must be
% used instead.
%
% Returns an updated state, and will trigger a callback to the onActorCreated/4
% actor oneway of this creating actor when the created one will be ready.
%
% (exported helper function, to be used by actual actors)
%
-spec create_actor( classname(), [ method_argument() ], wooper:state() ) ->
						wooper:state().
create_actor( ActorClassname, ActorConstructionParameters, State )
  when is_atom( ActorClassname )
	   andalso is_list( ActorConstructionParameters ) ->

	% The checking that the simulation is already running is done in
	% send_actor_message/3.

	%?info_fmt( "Actor '~ts' (~w) creating at runtime a new instance of ~p.",
	%			[ ?getAttr(name), self(), ActorClassname ] ),

	cond_utils:if_defined( simdiasca_debug_runtime_creations,
		trace_utils:info_fmt(
			"Actor '~ts' (~w) creating at runtime a new instance of ~p.",
			[ ?getAttr(name), self(), ActorClassname ] ) ),

	% The load balancer is an actor, thus two diascas will be requested.

	% No user tag specified, thus using implicitly a tag equal to:
	% {ActorClassname, ActorConstructionParameters}.
	%
	% Will trigger back, on this actor, onActorCreated/4 in two diascas:
	%
	send_actor_message( ?getAttr(load_balancer_pid),
		{ createRuntimeActor, [ ActorClassname, ActorConstructionParameters ] },
		State ).



% @doc Triggers the creation of a new actor, while the simulation is running,
% using the specified user tag for that.
%
% This is an actor-level helper function, which takes care automatically of the
% sending (at this tick T, diasca D) of the createActor actor message to the
% load balancer, which in turn, at the next diasca (still during T, at diasca
% D+1), will effectively create the targeted actor (with a remote synchronous
% timed new) until, at diasca D+2 of the same tick T, this creating actor has
% its onActorCreated/4 method called with the PID of the newly created actor;
% unlike with initial creations (which can return directly the PID of the newly
% created instance), the creation tag (by default a pair made of the classname
% and the list of construction parameters) is returned to the caller so that it
% can establish to which creation each callback corresponds.
%
% The load balancer is to create actors based on actor messages, for
% reproducibility reasons.
%
% The actual placement of the created actor is fully determined by the load
% balancer.
%
% Parameters are:
%
% - ActorClassname is the classname of the actor to create (ex:
% the 'class_TestActor' atom)
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: ["MyActorName", 50])
%
% - CreatedActorTag can be any term chosen by the user (ex: an atom like
% 'my_building_147')
%
% Note: if wanting to create an actor before the simulation is started, then
% create_initial_actor/{2,3} or create_initial_placed_actor/{3,4} must be used
% instead.
%
% Returns an updated state, and will trigger a callback to the onActorCreated/4
% actor oneway of this creating actor when the created one will be ready.
%
% (exported helper function, to be used by actual actors)
%
-spec create_actor( classname(), [ method_argument() ], tag(),
					wooper:state() ) -> wooper:state().
create_actor( ActorClassname, ActorConstructionParameters, ActorTag, State )
  when is_atom( ActorClassname )
	   andalso is_list( ActorConstructionParameters ) ->

	% The checking that the simulation is already running is done in
	% send_actor_message/3.

	cond_utils:if_defined( simdiasca_debug_runtime_creations,
		?info_fmt( "Actor '~ts' (~w) creating at runtime a new instance of ~p "
			"with user tag '~p'.",
			[ ?getAttr(name), self(), ActorClassname, ActorTag ] ) ),

	% The load balancer is an actor, thus two diascas will be requested.

	% Will trigger back, on this actor, onActorCreated/4 in two diascas:
	send_actor_message( ?getAttr(load_balancer_pid),
		{ createRuntimeActor,
			[ ActorClassname, ActorConstructionParameters, ActorTag ] },
		State ).



% @doc Triggers the creation of a set of new actors, while the simulation is
% running, each instance creation being able to include a creation tag.
%
% This is an actor-level helper function, which takes care automatically of the
% sending (at this tick T, diasca D) of the createActor actor messages to the
% load balancer, which in turn, at the next diasca (still during T, at diasca
% D+1), will effectively create the targeted actors (with a remote synchronous
% timed new) until, at diasca D+2 of the same tick T, this creating actor
% receives as many calls to its onActorCreated/4 method as it had actors to
% created, each of these calls specifying the PID of one of the newly created
% actor; unlike with initial creations (which can return directly the PID of the
% newly created instance), the creation tag (by default a pair made of the
% classname and the list of construction parameters) is returned to the caller
% so that it can establish to which creation each callback corresponds.
%
% The load balancer is to create actors based on actor messages, for
% reproducibility reasons.
%
% The actual placement of the created actors is fully determined by the load
% balancer.
%
% Parameters are:
%
% - ActorConstructionList is a list of instance creation specifications (tagged
% or not)
%
% - State: the initial state
%
% Returns an updated state, and will trigger one callback to the
% onActorCreated/4 actor oneway of this creating actor when each created one
% will be ready.
%
% (exported helper function, to be used by actual actors)
%
-spec create_actors(
		[ instance_creation_spec() | tagged_instance_creation_spec() ],
		wooper:state() ) -> wooper:state().
create_actors( ActorConstructionList, State )
  when is_list( ActorConstructionList ) ->

	% Note: not tested yet, beware!

	% The checking that the simulation is already running is done in
	% send_actor_message/3.

	cond_utils:if_defined( simdiasca_debug_runtime_creations,
		?info_fmt( "Actor '~ts' (~w) creating at runtime ~B new actors, "
			"whose creation list is: ~ts",
			[ ?getAttr(name), self(), length( ActorConstructionList ),
			  text_utils:strings_to_string( [ text_utils:format( "~p",
					[ Spec ] ) || Spec <- ActorConstructionList ] ) ] ) ),

	LoadBalancerPid = ?getAttr(load_balancer_pid),

	% The load balancer is an actor, thus two diascas will be requested.

	% No user tag specified, thus using implicitly a tag equal to:
	% {ActorClassname, ActorConstructionParameters}.
	%
	% Will trigger back, on this actor, as many calls to onActorCreated/4 in two
	% diascas:
	%
	lists:foldl( fun( Spec, AccState ) ->
					send_actor_message( LoadBalancerPid,
						{ createRuntimeActor, Spec }, AccState )
				 end,
				 State, ActorConstructionList ).



% @doc Triggers the creation of a new actor with a placement hint, while the
% simulation is running.
%
% This is an actor-level helper function, which takes care automatically of the
% sending (at this tick T, diasca D) of the createActor actor message to the
% load balancer, which in turn, at the next diasca (still during T, at diasca
% D+1), will effectively create the targeted actor (with a remote synchronous
% timed new) until, at diasca D+2 of the same tick T, this creating actor has
% its onActorCreated/4 method called with the PID of the newly created actor.
%
% The load balancer is to create actors based on actor messages, for
% reproducibility reasons.
%
% The actual placement of the created actor is fully determined by the specified
% placement hint.
%
% Helper parameters are:
%
% - ActorClassname is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: ["MyActorName", 50])
%
% - PlacementHint can be any Erlang term (ex: an atom); it allows to create all
% actors (both initial or simulation-time ones) for which the same placement
% hint was specified on the same computing node, for best performances
%
% Note: if wanting to create an actor before the simulation is started, then
% create_initial_actor/{2,3} or create_initial_placed_actor/{3,4} must be used
% instead.
%
% Returns an updated state, and will trigger a callback to the onActorCreated/4
% actor oneway of this creating actor when the created one will be ready.
%
% (exported helper function, used by actors)
%
-spec create_placed_actor( classname(), [ method_argument() ],
	  class_LoadBalancer:placement_hint(), wooper:state() ) -> wooper:state().
create_placed_actor( ActorClassname, ActorConstructionParameters,
					 PlacementHint, State )
  when is_atom( ActorClassname )
	   andalso is_list( ActorConstructionParameters ) ->

	% The checking that the simulation is already running is done in
	% send_actor_message/3.

	% Will trigger back, on this actor, onActorCreated/4 in two diascas:
	send_actor_message( ?getAttr(load_balancer_pid),
		{ createRuntimePlacedActor, [ ActorClassname,
					ActorConstructionParameters, PlacementHint ] }, State ).



% @doc Triggers the creation of a new actor with a user-defined tag and a
% placement hint, while the simulation is running.
%
% This is an actor-level helper function, which takes care automatically of the
% sending (at this tick T, diasca D) of the createActor actor message to the
% load balancer, which in turn, at the next diasca (still during T, at diasca
% D+1), will effectively create the targeted actor (with a remote synchronous
% timed new) until, at diasca D+2 of the same tick T, this creating actor has
% its onActorCreated/4 method called with the PID of the newly created actor.
%
% The load balancer is to create actors based on actor messages, for
% reproducibility reasons.
%
% The actual placement of the created actor is fully determined by the specified
% placement hint.
%
% Helper parameters are:
%
% - ActorClassname is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: ["MyActorName", 50])
%
% - CreatedActorTag can be any term chosen by the user (ex: an atom like
% 'my_building_147')
%
% - PlacementHint can be any Erlang term (ex: an atom); it allows to create all
% actors (both initial or simulation-time ones) for which the same placement
% hint was specified on the same computing node, for best performances
%
% Note: if wanting to create an actor before the simulation is started, then
% create_initial_actor/{2,3} or create_initial_placed_actor/{3,4} must be used
% instead.
%
% Returns an updated state, and will trigger a callback to the onActorCreated/4
% actor oneway of this creating actor when the created one will be ready.
%
% (exported helper function, used by actors)
%
-spec create_placed_actor( classname(), [ method_argument() ], tag(),
			class_LoadBalancer:placement_hint(), wooper:state() ) ->
								wooper:state().
create_placed_actor( ActorClassname, ActorConstructionParameters, ActorTag,
					 PlacementHint, State )
  when is_atom( ActorClassname )
	   andalso is_list( ActorConstructionParameters ) ->

	% The checking that the simulation is already running is done in
	% send_actor_message/3.

	% Will trigger back, on this actor, onActorCreated/4 in two diascas:
	send_actor_message( ?getAttr(load_balancer_pid),
		{ createRuntimePlacedActor, [ ActorClassname,
			ActorConstructionParameters, ActorTag, PlacementHint ] }, State ).



% @doc Allows an actor to declare a probe, whose timestamps will be expressed in
% ticks, with specified parameters.
%
% The probe creation may or may not be accepted by the result manager.
%
% If yes, PID of the newly created probe will be returned.
%
% If no, the 'non_wanted_probe' atom will be returned.
%
% (helper function)
%
-spec declare_probe( class_Probe:name_options(),
		[ class_Probe:declared_curve_name() ], [ class_Probe:declared_zone() ],
		title(), maybe( label() ), label() ) -> class_Probe:probe_ref().
declare_probe( NameOptions, CurveNames, Zones, Title, MaybeXLabel, YLabel ) ->

	% From an actor, any created probe will write its files under the directory
	% for temporary data (by default under '/tmp'), the current directory of all
	% computing nodes:
	%
	class_Probe:declare_result_probe( NameOptions, CurveNames, Zones, Title,
									  MaybeXLabel, YLabel ).



% @doc Allows an actor to declare a probe, whose timestamps will be expressed as
% actual times and dates, with specified parameters.
%
% The probe creation may or may not be accepted by the result manager.
%
% If yes, PID of the newly created probe will be returned.
%
% If no, the 'non_wanted_probe' atom will be returned.
%
% Note that the specified state shall be the one of a class_Actor instance.
%
% (helper function)
%
-spec declare_probe( class_Probe:name_options(),
		[ class_Probe:declared_curve_name() ], [ class_Probe:declared_zone() ],
		title(), maybe( label() ), label(),
		virtual_seconds() | wooper:state() ) -> class_Probe:probe_ref().
declare_probe( NameOptions, CurveNames, Zones, Title, MaybeXLabel, YLabel,
			   TickDuration ) when is_float( TickDuration ) ->
	class_Probe:declare_result_probe( NameOptions, CurveNames, Zones, Title,
									  MaybeXLabel, YLabel, TickDuration );

declare_probe( NameOptions, CurveNames, Zones, Title, MaybeXLabel, YLabel,
			   State ) ->

	% If this fails, the specified state must not be the one of an actor:
	TickDuration = ?getAttr(simulation_tick_duration),

	% From an actor, any created probe will write its files under the directory
	% for temporary data (by default under '/tmp'), the current directory of all
	% computing nodes:
	%
	class_Probe:declare_result_probe( NameOptions, CurveNames, Zones, Title,
									  MaybeXLabel, YLabel, TickDuration ).



% @doc Allows an actor to declare a probe, whose timestamps will be expressed as
% actual times and dates, with specified parameters and extra settings.
%
% The probe creation may or may not be accepted by the result manager.
%
% If yes, PID of the newly created probe will be returned.
%
% If no, the 'non_wanted_probe' atom will be returned.
%
% Note that the specified state shall be the one of a class_Actor instance.
%
% (helper function)
%
-spec declare_probe( class_Probe:name_options(),
		[ class_Probe:declared_curve_name() ], [ class_Probe:declared_zone() ],
		title(), maybe( label() ), label(),
		class_Probe:settings_table(), virtual_seconds() | wooper:state() ) ->
							class_Probe:probe_ref().
declare_probe( NameOptions, CurveNames, Zones, Title, MaybeXLabel, YLabel,
		ExtraSettingsTable, TickDuration ) when is_float( TickDuration ) ->

	class_Probe:declare_result_probe( NameOptions, CurveNames, Zones, Title,
		MaybeXLabel, YLabel, ExtraSettingsTable, TickDuration );

declare_probe( NameOptions, CurveNames, Zones, Title, MaybeXLabel, YLabel,
			   ExtraSettingsTable, State ) ->

	% If this fails, the specified state must not be the one of an actor:
	TickDuration = ?getAttr(simulation_tick_duration),

	% From an actor, any created probe will write its files under the directory
	% for temporary data (by default under '/tmp'), the current directory of all
	% computing nodes:
	%
	class_Probe:declare_result_probe( NameOptions, CurveNames, Zones, Title,
		MaybeXLabel, YLabel, ExtraSettingsTable, TickDuration ).



% @doc Allows to enable this actor to make use of the data-exchange service.
%
% Returns an updated state.
%
% (helper function)
%
-spec enable_data_exchange( wooper:state() ) -> wooper:state().
enable_data_exchange( State ) ->

	% Multiple enablings not prohibited:
	%undefined = ?getAttr(exchange_settings),

	setAttribute( State, exchange_settings,
				  class_DataExchanger:get_actor_exchange_settings() ).



% @doc Allows this actor to define a new set of data entries.
%
% A data not specifying a qualifier will be defined with the default one.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the definition will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the definition will occur in-between the end of the current tick and
% the beginning of the next one.
%
-spec define_data( class_DataExchanger:entries(), wooper:state() ) -> void().
define_data( EntryList, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { defineData, [ EntryList ], self() },
	receive

		{ wooper_result, data_defined } ->
			ok

	end.



% @doc Allows this actor to define a new data entry.
%
% The data will be defined with the default qualifier.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the definition will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the definition will occur in-between the end of the current tick and
% the beginning of the next one.
%
-spec define_data( key(), value(), wooper:state() ) -> void().
define_data( Key, Value, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { defineData, [ Key, Value ], self() },
	receive

		{ wooper_result, data_defined } ->
			ok

	end.



% @doc Allows this actor to define a new data entry.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the definition will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the definition will occur in-between the end of the current tick and
% the beginning of the next one.
%
-spec define_data( key(), value(), qualifier(), wooper:state() ) -> void().
define_data( Key, Value, Qualifier, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { defineData, [ Key, Value, Qualifier ], self() },
	receive

		{ wooper_result, data_defined } ->
			ok

	end.



% @doc Allows this actor to modify a new set of data entries.
%
% A data not specifying a qualifier will be defined with the default one.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the modification will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the modification will occur in-between the end of the current tick
% and the beginning of the next one.
%
-spec modify_data( class_DataExchanger:entries(), wooper:state() ) -> void().
modify_data( EntryList, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { modifyData, [ EntryList ], self() },
	receive

		{ wooper_result, data_modified } ->
			ok

	end.



% @doc Allows this actor to define a new data entry.
%
% The data will be defined with the default qualifier.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the modification will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the modification will occur in-between the end of the current tick
% and the beginning of the next one.
%
-spec modify_data( key(), value(), wooper:state() ) -> void().
modify_data( Key, Value, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { modifyData, [ Key, Value ], self() },
	receive

		{ wooper_result, data_modified } ->
			ok

	end.



% @doc Allows this actor to define a new data entry.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the modification will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the modification will occur in-between the end of the current tick
% and the beginning of the next one.
%
-spec modify_data( key(), value(), qualifier(), wooper:state() ) -> void().
modify_data( Key, Value, Qualifier, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { modifyData, [ Key, Value, Qualifier ], self() },
	receive

		{ wooper_result, data_modified } ->
			ok

	end.




% @doc Returns the value associated to the specified key (an atom).
%
% Actors can read directly any number of data without any particular precautions
% on the same tick, and these operations will be as cheap as reasonably
% possible.
%
% (helper function)
%
-spec read_data( key(), wooper:state() ) -> value().
read_data( Key, State ) ->

	% Reads are purely local (and enable_data_exchange/1 must have been called
	% beforehand):
	%
	{ _RootExchangerPid, LocalExchangerPid } = ?getAttr(exchange_settings),

	% The data-exchanger service has been designed so that this reading can be
	% done locally and directly, with no message reordering nor latency:
	LocalExchangerPid ! { readData, Key, self() },
	receive

		{ wooper_result, V } ->
			V

	end.



% @doc Returns the value and qualifier (as a {Value,Qualifier} pair) associated
% to the specified key (an atom).
%
% Actors can read directly any number of data without any particular precautions
% on the same tick, and these operations will be as cheap as reasonably
% possible.
%
% (helper function)
-spec read_qualified_data( key(), wooper:state() ) -> qualified_value().
read_qualified_data( Key, State ) ->

	% Reads are purely local (and enable_data_exchange/1 must have been called
	% beforehand):
	%
	{ _RootExchangerPid, LocalExchangerPid } = ?getAttr(exchange_settings),

	% The data-exchanger service has been designed so that this reading can be
	% done locally and directly, with no message reordering nor latency:
	%
	LocalExchangerPid ! { readQualifiedData, Key, self() },
	receive

		{ wooper_result, V } ->
			V

	end.



% @doc Returns a reordered version of the specified list of messages for the
% current diasca, to obtain an order satisfying the expected properties for the
% simulation.
%
% Depending on the simulator settings (second parameter of this function), the
% ordering (if any is requested) will be performed either so that reproductivity
% is ensured (i.e. events are sorted according to a constant arbitrary order),
% and/or so that "ergodicity" is ensured, i.e. so that events are shuffled in an
% uniform way (random permutations).
%
% See the 'Reordering Of Actor Messages' section of documentation for more
% information.
%
apply_reordering( MessagesForCurrentDiasca, unordered ) ->
	% Nothing to do here!
	MessagesForCurrentDiasca;

% This is the heart of Sim-Diasca:
apply_reordering( MessagesForCurrentDiasca, constant_arbitrary_order ) ->

	% Pending messages for this diasca are an unordered list of {Pid,AAI,Msg}
	% elements.
	%
	% Sorts the incoming messages based first on the message (element 3), then,
	% if necessary on the sender AAI (element 2), if ever two messages had an
	% identical hash (ex: same message sent by different actors), so that the
	% reproducibility is ensured (as there is a bijection between AAI and PID,
	% PID do not need to be taken into account; and if an actor sends the same
	% message to the same actor more than once at the same diasca, their
	% relative order will not matter - they will be exactly the same).
	%
	% Therefore the PID - which is only a technical identifier - is ignored
	% here.
	%
	% Ex: if having a message list L = [ {Pa,5,5}, {Pb,4,5}, {Pc,6,5}, {Pd,1,7},
	% {Pe,10,5}, {Pf,2,5}, {Pg,3,5}, {Ph,7,5}, {Ph,7,1}, {Pa,5,8}, {Pa,5,1} ]
	% then 'lists:keysort( 3, lists:keysort(2,L) )' returns:
	% [ {Pa,5,1}, {Ph,7,1}, {Pf,2,5}, {Pg,3,5}, {Pb,4,5}, {Pa,5,5}, {Pc,6,5},
	% {Ph,7,5}, {Pe,10,5}, {Pd,1,7}, {Pa,5,8}] (P stands for 'PID'), i.e. the
	% list is sorted first according to its third member (the message, here an
	% integer), then to its second (the AAI).
	%
	% Note also that the first sort (chronologically) yields:
	% lists:keysort(2,L) = [ {Pd,1,7}, {Pf,2,5}, {Pg,3,5}, {Pb,4,5}, {Pa,5,5},
	% {Pa,5,8}, {Pa,5,1}, {Pc,6,5}, {Ph,7,5}, {Ph,7,1}, {Pe,10,5} ]. i.e.
	% it is sorted in ascending AAI order.
	%
	% Note also that using keysort implies relying not on the hash value of the
	% term, but on the natural order of Erlang terms.
	%
	lists:keysort( 3, lists:keysort( 2, MessagesForCurrentDiasca ) );


apply_reordering( MessagesForCurrentDiasca, constant_permuted_order ) ->

	% Returns a reordered version of the specified list of messages for the
	% current tick, to obtain a constant order compatible to either simulation
	% modes (reproducible or ergodic), as they are determined here only by the
	% random seed they rely on.
	%
	% Here we just need to perform a random (i.e. using the actor's correctly
	% seeded random generator) shuffle of messages (uniform permutation) once
	% they have been first put into a stable order (that's all!):
	%
	list_utils:random_permute( apply_reordering( MessagesForCurrentDiasca,
												 constant_arbitrary_order ) ).



% @doc Returns a textual representation of the specified state, listing only the
% attributes specifically introduced to specialise the actor generic class
% (omitting all technical ones introduced by the engine).
%
-spec state_to_string( wooper:state() ) -> ustring().
state_to_string( State ) ->

	AttrPairs = get_actor_specialised_attributes( State ),

	AttrStrings = lists:sort( [ text_utils:format( "~ts: ~ts", [ AttrName,
		   text_utils:term_to_bounded_string( AttrValue, _MaxLen=1000 ) ] )
								|| { AttrName, AttrValue } <- AttrPairs ] ),

	EnumString = text_utils:strings_to_string( AttrStrings ),

	text_utils:format( "~B name/value actor-level attribute pairs (sorted "
		"alphabetically): ~ts", [ length( AttrPairs ), EnumString ] ).



% @doc Returns a list of the state attribute entries of this instance, from
% which all the ones inherited through class_Actor and above (mother classes)
% have been removed.
%
% As a consequence, only the ones specific to the current type of actor remain.
%
% (internal helper)
%
-spec get_actor_specialised_attributes( wooper:state() ) ->
											[ attribute_entry() ].
get_actor_specialised_attributes( State ) ->

	AllAttrs = wooper:get_all_attributes( State ),

	BaseAttrNames = get_all_base_attribute_names(),

	list_table:remove_entries( BaseAttrNames, AllAttrs ).



% @doc Returns the names of all the base state attributes (be they defined by
% this class or inherited).
%
-spec get_all_base_attribute_names() ->
								static_return( [ wooper:attribute_name() ] ).
get_all_base_attribute_names() ->

	AttrNames =
		wooper_introspection:get_class_specific_attribute_names( ?MODULE )
		++ list_utils:flatten_once(
				[ wooper_introspection:get_class_specific_attribute_names( C )
					|| C <- ?superclasses ] ),

	wooper:return_static( AttrNames ).



% @doc Returns the highest acceptable idle duration, in milliseconds, for the
% completion of the subscription process.
%
-spec get_maximum_subscription_duration() -> milliseconds().


-ifdef(exec_target_is_production).


% In production mode here:

get_maximum_subscription_duration() ->
	% 30 minutes:
	30 * 60 * 1000.


-else. % exec_target_is_production


% In development mode here:

get_maximum_subscription_duration() ->
	% 15 seconds:
	15 * 1000.


-endif. % exec_target_is_production
