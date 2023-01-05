% Copyright (C) 2008-2023 EDF R&D

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

% Author: Jingxuan Ma [jingxuan (dot) ma (at) edf (dot) fr]


% This file is part of forest ecosystem test case, which is a Sim-Diasca
% integration test example.


% @doc Class modelling some kind of (very strange) forest.
-module(class_Forest).


-define( class_description,
		 "The objective of this class is to show:"
		 " - the significant features of an actor scheduled totally in "
		 "periodic mode. The periodic mode scheduling means all activities "
		 "are spontaneously scheduled"
		 " - how to use datalogger for virtual probe creation and data storage "
		 " - how to create actor before simulation start and while the "
		 "simulation is running" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


-export([ notify_be_registered/2, beginCompetition/2 ]).


-type forest_pid() :: actor_pid().

-export_type([ forest_pid/0 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "SSI-Test.Forest" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Shorthands:

-type tick_offset() :: class_TimeManager:tick_offset().



% The class-specific attributes of a forest are:
-define( class_attributes, [

	{ periodic, "the forest spontaneous activity period" },

	{ cycle, "the life cycle in the forest; it is defined as 12 ticks, which "
	  "represent 12 months per year" },

	{ famine_alert_seasons, "a list of periods at which the forest sends "
	  "famine alerts to squirrels" },

	{ savage_alert_seasons, "a list of periods at which the forest sends "
	  "savage attack alerts to squirrels" },

	{ squirrel_reproduce_seasons, "a list of periods at which the forest sends "
	  "reproduction message to the female squirrels" },

	{ fire_alert_triggers, "a list of periods at which the forest sends fire "
	  "alert to oaks" },

	{ oak_reproduce_period, "the period of a new oak available" },

	{ oaks, "a list of the PIDs of the oaks" },

	{ squirrels, "a list of the PIDs of squirrels" },

	{ winner, "corresponds to {WinnerPid, WinnerTailLength}" },

	{ target_peers, "a list of the PIDs of all the forest inhabitants" },

	{ virtual_probe_id,
	  "Identifier of the virtual probe created by the datalogger" },

	{ datalogger_pid, "records the datalogger instance PID" },

	{ termination_initiated, "when it is true, means the forest instance is "
	  "ready to be terminated; by default, its value is false" },

	{ termination_waiting_ticks, "the ticks between the actor termination "
	  "notification and its actual termination" },

	{ termination_tick_offset,
	  "duration after which this actor should terminate" } ] ).



% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().



% @doc Constructs a forest, from:
%
% - ActorSettings is the engine settings for this actor
% - DomainName is a plain string
% - Longevity is expressed in ticks
%
% TODO: represent all ticks in seconds, for example.
%
-spec construct( wooper:state(), class_Actor:actor_settings(), ustring(),
				 tick_offset() ) -> wooper:state().
construct( State, ActorSettings, DomainName, Longevity ) ->

	% Firstly, the mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(DomainName) ),

	% The life-cycle of that probe is not managed by this actor:
	{ DataLoggerPid, ForestVirtualProbeId } =
		case class_DataLogger:create_virtual_probe(

			_ProbeName="Forest Ecosystem Probe",

			_ForestCurveNames=[ "Number of squirrels living in the forest",
								"Number of oaks in the forest" ],

			_ForestZones=[],

			_Title=text_utils:format( "Monitoring Forest ecosystem evolution "
									  "for ~p", [ self() ] ),

			_FirstXLabel="Simulation tick",

			_FirstYLabel="Forester dweller number" ) of

				non_wanted_virtual_probe ->
					{ undefined, undefined };

				VirtualProbe ->
					VirtualProbe

	end,

	%DataLoggerPid ! { getProbeTable, MyVirtualProbeId, self() },
	%ProbeTable = test_receive(),

	%LongevityInTicks = class_Actor:convert_seconds_to_ticks( Longevity,
	%  ActorState ),

	% Then the class-specific attributes:
	StartingState = setAttributes( ActorState, [
		{ periodic, 1 },
		{ cycle, 12 },
		{ famine_alert_seasons, [ 3 ] },
		{ savage_alert_seasons, [ 9 ] },
		{ squirrel_reproduce_seasons, [ 5 ] },
		{ fire_alert_seasons, [ 11 ] },
		{ affected_oaks_count, 0 },
		{ oaks, [] },
		{ squirrels, [] },
		{ male_squirrels, [] },
		{ female_squirrels, [] },
		{ winner, { undefined, undefined } },
		{ nb_reply, 0 },
		{ oak_reproduce_period, 20 },
		{ target_peers, [] },
		{ termination_initiated, false },
		{ longevity, Longevity },
		{ termination_waiting_ticks, 10 },
		{ termination_tick_offset, 1000 },
		{ virtual_probe_id, ForestVirtualProbeId },
		{ datalogger_pid, DataLoggerPid } ] ),

	?send_info( StartingState, "Creating a forest." ),

	StartingState.




% Methods implementation section.


% @doc Simply schedules this just created actor at the next tick (diasca 0).
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( ScheduledState ).



% @doc The spontaneous behaviour of a forest instance.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTick = ?getAttr(current_tick_offset),
	WaitTicks = ?getAttr(termination_waiting_ticks),

	NewState = case ?getAttr(termination_initiated) of


		true when WaitTicks > 0 ->

			NewWaitTick = ?getAttr(termination_waiting_ticks) -1,

			TermState = setAttribute( State, termination_waiting_ticks,
									  NewWaitTick ),

			executeOneway( TermState, scheduleNextSpontaneousTick );


		true ->
			?notice_fmt( "The forest is terminating at tick #~B.",
					   [ CurrentTick ] ),

			executeOneway( State, declareTermination );


		false ->

			UpdatedState = case CurrentTick of

				0 ->
					?notice( "The forest is waiting for its dwellers." ),
					State;

				_Others ->

					case test_trigger_termination_conditions( State ) of

						true ->
							notify_termination( State );

						false ->
							perform_spontaneous_action( State )

					end

			end,

			NewTick = CurrentTick + ?getAttr(periodic),
			executeOneway( UpdatedState, addSpontaneousTick, NewTick )

	end,

	OakList = ?getAttr(oaks),
	NbOak = length( OakList ),

	SquList = ?getAttr(squirrels),
	NbSquirrel = length( SquList ),

	% Sends data to the virtual probe:
	case ?getAttr(datalogger_pid) of

		undefined ->
			ok;

		DataloggerPid ->

			case ?getAttr(virtual_probe_id) of

				undefined ->
					ok;

				ProbePid ->
					DataloggerPid ! { setData,
						[ ProbePid, CurrentTick, { NbSquirrel, NbOak } ] }

			end

	end,

	wooper:return_state( NewState ).



% @doc Called for creating initial Forest dwellers.
%
% It must be called before the simulation start
%
-spec create_initial_foresters( count(), count(), forest_pid() ) ->
				static_void_return().
create_initial_foresters( NbOak, NbSquirrel, ForestPid ) ->

	% The initial oak actors are created as initial placed actors:
	initial_placed_inhabitant_creation( ForestPid, NbOak, "class_Oak", [] ),

	% The initial squirrel actors are created as normal initial actors:
	NbFSquirrel = round( NbSquirrel / 2 ),

	initial_inhabitant_creation( ForestPid, NbFSquirrel,
								 "class_FemaleRedSquirrel", [] ),

	initial_inhabitant_creation( ForestPid, NbSquirrel-NbFSquirrel,
								 "class_MaleRedSquirrel", [] ),

	wooper:return_static_void().



% @doc Called to add a specified peer to known peers.
-spec addInPeers( wooper:state(), classname(), sending_actor_pid() ) ->
						actor_oneway_return().
addInPeers( State, Classname, SenderPid ) ->

	?notice_fmt( "~ts with ~w is added in forest peers.",
			   [ Classname, SenderPid ] ),

	UpdatedState = case ?getAttr(termination_initiated) of

		false ->
			?notice_fmt( "~ts with ~w is added in forest peers.",
						 [ Classname, SenderPid ] ),
			add_pid_in_peers( State, Classname, SenderPid );

		true ->
			?notice_fmt( "~ts with ~w cannot be added in forest peers.",
						 [ Classname, SenderPid ] ),
			class_Actor:send_actor_message( SenderPid, forestDestroyed, State )

	end,

	actor:return_state( UpdatedState ).



% @doc Called to delete a specified peer from known target peers.
-spec deleteFromPeers( wooper:state(), sending_actor_pid() ) ->
								actor_oneway_return().
deleteFromPeers( State, PeerPid ) ->

	?notice_fmt( "~w is removed from forest peers.", [ PeerPid ] ),

	OakList       = ?getAttr(oaks),
	SquirrelList  = ?getAttr(squirrels),
	MSquirrelList = ?getAttr(male_squirrels),
	FSquirrelList = ?getAttr(female_squirrels),

	UpdatedOakList       = lists:delete( PeerPid, OakList ),
	UpdatedSquirrelList  = lists:delete( PeerPid,SquirrelList ),
	UpdatedMSquirrelList = lists:delete( PeerPid, MSquirrelList ),
	UpdatedFSquirrelList = lists:delete( PeerPid, FSquirrelList ),

	UpdatedState = updated_peers( State, UpdatedOakList, UpdatedSquirrelList,
								  UpdatedMSquirrelList, UpdatedFSquirrelList ),

	actor:return_state( UpdatedState ).



% @doc Called by a squirrel in order to be allocated to a new oak.
-spec requiredLocation( wooper:state(), sending_actor_pid() ) ->
							  actor_oneway_return().
requiredLocation( State, SquirrelPid ) ->

	OakList = ?getAttr(oaks),
	Length = length( OakList ),

	{ UpdatedState, OakPid } = case ?getAttr(affected_oaks_count) of

		AffectedOaksCount when AffectedOaksCount < Length ->

			Pid = lists:nth( AffectedOaksCount + 1, OakList ),
			NewState = setAttribute( State, affected_oaks_count,
									 AffectedOaksCount + 1 ),
			{ NewState, Pid };


		_Others ->

			NewState = setAttribute( State, affected_oaks_count, 1 ),
			Pid = hd( OakList ),
			{ NewState, Pid }

		end,

	% Informing the squirrel of its new oak PID:
	StateAfterInformSquirrel = class_Actor:send_actor_message( SquirrelPid,
		{ beAllocated, OakPid }, UpdatedState ),

	% Informing oak about its new lodger:
	NewUpdatedState = class_Actor:send_actor_message( OakPid,
		{ beAffected, SquirrelPid }, StateAfterInformSquirrel ),

	actor:return_state( NewUpdatedState ).



% @doc Called by a female squirrel.
%
% When the forest receives this message, it sends a "beInvited" message to all
% male squirrels for launching a competition.
%
-spec beginCompetition( wooper:state(), sending_actor_pid() ) ->
								actor_oneway_return().
beginCompetition( State, LauncherPid ) ->

	?notice_fmt( "Initiated by ~w: a competition invitation is sent to "
		"all male squirrels, ~p.",
		[ LauncherPid, ?getAttr(male_squirrels) ] ),

	MSquirrelList = ?getAttr(male_squirrels),

	NState = case MSquirrelList of

		[] ->
			class_Actor:send_actor_message( LauncherPid,
				{ theWinner, undefined }, State );

		MSquirrelList ->

			SendFun = fun( InhabitantPid, FunState ) ->
					class_Actor:send_actor_message( InhabitantPid,
						{ beInvited, LauncherPid }, FunState )
			end,

			% Returns an updated state:
			lists:foldl( SendFun, State, MSquirrelList )

	end,
	setAttributes( NState, [
		{ winner, { _WinnerPid=undefined, _WinnerTail=undefined } },
		{ nb_reply, 0 } ] ).



% @doc Called by the competition participant (male squirrel).
%
% When the forest receives this message, it will compare the tail lengths and
% update winner attribute with male squirrel PID and max tail length.
%
-spec informedParticipation( wooper:state(), actor_pid(), number(),
							 sending_actor_pid() ) -> actor_oneway_return().
informedParticipation( State, LauncherPid, TailLength, SenderPid ) ->

	?notice_fmt( "The participation of ~w is received, its tail length is ~p.",
				 [ SenderPid, TailLength ] ),

	ReceivedReply = ?getAttr(nb_reply) + 1,

	{ Winner, Length } = case ?getAttr(winner) of

		{ undefined, undefined } when TailLength =:= refused ->
			{ undefined, undefined };

		{ undefined, undefined } ->
			{ SenderPid, TailLength };

		{ _WinnerPid, WinnerTail } when WinnerTail < TailLength ->
			{ SenderPid, TailLength };

		{ WinnerPid, WinnerTail } when WinnerTail >= TailLength ->
			{ WinnerPid, WinnerTail }

	end,

	NbMSquirrel = length( ?getAttr(male_squirrels) ),

	NState = case NbMSquirrel of

		N when N =:= ReceivedReply ->
			class_Actor:send_actor_message( LauncherPid,
											{ theWinner, Winner }, State );

		_Others ->
			State

	end,

	FinalState = setAttributes( NState, [ { nb_reply, ReceivedReply },
										  { winner, { Winner, Length } } ] ),

	actor:return_state( FinalState ).




% Probe-related section.


% @doc Returns the PID of the forest probe.
-spec getProbe( wooper:state() ) -> const_request_return( probe_ref() ).
getProbe( State ) ->
	wooper:const_return_result( ?getAttr(probe_ref) ).




% @doc Returns the PID of the forest virtual probe.
%
% The virtual probe is created by data logger.
%
% Useful for the calling test, so that it can control by itself the probe, as,
% depending on whether it is run in batch mode or not, probe displaying is
% wanted or not.
%
-spec getVirtualProbe( wooper:state() ) ->
			const_request_return( class_DataLogger:virtual_probe_reference() ).
getVirtualProbe( State ) ->
	wooper:const_return_result( ?getAttr(virtual_probe_id) ).



% @doc Called for returning the singleton datalogger PID.
-spec getDataLoggerPid( wooper:state() ) ->
								const_request_return( datalogger_pid() ).
getDataLoggerPid( State ) ->
	wooper:const_return_result( ?getAttr(datalogger_pid) ).



% Helper functions


% Initial forest dweller creation session; these functions can only be called
% before the simulation starts.



% @doc Called to create synchronously a number of initial placed actors:
%
% - Number is the number of inhabitants to be created
% - Classname is the class name
% - CreatedList is the created pid list
%
% A CreatedPidList is returned.
%
% NB: in this test, all trees must be created in the qform of placed actor with
% PlacementHint = forest pid

initial_placed_inhabitant_creation( _ForestPid, 0, _Classname, CreatedList ) ->
	CreatedList;


initial_placed_inhabitant_creation( ForestPid, Number, Classname,
									CreatedList ) ->

	ActualCreatedNb = length( CreatedList ) + 1,

	Name = Classname ++ integer_to_list( ActualCreatedNb ),

	NewPid = class_Actor:create_initial_placed_actor(
		text_utils:string_to_atom( Classname ),
		[ _Name=Name, _DefaultAge=ActualCreatedNb, _ForestPid=ForestPid ],
		_PlacementHint=forest_inhabitant ),

	UpdatedCreatedList = [ NewPid | CreatedList ],

	initial_placed_inhabitant_creation( ForestPid, Number-1, Classname,
										UpdatedCreatedList ).



% @doc Called to create synchronously a number of initial actors.
%
% Parameters are:
%
% - Number is the number of inhabitants to be created
% - Classname is the class name
% - CreatedList is the created PID list
%
% A CreatedPidList is returned.
%
initial_inhabitant_creation( _ForestPid, 0, _Classname, CreatedList ) ->
	CreatedList;


initial_inhabitant_creation( ForestPid, Number, Classname, CreatedList ) ->

	ActualCreatedNb = length( CreatedList ) + 1,

	Name = lists:subtract( Classname, "class_" ) ++ " #"
		++ integer_to_list( ActualCreatedNb ),

	NewPid = class_Actor:create_initial_actor(
		text_utils:string_to_atom( Classname ),
		[ _Name=Name, _DefaultAge=ActualCreatedNb, _Forest=ForestPid ] ),

	UpdatedCreatedList = [ NewPid | CreatedList ],

	initial_inhabitant_creation( ForestPid, Number-1, Classname,
								 UpdatedCreatedList ).



% Runtime forest dweller creation section.


% @doc Called to create an oak actor (placed actor) in simulation time.
new_placed_oak_creation( State ) ->

	OakList = ?getAttr(oaks),

	ActualCreatedNb = length( OakList ) + 1,

	Name = "class_Oak" ++ integer_to_list( ActualCreatedNb ),

	class_Actor:create_placed_actor( class_Oak,
		[ _Name=Name, _defaultAge=0, _Forest=self() ],
		_PlacementHint=self(), State ).




% Spontaneous activities verification section.


% @doc Called to verify if any simulation termination condition is satisfied.
%
% The normal simulation termination conditions are:
% - the termination offset is reached
% - there is no oak in the forest
% - there is no squirrel in the forest
%
-spec test_trigger_termination_conditions( wooper:state() ) -> boolean().
test_trigger_termination_conditions( State ) ->

	TerminationOffset = ?getAttr(termination_tick_offset),

	Nb_Oak = length( ?getAttr(oaks) ),

	Nb_Squirrel = length( ?getAttr(squirrels) ),

	case ?getAttr(current_tick_offset) of

		PastOffset when PastOffset >= TerminationOffset ->
			?notice( "Forest is ruined because its longevity is reached." ),
			true;

		_Others ->

			case Nb_Oak of

				0 ->
					?notice( "Forest is ruined because of no any tree." ),
					true;

				_OtherOak ->

					case Nb_Squirrel of

						0 ->
							?notice( "Forest is dying, as there is no "
									 "living being in it." ),
							true;

						_AtLeastOne ->
							false

					end

			end

	end.



% @doc Forest-specific periodic spontaneous behaviours.
perform_spontaneous_action( State ) ->

	CurrentOffset = ?getAttr(current_tick_offset),

	ActionTrigger= CurrentOffset rem ?getAttr(cycle),

	% Get trigger period lists:
	FamineTriggers = ?getAttr(famine_alert_seasons),
	SavageTriggers = ?getAttr(savage_alert_seasons),
	FireTriggers   = ?getAttr(fire_alert_seasons),

	ReproductionTriggers = ?getAttr(squirrel_reproduce_seasons),

	NewState = case lists:member( ActionTrigger, FamineTriggers ) of

		true ->
			notify_alert( State, famine );

		false ->
			case lists:member( ActionTrigger, SavageTriggers) of

				true ->
					notify_alert( State, savage );

				false ->
					case lists:member( ActionTrigger, FireTriggers ) of

						true ->
							notify_alert( State, fire );

						false ->
							case lists:member( ActionTrigger,
											   ReproductionTriggers ) of

								true ->
									notify_alert( State, reproduction );

								false ->
									State

							end
					end

			end

	end,

	case CurrentOffset rem ?getAttr(oak_reproduce_period) of

		0 ->
			?notice_fmt( "A new oak is availabe at tick #~p.",
						 [ CurrentOffset ] ),
			new_placed_oak_creation( NewState );

		_Others ->
			NewState

	end.




% Forest spontaneous activity execution section.


% @doc Notifies the forest dwellers that they are registered to the forest.
%
% (helper)
%
-spec notify_be_registered( wooper:state(), [ actor_pid() ] ) -> wooper:state().
notify_be_registered( State, InHabitantList ) ->

	SendFun = fun( InhabitantPid, FunState ) ->

		% Returns an updated state:
		class_Actor:send_actor_message( InhabitantPid, beRegistered, FunState )

	end,

	% Returns an updated state:
	lists:foldl( SendFun, State, InHabitantList ).



% @doc Sending alert to relative forest dwellers.
%
% (helper)
%
notify_alert( State, Alert ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	RelativePidList = case Alert of

		fire ->
			[ hd( ?getAttr(oaks) ) ];

		famine ->
			?getAttr(squirrels);

		savage ->
			[ hd( ?getAttr(squirrels) ) ];

		reproduction ->
			?getAttr(female_squirrels)

		% Completly covered:
		%_Others ->
		%   ?getAttr(target_peers)

		end,

	?notice_fmt( "A ~p alert is broadcast to relative forest dwellers ~p "
		"at tick offset #~p.",
		[ Alert, RelativePidList, CurrentTickOffset ] ),

	SendFun = fun( InhabitantPid, FunState ) ->

		% Returns an updated state:
		class_Actor:send_actor_message( InhabitantPid,
										{ beAlert, Alert }, FunState )

	end,

	% Returns an updated state:
	lists:foldl( SendFun, State, RelativePidList ).



% @doc The forest informs its dwellers of its termination.
%
% (helper)
%
notify_termination( State ) ->

	?notice( "A forest destroyed message is sent to the forest dwellers." ),

	NewState = case ?getAttr(target_peers) of

		[] ->
			State;

		TargetPeers ->
			SendFun = fun( InhabitantPid, FunState ) ->

				%Returns an updated state:
				class_Actor:send_actor_message( InhabitantPid, forestDestroyed,
												FunState )

			end,

			% Returns an updated state:
			lists:foldl( SendFun, State, TargetPeers )

		end,

	% Prepares itself for termination:
	prepare_termination( NewState ).




% An updated state is returned.
%
% (helper)
%
prepare_termination( State ) ->

	CurrentOffset = ?getAttr(current_tick_offset),

	setAttributes( State, [ { termination_initiated, true },
							{ termination_tick_offset, CurrentOffset } ] ).



add_pid_in_peers( State, Classname, PeerPid ) ->

	OakList       = ?getAttr(oaks),
	SquirrelList  = ?getAttr(squirrels),
	MSquirrelList = ?getAttr(male_squirrels),
	FSquirrelList = ?getAttr(female_squirrels),

	{ NOakList, NSquirrelList, NMSquirrelList, NFSquirrelList } =

		case Classname of

			class_Oak ->
				NewOakList = [ PeerPid | OakList ],
				{ NewOakList, SquirrelList, MSquirrelList, FSquirrelList };

			class_MaleRedSquirrel ->
				NewSquirrelList = [ PeerPid | SquirrelList ],
				NewMSquirrelList = [ PeerPid | MSquirrelList ],
				{ OakList, NewSquirrelList, NewMSquirrelList, FSquirrelList };

			class_FemaleRedSquirrel ->
				NewSquirrelList = [ PeerPid | SquirrelList ],
				NewFSquirrelList = [ PeerPid | FSquirrelList ],
				{ OakList, NewSquirrelList, MSquirrelList, NewFSquirrelList }

		end,

	updated_peers( State, NOakList, NSquirrelList, NMSquirrelList,
				   NFSquirrelList ).



% Called to return a updated state with the updated forest dweller list.
%
% (helper)
%
updated_peers( State, OakList, SquirrelList, MSquirrelList, FSquirrelList ) ->

	UpdatedOakNumber = length( OakList ),
	UpdatedSquirreNumber = length( SquirrelList ),
	UpdatedTargetPeers = OakList ++ SquirrelList,

	setAttributes( State, [
		{ nb_oak, UpdatedOakNumber },
		{ oaks, OakList },
		{ nb_squirrel, UpdatedSquirreNumber },
		{ squirrels, SquirrelList },
		{ male_squirrels, MSquirrelList },
		{ female_squirrels, FSquirrelList },
		{ target_peers, UpdatedTargetPeers } ] ).
