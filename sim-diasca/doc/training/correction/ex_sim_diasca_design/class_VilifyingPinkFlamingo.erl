% Copyright (C) 2008-2021 EDF R&D
%
% This file is part of the Sim-Diasca training material.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)

-module(class_VilifyingPinkFlamingo).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ViviparousBeing, class_Actor ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Flamingo.Vilifying" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% Shorthands:

-type ustring() :: text_utils:ustring().

-type heigth() :: class_PinkFlamingo:heigth().
-type color() :: class_PinkFlamingo:color().
-type flamingo_pid() :: class_PinkFlamingo:flamingo_pid().
-type filtering_location() :: class_PinkFlamingo:filtering_location().



% The class-specific attributes of such a flamingo are:
-define( class_attributes, [
  { heigth, heigth(), "Heigth of this flamingo" },
  { feather_color, color(), "From grey to pink" },
  { rival_flamingo, maybe( flamingo_pid() ), "PID of any rival" },
  { filter_location, filtering_location(), "the current filtering spot" } ] ).


-export_type([ heigth/0, color/0, flamingo_pid/0, filtering_location/0 ]).




% Constructs a new VilifyingPinkFlamingo:
%
% - ActorSettings corresponds to the engine settings for this actor
%
% - Name is a string corresponding to the name of the flamingo
%
% - Heigth is its initial (floating-point) heigth, in centimetres
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), heigth() ) -> wooper:state().
construct( State, ActorSettings, Name, Heigth )
  when is_list( Name ) andalso is_float( Heigth ) ->

	% First the direct mother classes:
	ViviparousBeingState = class_ViviparousBeing:construct( State ),

	ActorState = class_Actor:construct( ViviparousBeingState,
										?trace_categorize(Name) ),

	?send_notice_fmt( ActorState, "Creating a vilifying pink flamingo "
		"named '~ts' (AAI: ~B) whose heigth is ~p centimeters.",
		[ ?trace_categorize(Name), ActorSettings, Heigth ] ),

	% Then the class-specific attributes:
	% (name is no more class-specific, as defined in class_Actor)
	%
	% The rival_flamingo attribute holds the PID of the rival of that flamingo
	% (if any).
	%
	setAttributes( ActorState, [
		{ heigth, Heigth },
		{ feather_color, pink },
		{ rival_flamingo, undefined },
		{ filter_location, camargue } ] ).



% Overridden destructor:
-spec delete( wooper:state() ) -> wooper:state().
delete( State ) ->

	?notice_fmt( "Deletion of vilifying pink flamingo named '~ts'.",
				 [ ?getAttr(name) ] ),

	State.



% The spontaneous behaviour of a vilifying pink flamingo.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	NewState = case ?getAttr(rival_flamingo) of

		undefined ->
			mumble( State );

		RivalPid ->
			% Will vilify the rival if the flamingo logic tells to do so:
			case class_Actor:get_current_tick( State ) rem 3 of

				0 ->
					vilify( RivalPid, State );

				_Other ->
					filterPlankton( State )

			end

	end,

	wooper:return_state( NewState ).




% Actor oneway section.


% Notifies this flamingo that it has just been vilified.
%
% If a flamingo is vilified by another one which was not its rival, then the
% vilifier becomes its new rival.
%
% Called by a rival flamingo.
%
-spec beVilified( wooper:state(), ustring(), sending_actor_pid() ) ->
						actor_oneway_return().
beVilified( State, VilificationMessage, SendingActorPid ) ->

	% Sender has to specify who it is, as it is not a request:
	NewState = case ?getAttr(rival_flamingo) of

		SendingActorPid ->

			?info_fmt( "I am flamingo ~ts, and I am vilified by "
				"my rival ~w, whose message is: '~ts'.",
				[ ?getAttr(name), SendingActorPid, VilificationMessage ] ),

			State;

		_Other->

			?info_fmt( "I am flamingo ~ts, and I am vilified by ~w, "
				"whose message is: '~ts'. It was not my rival but now it is.",
				[ ?getAttr(name), SendingActorPid, VilificationMessage ] ),

			setAttribute( State, rival_flamingo, SendingActorPid )

	end,

	wooper:return_state( NewState ).




% Request section.


% Returns the feather color of the flamingo.
-spec getFeatherColor( wooper:state() ) -> const_request_return( color() ).
getFeatherColor( State ) ->
	wooper:const_return_result( ?getAttr(feather_color) ).




% Oneway section.


% Makes this flamingo discover a new rival to vilify.
%
% The flamingo forgets any previously identified rival.
%
-spec beNotifiedOfRival( wooper:state(), sending_actor_pid() ) ->
							   actor_oneway_return().
beNotifiedOfRival( State, RivalPid ) ->
	actor:return_state(	setAttribute( State, rival_flamingo, RivalPid ) ).




% Static section.


% Let's say an average means something here:
-spec get_mean_children_count() -> static_return( basic_utils:count() ).
get_mean_children_count() ->
	wooper:return_static( 1.7 ).




% Helper function section.


% Action to be done when this flamingo decides to mumble.
%
% Returns an updated state.
%
% (helper function)
%
-spec mumble( wooper:state() ) -> wooper:state().
mumble( State ) ->
	?info( "Mumble, mumble. Life is sweet without a rival." ),
	State.



% Action to be done when this flamingo decides to vilify another flamingo.
%
% Returns an updated state.
%
% (helper function)
%
-spec vilify( flamingo_pid(), wooper:state() ) -> wooper:state().
vilify( RivalPid, State ) ->

	?info_fmt( "Vilifying now ~w.", [ RivalPid ] ),

	Message = text_utils:format( "Me, ~ts, hereby testify that you are the "
		"smallest flamingo that ever existed. Sincerily yours.",
		[ ?getAttr(name) ] ),

	class_Actor:send_actor_message( RivalPid,
							{ beVilified, [ Message ] }, State ).



% Requests the flamingo to filter plankton in specified location.
%
% Note: this is a modified version of the previous oneway.
%
% Returns an updated state.
%
% (helper function)
%
-spec filterPlankton( wooper:state() ) -> wooper:state().
filterPlankton( State ) ->

	Location = ?getAttr(filter_location),

	HeigthGain = case Location of

		camargue ->
			2.5;

		chile ->
			1.0

	end,

	NewHeigth = ?getAttr(heigth) + HeigthGain,

	?info_fmt( "Filtering plankton in ~w, my new heigth is ~f, "
		"my rival ~w will not believe its eyes.",
		[ Location, NewHeigth, ?getAttr(rival_flamingo) ] ),

	setAttribute( State, heigth, NewHeigth ).
