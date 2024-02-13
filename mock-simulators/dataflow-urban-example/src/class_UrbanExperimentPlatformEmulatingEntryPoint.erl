% Copyright (C) 2016-2024 EDF R&D

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

% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]


% @doc Example of a <b>platform-emulating experiment entry point</b>.
-module(class_UrbanExperimentPlatformEmulatingEntryPoint).


-define( class_description,
		 "This example of experiment entry point starts each step of this "
		 "urban case experiment."
		 "It introduces changesets as if it had fetched them from an overall, "
		 "unspecified platform (ex: through REST calls directly done from "
		 "here): this entry point thus emulates the use of such a third-party "
		 "platform." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ExperimentEntryPoint ] ).


% Attributes that are specific to the urban experiment entry point are:
-define( class_attributes, [

	{ current_step, step_count(),
	  "the current step at which the experiment is" },

	{ max_step, step_count(),
	  "the maximum step that the experiment may reach" },

	{ base_distance_covered, unit_utils:kilometers(),
	  "the mean total distance covered by all the persons of a given household "
	  "at simulation start (meant to vary over time)" },

	{ entry_probe_ref, class_Probe:probe_ref(), "a basic probe allowing to "
	  "monitor the data injected by this entry point into the dataflow" } ] ).



% Helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Urban-Example.EntryPoint" ).


% For types and shorthands:
-include("sim_diasca_for_actors.hrl").


% For transport_unit_pid/0 and all:
-include("urban_example_defines.hrl").


% We define macros to share these values across clauses:

-define( first_district_external_id, "District-9" ).

-define( first_building_external_id,  "First Building" ).
-define( second_building_external_id, "Second Building" ).

-define( first_household_external_id,  "Household #1" ).
-define( second_household_external_id, "Household #2" ).
-define( third_household_external_id,  "Household #3" ).



% Design notes:
%
% Regarding dataflows:
%
% We consider here that this entry point manages exactly one dataflow (i.e. a
% single PID shall be stored in its 'dataflows' attribute).
%
%
% Regarding event management:
%
% There is a gotcha when creating two dataflow objects (ex: a building and a
% district) thanks to two successive creation events, and wanting then to
% connect them (declaring to both that the building is inside the district)
% thanks to a third (association) event: initially they were each created with
% no link to the other (no PID among their construction parameters), and the
% processing of the association event included a look-up of their respective
% names into the identification server.
%
% However the three events were processed in a row (in the same diasca), leading
% to the failure of the name look-up, as the building and the district were to
% be created in future diascas (and thus could not have declared yet their PID
% to the identification server).
%
% Solution:
%
% 1. create all dataflow objects with the external identifiers of others, and
% have them declare their own PID/external identifier ASAP, at construction time
% and through WOOPER messages

% 2. resolve the external identifiers they rely upon later (at their own first
% diasca)
%
% More generally, respecting the causality (ex: a building must have been
% created *before* attempting to associate it) is to be done thanks to the
% induced events: the association must be induced by the creation.
%
% Note also that if, from a given method (thus in the same diasca), multiple
% changesets are injected, due to message automatic reordering there is no
% guarantee that they will be processed (by the world manager) in the order of
% their sending (these changesets should better anyway be first aggregated in a
% single, overall one). The main solution to force a given causality-induced
% order is to rely an events declared as induced (significantly better than for
% example performing changeset injection over a series of diascas).


% Implementation notes:
%
% We emulate a mode of operation in which a remote platform would be requested
% for the changesets.



% Shorthands:

-type ustring() :: text_utils:ustring().

-type step_count() :: class_ExperimentManager:step_count().



% @doc Constructs the urban-example experiment entry point, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Dataflows is a list of the dataflows that this entry point should drive
%
% - ExperimentStepStart is the step at which the experiment shall start
%
% - ExperimentStepStop is the step at which the experiment shall stop
%
% - ExperimentManagerPid is the PID of the experiment manager
%
% - WorldManagerPid is the PID of the world manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
	[ dataflow_pid() ], step_count(), step_count(),
	experiment_manager_pid(), world_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, Dataflows, ExperimentStepStart,
		   ExperimentStepStop, ExperimentManagerPid, WorldManagerPid ) ->

	% Checking that we have the single dataflow expected here:
	[ _Single ] = Dataflows,

	% First the direct mother class:
	EntryState = class_ExperimentEntryPoint:construct( State, ActorSettings,
						Dataflows, ExperimentManagerPid, WorldManagerPid ),

	EntryProbeRef = create_entry_probe(),

	% Then the class-specific actions:
	setAttributes( EntryState, [ { entry_probe_ref, EntryProbeRef },
								 { current_step, ExperimentStepStart },
								 { max_step, ExperimentStepStop } ] ).



% Section for actor oneways.


% @doc Starts the evaluation of the urban experiment for the current tick.
%
% Typically called by the experiment exit point, for synchronisation reasons.
%
-spec startExperimentTick( wooper:state(), sending_actor_pid() ) ->
									actor_oneway_return().
startExperimentTick( State, _SendingActorPid ) ->

	CurrentStep = ?getAttr(current_step),

	MaxStep = ?getAttr(max_step),

	?debug_fmt( "Starting experiment step ~B/~B", [ CurrentStep, MaxStep ] ),

	% For this example, we emulate the use of an external source of information
	% that is to apply the overall reference scenario (please refer to the
	% simulation case for the detailed course of action)

	% Please refer to the related simulation case
	% (dataflow_urban_example_platform_emulating_case.erl) for the detailed
	% simulation agenda implemented here.

	% Yearly period here:
	CurrentYear = CurrentStep,

	NewYearInitialState = update_dataflow_for_year( CurrentYear, State ),

	FinalState = setAttribute( NewYearInitialState, current_step,
							   CurrentStep+1 ),

	actor:return_state( FinalState ).




% Helper functions.


% @doc Creates a probe monitory the inputs from this entry point.
-spec create_entry_probe() -> class_Probe:probe_ref().
create_entry_probe() ->

	ProbeName = "Monitoring the setting of input ports over the dataflow, "
		"thanks to a probe attached to the urban experiment entry point",

	CurveNames = [
		"'average\_journey' input port of the first transportation unit",
		"'average\_journey' input port of the second transportation unit",
		"'average\_journey' input port of the third transportation unit",
		"'transformer\_efficiency' input port of the aggregating energy unit"
				 ],

	class_Probe:declare_result_probe( ProbeName, CurveNames,
		_Zones=[], _Title=ProbeName, _XLabel="Simulated Year",
		_YLabel="Three inputs set as distance (in km), "
				"one set as efficiency (in %)" ).



% Year-specific handlers.
%
% We are just emulating the presence of a platform here. For example we consider
% that we actually fetch changesets from a remote server exposing a REST
% interface. In practise we rely for this test case on hardcoded changesets
% (whence they come has no impact on the tested dataflow logic).




% @doc Manages the dataflow changes for the specified year, by applying an
% hardcoded changeset as if it had been fetched from a third-party platform.
%
% Note that (notably to inspect the traces), a tick offset T corresponds to the
% year 2020+T (so, for example, the simulation timestamps for year 2026 are in
% the form {6,_}).
%
% (helper)
%
-spec update_dataflow_for_year( unit_utils:years(), wooper:state() ) ->
										wooper:state().
update_dataflow_for_year( Year=2020, State ) ->
	?info_fmt( "~B, initial year, nothing special.", [ Year ] ),
	State;


update_dataflow_for_year( Year=2021, State ) ->

	?info_fmt( "~B, same as previous, boring year, nothing special.",
				[ Year ] ),

	State;


update_dataflow_for_year( Year=2022, State ) ->

	DistrictExternalId = ?first_district_external_id,

	FirstBuildingExternalId = ?first_building_external_id,
	SecondBuildingExternalId = ?second_building_external_id,

	FirstHouseholdExternalId = ?first_household_external_id,
	SecondHouseholdExternalId = ?second_household_external_id,

	?info_fmt( "This is year ~B, beginning of the real action, i.e. "
		"the actual set-up of the simulation world, "
		"in terms of dataflow objects:~n"
		" - creation of the '~ts' dataflow object~n"
		" - creation of the '~ts' and of '~ts' dataflow objects, "
		"both associated (located_in_district) to '~ts'~n"
		" - creation of the '~ts' and '~ts' dataflow objects, "
		"both associated (living_in_building) to '~ts'~n",
		[ Year, DistrictExternalId, FirstBuildingExternalId,
		  SecondBuildingExternalId, DistrictExternalId,
		  FirstHouseholdExternalId, SecondHouseholdExternalId,
		  FirstBuildingExternalId ] ),

	% A single one expected here anyway:
	TargetDataflowPid = hd( ?getAttr(dataflows) ),


	% First we create the district:

	DistrictCreationEvent = #creation_event{
		object_type=class_District,
		external_id=DistrictExternalId,
		construction_parameters=[ "Columbia District", 1210.0, urban ],
		dataflow_pid=TargetDataflowPid },


	% Then we create two buildings, located in this district:

	% These association events have to be defined first, as they will be
	% introduced as induced events defined afterwards: to associate a building
	% to a district, they shall both exist beforehand, thus these associations
	% will be consequences of the next building creations:
	%
	FirstBuildingAssociationEvent = #binary_association_event{
		association_type=located_in_district,
		source_object_type=class_Building,
		target_object_type=class_District,
		source_external_id=FirstBuildingExternalId,
		target_external_id=DistrictExternalId,
		dataflow_pid=TargetDataflowPid },


	% With runtime creations, we do not have yet the PID of the parent
	% (actually, the district just above), hence set to undefined (nevertheless
	% kept so that urban objects can be created interlinked, programmatically):
	%
	UndefinedDistrict = undefined,

	% We then register this building in its district, just previously created.
	%
	% Once this first building will be created, it will be automatically
	% associated in turn to its district:
	%
	FirstBuildingCreationEvent = #creation_event{
		object_type=class_Building,
		external_id=FirstBuildingExternalId,

		% The construction parameters to create a building are its name, its
		% postal address and its parent district:
		%
		construction_parameters=[ "4 Main Street, Forest Hills, NY",
								  UndefinedDistrict ],

		dataflow_pid=TargetDataflowPid,
		induced_events=[ FirstBuildingAssociationEvent ] },


	% Changesets can be injected incrementally:
	FirstChangeset = [ DistrictCreationEvent, FirstBuildingCreationEvent ],

	?debug_fmt( "Sending a first creation/binary association changeset to "
		"the world manager for year ~B: ~ts",
		[ Year, dataflow_support:changeset_to_string( FirstChangeset ) ] ),

	WorldManagerPid = ?getAttr(world_manager_pid),

	FirstSentState = class_Actor:send_actor_message( WorldManagerPid,
						{ injectChangeset, [ FirstChangeset ] }, State ),


	% The second building will be associated as well to District-9:

	SecondBuildingAssociationEvent = #binary_association_event{
		association_type=located_in_district,
		source_object_type=class_Building,
		target_object_type=class_District,
		source_external_id=SecondBuildingExternalId,
		target_external_id=DistrictExternalId,
		dataflow_pid=TargetDataflowPid },

	SecondBuildingCreationEvent = #creation_event{
		object_type=class_Building,
		external_id=SecondBuildingExternalId,
		construction_parameters=[ "12-14 Walnut Avenue, Denver, Colorado",
								  UndefinedDistrict ],
		dataflow_pid=TargetDataflowPid,
		induced_events=[ SecondBuildingAssociationEvent ] },


	% Now, we create two households, which are both to live in the first
	% building:

	% As always, we start by the induced events:
	FirstHouseholdAssociationEvent = #binary_association_event{
		association_type=living_in_building,
		source_object_type=class_Household,
		target_object_type=class_Building,
		source_external_id=FirstHouseholdExternalId,
		target_external_id=FirstBuildingExternalId,
		dataflow_pid=TargetDataflowPid },

	% Then the upper, parent one (household not having their building yet):
	UndefinedBuilding = undefined,

	FirstHouseholdCreationEvent = #creation_event{
		object_type=class_Household,
		external_id=FirstHouseholdExternalId,

		% The construction parameters to create an household are the household
		% name, last name, number of adults, number of children, disposable
		% income (in euros per year), mean distance covered (in km) and the
		% parent building:
		%
		construction_parameters=[ "The Evans", 2, 2, 280000, 2.18,
								  UndefinedBuilding ],

		dataflow_pid=TargetDataflowPid,
		induced_events=[ FirstHouseholdAssociationEvent ] },


	% The second household now, in the same building:

	% As always, we start by the induced events:
	SecondHouseholdAssociationEvent = #binary_association_event{
		association_type=living_in_building,
		source_object_type=class_Household,
		target_object_type=class_Building,
		source_external_id=SecondHouseholdExternalId,
		target_external_id=FirstBuildingExternalId,
		dataflow_pid=TargetDataflowPid },

	% Then the upper, parent one (household not having their building yet):
	UndefinedBuilding = undefined,

	SecondHouseholdCreationEvent = #creation_event{
		object_type=class_Household,
		external_id=SecondHouseholdExternalId,
		construction_parameters=[ "The Ramones", 4, 0, 210000, 1.71,
								  UndefinedBuilding ],
		dataflow_pid=TargetDataflowPid,
		induced_events=[ SecondHouseholdAssociationEvent ] },


	SecondChangeset = [ SecondBuildingCreationEvent,
						FirstHouseholdCreationEvent,
						SecondHouseholdCreationEvent ],

	?debug_fmt( "Sending a second similar changeset to the world manager "
		"for year ~B: ~ts",
		[ Year, dataflow_support:changeset_to_string( SecondChangeset ) ] ),

	% The computation-side changes in the simulation are to be done by the unit
	% managers, namely here class_UrbanUnitManager:
	%
	%  - set-up of the corresponding processing (dataflow units)
	%    * creation of the "First Household Transportation Demand" unit,
	%        associated to "Household-1"
	%    * creation of the "Second Household Transportation Demand" unit,
	%        associated to "Household-2"
	%    * creation of the "First Per-Building Energy Demand" unit
	%
	%  - create channels (links from an output port to an input one)
	%    * between "Household-1" and "First Household Transportation Demand"
	%    * etc.
	%
	%  - destruction of units (typically if associated to dataflow objects that
	%  are themselves removed)

	% Returns an updated state:
	SecondSentState = class_Actor:send_actor_message( WorldManagerPid,
		{ injectChangeset, [ SecondChangeset ] }, FirstSentState ),

	class_Actor:send_actor_message( WorldManagerPid,
									notifyAllChangesetsInjected,
									SecondSentState );



update_dataflow_for_year( Year=2023, State ) ->

	?info_fmt( "~B, update of various dataflow attributes of various "
				"dataflow objects.", [ Year ] ),

	% A single one expected here anyway:
	TargetDataflowPid = hd( ?getAttr(dataflows) ),

	SecondHouseholdUpdateEvent = #update_event{
		object_type=class_Household,
		external_id= <<"Household #2">>,
		updates=[ { "mean_distance_covered", 1.17 } ],
		dataflow_pid=TargetDataflowPid },

	Changeset = [ SecondHouseholdUpdateEvent ],

	?debug_fmt( "Sending a minimal update changeset to the world manager "
		"for year ~B: ~ts",
		[ Year,	dataflow_support:changeset_to_string( Changeset ) ] ),

	WorldManagerPid = ?getAttr(world_manager_pid),

	SentState = class_Actor:send_actor_message( WorldManagerPid,
									{ injectChangeset, [ Changeset ] }, State ),

	class_Actor:send_actor_message( WorldManagerPid,
									notifyAllChangesetsInjected, SentState );


update_dataflow_for_year( Year=2024, State ) ->

	?info_fmt( "~B, another update of various dataflow attributes "
				"of various dataflow objects.", [ Year ] ),

	% A single one expected here anyway:
	TargetDataflowPid = hd( ?getAttr(dataflows) ),

	SecondBuildingUpdateEvent = #update_event{
		object_type=class_Building,
		external_id= <<"Second Building">>,
		updates=[ { "postal_address", "12 Walnut Avenue, Denver, Colorado" },
				  { "number_of_floors", 2 } ],
		dataflow_pid=TargetDataflowPid },

	% Here we intentionally update the administrative name with the same value:
	%
	DistrictUpdateEvent = #update_event{
		object_type=class_District,
		external_id= <<"District-9">>,
		updates=[ { "administrative_name", "Columbia District" },
				  { "ground_surface", 1300.0 } ],
		dataflow_pid=TargetDataflowPid },

	Changeset = [ SecondBuildingUpdateEvent, DistrictUpdateEvent ],

	?debug_fmt( "Sending an update changeset to the world manager "
		"for year ~B: ~ts",
		[ Year, dataflow_support:changeset_to_string( Changeset ) ] ),

	WorldManagerPid = ?getAttr(world_manager_pid),

	SentState = class_Actor:send_actor_message( WorldManagerPid,
									{ injectChangeset, [ Changeset ] }, State ),

	class_Actor:send_actor_message( WorldManagerPid,
									notifyAllChangesetsInjected, SentState );


update_dataflow_for_year( Year=2025, State ) ->

	?info_fmt( "~B, nothing special performed.", [ Year ] ),

	State;


update_dataflow_for_year( Year=2026, State ) ->

	?info_fmt( "~B, new creations.", [ Year ] ),

	TargetDataflowPid = hd( ?getAttr(dataflows) ),

	ThirdHouseholdExternalId = ?third_household_external_id,

	SecondBuildingExternalId = ?second_building_external_id,

	?debug_fmt( "This is year ~B, follow-up of the real action, i.e. "
		"performing a new update of the structure of the simulation "
		"world, in terms of dataflow objects:~n"
		" - creation of the '~ts' dataflow object, associated "
		  "(living_in_building) to '~ts'~n",
		[ Year, ThirdHouseholdExternalId, SecondBuildingExternalId ] ),

	% As always, we start by the induced events:
	ThirdHouseholdAssociationEvent = #binary_association_event{
		association_type=living_in_building,
		source_object_type=class_Household,
		target_object_type=class_Building,
		source_external_id=ThirdHouseholdExternalId,
		target_external_id=SecondBuildingExternalId,
		dataflow_pid=TargetDataflowPid },

	% Then the upper, parent one (household not having their building yet):
	UndefinedBuilding = undefined,

	ThirdHouseholdCreationEvent = #creation_event{
		object_type=class_Household,
		external_id=ThirdHouseholdExternalId,
		construction_parameters=[ "The Stallones", 1, 0, 951000, 1.15,
								  UndefinedBuilding ],
		dataflow_pid=TargetDataflowPid,
		induced_events=[ ThirdHouseholdAssociationEvent ] },

	% The computation-side changes in the simulation are to be done by the unit
	% managers, namely here class_UrbanUnitManager:
	%
	%  - set-up of the corresponding processing (dataflow units)
	%    * creation of the "First Household Transportation Demand" unit,
	%        associated to "Household-1"
	%    * creation of the "Second Household Transportation Demand" unit,
	%        associated to "Household-2"
	%    * creation of the "First Per-Building Energy Demand" unit
	%
	%  - create channels (links from an output port to an input one)
	%    * between "Household-1" and "First Household Transportation Demand"
	%    * etc.

	%
	%    * creation of the "Third Household Transportation Demand" unit, not
	%    associated to any dataflow block

	Changeset = [ ThirdHouseholdCreationEvent ],

	WorldManagerPid = ?getAttr(world_manager_pid),

	SentState = class_Actor:send_actor_message( WorldManagerPid,
						{ injectChangeset, [ Changeset ] }, State ),

	class_Actor:send_actor_message( WorldManagerPid,
									notifyAllChangesetsInjected, SentState );


update_dataflow_for_year( Year=2027, State ) ->

	?info_fmt( "~B, destruction of an household.", [ Year ] ),

	% We introduce first a disassociation event (so that the household is not
	% linked with any other block anymore), then (i.e. as an induced event) the
	% household is to be destructed for good.

	SecondHouseholdExternalId = <<"Household #2">>,

	?debug_fmt( "This is year ~B, we destruct household '~ts', which "
		"should lead to the destruction of its associated "
		"transportation demand unit.", [ Year, SecondHouseholdExternalId ] ),

	TargetDataflowPid = hd( ?getAttr(dataflows) ),

	% We define first the destruction event, that will specified as the induced
	% event of the first disassociation:

	SecondHouseholdDestructionEvent = #destruction_event{
		object_type=class_Household,
		external_id=SecondHouseholdExternalId,
		dataflow_pid=TargetDataflowPid },


	FirstBuildingExternalId = ?first_building_external_id,

	% No binary disassociation event specifically defined, we use the
	% information field to specify any element needed:
	%
	SecondHouseholdDisassociationEvent = #disassociation_event{
		object_type=class_Household,
		external_id=SecondHouseholdExternalId,
		disassociation_information={ living_in_building,
									 FirstBuildingExternalId },
		dataflow_pid=TargetDataflowPid,
		induced_events=[ SecondHouseholdDestructionEvent ] },


	Changeset = [ SecondHouseholdDisassociationEvent ],

	WorldManagerPid = ?getAttr(world_manager_pid),

	SentState = class_Actor:send_actor_message( WorldManagerPid,
						{ injectChangeset, [ Changeset ] }, State ),

	class_Actor:send_actor_message( WorldManagerPid,
									notifyAllChangesetsInjected, SentState );


update_dataflow_for_year( Year, State ) when Year > 2027 ->

	?info( "From 2028 onward, update of various dataflow attributes "
		   "of various dataflow objects with some constant values, "
		   "some other not." ),

	% Add update events if wanted.

	% (termination decided by the exit point)

	State.



% @doc Returns a textual description of this entry point.
%
% (helper)
%
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	EntryString = class_ExperimentEntryPoint:to_string( State ),

	ProbeString = case ?getAttr(entry_probe_ref) of

		non_wanted_probe ->
			"not using a probe";

		ProbePid ->
			text_utils:format( "using probe ~p", [ ProbePid ] )

	end,

	text_utils:format( "Urban changeset-based ~ts, ~ts, ~ts, ~ts",
					   [ EntryString, ProbeString ] ).
