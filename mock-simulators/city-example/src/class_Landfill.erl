% Copyright (C) 2012-2021 EDF R&D

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


-module(class_Landfill).


-define( class_description, "Class modelling a landfill." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor, class_WasteUnloadingPoint,
						 class_PointOfInterest ] ).


% Exported helpers:
-export([ to_string/1 ]).


% The class-specific attributes of a landfill are:
-define( class_attributes, [

  { probe_ref, class_Probe:probe_ref(), "the PID (if any) of the probe "
	"declared to track waste stocks in this landfill" } ] ).


% Inherited attributes of interest:
%
% - waste_capacity :: [ waste_capacity() ] is a plain list storing the state of
% the waste storage tanks (inherited twice, wanted once)


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.Waste.Landfill" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% For waste_tank() and al:
-include("city_example_types.hrl").



% Implementation notes:
%
% A landfill is the final stage of waste treatment: all kinds of waste are
% stored then, mostly expected to be bottom ash. The capacity of a landfill is
% generally huge.



% Creates a new landfill.
%
% Construction parameters are:
%
% - ActorSettings is the AAI assigned by the load-balancer to this actor
%
% - Name is the name of this landfill (as a plain string)
%
% - Location: the (static) location of this landfill
%
% - CapacityInformation describes the waste storage capacity of this landfill
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_GIS:static_location(), gis_pid() ) ->
					   wooper:state().
construct( State, ActorSettings, Name, Location, GISPid ) ->

	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(Name) ),

	{ CapacityInformation, TankCurveNames } = build_capacity(),

	UnloadState = class_WasteUnloadingPoint:construct( ActorState, Location,
													   CapacityInformation ),

	PointState = class_PointOfInterest:construct( UnloadState, Name, Location,
												  GISPid ),

	% Depending on the choice of the result manager, it will be either a PID (if
	% the corresponding result is wanted) or a 'non_wanted_probe' atom:
	%
	WasteStockProbeRef = class_Actor:declare_probe(
				_Name=text_utils:format( "~s Waste Stock Probe", [ Name ] ),
				_Curves=TankCurveNames,
				_Zones=[],
				_Title=text_utils:format( "Waste Storage Monitoring "
										  "for Landfill ~s", [ Name ] ),
				_XLabel="Simulation time",
				_YLabel="Tons of wastes in each tank of this landfill",
				PointState ),

	setAttributes( PointState, [ { waste_capacity, CapacityInformation },
								 { probe_ref, WasteStockProbeRef },
								 { color, orange } ] ).



% Methods section.



% First scheduling of a landfill.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) -> actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% A landfill is mostly passive.

	case ?getAttr(probe_ref) of

		non_wanted_probe ->
			ok;

		ProbePid ->
			ProbePid ! { setTickOffset, ?getAttr(current_tick_offset) }

	end,

	?info_fmt( "Landfill just created: ~s", [ to_string( State ) ] ),

	% To record initial state in probe (and possibly trace state):
	PlanState = class_Actor:scheduleNextSpontaneousTick( State ),

	actor:return_state( PlanState ).




% The definition of the spontaneous behaviour of this landfill.
-spec actSpontaneous( wooper:state() ) -> const_oneway_return().
actSpontaneous( State ) ->

	% No spontaneous life by itself (mostly triggered).
	send_data_to_probe( State ),

	?info_fmt( "~s just created", [ to_string( State ) ] ),

	wooper:const_return().



% Tries to unload to this landfill as much as possible of the specified mass of
% specified waste type (possibly any) from the caller (which is expected to be a
% waste transport requesting to empty its waste).
%
% The answer (the actor message sent back) will be:
%
% - either a notifyUnloadedWaste to acknowledge for good the waste transaction
%
% - or a notifyNoUnloadedWaste to report that no waste unloading will occur this
% time (transaction failed)
%
-spec unloadWaste( wooper:state(), waste_type(), unit_utils:tons(),
				   sending_actor_pid() ) -> actor_oneway_return().
unloadWaste( State, WasteType, ProposedMass, WasteUnloaderPid ) ->

	% First call the parent base implementation:
	ParentState = executeOnewayAs( State, class_WasteUnloadingPoint,
			unloadWaste, [ WasteType, ProposedMass, WasteUnloaderPid ] ),

	% Then update the probe:
	send_data_to_probe( ParentState ),

	actor:return_state( ParentState ).



% Static methods section.


% Generates a list of instance definitions for the specified number of initial
% landfills.
%
-spec generate_definitions( basic_utils:count(), location_generator_pid(),
							gis_info() ) ->
		 static_return( [ class_Actor:instance_creation_spec() ] ).
generate_definitions( LandfillCount, LocationGeneratorPid, GISInfo ) ->

	% Triggers the location generation request in parallel:
	LocationGeneratorPid ! { generateNonAdjacentLocations,
			   [ LandfillCount,
				 get_min_distance_between_landfills_and_others(),
				 get_min_distance_between_two_landfills() ], self() },

	CreationSpecs = define_landfills( LandfillCount, GISInfo, _Acc=[] ),

	wooper:return_static( CreationSpecs ).



% Helper functions.


define_landfills( _LandfillCount=0, GISInfo, Acc ) ->

	% All landfills defined, adding locations as returned by the
	% generateNonAdjacentLocations request:
	%
	receive

		{ wooper_result, Locations } when is_list( Locations ) ->
			% Creates now the full construction parameters:
			merge_parameters( Acc, Locations, GISInfo )

	end;

define_landfills( LandfillCount, GISInfo, Acc ) ->

	% Defines the build parameters for a new landfill; we want to end up with a
	% list of { class_Landfill, [ Name, Location ] } elements.

	Name = text_utils:format( "Landfill-~B", [ LandfillCount ] ),

	define_landfills( LandfillCount-1, GISInfo, [ Name | Acc ] ).



% Adds the location to the landfill build parameters (a kind of zip
% operation):
%
merge_parameters( Params, Locations, GISInfo ) ->
	% In-order is better:
	lists:reverse( merge_parameters( Params, Locations, _Acc=[], GISInfo ) ).


merge_parameters( _Params=[], _Locations=[], Acc, _GISInfo ) ->
	Acc;

merge_parameters( _Params=[ Name | Tp ], _Locations=[ Loc | Tl ], Acc,
				  GISInfo ) ->

	NewLandfillDef = { class_Landfill, [ Name, { wgs84_cartesian, Loc },
										 GISInfo ] },

	merge_parameters( Tp, Tl, [ NewLandfillDef | Acc ], GISInfo ).


% In meters:
get_min_distance_between_landfills_and_others() ->
	40.


% In meters:
get_min_distance_between_two_landfills() ->
	300.



% Sends waste data to probe (if any).
%
% (helper)
%
-spec send_data_to_probe( wooper:state() ) -> void().
send_data_to_probe( State ) ->

	% Avoid doing useless operations:
	case ?getAttr(probe_ref) of

		non_wanted_probe ->
			ok;

		ProbePid ->

			% Already correctly ordered by design:
			TankList = ?getAttr(waste_capacity),

			WasteStockSample = list_to_tuple( [
				  Tank#waste_tank.current_mass_stored || Tank <- TankList ]),

			class_Probe:send_data( ProbePid, ?getAttr(current_tick_offset),
								   WasteStockSample )

	end.



% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	CapacityInfo = waste_utils:waste_capacity_to_string(
					 ?getAttr(waste_capacity) ),

	text_utils:format( "Landfill '~s' (AAI: ~B) located at ~s (~s) making use "
		"of ~s, whose random state is ~p",
		[ ?getAttr(name),
		  class_Actor:get_abstract_identifier( State ),
		  class_GeolocalizedElement:interpret_location( State ),
		  class_PointOfInterest:to_string( State ), CapacityInfo,
		  random_utils:get_random_state() ] ).



% Returns a pair made of the waste capacities for a landfill, and a list of
% corresponding curve descriptions.
%
build_capacity() ->
	% We create one (big) waste tank for each waste type:
	create_waste_tank( waste_utils:get_waste_types(), _AccTank=[], _AccDesc=[],
					   _Count=0 ).


create_waste_tank( _WasteType=[], AccTank, AccDesc, _Count ) ->
	{ AccTank, lists:reverse( AccDesc ) };

create_waste_tank( _WasteType=[ Type | T ], AccTank, AccDesc, Count ) ->

	Id = Count + 1,

	% All tanks start initially empty, and are huge:
	NewTank = #waste_tank{ id=Id,
						   allowed_types=[ Type ],
						   current_type=none,
						   current_volume_stored=0.0,
						   max_volume_stored=5000000000.0,

						   % The current mass of waste stored:
						   current_mass_stored=0.0,

						   % The maximum mass of waste stored:
						   max_mass_stored=40000000000.0,

							% Tells whether the tank is being processed (used)
							% or idle:
						   %
						   busy=false },

	NewDesc = text_utils:format( "Quantity of waste stored in waste tank #~B "
		"used for waste type '~s' (in tons)", [ Id, Type ] ),

	create_waste_tank( T, [ NewTank | AccTank ], [ NewDesc | AccDesc ], Id ).
