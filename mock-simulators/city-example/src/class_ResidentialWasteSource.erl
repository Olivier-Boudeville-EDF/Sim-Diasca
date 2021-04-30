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


-module(class_ResidentialWasteSource).


-define( class_description,
		 "Class modelling a residential waste source, which is a specific kind "
		 "of waste loading point: it produces (residential) wastes "
		 "spontaneously, in a stochastic manner." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_StochasticActor, class_WasteLoadingPoint,
						 class_PointOfInterest ] ).


% For waste_capacity() and al:
-include("city_example_types.hrl").


% For city_max_relative_error:
-include("city_example_settings.hrl").



% The class-specific attributes of a residential waste source instance are:
-define( class_attributes, [

  { production_type, waste_type(),
	"the (single) type of waste produced by this source" },

  { production_quantity, unit_utils:tons(),
	"the (average) quantity of waste produced by a production cycle" },

  { production_duration, unit_utils:seconds(),
	"the (average) duration of a production cycle" },

  { probe_ref, probe_ref(), "the PID of the production probe (if any)" } ] ).



-type source_pid() :: actor_pid().

-export_type([ source_pid/0 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "City-example.Waste.Source.Residential" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Creates a new residential waste source.
%
% Construction parameters are:
%
% - Location, which is the location of this source
%
% - ProductionType is the type of waste produced
%
% - ProductionQuantity is the quantity of waste produced by cycle
%
% - LocalStorage is the quantity of waste that can be locally stored
%
% - ProductionDuration is the duration of a waste production cycle
%
% - GISPid is the PID of the GIS
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_GIS:location(), waste_type(),
				 unit_utils:tons(), unit_utils:tons(), unit_utils:seconds(),
				 gis_pid() ) -> wooper:state().
construct( State, ActorSettings, Name, Location, ProductionType,
		  ProductionQuantity, LocalStorage, ProductionDuration, GISPid ) ->

	% We cannot declare directly production_duration_law here, as it should
	% depend on a lambda compute from a time-conversion, only available when
	% using an Actor state. Hence we declare this law in a second time:
	%
	ActorState = class_StochasticActor:construct( State, ActorSettings,
												  ?trace_categorize(Name),
	  [ { production_quantity_law,
		  { gaussian, _Mu=ProductionQuantity, _Sigma=2.0 } } ] ),

	ProductionTickDuration = class_Actor:convert_seconds_to_ticks(
				ProductionDuration, ?city_max_relative_error, ActorState ),

	AddedState = class_StochasticActor:add_law( production_duration_law,
	  { positive_integer_exponential, _Lamba=1/ProductionTickDuration },
	  ActorState ),

	% One tank per residential source, initially empty:
	Tank = #waste_tank{ id=1,
						allowed_types=[ ProductionType ],
						current_type=none,
						current_volume_stored=0.0,
						max_volume_stored=0.0,
						current_mass_stored=0.0,
						max_mass_stored=LocalStorage,
						busy=false },

	WasteCapacity = [ Tank ],

	LoadingState = class_WasteLoadingPoint:construct( AddedState,
											Location, WasteCapacity ),

	POIState = class_PointOfInterest:construct( LoadingState, Name, Location,
												GISPid ),

	% Depending on the choice of the result manager, it will be either a PID (if
	% the corresponding result is wanted) or a 'non_wanted_probe' atom:
	%
	WasteStockProbeRef = class_Actor:declare_probe(
				_Name=text_utils:format( "~s Produced Waste Stock Probe",
										 [ Name ] ),
				_Curves=[ text_utils:format( "Quantity of waste of type ~w "
							 "still in tank (in tons)", [ ProductionType ] ) ],
				_Zones=[],
				_Title=text_utils:format( "Waste Production & Storage "
					"Monitoring for Residential Waste Source ~s", [ Name ] ),
				_XLabel="Simulation time",
				_YLabel="Tons of wastes still stored by this "
						"residential waste source",
				POIState ),

	setAttributes( POIState, [ { production_type, ProductionType },
							   { production_quantity, ProductionQuantity },
							   { production_duration, ProductionTickDuration },
							   { probe_ref, WasteStockProbeRef },
							   { color, orange } ] ).



% Methods section.



% First scheduling of a residential waste source.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) -> actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?info_fmt( "~s just created", [ to_string( State ) ] ),

	case ?getAttr(probe_ref) of

		non_wanted_probe ->
			ok;

		ProbePid ->
			ProbePid ! { setTickOffset, ?getAttr(current_tick_offset) }

	end,

	PlanState = class_Actor:scheduleNextSpontaneousTick( State ),

	actor:return_state( PlanState ).




% The definition of the spontaneous behaviour of this residential source.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	?info_fmt( "~s acting spontaneously.", [ to_string( State ) ] ),

	CurrentTickOffset = ?getAttr(current_tick_offset),

	% One tank per source:
	[ Tank ] = ?getAttr(waste_capacity),

	% The domain-specific rule here is that we can forecast the duration of the
	% next production iteration, but not its mass, which is known a-posteriori:
	%
	{ ProducedMass, NewProductionDuration } =
		compute_production_parameters( State ),

	?info_fmt( "Produced mass: ~f tons, duration: ~B ticks.",
			   [ ProducedMass, NewProductionDuration ] ),

	ActualAddedMass = case Tank#waste_tank.max_mass_stored -
								  Tank#waste_tank.current_mass_stored  of

		Margin when Margin < ProducedMass ->

			% We do not want this benchmarking case fail because of an
			% unreachable waste source never unloaded, so we just saturate here:
			%
			Margin;

			%( { overloaded_residential_waste_source, self(), ProducedMass,
			%     Margin } );

		_SufficientMargin ->
			ProducedMass

	end,

	UpdatedTank = waste_utils:add_waste_to_tank( Tank, ActualAddedMass,
												 ?getAttr(production_type) ),

	MassState = setAttribute( State, waste_capacity, [ UpdatedTank ] ),

	NextProductionTick = CurrentTickOffset + NewProductionDuration,

	% Manages automatically the fact that the creation of this probe may have
	% been rejected by the result manager:
	%
	class_Probe:send_data( ?getAttr(probe_ref), CurrentTickOffset,
					{ Tank#waste_tank.current_mass_stored + ActualAddedMass } ),

	PlanState = class_Actor:add_spontaneous_tick( NextProductionTick,
												  MassState ),

	wooper:return_state( PlanState ).



% Tries to load from this waste source as much as possible of the specified mass
% compatible with specified waste type into the calling actor, which is expected
% to be a waste transport, located in this point, looking for additional waste.
%
% The answer (the actor message sent back) will be:
%
% - either a notifyLoadedWaste to acknowledge once for good the waste
% transaction
%
% - or a notifyNoLoadedWaste to report that no waste loading will occur this
% time (transaction failed)
%
-spec loadWaste( wooper:state(), waste_type(), unit_utils:tons(),
				 sending_actor_pid() ) -> actor_oneway_return().
loadWaste( State, WasteType, MaxWantedMass, WasteLoaderPid ) ->

	% First call the parent base implementation:
	ParentState = executeOnewayAs( State, class_WasteLoadingPoint, loadWaste,
						   [ WasteType, MaxWantedMass, WasteLoaderPid ] ),

	% Then update the probe:

	[ Tank ] = getAttribute( ParentState, waste_capacity ),

	Mass = Tank#waste_tank.current_mass_stored,

	class_Probe:send_data( ?getAttr(probe_ref), ?getAttr(current_tick_offset),
						   { Mass } ),

	actor:return_state( ParentState ).



% Returns a textual description of this instance.
-spec toString( wooper:state() ) -> const_request_return( string() ).
toString( State ) ->
	wooper:const_return_result( to_string( State ) ).



% Computes the newly produced mass of waste and the duration of the next
% production iteration.
%
compute_production_parameters( State ) ->

	% We do no want waste to be consumed!
	AdditionalMass = case class_StochasticActor:get_random_value_from(
										production_quantity_law, State ) of

		M when M < 0 ->
			-M;

		M ->
			M

	end,

	% At least one tick away:
	NextDuration = max( 1, class_StochasticActor:get_random_value_from(
							   production_duration_law, State ) ),

	{ AdditionalMass, NextDuration }.





% Static methods section.



% Generates a list of instance definitions for the specified number of
% residential waste sources.
%
-spec generate_definitions( basic_utils:count(), location_generator_pid(),
							pid() | instance_loading:id_ref() ) ->
					static_return( [ class_Actor:instance_creation_spec() ] ).
generate_definitions( ResidentialSourceCount, LocationGeneratorPid, GISInfo ) ->

	% Triggers the location generation request in parallel:
	LocationGeneratorPid ! { generateNonAdjacentLocations,
		[ ResidentialSourceCount,
		  get_min_distance_between_residential_sources_and_others(),
		  get_min_distance_between_two_residential_sources() ], self() },

	CreationSpecs = define_residential_waste_sources( ResidentialSourceCount,
													  GISInfo, _Acc=[] ),

	wooper:return_static( CreationSpecs ).




% Helper functions.


define_residential_waste_sources( _ResidentialSourceCount=0, GISInfo, Acc ) ->

	% All residential sources defined, adding locations as returned by the
	% generateNonAdjacentLocations request:
	%
	receive

		{ wooper_result, Locations } when is_list( Locations )->
			% Creates now the full construction parameters:
			merge_parameters( Acc, Locations, GISInfo )

	end;

define_residential_waste_sources( ResidentialSourceCount, GISInfo, Acc ) ->

	% Defines the build parameters for a new residential source; we want to end
	% up with a list of { class_ResidentialWasteSource, [ Name, Location,
	%  ProductionType, ProductionQuantity, ProductionDuration ] } elements.

	Name = text_utils:format( "ResidentialWasteSource-~B",
							  [ ResidentialSourceCount ] ),

	ProductionType = list_utils:draw_element(
					   waste_utils:get_incinerable_waste_types() ),

	% 60 kg on average, before being set to at least 10 kg:
	ProductionQuantity = max( 0.01,
		class_RandomManager:get_exponential_value( _ProdLambda=1/0.06 ) ),

	% 500 kg on average:
	LocalStorage = 0.3 +
		class_RandomManager:get_positive_integer_exponential_value(
														  _StoreLambda=0.2 ),

	% Twice per week (homes are therefore synchronized and will remain so), in
	% seconds:
	%
	ProductionDuration = 7 * 24 * 60 * 60 / 2,


	% Location to be added later:
	NewAcc = [ { Name, ProductionType, float( ProductionQuantity ),
				float( LocalStorage ), ProductionDuration } | Acc ],

	define_residential_waste_sources( ResidentialSourceCount - 1, GISInfo,
									  NewAcc ).




% Adds the location to the wastesource build parameters (a kind of zip
% operation):
%
merge_parameters( Params, Locations, GISInfo ) ->
	% In-order is better:
	lists:reverse( merge_parameters( Params, Locations, _Acc=[], GISInfo ) ).


merge_parameters( _Params=[], _Locations=[], Acc, _GISInfo ) ->
	Acc;

merge_parameters( _Params=[ {  Name, ProductionType, ProductionQuantity,
							   LocalStorage, ProductionDuration } | Tp ],
				  _Locations=[ Loc | Tl ], Acc, GISInfo ) ->

	NewResidentialSourceDef = { class_ResidentialWasteSource, [ Name,
		   { wgs84_cartesian, Loc }, ProductionType, ProductionQuantity,
						LocalStorage, ProductionDuration, GISInfo ] },

	merge_parameters( Tp, Tl, [ NewResidentialSourceDef | Acc ], GISInfo ).



% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	DurationInSeconds = class_Actor:convert_ticks_to_seconds(
						  ?getAttr(production_duration), State ),

	% One one tank per source:
	[ WasteTank ] = ?getAttr(waste_capacity),

	text_utils:format( "Residential waste source '~s' (AAI: ~B) located at ~s "
		"(~s), generating on average ~f tons of waste of type ~p "
		"every ~s (~B ticks) on average, using for storage ~s, "
		"whose random state is ~p",
		[ ?getAttr(name),
		  class_Actor:get_abstract_identifier( State ),
		  class_GeolocalizedElement:interpret_location( State ),
		  class_PointOfInterest:to_string( State ),
		  ?getAttr(production_quantity),
		  ?getAttr(production_type),
		  time_utils:duration_to_string( 1000 * DurationInSeconds ),
		  ?getAttr(production_duration),
		  waste_utils:waste_tank_to_string( WasteTank ),
		  random_utils:get_random_state() ] ).


% In meters:

get_min_distance_between_residential_sources_and_others() ->
	30.


% In meters:
get_min_distance_between_two_residential_sources() ->
	30.
