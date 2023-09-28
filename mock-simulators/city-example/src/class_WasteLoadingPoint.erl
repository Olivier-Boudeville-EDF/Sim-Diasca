% Copyright (C) 2012-2023 EDF R&D
%
% This file is part of Sim-Diasca.
%
% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: 2012.


% @doc Class modelling a <b>waste loading point</b>.
-module(class_WasteLoadingPoint).


-define( class_description,
		 "Class modelling a waste loading point, i.e. a physical location "
		 "from which wastes can be loaded, by garbage trucks." ).


% For waste_tank() and al:
-include("city_example_types.hrl").


% For city_max_relative_error:
-include("city_example_settings.hrl").


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_GeoContainer ] ).


% The class-specific attributes of an instance of loading point are:
-define( class_attributes, [

	{ waste_capacity, [ waste_capacity() ],
	  "a plain list storing the state of the waste storage tanks" } ] ).



% Design notes:
%
% Even if usually the type of waste wanted to be loaded/unloaded for a loading
% point can be deduced, we specify the type of wastes that is targered
% nevertheless, for more flexibility later.


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.Waste.LoadingPoint" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").




% @doc Creates a waste loading point.
%
% Construction parameters are:
%
% - Location: the location of this loading point
%
% - CapacityInformation describes the waste storage capacity of this point
%
-spec construct( wooper:state(), class_GIS:location(), waste_capacity() ) ->
						wooper:state().
construct( State, Location, CapacityInformation ) ->

	ContainerState = class_GeoContainer:construct( State, Location ),

	Tanks = manage_capacity_information( CapacityInformation ),

	setAttribute( ContainerState, waste_capacity, Tanks ).




% Methods section.



% @doc Tries to load from this loading point as much as possible of the
% specified mass compatible with specified waste type into the calling actor,
% which is expected to be a waste transport, located at this point and looking
% for additional waste.
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

	WasteTanks = ?getAttr(waste_capacity),

	[ waste_utils:check_waste_tank( T ) || T <- WasteTanks ],

	{ _State, DescString } = executeRequest( State, toString ),

	?info_fmt( "Trying to load up to ~f tons of waste of "
		"type compatible with '~ts' from the ~B tanks of ~ts "
		"to docked waste transport ~w.",
		[ MaxWantedMass, WasteType, length( WasteTanks ),
		  DescString, WasteLoaderPid ] ),

	LoadState = case get_waste_from_tanks( WasteType, MaxWantedMass,
										   WasteTanks ) of

		false ->
			?info_fmt( "No waste could be loaded to transport ~w.",
					   [ WasteLoaderPid ] ),

			class_Actor:send_actor_message( WasteLoaderPid,
											notifyNoLoadedWaste, State );


		{ NewWasteTanks, RemainingFreeMass, Type } ->

			% Waste loaded:
			LoadedMass = MaxWantedMass - RemainingFreeMass,

			LoadingTickCount = get_loading_duration( Type, LoadedMass, State ),

			?info_fmt( "Loading, to transport ~w, ~f tons of waste "
				"of type ~ts, this will last for ~B ticks.",
				[ WasteLoaderPid, LoadedMass, Type, LoadingTickCount ] ),

			SentState = class_Actor:send_actor_message( WasteLoaderPid,
				{ notifyLoadedWaste,
					[ LoadedMass, Type, LoadingTickCount ] },
				State ),

			setAttribute( SentState, waste_capacity, NewWasteTanks )

	end,

	[ waste_utils:check_waste_tank( T )
		|| T <- getAttribute( LoadState, waste_capacity ) ],

	{ _SameState, Desc } = executeRequest( LoadState, toString ),

	?info_fmt( "After this loading attempt, new state is: ~ts.", [ Desc ] ),

	actor:return_state( LoadState ).



% @doc Does its best to retrieve the specified quantity of waste (compatible
% with the specified type) from the specified waste tanks.
%
% Returns either 'false' if no waste at all was taken from tanks, otherwise
% returns a triplet made of updated waste tanks, the remaining requested mass
% that could not be transferred (if any) and the overall type of the waste that
% has been loaded.
%
% (helper)
%
get_waste_from_tanks( WasteType, MaxWantedMass, WasteTanks ) ->
	get_waste_from_tanks( WasteType, MaxWantedMass, WasteTanks, _AccTanks=[],
						  _LoadedWasteType=undefined ).


get_waste_from_tanks( _WasteType, _RemainingFreeMass, _WasteTanks=[], _AccTanks,
					  _LoadedWasteType=undefined ) ->
	% No loaded waste type defined, hence nothing loaded:
	false;

get_waste_from_tanks( _WasteType, RemainingFreeMass, _WasteTanks=[], AccTanks,
					  LoadedWasteType ) ->
	{ AccTanks, RemainingFreeMass, LoadedWasteType };


get_waste_from_tanks( WasteType, RemainingFreeMass, _WasteTanks=[
		Tank=#waste_tank{ current_mass_stored=CurrentTankMass } | T ],
		AccTanks, LoadedWasteType ) ->

	case math_utils:is_null( CurrentTankMass ) of

		true ->
			% An empty tank is of no use here, continuing iterating:
			get_waste_from_tanks( WasteType, RemainingFreeMass, T,
								  [ Tank | AccTanks ], LoadedWasteType );

		% Non-empty tank here:
		false ->
			TankWasteType = Tank#waste_tank.current_type,

			case waste_utils:can_be_mixed( WasteType, TankWasteType ) of

				true ->

					% Yes, so let's empty this tank as much as possible:
					case CurrentTankMass > RemainingFreeMass of

						true ->

							% Here we will saturate the truck with this tank:
							UpdatedTank = waste_utils:remove_waste_from_tank(
								Tank, RemainingFreeMass ),

							% Truck full, hence no need to recurse more:
							NewTanks = [ UpdatedTank | T ] ++ AccTanks,

							% We update the type as well, otherwise we could
							% keep the one of the possibly empty truck which
							% would then be 'none':
							%
							{ NewTanks, _NoMoreMass=0.0, TankWasteType };

						false ->
							% Here we will fully deplete the tank:
							UpdatedTank = waste_utils:remove_waste_from_tank(
								Tank, CurrentTankMass ),

							NewRemainingFreeMass =
								RemainingFreeMass - CurrentTankMass,

							% Same remark for the update of waste type:
							get_waste_from_tanks( WasteType,
								NewRemainingFreeMass, T,
								[ UpdatedTank | AccTanks ], TankWasteType )

					end ;

				false ->
					% Unmatching waste type for this tank, let's continue then:
					get_waste_from_tanks( WasteType, RemainingFreeMass,  T,
						[ Tank | AccTanks ], LoadedWasteType )

			end

	end.




% Helper functions.


% @doc Checkings.
%
% (helper)
%
manage_capacity_information( CapacityInformation ) ->
	[ waste_utils:check_waste_tank( Tank ) || Tank <- CapacityInformation ],
	CapacityInformation.



% @doc Returns the duration needed, in ticks, for the loading of specified mass
% of specified waste type in a waste transport.
%
% (helper)
%
get_loading_duration( _WasteType, LoadedMass, State ) ->

	% A base of 3 minutes, plus 2 minutes per ton (the waste type does not
	% matter in this model):
	%
	Seconds = ( 3 + 2 * LoadedMass ) * 60,

	class_Actor:convert_seconds_to_ticks( Seconds, ?city_max_relative_error,
										  State ).
