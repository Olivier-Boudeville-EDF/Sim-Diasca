% Copyright (C) 2012-2022 EDF R&D

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


% @doc Class modelling a <b>waste unloading point</b>.
-module(class_WasteUnloadingPoint).


-define( class_description,
		 "Class modelling a waste unloading point, i.e. a physical location to "
		 "which wastes can be unloaded, by garbage trucks." ).


% For waste_tank() and al:
-include("city_example_types.hrl").


% For city_max_relative_error:
-include("city_example_settings.hrl").


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_GeoContainer ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.Waste.UnloadingPoint" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% The class-specific attributes of an instance of unloading point are:
-define( class_attributes, [

	{ waste_capacity, [ waste_capacity() ],
	  "is a plain list storing the state of the waste storage tanks" } ] ).




% @doc Creates a waste unloading point.
%
% Construction parameters are:
%
% - Location: the location of this unloading point
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


% @doc Tries to unload to this unloading point as much as possible of the
% specified mass of specified waste type (possibly any) from the caller (which
% is expected to be a waste transport requesting to empty its waste).
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
unloadWaste( _State, _WasteType=none, _ProposedMass, _WasteUnloaderPid ) ->
	throw( cannot_unload_untyped_waste );

unloadWaste( State, WasteType, ProposedMass, WasteUnloaderPid ) ->

	WasteTanks = ?getAttr(waste_capacity),

	[ waste_utils:check_waste_tank( T ) || T <- WasteTanks ],

	{ _State, DescString } = executeRequest( State, toString ),

	?info_fmt( "Trying to dispatch ~f tons of waste of type ~ts "
		"into ~B tanks in ~ts.",
		[ ProposedMass, WasteType, length( WasteTanks ), DescString ] ),

	UnloadState = case dispatch_waste_into_tanks( WasteType, ProposedMass,
												  WasteTanks ) of

		false ->
			?info_fmt( "No waste could be unloaded from transport ~w.",
						[ WasteUnloaderPid ] ),
			class_Actor:send_actor_message( WasteUnloaderPid,
											notifyNoUnloadedWaste, State );

		{ NewWasteTanks, RemainingMass } ->

			UnloadedMass = ProposedMass - RemainingMass,

			UnloadingTickCount =
				 get_unloading_duration( WasteType, UnloadedMass, State ),

			?info_fmt( "Unloading, from transport ~w, ~f tons of waste "
				"of type ~ts, this will last for ~B ticks.",
				[ WasteUnloaderPid, UnloadedMass, WasteType,
				  UnloadingTickCount ] ),

			SentState = class_Actor:send_actor_message( WasteUnloaderPid,
				{ notifyUnloadedWaste,
					[ UnloadedMass, WasteType, UnloadingTickCount ] }, State ),

			setAttribute( SentState, waste_capacity, NewWasteTanks )

	end,

	[ waste_utils:check_waste_tank( T )
		|| T <- getAttribute( UnloadState, waste_capacity ) ],

	actor:return_state( UnloadState ).




% Helper functions.


% Checkings.
%
% (helper)
%
manage_capacity_information( CapacityInformation ) ->
	[ waste_utils:check_waste_tank( Tank ) || Tank <- CapacityInformation ].



% @doc Does its best to dispatch the specified quantity of waste (of a specified
% type) into the specified waste tanks.
%
% Returns either 'false' if no waste at all was transferred to tanks, otherwise
% returns a pair of updated waste tanks and the remaining mass that could not be
% transferred (if any), and thus is remaining.
%
% (helper)
%
dispatch_waste_into_tanks( WasteType, ProposedMass, WasteTanks ) ->
	dispatch_waste_into_tanks( WasteType, ProposedMass, WasteTanks, _Acc=[] ).


% Two terminating cases:
dispatch_waste_into_tanks( _WasteType, _RemainingMass, _WasteTanks=[],
						   _Acc=[] ) ->
	% No tank changed:
	false;

dispatch_waste_into_tanks( _WasteType, RemainingMass, _WasteTanks=[],
						   AccTank ) ->
	% At least one tank received waste:
	[ waste_utils:check_waste_tank( T ) || T <- AccTank ],
	{ AccTank, RemainingMass };


dispatch_waste_into_tanks( WasteType, RemainingMass, _WasteTanks=[
	  Tank=#waste_tank{ allowed_types=AllowedTypes,
						current_type=TankWasteType,
						current_mass_stored=CurrentMass,
						max_mass_stored=MaxMass } | T ], AccTank ) ->

	% To be accepted, an incoming waste must be among the allowed ones, and
	% compatible with what is already stored (if any):
	%
	case lists:member( WasteType, AllowedTypes )
		andalso waste_utils:can_be_mixed( TankWasteType, WasteType ) of

		true ->

			% Eligible tank, waste-type. Has room left?
			case MaxMass - CurrentMass of

				Margin when Margin > 0.0 ->

					% Yes, this tank has room, at least to some extent:
					case RemainingMass > Margin of

						true ->

							% We can fill this tank, but some waste will remain:
							UpdatedTank = waste_utils:add_waste_to_tank( Tank,
											Margin, WasteType ) ,

							waste_utils:check_waste_tank( UpdatedTank ),

							% So we keep on iterating here:
							dispatch_waste_into_tanks( WasteType,
								RemainingMass - Margin, T,
								[ UpdatedTank | AccTank ] );


						false ->

							% We can fully put the remaining waste into that
							% tank:
							%
							LastTank = waste_utils:add_waste_to_tank( Tank,
									RemainingMass, WasteType ),

							waste_utils:check_waste_tank( LastTank ),

							% Returning directly here (no recursion):
							{ [ LastTank | T ] ++ AccTank, _RemainingMass=0.0 }

					end;

				_ZeroMargin ->
					%trace_utils:debug( "Tank full." ),
					dispatch_waste_into_tanks( WasteType, RemainingMass, T,
											   [ Tank | AccTank ] )
			end;


		false ->

			%trace_utils:debug_fmt( "Non-compatible waste types (tank: ~ts, "
			%   "waste: ~ts), "continuing iterating.",
			%   [ TankWasteType, WasteType ] ),

			dispatch_waste_into_tanks( WasteType, RemainingMass, T,
									   [ Tank | AccTank ] )

	end.



% @doc Returns the duration needed, in ticks, for the unloading of specified
% mass of specified waste type in a waste transport.
%
% (helper)
%
get_unloading_duration( _WasteType, UnloadedMass, State ) ->

	% A base of 2 minutes, plus 1 minute per ton:
	Seconds = ( 2 + 1 * UnloadedMass ) * 60,

	class_Actor:convert_seconds_to_ticks( Seconds, ?city_max_relative_error,
										  State ).
