% Copyright (C) 2012-2024 EDF R&D

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


% @doc Class modelling an <b>abstract waste transport</b>.
-module(class_WasteTransport).


-define( class_description, "Class modelling an abstract waste transport." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor, class_GeolocalizedElement ] ).


% The class-specific attributes of an instance of a waste transport instance
% are:
%
-define( class_attributes, [

	{ tank, waste_tank(), "the tank this transport stores waste in" },

	{ probe_ref, probe_ref(),
	  "the probe (if any) associated to this transport" } ] ).



% For all shared defines and types:
-include("city_example_types.hrl").


% Design notes:
%
% - currently a waste transport only transports one kind of waste (even if it,
% when empty, can load various different types of wastes)
%
% - a waste transport comprises exactly one waste tank



% Type section.


-type supported_waste_state() :: physical_state().
% Describes the state (ex: liquid, solid, etc.) of the wastes that can be
% transported.


-export_type([ supported_waste_state/0 ]).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.Waste.Transport" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Constructs a waste transport, from following parameters:
%
% - InitialLocation is the (initial) location of this waste transport (generally
% a point of interest)
%
% - MaxTransportedMass :: unit_utils:tons() is the maximum transported mass
%
% - MaxTransportedVolume :: unit_utils:cubic_meters() is the maximum
% transported volume
%
% - SupportedWasteStates :: [ supported_waste_state() ] is the list of the waste
% states this transport can support
%
% A waste transport is created empty.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_GIS:location(),
				 unit_utils:cubic_meters(), unit_utils:tons(),
				 [ supported_waste_state() ] ) -> wooper:state().
construct( State, ActorSettings, TransportName, InitialLocation,
		   MaxTransportedVolume, MaxTransportedMass, SupportedWasteStates ) ->

	FullName = ?trace_categorize(TransportName),

	ActorState = class_Actor:construct( State, ActorSettings, FullName ),

	GeoState = class_GeolocalizedElement:construct( ActorState,
													InitialLocation ),

	Tank = #waste_tank{
		id=1,

		% All types allowed here:
		allowed_types=SupportedWasteStates,

		current_type=none,
		current_volume_stored=0.0,
		max_volume_stored=MaxTransportedVolume,
		current_mass_stored=0.0,
		max_mass_stored=MaxTransportedMass,
		busy=false },

	ActualName = pair:first( FullName ),

	% Depending on the choice of the result manager, it will be either a PID (if
	% the corresponding result is wanted) or a 'non_wanted_probe' atom:
	%
	TransportProbeRef = class_Actor:declare_probe(
		_Name=text_utils:format( "~ts Transported Waste Stock Probe",
								 [ ActualName ] ),
		_Curves=[ "Quantity of waste currently stored (in tons)" ],
		_Zones=[],
		_Title=text_utils:format( "Waste Storage Monitoring "
								  "for Transport ~ts", [ ActualName ] ),
		_XLabel="Simulation time",
		_YLabel="Tons of wastes currently transported",
		GeoState ),

	setAttributes( GeoState, [ { tank, Tank },
							   { probe_ref, TransportProbeRef } ] ).




% Section for member methods.


% @doc Sends an update to the associated probe.
-spec updateProbe( wooper:state() ) -> const_oneway_return().
updateProbe( State ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),

	% Manages automatically the fact that the creation of this probe may have
	% been rejected by the result manager:
	%
	class_Probe:send_data( ?getAttr(probe_ref), CurrentTickOffset,
		{ (?getAttr(tank))#waste_tank.current_mass_stored } ),

	wooper:const_return().



% Section for plain methods.


% @doc Returns a string describing the state of this instance.
-spec toString( wooper:state() ) -> const_request_return( ustring() ).
toString( State ) ->

	Tank = ?getAttr(tank),

	FinalString = text_utils:format( "Waste transport containing ~ts",
					[ waste_utils:waste_tank_to_string( Tank ) ] ),

	wooper:const_return_result( FinalString ).
