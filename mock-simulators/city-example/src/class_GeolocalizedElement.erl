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


% @doc Class modelling any model element that can be <b>geolocalized in the
% simulation world</b>.
%
-module(class_GeolocalizedElement).


-define( class_description,
		 "Class modelling any model element that can be geolocalized in the "
		 "simulation world."
		 "See also: class_GIS.erl" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).



% Exported helpers.
-export([ enter_in/2, interpret_location/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.GeolocalizedElement" ).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").



% Shorthands:

-type ustring() :: text_utils:ustring().


% The class-specific attributes of an instance of a geolocalized element are:
-define( class_attributes, [

	{ location, class_GIS:geo_coordinate(), "is either the current location of "
	  "this element or the PID of its immediate including geocontainer" },

	{ local_tracker_pid, instance_tracker_pid(),
	  "the PID of the local instance tracker (only for debugging purposes)" }

] ).




% @doc Creates a geolocalized element.
%
% The only parameter is the starting location, which is either:
%
%  - GeoContainerPid :: class_GeoContainer:container_pid() the PÏD of a
%  geo-container instance in which this element will be located initially
%
%  - {CoordinateType, Location}
%
%  - Location, where Location is implicitly a WGS84 polar coordinate
%
-spec construct( wooper:state(), class_GIS:location() ) -> wooper:state().
construct( State, GeoContainerPid ) when is_pid( GeoContainerPid ) ->
	enter_in( GeoContainerPid, State );


construct( State, { wgs84_polar, PolarCoord } ) ->

	CartesianCoord = class_GIS:wgs84_polar_to_cartesian( PolarCoord ),

	construct( State, { wgs84_cartesian, CartesianCoord } );


construct( State, { wgs84_cartesian, CartesianCoord } ) ->
	setAttributes( State, [ { location, CartesianCoord },
							{ local_tracker_pid,
							  class_InstanceTracker:get_local_tracker() } ] );


construct( State, ImplicitlyWGS84PolarCoord ) ->
	construct( State, { wgs84_polar, ImplicitlyWGS84PolarCoord } ).




% Methods section.


% @doc Returns the current location of this element (possibly the PID of a
% container).
%
-spec getLocation( wooper:state() ) ->
							const_request_return( class_GIS:geo_coordinate() ).
getLocation( State ) ->
	wooper:const_return_result( ?getAttr(location) ).



% @doc Returns the current actual (raw) location of this element.
-spec getActualLocation( wooper:state() ) ->
						 const_request_return( class_GIS:raw_location() ).
getActualLocation( State ) ->

	Loc = case ?getAttr(location) of

		ContainerPid when is_pid( ContainerPid ) ->

			% Recurses:
			ContainerPid ! { getActualLocation, [], self() },
			receive

				{ wooper_result, ActualLocation } ->
					ActualLocation

			end;

		CartesianCoord  ->
			CartesianCoord

	end,

	wooper:const_return_result( Loc ).



% @doc Sets the current location of this element.
-spec setLocation( wooper:state(), class_GIS:geo_coordinate() ) ->
						 oneway_return().
setLocation( State, NewLocation ) ->

	NewState = setAttribute( State, location, NewLocation ),

	wooper:return_state( NewState ).



% Helper functions.


% @doc Enters in specified geo-container, expecting this operation to succeed.
%
% Returns an updated state.
%
% Note: not synchronised in simulation (internal use only).
%
% (helper)
%
enter_in( GeoContainerPid, State ) ->

	GeoContainerPid ! { requestEntry, [], self() },

	receive

		{ wooper_result, entered } ->
			setAttributes( State, [
				{ location, GeoContainerPid },
				{ local_tracker_pid,
				  class_InstanceTracker:get_local_tracker() } ] );

		{ wooper_result, entry_refused } ->
			throw( { entry_refused, GeoContainerPid } )

	end.



% @doc Returns a textual description of the location of this element.
%
% (helper)
%
-spec interpret_location( wooper:state() ) -> ustring().
interpret_location( State ) ->

	case ?getAttr(location) of

		Pid when is_pid( Pid ) ->
			text_utils:format( "inside geocontained ~w", [ Pid ] );

		Loc ->
			class_GIS:wgs84_cartesian_to_string( Loc )

	end.
