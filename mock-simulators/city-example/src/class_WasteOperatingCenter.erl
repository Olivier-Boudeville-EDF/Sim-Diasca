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


% @doc Class modelling a <b>waste operating center</b>.
-module(class_WasteOperatingCenter).


-define( class_description,
		 "Class modelling a waste operating center, i.e. a centralised "
		 "organisation that drives a waste chain (waste loading and unloading "
		 "points, garbage trucks, etc.)" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_GeolocalizedElement ] ).



-type entry_outcome() :: 'entered' | 'entry_refused'.
% Possible answer to an entry request.



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.GeoContainer" ).


% The class-specific attributes of a geolocalized element are:
-define( class_attributes, [

	{ location, class_GIS:geo_coordinate(),
	  "the current location of this element" } ] ).


-include_lib("wooper/include/wooper.hrl").



% @doc Creates a waste operating center.
%
% The only parameter is the starting location, which is either:
%
% - ContainerPid :: pid() is the PID of a parent geo-container, supposedly able
% to accept this container
%
% - { CoordinateType :: class_GIS:geolocation_flavour(), Location::
% class_GIS:geolocation_coordinate() }
%
% - Location :: class_GIS:geolocation_coordinate(), where Location is implicitly
% a WGS84 polar coordinate
%
-spec construct( wooper:state(), class_GIS:location() ) -> wooper:state().
construct( State, ContainerPid ) when is_pid( ContainerPid ) ->
	class_GeolocalizedElement:construct( State, ContainerPid );


construct( State, { wgs84_polar, PolarCoord } ) ->
	CartesianCoord = class_GIS:wgs84_polar_to_cartesian( PolarCoord ),
	construct( State, { wgs84_cartesian, CartesianCoord } );


construct( State, { wgs84_cartesian, PolarCoord } ) ->
	setAttribute( State, location, PolarCoord );


construct( State, ImplicitlyWGS84PolarCoord ) ->
	construct( State, { wgs84_polar, ImplicitlyWGS84PolarCoord } ).



% Methods section.


% @doc Requests this container to enter.
-spec requestStaticEntry( wooper:state() ) -> request_return( entry_outcome() ).
requestStaticEntry( _State ) ->
	throw( is_abstract ).
