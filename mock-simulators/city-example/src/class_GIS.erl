% Copyright (C) 2012-2023 EDF R&D

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


% Class offering basic <b>GIS</b> (<em>Geographic Information System</em>)
% services.
%
-module(class_GIS).


-define( class_description,
		 "Class offering basic GIS (Geographic Information System) services, "
		 "in terms of computation (not rendering)."
		 "The GIS acts as a location-based lookup service." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).


-type gis_pid() :: actor_pid().

-export_type([ gis_pid/0 ]).


% For testing:
%-export([ insert_poi_pair/2 ]).



% Design notes:
%
% We had to choose an internal geographic coordinate system.
%
% See http://en.wikipedia.org/wiki/Geographic_coordinate_system for a general
% discussion.
%
% We preferred the WGS 84 standard (World Geodetic System, last revised in 2004,
% see http://en.wikipedia.org/wiki/World_Geodetic_System), which is the
% reference coordinate system used by the Global Positioning System.
%
% See also: http://www.ngs.noaa.gov/faq.shtml#WGS84


% The GIS is itself an actor (a mostly idle one), as all points of interest (ex:
% an incinerator) need to register themselves to the GIS, so they need to rely
% on its PID - as soon as the initialisation phase. Thus the GIS is an actor, so
% that he can have its user identifier defined, and referenced by all points of
% interest.


% In the WGS system, a three-dimensional orthogonal coordinate system is placed
% in the center of mass (centroid) of the Earth.
%
% The Z axis (for ellipsoid height) points directly through the geographic North
% pole.
%
% The X axis (for latitude) points through the point where the equator meets the
% zero-meridian as defined in the geographic coordinate system (this point is
% located in the Atlantic Ocean). So it passes through the Prime Meridian at the
% Equator.
%
% The Y axis (for longitude) is defined to be orthogonal with the X and Z axis,
% and thus points through a point in the Eastern Indian Ocean (passing through
% 90 degrees East longitude at the Equator).


% Any point on Earth is defined by coordinates on these three axes, X, Y and Z,
% measured in meters or, here, in degrees for latitude and longitude
% (complemented with an altitude for example).

% A high-accuracy version of WGS84, known as ITRS (International Terrestrial
% Reference System) has been created in a number of versions since 1989.



% Type section.


-type degrees() :: unit_utils:degrees().
% Actual complete, approximated floating-point measurement of an angle (not a
% {Degree, Minute} pair for example, where a minute is 1/60 of a degree; no
% second either).


-type latitude() :: degrees().
% In degrees, North positive.

-type longitude() :: degrees().
% In degrees, East positive:


-type elevation() :: meters().
% Above the reference ellipsoid.


-type length() :: meters().
% Cartesian length.


-type wgs84_polar_coord() :: { latitude(), longitude(), elevation() }.
% A WGS polar coordinate.


% A linear:coordinate() is more general:
-type cartesian_coord() :: meters().


-type x_coord() :: cartesian_coord().

-type y_coord() :: cartesian_coord().

-type z_coord() :: cartesian_coord().


-type wgs84_cartesian_coord() :: { x_coord(), y_coord(), z_coord() }.
% Homomorphic to point3:point3().


-type geolocation_flavour() :: 'wgs84_polar'
							 | 'wgs84_polar_without_elevation'
							 | 'wgs84_cartesian'.
% List of the supported geolocation systems.


-type geolocation_coordinate() :: wgs84_polar_coord() | wgs84_cartesian_coord().


-type raw_location() :: wgs84_cartesian_coord().
% The actual coordinate, which is actually used, in fine (canonical, internal
% one).


-type geocontainer_pid() :: pid().
% To document that a PID belongs to a geo-container.


-type geo_coordinate() :: raw_location() | geocontainer_pid().
% Internal geographic representation, which is either direct (raw), or the PID
% of a geocontainer.


-type data_source() :: 'none' | ustring().
% Describes the various data sources to initialize this GIS service.


-type location_name() :: bin_string().
% The name of a location.


-type location_spec() :: { location_name(), geolocation_coordinate() }.
% The specification of a location (with an implicit geolocation flavour).



-type static_location() :: raw_location()
	| { class_GIS:geolocation_flavour(), class_GIS:geolocation_coordinate() }.
% Describes a static location, that is a location that corresponds to a fixed
% position in space (everything but the PID of a geocontainer).


-type location() :: geocontainer_pid() | static_location().
% Describes any kind of location.

-type road_network_pid() :: class_Mesh:mesh_pid().


-export_type([ geolocation_flavour/0, geolocation_coordinate/0, raw_location/0,
			   geo_coordinate/0,
			   degrees/0, latitude/0, longitude/0, elevation/0, length/0,
			   wgs84_polar_coord/0,
			   x_coord/0, y_coord/0, z_coord/0, wgs84_cartesian_coord/0,
			   location_name/0, static_location/0, location/0,
			   road_network_pid/0 ] ).


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type meters() :: unit_utils:meters().



% The class-specific attributes of a GIS instance are:
-define( class_attributes, [

	{ location_table, table( location_name(), geo_coordinate() ),
	  "a table whose keys are location names (i.e. binaries) and whose values "
	  "are their actual geolocation" },

	{ poi_table, table( poi_pid(), raw_location() ),
	  "a table allowing to resolve easily the actual location of POIs; "
	  "later we could imagine a better, spatial data-structure, like an octree "
	  "or a BSP, for faster topological look-ups" },

	{ road_network, road_network_pid(),
	  "the PID of the mesh storing the road network" },

	{ rendering_requested, boolean(),
	  "tells whether a rendering of the internal road network is requested" }

] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.GIS" ).


% For types:
-include("city_example_types.hrl").


% For all shared defines and types:
-include("class_GIS.hrl").


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


-define( internal_geolocation_flavour, wgs84_cartesian ).


% Used whenever (polar) coordinates do not include elevation:
-define( default_elevation, 1500.0 ).


% Parameters section.

% In meters:
-define( wgs84_major, 6378137.0 ).


% In meters:
-define( wgs84_minor, 6356752.3142 ).


% Eccentricity:
% (other definition: 0.081819191025)
-define( wgs84_eccentricity, 0.081819190928906199466 ).


% Eccentricity squared:
-define( wgs84_squared_eccentricity, 0.006694380004260806515 ).



% Implementation notes:
%
% The GIS service is currently meant to be a singleton.
%
% Warning: the GIS includes a mesh, which will interact (potentially
% asynchronously) with various nodes and edges. Beware of receiving unexpected
% messages!




% @doc Constructs a GIS service, from following parameters:
%
% - InitialDataSource is either 'none' (no initial data will then be read) or a
% filename (as a plain string) corresponding to a location data file
%
% - PrepareRendering tells whether this GIS shall generate a rendering of its
% road network (i.e. its internal mesh)
%
-spec construct( wooper:state(), class_Actor:actor_settings(), data_source(),
				 boolean() ) -> wooper:state().
construct( State, ActorSettings, 'none', PrepareRendering ) ->
	construct_common( ActorSettings, PrepareRendering, State );


construct( State, ActorSettings, Filename, PrepareRendering )
  when is_list( Filename ) ->

	BasicState = construct_common( ActorSettings, PrepareRendering, State ),

	FullFilename = file_utils:join( [
		class_Actor:get_deployed_root_directory( State ),
		"mock-simulators", "city-example", "test", Filename ] ),

	case file_utils:is_existing_file( FullFilename ) of

		true ->
			InitialTable = getAttribute( BasicState, location_table ),
			NewLocationTable = add_locations_from( FullFilename, InitialTable ),
			setAttribute( BasicState, location_table, NewLocationTable ) ;

		false ->
			throw( { gis_data_file_not_found, FullFilename } )

	end.



-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Knowing that, on successful termination, actors are deleted slightly
	% before results are requested, actors (like this GIS) should not in their
	% destructor delete any result producer they may have (like this mesh): not
	% only the result manager may then fail to interact with them, but also
	% deleting these producers may raise exceptions, as their result may not be
	% collected or even produced.

	%RoadNetworkPid = ?getAttr(road_network),

	% Would not suffice:
	%RoadNetworkPid ! { setResultProducedStatus, true },
	%RoadNetworkPid ! { setResultCollectedStatus, true },

	% As a consequence, this is a minor process leak (GIS is a singleton and has
	% only one mesh):
	%RoadNetworkPid ! delete,

	% Useless:
	%naming_utils:unregister( ?gis_name, global_only ),

	State.




% Section for member methods.


% @doc This is a passive actor: no spontaneous action from first diasca onward.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	InitialState = case ?getAttr(rendering_requested) of

		true ->
			% We wait for the second diasca, so that all initial actors are
			% known to be ready:
			%
			SentState = class_Actor:send_actor_message( _Target=self(),
								generateRendering, State ),

			setAttribute( SentState, rendering_requested, false );

		false ->
			State

	end,

	actor:return_state( InitialState ).



% @doc Generates a rendering of the road network.
-spec generateRendering( wooper:state(), sending_actor_pid() ) ->
										const_actor_oneway_return().
generateRendering( State, _SendingActorPid ) ->

	?getAttr(road_network) !
		{ generateTopologicalView, _DisplayWanted=false, self() },

	receive

		{ wooper_result, topological_view_generated } ->
			ok
	end,

	actor:const_return().



% @doc Records the specified locations in the GIS.
%
% (request, for synchronisation purposes)
%
-spec recordLocations( wooper:state(), geolocation_flavour(),
		[ location_spec() ] ) -> request_return( 'locations_recorded' ).
recordLocations( State, _GeolocationFlavour=?internal_geolocation_flavour,
				 LocationSpecs ) ->

	% Already in the right flavour:
	NewTable = table:add_entries( LocationSpecs, ?getAttr(location_table ) ),

	wooper:return_state_result(
		setAttribute( State, location_table, NewTable ), locations_recorded );


recordLocations( State, GeolocationFlavour, LocationSpecs ) ->

	ConvertedSpecs = [ { Name, convert_coordinate( Coord,
			_From=GeolocationFlavour, _To=?internal_geolocation_flavour ) }
							|| { Name, Coord } <- LocationSpecs ],

	{ RecordState, Res } = recordLocations( State,
								?internal_geolocation_flavour, ConvertedSpecs ),

	wooper:return_state_result( RecordState, Res ).




% @doc Resolves the specified location: returns its corresponding geolocation
% coordinates.
%
-spec resolveLocation( wooper:state(), location_name() ) ->
								const_request_return( geo_coordinate() ).
resolveLocation( State, LocationName ) ->

	LocationTable = ?getAttr(location_table),

	case table:lookup_entry( _K=LocationName, LocationTable ) of

		{ value, Coord } ->
			wooper:const_return_result( Coord );

		_ ->
			throw( { unknown_location, LocationName } )

	end.



% @doc Records the location of the specified point of interest.
%
% Supersedes any previous information for that POI.
%
-spec recordPointOfInterest( wooper:state(), poi_pid(), static_location() ) ->
									oneway_return().
recordPointOfInterest( State, POIPid, Location ) ->

	RawLocation = static_to_raw_location( Location ),

	NewPoiTable = table:add_entry( _K=POIPid, _V=RawLocation,
								   ?getAttr(poi_table) ),

	wooper:return_state( setAttribute( State, poi_table, NewPoiTable ) ).



% @doc Returns the point of interest nearest to the specified one.
-spec searchNearestPointOfInterest( wooper:state(), poi_pid() ) ->
											request_return( poi_pid() ).
searchNearestPointOfInterest( State, POIPid ) ->

	{ NewState, _Res=[ UniquePoint ] } =
		searchNearestPointsOfInterest( State, POIPid, _Count=1 ),

	wooper:return_state_result( NewState, UniquePoint ).



% @doc Returns the specified number of points of interest that are nearest to
% the specified one.
%
-spec searchNearestPointsOfInterest( wooper:state(), poi_pid(),
								count() ) -> request_return( [ poi_pid() ] ).
searchNearestPointsOfInterest( State, POIPid, Count ) ->

	{ NewState, Res } =
		searchNearestPointsOfInterest( State, POIPid, _ExcludedPOIs=[], Count ),

	wooper:return_state_result( NewState, Res ).



% @doc Returns the specified number of points of interest that are nearest to
% the specified one and are not among the specified ones to exclude.
%
% Returns POIs are sorted from closest to farthest.
%
-spec searchNearestPointsOfInterest( wooper:state(), poi_pid(), [ poi_pid() ],
						count() ) -> const_request_return( [ poi_pid() ] ).
searchNearestPointsOfInterest( State, POIPid, ExcludedPOIs, Count ) ->

	SelectedPOIs = get_nearest( POIPid, ExcludedPOIs, Count,
								?getAttr(poi_table) ),

	wooper:const_return_result( SelectedPOIs ).



% @doc Declares the specified road into this GIS.
%
% (request, for synchronicity)
%
-spec declareRoad( wooper:state(), road_pid(), poi_pid(), poi_pid() ) ->
									const_request_return( 'road_declared' ).
declareRoad( State, RoadPid, SourcePOIPid, TargetPOIPid ) ->

	Mesh = ?getAttr(road_network),

	Mesh ! { addLink, [ RoadPid, SourcePOIPid, TargetPOIPid ] },

	wooper:const_return_result( road_declared ).



% @doc Declares specified roads into this GIS.
%
% (request, for synchronicity)
%
-spec declareRoads( wooper:state(),
					[ { road_pid(), poi_pid(), poi_pid() } ] ) ->
							const_request_return( 'roads_declared' ).
declareRoads( State, RoadDeclarations ) ->

	Mesh = ?getAttr(road_network),

	% The synchronicity with the mesh comes from the fact that the GIS is the
	% single process to communicate with it:

	[ Mesh ! { addLink, [ RoadPid, SourcePOIPid, TargetPOIPid ] }
		|| { RoadPid, SourcePOIPid, TargetPOIPid } <- RoadDeclarations ],

	wooper:const_return_result( roads_declared ).



% @doc Declares specified POI into this GIS.
%
% (request, for synchronicity)
%
-spec declarePOI( wooper:state(), poi_pid() ) ->
									const_request_return( 'poi_declared' ).
declarePOI( State, POIPid ) ->

	?getAttr(road_network) ! { addNode, POIPid },

	wooper:const_return_result( poi_declared ).



% @doc Declares specified POIs into this GIS.
%
% (request, for synchronicity)
%
-spec declarePOIs( wooper:state(), [ poi_pid() ] ) ->
								const_request_return( 'poi_list_declared' ).
declarePOIs( State, POIPidList ) ->

	Mesh = ?getAttr(road_network),

	% The synchronicity with the mesh comes from the fact that the GIS is the
	% single process to communicate with it:

	[ Mesh ! { addNode, POIPid } || POIPid <- POIPidList ],

	wooper:const_return_result( poi_list_declared ).



% @doc Renders a view of the current state of this GIS.
%
% (const request, for synchronicity)
%
-spec render( wooper:state(), file_utils:directory_name() ) ->
								const_request_return( 'gis_rendering_done' ).
render( State, OutputDirectory ) ->

	RoadNetworkPid = ?getAttr(road_network),

	RoadNetworkPid ! { setRenderingDirectory, [ OutputDirectory ] },

	RoadNetworkPid ! { generateTopologicalView, _DisplayWanted=true, self() },

	receive

		{ wooper_result, topological_view_generated } ->
			wooper:const_return_result( gis_rendering_done )

	end.



% @doc Returns the PID of the road network (a mesh) owned by this GIS.
-spec getRoadNetworkPid( wooper:state() ) ->
								const_request_return( road_network_pid() ).
getRoadNetworkPid( State ) ->
	wooper:const_return_result( ?getAttr(road_network) ).



% @doc Traces the current content of this GIS (through the trace system).
-spec traceContent( wooper:state() ) -> const_oneway_return().
traceContent( State ) ->

	{ _SameState, ContentString } = executeRequest( State, toString ),

	?notice_fmt( "Current content: ~ts", [ ContentString ] ),

	wooper:const_return().



% @doc Returns a string describing the state of this GIS instance.
-spec toString( wooper:state() ) -> const_request_return( ustring() ).
toString( State ) ->

	LocationTable = ?getAttr(location_table),

	Locations = table:enumerate( LocationTable ),

	LocationStrings = [ text_utils:format( "~ts, at ~ts", [ LocName,
		wgs84_cartesian_to_string( LocCoord ) ] )
								|| { LocName, LocCoord } <- Locations ],

	FinalLocationString = case LocationStrings of

		[] ->
			"GIS not recording any location ";

		_ ->
			text_utils:format( "GIS recording following ~B locations: ~ts",
				[ length( LocationStrings ),
				  text_utils:strings_to_string( LocationStrings ) ] )

	 end,


	POITable = ?getAttr(poi_table),

	POILocPairs = table:enumerate( POITable ),

	POIStrings = [ text_utils:format( "~w, at ~ts", [ POIPid ,
		wgs84_cartesian_to_string( POICoord ) ] )
								|| { POIPid, POICoord } <- POILocPairs ],

	FinalPOIString = case POIStrings of

		[] ->
			"and not recording any point of interest";

		_ ->
			text_utils:format(
				"and recording following ~B points of interest: ~ts",
				[ length( POIStrings ),
				  text_utils:strings_to_string( POIStrings ) ] )

	 end,

	wooper:const_return_result( FinalLocationString ++ FinalPOIString ).




% Section for static methods.


% @doc Returns the PID of the default GIS (if any), or throws an exception.
-spec get_service() -> static_return( gis_pid() ).
get_service() ->
	GisPid = naming_utils:get_registered_pid_for( ?gis_name, global ),
	wooper:return_static( GisPid ).




% Conversion section.


% @doc Converts specified WGS84 geodetic latitude, longitude, elevation into
% cartesian geocentric coordinates.
%
-spec wgs84_polar_to_cartesian( wgs84_polar_coord() ) ->
									static_return( wgs84_cartesian_coord() ).
wgs84_polar_to_cartesian( { Latitude, Longitude, Elevation } ) ->

	% Source: http://www.forumsig.org/showthread.php?t=9120:

	% Note: should be checked against authoritative source.

	SinLatitude = math:sin( Latitude ),

	% Length of the normal line segment from the surface to the spin axis:
	L = ?wgs84_major / math:sqrt( 1.0 -
			( ?wgs84_squared_eccentricity * SinLatitude * SinLatitude ) ),

	% Lengthens the normal line to account for height above the surface:
	H = L + Elevation,

	CosLatitude = math:cos( Latitude ),

	X = H * CosLatitude * math:cos( Longitude ),

	Y = H * CosLatitude * math:sin( Longitude ),

	Z = ( L * ( (?wgs84_minor*?wgs84_minor) / (?wgs84_major*?wgs84_major) )
			+ Elevation ) * SinLatitude,

	wooper:return_static( { X, Y, Z } ).



% @doc Converts WGS84 cartesian geocentric coordinates into geodetic latitude,
% longitude, elevation.
%
-spec wgs84_cartesian_to_polar( wgs84_cartesian_coord() ) ->
									static_return( wgs84_polar_coord() ).
wgs84_cartesian_to_polar( _CartesianCoord ) ->
	throw( not_implemented_yet ).



% @doc Converts specified coordinates, expressed in the specified original
% geolocation flavour into the target one.
%
-spec convert_coordinate( geolocation_coordinate(), geolocation_flavour(),
		geolocation_flavour() ) -> static_return( geolocation_coordinate() ).
convert_coordinate( Coord, Flavour, Flavour ) ->
	wooper:return_static( Coord );

convert_coordinate( Coord, _OriginalFlavour=wgs84_polar,
					_TargetFlavour=wgs84_cartesian ) ->
	wooper:return_static( wgs84_polar_to_cartesian( Coord ) );

convert_coordinate( Coord, _OriginalFlavour=wgs84_cartesian,
					_TargetFlavour=wgs84_polar ) ->
	wooper:return_static( wgs84_cartesian_to_polar( Coord ) ).



% @doc Converts a static location into a raw, canonical one.
-spec static_to_raw_location( static_location() ) ->
									static_return( raw_location() ).
static_to_raw_location( { wgs84_polar, Coord } ) ->
	wooper:return_static( wgs84_polar_to_cartesian( Coord ) );

static_to_raw_location( { wgs84_cartesian, Coord } ) ->
	wooper:return_static( Coord ).




% Display section.


% @doc Returns a plain string describing specified WGS84 polar coordinate.
-spec wgs84_polar_to_string( wgs84_polar_coord() ) ->
								  static_return( ustring() ).
wgs84_polar_to_string( { Latitude, Longitude, Elevation } ) ->
	wooper:return_static(
		text_utils:format( "WGS84 polar coordinates: latitude = ~f degrees, "
						   "longitude = ~f degrees, elevation = ~f meters",
						   [ Latitude, Longitude, Elevation ] ) ).



% @doc Returns a plain string describing specified WGS84 cartesian coordinate.
-spec wgs84_cartesian_to_string( wgs84_cartesian_coord() ) ->
										static_return( ustring() ).
wgs84_cartesian_to_string( { X, Y, Z } ) ->
	wooper:return_static(
		text_utils:format( "WGS84 cartesian coordinates: X = ~f meters, "
						   "Y = ~f meters, Z = ~f meters", [ X, Y, Z ] ) ).



% @doc Returns the distance between the two specified geolocalized elements.
-spec compute_distance( geolocalized_pid(), geolocalized_pid() ) ->
												static_return( meters() ).
compute_distance( FirstElementPid, SecondElementPid ) ->

	FirstElementPid ! { getActualLocation, [], self() },

	FirstLoc = receive

		{ wooper_result, FirstCartesianCoord } ->
			FirstCartesianCoord

	end,

	SecondElementPid ! { getActualLocation, [], self() },

	SecondLoc = receive

		{ wooper_result, SecondCartesianCoord } ->
			SecondCartesianCoord

	end,

	wooper:return_static( point3:distance( FirstLoc, SecondLoc ) ).



% @doc Renders (synchronously) a view of the specified GIS.
-spec render_state( gis_pid() ) -> static_void_return().
render_state( GISPid ) ->

	GISPid ! { render, [], self() },

	receive

		{ wooper_result, gis_rendering_done } ->
			wooper:return_static_void()

	end.



% @doc Shutdowns the specified GIS service.
-spec shutdown( gis_pid() ) -> static_void_return().
shutdown( GISPid ) ->
	wooper:delete_synchronously_instance( GISPid ),
	wooper:return_static_void().




% Helper section.


% @doc Common part for all constructors.
%
% (helper)
%
construct_common( ActorSettings, PrepareRendering, State ) ->

	% First the direct mother classes:
	TraceState = class_Actor:construct( State, ActorSettings,
										?trace_categorize("GIS service") ),

	% Then the class-specific actions:

	% GIS bound to be a massive bottleneck:
	erlang:process_flag( priority, _Level=high ),

	% Ensures also it is a singleton indeed:
	naming_utils:register_as( ?gis_name, global_only ),

	RoadNetwork = class_Mesh:new_link( _MeshName="Road Network 2D Topology",
									   _Opts=[] ),

	EmptyTable = table:new(),

	setAttributes( TraceState, [ { location_table, EmptyTable },
								 { poi_table, EmptyTable },
								 { road_network, RoadNetwork },
								 { rendering_requested, PrepareRendering } ] ).



% @doc Adds locations read from specified file (supposedly already existing)
% into specified table, and returns an updated table.
%
% (helper)
%
add_locations_from( Filename, LocationTable ) ->
	add_locations_from( Filename, LocationTable,
				_AssumedGeolocationFlavour=wgs84_polar_without_elevation ).



add_locations_from( Filename, LocationTable, InputGeolocationFlavour ) ->

	% We expect a file where lines are in the form of:
	% { "Milpa Alta", { 19.192222, -99.023056 } }:
	%
	{ ok, LineElements } = file:consult( Filename ),

	%trace_utils:debug_fmt( "Read lines:~n~p", [ LineElements ] ),

	% Not using a list comprehension, as we want to crash if finding other
	% elements than (proper) pairs:

	%LocationSpecs = [ { text_utils:string_to_binary(LocationName),
	%	convert_coordinate( Coord, _From=InputGeolocationFlavour,
	%					   _To=?internal_geolocation_flavour )
	%				 | { LocationName, Coord } <- LineElements ],

	ReadEntries = filter_location_specs( LineElements,
										 InputGeolocationFlavour, _Acc=[] ),

	% Returns an updated table:
	table:add_entries( ReadEntries, LocationTable ).


% (helper)
filter_location_specs( _LineElements=[], _InputGeolocationFlavour, Acc ) ->
	% Order does not matter:
	Acc;

filter_location_specs( [ { LocationName, _Coord={ Latitude, Longitude } } | T ],
					   wgs84_polar_without_elevation, Acc ) ->

	BinLocationName = text_utils:string_to_binary( LocationName ),

	% Lacking elevation with this format:
	NewCoord = { Latitude, Longitude, ?default_elevation },

	filter_location_specs( T, wgs84_polar_without_elevation,
						   [ { BinLocationName, NewCoord } | Acc ] ).




% @doc Returns the TargetCount closest POIs from TargetPOIPid found in PoiTable
% that are not in the ExcludedPOIs list (and that are not TargetPOIPid either).
%
% (helper)
%
get_nearest( TargetPOIPid, ExcludedPOIs, TargetCount, PoiTable ) ->

	TargetPOILoc = table:get_value( TargetPOIPid, PoiTable ),

	% List of {poi_pid(), raw_location()} pairs:
	PoiInputList = table:enumerate( PoiTable ),

	% AccPOI :: [{ poi_pid(), square_distance()}], i.e. it will be a list of
	% pairs of the currently selected POIs each associated to their cached
	% square distance to POIPid. The pairs are ordered, from farthest to
	% closest.

	% POICount is the (cached) length of AccPOI:
	get_nearest( TargetPOIPid, TargetPOILoc, ExcludedPOIs, PoiInputList,
				 _POICount=0, _AccPOI=[], TargetCount, PoiTable ).


% Here we exhausted the POI candidates, but did not reach the specified count:
get_nearest( _TargetPOIPid, _TargetPOILoc, _ExcludedPOIs, _PoiInputList=[],
			 POICount, _AccPOI, TargetCount, _PoiTable )
  when POICount < TargetCount ->

	throw( { get_nearest_failed, not_enough_poi_candidates,
			 POICount, TargetCount } );


% Only success case (no more candidates, and TargetCount reached):
get_nearest( _TargetPOIPid, _TargetPOILoc, _ExcludedPOIs, _PoiInputList=[],
			 TargetCount, AccPOI, TargetCount, _PoiTable ) ->

	% First, keep the order but remove the cached distances:
	POIs = [ POI || { POI, _CachedDistance } <- AccPOI ],

	% Was stored with the most remote first:
	lists:reverse( POIs );


% From here there is always a new candidate to pop from input list:

% Not selecting the target POI as candidate:
get_nearest( TargetPOIPid, TargetPOILoc, ExcludedPOIs,
			 _PoiInputList=[ { TargetPOIPid, _CandidateLoc } | T ],
			 POICount, AccPOI, TargetCount, PoiTable ) ->
	get_nearest( TargetPOIPid, TargetPOILoc, ExcludedPOIs, T, POICount, AccPOI,
				 TargetCount, PoiTable );


% Here we do not have fully populated AccPOI:
get_nearest( TargetPOIPid, TargetPOILoc, ExcludedPOIs,
			 _PoiInputList=[ { CandidatePid, CandidateLoc } | T ],
			 POICount, AccPOI, TargetCount, PoiTable )
  when POICount < TargetCount ->

	case lists:member( CandidatePid, ExcludedPOIs ) of

		true ->
			% Let's skip this candidate then:
			get_nearest( TargetPOIPid, TargetPOILoc, ExcludedPOIs, T, POICount,
						 AccPOI, TargetCount, PoiTable );

		false ->

			% We will accept all candidates until reaching the count:
			POIDistance = point3:square_distance( TargetPOILoc,
												  CandidateLoc ),

			NewAccPOI = insert_poi_pair( { CandidatePid, POIDistance },
										 AccPOI ),

			get_nearest( TargetPOIPid, TargetPOILoc, ExcludedPOIs, T,
						 POICount+1, NewAccPOI, TargetCount, PoiTable )

	end;


% Here we have already a fully populated AccPOI:
get_nearest( TargetPOIPid, TargetPOILoc, ExcludedPOIs,
			 _PoiInputList=[ { CandidatePid, CandidateLoc } | T ], POICount,
			 AccPOI=[ {_FarthestPOI,MaxDistance} | TAcc ], TargetCount,
			 PoiTable ) ->

	case lists:member( CandidatePid, ExcludedPOIs ) of

		true ->
			% Let's skip this candidate then:
			get_nearest( TargetPOIPid, TargetPOILoc, ExcludedPOIs, T, POICount,
						 AccPOI, TargetCount, PoiTable );

		false ->

			% We will accept this candidate iff it is closer than the head of
			% AccPOI, as it will replace one of its elements:
			%
			case point3:square_distance( TargetPOILoc, CandidateLoc ) of

				D when D < MaxDistance ->
					% We have a new winner! It will replace the farthest one,
					% but we have to insert it in the right position though, as
					% next points can remove any number of other already
					% selected points:
					%
					NewAccPOI = insert_poi_pair( { CandidatePid, D }, TAcc ),

					% Just replaced and reordered, same length:
					get_nearest( TargetPOIPid, TargetPOILoc, ExcludedPOIs, T,
								 POICount, NewAccPOI, TargetCount, PoiTable );

				_ ->
					% Too far, hence not interesting:
					get_nearest( TargetPOIPid, TargetPOILoc, ExcludedPOIs, T,
								 POICount, AccPOI, TargetCount, PoiTable )

			end

	end.




% @doc Inserts the specified {POIPid, SquareDistance} pair into POIList, which
% is an ordered list of such pairs, in decreasing SquareDistance order.
%
% (helper)
%
insert_poi_pair( Pair, POIList ) ->
	insert_poi_pair( Pair, POIList, _Acc=[] ).



% Closer than all:
insert_poi_pair( Pair, _POIList=[], Acc ) ->
	lists:reverse( [ Pair | Acc ] );

% Head is farther, continue iterating:
insert_poi_pair( Pair={ _POIPid, SquareDistance }, [ H={ _PidH, SDistH } | T ],
				 Acc ) when SDistH > SquareDistance ->
	insert_poi_pair( Pair, T, [ H | Acc ] );

% Implicitly the pair is just closer than this head:
insert_poi_pair( Pair, POIList, Acc ) ->
	lists:reverse( Acc ) ++ [ Pair | POIList ].
