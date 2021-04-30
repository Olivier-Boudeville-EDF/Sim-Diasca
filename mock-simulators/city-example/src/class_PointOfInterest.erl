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


-module(class_PointOfInterest).


-define( class_description,
		 "Class modeling a point of interest, which is basically any named "
		 "point on a thoroughfare network." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_GeoContainer, class_Graphable,
						 class_EngineBaseObject ] ).


% The class-specific attributes of an instance of a point of interest are:
-define( class_attributes, [

  { inbound_roads, [ road_pid() ],
	"listing the PID of the roads pointing to this location of interest" },

  { outbound_roads, [ road_pid() ],
	"listing the PID of the roads pointing from this location of interest" } ] ).


% Helpers:
-export([ to_string/1 ]).



% For type definitions:
-include("city_example_types.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.PointOfInterest" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Creates a new point of interest (POI), which is supposed located in a fixed,
% static position.
%
% The construction parameters are the name of this POI and its location, which
% is either:
%
% - { CoordinateType :: class_GIS:geolocation_flavour(), Location::
% class_GIS:geolocation_coordinate() }
%
% - Location :: the corresponding (static) location
%
-spec construct( wooper:state(), class_TraceEmitter:emitter_init(),
				 class_GIS:static_location(), gis_pid() ) -> wooper:state().
construct( State, Name, Location, GISPid ) ->

	GISPid ! { recordPointOfInterest, [ self(), Location ] },

	TraceState = class_EngineBaseObject:construct( State,
												   ?trace_categorize( Name ) ),

	GeoState = class_GeoContainer:construct( TraceState, Location ),

	Label = Name ++ "\\n" ++ text_utils:pid_to_string( self() ),

	GraphableState = class_Graphable:construct( GeoState,
								[ { label, Label }, { color, black } ] ),

	setAttributes( GraphableState, [
			{ inbound_roads, [] },
			{ outbound_roads, [] } ] ).




% Actor oneway section.



% Requests whether this POI can accept the specified vehicle and, if yes, accept
% it.
%
% Note: not to be mixed up with the class_GeoContainer:requestEntry/1 request.
%
-spec requestEntry( wooper:state(), vehicle_pid() ) -> actor_oneway_return().
requestEntry( State, VehiclePid ) ->

	% Each POI is a geo-container:
	{ ReqState, EnterRequestOutcome } =
		class_GeoContainer:request_entry( VehiclePid, State ),

	?debug_fmt( "Vehicle ~w requested entry, answer is ~p.",
				[ VehiclePid, EnterRequestOutcome ] ),

	% We must call a method to resolve the actual class name (probably a child
	% class of PointOfInterest:
	%
	{ _SameState, Classname } = executeRequest( ReqState, getClassname ),

	% Sends back the result:
	SentState = class_Actor:send_actor_message( VehiclePid,
			{ notifyEntryOutcome, [ EnterRequestOutcome, Classname ] },
			ReqState ),

	actor:return_state( SentState ).



% Requests this POI to find for the specified vehicle a road going to specified
% POI, and requests that road to accept it.
%
-spec takeRoadTo( wooper:state(), poi_pid(), vehicle_pid() ) ->
						actor_oneway_return().
takeRoadTo( State, TargetPOI, VehiclePid ) ->

	RoadPid = find_road_to( TargetPOI, State ),

	?debug_fmt( "Notifying on behalf of vehicle ~w road ~w that "
				"it enters it in order to go to POI ~w.",
				[ VehiclePid, RoadPid, TargetPOI ] ),

	% Sends the request on behalf of the vehicle, which will be notified
	% directly by the road thanks to a notifyRoadEntry call:
	%
	SentState = class_Actor:send_actor_message( RoadPid,
					{ driveIn, VehiclePid }, State ),

	actor:return_state( SentState ).




% Methods section.


% Declares an additional inbound road.
-spec declareInboundRoad( wooper:state(), road_pid() ) -> actor_oneway_return().
declareInboundRoad( State, RoadPid ) ->
	actor:return_state( appendToAttribute( State, inbound_roads, RoadPid ) ).



% Declares an additional outbound road.
-spec declareOutboundRoad( wooper:state(), road_pid() ) -> actor_oneway_return().
declareOutboundRoad( State, RoadPid ) ->
	actor:return_state( appendToAttribute( State, outbound_roads, RoadPid ) ).



% Returns the road connectivity of this point of interest.
-spec getConnectivity( wooper:state() ) ->
						const_request_return( { [ road_pid() ], [ road_pid() ] } ).
getConnectivity( State ) ->
	wooper:const_return_result(
	  { ?getAttr(inbound_roads), ?getAttr(outbound_roads) } ).



% Returns a list of the outbound POIs, i.e. the POIs that can be reached with a
% road starting from this POI.
%
-spec getOutboundPOIs( wooper:state() ) -> const_request_return( [ poi_pid() ] ).
getOutboundPOIs( State ) ->

	% We must rely on the AAIs in order to return a reproducible PID list:
	%
	% (we receive an unordered list of { RoadAAI, POIPID } pairs)
	%
	ReceivedPOIPairs = wooper:obtain_results_for_requests(
						 _RequestName=getTargetPOI, _RequestArgs=[],
						 _TargetInstancePIDs=?getAttr(outbound_roads) ),

	% We use AAI to sort POI PIDs reproducibly:
	SortedPairs = lists:keysort( _AAIIndex=1, ReceivedPOIPairs ),

	% Drop AAI and avoid useless duplicates:
	%SelectedPOIs = list_utils:uniquify( [ PoiPID
	%					  || { _RoadAAI, PoiPID } <- SortedPairs ] ),

	SelectedPOIs = [ PoiPID || { _RoadAAI, PoiPID } <- SortedPairs ],

	wooper:const_return_result( SelectedPOIs ).



% Registers this point of interest into the specified GIS.
%
% Note: not blocking, beware to synchronicity!
%
-spec registerInGIS( wooper:state(), gis_pid() ) -> const_oneway_return().
registerInGIS( State, GISPid ) ->

	GISPid ! { declarePOI, self() },

	wooper:const_return().



% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	TrackerPid = ?getAttr(local_tracker_pid),

	Inbounds = ?getAttr(inbound_roads),
	Outbounds = ?getAttr(outbound_roads),

	RoadFun = fun( RoadPid, AccStrings ) ->

			RoadAAI = class_InstanceTracker:get_identifier_for( RoadPid,
																TrackerPid ),

			[ text_utils:format( "~B (~w)",
								 [ RoadAAI, RoadPid ] ) | AccStrings ]

	end,

	InStrings = lists:foldl( RoadFun, _Acc0=[], _InList=Inbounds ),

	InString = text_utils:join( ", ", lists:reverse( InStrings ) ),

	OutStrings = lists:foldl( RoadFun, _Acc0=[], _OutList=Outbounds ),

	OutString = text_utils:join( ", ", lists:reverse( OutStrings ) ),


	text_utils:format( "Point of interest '~s' located at ~s, "
					   "having ~B inbound road(s) (i.e. ~s) and "
					   "~B outbound road(s) (i.e. ~s)",
					   [ ?getAttr(name),
						 class_GeolocalizedElement:interpret_location( State ),
						 length( Inbounds ),
						 InString,
						 length( Outbounds ),
						 OutString ] ).



% Static methods section.


% Helper functions.



% Returns one of the roads that lead directly to specified POI, chosen at
% random.
%
% (helper)
%
find_road_to( TargetPOI, State ) ->

	OutboundRoads = ?getAttr(outbound_roads),

	% Allowed as connectivity is static:
	CandidateRoads = lists:foldl( fun( OutBoundRoad, Acc ) ->
						 OutBoundRoad ! { getTargetPOI, [], self() },
						 receive

							 { wooper_result, { _RoadAAI, TargetPOI } } ->
								 [ OutBoundRoad | Acc ];

							 { wooper_result, { _RoadAAI, _OtherPOI } } ->
								 Acc

						 end
								  end,
				 _Acc0=[],
				_List=OutboundRoads ),

	case CandidateRoads of

		[] ->
			throw( { cannot_reach, { from, self() }, { to, TargetPOI },
					 { using_roads, OutboundRoads } } );

		_ ->
			ok

	end,

	list_utils:draw_element( CandidateRoads ).
