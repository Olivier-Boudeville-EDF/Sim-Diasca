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


-module(class_RoadJunction).


-define( class_description,
		 "Class modelling a road junction, where at least two roads meet." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor, class_PointOfInterest ] ).




% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.Traffic.Road" ).



-type road_count() :: basic_utils:count().

-export_type([ road_count/0 ]).



% Allows to use macros for trace sending (to be included after the WOOPER
% header):
%
-include("sim_diasca_for_actors.hrl").


% location_generator_pid(), gis_pid():
-include("city_example_types.hrl").



% The class-specific attributes of an instance of a road junction are:
-define( class_attributes, [

  { connectivity, { road_count(), road_count() },
	"respectively the expected final number of inbound and outbound roads" } ] ).



% Inherited attributes of interest:
%
% - inbound_roads :: [ road_pid() ], listing the PID of the roads pointing to
% this junction
%
% - outbound_roads :: [ road_pid() ], listing the PID of the roads pointing from
% this junction





% Implementation notes:
%
% The connectivity (with roads) is resolved after the construction of the
% junction.



% Creates a new road junction.
%
% Construction parameters are:
%
% - ActorSettings is the AAI assigned by the load-balancer to this actor
%
% - Name is the name of this junction (as a plain string)
%
% - Location: the (static) location of this junction
%
% - InboundCount: the number of inbound roads for this junction
%
% - OutboundCount: the number of inbound roads for this junction
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_GIS:static_location(),
				 basic_utils:count(), basic_utils:count(), gis_pid() ) ->
					   wooper:state().
construct( State, ActorSettings, Name, Location, InboundCount, OutboundCount,
		   GISPid ) ->

	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(Name) ),

	PointState = class_PointOfInterest:construct( ActorState, Name, Location,
												  GISPid ),

	setAttributes( PointState, [
		{ connectivity, { InboundCount, OutboundCount } },
		{ color, green } ] ).



% Methods section.



% First scheduling on a road junction.
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) -> actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% This actor is mostly passive.

	?info_fmt( "Road junction just created: ~s", [ to_string( State ) ] ),

	% Done once, to be able to trace a state in which the connectivity has been
	% updated (roads notify their endpoint in first diasca):
	%
	PlanState = class_Actor:scheduleNextSpontaneousTick( State ),

	actor:return_state( PlanState ).




% The definition of the spontaneous behaviour of this road junction.
-spec actSpontaneous( wooper:state() ) -> const_oneway_return().
actSpontaneous( State ) ->

	% Output once (no next tick planned):
	?info_fmt( "~s ready.", [ to_string( State ) ] ),

	wooper:const_return().



% Returns the unsatisfied connections (if any) for that junction, i.e.:
%
% - 'fully_connected' if that junction is full
%
% - or { lacking_outbounds, LackOutboundCount, OutboundPOIs } if outbound
% connections are lacking (LackOutboundCount is their count, OutboundPOIs is the
% list of PIDs of the already-connected outbound POIs)
%
% - or { lacking_inbounds, LackInboundCount, InboundPOIs } is the same for
% inbounds
%
% - or { lacking_both, LackInboundCount, InboundPOIs, LackOutboundCount,
% OutboundPOIs } if connections are lacking in both directions
%
getUnsatisfiedConnections( State ) ->

	{ InboundCount, OutboundCount } = ?getAttr(connectivity),

	Inbound = ?getAttr(inbound_roads),
	Outbound = ?getAttr(outbound_roads),

	InboundLen = length( Inbound ),
	OutboundLen = length( Outbound ),

	Res = case InboundLen =:= InboundCount of

		true ->

			case OutboundLen =:= OutboundCount of

				true ->
					fully_connected;

				false ->

					LackOutboundCount = OutboundCount - OutboundLen,

					OutboundPOIs = [ resolve_road_endpoint( R )
									 || R <- Outbound ],

					{ lacking_outbounds, LackOutboundCount, OutboundPOIs }

			end;


		false ->

			case OutboundLen =:= OutboundCount of

				true ->
					LackInboundCount = InboundCount - InboundLen,

					InboundPOIs = [ resolve_road_endpoint( R )
									|| R <- Inbound ],

					{ lacking_inbounds, LackInboundCount, InboundPOIs };

				false ->

					LackOutboundCount = OutboundCount - OutboundLen,

					OutboundPOIs = [ resolve_road_endpoint( R )
									 || R <- Outbound ],

					LackInboundCount = InboundCount - InboundLen,

					InboundPOIs = [ resolve_road_endpoint( R )
									|| R <- Inbound ],

					{ lacking_both, LackInboundCount, InboundPOIs,
					  LackOutboundCount, OutboundPOIs }


			end

	end,

	wooper:const_return_result( Res ).




% Returns the PID of the POI at the other end of the specified road.
%
% (helper)
%
resolve_road_endpoint( RoadPid ) ->

	RoadPid ! { getOtherEndpoint, [], self() },

	receive

		{ wooper_result, POI } ->
			POI

	end.




% Static method section.


% Generates a list of instance definitions for the specified number of
% initial road junctions.
%
-spec generate_definitions( basic_utils:count(), location_generator_pid(),
							gis_pid() | instance_loading:id_ref() ) ->
					static_return( [ class_Actor:instance_creation_spec() ] ).
generate_definitions( JunctionCount, LocationGeneratorPid, GISInfo ) ->

	% Triggers the location generation request in parallel:
	LocationGeneratorPid ! { generateNonAdjacentLocations,
			   [ JunctionCount,
				 get_min_distance_between_road_junctions_and_others(),
				 get_min_distance_between_two_road_junctions() ], self() },

	CreationSpecs = define_junctions( JunctionCount, GISInfo, _Acc=[] ),

	wooper:return_static( CreationSpecs ).



define_junctions( _Junctioncount=0, GISInfo, Acc ) ->

	% All road junctions defined, adding locations as returned by the
	% generateNonAdjacentLocations request:
	%
	receive

		{ wooper_result, Locations } when is_list( Locations ) ->
			% Creates now the full construction parameters:
			merge_parameters( Acc, Locations, GISInfo )

	end;

define_junctions( JunctionCount, GISInfo, Acc ) ->

	% Defines the build parameters for a new junction; we want to end up with a
	%   list of {class_RoadJunction, [Name, Location, InboundCount,
	%                                 OutboundCount, GISInfo]} elements.

	Name = text_utils:format( "RoadJunction-~B", [ JunctionCount ] ),

	% Inbound and outbound must be each:
	%
	% - positive integer
	% - at least 1
	% - on average, 3
	% - no more than 5 each

	Mean = 2,
	StdDeviation = 2,

	DrawnInboundCount = min( 5,
			  1 + class_RandomManager:get_positive_integer_gaussian_value(
												Mean, StdDeviation ) ),
	DrawnOutboundCount = min( 1,
			  1 + class_RandomManager:get_positive_integer_gaussian_value(
												Mean, StdDeviation ) ),


	% Location and GIS PID to be added later:
	NewAcc = [ { Name, DrawnInboundCount, DrawnOutboundCount } | Acc ],

	define_junctions( JunctionCount-1, GISInfo, NewAcc ).



% Adds the location to the road build parameters (a kind of zip operation):
merge_parameters( Params, Locations, GISInfo ) ->
	% In-order is better:
	lists:reverse( merge_parameters( Params, Locations, _Acc=[], GISInfo ) ).


merge_parameters( _Params=[], _Locations=[], Acc, _GISInfo ) ->
	Acc;

merge_parameters( _Params=[ { Name, InboundCount, OutboundCount } | Tp ],
				  _Locations=[ Loc | Tl ], Acc, GISInfo ) ->

	NewRoadDef = { class_RoadJunction, [ Name, { wgs84_cartesian, Loc },
								InboundCount, OutboundCount, GISInfo ] },

	merge_parameters( Tp, Tl, [ NewRoadDef | Acc ], GISInfo ).



% In meters:
%
% (i.e. the minimal road length)
%
get_min_distance_between_road_junctions_and_others() ->
	15.


% In meters:
get_min_distance_between_two_road_junctions() ->
	25.



% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	text_utils:format( "Road junction '~s' (AAI: ~B) located at ~s (~s), "
		"whose random state is ~w",
		[ ?getAttr(name),
		  class_Actor:get_abstract_identifier( State ),
		  class_GeolocalizedElement:interpret_location( State ),
		  class_PointOfInterest:to_string( State ),
		  random_utils:get_random_state() ] ).
