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


% @doc Class in charge of the <b>generation of adequate locations</b>.
-module(class_LocationGenerator).


-define( class_description,
		 "Class in charge of the generation of adequate locations."
		 "This allows to generate locations:"
		 " - simply within the world bounds"
		 " - and/or sufficiently far from any set of locations, including "
		 "all other generated ones" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).



% The class-specific attributes of an instance of Location Generator are:
-define( class_attributes, [

	{ world_dimensions, point3(),
	  "allows to record the bounds of the (3D) world of interest, "
	  "from the origin to the specified point" },

	{ past_locations, [ raw_location() ],
	  "the set of all past generated locations" } ] ).


% For all shared defines and types:
-include("city_example_types.hrl").



% Design notes:
%
% - the world could be partitioned a lot more efficiently (octree, BSP, etc.)
%
% - location generators are likely to be bottlenecks when initializing a
% simulation: the location of the potentially many initial actors is determined
% sequentially




% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.LocationGenerator" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type point3() :: point3:point3().

-type raw_location() :: class_GIS:raw_location().
-type length() :: class_GIS:length().



% @doc Constructs a location generator, from following parameters:
%
% - WorldDimensions={XLen, YLen, ZLen} describes the extent of the world (which
% is a right_cuboid bounding box), from the origin and alongside the three
% canonical axes, specified thanks to the opposite point to the origin
%
-spec construct( wooper:state(), class_TraceEmitter:emitter_init(),
				 point3() ) -> wooper:state().
construct( State, Name, WorldDimensionsP ) ->

	% Otherwise a non-constant seed will be assigned, and location
	% reproducibility is lost:
	%
	random_utils:start_random_source( default_seed ),

	EmitterState = class_EngineBaseObject:construct( State,
													 ?trace_categorize(Name) ),

	% Bound to be a massive bottleneck:

	?send_info_fmt( EmitterState,
		"Creating location generator with ~ts as world bounds.",
		[ point3:to_string( WorldDimensionsP ) ] ),

	setAttributes( EmitterState, [ { world_dimensions, WorldDimensionsP },
								   { past_locations, [] } ] ).



% Section for member methods.


% @doc Generates randomly (with an uniform law) a new location within the world
% bounds.
%
% There is possibly already something exactly at the same place.
%
-spec generateLocation( wooper:state() ) ->
								request_return( raw_location() ).
generateLocation( State ) ->

	Loc = draw_location( ?getAttr(world_dimensions) ),

	wooper:return_state_result( appendToAttribute( State, past_locations, Loc ),
								Loc ).



% @doc Generates randomly a new location within the world bounds, ensuring it is
% not in the specified radius from any of the past generated locations.
%
-spec generateNonAdjacentLocation( wooper:state(), length() ) ->
										request_return( raw_location() ).
generateNonAdjacentLocation( State, Radius ) ->

	Loc = generate_non_adjacent_from( ?getAttr(past_locations), Radius,
									  ?getAttr(world_dimensions) ),

	wooper:return_state_result( appendToAttribute( State, past_locations, Loc ),
								Loc ).



% @doc Generates randomly a new location within the world bounds, ensuring it is
% not in specified radius of any of the specified locations (regardless of the
% past determined ones - but recording these newly returned locations).
%
-spec generateNonAdjacentLocation( wooper:state(), [ raw_location() ],
							length() ) -> request_return( raw_location() ).
generateNonAdjacentLocation( State, Locations, Radius ) ->

	Loc = generate_non_adjacent_from( Locations, Radius,
									  ?getAttr(world_dimensions) ),

	wooper:return_state_result( appendToAttribute( State, past_locations, Loc ),
								Loc ).



% @doc Generates randomly the specified number of new locations within the world
% bounds, ensuring they are not in the specified radius GeneralRadius from any
% of the already-generated locations nor in the specified radius PeerRadius from
% the other locations generated by this call.
%
-spec generateNonAdjacentLocations( wooper:state(), count(), length(),
					length() ) -> request_return( [ raw_location() ] ).
generateNonAdjacentLocations( State, LocationCount, GeneralRadius,
							  PeerRadius ) ->

	PastLocations = ?getAttr(past_locations),

	NewLocations = generate_non_adjacent_from_both( LocationCount,
		GeneralRadius, PeerRadius, ?getAttr(world_dimensions), PastLocations ),

	NewState = setAttribute( State, past_locations,
							 NewLocations ++ PastLocations ),

	wooper:return_state_result( NewState, NewLocations ).



% @doc Returns a string describing the state of this instance.
-spec toString( wooper:state() ) -> const_request_return( ustring() ).
toString( State ) ->

	FinalString = text_utils:format( "Location generator having ~ts "
		"for world bounds, and having generated ~B locations yet",
		[ ?getAttr(world_dimensions), length( ?getAttr(past_locations) ) ] ),

	wooper:const_return_result( FinalString ).




% Helper section.



% @doc Returns a uniformly random location within the specified world bounds,
% each coordinate being in [0, MaxLen-1].
%
draw_location( _WorldDimensionsP={ XLen, YLen, ZLen } ) ->

	% These are floating-point values, however they correspond to integers:
	{ class_RandomManager:get_uniform_floating_point_value( XLen - 1 ),
	  class_RandomManager:get_uniform_floating_point_value( YLen - 1 ),
	  class_RandomManager:get_uniform_floating_point_value( ZLen - 1 ) }.



% @doc Generates at random a location that is not within Radius of specified
% locations.
%
generate_non_adjacent_from( Locations, Radius, WorldDimensions ) ->

	SquareRadius = math_utils:squarify( Radius ),

	% Up to Count generation attempts will be made:
	AttemptCount=1000,

	generate_non_adjacent_from( Locations, SquareRadius, AttemptCount,
								WorldDimensions ).


% (helper)
generate_non_adjacent_from( Locations, SquareRadius, _AttemptCount=0,
							_WorldDimensions ) ->

	% All attempts exhausted:
	throw( { location_generation_failed, SquareRadius,
			 length( Locations ) } );


generate_non_adjacent_from( Locations, SquareRadius, AttemptCount,
							WorldDimensions ) ->

	Loc = draw_location( WorldDimensions ),

	case is_location_close( Loc, Locations, SquareRadius ) of

		true ->
			generate_non_adjacent_from( Locations, SquareRadius,
										AttemptCount-1, WorldDimensions );

		false ->
			Loc

	end.



% @doc Generates at random the specified number of locations, each further than
% GeneralRadius of any previous locations, and further than PeerRadius of the
% other locations in the returned list.
%
generate_non_adjacent_from_both( LocationCount, GeneralRadius, PeerRadius,
								 WorldDimensions, PastLocations ) ->

   %trace_utils:debug_fmt( "Generating ~B locations in world ~p, "
   %   "each separated from the others by at least ~w meters, "
   %   "each separated from past locations ~w of at least ~w meters.",
   %   [ LocationCount, WorldDimensions, GeneralRadius, PastLocations,
   %     GeneralRadius ] ),

	GeneralSquareRadius = math_utils:squarify( GeneralRadius ),

	PeerSquareRadius = math_utils:squarify( PeerRadius ),

	generate_non_adjacent_from_both( LocationCount, GeneralSquareRadius,
		PeerSquareRadius, WorldDimensions, PastLocations, _AccGenerated=[],
		_AttemptCount=5000 ).



% (helper)
generate_non_adjacent_from_both( _LocationCount=0, _GeneralSquareRadius,
		 _PeerSquareRadius, _WorldDimensions, _PastLocations, AccGenerated,
		 _AttemptCount ) ->

	%trace_utils:debug_fmt( "Generated locations: ~p.", [ AccGenerated ] ),
	AccGenerated;


generate_non_adjacent_from_both( _LocationCount, GeneralSquareRadius,
		PeerSquareRadius, _WorldDimensions, PastLocations, AccGenerated,
		_AttemptCount=0 ) ->
	throw( { location_generation_failed, GeneralSquareRadius,
			 PeerSquareRadius, length( PastLocations ),
			 length( AccGenerated ) } );


generate_non_adjacent_from_both( LocationCount, GeneralSquareRadius,
		PeerSquareRadius, WorldDimensions, PastLocations, AccGenerated,
		AttemptCount ) ->

	% We maybe could start with the finest radius.

	% First, let's generate a point far enough from the past locations:
	CandidateLoc = generate_non_adjacent_from( PastLocations,
						GeneralSquareRadius, WorldDimensions ),

	case is_location_close( CandidateLoc, AccGenerated, PeerSquareRadius ) of

		true ->
			% Too close from counterparts; we need to generate a new candidate:
			% (executing exactly the same call again - except count)
			%
			generate_non_adjacent_from_both( LocationCount, GeneralSquareRadius,
				PeerSquareRadius, WorldDimensions, PastLocations, AccGenerated,
				AttemptCount-1 );

		false ->
			% Perfect, let's acknowledge this candidate and continue:
			generate_non_adjacent_from_both( LocationCount-1,
				GeneralSquareRadius,PeerSquareRadius, WorldDimensions,
				PastLocations, [ CandidateLoc | AccGenerated ], AttemptCount )

	end.



% @doc Tells whether the specified location is close (based on the specified
% square radius) of the specified locations.
%
is_location_close( _Location, _Locations=[], _SquareRadius ) ->
	false;

is_location_close( Location, _Locations=[ H | T ], SquareRadius ) ->

	case point3:is_within_square( Location, H, SquareRadius ) of

		true ->
			% Collision!
			true;

		false ->
			is_location_close( Location, T, SquareRadius )

	end.
