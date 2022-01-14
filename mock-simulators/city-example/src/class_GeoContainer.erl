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


% @doc Class modelling a <b>geographical container</b>.
-module(class_GeoContainer).


-define( class_description,
		 "Class modelling a geographical container, which is a geolocalized "
		 "element containing potentially other geolocalized elements." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_GeolocalizedElement ] ).


% The class-specific attributes of an instance of a geo-container element are:
-define( class_attributes, [

	{ contained, [ geolocalized_pid() ], "a list of the geolocalized elements "
	  "currently contained by this geocontainer" } ] ).


% Exported helpers:
-export([ request_entry/2 ]).


-type container_pid() :: pid().

-export_type([ container_pid/0 ]).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "City-example.GeoContainer" ).


% For types:
-include("city_example_types.hrl").


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").



% Implementation notes:
%
% Currently all geo-containers have an unbounded capacity.
% As a result, contained elements are simply stored in a list.



% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Creates a geo-container, initially empty.
%
% The only parameter is the starting location, which is either:
%
% - ContainerPid :: container_pid() is the PID of a parent geo-container,
% supposedly able to accept this container
%
% - {CoordinateType :: class_GIS:geolocation_flavour(), Location::
% class_GIS:geolocation_coordinate()}
%
% - Location :: class_GIS:geolocation_coordinate(), where Location is implicitly
% a WGS84 polar coordinate
%
-spec construct( wooper:state(), class_GIS:location() ) -> wooper:state().
construct( State, AnyKindOfLocation ) ->

	LocalizedState = class_GeolocalizedElement:construct( State,
														  AnyKindOfLocation ),

	setAttribute( LocalizedState, contained, [] ).




% Methods section.


% @doc Requests this container to enter.
%
% This default implementation always accepts this incoming geo-element.
%
-spec requestEntry( wooper:state() ) -> request_return( entry_outcome() ).
requestEntry( State ) ->

	GeoRequester = ?getSender(),

	{ EnterState, Outcome } = request_entry( GeoRequester, State ),

	wooper:return_state_result( EnterState, Outcome ).



% @doc Traces current state.
-spec traceContent( wooper:state() ) -> const_oneway_return().
traceContent( State ) ->

	?notice( to_string( State ) ),

	wooper:const_return().



% @doc Returns a string describing the state of this instance.
-spec toString( wooper:state() ) -> const_request_return( ustring() ).
toString( State ) ->
	wooper:const_return_result( to_string( State ) ).




% Helper functions.


% Returns whether specified geolocalized element has been accepted by that
% container.
%
% (helper)
%
-spec request_entry( geolocalized_pid(), wooper:state() ) ->
						{ wooper:state(), entry_outcome() }.
request_entry( GeoRequester, State ) ->

	?info_fmt( "Accepting geolocalized element ~w.", [ GeoRequester ] ),

	{ appendToAttribute( State, contained, GeoRequester ), entered }.



% @doc Returns a textual description of that instance.
%
% (helper)
%
to_string( State ) ->

	ContainedString = case ?getAttr(contained) of

		[] ->
			"not containing any geo-element";

		List ->
			text_utils:format( "containing following ~B geo-elements: ~w",
							   [ length( List ), List ] )

	end,

	text_utils:format( "geocontainer named '~ts' located at ~w ~ts",
				   [ ?getAttr(name), ?getAttr(location), ContainedString ] ).
