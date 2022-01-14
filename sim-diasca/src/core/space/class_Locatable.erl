% Copyright (C) 2008-2022 EDF R&D

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


% @doc Locatable class, base of all instances having <b>in-world 3D
% coordinates</b>.
%
-module(class_Locatable).


-define( class_description, "Locatable class, base of all instances having "
							"in-world 3D coordinates." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).


% The class-specific attributes:
-define( class_attributes, [
			{ location, location(), "current location of this instance" } ] ).


% Helper functions.
-export([ describe_location/1 ]).


-type location() :: point3:point3().
% Describes a location in the 3D environment.



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").



% Implementation notes:
%
% Used to be a trace emitter, but it is not strictly needed.



% Shorthands:

-type ustring() :: text_utils:ustring().

-type coordinate() :: linear:coordinate().



% @doc Constructs a locatable instance, based on a record of an in-world
% location.
%
-spec construct( wooper:state(), location() ) -> wooper:state().
construct( State, Location ) ->

	% First the direct mother classes, then this class-specific actions:
	%TraceState = class_TraceEmitter:construct( State, "Locatable" ),

	%?send_notice_fmt( TraceState,
	%   "Creating a locatable whose location is ~ts.",
	%   [ space:location_to_string( Location ) ] ),

	setAttribute( State, location, Location ).




% Methods section.


% @doc Returns the in-world location of this locatable.
-spec getLocation( wooper:state() ) -> const_request_return( location() ).
getLocation( State ) ->
	wooper:const_return_result( ?getAttr(location) ).



% @doc Sets the in-world location of this locatable.
-spec setLocation( wooper:state(), location() ) -> oneway_return().
setLocation( State, NewLocation ) ->
	wooper:return_state( setAttribute( State, location, NewLocation ) ).



% @doc Returns the in-world abscissa of this locatable.
-spec getAbscissa( wooper:state() ) -> const_request_return( coordinate() ).
getAbscissa( State ) ->

	{ X, _Y, _Z } = ?getAttr(location),

	wooper:const_return_result( X ).



% @doc Sets the in-world abscissa of this locatable.
-spec setAbscissa( wooper:state(), coordinate() ) -> oneway_return().
setAbscissa( State, NewX ) ->

	{ _X, Y, Z } = ?getAttr(location),

	wooper:return_state(
	   setAttribute( State, location, { NewX, Y, Z } ) ).



% @doc Returns the in-world ordinate of this locatable.
-spec getOrdinate( wooper:state() ) ->
								const_request_return( coordinate() ).
getOrdinate( State ) ->

	{ _X, Y, _Z } = ?getAttr(location),

	wooper:const_return_result( Y ).



% @doc Sets the in-world ordinate of this locatable.
-spec setOrdinate( wooper:state(), coordinate() ) -> oneway_return().
setOrdinate( State, NewY ) ->

	{ X, _Y, Z } = ?getAttr(location),

	wooper:return_state( setAttribute( State, location, { X, NewY, Z } ) ).



% @doc Returns the in-world altitude of this locatable.
-spec getAltitude( wooper:state() ) ->
									const_request_return( coordinate() ).
getAltitude( State ) ->

	{ _X, _Y, Z } = ?getAttr(location),

	wooper:const_return_result( Z ).



% @doc Sets the in-world altitude of this locatable.
-spec setAltitude( wooper:state(), coordinate() ) -> oneway_return().
setAltitude( State, NewZ ) ->

	{ X, Y, _Z } = ?getAttr(location),

	wooper:return_state( setAttribute( State, location, { X, Y, NewZ } ) ).




% Section for helper functions (not methods).


% @doc Returns the location of this Locatable.
%
% Note: is never and cannot be overloaded.
%
-spec describe_location( wooper:state() ) -> ustring().
describe_location( State ) ->
	text_utils:format( "~w", [ ?getAttr(location) ] ).
