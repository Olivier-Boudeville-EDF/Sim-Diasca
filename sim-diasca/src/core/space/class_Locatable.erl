% Copyright (C) 2008-2021 EDF R&D

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


% Describes a location in the 3D context:
-type location() :: linear_3D:point().



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").



% Implementation notes:
%
% Used to be a trace emitter, but it is not strictly needed.



% Constructs a new locatable instance, based on a record of an in-world
% location.
%
-spec construct( wooper:state(), location() ) -> wooper:state().
construct( State, Location ) ->

	% First the direct mother classes, then this class-specific actions:
	%TraceState = class_TraceEmitter:construct( State, "Locatable" ),

	%?send_notice_fmt( TraceState,
	%	"Creating a new locatable whose location is ~s.",
	%	[ space:location_to_string( Location ) ] ),

	setAttribute( State, location, Location ).




% Methods section.


% Returns the in-world location of this locatable.
-spec getLocation( wooper:state() ) -> const_request_return( location() ).
getLocation( State ) ->
	wooper:const_return_result( ?getAttr(location) ).



% Sets the in-world location of this locatable.
-spec setLocation( wooper:state(), location() ) -> oneway_return().
setLocation( State, NewLocation ) ->
	wooper:return_state( setAttribute( State, location, NewLocation ) ).



% Returns the in-world abscissa of this locatable.
-spec getAbscissa( wooper:state() ) ->
						 const_request_return( linear:coordinate() ).
getAbscissa( State ) ->

	{ X, _Y, _Z } = ?getAttr(location),

	wooper:const_return_result( X ).



% Sets the in-world abscissa of this locatable.
-spec setAbscissa( wooper:state(), linear:coordinate() ) -> oneway_return().
setAbscissa( State, NewX ) ->

	{ _X, Y, Z } = ?getAttr(location),

	wooper:return_state(
	   setAttribute( State, location, { NewX, Y, Z } ) ).



% Returns the in-world ordinate of this locatable.
-spec getOrdinate( wooper:state() ) ->
						 const_request_return( linear:coordinate() ).
getOrdinate( State ) ->

	{ _X, Y, _Z } = ?getAttr(location),

	wooper:const_return_result( Y ).



% Sets the in-world ordinate of this locatable.
-spec setOrdinate( wooper:state(), linear:coordinate() ) -> oneway_return().
setOrdinate( State, NewY ) ->

	{ X, _Y, Z } = ?getAttr(location),

	wooper:return_state( setAttribute( State, location, { X, NewY, Z } ) ).



% Returns the in-world altitude of this locatable.
-spec getAltitude( wooper:state() ) ->
						 const_request_return( linear:coordinate() ).
getAltitude( State ) ->

	{ _X, _Y, Z } = ?getAttr(location),

	wooper:const_return_result( Z ).



% Sets the in-world altitude of this locatable.
-spec setAltitude( wooper:state(), linear:coordinate() ) -> oneway_return().
setAltitude( State, NewZ ) ->

	{ X, Y, _Z } = ?getAttr(location),

	wooper:return_state( setAttribute( State, location, { X, Y, NewZ } ) ).




% Section for helper functions (not methods).


% Returns the location of this Locatable.
%
% Note: is never and cannot be overloaded.
%
-spec describe_location( wooper:state() ) -> string().
describe_location( State ) ->
	text_utils:format( "~w", [ ?getAttr(location) ] ).
