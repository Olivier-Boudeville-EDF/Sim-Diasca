% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Monday, February 15, 2010.


% Gathering of various facilities for linear operations.
-module(linear).


% Relatively aggressive inlining for basic operations:
-compile( inline ).
-compile( { inline_size, 48 } ).


% For printout_*:
-include("linear.hrl").



% These type names are too general to be defined in the hrl file (i.e. in the
% root namespace).

% By default (unless specified), most values (ex: coordinates, distances) are
% floating-point ones.
%
% Integer counterparts are usually defined (integer_*), as well as values that
% may be either integer or floating-point ones (any_*).
%
% As a result, even if many operations could have remained polymorphic if
% relying on the any_* types, here we chose to emphasize on floating-point
% numbers.


% Cartesian (floating-point) coordinates in a referential:
-type coordinate() :: float().


% Cartesian integer coordinates in a referential:
-type integer_coordinate() :: integer().


% Cartesian coordinates in a referential:
-type any_coordinate() :: number().



% Usually multipliers involved in equations:
-type factor() :: float().


% Usually multipliers involved in equations:
-type integer_factor() :: integer().


% Usually multipliers involved in equations:
-type any_factor() :: number().



% Distance (as floating-point) between two points (ex: to express lengths):
-type distance() :: float().


% Integer distance between two points (ex: to express lengths):
-type integer_distance() :: integer().


% Distance between two points (ex: to express lengths):
-type any_distance() :: number().



% Mostly for clarity:

% Radius (as floating-point):
-type radius() :: distance().


% Radius (as integer):
-type integer_radius() :: integer_distance().

% Radius:
-type any_radius() :: any_distance().



% Square of a distance between two points, as a floating-point value (cheaper to
% compute, when applying the square root operator is not needed, like when
% comparing distances):
%
-type square_distance() :: float().


% Square of a distance between two points, as an integer value (cheaper to
% compute, when applying the square root operator is not needed, like when
% comparing distances):
%
-type integer_square_distance() :: integer().


% Square of a distance between two points (cheaper to compute, when applying the
% square root operator is not needed, like when comparing distances):
%
-type any_square_distance() :: number().


% Area of a surface:
-type area() :: float().


-export_type([ coordinate/0, integer_coordinate/0, any_coordinate/0,
			   factor/0, integer_factor/0, any_factor/0,
			   distance/0, integer_distance/0, any_distance/0,
			   radius/0, integer_radius/0, any_radius/0,
			   square_distance/0, integer_square_distance/0,
			   any_square_distance/0,
			   area/0 ]).


-export([ coord_to_string/1 ] ).


% Shorthands:
-type ustring() :: text_utils:ustring().


% Returns a textual representation of a coordinate.
-spec coord_to_string( any_coordinate() ) -> ustring().
coord_to_string( Coord ) when is_float( Coord ) ->

	% For testing:
	%text_utils:format( "XX~*.*.*fXX~n", [ 14, 12, $a, 1/3 ] ).

	% Hopefully the format string is resolved at compile-time:
	%text_utils:format( "~" ++ ?printout_width ++ "." ++ ?printout_precision
	%				   ++ ". p", [ Coord ] ).
	text_utils:format( "~" ++ ?printout_width ++ "." ++ ?printout_precision
					   ++ ". f", [ Coord ] );

coord_to_string( Coord ) when is_integer( Coord ) ->
	text_utils:format( "~" ++ ?printout_width ++ "." ++ ?printout_precision
					   ++ ". B", [ Coord ] ).
