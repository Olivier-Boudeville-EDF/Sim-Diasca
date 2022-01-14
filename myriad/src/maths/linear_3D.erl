% Copyright (C) 2003-2022 Olivier Boudeville
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


% @doc Gathering of various <b>three dimensional "linear"</b> facilities.
%
% See `linear_3D_test.erl' for the corresponding test.
%
-module(linear_3D).



% Design notes.

% In terms of 3D orientation, conventions may differ:
%
% - Myriad, like Blender for example, considers that the "up" direction is the
% +Z axis direction (see https://mMyriad.esperide.org/#geometric-conventions for
% more details)
%
% - other tools, like glTF, define +Y as "up" (+Z as "forward", and -X as
% "right")
%
% So a point P located, based on Myriad conventions, at coordinates P1={A,B,C}
% shall be known of members of the Y-UP club as being at P2={A,C,-B}.
%
% Said otherwise, P2 = Pmg.P1 where the Pmg is the transition matrix from a
% MyriadGUI referential to a glTF one, with:
%
% Pmg = [ 1   0  0 ]
%       [ 0   0  1 ]
%       [ 0  -1  0 ]
%
% The reverse transition is the transpose, i.e.:
% Pgm = Pmg^-1 = tPmg = [ 1   0   0 ]
%                       [ 0   0  -1 ]
%                       [ 0   1   0 ]
%
% i.e. a point at Y-UP coordinates P1={A,B,C} shall be, internally, a point at
% P2={A,-C,B}.
%
% See the point3:yup_point3/0 and vector3:yup_vector3/0 types, and
% {point3,vector3}_to_yup/1 and yup_to_{point3,vector3}/1.


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% Shorthands:

-type factor() :: math_utils:factor().

-type point3() :: point3:point3().

-type normal3() :: vector3:normal3().
-type unit_normal3() :: vector3:unit_normal3().



% Section about lines and planes.


-type line3() :: { A :: factor(), B :: factor(), C :: factor(), D :: factor() }.
% A 3D line, whose equation A.x + B.y + C.z + D = 0, can be defined by its four
% factors {A,B,C,D}.


-type plane3() :: { normal3(), factor() }.
% A plane, whose general equation is: A.x + B.y + C.z + D = 0, where:
%
% - P={x,y,z} is a point belonging to this plane
% - N=[A,B,C] is a (non-necessarily unit) normal vector to this plane
% - D= -A.x0 - B.y0 - C.z0
%
% See [http://mathworld.wolfram.com/Plane.html].
%
% So a plane may be described as (N,D).


-type hessian_plane3() :: { unit_normal3(), factor() }.
% A plane in Hessian normal form.
%
% See [http://mathworld.wolfram.com/HessianNormalForm.html].


-export_type([ line3/0, plane3/0, hessian_plane3/0 ]).



% Section about shapes.


-type shape() :: 'sphere' | 'right_cuboid'.
% Various types of known 3D shapes (basic geometries).


-export_type([ shape/0 ]).


-export([ get_origin/0 ]).


% @doc Returns the origin of this referential.
-spec get_origin() -> point3().
get_origin() ->
	Zero = 0.0,
	{ Zero, Zero, Zero }.
