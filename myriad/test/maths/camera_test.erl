% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Wednesday, October 9, 2024.

-module(camera_test).

-moduledoc """
Unit tests for the **camera** facilities.

See the `camera` tested module.
""".


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the camera support." ),

	Position = { 0.0, 0.0, 0.0 }, % Origin-centered
	TargetPoint = { 0.0, 0.0, -17.0 }, % Still towards -Z
	UpVec = [ 0.0, 1.0, 0.0 ],

	Cam1 = camera:create( Position, TargetPoint, UpVec ),

	ViewMat4 = camera:get_view_matrix( Cam1 ),

	test_facilities:display( "For a camera located at ~ts, targeted at ~ts "
		"and whose up direction is ~ts, the view matrix is: ~ts",
		[ point3:to_string( Position ), point3:to_string( TargetPoint ),
		  vector3:to_string( UpVec ), matrix4:to_string( ViewMat4 ) ] ),

	test_facilities:stop().
