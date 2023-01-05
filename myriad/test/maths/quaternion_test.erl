% Copyright (C) 2021-2023 Olivier Boudeville
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
% Creation date: Friday, October 8, 2021.


% @doc Unit tests for the <b>quaternion</b> facilities.
%
% See the quaternion tested module.
%
-module(quaternion_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	NullQ = quaternion:null(),

	% Octave: Q = quaternion(2, 3, 4, 1)
	% The 'quaternion' must have been installed beforehand, for example with:
	% pkg install -forge quaternion
	%
	% (beware that the real part is to be specified last)
	Q = quaternion:new( 1, 2, 3, 4 ),

	test_facilities:display( "Base textual representation for Q = ~w: ~ts",
							 [ Q, quaternion:to_string( Q ) ] ),

	test_facilities:display( "Base textual representation for NullQ = ~w: ~ts",
							 [ NullQ, quaternion:to_string( NullQ ) ] ),

	test_facilities:display( "Compact textual representation for Q = ~w: ~ts",
							 [ Q, quaternion:to_compact_string( Q ) ] ),

	test_facilities:display( "User-friendly textual representation "
		"for Q = ~w: ~ts", [ Q, quaternion:to_user_string( Q ) ] ),

	% Octave: Q1 = quaternion(7, -3, 4.0, 0)
	Q1 = quaternion:new( 0, 7, -3, 4.0 ),

	AddQ = quaternion:add( Q, Q1 ),
	AddQ = { 1.0, 9.0, 0.0, 8.0 },

	Axis = [ 12.0, 1.0, 2.0 ],
	UnitAxis = vector3:normalise( Axis ),

	RadAngle = math:pi() / 7,

	DirectRot3 = matrix3:rotation( UnitAxis, RadAngle ),

	UnitQ = quaternion:rotation( UnitAxis, RadAngle ),

	Rot3FromQ = quaternion:to_rot_matrix3( UnitQ ),

	true = matrix3:are_equal( DirectRot3, Rot3FromQ ),

	test_facilities:stop().
