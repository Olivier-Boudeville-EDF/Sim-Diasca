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
% Creation date: Monday, October 4, 2021.


% @doc Unit tests for the <b>points of arbitrary dimensions</b>.
%
% See the point tested module.
%
-module(point_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	Null2D = point:null( _Dim=2 ),

	Null2D = point:new( { 0.0, 0.0 } ),
	Null2D = point:new( [ 0.0, 0.0 ] ),

	Vec1 = [ 0.0, 1.0, 2.0 ],

	P1 = list_to_tuple( Vec1 ),

	P1 = point:from_vector( Vec1 ),

	Vec1 = point:to_vector( P1 ),

	P2 = point:new( { 1/3, 2.0, 3330.0 } ),
	3 = point:dimension( P2 ),

	P3 = point:new( { 0, 222, 456789 } ),

	test_facilities:display( "Base textual representation for ~w:~n~ts",
							 [ P2, point:to_string( P2 ) ] ),

	test_facilities:display( "Compact textual representation for ~w: ~ts",
							 [ P2, point:to_compact_string( P2 ) ] ),

	test_facilities:display( "Basic representation for ~w: ~ts",
							 [ P2, point:to_basic_string( P2 ) ] ),

	test_facilities:display( "User-friendly representation for ~w: ~ts",
							 [ P2, point:to_user_string( P2 ) ] ),

	test_facilities:display( "User-friendly representation for ~w: ~ts",
							 [ P3, point:to_user_string( P3 ) ] ),

	test_facilities:stop().
