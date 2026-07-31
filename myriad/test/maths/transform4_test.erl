% Copyright (C) 2024-2026 Olivier Boudeville
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
% Creation date: Monday, February 5, 2024.

-module(transform4_test).

-moduledoc """
Unit tests for the **3D transformations based on 4x4 matrices**.

See the transform4 tested module.
""".



-include_lib("myriad/include/matrix4.hrl").


% For run/0 export and al:
-include("test_facilities.hrl").


% Comparisons are made of this specialised vector4 implementation with the one
% for arbitrary vectors (see vector.erl).
%
-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    TId = transform4:identity(),

    transform4:check( TId ),

    CM = #compact_matrix4{ m11=1, m12=8, m13=3, tx=1,
                           m21=4, m22=5, m23=6, ty=2,
                           m31=7, m32=8, m33=9, tz=3  },

    T1 = transform4:new( CM ),

    % Useless as done by most operations: transform4:check( T1 ),

    CM = transform4:get_reference( T1 ),

    true = matrix4:are_equal( matrix4:identity(),
        matrix4:mult( CM, transform4:get_inverse( T1 ) ) ),

    test_facilities:display( "Base textual representation for T1=~w~n; "
        "user-friendly one: ~ts~n", [ T1, transform4:to_string( T1 ) ] ),


    VT = [ 1, 2, 3 ],

    Tt = transform4:translation( VT ),

    test_facilities:display( "Translation transform of vector ~ts: ~ts~n",
        [ vector3:to_string( VT ), transform4:to_string( Tt ) ] ),

    UnitVT = vector3:normalise( VT ),

    RadAngle = math:pi() / 7,

    Tr = transform4:rotation( UnitVT, RadAngle ),

    test_facilities:display( "Rotation transform of unit axis ~ts "
        "and angle ~w radians: ~ts~n",
        [ vector3:to_string( UnitVT ), RadAngle, transform4:to_string( Tr ) ] ),


    Factors = { _Sx=1.2, _Sy=0.9, _Sz=0.8 },

    Ts = transform4:scaling( Factors ),

    test_facilities:display( "Scaling transform of factors ~w: ~ts~n",
        [ Factors, transform4:to_string( Ts ) ] ),


    O = { 4, -4, 2 },

    X = vector3:normalise( VT ),

    % We want Y to be orthogonal to X so that we have an orthogonal basis:
    Y = vector3:normalise( vector3:get_orthogonal( X ) ),

    % Hence normalised by design (because X and Y are normalised, *and* are
    % orthogonal):
    %
    Z = vector3:cross_product( X, Y ),

    Ttr = transform4:transition( _Origin=O, X, Y, Z ),

    test_facilities:display( "Transition transform to the coordinate system of "
        "origin ~ts with axes: ~ts, ~ts and ~ts is: ~ts~n",
        [ point3:to_string( O ), vector3:to_string( X ), vector3:to_string( Y ),
          vector3:to_string( Z ), transform4:to_string( Ttr ) ] ),


    VTOther = [ 7, -11, 1 ],

    TBase = Ttr,
    test_facilities:display( "Using from now base transform ~ts~n",
                             [ transform4:to_string( TBase ) ] ),

    TlTBase = transform4:translate_left( VTOther, TBase ),

    test_facilities:display( "Base transform when left-translated of ~ts: "
        "~ts~n", [ vector3:to_string( VTOther ),
                   transform4:to_string( TlTBase ) ] ),


    TrTBase = transform4:translate_right( TBase, VTOther ),

    test_facilities:display( "Base transform when right-translated of ~ts: "
        "~ts~n",
        [ vector3:to_string( VTOther ), transform4:to_string( TrTBase ) ] ),


    UnitVTOther = vector3:normalise( VTOther ),

    RadAngleOther = math:pi() / 11,

    RlTBase = transform4:rotate_left( UnitVTOther, RadAngleOther, TBase ),

    test_facilities:display( "Base transform when left-rotated "
        "around axis ~ts of an angle of ~w radians: "
        "~ts~n", [ vector3:to_string( UnitVTOther ), RadAngleOther,
                   transform4:to_string( RlTBase ) ] ),

    RrTBase = transform4:rotate_right( TBase, UnitVTOther, RadAngleOther ),

    test_facilities:display( "Base transform when right-rotated "
        "around axis ~ts of an angle of ~w radians: "
        "~ts~n", [ vector3:to_string( UnitVTOther ), RadAngleOther,
                   transform4:to_string( RrTBase ) ] ),


    FactorsOther = { 1.8, 30.0, 12.5 },

    SlTBase = transform4:scale_left( FactorsOther, TBase ),

    test_facilities:display( "Base transform when left-scaled "
        "of factors ~w: ~ts~n",
        [ FactorsOther, transform4:to_string( SlTBase ) ] ),


    SrTBase = transform4:scale_right( TBase, FactorsOther ),

    test_facilities:display( "Base transform when right-scaled "
        "of factors ~w: ~ts~n",
        [ FactorsOther, transform4:to_string( SrTBase ) ] ),


    test_facilities:stop().
