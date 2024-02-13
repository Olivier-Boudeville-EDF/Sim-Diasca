% Copyright (C) 2014-2024 Olivier Boudeville
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
% Creation date: Monday, November 17, 2014



% @doc Basic <b>numerical solver based on the classic Runge–Kutta fourth-order
% method</b>, operating on three dimensions.
%
% See [http://en.wikipedia.org/wiki/List_of_Runge–Kutta_methods].
%
% We want to evaluate a given function f, whose spec could be: `f( time(),
%     vector() ) -> vector()' that would comply to the following equation:
%     `dy/dt = f(t,y)'.
%
% For that we compute `yn+1 = yn + h.sum(bi.ki)' with `ki=f(ti,yi)', with ti and
% yi depending on the order, and h being the chosen timestep. ki and bi are
% determined by the corresponding Butcher tableau.
%
% The implementation of `f' corresponds here to the anonymous function `F'.
%
-module(rk4_solver).



-export([ compute_next_estimate3p/4, compute_next_estimate3v/4 ]).


-type time() :: float().
% Simulation time.


% Definition depends on the function of interest, more precisely on the
% dimension of the space the function to evaluate is an endomorphism of.

% Functions may deal with points (then suffixed with 'p'), vectors ('v' suffix)
% or scalars (no suffix).


-type f1() :: fun( ( time(), scalar() ) -> scalar() ).
% The function f in the equation that we want to solve numerically, here in a 1D
% space, based on scalars.


-type f2p() :: fun( ( time(), point2() ) -> point2() ).
% The function f in the equation that we want to solve numerically, here in a 2D
% space, based on points.

-type f2v() :: fun( ( time(), vector2() ) -> vector2() ).
% The function f in the equation that we want to solve numerically, here in a 2D
% space, based on vectors.


-type f3p() :: fun( ( time(), point3() ) -> point3() ).
% The function f in the equation that we want to solve numerically, here in a 3D
% space, based on points.

-type f3v() :: fun( ( time(), vector3() ) -> vector3() ).
% The function f in the equation that we want to solve numerically, here in a 3D
% space, based on vectors.


-export_type([ time/0, f1/0, f2p/0, f2v/0, f3p/0, f3v/0 ]).



% We use here the "original" Runge–Kutta method, i.e. the classic fourth-order
% method.


% Shorthands:

-type scalar() :: linear:scalar().

-type point2() :: point2:point2().
-type point3() :: point3:point3().

-type vector2() :: vector2:vector2().
-type vector3() :: vector3:vector3().


% @doc Computes the next point (yn+1), based on the current one (yn), the
% function (F) and the timestep (h).
%
-spec compute_next_estimate3p( f3p(), point3(), time(), time() ) -> point3().
compute_next_estimate3p( F, Point, Time, Step ) ->

	% See compute_next_estimate3v/4 for comments.

	K1 = vector3:from_point( F( Time, Point ) ),

	HalfStep = Step / 2.0,

	% tn + h/2:
	OneHalfStepAfter = Time + HalfStep,

	% yn + h/2.k1:
	SecondPoint = point3:translate( Point, vector3:scale( K1, HalfStep ) ),

	K2 = vector3:from_point( F( OneHalfStepAfter, SecondPoint ) ),

	% yn + h/2.k2:
	ThirdPoint = point3:translate( Point, vector3:scale( K2, HalfStep ) ),

	K3 = vector3:from_point( F( OneHalfStepAfter, ThirdPoint ) ),

	% yn + h.k3:
	FourthPoint = point3:translate( Point, vector3:scale( K3, Step ) ),

	OneFullStepAfter = Time + Step,
	K4 = vector3:from_point( F( OneFullStepAfter, FourthPoint ) ),

	MidStep = Step / 6.0,
	FullStep = Step / 3.0,

	SK1 = vector3:scale( K1, MidStep ),
	SK2 = vector3:scale( K2, FullStep ),
	SK3 = vector3:scale( K3, FullStep ),
	SK4 = vector3:scale( K4, MidStep ),

	% yn+1 = yn + h.( 1/6.k1 + 1/3.k2 + 1/3.k3 + 1/6.k4 )
	ResVec = vector3:add( [ SK1, SK2, SK3, SK4 ] ),
	point3:translate( Point, ResVec ).



% @doc Computes the next vector (yn+1), based on the current one (yn), the
% function (F) and the timestep (h).
%
-spec compute_next_estimate3v( f3v(), vector3(), time(), time() ) -> vector3().
compute_next_estimate3v( F, Vector, Time, Step ) ->

	%trace_utils:debug_fmt( "~w computing at ~p from point ~p.",
	%                       [ self(), Time, Point ] ),

	% Ad-hoc implementation of a Butcher tableau, for RK4 (s=4):

	% yn+1 = yn + h.sum(i=1 to s, bi.ki)
	% with ki = f( tn + ci.h, yn + h.sum(j=1 to s, aij.kj) )

	% Here: yn+1 = yn + h.( 1/6.k1 + 1/3.k2 + 1/3.k3 + 1/6.k4 )

	% With:
	%  k1 = f( tn,       yn          )
	%  k2 = f( tn + h/2, yn + h/2.k1 )
	%  k3 = f( tn + h/2, yn + h/2.k2 )
	%  k4 = f( tn + h,   yn + h.  k3 )

	K1 = F( Time, Vector ),

	HalfStep = Step / 2.0,

	% tn + h/2:
	OneHalfStepAfter = Time + HalfStep,

	% yn + h/2.k1:
	SecondPoint = vector3:add( Vector, vector3:scale( K1, HalfStep ) ),

	K2 = F( OneHalfStepAfter, SecondPoint ),

	% yn + h/2.k2:
	ThirdPoint = vector3:add( Vector, vector3:scale( K2, HalfStep ) ),

	K3 = F( OneHalfStepAfter, ThirdPoint ),

	% yn + h.k3:
	FourthPoint = vector3:add( Vector, vector3:scale( K3, Step ) ),

	OneFullStepAfter = Time + Step,
	K4 = F( OneFullStepAfter, FourthPoint ),

	MidStep = Step / 6.0,
	FullStep = Step / 3.0,

	SK1 = vector3:scale( K1, MidStep ),
	SK2 = vector3:scale( K2, FullStep ),
	SK3 = vector3:scale( K3, FullStep ),
	SK4 = vector3:scale( K4, MidStep ),

	% yn+1 = yn + h.( 1/6.k1 + 1/3.k2 + 1/3.k3 + 1/6.k4 )
	vector3:add( [ Vector, SK1, SK2, SK3, SK4 ] ).
