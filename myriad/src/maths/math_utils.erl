% Copyright (C) 2010-2021 Olivier Boudeville
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


% Gathering of various general purpose basic math facilities.
%
% See math_utils_test.erl for the corresponding test.
%
-module(math_utils).


% General operations:
-export([ floor/1, ceiling/1, round_after/2,
		  float_to_integer/1, float_to_integer/2,
		  modulo/2, clamp/3, squarify/1 ]).

-compile({ inline, [ floor/1, ceiling/1, round_after/2,
					 float_to_integer/1, float_to_integer/2,
					 modulo/2, clamp/3, squarify/1 ] }).


% Operations on floating-point values (in Erlang, a float is a C double):
-export([ are_close/2, are_close/3,
		  are_relatively_close/2, are_relatively_close/3,
		  get_relative_difference/2, is_null/1 ]).

-compile({ inline, [ are_close/2, are_close/3,
					 are_relatively_close/2, are_relatively_close/3,
					 get_relative_difference/2, is_null/1 ] }).


% Operations with angles:
-export([ radian_to_degree/1, canonify/1 ]).

-compile({ inline, [ radian_to_degree/1, canonify/1 ] }).


% For epsilon define:
-include("math_utils.hrl").



% Type declarations:

-type non_zero_integer() :: pos_integer() | neg_integer().


% Standard deviation, as used to describe a Gaussian curve:
-type standard_deviation() :: float().

% Variance, the square of a standard deviation:
-type variance() :: float().


% For percentages (1.0 corresponding to 100%):
%
% See also: text_utils:percent_to_string/{1,2}.
%
-type percent() :: float().


% For integer percentages (in [0..100] generally):
-type integer_percent() :: integer().


% For probabilities:
-type probability() :: float().


% Describes a desired conversion, which is either exact or approximate, based on
% an absolute or relative comparison, with a default epsilon threshold or a
% user-defined one.
%
-type conversion_type() :: 'exact' | 'absolute' | { 'absolute', float() }
						 | 'relative' | { 'relative', float() }.


-export_type([ non_zero_integer/0, standard_deviation/0, variance/0,
			   percent/0, integer_percent/0, probability/0 ]).





% General section.


%
% Floor returns the biggest integer smaller than the specified floating-point
% value.
%
% Inspired from http://schemecookbook.org/Erlang/NumberRounding.
%
% Note: used to be deprecated in favor of math:floor/1, yet we prefer the
% version here, which returns an integer rather than a float.
%
-spec floor( number() ) -> integer().
floor( X ) ->

	% As math:floor/1 returns a float:
	erlang:trunc( math:floor( X ) ).

% Also possible:
%
%	T = erlang:trunc( X ),
%
%	case X - T of
%
%		Neg when Neg < 0 ->
%			T - 1;
%
%		%Pos when Pos > 0 ->
%		%   T;
%
%		_PositiveOrNull ->
%           T
%
%	end.



% Ceiling returns the smallest integer bigger than the specified floating-point
% value.
%
% Inspired from http://schemecookbook.org/Erlang/NumberRounding.
%
-spec ceiling( number() ) -> integer().
ceiling( X ) ->

	T = erlang:trunc( X ),

	case X - T of

		Pos when Pos > 0 ->
			T + 1;

		%Neg when Neg < 0 ->
		%  T;

		_NegativeOrNull ->
			T

	end.



% Rounds the specified floating-point number at specified offset after the
% decimal point.
%
% Ex: round_after( 12.3456, 3 ) = 12.346.
%
-spec round_after( float(), basic_utils:count() ) -> float().
round_after( F, DigitCount ) ->

	Multiplier = math:pow( 10, DigitCount ),

	% Certainly clumsy, but works:
	erlang:round( Multiplier * F ) / Multiplier.





% Converts specified float to integer.
%
% The float must exactly match the integer value.
%
% Ex: float_to_integer( 5.0 ) = 5, while float_to_integer( 5.0000001 ) will
% crash.
%
-spec float_to_integer( float() ) -> integer().
float_to_integer( F ) ->
	float_to_integer( F, exact ).



% Converts specified float to integer, using specified conversion tolerance.
-spec float_to_integer( float(), conversion_type() ) -> integer().
float_to_integer( F, _ConversionType=exact ) ->

	Int = round( F ),

	case Int - F of

		0.0 ->
			Int;

		Diff ->
			throw( { non_exact_integer_conversion, { F, Int }, Diff } )

	end;

float_to_integer( F, _ConversionType=absolute ) ->
	float_to_integer( F, { absolute, ?epsilon } );

float_to_integer( F, _ConversionType={ absolute, Epsilon } ) ->

	Int = round( F ),

	case are_close( F, Int, Epsilon ) of

		true ->
			Int;

		false ->
			throw( { too_inexact_integer_conversion, { F, Int },
				   { absolute, Epsilon } } )

	end;

float_to_integer( F, _ConversionType=relative ) ->
	float_to_integer( F, { relative, ?epsilon } );

float_to_integer( F, _ConversionType={ relative, Epsilon } ) ->
	Int = round( F ),

	case are_relatively_close( F, Int, Epsilon ) of

		true ->
			Int;

		false ->
			throw( { too_inexact_integer_conversion, { F, Int },
				   { relative, Epsilon } } )

	end.



% Returns the positive remainder of the division of X by Y, in [0;Y[.
%
% In Erlang, -5 rem 3 is -2, whereas this function will return 1,
% since -5 = -2 * 3 + 1.
%
-spec modulo( integer(), non_zero_integer() ) -> non_neg_integer().
modulo( X, Y ) when X > 0 ->
	X rem Y;

modulo( X, Y ) when X < 0 ->

	K = (-X div Y) + 1,

	PositiveX = X + K*Y,

	%io:format( "K=~B, PositiveX=~B~n.", [ K, PositiveX ] ),

	PositiveX rem Y;

modulo( 0, _Y ) ->
	0.



% Clamps specified value between specified bounds: the returned value V is in
% [Min,Max].
%
% We expect that Min <= Max.
%
-spec clamp( number(), number(), number() ) -> number().
clamp( Min, _Max, Value ) when Value < Min ->
	Min;

clamp( _Min, Max, Value ) when Value > Max ->
	Max;

clamp( _Min, _Max, Value ) ->
	Value.



% Returns the square, augmented of a little margin, of the specified element.
squarify( L ) ->
	% "Taylor series", square( epsilon ) is negligible here:
	L * ( L + ?epsilon ).




% Floating-point section.


% Returns true iff the two specified floating-point numbers are deemed close
% enough to be equal, based on default epsilon threshold.
%
-spec are_close( number(), number() ) -> boolean().
are_close( X, Y ) ->
	erlang:abs( X - Y ) < ?epsilon.


% Returns true iff the two specified floating-point numbers are deemed close
% enough to be equal, based on specified epsilon threshold.
%
-spec are_close( number(), number(), number() ) -> boolean().
are_close( X, Y, Epsilon ) ->
	erlang:abs( X - Y ) < Epsilon.



% Returns true iff the two specified floating-point numbers are deemed close
% enough, relatively, to be equal.
%
% The difference between these numbers, divided by their average (a.k.a. the
% relative error), must be smaller than the default epsilon threshold, i.e. the
% maximum tolerance.
%
-spec are_relatively_close( number(), number() ) -> boolean().
are_relatively_close( X, Y ) ->
	are_relatively_close(  X, Y, ?epsilon ).



% Returns true iff the two specified (usually floating-point) numbers are deemed
% close enough, relatively, to be equal.
%
% The difference between these numbers, divided by their average (a.k.a. the
% relative error), must be smaller than the specified epsilon threshold,
% i.e. the maximum tolerance.
%
% Ex: to know whether X and Y are equal with a 5% tolerance, use:
% math_utils:are_relatively_close( X, Y, _Tolerance=0.05 ).
%
-spec are_relatively_close( number(), number(), number() ) -> boolean().
are_relatively_close( X, Y, Epsilon ) ->

	% We will divide by X+Y ... provided this is not null:
	case X+Y of

		0.0 ->
			% X+Y=0, okay; they will be relatively close iff absolutely close
			% (between them, and to zero) here (think for example to X=3 and
			% Y=-3):
			are_close( X, Y, Epsilon );

		_ ->
			%io:format( "X= ~p, Y= ~p~n", [ X, Y ] ),
			2 * erlang:abs( ( X - Y ) / ( X + Y ) ) < Epsilon

	end.



% Returns the relative difference between the two specified numbers.
%
% We consider that if both number are null, then their relative difference is
% also null.
%
-spec get_relative_difference( number(), number() ) ->
									 float() | 'difference_not_computable'.
get_relative_difference( X, Y ) ->

	% Previously was:
	%case -X of
	%
	%	Y ->
	%		difference_not_computable;
	%
	%	_ ->
	%		2 * erlang:abs( X - Y ) / ( X + Y )
	%
	%end.
	% Yet this did not catch cases like: X= 0.0, Y= 0, so:

	% Preventing any future division by zero:
	case X+Y of

		0.0 ->

			% Not =:=, we want X to be converted to a float if needed:
			case X == 0.0 of

				true ->
					0.0;

				false ->
					% Should not happen often:
					throw( { difference_not_computable, X, Y } )

			end;

		Sum ->
			2 * erlang:abs( ( X - Y ) / Sum )

	end.



% Returns true iff the specified number (floating-point or even integer) is
% deemed close enough to zero to be null.
%
-spec is_null( number() ) -> boolean().
is_null( X ) ->
	erlang:abs( X ) < ?epsilon.




% Angle section.

% As we try to remain as much as possible with integer computations, for angle
% we tend to prefer expressing them in degrees rather than in radians.

% Angles in degrees are preferably kept in the [0;360[ interval, i.e. as
% positive integers.


% Converts specified angle in radian into the same angle expressed in degrees.
-spec radian_to_degree( unit_utils:radians() ) -> unit_utils:degrees().
radian_to_degree( AngleInRadians ) ->
	AngleInRadians * 180 / math:pi().


% Canonifies specified angle in degrees, i.e. ensures the returned value that
% corresponds to the specified angle is in the [0;360[ interval.
%
-spec canonify( number() ) -> unit_utils:int_degrees().
canonify( AngleInDegrees ) when is_integer( AngleInDegrees ) ->
	modulo( AngleInDegrees, 360 );

% Here we assume it is a floating-point value, positive or not.
canonify( AngleInDegrees ) ->
	AngleInDegrees - 360 * math:floor( AngleInDegrees / 360 ).
