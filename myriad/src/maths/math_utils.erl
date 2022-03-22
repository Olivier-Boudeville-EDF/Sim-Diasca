% Copyright (C) 2010-2022 Olivier Boudeville
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


% @doc Gathering of various <b>general-purpose basic math</b> facilities.
%
% See `math_utils_test.erl' for the corresponding test.
%
% See `random_utils' for random-related operations.
%
-module(math_utils).


% General operations:
-export([ floor/1, ceiling/1, round_after/2,
		  float_to_integer/1, float_to_integer/2,
		  modulo/2, clamp/2, clamp/3, squarify/1,
		  get_next_power_of_two/1 ]).

-compile({ inline, [ floor/1, ceiling/1, round_after/2,
					 float_to_integer/1, float_to_integer/2,
					 modulo/2, clamp/3, squarify/1 ] }).


% Operations on floating-point values (in Erlang, a float is a C double):
-export([ are_close/2, are_close/3, are_equal/2, are_equal/3,
		  are_relatively_close/2, are_relatively_close/3,
		  get_relative_difference/2, is_null/1 ]).

-compile({ inline, [ are_close/2, are_close/3,
					 are_relatively_close/2, are_relatively_close/3,
					 get_relative_difference/2, is_null/1 ] }).


% Operations with angles:
-export([ radian_to_degree/1, canonify/1 ]).

-compile({ inline, [ radian_to_degree/1, canonify/1 ] }).


% Operations related to functions:
-export([ sample/4, sample_as_pairs/4, normalise/2 ]).


% For epsilon define:
-include("math_utils.hrl").



% Type declarations:

-type non_zero_integer() :: pos_integer() | neg_integer().


-type infinite_number() :: '-infinity' | number() | 'infinity'.
% An (unbounded) number that may be considered as equal to a (positive or
% negative) infinite value.


-type infinite_range() ::
		{ Min :: infinite_number(), Max :: infinite_number() }.
% A possibly infinite range.


-type factor() :: dimensionless().
% A floating-point factor, typically in [0.0,1.0], that is a multiplier involved
% in an equation.


-type integer_factor() :: integer().
% An integer factor, typically in [0.0,1.0], that is a multiplier involved in an
% equation.

-type any_factor() :: number().
% A factor, typically in [0.0,1.0], that is a multiplier involved in an
% equation.


-type positive_factor() :: factor().
% A factor expected to be strictly positive.


-type non_negative_factor() :: factor().
% A factor expected to be positive or null.



-type standard_deviation() :: float().
% Standard deviation, as used to describe a Gaussian curve.


-type variance() :: float().
% Variance, the square of a standard deviation.


-type ratio() :: dimensionless().
% A ration between two values.


-type percent() :: ratio().
% For percentages (1.0 corresponding to 100%).
%
% See also: text_utils:percent_to_string/{1,2}.


-type integer_percent() :: integer().
% For integer percentages (in [0..100] generally).


-type probability() :: float().
% For probabilities, typically ranging in [0.0,1.0].


-type probability_like() :: number().
% A non-negative number (an integer or floating-point) that can be translated to
% a normalized probability by scaling it.
%
% Ex: if considering two exclusive events whose
% respective likeliness is quantified as 20 and 30, then these probability-like
% elements can be translated to actual (normalized) probabilities of
% 20/(20+30)=0.4 and 0.6.


-type conversion_type() :: 'exact' | 'absolute' | { 'absolute', float() }
						 | 'relative' | { 'relative', float() }.
% Describes a desired conversion, which is either exact or approximate, based on
% an absolute or relative comparison, with a default epsilon threshold or a
% user-defined one.




-export_type([ factor/0, integer_factor/0, any_factor/0,
			   positive_factor/0, non_negative_factor/0,
			   non_zero_integer/0,
			   infinite_number/0, infinite_range/0,
			   standard_deviation/0, variance/0,
			   ratio/0, percent/0, integer_percent/0,
			   probability/0, probability_like/0 ]).


% Shorthands:

-type positive_index() :: basic_utils:positive_index().

-type dimensionless() :: unit_utils:dimensionless().
-type degrees() :: unit_utils:degrees().
-type int_degrees() :: unit_utils:int_degrees().
-type radians() :: unit_utils:radians().


% General section.


% @doc Returns the biggest integer smaller than the specified
% floating-point value.
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



% @doc Returns the smallest integer bigger than the specified floating-point
% value.
%
% Note: used to be deprecated in favor of math:ceil/1, yet we prefer the version
% here, which returns an integer rather than a float.
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



% @doc Rounds the specified floating-point number at specified offset after the
% decimal point.
%
% Ex: round_after(12.3456, _DigitCount3) = 12.346.
%
-spec round_after( float(), basic_utils:count() ) -> float().
round_after( F, DigitCount ) ->

	Multiplier = math:pow( 10, DigitCount ),

	% Certainly clumsy, but works:
	erlang:round( Multiplier * F ) / Multiplier.





% @doc Converts specified float to integer.
%
% The float must exactly match the integer value.
%
% Ex: float_to_integer(5.0) = 5, while float_to_integer(5.0000001) will crash.
%
-spec float_to_integer( float() ) -> integer().
float_to_integer( F ) ->
	float_to_integer( F, exact ).



% @doc Converts specified float to integer, using specified conversion
% tolerance.
%
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



% @doc Returns the positive remainder of the division of X by Y, in [0;Y[.
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



% @doc Clamps specified value between specified bounds: the returned value V is
% in [Min, Max].
%
% We expect that `Min <= Max'.
%
-spec clamp( number(), number(), number() ) -> number().
clamp( Min, _Max, Value ) when Value < Min ->
	Min;

clamp( _Min, Max, Value ) when Value > Max ->
	Max;

clamp( _Min, _Max, Value ) ->
	Value.



% @doc Clamps the specified possibly-infinite number within the specified
% possibly-infinite range.
%
-spec clamp( infinite_number(), infinite_range() ) -> infinite_number().
clamp( Number, _R={ '-infinity', 'infinity' } ) ->
	Number;

clamp( Number, _R={ Min, 'infinity' } ) when Number < Min ->
	Min;

clamp( Number, _R={ '-infinity', Max } ) when Number > Max ->
	Max;

% Check as well:
clamp( Number, _R={ Min, Max } ) when  Min < Max, Number < Min ->
	Min;

clamp( Number, _R={ Min, Max } ) when  Min < Max, Number > Max ->
	Max;

clamp( Number, _R={ Min, Max } ) when  Min < Max ->
	Number.



% @doc Returns the square, augmented of a little margin, of the specified
% element.
%
squarify( L ) ->
	% "Taylor series", square(epsilon) is negligible here:
	L * ( L + ?epsilon ).



% @doc Returns the smallest power of two that is greater or equal to the
% specified integer.
%
% Ex: math_utils:get_next_power_of_two( 5 ) = 8.
%
-spec get_next_power_of_two( non_neg_integer() ) -> pos_integer().
get_next_power_of_two( I ) ->
	get_next_power_of_two( I, _MinCandidate=1 ).


% (helper)
get_next_power_of_two( I, Candidate ) when Candidate >= I ->
	Candidate;

get_next_power_of_two( I, Candidate ) ->
	get_next_power_of_two( I, 2*Candidate ).



% Floating-point section.



% @doc Returns true iff the two specified floating-point numbers are deemed
% close enough to be equal, based on default epsilon threshold.
%
% Note that such absolute tolerance comparison fails when X and Y become large,
% so generally are_relatively_close/2 shall be preferred.
%
-spec are_close( number(), number() ) -> boolean().
are_close( X, Y ) ->
	erlang:abs( X - Y ) < ?epsilon.


% @doc Returns true iff the two specified floating-point numbers are deemed
% close enough to be equal, based on specified epsilon threshold.
%
-spec are_close( number(), number(), number() ) -> boolean().
are_close( X, Y, Epsilon ) ->
	erlang:abs( X - Y ) < Epsilon.



% @doc Returns true iff the two specified floating-point numbers are deemed
% close enough to be equal, based on default epsilon threshold.
%
% Note: alias of are_close/2, defined for consistency.
%
-spec are_equal( number(), number() ) -> boolean().
are_equal( X, Y ) ->
	erlang:abs( X - Y ) < ?epsilon.


% @doc Returns true iff the two specified floating-point numbers are deemed
% close enough to be equal, based on specified epsilon threshold.
%
% Note: alias of are_close/3, defined for consistency.
%
-spec are_equal( number(), number(), number() ) -> boolean().
are_equal( X, Y, Epsilon ) ->
	erlang:abs( X - Y ) < Epsilon.



% @doc Returns true iff the two specified floating-point numbers are deemed
% close enough, relatively, to be equal.
%
% The difference between these numbers, divided by their average (a.k.a. the
% relative error), must be smaller than the default epsilon threshold, ie the
% maximum tolerance.
%
-spec are_relatively_close( number(), number() ) -> boolean().
are_relatively_close( X, Y ) ->
	are_relatively_close( X, Y, ?epsilon ).



% @doc Returns true iff the two specified (usually floating-point) numbers are
% deemed close enough, relatively, to be equal.
%
% The difference between these numbers, divided by their average (a.k.a. the
% relative error), must be smaller than the specified epsilon threshold,
% ie the maximum tolerance.
%
% Ex: to know whether X and Y are equal with a 5% tolerance, use:
% math_utils:are_relatively_close(X, Y, _Tolerance=0.05).
%
-spec are_relatively_close( number(), number(), number() ) -> boolean().
are_relatively_close( X, Y, Epsilon ) ->

	% are_close/2 is not satisfactory when X and Y are small:

	% As for: 'abs( X - Y ) < ?epsilon * max( abs(X), abs(Y) )' it would fail
	% when X and Y are small.

	% Another approach than the one currently used below is to perform a
	% comparison with a relative tolerance for large values, and an absolute
	% tolerance for small values, as described by Christer Ericson in
	% http://realtimecollisiondetection.net/blog/?p=89 and in his book
	% "Real-Time Collision Detection", yielding to:
	%
	% abs( X - Y ) =< ?epsilon * max( 1.0, max( abs(X), abs(Y) ) ).

	% Here, we will divide by X+Y ... provided this is not null:
	case X+Y of

		0.0 ->
			% X+Y=0, okay; they will be relatively close iff absolutely close
			% (between them, and to zero) here (think for example to X=3 and
			% Y=-3):
			%
			are_close( X, Y, Epsilon );

		_ ->
			%trace_utils:debug_fmt( "X= ~p, Y= ~p~n", [ X, Y ] ),
			2 * erlang:abs( ( X - Y ) / ( X + Y ) ) < Epsilon

	end.



% @doc Returns the relative difference between the two specified numbers.
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



% @doc Returns true iff the specified number (floating-point or even integer) is
% deemed close enough to zero to be null.
%
-spec is_null( number() ) -> boolean().
is_null( X ) ->
	erlang:abs( X ) < ?epsilon.




% Angle section.

% As we try to remain as much as possible with integer computations, for angle
% we tend to prefer expressing them in degrees rather than in radians.

% Angles in degrees are preferably kept in the [0;360[ interval, ie as
% positive integers.


% @doc Converts specified angle in radian into the same angle expressed in
% degrees.
%
-spec radian_to_degree( radians() ) -> degrees().
radian_to_degree( AngleInRadians ) ->
	AngleInRadians * 180 / math:pi().


% @doc Canonifies specified angle in degrees, ie ensures the returned value that
% corresponds to the specified angle is in the [0;360[ interval.
%
-spec canonify( number() ) -> int_degrees().
canonify( AngleInDegrees ) when is_integer( AngleInDegrees ) ->
	modulo( AngleInDegrees, 360 );

% Here we assume it is a floating-point value, positive or not.
canonify( AngleInDegrees ) ->
	AngleInDegrees - 360 * math:floor( AngleInDegrees / 360 ).



% @doc Samples the specified function taking a single numerical argument, by
% evaluating it on every point in turn from Start until up to Stop, with
% specified increment: returns the ordered list of the corresponding values that
% it took.
%
-spec sample( fun( ( number() ) -> T ), number(), number(), number() ) -> [ T ].
sample( Fun, Start, Stop, Increment ) ->
	sample( Fun, _Current=Start, Stop, Increment, _Acc=[] ).


% (helper)
sample( _Fun, Current, Stop, _Increment, Acc ) when Current > Stop ->
	lists:reverse( Acc );

sample( Fun, Current, Stop, Increment, Acc ) ->
	NewValue = Fun( Current ),
	sample( Fun, Current+Increment, Stop, Increment, [ NewValue | Acc ] ).



% @doc Samples the specified function taking a single numerical argument, by
% evaluating it on every point in turn from Start until up to Stop, with
% specified increment: returns the ordered list of the corresponding {X,f(X)}
% pairs that it took.
%
-spec sample_as_pairs( fun( ( number() ) -> T ), number(), number(),
					   number() ) -> [ { number(), T } ].
sample_as_pairs( Fun, Start, Stop, Increment ) ->
	sample_as_pairs( Fun, _Current=Start, Stop, Increment, _Acc=[] ).


% (helper)
sample_as_pairs( _Fun, Current, Stop, _Increment, Acc ) when Current > Stop ->
	lists:reverse( Acc );

sample_as_pairs( Fun, Current, Stop, Increment, Acc ) ->
	NewValue = Fun( Current ),
	sample_as_pairs( Fun, Current+Increment, Stop, Increment,
					 [ { Current, NewValue }| Acc ] ).



% @doc Normalises, in the specified list of tuples, the elements at the
% specified index (expected to be floats), so that their sum is equal to 1.0.
%
% Ex: normalise([{a,3}, {"hello",5}, {1,2}], _Index=2)
%                 = [{a,0.3}, {"hello",0.5}, {1,0.2}]
%
-spec normalise( [ tuple() ], positive_index() ) -> [ tuple() ].
normalise( _DataTuples=[], _Index ) ->
	throw( no_data_tuple );

normalise( DataTuples, Index ) ->
	Sum = get_sum( DataTuples, Index, _Sum=0 ),
	[ scale( Tuple, Index, Sum ) || Tuple <- DataTuples ].


% (helper)
get_sum( _DataTuples=[], _Index, Sum ) ->
	Sum;

get_sum( _DataTuples=[ Tuple | T ], Index, Sum ) ->
	Elem = element( Index, Tuple ),
	get_sum( T, Index, Sum + Elem ).


% (helper)
scale( Tuple, Index, Sum ) ->
	NewElem = element( Index, Tuple ) / Sum,
	setelement( Index, Tuple, NewElem ).
