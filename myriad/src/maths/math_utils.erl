% Copyright (C) 2010-2023 Olivier Boudeville
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
		  sign/1,
		  square/1, int_pow/2, get_next_power_of_two/1,
		  ln/1 ]).

-compile({ inline, [ floor/1, ceiling/1, round_after/2,
					 float_to_integer/1, float_to_integer/2,
					 modulo/2, clamp/3, squarify/1 ] }).


% Operations on floating-point values (in Erlang, a float is a C double):
%
% (note also that now +0.0 and -0.0 do not match)
%
-export([ are_close/2, are_close/3, are_equal/2, are_equal/3,
		  are_relatively_close/2, are_relatively_close/3,
		  get_relative_difference/2, is_null/1, is_null/2,
		  is_greater/2, is_lower/2, is_greater/3, is_lower/3 ]).

-compile({ inline, [ are_close/2, are_close/3,
					 are_relatively_close/2, are_relatively_close/3,
					 get_relative_difference/2, is_null/1,
					 is_greater/2, is_lower/2, is_greater/3, is_lower/3 ] }).


% Operations on number():
-export([ is_null_number/1 ]).


% Operations with angles:
-export([ radians_to_degrees/1, degrees_to_radians/1,
		  % Obsolete forms: radian_to_degree/1, degree_to_radian/1,
		  canonify/1 ]).

-compile({ inline,
		   [ radians_to_degrees/1, degrees_to_radians/1, canonify/1 ] }).


% Operations related to functions:
-export([ evaluate/2, sample/4, sample_as_pairs/4, normalise/2,

		  compute_support/1, compute_support/3, compute_support/4,

		  compute_integer_support/1, compute_integer_support/3,
		  compute_integer_support/4,

		  canonicalise_bounds/1, canonicalise_integer_bounds/1,
		  is_within_bounds/2, are_within_bounds/2,
		  bounds_to_string/1, integer_bounds_to_string/1 ]).


% Operations related to probabilities:
-export([ probability_to_string/1, probability_like_to_string/1 ]).


% Operations related to percentages:
-export([ check_percent_basic_range/1 ]).


% Special functions:
-export([ factorial/1, gamma/1 ]).


% For epsilon define:
-include("math_utils.hrl").


% Must be fine enough to minimise the risk of missing zeros:
-define( base_increment, 0.01 ).

% For quick lookups:
-define( coarse_increment, 0.1 ).


% Type declarations:

-type non_zero_integer() :: pos_integer() | neg_integer().


-type infinite_number() :: '-infinity' | number() | 'infinity'.
% An (unbounded) number that may be considered as equal to a (positive or
% negative) infinite value.


-type infinite_range() ::
		{ Min :: infinite_number(), Max :: infinite_number() }.
% A possibly infinite range.


-type factor() :: dimensionless().
% A floating-point factor, typically in [0.0, 1.0], that is a multiplier
% involved in an equation.


-type integer_factor() :: integer().
% An integer factor, typically in [0.0, 1.0], that is a multiplier involved in
% an equation.

-type any_factor() :: number().
% A factor, typically in [0.0, 1.0], that is a multiplier involved in an
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



% See also the random_utils module for:

-type probability() :: float().
% For probabilities, typically ranging in [0.0, 1.0].


-type probability_like() :: number().
% A non-negative number (an integer or floating-point) that can be translated to
% a normalised probability by scaling it to [0.0, 1.0].
%
% For example if considering two exclusive events whose
% respective likeliness is quantified as 20 and 30, then these probability-like
% elements can be translated to actual (normalised) probabilities of
% 20/(20+30)=0.4 and 0.6.



-type conversion_type() :: 'exact' | 'absolute' | { 'absolute', float() }
						 | 'relative' | { 'relative', float() }.
% Describes a desired conversion, which is either exact or approximate, based on
% an absolute or relative comparison, with a default epsilon threshold or a
% user-defined one.



-type bound() :: float().
-type integer_bound() :: integer().

-type bounds() :: { MinBound :: bound(), MaxBound :: bound() }.
% An interval, based on upper and lower (floating-point) bounds, typically of a
% compact support.


-type integer_bounds() ::
		{ MinBound :: integer_bound(), MaxBound :: integer_bound() }.
% An interval, based on upper and lower integer bounds, typically of a compact
% support.


-type abscissa() :: float().

-type integer_abscissa() :: integer().

-type any_abscissa() :: abscissa() | integer_abscissa().


-type integer_to_integer_fun() :: fun( ( integer() ) -> integer() ).
% A function returning an integer from an integer.

-type float_to_integer_fun() :: fun( ( float() ) -> integer() ).
% A function returning an integer from a float.


-type number_to_integer_fun() :: fun( ( number() ) -> integer() ).
% A function returning an integer from a number (typically a float).

-type number_to_float_fun() :: fun( ( number() ) -> float() ).
% A function returning a float from a number (typically a float).


-type integer_to_float_fun() :: fun( ( integer() ) -> float() ).
% A function returning a float from an integer.

-type float_to_float_fun() :: fun( ( float() ) -> float() ).
% A function returning a float from an integer.


-type float_result_value() :: float().
% A floating-point value obtained when evaluating a function.



-export_type([ factor/0, integer_factor/0, any_factor/0,
			   positive_factor/0, non_negative_factor/0,
			   non_zero_integer/0,
			   infinite_number/0, infinite_range/0,
			   standard_deviation/0, variance/0,
			   ratio/0, percent/0, integer_percent/0,
			   probability/0, probability_like/0,
			   bound/0, integer_bound/0,
			   bounds/0, integer_bounds/0,
			   abscissa/0, integer_abscissa/0, any_abscissa/0,
			   integer_to_integer_fun/0, float_to_integer_fun/0,
			   number_to_integer_fun/0, number_to_float_fun/0,
			   integer_to_float_fun/0, float_to_float_fun/0,
			   float_result_value/0 ]).


% Local types:

-type increment() :: float().
-type integer_increment() :: integer().

-type epsilon() :: float().
% A smaller, positive floating-point value.


% Shorthands:

-type positive_index() :: basic_utils:positive_index().
-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type dimensionless() :: unit_utils:dimensionless().
-type degrees() :: unit_utils:degrees().
-type int_degrees() :: unit_utils:int_degrees().
-type any_degrees() :: unit_utils:any_degrees().
-type radians() :: unit_utils:radians().


% General section.


% @doc Rounds down the specified floating-point value: returns the biggest
% integer smaller than the specified floating-point value.
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



% @doc Rounds up the specified floating-point value: returns the smallest
% integer bigger than the specified floating-point value.
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



% @doc Rounds the specified floating-point number at the specified offset after
% the decimal point.
%
% For example round_after(12.3456, _DigitCount3) = 12.346.
%
-spec round_after( float(), count() ) -> float().
round_after( F, DigitCount ) ->

	Multiplier = math:pow( 10, DigitCount ),

	% Certainly clumsy, but works:
	erlang:round( Multiplier * F ) / Multiplier.





% @doc Converts the specified float to integer.
%
% The float must exactly match the integer value.
%
% For example float_to_integer(5.0) = 5, while float_to_integer(5.0000001) will
% crash.
%
-spec float_to_integer( float() ) -> integer().
float_to_integer( F ) ->
	float_to_integer( F, exact ).



% @doc Converts the specified float to integer, using the specified conversion
% tolerance.
%
-spec float_to_integer( float(), conversion_type() ) -> integer().
float_to_integer( F, _ConversionType=exact ) ->

	Int = round( F ),

	Diff = Int - F,

	case Diff == 0 of

		true ->
			Int;

		false ->
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



% @doc Clamps the specified value between specified bounds: the returned value
% is in [Min, Max].
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



% @doc Returns the sign of the specified value, either as 1 or -1.
-spec sign( number() ) -> 1 | -1.
sign( N ) when N >= 0 ->
	1;

sign( _N ) ->
	-1.


% @doc Returns the square of the specified number.
%
% Always useful.
%
-spec square( number() ) -> number().
square( N ) ->
	N*N.


% @doc Returns the power of the specified number X to the specified positive
% integer N: X^N.
%
% Designed for integer exponents, as opposed to math:pow/1.
%
% Refer to
% https://stackoverflow.com/questions/38533797/how-to-calculate-5262144-in-erlang/38534076#38534076
% for a presumably faster version.
%
-spec int_pow( number(), non_neg_integer() ) -> number().
int_pow( _X, _N=0 ) ->
	1;

int_pow( X, _N=1 ) ->
	X;

int_pow( X, N ) when is_integer( N ) ->
	X * int_pow( X, N-1 ).



% @doc Returns the smallest power of two that is greater or equal to the
% specified integer.
%
% For example math_utils:get_next_power_of_two(5) = 8.
%
-spec get_next_power_of_two( non_neg_integer() ) -> pos_integer().
get_next_power_of_two( I ) ->
	get_next_power_of_two( I, _MinCandidate=1 ).


% (helper)
get_next_power_of_two( I, Candidate ) when Candidate >= I ->
	Candidate;

get_next_power_of_two( I, Candidate ) ->
	get_next_power_of_two( I, 2*Candidate ).



% @doc Returns the natural logarithm (that is the log in base e) of the
% specified number.
%
% The natural logarithm n of x>0 is the power to which e would have to be raised
% to equal x: n = ln(x) or e^n = x.
%
% Logarithms in other bases (here: than e) differ only by a constant multiplier
% from the natural logarithm, and can be defined in terms of the latter: for a
% base 'b', logb(x) = ln(x)/ln(b) = ln(x)*logb(e).
%
% Note that math:log/1 is x -> ln(x), whereas math:log10/1 is the standard base
% 10 log.
%
-spec ln( number() ) -> float().
ln( X ) ->
	math:log( X ).



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
% Note that such absolute tolerance comparison fails when X and Y become large,
% so generally are_relatively_close/2 shall be preferred.
%
-spec are_close( number(), number(), epsilon() ) -> boolean().
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
-spec are_equal( number(), number(), epsilon() ) -> boolean().
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
% that is the maximum tolerance.
%
% For example to know whether X and Y are equal with a 5% tolerance, use:
% math_utils:are_relatively_close(X, Y, _Tolerance=0.05).
%
-spec are_relatively_close( number(), number(), epsilon() ) -> boolean().
are_relatively_close( X, Y, Epsilon ) ->

	% are_close/2 is not satisfactory at least when X and Y are large.
	%
	% As for: 'abs( X - Y ) < ?epsilon * max( abs(X), abs(Y) )' it would fail
	% when X and Y are small.

	% Another approach than the one below is to perform a comparison with a
	% relative tolerance for large values, and an absolute tolerance for small
	% values, as described by Christer Ericson in
	% http://realtimecollisiondetection.net/blog/?p=89 and in his book
	% "Real-Time Collision Detection", yielding to:
	%
	abs( X - Y ) =< Epsilon * max( 1.0, max( abs(X), abs(Y) ) ).

	% The previous implementation was:

	% The difference between these numbers, divided by their average (a.k.a. the
	% relative error), must be smaller than the specified epsilon threshold, ie
	% the maximum tolerance.

	% Here, we will divide by X+Y ... provided that this is not null:
	% case X+Y of

	%	0.0 ->
	%		% X+Y=0, okay; then they will be relatively close iff absolutely
	%		% close (between them, and to zero) here (think for example to X=3
	%		% and Y=-3):
	%		%
	%		are_close( X, Y, Epsilon );

	%	0 ->
	%		% Implies X and Y are both integers, so:
	%		X =:= Y;

	%	_ ->
	%		%trace_utils:debug_fmt( "X= ~p, Y= ~p~n", [ X, Y ] ),
	%		2 * erlang:abs( ( X - Y ) / ( X + Y ) ) < Epsilon

	% end.



% @doc Returns the relative difference between the two specified numbers.
%
% We consider that if both number are null, then their relative difference is
% also null.
%
-spec get_relative_difference( number(), number() ) ->
								float(). % | 'difference_not_computable'.
get_relative_difference( X, Y ) ->

	% Previously was:
	%case -X of
	%
	%   Y ->
	%       difference_not_computable;
	%
	%   _ ->
	%       2 * erlang:abs( X - Y ) / ( X + Y )
	%
	%end.
	% Yet this did not catch cases like: X=0.0, Y=0, so:

	% Avoiding to match directly with 0.0, as wanting to match -0.0 as well:
	Sum = X + Y,

	% Preventing any future division by zero:
	%
	% (avoiding to match directly with 0.0, as wanting to match -0.0 as well)
	%
	case Sum == 0 of

		true ->
			% Not =:=, we want X to be converted to a float if needed:
			case X == 0.0 of

				true ->
					0.0;

				false ->
					% Should not happen often:
					throw( { difference_not_computable, X, Y } )

			end;

		false ->
			2 * erlang:abs( ( X - Y ) / Sum )

	end.



% @doc Returns true iff the specified number (floating-point or even integer) is
% deemed close enough to zero to be null.
%
-spec is_null( number() ) -> boolean().
is_null( X ) ->
	erlang:abs( X ) < ?epsilon.


% @doc Returns true iff the specified number (floating-point or even integer) is
% deemed close enough (based on specified epsilon) to zero to be null.
%
-spec is_null( number(), epsilon() ) -> boolean().
is_null( X, Epsilon ) ->
	erlang:abs( X ) < Epsilon.



% @doc Returns true iff the specified number is deemed close enough to zero to
% be null.
%
-spec is_null_number( number() ) -> boolean().
is_null_number( 0 ) ->
	true;

is_null_number( I ) when is_integer( I )  ->
	false;

% float() expected then:
is_null_number( F ) ->
	erlang:abs( F ) < ?epsilon.



% @doc Returns whether X is greater than Y (with a small margin), both being
% expected to be floats.
%
% No is_greater_or_equal/2 makes sense with floats.
%
-spec is_greater( float(), float() ) -> boolean().
is_greater( X, Y ) ->
	% Could make sense, yet we do not want, for any (X,Y), is_greater(X,Y) and
	% is_equal(X,Y) to be both true:
	% X > Y - ?epsilon.
	X > Y + ?epsilon.


% @doc Returns whether X is greater than Y, with the specified margin, both
% being expected to be floats.
%
% No is_greater_or_equal/3 makes sense with floats.
%
-spec is_greater( float(), float(), epsilon() ) -> boolean().
is_greater( X, Y, Epsilon ) ->
	% Refer to is_greater/2 for further details:
	X > Y + Epsilon.



% @doc Returns whether X is lower than Y (with a small margin), both being
% expected to be floats.
%
% No is_lower_or_equal/2 makes sense with floats.
%
-spec is_lower( float(), float() ) -> boolean().
is_lower( X, Y ) ->
	% Could make sense, yet we do not want, for any (X,Y), is_lower(X,Y) and
	% is_equal(X,Y) to be both true:
	% X < Y + ?epsilon.
	X < Y - ?epsilon.


% @doc Returns whether X is lower than Y (with a small margin), both being
% expected to be floats.
%
% No is_lower_or_equal/3 makes sense with floats.
%
-spec is_lower( float(), float(), epsilon() ) -> boolean().
is_lower( X, Y, Epsilon ) ->
	X < Y - Epsilon.



% Angle section.

% As we try to remain as much as possible with integer computations, for angle
% we tend to prefer expressing them in degrees rather than in radians.

% Angles in degrees are preferably kept in the [0;360[ interval, that is as
% positive integers.


% @doc Converts the specified angle in radians into the same angle expressed in
% degrees.
%
-spec radians_to_degrees( radians() ) -> degrees().
radians_to_degrees( AngleInRadians ) ->
	AngleInRadians * 180 / math:pi().


% at-doc Converts the specified angle in radians into the same angle expressed
% in degrees.
%
% The plural form radians_to_degrees/1 is now to be preferred.
%
%-spec radian_to_degree( radians() ) -> degrees().
%radian_to_degree( AngleInRadians ) ->
%   radians_to_degrees( AngleInRadians ).



% @doc Converts the specified angle in degrees into the same angle expressed in
% radians.
%
-spec degrees_to_radians( any_degrees() ) -> radians().
degrees_to_radians( AngleInDegrees ) ->
	AngleInDegrees * math:pi() / 180.


% at-doc Converts the specified angle in degrees into the same angle expressed
% in radians.
%
% The plural form degrees_to_radians/1 is now to be preferred.
%
%-spec degree_to_radian( degrees() ) -> radians().
%degree_to_radian( AngleInDegrees ) ->
%   degrees_to_radians( AngleInDegrees ).



% @doc Canonifies the specified angle in degrees, ie ensures the returned value
% that corresponds to the specified angle is in the [0;360[ interval.
%
-spec canonify( number() ) -> int_degrees().
canonify( AngleInDegrees ) when is_integer( AngleInDegrees ) ->
	modulo( AngleInDegrees, 360 );

% Here we assume it is a floating-point value, positive or not.
canonify( AngleInDegrees ) ->
	AngleInDegrees - 360 * math:floor( AngleInDegrees / 360 ).



% @doc Evaluates the specified function as the specified abscissa, and returns
% the corresponding value if it could be computed, otherwise returns 'undefined'
% (this happens when typically 'badarith' is thrown due to an operation failing,
% like for math:pow(1000,1000).
%
-spec evaluate( float_to_float_fun(), abscissa() ) ->
											maybe( float_result_value() ).
evaluate( Fun, Abs ) ->
	try

		Fun( Abs )

	catch error:badarith ->
		undefined

	end.



% @doc Samples the specified function taking a single numerical argument, by
% evaluating it on every point in turn from Start until up to Stop, with
% specified increment: returns the ordered list of the corresponding values that
% it took.
%
-spec sample( fun( ( number() ) -> T ), number(), number(), number() ) -> [ T ].
sample( Fun, StartPoint, StopPoint, Increment ) ->
	sample( Fun, _Current=StartPoint, StopPoint, Increment, _Acc=[] ).


% (helper)
sample( _Fun, CurrentPoint, StopPoint, _Increment, Acc )
											when CurrentPoint > StopPoint ->
	lists:reverse( Acc );

sample( Fun, CurrentPoint, StopPoint, Increment, Acc ) ->
	% Not trying to resist errors with evaluate/2:
	NewValue = Fun( CurrentPoint ),
	sample( Fun, CurrentPoint + Increment, StopPoint, Increment,
			[ NewValue | Acc ] ).



% @doc Samples uniformly the specified function taking a single numerical
% argument, by evaluating it on every point in turn from Start until up to Stop,
% with specified increment: returns the ordered list of the corresponding
% {X,f(X)} pairs that it took.
%
-spec sample_as_pairs( fun( ( number() ) -> T ), number(), number(),
					   number() ) -> [ { number(), T } ].
sample_as_pairs( Fun, StartPoint, StopPoint, Increment ) ->
	sample_as_pairs( Fun, _CurrentPoint=StartPoint, StopPoint, Increment,
					 _Acc=[] ).


% (helper)
sample_as_pairs( _Fun, CurrentPoint, StopPoint, _Increment, Acc )
								when CurrentPoint > StopPoint ->
	lists:reverse( Acc );

sample_as_pairs( Fun, CurrentPoint, StopPoint, Increment, Acc ) ->
   NewValue = Fun( CurrentPoint ),
   sample_as_pairs( Fun, CurrentPoint+Increment, StopPoint, Increment,
					[ { CurrentPoint, NewValue } | Acc ] ).



% @doc Normalises, in the specified list of tuples (possibly just pairs), the
% elements at the specified index (expected to be numbers, whose sum is
% non-null), so that their sum becomes equal to 1.0.
%
% Typically useful for probabilities.
%
% For example normalise([{a,3}, {"hello",5}, {1,2}], _Index=2)
%                 = [{a,0.3}, {"hello",0.5}, {1,0.2}]
%
-spec normalise( [ tuple() ], positive_index() ) -> [ tuple() ].
normalise( _DataTuples=[], _Index ) ->
	throw( no_data_tuple );

normalise( DataTuples, Index ) ->
	Sum = get_sum( DataTuples, Index, _Sum=0 ),
	case is_null( Sum ) of

		true ->
			throw( { null_sum, Sum, DataTuples, Index } );

		false ->
			[ scale( Tuple, Index, Sum ) || Tuple <- DataTuples ]

	end.


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



% @doc Returns an evaluation of the (supposedly finite, compact, to some extent
% centered around zero) support of the specified function, taking one
% floating-point value and returning one, as an interval with no bound
% restriction.
%
% This simple heuristics assumes that this function vanishes at infinity; starts
% looking around zero, based on a default epsilon.
%
% Typically useful to properly discretise probability density functions (with
% floating-point samples).
%
-spec compute_support( float_to_float_fun() ) -> bounds().
compute_support( Fun ) ->
	compute_support( Fun, _MaybeMin=undefined, _MaybeMax=undefined ).



% @doc Returns an evaluation of the (supposedly finite, compact, to some extent
% centered around zero) support of the specified function, taking one
% floating-point value and returning one, as an interval complying to the
% specified bound restriction (to limit it in a given interval, possibly because
% this function is not defined out of it).
%
% This simple heuristics assumes that this function vanishes at infinity; starts
% looking around zero, based on a default epsilon.
%
% Typically useful to properly discretise probability density functions (with
% floating-point samples).
%
-spec compute_support( float_to_float_fun(),
					   maybe( bound() ), maybe( bound() ) ) -> bounds().
compute_support( Fun, MaybeMin=undefined, MaybeMax=undefined ) ->
	compute_support( Fun, _Origin=0.0, MaybeMin, MaybeMax );

compute_support( Fun, MaybeMin=undefined, Max ) ->
	Origin = Max - abs( Max / 2 ),
	compute_support( Fun, Origin, MaybeMin, Max );

compute_support( Fun, Min, MaybeMax=undefined ) ->
	Origin = Min + abs( Min / 2 ),
	compute_support( Fun, Origin, Min, MaybeMax );

compute_support( Fun, Min, Max ) ->
	Origin = ( Min + Max ) / 2,
	compute_support( Fun, Origin, Min, Max ).



% @doc Returns an evaluation of the (supposedly finite, compact, to some extent
% centered around the specified origin) support of the specified function,
% taking one floating-point value and returning one, as an interval complying to
% the specified bound restriction (to limit it in a given interval, possibly
% because this function is not defined out of it).
%
% This simple heuristics assumes that this function vanishes at infinity; starts
% looking around zero, based on a default epsilon.
%
% Typically useful to properly discretise probability density functions (with
% floating-point samples).
%
-spec compute_support( float_to_float_fun(), abscissa(),
					   maybe( bound() ), maybe( bound() ) ) -> bounds().
compute_support( Fun, Origin, MaybeMin, MaybeMax ) ->

	% Tests are limited, should for example x -> sin(x) be specified:
	Bounds = compute_support( Fun, Origin, MaybeMin, MaybeMax, ?base_increment,
							  _RemainingTests=128, _Epsilon=?epsilon ),

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:debug_fmt( "Returning float support ~ts.",
			[ bounds_to_string( Bounds ) ] ) ),

	Bounds.



% Searches the function support from the specified origin.
%
% (helper)
%
-spec compute_support( float_to_float_fun(), abscissa(), maybe( bound() ),
			maybe( bound() ), increment(), count(), epsilon() ) -> bounds().
compute_support( Fun, Origin, MaybeMin, MaybeMax, Inc, RemainingTests,
				 Epsilon ) ->

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "Computing the support of ~p from origin ~f, "
			"with following restrictions: min=~w, max=~w.",
			[ Fun, Origin, MaybeMin, MaybeMax ] ) ),

	% Searches first a non-null point, the "pivot", which is the first non-null
	% point found around the origin, looking alternatively on each side based on
	% exponentially-increasing distance:
	%
	Pivot = case search_non_null( Fun, Origin, MaybeMin, MaybeMax, Inc,
								  RemainingTests, Epsilon ) of

		undefined ->
			throw( { no_non_null_point_found, Fun } );

		P ->
			P

	end,

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "Pivot found at abscissa ~w.", [ Pivot ] ) ),

	% Now tries to find from the pivot the right end of the support, first by
	% quickly converging to any remote zero:
	%
	FirstRightZero = search_first_null( Fun, Pivot, MaybeMax, ?coarse_increment,
										RemainingTests, Epsilon ),

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "First right zero found at abscissa ~w.",
							  [ FirstRightZero ] ) ),

	% Tries to find a closer zero than this first one:
	MinRightZero = minimise_zero( Fun, Pivot, FirstRightZero,
								  MaybeMin, MaybeMax, Epsilon ),

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "Right zero minimised at abscissa ~w, "
			"now looking for left zeros.", [ MinRightZero ] ) ),

	% To find the left end of the support (hence opposite direction):
	FirstLeftZero = search_first_null( Fun, Pivot, MaybeMin, -?coarse_increment,
									   RemainingTests, Epsilon ),

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "First left zero found at abscissa ~w.",
							  [ FirstLeftZero ] ) ),

	% Minimised in terms of absolute value (hence closer to the origin):
	MaxLeftZero = minimise_zero( Fun, Pivot, FirstLeftZero,
								 MaybeMin, MaybeMax, Epsilon ),

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "Left zero maximised at abscissa ~w.",
							  [ MaxLeftZero ] ) ),

	B = { MaxLeftZero, MinRightZero },

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "Returning following support: ~ts.",
							  [ math_utils:bounds_to_string( B ) ] ) ),

	B.



% Explores exponentially both sides alternatively of the specified origin,
% looking for any non-null value.
%
% (helper)
-spec search_non_null( float_to_float_fun(), abscissa(), maybe( bound() ),
					   maybe( bound() ), increment(), count(), epsilon() ) ->
							maybe( abscissa() ).
% Search failed:
search_non_null( Fun, Origin, MaybeMin, MaybeMax, Inc,
				 _RemainingTests=0, _Epsilon ) ->

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:debug_fmt( "The search for a non-zero of ~p on both sides "
			"of ~f failed, whereas last increment was ~f (min: ~w, max: ~w).",
			[ Fun, Origin, Inc, MaybeMin, MaybeMax ] ),
		basic_utils:ignore_unused( [ Fun, Origin, Inc, MaybeMin, MaybeMax ] ) ),

	undefined;


search_non_null( Fun, Origin, MaybeMin, MaybeMax, Inc, RemainingTests,
				 Epsilon ) ->

	TestedPoint = Origin + Inc,

	% Hence growing exponentially, on either side alternatively:
	%Factor = -2.0,
	Factor = -1.1,

	case is_within( TestedPoint, MaybeMin, MaybeMax ) of

		true ->
			cond_utils:if_defined( myriad_debug_math,
				trace_utils:debug_fmt( "Searching for a non-null point at "
									   "abscissa ~w.", [ TestedPoint ] ) ),

			case evaluate( Fun, TestedPoint ) of

				undefined ->
					% Often unlikely to get better:
					search_non_null( Fun, Origin, MaybeMin, MaybeMax,
						Factor * Inc, RemainingTests-1, Epsilon );

				Value ->
					case is_null( Value, Epsilon ) of

						true ->
							search_non_null( Fun, Origin, MaybeMin, MaybeMax,
								Factor * Inc, RemainingTests-1, Epsilon );

						false ->
							TestedPoint

					end

			end;

		false ->
			% No test spent here, on this side:
			search_non_null( Fun, Origin, MaybeMin, MaybeMax, Factor * Inc,
							 RemainingTests, Epsilon )

	end.




% Searches by dichotomy in one direction (defined by the sign of the increment)
% the first null point, for which the next ones being are supposed to be all
% null, found from the specified origin (whose value is expected to be
% non-null).
%
-spec search_first_null( float_to_float_fun(), abscissa(), maybe( bound() ),
						 increment(), count(), epsilon() ) -> abscissa().
search_first_null( Fun, Pivot, MaybeMax, Inc, _RemainingTests=0, _Epsilon ) ->

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:debug_fmt( "The search for a first zero of ~p "
			"from pivot ~f failed, whereas last increment was ~f (max: ~w).",
			[ Fun, Pivot, Inc, MaybeMax ] ),
		basic_utils:ignore_unused( [ Fun, Pivot, Inc, MaybeMax ] ) ),

	throw( { no_null_point_found, Fun, Pivot, MaybeMax } );

search_first_null( Fun, Pivot, MaybeMax, Inc, RemainingTests, Epsilon ) ->

	TestedPoint = Pivot + Inc,

	case is_before( TestedPoint, MaybeMax, Inc ) of

		true ->
			cond_utils:if_defined( myriad_debug_math,
				trace_utils:debug_fmt( "Testing point ~f as potential zero.",
									   [ TestedPoint ] ) ),

			case evaluate( Fun, TestedPoint ) of

				undefined ->
					% If ever it improved:
					search_first_null( Fun, Pivot, MaybeMax, 2.0 * Inc,
									   RemainingTests-1, Epsilon );

				Value ->
					case is_null( Value, Epsilon ) of

						true ->
							cond_utils:if_defined( myriad_debug_math,
								trace_utils:debug_fmt( "Point ~f is a zero.",
													   [ TestedPoint ] ) ),

							TestInc = ?coarse_increment * sign( Inc ),

							% As we do not want to be fooled by an only-local
							% zero:
							case search_non_null_one_direction( Fun,
									TestedPoint, _MaybeMin=undefined, MaybeMax,
									TestInc, _RemTests=48, Epsilon ) of

								undefined ->
									% OK, confirmed (a relevant trace already
									% emitted):

									%trace_utils:debug_fmt( "Point ~f is "
									%   "considered as a zero onward.",
									%   [ TestedPoint ] ),
									TestedPoint;

								FartherNonZero ->
									trace_utils:warning_fmt( "A non-zero "
										"farther than the current zero "
										"candidate ~f has been detected: ~f; "
										"recalibrating.",
										[ TestedPoint, FartherNonZero ] ),
									search_first_null( Fun, FartherNonZero,
										MaybeMax, Inc, _NewTest=48, Epsilon )

							end;

						false ->
							% Same sign for increment, hence same direction,
							% again exponential:
							%
							search_first_null( Fun, Pivot, MaybeMax, 2.0 * Inc,
								RemainingTests-1, Epsilon )

					end

			end;

		false ->
			% We consider it as a zero (beginning of the open interval on which
			% the function is not defined):
			%
			% (it is necessarily not 'undefined')
			%
			MaybeMax

	end.



% @doc Tells whether the specified value lies within the specified maybe-bounds
% (included).
%
-spec is_within( float(), maybe( bound() ), maybe( bound() ) ) -> boolean().
is_within( _V, _MaybeMin=undefined, _MaybeMax=undefined ) ->
	true;

is_within( V, _MaybeMin=undefined, Max ) when V =< Max ->
	true;

is_within( V, Min, _MaybeMax=undefined ) when V >= Min ->
	true;

is_within( V, Min, Max ) when V >= Min andalso V =< Max ->
	true;

is_within( _V, _Min, _Max ) ->
	false.



% @doc Tells whether the specified value lies "before" (closer to the opposite
% end, based on the sign of the increment) the specified maybe-maximum.
%
-spec is_before( float(), maybe( bound() ), increment() ) -> boolean().
is_before( _V, _MaybeMax=undefined, _Inc ) ->
	true;

is_before( V, Max, Inc ) when Inc >= 0 andalso V =< Max ->
	true;

is_before( V, Max, Inc ) when Inc < 0 andalso V >= Max ->
	true;

is_before( _V, _Max, _Inc ) ->
	false.





% Tries to find, starting from the (non-zero) pivot, a zero closer (to the
% pivot) than the specified one.
%
% Proceeds by dichotomy: starting from the pivot and a farther zero, tests the
% midpoint: if it seems to be a "permanent zero", then recurses with it as new
% farther zero; otherwise, use it as a new pivot. Each iteration will halve the
% search range, until it gets negligible.
%
% Note that FartherZero is not necessarily on the right of the pivot.
%
% No bounds checked, as expected to remain within an already validated interval.
%
-spec minimise_zero( float_to_float_fun(), abscissa(), abscissa(),
			maybe( bound() ), maybe( bound() ), epsilon() ) -> abscissa().
minimise_zero( Fun, Pivot, FartherZero, MaybeMin, MaybeMax, Epsilon ) ->
	case are_close( Pivot, FartherZero, Epsilon ) of

		% This is the ending criterion:
		true ->
			% Before returning our elected zero, we check it a bit more:
			Inc = ?base_increment * sign( FartherZero - Pivot ),

			% Searches a non-zero away (to the right infinite) from our zero:
			case search_non_null_one_direction( Fun, FartherZero, MaybeMin,
					MaybeMax, Inc, _RemainingTests=64, Epsilon ) of

				% Validated:
				undefined ->
					FartherZero;

				UnexpectedZero ->
					trace_utils:warning_fmt(
						"Outlier zero found at ~f, reconverging.",
						[ UnexpectedZero ] ),
					minimise_zero( Fun, Pivot, UnexpectedZero,
								   MaybeMin, MaybeMax, Epsilon )

			end;


		% Still having to narrow down:
		false ->
			Midpoint = ( Pivot + FartherZero ) / 2.0,

			cond_utils:if_defined( myriad_debug_math,
				trace_utils:debug_fmt( "Testing midpoint ~f.", [ Midpoint ] ) ),

			case evaluate( Fun, Midpoint ) of

				undefined ->
					% Not knowing how to handle:
					throw( { undefined_midpoint, Midpoint, Pivot,
							 FartherZero } );

				MidValue ->

					case is_null( MidValue, Epsilon ) of

						true ->
							% A check, should we have found only a local zero,
							% with non-null values present farther away:

							Inc = ?coarse_increment
								* sign( FartherZero - Pivot ),

							cond_utils:if_defined( myriad_debug_math,
								trace_utils:debug_fmt( "Midpoint ~f seems to "
									"be a good candidate for closest zero.",
									[ Midpoint ] ) ),

							% Just a light, raw, security operating on an
							% intermediary candidate:
							%
							case search_non_null_one_direction( Fun, Midpoint,
								MaybeMin, MaybeMax, Inc, _RemainingTests=24,
								Epsilon ) of

							% Midpoint looks like a good zero then, let's use it
							% from now:
							%
							undefined ->
								minimise_zero( Fun, Pivot, Midpoint,
											   MaybeMin, MaybeMax, Epsilon );

							% It was not a "permanent" zero, let's offset the
							% pivot to this farther position:
							%
							NewPivot ->
								minimise_zero( Fun, NewPivot, FartherZero,
											   MaybeMin, MaybeMax, Epsilon )

							end;

						false ->
							% Not a zero at all, so it becomes the new pivot:
							minimise_zero( Fun, Midpoint, FartherZero,
										   MaybeMin, MaybeMax, Epsilon )

					end

			end

	end.



% @doc Searches a non-zero function value in a single direction (as opposed to
% search_non_null/5), determined by the sign of the increment.
%
-spec search_non_null_one_direction( float_to_float_fun(), abscissa(),
		maybe( bound() ), maybe( bound() ), increment(), count(), epsilon() ) ->
								maybe( abscissa() ).
search_non_null_one_direction( _Fun, Origin, MaybeMin, MaybeMax, Inc,
							   _RemainingTests=0, _Epsilon ) ->

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:debug_fmt( "Only zeros found from one side of ~f, "
			"last increment being ~f (min: ~w, max: ~w).",
			[ Origin, Inc, MaybeMin, MaybeMax ] ),
		basic_utils:ignore_unused( [ Origin, Inc, MaybeMin, MaybeMax ] ) ),

	undefined;

search_non_null_one_direction( Fun, Origin, MaybeMin, MaybeMax, Inc,
							   RemainingTests, Epsilon ) ->

	TestedPoint = Origin + Inc,

	case is_within( TestedPoint, MaybeMin, MaybeMax ) of

		true ->

			%trace_utils:debug_fmt( "Searching for a non-null point "
			%   "at abscissa ~w.", [ TestedPoint ] ),

			case evaluate( Fun, TestedPoint ) of

				undefined ->
					% If ever useful:
					Factor = 1.2,
					search_non_null_one_direction( Fun, Origin, MaybeMin,
						MaybeMax, Factor * Inc, RemainingTests-1, Epsilon );

				Value ->
					case is_null( Value, Epsilon ) of

						true ->
							% Hence growing exponentially:
							Factor = 1.2,
							search_non_null_one_direction( Fun, Origin,
								MaybeMin, MaybeMax, Factor * Inc,
								RemainingTests-1, Epsilon );

						false ->
							TestedPoint

					end
			end;

		false ->
			% Stopping here, as already outside of allowed bounds:
			undefined

	end.





% @doc Returns an evaluation of the (supposedly finite, compact, to some extent
% centered around zero) support of the specified function, taking one integer
% and returning a floating-point value, as an interval with no bound
% restriction.
%
% This simple heuristics assumes that this function vanishes at infinity, and
% starts looking around zero.
%
% Typically useful to properly discretise probability density functions.
%
-spec compute_integer_support( integer_to_float_fun() ) -> integer_bounds().
compute_integer_support( Fun ) ->
	compute_integer_support( Fun, _MaybeMin=undefined, _MaybeMax=undefined ).


% @doc Returns an evaluation of the (supposedly finite, compact, to some extent
% centered around zero) support of the specified integer-returning function, as
% an interval complying to the specified bound restriction (to limit it in a
% given interval, possibly because this function is not defined out of it).
%
% This simple heuristics assumes that this function vanishes at infinity, and
% starts looking around zero.
%%
% Typically useful to properly discretise probability density functions.
%
-spec compute_integer_support( integer_to_float_fun(), maybe( integer_bound() ),
								maybe( integer_bound() ) ) -> integer_bounds().
compute_integer_support( Fun, MaybeMin=undefined, MaybeMax=undefined ) ->
	compute_integer_support( Fun, _Origin=0, MaybeMin, MaybeMax );

compute_integer_support( Fun, MaybeMin=undefined, Max ) ->
	Origin = Max - abs( Max div 2 ),
	compute_integer_support( Fun, Origin, MaybeMin, Max );

compute_integer_support( Fun, Min, MaybeMax=undefined ) ->
	Origin = Min + abs( Min div 2 ),
	compute_integer_support( Fun, Origin, Min, MaybeMax );

compute_integer_support( Fun, Min, Max ) ->
	% To support floats (and will be rounded):
	Origin = ( Min + Max ) / 2,
	compute_integer_support( Fun, Origin, Min, Max ).



% @doc Returns an evaluation of the (supposedly finite, compact, to some extent
% centered around the specified origin) support of the specified
% integer-returning function, as an interval complying to the specified bound
% restriction (to limit it in a given interval, possibly because this function
% is not defined out of it).
%
% This simple heuristics assumes that this function vanishes at infinity, and
% starts looking around zero.
%%
% Typically useful to properly discretise probability density functions.
%
-spec compute_integer_support( integer_to_float_fun(), any_abscissa(),
	maybe( integer_bound() ), maybe( integer_bound() ) ) -> integer_bounds().
compute_integer_support( Fun, AnyOrigin, MaybeMin, MaybeMax ) ->

	% Integer wanted in all cases:
	Origin = round( AnyOrigin ),

	% Tests are limited, should for example x -> sin(x) be specified:
	%
	% (lower test counts, yet multiple ones attempted)
	%
	Bounds = compute_integer_support( Fun, Origin, MaybeMin, MaybeMax,
		_Increment=1, _RemainingTests=64, _Epsilon=?epsilon ),

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:debug_fmt( "Returning integer support ~ts.",
			[ integer_bounds_to_string( Bounds ) ] ) ),

	Bounds.



% Searches the function support from the specified origin.
%
% (helper)
%
-spec compute_integer_support( integer_to_float_fun(), integer_abscissa(),
		maybe( integer_bound() ), maybe( integer_bound() ), integer_increment(),
		count(), epsilon() ) -> bounds().
compute_integer_support( Fun, Origin, MaybeMin, MaybeMax, Inc, RemainingTests,
						 Epsilon ) ->

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "Computing the integer support of ~p "
			"from origin ~B, with following restrictions: min=~w, max=~w.",
			[ Fun, Origin, MaybeMin, MaybeMax ] ) ),

	% Searches first a non-null point, the "pivot", which is the first non-null
	% point found around the origin, looking alternatively on each side based on
	% exponentially-increasing distance:
	%
	Pivot = case search_integer_non_null( Fun, Origin, MaybeMin, MaybeMax, Inc,
										  RemainingTests, Epsilon ) of

		undefined ->
			%case search_integer_additive_non_null(
			throw( { no_non_null_point_found, Fun } );

		P ->
			P

	end,

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "Pivot found at abscissa ~B.", [ Pivot ] ) ),

	% Now tries to find from the pivot the right end of the support, first by
	% quickly converging to any remote zero:
	%
	FirstRightZero = search_integer_first_null( Fun, Pivot, MaybeMax,
		_RightwiseInc=1, RemainingTests, Epsilon ),

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "First right zero found at abscissa ~B.",
							  [ FirstRightZero ] ) ),

	MinRightZero = minimise_integer_zero( Fun, Pivot, FirstRightZero,
										  MaybeMin, MaybeMax, Epsilon ),

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "Right zero minimised at abscissa ~B, "
			"now looking for left zeros.", [ MinRightZero ] ) ),

	% To find the left end of the support (hence opposite direction):
	FirstLeftZero = search_integer_first_null( Fun, Pivot, MaybeMin,
		_LeftwiseInc=-1, RemainingTests, Epsilon ),

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "First left zero found at abscissa ~B.",
							  [ FirstLeftZero ] ) ),

	% Minimised in terms of absolute value (hence closer to the origin):
	MaxLeftZero = minimise_integer_zero( Fun, Pivot, FirstLeftZero,
										 MaybeMin, MaybeMax, Epsilon ),

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "Left zero maximised at abscissa ~B.",
							  [ MaxLeftZero ] ) ),

	B = { MaxLeftZero, MinRightZero },

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:info_fmt( "Returning following support: ~ts.",
							  [ math_utils:integer_bounds_to_string( B ) ] ) ),

	B.



% Explores exponentially both sides alternatively of the specified origin,
% looking for any non-null value.
%
% (helper)
-spec search_integer_non_null( integer_to_float_fun(), integer_abscissa(),
		maybe( bound() ), maybe( bound() ), integer_increment(), count(),
		epsilon() ) -> maybe( integer_abscissa() ).
% Search failed:
search_integer_non_null( Fun, Origin, MaybeMin, MaybeMax, Inc,
						 _RemainingTests=0, _Epsilon ) ->

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:debug_fmt( "The search for a non-zero of ~p on both sides "
			"of ~B failed, whereas last increment was ~B (min: ~w, max: ~w).",
			[ Fun, Origin, Inc, MaybeMin, MaybeMax ] ),
		basic_utils:ignore_unused( [ Fun, Origin, Inc, MaybeMin, MaybeMax ] ) ),

	undefined;


search_integer_non_null( Fun, Origin, MaybeMin, MaybeMax, Inc, RemainingTests,
						 Epsilon ) ->

	TestedPoint = Origin + Inc,

	% Hence growing exponentially, on either side alternatively:
	Factor = case Inc >= 0 of

		true ->
			-1;

		false ->
			-2

	end,

	case is_within( TestedPoint, MaybeMin, MaybeMax ) of

		true ->
			case evaluate( Fun, TestedPoint ) of

				undefined ->
					% Possibly useful:
					search_integer_non_null( Fun, Origin, MaybeMin, MaybeMax,
						Factor * Inc, RemainingTests-1, Epsilon );

				Value ->
					cond_utils:if_defined( myriad_debug_math,
						trace_utils:debug_fmt( "Searching for a non-null point "
							"at abscissa ~B, got ~w.",
							[ TestedPoint, Value ] ) ),

					case is_null( Value, Epsilon ) of

						true ->
							search_integer_non_null( Fun, Origin,
								MaybeMin, MaybeMax, Factor * Inc,
								RemainingTests-1, Epsilon );

						false ->
							TestedPoint

					end

			end;

		false ->
			% No test spent here, on this side:
			search_integer_non_null( Fun, Origin, MaybeMin, MaybeMax,
									 Factor * Inc, RemainingTests, Epsilon )

	end.



% Searches by dichotomy in one direction (defined by the sign of the increment)
% the first null point, for which the next ones being are supposed to be all
% null, found from the specified origin (whose value is expected to be
% non-null).
%
-spec search_integer_first_null( integer_to_float_fun(), integer_abscissa(),
			maybe( bound() ), integer_increment(), count(), epsilon() ) ->
				integer_abscissa().
search_integer_first_null( Fun, Pivot, MaybeMax, Inc, _RemainingTests=0,
						   _Epsilon ) ->

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:debug_fmt( "The search for a first zero of ~p "
			"from pivot ~B failed, whereas last increment was ~B (max: ~w).",
			[ Fun, Pivot, Inc, MaybeMax ] ),
		basic_utils:ignore_unused( [ Fun, Pivot, Inc, MaybeMax ] ) ),

	throw( { no_null_point_found, Fun, Pivot, MaybeMax } );

search_integer_first_null( Fun, Pivot, MaybeMax, Inc, RemainingTests,
						   Epsilon ) ->

	TestedPoint = Pivot + Inc,

	case is_before( TestedPoint, MaybeMax, Inc ) of

		true ->
			cond_utils:if_defined( myriad_debug_math,
				trace_utils:debug_fmt( "Testing point ~B as potential zero.",
									   [ TestedPoint ] ) ),

			case evaluate( Fun, TestedPoint ) of

				undefined ->
					search_integer_first_null( Fun, Pivot, MaybeMax, 2*Inc,
											   RemainingTests-1, Epsilon );

				Value ->
					case is_null( Value, Epsilon ) of

						true ->
							cond_utils:if_defined( myriad_debug_math,
								trace_utils:debug_fmt(
									"Point ~B is a zero (got ~w).",
									[ TestedPoint, Value ] ) ),

							TestInc = sign( Inc ),

							% As we do not want to be fooled by an only-local
							% zero:
							%
							case search_integer_non_null_one_direction( Fun,
								TestedPoint, _MaybeMin=undefined, MaybeMax,
								TestInc, _RemTests=48, Epsilon ) of

								undefined ->
									% OK, confirmed (a relevant trace already
									% emitted):
									%trace_utils:debug_fmt(
									%"Point ~B is considered as a zero onward.",
									%   [ TestedPoint ] ),
									TestedPoint;

								FartherNonZero ->
									trace_utils:warning_fmt( "A non-zero "
										"farther than the current zero "
										"candidate ~B has been "
										"detected: ~B; recalibrating.",
										[ TestedPoint, FartherNonZero ] ),
									search_integer_first_null( Fun,
										FartherNonZero,	MaybeMax, Inc,
										_NewTest=48, Epsilon )

							end;

						false ->
							% Same sign for increment, hence same direction,
							% again exponential:
							%
							search_integer_first_null( Fun, Pivot, MaybeMax,
								2*Inc, RemainingTests-1, Epsilon )

					end

				end;

		false ->
			% We consider it as a zero (beginning of the open interval on which
			% the function is not defined):
			%
			% (it is necessarily not 'undefined')
			%
			MaybeMax

	end.



% @doc Searches a non-zero function value in a single direction (as opposed to
% search_non_null/5), determined by the sign of the increment.
%
-spec search_integer_non_null_one_direction( integer_to_float_fun(),
		integer_abscissa(), maybe( bound() ), maybe( bound() ),
		integer_increment(), count(), epsilon() ) ->
			maybe( integer_abscissa() ).
search_integer_non_null_one_direction( _Fun, Origin, MaybeMin, MaybeMax, Inc,
									   _RemainingTests=0, _Epsilon ) ->

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:debug_fmt( "Only zeros found from one side of ~B, "
			"last increment being ~B (min: ~w, max: ~w).",
			[ Origin, Inc, MaybeMin, MaybeMax ] ),
		basic_utils:ignore_unused( [ Origin, Inc, MaybeMin, MaybeMax ] ) ),

	undefined;

search_integer_non_null_one_direction( Fun, Origin, MaybeMin, MaybeMax, Inc,
									   RemainingTests, Epsilon ) ->

	TestedPoint = Origin + Inc,

	case is_within( TestedPoint, MaybeMin, MaybeMax ) of

		true ->

			%trace_utils:debug_fmt( "Searching for a non-null point "
			%   "at abscissa ~B.", [ TestedPoint ] ),

			case evaluate( Fun, TestedPoint ) of

				undefined ->
					search_integer_non_null_one_direction( Fun, Origin,
						MaybeMin, MaybeMax, 2*Inc, RemainingTests-1, Epsilon );

				Value ->
					case is_null( Value, Epsilon ) of

						true ->
							% Hence growing exponentially:
							search_integer_non_null_one_direction( Fun, Origin,
								MaybeMin, MaybeMax, 2*Inc, RemainingTests-1,
								Epsilon );

						false ->
							TestedPoint

					end

			end;

		false ->
			% Stopping here, as already outside of allowed bounds:
			undefined

	end.



% Tries to find, starting from the (non-zero) pivot, a zero closer (to the
% pivot) than the specified one.
%
% Proceeds by dichotomy: starting from the pivot and a farther zero, tests the
% midpoint: if it seems to be a "permanent zero", then recurses with it as new
% farther zero; otherwise, use it as a new pivot. Each iteration will halve the
% search range, until it gets negligible.
%
% Note that FartherZero is not necessarily on the right of the pivot.
%
% No bounds checked, as expected to remain within an already validated interval.
%
-spec minimise_integer_zero( integer_to_float_fun(), integer_abscissa(),
		integer_abscissa(), maybe( bound() ), maybe( bound() ), epsilon() ) ->
			integer_abscissa().
% Finished, this is the ending criterion, the search range is now mostly empty:
minimise_integer_zero( Fun, Pivot, FartherZero, MaybeMin, MaybeMax,
					   Epsilon ) when abs( Pivot - FartherZero ) =< 1 ->

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:debug_fmt( "Integer zero minimised: pivot is ~B, "
			"farther zero is ~B.", [ Pivot, FartherZero ] ) ),

	% Before returning our elected zero, we check it a bit more:
	Inc = sign( FartherZero - Pivot ),

	% Searches a non-zero away (to the right infinite) from our zero:
	case search_integer_non_null_one_direction( Fun, FartherZero, MaybeMin,
			MaybeMax, Inc, _RemainingTests=64, Epsilon ) of

		% Validated:
		undefined ->
			Pivot;

		UnexpectedZero ->
			trace_utils:warning_fmt( "Outlier zero found at ~B "
				"(from pivot ~B), reconverging.", [ UnexpectedZero, Pivot ] ),
			minimise_integer_zero( Fun, Pivot, UnexpectedZero,
								   MaybeMin, MaybeMax, Epsilon )

	end;


% Still having to narrow down:
minimise_integer_zero( Fun, Pivot, FartherZero, MaybeMin, MaybeMax, Epsilon ) ->

	Midpoint = ( Pivot + FartherZero ) div 2,

	cond_utils:if_defined( myriad_debug_math,
		trace_utils:debug_fmt( "Testing midpoint ~B (pivot: ~B, zero: ~B).",
							   [ Midpoint, Pivot, FartherZero ] ) ),

	case evaluate( Fun, Midpoint ) of

		undefined ->
			% Not knowing how to handle:
			throw( { undefined_midpoint, Midpoint, Pivot, FartherZero } );

		MidValue ->

			case is_null( MidValue, Epsilon ) of

				true ->
					% A check, should we have found only a local zero, with
					% non-null values present farther away:

					Inc = sign( FartherZero - Pivot ),

					cond_utils:if_defined( myriad_debug_math,
						trace_utils:debug_fmt( "Midpoint ~B seems to be a good "
							"candidate for closest zero.", [ Midpoint ] ) ),

					% Just a light, raw, security operating on an intermediary
					% candidate:
					%
					case search_integer_non_null_one_direction( Fun, Midpoint,
						MaybeMin, MaybeMax, Inc, _RemainingTests=24,
						Epsilon ) of

						% Midpoint looks like a good zero then, let's use it
						% from now:
						%
						undefined ->
							minimise_integer_zero( Fun, Pivot, Midpoint,
								MaybeMin, MaybeMax, Epsilon );

						% It was not a "permanent" zero, let's offset the pivot
						% to this farther position:
						%
						NewPivot ->
							minimise_integer_zero( Fun, NewPivot, FartherZero,
												   MaybeMin, MaybeMax, Epsilon )

					end;

				false ->
					% Not a zero at all, so it becomes the new pivot:
					minimise_integer_zero( Fun, Midpoint, FartherZero,
						MaybeMin, MaybeMax, Epsilon )

			end

	end.



% @doc Checks that the specified term corresponds to correct (floating-point)
% bounds, and returns it once canonicalised.
%
-spec canonicalise_bounds( term() ) -> bounds().
canonicalise_bounds( B={ MinBound, MaxBound } ) ->
	MinBoundf = float( MinBound ),
	MaxBoundf = float( MaxBound ),
	case MinBoundf =< MaxBoundf of

		true ->
			{ MinBoundf, MaxBoundf };

		false ->
			throw( { bound_mismatch, B } )

	end.



% @doc Checks that the specified term corresponds to correct bounds, and returns
% it once canonicalised.
%
-spec canonicalise_integer_bounds( term() ) -> integer_bounds().
canonicalise_integer_bounds( B={ MinBound, MaxBound } )
		when is_integer( MinBound ) andalso is_integer( MaxBound )
			 andalso MinBound =< MaxBound ->
	B;

canonicalise_integer_bounds( B ) ->
	throw( { invalid_integer_bounds, B } ).



% @doc Tells whether the specified number is within (is included in;
% non-strictlyy, that is this number being allowed to be equal to one bound) the
% specified bounds.
%
-spec is_within_bounds( number(), bounds() ) -> boolean().
is_within_bounds( Number, _Bounds={ Min, Max } )
		when Number >= Min andalso Number =< Max ->
	true;

is_within_bounds( _Number, _Bounds ) ->
	false.


% @doc Tells whether the specified candidate bounds are within (are included in;
% non-strictly, that is bounds being allowed to be equal) the full ones.
%
% Note: one should ensure that well-typed bounds are indeed specified.
%
-spec are_within_bounds( bounds(), bounds() ) -> boolean().
are_within_bounds( _CandidateBounds={ CMin, CMax },
				   _FullBounds={ FMin, FMax } ) when CMin >= FMin andalso
													 CMax =< FMax ->
	true;

are_within_bounds( _CandidateBounds, _FullBounds ) ->
	false.



% @doc Returns a textual description of the specified bounds.
-spec bounds_to_string( bounds() ) -> ustring().
bounds_to_string( _Bounds={ Min, Max } ) ->
	text_utils:format( "[~f, ~f]", [ Min, Max ] ).


% @doc Returns a textual description of the specified integer bounds.
-spec integer_bounds_to_string( bounds() ) -> ustring().
integer_bounds_to_string( _Bounds={ Min, Max } ) ->
	text_utils:format( "[~B, ~B]", [ Min, Max ] ).




% @doc Returns a textual description of the specified probability.
-spec probability_to_string( probability() ) -> ustring().
probability_to_string( Probability ) ->
	% Only one significant number after the comma for readibility:
	text_utils:format( "probability of about ~.1f%", [ 100 * Probability ] ).



% @doc Returns a textual description of the specified probability.
-spec probability_like_to_string( probability_like() ) -> ustring().
probability_like_to_string( ProbLike ) when is_float( ProbLike )
		andalso ProbLike >= 0.0 andalso ProbLike =< 1.0 ->
	% Supposedly normalised then:
	probability_to_string( ProbLike );

probability_like_to_string( ProbLike ) ->
	text_utils:format( "non-normalised probability of ~w", [ ProbLike ] ).


% @doc Checks that the specified percentage is in the usual [0.0, 1.0] range.
%
% Returns that percentage.
%
-spec check_percent_basic_range( percent() ) -> percent().
check_percent_basic_range( P ) when P >= 0.0 andalso P =< 1.0 ->
	P;

check_percent_basic_range( P ) ->
	throw( { percentage_not_in_range, P } ).




% Section for special functions.


% @doc Returns the factorial of the argument N, that is: N!.
-spec factorial( non_neg_integer() ) -> pos_integer().
factorial( _N=0 ) ->
	1;

factorial( N ) when N > 0 ->
	N * factorial( N-1 ).



% @doc Evaluates the well-known Gamma function for the specified value (integer
% or floating-point: no need felt for one operating on complex numbers).
%
%
%              +infinity
%             /
% gamma (z) = | t^(z-1).exp (-t).dt
%            /
%         t=0
%
% Exact results are returned for integer values, approximated ones for
% floating-point ones.
%
% Refer to https://en.wikipedia.org/wiki/Gamma_function for more information.
%
-spec gamma( pos_integer() ) -> pos_integer();
		   ( float() ) -> float().
gamma( I ) when is_integer( I ) andalso I > 0 ->
	factorial( I-1 );

gamma( F ) when is_float( F ) andalso F > 0 ->
	% We rely here on the Lanczos approximation (see
	% https://en.wikipedia.org/wiki/Lanczos_approximation):

	G = 7,

	% Not used:
	%N = 9,

	P = [ 0.99999999999980993, 676.5203681218851, -1259.1392167224028,
		  771.32342877765313, -176.61502916214059, 12.507343278686905,
		  -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7 ],

	gamma( F, G, P );

gamma( Other ) ->
	throw( { invalid_argument, Other } ).



% (helper)
gamma( F, G, P ) when F < 0.5 ->

	% Using the reflection formula, as the Lanczos method would not be valid
	% for Re(F) < 0.5 :
	%
	Pi = math:pi(),
	Pi / ( math:sin( Pi * F ) * gamma( 1-F, G, P ) );


gamma( F, G, _P=[ P0 | PT ] ) -> % when F >= 0.5

	% F corresponds to the "z" variable in the algorithm.

	OffsetF = F - 1,

	% V corresponds to the "x" variable in the algorithm.
	V = add_gamma( PT, OffsetF, _Inc=1, _AccV=P0 ),

	T = OffsetF + G + 0.5,

	math:sqrt( 2*math:pi() ) * math:pow( T, OffsetF + 0.5 )
		* math:exp( -T ) * V.



% (helper)
add_gamma( _P=[], _OffsetF, _Inc, AccV ) ->
	AccV;

add_gamma( _P=[ H | T ], OffsetF, Inc, AccV ) ->
	NewAccV = AccV + H / ( OffsetF + Inc),
	add_gamma( T, OffsetF, Inc+1, NewAccV ).
