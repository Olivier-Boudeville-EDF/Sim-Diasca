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


% Unit tests for the basic utils toolbox.
%
% See the basic_utils.erl tested module.
%
-module(basic_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For myriad_spawn*:
-include("spawn_utils.hrl").



-spec check_process_specific_values( integer(), integer() ) -> void().
check_process_specific_values( Min, Max ) ->

	Self = self(),

	F = fun() ->
			Self ! basic_utils:get_process_specific_value( Min, Max )
		end,

	[ spawn( F ) || _X <- lists:seq( 1, 10 ) ],

	G = fun() ->
			receive V ->
					V
			end
		end,

	[ test_facilities:display(
				"Generating a process-specific value in [~B;~B[: ~p.",
				[ Min, Max, G() ] ) || _Y <- lists:seq( 1, 10 ) ].



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the display of a static test message." ),

	test_facilities:display( "Testing the display of a ~ts test message.",
							 [ dynamic ] ),

	basic_utils:checkpoint( 1 ),

	basic_utils:checkpoint( 2 ),

	basic_utils:display( "standalone normal display" ),

	basic_utils:display( "normal display ~ts", [ "with a format string" ] ),

	basic_utils:display_error( "standalone error display" ),

	basic_utils:display_error( "error display ~ts",
							   [ "with a format string" ] ),

	FirstVersion  = { 0, 0, 0 },
	SecondVersion = { 0, 0, 1 },
	ThirdVersion  = { 0, 1, 0 },
	FourthVersion = { 1, 0, 0 },
	FifthVersion  = { 1, 1, 1 },

	first_bigger = basic_utils:compare_versions( SecondVersion, FirstVersion ),
	first_bigger = basic_utils:compare_versions( ThirdVersion, SecondVersion ),
	first_bigger = basic_utils:compare_versions( FifthVersion, FirstVersion ),

	second_bigger = basic_utils:compare_versions( FirstVersion, FourthVersion ),
	second_bigger = basic_utils:compare_versions( ThirdVersion, FourthVersion ),
	second_bigger = basic_utils:compare_versions( SecondVersion, ThirdVersion ),

	equal = basic_utils:compare_versions( FirstVersion, FirstVersion ),
	equal = basic_utils:compare_versions( ThirdVersion, ThirdVersion ),
	equal = basic_utils:compare_versions( FifthVersion, FifthVersion ),

	test_facilities:display( "Comparisons of versions like ~ts succeeded.",
							 [ text_utils:version_to_string( ThirdVersion ) ] ),


	FirstShortVersion  = { 0, 0 },
	SecondShortVersion = { 0, 1 },
	ThirdShortVersion  = { 1, 0 },

	first_bigger = basic_utils:compare_versions( SecondShortVersion,
												 FirstShortVersion ),

	first_bigger = basic_utils:compare_versions( ThirdShortVersion,
												 SecondShortVersion ),

	first_bigger = basic_utils:compare_versions( ThirdShortVersion,
												 FirstShortVersion ),


	second_bigger = basic_utils:compare_versions( FirstShortVersion,
												  SecondShortVersion ),

	second_bigger = basic_utils:compare_versions( SecondShortVersion,
												  ThirdShortVersion ),

	second_bigger = basic_utils:compare_versions( FirstShortVersion,
												  ThirdShortVersion ),


	equal = basic_utils:compare_versions( FirstShortVersion,
										  FirstShortVersion ),

	equal = basic_utils:compare_versions( SecondShortVersion,
										  SecondShortVersion ),

	equal = basic_utils:compare_versions( ThirdShortVersion,
										  ThirdShortVersion ),


	test_facilities:display( "Comparisons of versions like ~ts succeeded.",
		[ text_utils:version_to_string( ThirdVersion ) ] ),


	{ 4, 22, 11 } = basic_utils:parse_version( "4.22.11" ),

	test_facilities:display( "Generating a process-specific value: ~w.",
							 [ basic_utils:get_process_specific_value() ] ),

	{ Min, Max } = { 3, 16 },
	check_process_specific_values( Min, Max ),

	basic_utils:display_process_info( self() ),


	PSize = basic_utils:get_process_size( self() ),

	test_facilities:display( "Size of current process: ~B bytes, i.e. ~ts.",
		[ PSize, system_utils:interpret_byte_size( PSize ) ] ),


	Self = self(),

	test_facilities:display( "Testing myriad_spawn, based on a ~ts.",
							 [ ?myriad_spawn_info ] ),

	?myriad_spawn( fun() ->

					   % Closure:
					   Self ! myriad_spawned

				   end ),

	receive

		myriad_spawned ->
			ok

	end,


	test_facilities:display( "Testing myriad_spawn_link, based on a ~ts.",
							 [ ?myriad_spawn_info ] ),

	?myriad_spawn_link( fun() ->

					   % Closure:
					   Self ! myriad_spawned_linked

						end ),

	receive

		myriad_spawned_linked ->
			ok

	end,


	test_facilities:display( "This test was compiled with the execution target "
		"set to '~ts', and debug mode is ~ts.",
		[ basic_utils:get_execution_target(),
		  basic_utils:is_debug_mode_enabled() ] ),

	test_facilities:stop().
