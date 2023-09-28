% Copyright (C) 2008-2023 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: 2008.


% @doc Unit test for the RandomManager class implementation, regarding the
% <b>exponential distribution</b>.
%
% See the class_RandomManager.erl module.
%
-module(class_RandomManager_Exponential_test).


% For facilities common to all tests (this is not a simulation case):
-include("traces_for_tests.hrl").


% For random_manager_name:
-include("class_RandomManager.hrl").


-define( table_span, 70 ).



show_exponential( RandomManagerPid, Lambda ) ->

	?test_debug_fmt( "Requesting an exponential random value with lambda = ~p.",
					 [ Lambda ] ),

	RandomManagerPid ! { getExponentialValue, Lambda, self() },

	receive

		{ wooper_result, { exponential_value, Value } } ->
			?test_debug_fmt( "Received exponential random value: ~p.",
							 [ Value ] )

	end.



draw_exponential_values( _Count=0, Table, _Lambda, _RandomManagerPid ) ->
	Table;

draw_exponential_values( Count, Table, Lambda, RandomManagerPid ) ->
	RandomManagerPid ! { getPositiveIntegerExponentialValue, Lambda, self() },

	% Wanting a random value in ]1, ?table_span]:
	receive

		{ wooper_result, { positive_integer_exponential_1p_value, Value } }
				when Value > ?table_span ; Value =< 0 ->
			draw_exponential_values( Count, Table, Lambda, RandomManagerPid );

		{ wooper_result, { positive_integer_exponential_1p_value, Value } } ->
			NewCount = element( Value, Table ) + 1,
			draw_exponential_values( Count - 1,
				setelement( Value, Table, NewCount ), Lambda, RandomManagerPid )

	end.



% At index V there is the number of times V has been drawn.
make_table( Size ) ->
	erlang:make_tuple( Size, 0 ).


send_tables( FirstTable, SecondTable, ThirdTable, FourthTable, FifthTable,
			 ProbePid ) ->
	send_tables( FirstTable, SecondTable, ThirdTable, FourthTable, FifthTable,
				 ProbePid, _Count=1 ).


send_tables( _FirstTable, _SecondTable, _ThirdTable, _FourthTable, _FifthTable,
			 _ProbePid, _Count=?table_span + 1 ) ->
	ok;

send_tables( FirstTable, SecondTable, ThirdTable, FourthTable, FifthTable,
			 ProbePid, Count ) ->

	ProbePid ! { setData, [ Count, { element( Count, FirstTable ),
		element( Count, SecondTable ),  element( Count, ThirdTable ),
		element( Count, FourthTable ), element( Count, FifthTable ) } ] },

	send_tables( FirstTable, SecondTable, ThirdTable, FourthTable, FifthTable,
				 ProbePid, Count+1 ).



compute_mean( Table ) ->

	List = tuple_to_list( Table ),

	% Multiply the number of draws by the drawn value:
	% (hope the sum is not zero! Starting at index 1)
	%
	compute_mean( List, 1, 0 ) / compute_sum( List, 0 ).


% Counts the number of draws.
compute_sum( [], Count ) ->
	Count;

compute_sum( [ H | T ], Count ) ->
	compute_sum( T, Count + H ).


% Counts the sum of draws.
compute_mean( [], _Index, Acc ) ->
	Acc;

compute_mean( [ H | T ], Index, Acc ) ->
	compute_mean( T, Index + 1, Acc + H * Index ).



test_exponential_random( RandomManagerPid, Lambda ) ->

	?test_info( "Requesting exponential random values." ),

	show_exponential( RandomManagerPid, Lambda ),
	show_exponential( RandomManagerPid, Lambda ),
	show_exponential( RandomManagerPid, Lambda ),
	show_exponential( RandomManagerPid, Lambda ),
	show_exponential( RandomManagerPid, Lambda ),

	?test_info( "Computing and displaying the full actual "
				"exponential distribution." ),

	?test_warning( "As a large number of samples will be computed, the "
				   "operation may take some time." ),

	Values = make_table( ?table_span ),

	FirstExponentialTable = draw_exponential_values( 500, Values, Lambda,
													 RandomManagerPid ),

	SecondExponentialTable = draw_exponential_values( 5000-500,
							FirstExponentialTable, Lambda, RandomManagerPid ),

	ThirdExponentialTable = draw_exponential_values( 50000-5000,
							SecondExponentialTable, Lambda, RandomManagerPid ),

	FourthExponentialTable = draw_exponential_values( 500000-50000,
							ThirdExponentialTable, Lambda, RandomManagerPid ),

	FifthExponentialTable = draw_exponential_values( 5000000-500000,
							FourthExponentialTable, Lambda, RandomManagerPid ),

	Mean = compute_mean( FifthExponentialTable ),

	?test_notice_fmt( "Mean of this full actual exponential distribution "
					  "is ~p.", [ Mean ] ),

	MyExponentialProbe = class_Probe:create_facility_probe(

		_Title="Exponential probe",

		_Curves=[ "After 500 draws", "After 5000 draws", "After 50000 draws",
				  "After 500000 draws", "After 5000000 draws" ],

		_Zones=[],

		text_utils:format( "Test of the generation of exponential random "
						   "distributions with lambda = ~p.", [ Lambda ] ),

		text_utils:format( "Drawn values (mean value is ~p, "
						   "expected to be ~p)", [ Mean, 1 / Lambda ] ),

		"Number of times a value has been drawn" ),

	send_tables( FirstExponentialTable, SecondExponentialTable,
		ThirdExponentialTable, FourthExponentialTable, FifthExponentialTable,
		MyExponentialProbe ),

	?test_info( "Requesting the generation of exponential probe report." ),

	class_Probe:generate_report_for( MyExponentialProbe ),

	class_Probe:delete_facility_probe( MyExponentialProbe ).



% @doc Runs the tests, no prior RandomManager expected to be alive.
-spec run() -> no_return().
run() ->

	?test_start,

	class_ResultManager:create_mockup_environment(),

	?test_info( "Creating a random manager." ),
	class_RandomManager:create(),

	RandomManagerPid =
		naming_utils:wait_for_global_registration_of( ?random_manager_name ),

	Lambda = 0.1,
	test_exponential_random( RandomManagerPid, Lambda ),

	?test_info( "Removing random manager." ),
	wooper:delete_synchronously_instance( RandomManagerPid ),

	?test_stop.
