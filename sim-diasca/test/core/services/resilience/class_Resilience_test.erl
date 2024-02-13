% Copyright (C) 2012-2024 EDF R&D

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


% @doc Unit tests for the implementation of the <b>resilience manager</b>.
%
% See the class_ResilienceManager.erl module.
%
-module(class_Resilience_test).


% For facilities common to all cases:
-include("sim_diasca_for_cases.hrl").


% For result_manager_name:
-include("class_ResultManager.hrl").


% For k_record, etc.:
-include("class_ResilienceManager.hrl").



% @doc Runs the tests.
-spec run() -> no_return().
run() ->

	?case_start,

	FirstResilienceLevel = 5,

	FirstProtectedNodes = [ a, b, c, d, e, f ],

	?test_notice_fmt( "Creating first a ~B-map on nodes ~p.",
					  [ FirstResilienceLevel, FirstProtectedNodes ] ),

	FirstTestKMap = class_ResilienceManager:build_k_map( FirstResilienceLevel,
														 FirstProtectedNodes ),

	?test_notice_fmt( "For a resilience level of ~B, result is: ~ts",
		[ FirstResilienceLevel,
		  class_ResilienceManager:k_map_to_string( FirstTestKMap ) ] ),


	SecondResilienceLevel = 5,

	NodeCount = length( FirstProtectedNodes ),
	%NodeCount = 20,


	?test_notice_fmt( "Creating now a ~B-map on ~B nodes.",
					  [ SecondResilienceLevel, NodeCount ] ),

	SecondProtectedNodes = [ class_Graphable:new_link( [
		{ label, text_utils:format( "Node ~ts", [ N ] ) } ] )
		 || N <- FirstProtectedNodes ],
		%|| N <- lists:seq( 1, NodeCount ) ],


	?test_notice_fmt( "Building a k-map for k=~B and ~B nodes (~p).",
		[ SecondResilienceLevel, NodeCount, SecondProtectedNodes ] ),

	SecondTestKMap = class_ResilienceManager:build_k_map( SecondResilienceLevel,
													SecondProtectedNodes ),


	% Can be cyclic, and better displayed with nodes on a circle:
	ResilienceMesh = class_Mesh:new_link(
		text_utils:format( "Resilience ~B-map for ~B nodes",
						   [ SecondResilienceLevel, NodeCount ] ),
		_Opts=[ { layout, circo } ] ),

	KPairs = table:enumerate( SecondTestKMap ),

	% Two passes are needed, as all nodes must be declared first:
	[ ResilienceMesh ! { addNode, [ N ] } || { N, _KRecord } <- KPairs ],

	[ begin
		  [ begin
				Link = class_Graphable:new_link( [ { label, "" } ] ),
				ResilienceMesh ! { addLink, [ Link, N, Secured ] }
			end
			|| Secured <- KRecord#k_record.securing ]

	  end || { N, KRecord } <- KPairs ],

	% Prerequisite may not be available locally:
	case executable_utils:can_generate_png_from_graph() of

		true ->
			class_Mesh:generate_topological_view_for( ResilienceMesh );

		Hint ->
			?test_warning_fmt(
			   "No support found for the rendering of a graph: ~ts.", [ Hint ] )

	end,

	wooper:delete_synchronously_instance( ResilienceMesh ),

	?case_stop.
