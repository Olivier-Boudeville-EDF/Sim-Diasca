% Copyright (C) 2024-2024 EDF R&D
%
% This file is part of Sim-Diasca.
%
% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: Monday, January 15, 2024.


% @doc Actor test class regarding graph stream probes.
%
% See the graph_stream_probe_feeding_test module.
%
-module(class_TestGraphStreamActor).


-define( class_description, "Actor test class regarding graph stream probes" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).



% The class-specific attributes of this test actor are:
-define( class_attributes, [

	{ graph_probe_ref, class_GraphStreamProbe:graph_stream_probe_ref(),
	  "the tested graph stream probe (if any)" },

	{ nodes, [ node_id() ], "the identifiers of all created nodes" },

	{ edges, [ edge_id() ], "the identifiers of all created edges" } ] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.Test" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_actors.hrl").


% Shorthands:

-type tick_offset() :: class_TimeManager:tick_offset().

%-type node_id() :: gephi_support:node_id() .
%-type edge_id() :: gephi_support:edge_id() .



% @doc Constructs a test actor:
%
% - ActorSettings corresponds to the various information (e.g. AAI, seeding,
% ordering mode, etc.) that the load-balancer sets for each newly created actor
%
% - ActorName the name of the actor
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name() ) -> wooper:state().
construct( State, ActorSettings, ActorName ) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(ActorName) ),

	ProbeName = text_utils:format( "Test graph stream probe for ~ts",
								   [ ActorName ] ),

	GSProbeRef = class_GraphStreamProbe:declare_result_probe( ProbeName ),

	setAttributes( ActorState, [ { nodes, [] },
								 { edges, [] },
								 { graph_probe_ref, GSProbeRef },
								 { termination_tick_offset, 150 } ] ).




% Methods section.


% Management section of the actor.


% @doc The core of the test actor behaviour.
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),
	TerminationOffset = ?getAttr(termination_tick_offset),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case CurrentTickOffset of

		PastOffset when PastOffset >= TerminationOffset ->
			executeOneway( State, declareTermination );

		CurrentOffset ->
			% Non-termination behaviour:
			behave_normally( CurrentOffset, State )

	end,

	wooper:return_state( NewState ).



% @doc Overridden, in order to synchronise correctly the internal planning that
% this test actor maintains, and to start its behaviour.
%
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
											actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	UpdatedState = executeOneway( State, scheduleNextSpontaneousTick ),

	actor:return_state( UpdatedState ).



% @doc Manages the normal, non-termination behaviour of this actor.
%
% Returns an updated state.
%
% (helper)
%
-spec behave_normally( tick_offset(), wooper:state() ) -> void().
behave_normally( CurrentOffset, State ) ->

	Nodes = ?getAttr(nodes),
	Edges = ?getAttr(edges),

	NodeCount = length( Nodes ),
	EdgeCount = length( Edges ),

	?debug_fmt( "Acting spontaneously, having already created ~B nodes "
				"and ~B edges.", [ NodeCount, EdgeCount ] ),

	NewState = case ?getAttr(graph_probe_ref) of

		non_wanted_probe ->
			State;

		GraphStreamProbePid ->

			NodeId = text_utils:integer_to_binary( NodeCount + 1 ),
			NodeLabel = text_utils:bin_format( "Test node #~ts", [ NodeId ] ),

			% Asynchronous:
			GraphStreamProbePid ! { addNode, [ NodeId, NodeLabel ] },

			% Not too many not too soon:
			EdgeToCreateCount = NodeCount div 2,

			NewEdges = create_edges( EdgeToCreateCount, Nodes,
				GraphStreamProbePid, _AccEdges=Edges ),

			GraphState = setAttributes( State, [ { nodes, [ NodeId | Nodes ] },
												 { edges, NewEdges } ] ),

			% Safer:
			GraphStreamProbePid ! { sync, [], self() },
			receive

				{ wooper_result, graph_synchronised } ->
					GraphState

			end

	end,

	executeOneway( NewState, addSpontaneousTick, CurrentOffset + 5 ).



% Creates the specified number of edges.
create_edges( _EdgeToCreateCount=0, _Nodes, _GraphStreamProbePid, AccEdges ) ->
	AccEdges;

create_edges( EdgeToCreateCount, Nodes, GraphStreamProbePid, AccEdges ) ->

	EdgeId = text_utils:integer_to_binary( length( AccEdges ) + 1 ),

	FirstNodeId = list_utils:draw_element( Nodes ),
	SecondNodeId = list_utils:draw_element( Nodes ),

	IsDirected = random_utils:get_boolean(),

	GraphStreamProbePid !
		{ addEdge, [ EdgeId, FirstNodeId, SecondNodeId, IsDirected ] },

	create_edges( EdgeToCreateCount-1, Nodes, GraphStreamProbePid,
				  [ EdgeId | AccEdges ] ).
