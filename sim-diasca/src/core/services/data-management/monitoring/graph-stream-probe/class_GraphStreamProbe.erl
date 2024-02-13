% Copyright (C) 2022-2024 EDF R&D
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
% Authors: Jérôme Cantenot    [jerome (dot) cantenot (at) edf (dot) fr]
%          Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
%
% Creation date: Tuesday, June 21, 2022.


% @doc Probe able to process and display <b>streams of graphs</b>, that is
% graphs that may change over (simulation) time.
%
% A graph comprises nodes, edges and properties.
%
% Relies on the gephi_support Myriad module.
%
-module(class_GraphStreamProbe).


% Usage notes:
%
% A graph stream probe will fail if its target Gephi server is not listening at
% the target TCP port.
%
% The project path is typically defined among the deployment settings (rather
% than for example when creating a graph stream probe).


-define( class_description,
		 "Probe able to process and display streams of graphs.").


-define( superclasses, [ class_ResultProducer ] ).


-define( class_attributes, [

	{ graph_info, graph_info(),
	  "all information necessary to interact with a graph server of interest" },

	{ timestamp_table,
	  table( app_service_pid(), { ins(), [ { timestamp(), timestamp() } ] } ),
	  "a table associating to the PID of each service its INS and time-related "
	  "information, in reverse chronological order" } ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Result management.GraphStreamProbe" ).



% Type names better not too tied to Gephi:

-type project_path() :: gephi_support:project_path().

-type workspace_name() :: gephi_support:workspace_name().

-type graph_info() :: gephi_support:gephi_server_info().

-type graph_stream_probe_pid() :: class_ResultProducer:producer_pid().


-type graph_stream_probe_ref() :: 'non_wanted_probe' | graph_stream_probe_pid().
% A probe may not be a wanted result producer.


-type node_id() :: gephi_support:node_id().
-type edge_id() :: gephi_support:edge_id().


-export_type([ project_path/0, workspace_name/0, graph_info/0,
			   graph_stream_probe_pid/0, graph_stream_probe_ref/0,

			   node_id/0, edge_id/0 ]).


% For getAttr/1, etc.:
-include_lib("wooper/include/wooper.hrl").

% For app_info*:
-include_lib("traces/include/traces.hrl").



% Feeding such probes:
%
% Most operations are available either as oneways (most efficient, "fire and
% forget" operations) or as requests (more demanding yet safer, as synchronised
% by a returned message, whether or not a specific information is to be sent
% back).
%
% The goal is to avoid any race condition, for example if having triggered
% operations that are still pending whereas shutting down. Such operations may
% indeed not be processed yet, or may accumulate in memory (example of a
% simulation faster to progress than such a probe) and exhaust it.
%
% To prevent such issues, each batch of operations (preferably, for efficiency
% reasons, to each single operation) shall be synchronised; so each of the calls
% in the series may be synchronous, or, better, only the last one (possibly
% itself being a mere call to sync/1) can be synchronous (as they are already
% guaranteed to be evaluated in order). Then operations that are both safe and
% efficient are implemented.
%
% Note though that *sync operations are synchronous because a result is
% returned; the asynchronous operations are still synchronous with the Gephi
% server (due to the POST request being itself synchronous), but not synchronous
% with the caller.

% Method naming: update* (e.g. update{Node,Edge}) has been preferred to the
% original (Gephi) change* (as clearer, cannot be taken for switch*); alter*
% would have been suitable too.



% Implementation notes:
%
% This probe is based on a third-party, free software graph stream tool, namely
% Gephi (https://gephi.org/). See gephi_support.erl for all details.
%
% The Gephi instance, local or on a remote host, is supposed to have a project
% opened and a graph stream server launched. Only the workspace then is to be
% specified by the probe, being the only degree of freedom left for a given
% Gephi instance (host and port being set at the deployment step). At least in a
% given simulation, most if not all graph stream probes are likely to target the
% same workspace.
%
% This probe is to communicate directly over HTTP with the corresponding Gephi
% server.
%
% This communication may be asynchronous (fastest, yet prone to race conditions)
% or not.
%
% Synchronous operations do not return for example the identifiers involved, as
% a single calling process is generally involved.



% Shorthands:


-type bin_json() :: json_utils:bin_json().

-type probe_name_init() :: class_Probe:probe_name_init().
-type probe_name() :: class_Probe:probe_name().

-type producer_options() :: class_ResultProducer:producer_options().
-type producer_result() :: class_ResultProducer:producer_result().

-type timestamp() :: gephi_support:timestamp().
%-type gephi_server_info() :: gephi_support:gephi_server_info().

-type graph_value() :: gephi_support:graph_value().
-type graph_color() :: gephi_support:graph_color().
-type node_id() :: gephi_support:node_id().
-type edge_id() :: gephi_support:edge_id().
-type property_id() :: gephi_support:property_id().
-type property_table() :: gephi_support:property_table().
-type element_label() :: gephi_support:element_label().



% @doc Constructs a graph stream probe of the specified name, using the
% specified information about the graph stream server to act upon.
%
% Refer to declare_result_probe/1 for the actual creation API for actors and
% cases.
%
-spec construct( wooper:state(), probe_name_init(), graph_info() ) ->
									wooper:state().
construct( State, NameInit, GraphInfo ) ->

	ProbeName = class_Probe:get_actual_probe_name( NameInit ),

	% Does *not* declare to the result manager:
	ProducerState = class_ResultProducer:construct( State, ProbeName ),

	% For an increased interleaving:
	getAttribute( ProducerState, result_manager_pid ) !
		{ declareGraphStreamProbe,
		  [ text_utils:string_to_binary( ProbeName ),
			_IsTrackedProducer=true ], self() },

	?send_info_fmt( ProducerState, "Creating a Graph Stream probe, "
		"based on ~ts.", [ gephi_support:server_info_to_string( GraphInfo ) ] ),

	% We declare a trace bridge, so that we can collect the traces directly sent
	% by Myriad's modules, notably gephi_support.
	%
	class_TraceEmitter:register_bridge( ProducerState ),

	ReadyState = setAttributes( ProducerState, [
		{ graph_info, GraphInfo },

		% At least currently, as a result producer, no specific element will be
		% to report:
		%
		{ result_produced, true } ] ),

	% Needed even already done by the deployment manager (hence done by each
	% instance of such probes), as most are likely to be created on the same
	% node as the actors, hence on computing nodes (so starting this support on
	% the user node would not be enough):
	%
	gephi_support:start(),

	% After the call to the declareGraphStreamProbe/4 request:
	wait_result_declaration_outcome( ProbeName, ReadyState ).



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?info( "Deleting graph Stream probe." ),

	% Not desirable, as multiple instances of such probes may exist, and all the
	% others would still rely on the services that would be shut down:
	%
	%gephi_support:stop(),

	% No unregistering from the Gephi server needed.

	State.



% Section for graph interaction.
%
% Each operation is available first as an asynchronous operation, then as a
% synchronous one.


% @doc Adds (asynchronously) the specified node to the graph.
-spec addNode( wooper:state(), node_id() ) -> const_oneway_return().
addNode( State, NodeId ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding asynchronously node '~ts'.", [ NodeId ] ) ),

	gephi_support:add_node( NodeId, ?getAttr(graph_info) ),
	wooper:const_return().


% @doc Adds synchronously the specified node to the graph.
-spec addNodeSync( wooper:state(), node_id() ) ->
								const_request_return( 'node_added' ).
addNodeSync( State, NodeId ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding synchronously node '~ts'.", [ NodeId ] ) ),

	gephi_support:add_node( NodeId, ?getAttr(graph_info) ),
	wooper:const_return_result( node_added ).



% @doc Adds (asynchronously) the specified node, with the specified label, to
% the graph.
%
-spec addNode( wooper:state(), node_id(), element_label() ) ->
								const_oneway_return().
addNode( State, NodeId, NodeLabel ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding asynchronously node '~ts', labelled '~ts'.",
					[ NodeId, NodeLabel ] ) ),

	gephi_support:add_node( NodeId, NodeLabel, ?getAttr(graph_info) ),
	wooper:const_return().


% @doc Adds synchronously the specified node, with the specified label, to
% the graph.
%
-spec addNodeSync( wooper:state(), node_id(), element_label() ) ->
								const_request_return( 'node_added' ).
addNodeSync( State, NodeId, NodeLabel ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding synchronously node '~ts', labelled '~ts'.",
					[ NodeId, NodeLabel ] ) ),

	gephi_support:add_node( NodeId, NodeLabel, ?getAttr(graph_info) ),

	wooper:const_return_result( node_added ).



% @doc Adds (asynchronously) the specified node, with the specified label and
% color, to the graph.
%
-spec addNode( wooper:state(), node_id(), element_label(), graph_color() ) ->
								const_oneway_return().
addNode( State, NodeId, NodeLabel, NodeColor ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding asynchronously node '~ts', labelled '~ts', "
			"with color '~ts'.", [ NodeId, NodeLabel, NodeColor ] ) ),

	gephi_support:add_node( NodeId, NodeLabel, NodeColor,
							?getAttr(graph_info) ),

	wooper:const_return().


% @doc Adds synchronously the specified node, with the specified label and
% color, to the graph.
%
-spec addNodeSync( wooper:state(), node_id(), element_label(),
				   graph_color() ) -> const_request_return( 'node_added' ).
addNodeSync( State, NodeId, NodeLabel, NodeColor ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding synchronously node '~ts', labelled '~ts', "
			"with color '~ts'.", [ NodeId, NodeLabel, NodeColor ] ) ),

	gephi_support:add_node( NodeId, NodeLabel, NodeColor,
							?getAttr(graph_info) ),

	wooper:const_return_result( node_added ).



% @doc Updates (asynchronously) the specified property of the specified node to
% the specified constant (timestamp-less) value.
%
-spec updateNodeProperty( wooper:state(), node_id(), property_id(),
						  graph_value() ) -> const_oneway_return().
updateNodeProperty( State, NodeId, PropertyId, PropertyValue ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Updating asynchronously for node '~ts' "
			"property '~ts' to '~p'.",
			[ NodeId, PropertyId, PropertyValue ] ) ),

	gephi_support:update_node_property( NodeId, PropertyId, PropertyValue,
										?getAttr(graph_info) ),

	wooper:const_return().


% @doc Updates synchronously the specified property of the specified node to
% the specified constant (timestamp-less) value.
%
-spec updateNodePropertySync( wooper:state(), node_id(), property_id(),
		graph_value() ) -> const_request_return( 'node_updated' ).
updateNodePropertySync( State, NodeId, PropertyId, PropertyValue ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Updating synchronously for node '~ts' "
			"property '~ts' to '~p'.",
			[ NodeId, PropertyId, PropertyValue ] ) ),

	gephi_support:update_node_property( NodeId, PropertyId, PropertyValue,
										?getAttr(graph_info) ),

	wooper:const_return_result( node_updated ).



% @doc Updates (asynchronously) the specified properties of the specified node
% to the specified constant (timestamp-less) values.
%
-spec updateNodeProperties( wooper:state(), node_id(), property_table() ) ->
											const_oneway_return().
updateNodeProperties( State, NodeId, PropertyTable ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Updating asynchronously for node '~ts' "
			"the following property ~ts.",
			[ NodeId, table:to_string( PropertyTable ) ] ) ),

	gephi_support:update_node_properties( NodeId, PropertyTable,
										  ?getAttr(graph_info) ),

	wooper:const_return().


% @doc Updates synchronously the specified properties of the specified node
% to the specified constant (timestamp-less) values.
%
-spec updateNodePropertiesSync( wooper:state(), node_id(), property_table() ) ->
									const_request_return( 'node_updated' ).
updateNodePropertiesSync( State, NodeId, PropertyTable ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Updating asynchronously for node '~ts' "
			"the following property ~ts.",
			[ NodeId, table:to_string( PropertyTable ) ] ) ),

	gephi_support:update_node_properties( NodeId, PropertyTable,
										  ?getAttr(graph_info) ),

	wooper:const_return_result( node_updated ).


% @doc Updates (asynchronously) the specified property of the specified node to
% the specified value for the specified timestamp.
%
-spec updateNodeProperty( wooper:state(), node_id(), property_id(),
						  graph_value(), timestamp() ) -> const_oneway_return().
updateNodeProperty( State, NodeId, PropertyId, PropertyValue, Timestamp ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Updating asynchronously for node '~ts' at ~w "
			"property '~ts' to '~p'.",
			[ NodeId, Timestamp, PropertyId, PropertyValue ] ) ),

	gephi_support:update_node_property( NodeId, PropertyId, PropertyValue,
										Timestamp, ?getAttr(graph_info) ),

	wooper:const_return().


% @doc Updates synchronously the specified property of the specified node to
% the specified value for the specified timestamp.
%
-spec updateNodePropertySync( wooper:state(), node_id(), property_id(),
		graph_value(), timestamp() ) -> const_request_return( 'node_updated' ).
updateNodePropertySync( State, NodeId, PropertyId, PropertyValue, Timestamp ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Updating synchronously for node '~ts' at ~w "
			"property '~ts' to '~p'.",
			[ NodeId, Timestamp, PropertyId, PropertyValue ] ) ),

	gephi_support:update_node_property( NodeId, PropertyId, PropertyValue,
										Timestamp, ?getAttr(graph_info) ),

	wooper:const_return_result( node_updated ).



% @doc Updates (asynchronously) the specified properties of the specified node
% to the specified values for the specified timestamp.
%
-spec updateNodeProperties( wooper:state(), node_id(), property_table(),
							timestamp() ) -> const_oneway_return().
updateNodeProperties( State, NodeId, PropertyTable, Timestamp ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Updating asynchronously for node '~ts' at ~w "
			"the following property ~ts.",
			[ NodeId, Timestamp, table:to_string( PropertyTable ) ] ) ),

	gephi_support:update_node_properties( NodeId, PropertyTable, Timestamp,
										  ?getAttr(graph_info) ),

	wooper:const_return().


% @doc Updates synchronously the specified properties of the specified node to
% the specified values for the specified timestamp.
%
-spec updateNodePropertiesSync( wooper:state(), node_id(), property_table(),
		timestamp() ) -> const_request_return( 'node_updated' ).
updateNodePropertiesSync( State, NodeId, PropertyTable, Timestamp ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Updating asynchronously for node '~ts' at ~w "
			"the following property ~ts.",
			[ NodeId, Timestamp, table:to_string( PropertyTable ) ] ) ),

	gephi_support:update_node_properties( NodeId, PropertyTable, Timestamp,
										  ?getAttr(graph_info) ),

	wooper:const_return_result( node_updated ).



% @doc Adds (asynchronously) an edge whose identifier is specified, together
% with the identifiers of the first node and the second one, telling whether it
% is a directed edge (from first node to second one).
%
-spec addEdge( wooper:state(), edge_id(), node_id(), node_id(), boolean() ) ->
											const_oneway_return().
addEdge( State, EdgeId, FirstNodeId, SecondNodeId, IsDirected ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding asynchronously edge '~ts' from node '~ts' "
			"to '~ts'.", [ EdgeId, FirstNodeId, SecondNodeId ] ) ),

	gephi_support:add_edge( EdgeId, FirstNodeId, SecondNodeId,
							IsDirected, ?getAttr(graph_info) ),

	wooper:const_return().


% @doc Adds synchronously an edge whose identifier is specified, together with
% the identifiers of the first node and the second one, telling whether it is a
% directed edge (from first node to second one).
%
-spec addEdgeSync( wooper:state(), edge_id(), node_id(), node_id(),
				   boolean() ) -> const_request_return( 'edge_added' ).
addEdgeSync( State, EdgeId, FirstNodeId, SecondNodeId, IsDirected ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding synchronously edge '~ts' from node '~ts' "
			"to '~ts'.", [ EdgeId, FirstNodeId, SecondNodeId ] ) ),


	gephi_support:add_edge( EdgeId, FirstNodeId, SecondNodeId, IsDirected,
							?getAttr(graph_info) ),

	wooper:const_return_result( edge_added ).



% @doc Adds (asynchronously) an edge whose identifier is specified, together
% with the identifiers of the first node and the second one, telling whether it
% is a directed edge (from first node to second one) and what its color is.
%
-spec addEdge( wooper:state(), edge_id(), graph_color(), node_id(), node_id(),
			   boolean() ) -> const_oneway_return().
addEdge( State, EdgeId, EdgeColor, FirstNodeId, SecondNodeId, IsDirected ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding asynchronously edge '~ts' from node '~ts' "
			"to '~ts' (color: '~ts').",
			[ EdgeId, FirstNodeId, SecondNodeId, EdgeColor ] ) ),

	gephi_support:add_edge( EdgeId, EdgeColor, FirstNodeId, SecondNodeId,
							IsDirected, ?getAttr(graph_info) ),

	wooper:const_return().


% @doc Adds synchronously an edge whose identifier is specified, together with
% the identifiers of the first node and the second one, telling whether it is a
% directed edge (from first node to second one) and what its color is..
%
-spec addEdgeSync( wooper:state(), edge_id(), graph_color(),
				   node_id(), node_id(), boolean() ) ->
										const_request_return( 'edge_added' ).
addEdgeSync( State, EdgeId, EdgeColor, FirstNodeId, SecondNodeId,
			 IsDirected ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding synchronously edge '~ts' from node '~ts' "
			"to '~ts' (color: '~ts').",
			[ EdgeId, FirstNodeId, SecondNodeId, EdgeColor ] ) ),

	gephi_support:add_edge( EdgeId, EdgeColor, FirstNodeId, SecondNodeId,
							IsDirected, ?getAttr(graph_info) ),

	wooper:const_return_result( edge_added ).



% @doc Adds (asynchronously) an edge whose identifier is specified, together
% with the identifiers of the first node and the second one, telling whether it
% is a directed edge (from first node to second one) and what its label and
% color are.
%
-spec addEdge( wooper:state(), edge_id(), element_label(), graph_color(),
			   node_id(), node_id(), boolean() ) -> const_oneway_return().
addEdge( State, EdgeId, EdgeLabel, EdgeColor, FirstNodeId, SecondNodeId,
		 IsDirected ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding asynchronously edge '~ts' from node '~ts' "
			"to '~ts' (label '~ts'; color: '~ts').",
			[ EdgeId, FirstNodeId, SecondNodeId, EdgeLabel, EdgeColor ] ) ),

	gephi_support:add_edge( EdgeId, EdgeLabel, EdgeColor,
		FirstNodeId, SecondNodeId, IsDirected, ?getAttr(graph_info) ),

	wooper:const_return().


% @doc Adds synchronously an edge whose identifier is specified, together with
% the identifiers of the first node and the second one, telling whether it is a
% directed edge (from first node to second one) and what its color is..
%
-spec addEdgeSync( wooper:state(), edge_id(), element_label(), graph_color(),
				   node_id(), node_id(), boolean() ) ->
										const_request_return( 'edge_added' ).
addEdgeSync( State, EdgeId, EdgeLabel, EdgeColor, FirstNodeId, SecondNodeId,
			 IsDirected ) ->

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Adding synchronously edge '~ts' from node '~ts' "
			"to '~ts' (label '~ts'; color: '~ts').",
			[ EdgeId, FirstNodeId, SecondNodeId, EdgeLabel, EdgeColor ] ) ),

	gephi_support:add_edge( EdgeId, EdgeLabel, EdgeColor,
		FirstNodeId, SecondNodeId, IsDirected, ?getAttr(graph_info) ),

	wooper:const_return_result( edge_added ).





% @doc Updates (asynchronously) the specified property of the specified edge to
% the specified constant (timestamp-less) value.
%
-spec updateEdgeProperty( wooper:state(), edge_id(), property_id(),
						  graph_value() ) -> const_oneway_return().
updateEdgeProperty( State, EdgeId, PropertyId, PropertyValue ) ->

	gephi_support:update_edge_property( EdgeId, PropertyId, PropertyValue,
										?getAttr(graph_info) ),

	wooper:const_return().


% @doc Updates synchronously the specified property of the specified edge to the
% specified constant (timestamp-less) value.
%
-spec updateEdgePropertySync( wooper:state(), edge_id(), property_id(),
		graph_value() ) -> const_request_return( 'edge_updated' ).
updateEdgePropertySync( State, EdgeId, PropertyId, PropertyValue ) ->

	gephi_support:update_edge_property( EdgeId, PropertyId, PropertyValue,
										?getAttr(graph_info) ),

	wooper:const_return_result( edge_updated ).



% @doc Updates (asynchronously) the specified properties of the specified edge
% to the specified constant (timestamp-less) values.
%
-spec updateEdgeProperties( wooper:state(), edge_id(), property_table() ) ->
											const_oneway_return().
updateEdgeProperties( State, EdgeId, PropertyTable ) ->

	gephi_support:update_edge_properties( EdgeId, PropertyTable,
										  ?getAttr(graph_info) ),

	wooper:const_return().


% @doc Updates synchronously the specified properties of the specified edge to
% the specified constant (timestamp-less) values.
%
-spec updateEdgePropertiesSync( wooper:state(), edge_id(), property_table() ) ->
									const_request_return( 'edge_updated' ).
updateEdgePropertiesSync( State, EdgeId, PropertyTable ) ->

	gephi_support:update_edge_properties( EdgeId, PropertyTable,
										  ?getAttr(graph_info) ),

	wooper:const_return_result( edge_updated ).



% @doc Updates (asynchronously) the specified property of the specified edge to
% the specified value for the specified timestamp.
%
-spec updateEdgeProperty( wooper:state(), edge_id(), property_id(),
						  graph_value(), timestamp() ) -> const_oneway_return().
updateEdgeProperty( State, EdgeId, PropertyId, PropertyValue, Timestamp ) ->

	gephi_support:update_edge_property( EdgeId, PropertyId, PropertyValue,
										Timestamp, ?getAttr(graph_info) ),

	wooper:const_return().


% @doc Updates synchronously the specified property of the specified edge to the
% specified value for the specified timestamp.
%
-spec updateEdgePropertySync( wooper:state(), edge_id(), property_id(),
		graph_value(), timestamp() ) -> const_request_return( 'edge_updated' ).
updateEdgePropertySync( State, EdgeId, PropertyId, PropertyValue, Timestamp ) ->

	gephi_support:update_edge_property( EdgeId, PropertyId, PropertyValue,
										Timestamp, ?getAttr(graph_info) ),

	wooper:const_return_result( edge_updated ).



% @doc Synchronises this probe to the graph: ensures that all previous
% operations that are potentially still pending have been processed for good.
%
% Typically useful to close a series of asynchronous operations, in order to
% avoid any race condition.
%
-spec sync( wooper:state() ) -> const_request_return( 'graph_synchronised' ).
sync( State ) ->

	% No need to interact with the Gephi server, to which this probe is already
	% synchronised. We just synchronise this probe with the caller, thanks to
	% the use of a request (this one).

	cond_utils:if_defined( sim_diasca_debug_graph_streaming,
		?debug_fmt( "Syncronised with caller ~w.", [ ?getSender() ] ) ),

	wooper:const_return_result( graph_synchronised ).



% ResultProducer-related section.


% @doc Sends the specified results to the caller (generally the result manager).
%
% At least currently, such a probe has no specific result to report.
%
% (const request, for synchronous yet concurrent operations)
%
-spec sendResults( wooper:state(), producer_options() ) ->
						request_return( producer_result() ).
sendResults( State, _Options ) ->
	ResState = setAttribute( State, result_collected, true ),
	wooper:return_state_result( ResState, { self(), no_result } ).



% Static section.


% @doc Declares (synchronously) a new graph stream probe, to be seen as a result
% producer, and to be created either from an actor or from a test case.
%
% Returns:
%
% - either the PID of this newly created result probe, if graph streaming was
% enabled in the deployment settings (refer to its 'enable_graph_streaming'
% field) and also if the name of that probe is acknowledged as a wanted
% result by the result manager
%
% - or the 'non_wanted_probe' atom
%
% Note that the graph stream tool itself is to be launched by the deployment
% manager, only based on whether the graph stream service is enabled and whether
% the simulation is in batch mode (hence regardless of whether actual graph
% stream probes are created).
%
-spec declare_result_probe( probe_name_init() ) ->
								static_return( graph_stream_probe_ref() ).
declare_result_probe( NameInit ) ->

	ProbeName = class_Probe:get_actual_probe_name( NameInit ),

	ActualBinName = text_utils:string_to_binary( ProbeName ),

	ResultManagerPid = class_ResultManager:get_result_manager(),

	ResultManagerPid ! { isResultProducerWanted,
		[ ActualBinName, _Nature=graph_stream_probe ], self() },

	Res = receive

		{ wooper_result, { true, _Metadata } } ->

			DeployManPid = class_DeploymentManager:get_deployment_manager(),

			% In the future we could imagine using multiple, possibly per-probe,
			% workspaces, on a same Gephi instance (still controlled by the
			% deployment manager).
			%
			DeployManPid ! { getGraphStreamInformation, [], self() },

			% Disabled, as they are always metadata (e.g. layer versions, tick
			% duration, etc.):
			%
			%Metadata =:= [] orelse
			%   trace_utils:warning_fmt( "Ignoring, for graph stream probe "
			%       "'~ts', the following metadata: ~n ~p.",
			%       [ ProbeName, Metadata ] ),

			receive

				{ wooper_result, _MaybeGraphInfos=undefined } ->

					cond_utils:if_defined( sim_diasca_debug_graph_streaming,
						trace_utils:debug_fmt(
							"Graph stream probe '~ts' wanted.",
							[ ProbeName ] ) ),

					non_wanted_probe;

				{ wooper_result, GraphInfo } ->

					cond_utils:if_defined( sim_diasca_debug_graph_streaming,
						trace_utils:debug_fmt(
							"The graph stream probe '~ts' is wanted, "
							"and is to use a ~ts.", [ ProbeName,
								gephi_support:server_info_to_string( GraphInfo )
													] ) ),

					synchronous_new_link( NameInit, GraphInfo )

			end;


		{ wooper_result, false } ->
			cond_utils:if_defined( sim_diasca_debug_graph_streaming,
				trace_utils:debug_fmt(
					"Graph stream probe '~ts' not wanted.",
					[ ProbeName ] ) ),
			non_wanted_probe

	end,

	wooper:return_static( Res ).



% @doc Synchronises to the specified graph stream probe.
%
% Prevents the overflowing of it by callers.
%
-spec wait_for( graph_stream_probe_pid() ) -> static_void_return().
wait_for( GSPPid ) ->
	GSPPid ! { sync, [], self() },
	receive

		{ wooper_result, graph_synchronised } ->
			wooper:return_static_void()

	end.



% @doc Returns a JSON binary string aggregating the specified terms.
-spec encode_terms_to_json( [ term() ] ) -> static_return( bin_json() ).
encode_terms_to_json( Terms ) ->

	% Typically each term is [Timestamp :: iso8601_bin_string(), graph_term()]
	% 2-element list.

	TermsAsJson = [ json_utils:to_json( T ) || T <- Terms ],

	Bin = text_utils:bin_join( _Sep=$;, TermsAsJson ),

	wooper:return_static( Bin ).



% @doc Returns a JSON binary string appending to the end of the specified one
% the specified term.
%
-spec append_term_to_json( bin_json(), term() ) -> static_return( bin_json() ).
append_term_to_json( BaseBinJson, Term ) ->
	TermAsBinJson = json_utils:to_json( Term ),
	Bin = text_utils:bin_concatenate( BaseBinJson, TermAsBinJson ),
	wooper:return_static( Bin ).



% Section for helper functions (not methods).


% @doc Waits for the feedback of the result manager, after this probe declared
% itself to it (after a call to its declareGraphStreamProbe/3 request).
%
-spec wait_result_declaration_outcome( probe_name(), wooper:state() ) ->
												wooper:state().
wait_result_declaration_outcome( ProbeName, State ) ->

	%trace_utils:info_fmt( "Waiting for the result declaration outcome for "
	%                      "graph probe '~ts'.", [ ProbeName ] ),

	% Finally waits the answer from the result manager to a prior
	% declareGraphStreamProbe/3 call or similar:
	%
	% (in terms of result selection, what is true now may not be true anymore
	% later, if the state of the result manager changes)
	%
	receive

		{ wooper_result, output_not_requested } ->
			?info_fmt( "The graph stream probe '~ts' would not produce "
					   "an expected result.", [ ProbeName ] ),

			setAttribute( State, enabled_producer, false );


		{ wooper_result, output_requested } ->

			% Default is enabled_producer set to true:
			?info_fmt( "The graph stream probe '~ts' will produce an expected "
					   "result.", [ ProbeName ] ),

			State

	end.
