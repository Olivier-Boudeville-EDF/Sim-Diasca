% Copyright (C) 2016-2021 EDF R&D

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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


-module(class_LanguageBindingManager).


-define( class_description,
		 "Abstract class defining a binding manager for a given programming "
		 "language, i.e. facilities in order to drive a set of distributed "
		 "runtime containers for that language (ex: virtual machines, "
		 "interpreters, etc.), each running on a distinct computing node."
		 " This class defines the mother class of all (singleton) per-language "
		 "manager (each driving its set of runtime containers).").


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).


% The class-specific attributes:
-define( class_attributes, [

		{ node_table, node_table(), "table associating to each computing node "
		  "the PID of the binding container running on that node" },

		{ engine_root_dir, directory_path(),
		  "root directory in which the engine is located, on the user node" },

		{ epmd_port, net_utils:tcp_port(), "the TCP port at which the EPMD "
		  "daemon of interest can be reached (ex: so that any binding-created "
		  "node can find it)" },

		{ code_path, code_utils:code_path(), "the overall code path (if any) "
		  "to be used by each binding container in order to locate the binary "
		  "elements it is to load" },

		{ deployment_manager_pid, deployment_manager_pid(),
		  "PID of the deployment manager" } ] ).


-type manager_pid() :: agent_pid().


-type node_table() :: table( net_utils:atom_node_name(),
							 language_utils:runtime_container_pid() ).


% For child classes:
-export_type([ node_table/0, manager_pid/0 ]).




% Helpers defined for all processing units available from a binding:
-export([ get_encoded_input_ports_data/1,
		  get_encoded_input_port_iterations_data/1,

		  get_encoded_output_port_iterations_data/1,

		  apply_activation_results/2,

		  decode_input_port_specs/1, decode_output_port_specs/1 ]).


% Exported helpers:
-export([ to_string/1 ]).


% For agent_pid():
-include("engine_common_defines.hrl").

% For types and names related to dataflows:
-include("dataflow_defines.hrl").

% For types and names related to language bindings:
-include("bindings.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Deployment.LanguageBindingManager" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/traces.hrl").


% Centralisation for binding-based processing units:


% For input_port and all:
-include("class_DataflowBlock_defines.hrl").


% Describes (as a binary) the semantics associated to a dataflow value:
-type binary_value_semantics() :: binary().


% Describes (as a binary) the unit associated to a dataflow value:
-type binary_value_unit() :: binary().


% Describes (as a binary) the type associated to a dataflow value:
-type binary_value_type() :: binary().


% A list of the changes to perform on the output ports of a binding-implemented
% processing unit after its activation (i.e. the execution of its activate/1
% method, done in a runtime container of a binding):
%
-type activation_result() :: { output_port_name(),
							   { actual_value(), binary_value_semantics(),
								 binary_value_unit(), binary_value_type() } }.

-type activation_results() :: [ activation_result() ].



% Shorthands:

-type directory_path() :: file_utils:directory_path().

-type tcp_port() :: net_utils:tcp_port().

-type code_path() :: code_utils:code_path().



% Implementation notes:
%
% Each actual manager for a language Foobar is expected to register itself
% globally, by defining:
%
% -define( foobar_binding_manager_name, sim_diasca_foobar_binding_manager ).
%
% The process is to create all (Erlang) actors regardless of their relying or
% not on a language binding, knowing that some of them may internally rely on
% foreign code (ex: Python-based) that is to be evaluated by a binding container
% (ex: a Python interpreter) that will be chosen so that it runs on the same
% computing node as the created actor making use of it.
%
% The binding-specific code shall remain as much as possible separated from the
% rest of the code.



% Constructs a new, abstract, named, language binding manager, from:
%
% - BindingManagerName, the name of that binding manager
%
% - EngineRootDir, the root directory in which the engine is located, on the
% user node
%
% - EpmdPort, the TCP port of the EPMD daemon to rely on (if any)
%
% - CodePath, the code path to use for each bound runtime container
%
% - DeploymentManagerPid, the PID of the deployment manager
%
-spec construct( wooper:state(), class_TraceEmitter:emitter_init(),
		directory_path(), maybe( tcp_port() ), code_path(),
		deployment_manager_pid() ) -> wooper:state().
construct( State, BindingManagerName, EngineRootDir, EpmdPort, CodePath,
		   DeploymentManagerPid ) ->

	% First the direct mother class:
	BaseState = class_EngineBaseObject:construct( State,
								?trace_categorize(BindingManagerName) ),

	setAttributes( BaseState, [
		{ node_table, table:new() },
		{ engine_root_dir, EngineRootDir },
		{ epmd_port, EpmdPort },
		{ code_path, CodePath },
		{ deployment_manager_pid, DeploymentManagerPid } ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	NodeTable = ?getAttr(node_table),

	case table:is_empty( NodeTable ) of

		true ->
			?info( "Language manager destructed "
				   "(with no live runtime container)." ),
			State;

		false ->
			?warning_fmt( "Language manager destructed, while still having "
				"runtime containers registered: ~ts", [ to_string( State ) ] ),

			% Container destruction is language-specific, it is already too
			% late, we leave the node_table as it is.
			State

	end.



% Methods section.


% Returns the runtime container of the binding associated to the specified
% actor.
%
% In practice the returned runtime container is the one running on the same node
% as the sender, to lighten the load induced by their exchanges.
%
-spec getAssociatedRuntimeContainer( wooper:state(),
									 class_Actor:actor_pid() ) ->
				const_request_return( language_utils:runtime_container_pid() ).
getAssociatedRuntimeContainer( State, ActorPid ) ->

	ActorNode = node( ActorPid ),

	NodeTable = ?getAttr(node_table),

	case table:lookup_entry( _K=ActorNode, NodeTable ) of

		key_not_found ->
			throw( { no_runtime_container_on_node, ActorNode } );

		{ value, ContainerPid } ->
			wooper:const_return_result( ContainerPid )

	end.



% Helpers section.


% Returns a textual description of this language manager.
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	NodePairs = table:enumerate( ?getAttr(node_table) ),

	NodeStrings = [ text_utils:format( "container ~w running on node '~ts'",
		   [ ContainerPid, Node ] ) || { Node, ContainerPid } <- NodePairs ],

	EpmdString = case ?getAttr(epmd_port) of

		undefined ->
			"no EPMD port";

		Port ->
			text_utils:format( "EPMD port ~B", [ Port ] )

	end,

	text_utils:format( "binding manager federating ~B runtime containers: ~ts "
		"using ~ts and storing following code path: ~ts",
		[ length( NodeStrings ), text_utils:strings_to_string( NodeStrings ),
		  EpmdString,
		  code_utils:code_path_to_string( ?getAttr(code_path) ) ] ).



% The helpers below have been defined for all processing units available from a
% binding (ex: class_Dataflow{Python,Java}ProcessingUnit.erl).
%
% Note: they cannot be defined in dataflow_binding_utils.erl due to their use of
% the getAttr macro.




% Builds the exhaustive list of all port statuses, with their names.
-spec get_encoded_input_ports_data( wooper:state() ) ->
									[ { input_port_name(), value_status() } ].
get_encoded_input_ports_data( State ) ->

	% Gets the complete table of input ports:
	InputPortTable = ?getAttr(input_ports),

	% Gets the names of all ports, in binary format (thus already encoded), as
	% the keys of this table:
	%
	InputPortBinNames = table:keys( InputPortTable ),

	% Zips these encoded port names with the associated port statuses:
	[ begin

		  InputPortStatus = class_DataflowBlock:get_input_port_status(
													InputPortBinName, State ),

		  { InputPortBinName, InputPortStatus }

	  end || InputPortBinName <- InputPortBinNames ].




% Builds the encoded list of all input data needed for activation and related to
% input port iterations.
%
-spec get_encoded_input_port_iterations_data( wooper:state() ) ->
	[ { input_port_name(), iteration_multiplicity(), [ iterated_index() ] } ].
get_encoded_input_port_iterations_data( State ) ->

	% Builds the exhaustive list of all input port iterations (their current
	% states):

	InputPortIterationTable = ?getAttr(input_iterations),

	AllInputPortIterations = table:values( InputPortIterationTable ),

	% Returns, for each of these iterations, the data potentially relevant for
	% the computations made in the 'activate' bound method:
	%
	[ { InputPortIteration#input_port_iteration.base_name,
		InputPortIteration#input_port_iteration.multiplicity,
		InputPortIteration#input_port_iteration.port_indexes }

	  || InputPortIteration <- AllInputPortIterations ].



% Builds the encoded list of all output data needed for activation and related
% to output port iterations.
%
-spec get_encoded_output_port_iterations_data( wooper:state() ) ->
	[ { output_port_name(), iteration_multiplicity(), [ iterated_index() ] } ].
get_encoded_output_port_iterations_data( State ) ->

	% Builds the exhaustive list of all output port iterations (their current
	% states):

	OutputPortIterationTable = ?getAttr(output_iterations),

	AllOutputPortIterations = table:values( OutputPortIterationTable ),

	% Returns, from these iterations, the data potentially relevant for the
	% computations made in the 'activate' bound method:
	%
	[ { OutputPortIteration#output_port_iteration.base_name,
		OutputPortIteration#output_port_iteration.multiplicity,
		OutputPortIteration#output_port_iteration.port_indexes }

	  || OutputPortIteration <- AllOutputPortIterations ].



% Interprets the specified activation results as a list of tasks to achieve on
% the output ports, then performs them.
%
-spec apply_activation_results( activation_results(), wooper:state() ) ->
										wooper:state().
apply_activation_results( ActivationResults, State ) ->

	% Separates the names of ports from the data needed for creating the channel
	% values:
	%
	{ BinOutputPortNames, ChannelValueData } = lists:unzip( ActivationResults ),


	% Creates the channel values by decoding the fetched data:
	ChannelValues = [ begin

		  StringSems = [ text_utils:binary_to_string( Sem ) || Sem <- BinSems ],

		  Unit = text_utils:binary_to_string( BinUnit ),
		  Type = text_utils:binary_to_string( BinType ),

		  class_Dataflow:create_channel_value( Value, StringSems, Unit, Type )

		  end || { Value, BinSems, BinUnit, BinType } <- ChannelValueData ],

	% Sets the channel values on the corresponding output ports:
	OutputPortPairs = lists:zip( BinOutputPortNames, ChannelValues ),

	class_DataflowBlock:set_output_port_values( OutputPortPairs, State ).



% Turns all the binaries in the encoded input port specs (keys and values as
% well) into the types expected by the 'parse_raw_input_port_spec' function.
%
-spec decode_input_port_specs( [ { binary(), term() } ] ) ->
										[ { atom(), term() } ].
decode_input_port_specs( EncodedInputPortSpecs ) ->
	% Grabs the values and converts them one by one, according to their meaning:
	decode_input_port_specs_values( EncodedInputPortSpecs, _Acc=[] ).



% (helper)
decode_input_port_specs_values( _Proplist=[], Acc ) ->
	lists:reverse( Acc );

decode_input_port_specs_values( [ { <<"name">>, BinName } | T ], Acc )
  when is_binary( BinName ) ->

	NewAcc =
		[ { input_port_name, text_utils:binary_to_string( BinName ) } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


decode_input_port_specs_values( [ { <<"comment">>, BinComment } | T ], Acc )
  when is_binary( BinComment ) ->

	NewAcc = [ { comment, text_utils:binary_to_string( BinComment ) } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


decode_input_port_specs_values( [ _IterSpec={ <<"is_iteration">>, IterSpecVal }
								  | T ], Acc ) ->

	% An iteration specification from a binding can only be a boolean, or an
	% int, or a tuple, or imbricated tuples containing integers.
	%
	% All of these possible types of specification are transmitted as they are
	% in Erlang, and do not need any conversion:
	%
	decode_input_port_specs_values( T,
									[ { is_iteration, IterSpecVal } | Acc ] );

decode_input_port_specs_values( [ { <<"value_semantics">>, BinSemList } | T ],
								Acc ) when is_list( BinSemList ) ->

	StringSems = [ text_utils:binary_to_string( BinSem )
				   || BinSem <- BinSemList ],

	NewAcc = [ { value_semantics, StringSems } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


decode_input_port_specs_values( [ { <<"value_unit">>, BinUnit } | T ], Acc )
  when is_binary( BinUnit ) ->

	NewAcc = [ { value_unit, text_utils:binary_to_string( BinUnit ) } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


decode_input_port_specs_values(
  [ { <<"value_type_description">>, BinTypeDesc } | T ], Acc )
  when is_binary( BinTypeDesc ) ->

	NewAcc = [ { value_type_description,
				 text_utils:binary_to_string( BinTypeDesc ) } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


% Processes the specific case of the "in" constraint, which takes a list as
% parameters:
%
decode_input_port_specs_values( [
	  { <<"value_constraints">>, [ { <<"in">>, BinConstraints } ] } | T ], Acc )
  when is_list( BinConstraints ) ->

	% Checking:
	case lists:all( fun( C ) -> is_binary( C ) end, BinConstraints ) of

		true ->
			ok;

		false ->
			?notify_error_fmt( "Invalid constraints: the 'in' constraint came "
				"with a list of constrainst values that were "
				"not all binaries: '~p'", [ BinConstraints ] ),
			throw( { non_binary_constraint_in_list, BinConstraints } )

	end,

	DecodedConstraints = [ text_utils:binary_to_string( C )
						   || C <- BinConstraints ],

	NewAcc = [ { value_constraints, [ { in, DecodedConstraints } ]  } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


% Processes the other constrainsts, where they should all be binary:
decode_input_port_specs_values( [ { <<"value_constraints">>, BinConstraints }
								  | T ], Acc ) when is_list( BinConstraints ) ->

	% Checking:
	case lists:all( fun( C ) -> is_binary( C ) end, BinConstraints ) of

		true ->
			ok;

		false ->
			?notify_error_fmt( "Invalid constraints: some of the constraints "
							   "were not binary in: '~p'", [ BinConstraints ] ),
			throw( { non_binary_constraint_in_list, BinConstraints } )
	end,

	DecodedConstraints = [ text_utils:binary_to_atom( C )
						   || C <- BinConstraints ],

	NewAcc = [ { value_constraints, DecodedConstraints } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


decode_input_port_specs_values( [ OtherSpec | _T ], _Acc ) ->

	?notify_error_fmt( "Invalid input port specification received from the "
		"static declarations of the bound processing unit: ~p", [ OtherSpec ] ),

	throw( { invalid_input_port_spec, OtherSpec } ).



% Turns all the binaries in the encoded output port specs (keys and values as
% well) into the types expected by the 'parse_raw_output_port_spec' function:
%
-spec decode_output_port_specs( [ { binary(), term() } ] ) ->
										[ { atom(), term() } ].
decode_output_port_specs( EncodedOutputPortSpecs ) ->
	% Grabs the values and converts them one by one, according to their meaning:
	decode_output_port_specs_values( EncodedOutputPortSpecs, _Acc=[] ).


% (helper)
decode_output_port_specs_values( _Proplist=[], Acc ) ->
	lists:reverse( Acc );

decode_output_port_specs_values( [ { <<"name">>, BinName } | T ], Acc )
  when is_binary( BinName ) ->

	NewAcc = [ { output_port_name, text_utils:binary_to_string( BinName ) }
			   | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values( [ { <<"comment">>, BinComment } | T ], Acc )
  when is_binary( BinComment ) ->

	NewAcc = [ { comment, text_utils:binary_to_string( BinComment ) } | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values(
  [ _IterSpec={ <<"is_iteration">>, IterSpecVal } | T ], Acc ) ->

	% An iteration specification from a binding can only be a boolean, or an
	% int, or a tuple, or imbricated tuples containing integers.
	%
	% All of these possible types of specification are transmitted as are in
	% Erlang, and do not need any conversion:
	%
	decode_output_port_specs_values( T,
									 [ { is_iteration, IterSpecVal } | Acc ] );


decode_output_port_specs_values(
  [ _ResultSpec={ <<"produces_result">>, SpecVal } | T ], Acc ) ->

	% A 'produces_result' specification from a binding can only be a boolean
	% value.
	%
	% Since booleans are converted automatically by ErlPort, there is no need
	% for any extra conversion:
	%
	decode_output_port_specs_values( T,
									 [ { produces_result, SpecVal } | Acc ] );


decode_output_port_specs_values(
  [ { <<"value_semantics">>, BinSemList } | T ], Acc )
  when is_list( BinSemList ) ->

	StringSems = [ text_utils:binary_to_string( BinSem )
				   || BinSem <- BinSemList ],

	NewAcc = [ { value_semantics, StringSems } | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values( [ { <<"value_unit">>, BinUnit } | T ], Acc )
  when is_binary( BinUnit ) ->

	NewAcc = [ { value_unit, text_utils:binary_to_string( BinUnit ) } | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values(
  [ { <<"value_type_description">>, BinTypeDesc } | T ], Acc )
  when is_binary( BinTypeDesc ) ->

	NewAcc = [ { value_type_description,
				 text_utils:binary_to_string( BinTypeDesc ) } | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values(
  [ { <<"value_constraints">>, BinConstraints } | T ], Acc )
  when is_list( BinConstraints ) ->

	true = lists:all( fun( C ) -> is_binary( C ) end, BinConstraints ),

	DecodedConstraints =
		[ text_utils:binary_to_atom( C ) || C <- BinConstraints ],

	NewAcc = [ { value_constraints, DecodedConstraints } | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values( [ OtherSpec | _T ], _Acc ) ->

	?notify_error_fmt( "Invalid output port specification received from the "
		"static declarations of the bound processing unit: ~p", [ OtherSpec ] ),

	throw( { invalid_output_port_spec, OtherSpec } ).
