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


-module(class_DataflowBlock).


-define( class_description,
		 "Base class for all dataflow blocks, defining the constructs that are "
		 "common to most of the actual parts constituting a dataflow. "
		 "This is a specialization of the generic actor, meant to gather all "
		 "traits that are shared by the various dataflow blocks. "
		 "This class should provide most of the basics needed to properly "
		 "describe notably all dataflow objects and units. "
		 "When a dataflow is updated (typically based on changesets), the "
		 "various blocks impacted are placed in a suspended state. "
		 "They are made active again when the structure of the dataflow is "
		 "stable once again, i.e. when all updates have been applied."
		 "Please refer to the 'Sim-Diasca Dataflow HOWTO' for further "
		 "information. "
		 "See also: class_DataflowObject, class_DataflowProcessingUnit." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Actor ] ).



% Exported helpers, typically to be used as an API dedicated to cover the
% internal needs of blocks:
%
-export([ get_static_information/1, get_static_information/2,
		  get_block_pid/2, get_upstream_blocks/1, get_downstream_blocks/1,
		  get_directly_connected_blocks/1,
		  unset_input_port/2, unset_output_port/2,
		  get_input_entries/1, get_output_entries/1 ]).




% Design notes:
%
% Input and output ports are translated as records (hence passive
% datastructures), owned by the dataflow block that comprises them.
%
% Channels (a pair made of an input port and the output port feeding it) are
% abstracted out (they are not instances per se).
%
% In some cases (ex: dataflow objects), to an attribute (ex: a dataflow
% attribute named 'foobar') will be associated one input port and one output
% port. It is more convenient to have them bear the same name ('foobar') and be
% distinguished based on their type (either 'input_port' or 'output_port'),
% rather than to have to adopt cumbersome and problematic conventions (such as
% 'foobar_input' and 'foobar_output').
%
% For more information please refer to the 'Sim-Diasca Dataflow HOWTO' document.


% Semantics and types can be managed both at the class and instance levels:
% while many verifications can be done statically, and once for all for the
% instances of a given class of dataflow block, being able to evaluate dynamic
% dataflows (ex: with a block declaring additional ports at simulation time)
% certainly induces the need for runtime, instance-level verifications as well.

% Having a third-party create a channel between two dataflow blocks is to be
% done by contacting the source (the upstream block): for example, prior to
% simulation start, its connectOutputPortInitially/4 request shall be called.
%
% Then this source will send a requestConnectionToInput/5 request to the target,
% downstream block, whose result will tell the source whether the creation of
% the channel succeeded.


% An input port may be set by its upstream block more than once in a tick; it
% can also be set by a third-party (typically an experiment entry point),
% possibly even with the 'none' timestamp.


% All block instances shall register to their respective dataflow. It is to be
% done by all child classes of this class, thanks to their onFirstDiasca/2 actor
% oneway (so that the initial blocks and the ones created in the course of the
% simulation are treated identically).



% Implementation notes:
%
% The user is expected to rely only on plain strings, not binary ones; hence
% proper translations (especially when sending messages) shall be done at the
% interfaces (ex: thanks to static methods), so that (as much as possible) only
% binary strings are actually exchanged through messages.

% Iterated ports are absolutely standard ports.


% This central class offers many services. To keep the size of its sources
% reasonable, it has been split in three files:
%
% - class_DataflowBlock.erl: the main source (this file)
%
% - class_DataflowBlock_defines.hrl: an header, meant to be included in
% order to share type definitions and related blocks
%
% - class_DataflowBlock_functions.hrl: a gathering of related helper functions
%
% Note that by default modifying either of these last two headers will *not*
% trigger the recompilation of this class_DataflowBlock (consider removing its
% BEAM or touching class_DataflowBlock.erl for that).



% Private header to make this file smaller and allow to inspect more easily the
% definition of datastructures:
%
-include("class_DataflowBlock_defines.hrl").



% The class-specific attributes of a dataflow block are:
-define( class_attributes, [

	{ input_ports, input_port_table(), "an associative table that stores the "
	  "input ports of that dataflow block" },

	{ output_ports, output_port_table(), "an associative table that stores the "
	  "output ports of that dataflow block" },

	{ input_iterations, input_iteration_table(), "a table whose keys are the "
	  "names of the known input port iterations, their associated values being "
	  "the records that correspond to these iterations" },

	{ output_iterations, output_iteration_table(), "a table whose keys are the "
	  "names of the known output port iterations, their associated values "
	  "being the records that correspond to these iterations" },

	{ run_status, run_status(),
	  "describes the current mode of operation of this block" },

	{ dataflow_pid, dataflow_pid(),
	  "the PID of the dataflow including this block" },

	{ semantic_server_pid, semantic_server_pid(), "the PID of the semantic "
	  "server, possibly requested by this block" },

	{ type_server_pid, type_server_pid(), "the PID of the type server, "
	  "possibly requested by this block" },

	{ identification_server_pid, identification_server_pid(), "the PID of the "
	  "identification server, possibly requested by this block to resolve "
	  "identifiers" } ] ).


% To have the trace messages adequately sorted:
-define( trace_emitter_categorization, "Core.Dataflow.Block" ).



% For input_port_spec and all:
-include("dataflow_defines.hrl").



% For child classes:
-export_type([ input_port/0, output_port/0,
			   input_port_table/0, output_port_table/0,
			   input_iteration_table/0, output_iteration_table/0 ]).



% Describes an inbound connection request, to be sent by an upstream block (with
% its name and description) to a downstream one:
%
-type inbound_connection_info() :: { output_port_name(), port_description(),
		 class_DataflowUnitManager:canonical_downstream_port_spec() }.


% Shorthands:

-type ustring() :: text_utils:ustring().

-type canonical_connection_spec() ::
		class_DataflowUnitManager:canonical_connection_spec().

-type action_id() :: class_DataflowUnitManager:action_id().

-type type_entry() :: class_TypeServer:type_entry().
-type type_entries() :: class_TypeServer:type_entries().

-type managed_unit_spec() :: class_DataflowUnitManager:managed_unit_spec().

-type vocabulary() :: class_SemanticServer:vocabulary().



% Describes a fully explicit connection, as established by the downstream block.
-type connection_info() :: { output_port_name(), input_port_name() }.


% Helpers:
-export([ create_input_iterated_ports/4, create_output_iterated_ports/3,
		  get_input_port/3, get_output_port/3, get_output_port_status/2,
		  get_output_port_metadata/2,

		  get_input_port_status/2, get_input_port_value/2,
		  extract_input_port_value/3,
		  validate_value_for_input_port/4, assign_input_value/4,
		  set_input_port_values/2, set_input_port_direct_values/2,
		  set_output_port_value/3, set_output_port_values/2,
		  set_output_port_direct_values/2,
		  validate_value_for_output_port/4, assign_output_value/4,
		  % for exit point: get_output_port_value/2, get_output_port_values/2

		  get_input_port_iteration/2, get_input_iterated_ports/2,
		  get_all_input_iteration_values/2, set_all_output_iteration_values/3,

		  register_output_ports/4 ]).



% String-related helpers:
-export([ input_port_spec_to_string/1, output_port_spec_to_string/1,
		  to_string/1, io_to_string/1, io_to_string/2,
		  input_port_to_string/2, output_port_to_string/2,
		  input_port_iteration_to_string/1,
		  output_port_iteration_to_string/1, port_description_to_string/1,
		  value_to_string/1 ]).


% Record-related helpers:
-export([ parse_raw_input_port_spec/1, parse_raw_output_port_spec/1 ]).


% Describes the run status of this block, which maybe either active (runnable,
% i.e. able to be activated), suspended (i.e. currently disabled), or
% terminating (in the verge of destruction).
%
% By default a block is active when created.
%
-type run_status() :: 'active' | 'suspended' | 'terminating'.


-export_type([ connection_info/0, run_status/0 ]).


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").



% Templates for child classes:


% Returns the semantics statically declared by this dataflow block.
%
% Defining this method allows to ensure that all the ports ever created by this
% dataflow block will rely on user-level semantics among this explicitly stated
% list.
%
% Otherwise the list would be deduced from the initial port specifications, with
% no specific control.
%
% -spec get_declared_semantics() ->
%              static_return( class_SemanticServer:user_vocabulary() ).
% get_declared_semantics() ->
%	wooper:return_static( [ "an example" ] ).


% Returns the types statically declared by this dataflow block.
% -spec get_declared_types() ->
%    static_return( type_entries() ).
% get_declared_types() ->
%   wooper:return_static( [ { 'my_type', "'something'|'other thing'" } ] ).



% Implementation notes:
%
% As it should be the case, base semantics and type management are done
% statically (hence once per type of dataflow block, thanks to block and
% instance managers for example).
%
% Dynamic, per-instance semantics and type management is only done if/when
% necessary, i.e. if (and when) port creations that are themselves dynamic and
% per-instance are triggered.


% Currently the support of compound value types is incomplete, especially
% regarding units and constraints; for example, only simple, "scalar" units can
% be specified (ex: 'km', not {'W', 'm'}) - whereas values may be compounded
% (ex: of type {float(), float()}). The same applies to constraints.
%
% Compound units (ex: {'A', 'm', 'km'}) may be supported in the future.





% Constructs a new dataflow block from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - BlockName is a human-readable name for that dataflow block (as a plain,
% non-empty string); not an identifier
%
% - InputPortSpecs is a list of the specifications of the input ports defined by
% this dataflow block
%
% - OutputPortSpecs is a list of the specifications of the output ports defined
% by this dataflow block
%
-spec construct( wooper:state(), class_Actor:actor_settings(), ustring(),
			[ input_port_spec() ], [ output_port_spec() ], dataflow_pid() ) ->
						wooper:state().
construct( State, ActorSettings, BlockName, InputPortSpecs, OutputPortSpecs,
		   DataflowPid ) ->

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize(BlockName) ),

	% Registering to the dataflow is to be done in all child classes, from their
	% onFirstDiasca/2 actor oneway.

	% Currently we perform very extended checking, both static (done once per
	% class) and dynamic (for each instance), even if, for most of the types of
	% dataflow blocks, all their instances are bound to either all match or none
	% at all these information.

	SemanticServerPid = class_SemanticServer:get_server(),
	TypeServerPid = class_TypeServer:get_server(),

	IdentificationServerPid = class_IdentificationServer:get_server(),

	% These registerings involve some per-instance checking (semantics, type,
	% etc.) that may be superfluous if the actual block did not implement the
	% get_port_specifications/0 static method.

	EmptyTable = table:new(),

	% Prepare for the use of register_{in,out}put_ports/4:
	BlanckPortState = setAttributes( ActorState, [
			{ input_ports, EmptyTable },
			{ input_iterations, EmptyTable },
			{ output_ports, EmptyTable },
			{ output_iterations, EmptyTable } ] ),

	{ InputTable, InputIterationTable } = register_input_ports(
			InputPortSpecs, SemanticServerPid, TypeServerPid, BlanckPortState ),

	{ OutputTable, OutputIterationTable } = register_output_ports(
		 OutputPortSpecs, SemanticServerPid, TypeServerPid, BlanckPortState ),

	% Checks that the port specs are consistent with the static information.
	% This checking may be optional:
	%
	check_static_consistency( InputTable, OutputTable, BlanckPortState ),

	% Then the class-specific actions:
	setAttributes( BlanckPortState, [

		{ input_ports, InputTable },
		{ input_iterations, InputIterationTable },

		{ output_ports, OutputTable },
		{ output_iterations, OutputIterationTable },

		{ run_status, active },

		{ dataflow_pid, DataflowPid },

		{ semantic_server_pid, SemanticServerPid },
		{ type_server_pid, TypeServerPid },
		{ identification_server_pid, IdentificationServerPid } ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?info_fmt( "Being deleted, while being in following state: ~ts.",
				[ to_string( State ) ] ),

	State.




% Methods section.



% Port registration section.



% Checks and registers the input ports (including iterations) of this dataflow
% block based on the provided specifications, and declares their underlying
% semantics, if any.
%
-spec register_input_ports( [ input_port_spec() ], semantic_server_pid(),
							type_server_pid(), wooper:state() ) ->
							   { input_port_table(), input_iteration_table() }.
register_input_ports( InputPortSpecs, SemanticServerPid, TypeServerPid,
					  State ) ->

	PortNames = [ IS#input_port_spec.name || IS <- InputPortSpecs ],

	case InputPortSpecs of

		[] ->
			?void( "Dataflow block not registering any overall input port "
				   "specification" );

		L ->
			?void_fmt( "Dataflow block registering ~B overall input port "
				"specification(s): ~ts",
				[ length( L ), text_utils:strings_to_string( PortNames ) ] )

	end,

	register_input_ports( InputPortSpecs, ?getAttr(input_ports),
		?getAttr(input_iterations), SemanticServerPid, TypeServerPid, State ).



% Helper:
register_input_ports( _InputPortSpecs=[], InputTable, InputIterationTable,
					  _SemanticServerPid, _TypeServerPid, _State ) ->
	% Specs exhausted, work done:
	{ InputTable, InputIterationTable };


% Not a port iteration here:
register_input_ports( [ _InputPortSpec=#input_port_spec{
							name=Name,
							comment=Comment,
							is_iteration=false,
							value_semantics=Semantics,
							value_unit=Unit,
							value_type_description=TypeDescription,
							value_constraints=Constraints } | T ],
					  InputTable, InputIterationTable, SemanticServerPid,
					  TypeServerPid, State ) ->

	%?debug_fmt( "Registering a new direct, standard input port: ~ts",
	%			[ input_port_spec_to_string( InputPortSpec ) ] ),

	PortName = validate_port_name( Name, State ),

	{ ValidatedComment, ValidatedSemantics, ValidatedUnit, ValidatedType,
	  ValidatedConstraints } = validate_port_specs( _PortSpecType=input_port,
			PortName, Comment, Semantics, Unit, TypeDescription, Constraints,
			SemanticServerPid, TypeServerPid, State ),


	% Other values are default ones:
	NewInputPort = #input_port{ comment=ValidatedComment,
								value_semantics=ValidatedSemantics,
								value_unit=ValidatedUnit,
								value_type=ValidatedType,
								value_constraints=ValidatedConstraints },

	NewInputTable = table:add_new_entry( _K=PortName, _V=NewInputPort,
									   InputTable ),

	register_input_ports( T, NewInputTable, InputIterationTable,
						  SemanticServerPid, TypeServerPid, State );


% Port iteration here, with default setting:
register_input_ports(
		  [ InputPortSpec=#input_port_spec{ is_iteration=true } | T ],
		  InputTable, InputIterationTable, SemanticServerPid, TypeServerPid,
		  State ) ->
	register_input_ports(
	  [ InputPortSpec#input_port_spec{ is_iteration=0 } | T ],
	  InputTable, InputIterationTable, SemanticServerPid, TypeServerPid,
	  State );


register_input_ports( [ InputPortSpec=#input_port_spec{
							name=Name,
							comment=Comment,
							is_iteration=IterationSpec,
							value_semantics=Semantics,
							value_unit=Unit,
							value_type_description=TypeDescription,
							value_constraints=Constraints } | T ],
					  InputTable, InputIterationTable, SemanticServerPid,
					  TypeServerPid, State ) ->

	?void_fmt( "Registering a new input port iteration: ~ts",
			   [ input_port_spec_to_string( InputPortSpec ) ] ),

	% These information will be used for the creation of all iterated ports
	% (initial or not):

	BaseIterPortName = validate_iteration_name( Name, State ),

	{ ValidatedComment, ValidatedSemantics, ValidatedUnit, ValidatedType,
	  ValidatedConstraints } = validate_port_specs(
			_PortSpecType=input_port_iteration, BaseIterPortName, Comment,
			Semantics, Unit, TypeDescription, Constraints, SemanticServerPid,
			TypeServerPid, State ),

	{ InitialCount, _Bounds } = CanonicalMultiplicity =
		validate_iteration( IterationSpec, State ),

	BlankIteration = #input_port_iteration{
						base_name=BaseIterPortName,
						comment=ValidatedComment,
						multiplicity=CanonicalMultiplicity,
						value_semantics=ValidatedSemantics,
						value_unit=ValidatedUnit,
						value_type=ValidatedType,
						value_constraints=ValidatedConstraints },

	{ NewInputTable, NewIteration } = case InitialCount of

		0 ->
			?void_fmt( "Input port iteration '~ts' created, with no initial "
					   "port.", [ Name ] ),
			{ InputTable, BlankIteration };

		_ ->
			{ NewIterPortNames, NewInpTable, NewIter } =
				create_input_iterated_ports( InitialCount, BlankIteration,
										InputTable, _OutputPortId=undefined ),
			?void_fmt( "Input port iteration '~ts' created, with ~B initial "
					   "ports: ~p", [ Name, InitialCount, NewIterPortNames ] ),

			{ NewInpTable, NewIter }

	end,

	NewInputIterationTable = table:add_new_entry( BaseIterPortName,
										  NewIteration, InputIterationTable ),

	register_input_ports( T, NewInputTable, NewInputIterationTable,
						  SemanticServerPid, TypeServerPid, State );


% Invalid case:
register_input_ports( _InputPortSpecs=[ FaultyInputPortSpecs | _T ],
		 _InputTable, _InputIterationTable, _SemanticServerPid,
		_TypeServerPid, State ) ->
	?error_fmt( "Invalid input port specification: ~p",
				[ FaultyInputPortSpecs ] ),
	throw( { invalid_input_port_spec, FaultyInputPortSpecs } ).



% Creates, if possible, the specified number of input ports using the specified
% iteration, and returns a list of their names.
%
-spec createInputIteratedPorts( wooper:state(), input_iteration_name(),
			   iterated_count() ) -> request_return( [ input_port_name() ] ).
createInputIteratedPorts( State, InputIterationName, PortCount ) ->

	InputIteration = get_input_port_iteration( InputIterationName, State ),

	InputTable = ?getAttr(input_ports),

	{ NewIteratedPortNames, NewInputTable, NewInputIteration } =
		create_input_iterated_ports( PortCount, InputIteration, InputTable,
									 _OutputPortId=undefined ),

	NewInputIterationTable = table:update_entry( InputIterationName,
					NewInputIteration, ?getAttr(input_iterations) ),

	NewState = setAttributes( State, [
					{ input_iterations, NewInputIterationTable },
					{ input_ports, NewInputTable } ] ),

	wooper:return_state_result( NewState, NewIteratedPortNames ).



% Creates, if possible, the specified number of output ports using the specified
% iteration, and returns a list of their names.
%
-spec createOutputIteratedPorts( wooper:state(), output_iteration_name(),
				iterated_count() ) -> request_return( [ output_port_name() ] ).
createOutputIteratedPorts( State, OutputIterationName, PortCount ) ->

	OutputIteration = get_output_port_iteration( OutputIterationName, State ),

	OutputTable = ?getAttr(output_ports),

	{ NewIteratedPortNames, NewOutputTable, NewOutputIteration } =
		create_output_iterated_ports( PortCount, OutputIteration, OutputTable ),

	NewOutputIterationTable = table:update_entry( OutputIterationName,
					NewOutputIteration, ?getAttr(output_iterations) ),

	NewState = setAttributes( State, [
					  { output_iterations, NewOutputIterationTable },
					  { output_ports, NewOutputTable } ] ),

	wooper:return_state_result( NewState, NewIteratedPortNames ).



% Checks and registers the output ports of this dataflow block based on the
% provided specifications, and declares their underlying semantics, if any.
%
% (helper)
%
-spec register_output_ports( [ output_port_spec() ], semantic_server_pid(),
							 type_server_pid(), wooper:state() ) ->
						{ output_port_table(), output_iteration_table() }.
register_output_ports( OutputPortSpecs, SemanticServerPid, TypeServerPid,
					   State ) ->

	PortNames = [ OS#output_port_spec.name || OS <- OutputPortSpecs ],

	case OutputPortSpecs of

		[] ->
			?void( "Dataflow block not registering any overall output port "
				   "specification" );

		L ->
			?void_fmt( "Dataflow block registering ~B overall output port "
				"specification(s): ~ts",
				[ length( L ), text_utils:strings_to_string( PortNames ) ] )

	end,

	register_output_ports( OutputPortSpecs, ?getAttr(output_ports),
		?getAttr(output_iterations), SemanticServerPid, TypeServerPid, State ).


% Helper:
register_output_ports( _OutputPortSpecs=[], OutputTable, OutputIterationTable,
					   _SemanticServerPid, _TypeServerPid, _State ) ->
	% Specs exhausted, work done:
	{ OutputTable, OutputIterationTable };

% Not a port iteration here:
register_output_ports( [ _OutputPortSpec=#output_port_spec{
							 name=Name,
							 comment=Comment,
							 is_iteration=false,
							 produces_result=ResultSettings,
							 value_semantics=Semantics,
							 value_unit=Unit,
							 value_type_description=TypeDescription,
							 value_constraints=Constraints } | T ],
					   OutputTable, OutputIterationTable, SemanticServerPid,
					   TypeServerPid, State ) ->

	%?debug_fmt( "Registering a new direct, standard output port: ~ts",
	%			[ output_port_spec_to_string( OutputPortSpec ) ] ),

	PortName = validate_port_name( Name, State ),

	{ ValidatedComment, ValidatedSemantics, ValidatedUnit, ValidatedType,
	  ValidatedConstraints } = validate_port_specs( _PortSpecType=output_port,
			PortName, Comment, Semantics, Unit, TypeDescription, Constraints,
			SemanticServerPid, TypeServerPid, State ),

	ValidatedResultSettings = validate_result_settings( ResultSettings, State ),

	% Other values are default ones:
	OutputPort = #output_port{
					comment=ValidatedComment,
					produces_result=ValidatedResultSettings,
					value_semantics=ValidatedSemantics,
					value_unit=ValidatedUnit,
					value_type=ValidatedType,
					value_constraints=ValidatedConstraints },

	NewOutputTable = table:add_new_entry( _K=PortName, _V=OutputPort,
										  OutputTable ),

	register_output_ports( T, NewOutputTable, OutputIterationTable,
						   SemanticServerPid, TypeServerPid, State );



% Port iteration here, with default setting:
register_output_ports(
		  [ OutputPortSpec=#output_port_spec{ is_iteration=true } | T ],
		  OutputTable, OutputIterationTable, SemanticServerPid, TypeServerPid,
		  State ) ->
	register_output_ports(
	  [ OutputPortSpec#output_port_spec{ is_iteration=0 } | T ],
	  OutputTable, OutputIterationTable, SemanticServerPid, TypeServerPid,
	  State );



% Port iteration here:
register_output_ports( [ OutputPortSpec=#output_port_spec{
							 name=Name,
							 comment=Comment,
							 is_iteration=IterationSpec,
							 produces_result=ResultSettings,
							 value_semantics=Semantics,
							 value_unit=Unit,
							 value_type_description=TypeDescription,
							 value_constraints=Constraints } | T ],
					   OutputTable, OutputIterationTable, SemanticServerPid,
					   TypeServerPid, State ) ->

	?void_fmt( "Registering a new output port iteration: ~ts",
			   [ output_port_spec_to_string( OutputPortSpec ) ] ),

	% These information will be used for the creation of all iterated ports
	% (initial or not):

	BasePortName = validate_iteration_name( Name, State ),


	{ ValidatedComment, ValidatedSemantics, ValidatedUnit, ValidatedType,
	  ValidatedConstraints } = validate_port_specs(
			_PortSpecType=output_port_iteration, BasePortName, Comment,
			Semantics, Unit, TypeDescription, Constraints, SemanticServerPid,
			TypeServerPid, State ),

	ValidatedResultSettings = validate_result_settings( ResultSettings, State ),

	{ InitialCount, _Bounds } = CanonicalMultiplicity =
		validate_iteration( IterationSpec, State ),

	BlankIteration = #output_port_iteration{
						base_name=BasePortName,
						comment=ValidatedComment,
						produces_result=ValidatedResultSettings,
						multiplicity=CanonicalMultiplicity,
						value_semantics=ValidatedSemantics,
						value_unit=ValidatedUnit,
						value_type=ValidatedType,
						value_constraints=ValidatedConstraints },

	{ NewOutputTable, NewIteration } = case InitialCount of

		0 ->
			?void_fmt( "Output port iteration '~ts' created, with no initial "
					   "port.", [ Name ] ),
			{ OutputTable, BlankIteration };

		_ ->

			{ NewIterPortNames, NewInpTable, NewIter } =
				create_output_iterated_ports( InitialCount, BlankIteration,
											  OutputTable ),

			?void_fmt( "Output port iteration '~ts' created, with ~B initial "
					   "ports: ~p", [ Name, InitialCount, NewIterPortNames ] ),

			{ NewInpTable, NewIter }

	end,

	NewOutputIterationTable = table:add_new_entry( BasePortName, NewIteration,
												   OutputIterationTable ),

	register_output_ports( T, NewOutputTable, NewOutputIterationTable,
						   SemanticServerPid, TypeServerPid, State );


register_output_ports( _OutputPortSpecs=[ FaultyOutputPortSpecs | _T ],
		_OutputTable, _OutputIterationTable, _SemanticServerPid, _TypeServerPid,
		State ) ->

	?error_fmt( "Invalid output port specification: ~p",
				[ FaultyOutputPortSpecs ] ),

	throw( { invalid_output_port_spec, FaultyOutputPortSpecs } ).




% Validates the specified port specifications (common for input or output ones,
% iterations or not).
%
-spec validate_port_specs( port_spec_type(), port_string_name(),
			maybe( port_comment() ), user_value_semantics(),
			unit_utils:unit_string(), value_type_description(),
			value_constraints(), semantic_server_pid(), type_server_pid(),
			wooper:state() ) ->
		{ maybe( internal_comment() ), value_semantics(), value_unit(),
		  value_type(), value_constraints() }.
validate_port_specs( PortType, PortName, Comment, Semantics, Unit,
		TypeDescription, Constraints, SemanticServerPid, TypeServerPid,
		State ) ->

	% Validation exceptions are intercepted (and rethrown) to be able to emit
	% more informative traces.

	ValidatedComment = try validate_comment( Comment, State )

					   catch

		throw:CommentError ->

			?error_fmt( "Error while validating the comment '~ts' of ~ts: ~p",
				[ Comment, get_port_textual_description( PortType, PortName ),
				  CommentError ] ),

			throw( CommentError )

	end,


	ValidatedConstraints = try

		validate_constraints( Constraints, State )

						   catch

		throw:ConstraintError ->

			ConstraintString =
				dataflow_support:value_constraint_to_string( Constraints ),

			?error_fmt( "Error while validating the constraint '~ts' "
				"of ~ts: ~p", [ ConstraintString,
				  get_port_textual_description( PortType, PortName ),
				  ConstraintError ] ),

			throw( ConstraintError )

	end,

	ValidatedUnit = try validate_unit( Unit, State )

					catch

		throw:UnitError ->

			?error_fmt( "Error while validating the unit '~ts' of ~ts: ~p",
				[ Unit, get_port_textual_description( PortType, PortName ),
				  UnitError ] ),

			throw( UnitError )

	end,


	ValidatedSemantics = try

					validate_semantics( Semantics, SemanticServerPid, State )

						 catch

		throw:SemanticError ->

			% Warns in case of wrong semantics, yet does not fail.
			%
			% (or make that an option for development, not production)
			%
			?error_fmt( "Error while validating the semantics '~p' of ~ts:~n~p",
				[ Semantics, get_port_textual_description( PortType, PortName ),
				  SemanticError ] ),

			Semantics
			%throw( SemanticError )

	end,

	ValidatedType = try

		validate_type_description( TypeDescription, TypeServerPid, State )

	catch

		throw:TypeError ->
			?error_fmt( "Error while validating the type description '~ts' of "
				"~ts: ~p", [ TypeDescription,
				get_port_textual_description( PortType, PortName ),
				TypeError ] ),
			throw( TypeError )

	end,

	{ ValidatedComment, ValidatedSemantics, ValidatedUnit, ValidatedType,
	  ValidatedConstraints }.



% Describes specified designated port.
-spec get_port_textual_description( port_spec_type(), port_string_name() ) ->
										static_return( text_utils:ustring() ).
get_port_textual_description( _PortType=input_port_iteration, PortName ) ->
	wooper:return_static(
		text_utils:format( "input port iteration '~ts'", [ PortName ] ) );

get_port_textual_description( _PortType=output_port_iteration, PortName ) ->
	wooper:return_static(
		text_utils:format( "output port iteration '~ts'", [ PortName ] ) );

get_port_textual_description( _PortType=input_port, PortName ) ->
	wooper:return_static(
		text_utils:format( "input port '~ts'", [ PortName ] ) );

get_port_textual_description( _PortType=output_port, PortName ) ->
	wooper:return_static(
		text_utils:format( "output port '~ts'", [ PortName ] ) ).



% Connects synchronously and directly (i.e. based on a direct request, not on an
% actor message - thus to be done only initially, before the simulation is
% running) the specified output port of this dataflow block to the specified
% input port of the specified (downstream) dataflow block.
%
-spec connectOutputPortInitially( wooper:state(), output_port_name(),
  block_pid(), input_port_name() ) -> request_return( 'output_port_connected' ).
connectOutputPortInitially( State, OutputPortBinName, DownstreamBlockPid,
							InputPortBinName ) ->

	% As not based on actor messages:
	false = class_Actor:is_running( State ),

	?void_fmt( "Creating an initial channel from local output port ~w:~ts to "
		"(downstream) input port ~w:~ts.",
		[ self(), OutputPortBinName, DownstreamBlockPid, InputPortBinName ] ),

	OutputPortTable = ?getAttr(output_ports),

	% Will be the source endpoint of the channel:
	OutputPort = case table:lookup_entry( OutputPortBinName,
										  OutputPortTable ) of

		{ value, OPort } ->
			OPort;

		key_not_found ->
			?error_fmt( "Initial connection request from (local) output port "
				"'~ts' (to ~w:~ts) rejected, as this port does not "
				"exist, knowing that ~ts",
				[ OutputPortBinName, DownstreamBlockPid, InputPortBinName,
				  list_output_ports( OutputPortTable ) ] ),

			throw( { output_port_not_found,
					 text_utils:binary_to_string( OutputPortBinName ) } )

	end,

	OutputPortDescription = get_port_description( OutputPort, State ),

	% This block is both the requester and the target here:
	DownstreamBlockPid ! { requestConnectionToInputPortInitially,
	  [ InputPortBinName, self(), OutputPortBinName, OutputPortDescription ],
	  self() },

	% While waiting for the answer, updating this upstream block:

	InputPortId = { DownstreamBlockPid, InputPortBinName },

	NewFedPorts = [ InputPortId | OutputPort#output_port.fed_ports ],

	NewOutputPort = OutputPort#output_port{ fed_ports=NewFedPorts },

	NewOutputPortTable = table:update_entry( OutputPortBinName, NewOutputPort,
											 OutputPortTable ),

	NewState = setAttribute( State, output_ports, NewOutputPortTable ),

	% Interleaved answer from requestConnectionToInputPortInitially/5:
	receive

		{ wooper_result, input_port_connected } ->

			PortStrings = [ text_utils:format( "~w:~ts", [ B, P ] )
							|| { B, P } <- NewFedPorts ],

			?void_fmt( "Output port '~ts' now feeding following ~B input "
				"port(s): ~ts",
				[ OutputPortBinName, length( PortStrings ),
				  text_utils:strings_to_string( PortStrings ) ] ),

			wooper:return_state_result( NewState, output_port_connected )

	end.



% Connects synchronously and directly (i.e. based on a direct request, not on an
% actor message - thus to be done initially) the specified output port of the
% specified (upstream) dataflow block to the specified (local) input port of
% this dataflow block, checking thanks to their description whether they are
% compliant (i.e. whether they can form a channel).
%
% Typically called by the connectOutputPortInitially/4 method of the upstream
% dataflow block.
%
-spec requestConnectionToInputPortInitially( wooper:state(), input_port_name(),
		block_pid(), output_port_name(), port_description() ) ->
							request_return( 'input_port_connected' ).
requestConnectionToInputPortInitially( State, InputPortBinName,
			  UpstreamBlockPid, OutputPortBinName, OutputPortDescription ) ->

	% As not based on actor messages:
	false = class_Actor:is_running( State ),

	SenderPid = ?getSender(),

	?void_fmt( "Received from process ~w a connection request from (upstream) "
		"output port ~w:~ts to local input port '~ts'; this upstream "
		"output port is a ~ts",
		[ SenderPid, UpstreamBlockPid, OutputPortBinName, InputPortBinName,
		  port_description_to_string( OutputPortDescription ) ] ),

	InputPortTable = ?getAttr(input_ports),

	ConnectedState = connect_to_input_port( InputPortBinName, InputPortTable,
			UpstreamBlockPid, OutputPortBinName, OutputPortDescription,
			_InitiatorInfo=SenderPid, State ),

	wooper:return_state_result( ConnectedState, input_port_connected ).



% Connects (thanks to an actor message, hence when the simulation is running)
% the specified (local, standard) output port of this block to specified
% (remote, standard) input port of specified downstream block.
%
% This downstream block will trigger back the onChannelCreated/4 actor oneway of
% the caller of this first oneway, once done.
%
-spec connectOutputPort( wooper:state(), output_port_name(),
		{ downstream_block_pid(), input_port_name() }, action_id(),
		sending_actor_pid() ) -> actor_oneway_return().
connectOutputPort( State, OutputPortBinName,
	{ DownstreamBlockPid, InputPortBinName }, ActionId, SendingActorPid )
  when is_binary( OutputPortBinName ) andalso is_pid( DownstreamBlockPid )
	   andalso is_binary( InputPortBinName ) andalso is_integer( ActionId ) ->

	% Note: to keep in sync with the significantly similar operations performed
	% by connectOutputPortInitially/4 above.

	?void_fmt( "Connecting (local) output port '~ts' to the (downstream) "
		"input port '~ts' of block ~w, on behalf of actor ~w, "
		"for action #~B.",
		[ OutputPortBinName, InputPortBinName, DownstreamBlockPid,
		  SendingActorPid, ActionId ] ),

	OutputPortTable = ?getAttr(output_ports),

	% Will be the source endpoint of the channel:
	OutputPort = case table:lookup_entry( OutputPortBinName,
										  OutputPortTable ) of

		{ value, OPort } ->
			OPort;

		key_not_found ->
			?error_fmt( "Connection request from (local) output port '~ts' "
				"(to ~w:~ts) rejected, as this port does not exist, "
				"knowing that ~ts",
				[ OutputPortBinName, DownstreamBlockPid, InputPortBinName,
				  list_output_ports( OutputPortTable ) ] ),

			throw( { output_port_not_found,
					 text_utils:binary_to_string( OutputPortBinName ) } )

	end,

	OutputPortDescription = get_port_description( OutputPort, State ),

	ListenerActorPid = SendingActorPid,

	Oneway = { requestConnectionToInputPort, [ InputPortBinName,
				OutputPortBinName, OutputPortDescription, ListenerActorPid,
				ActionId ] },

	SentState =
		class_Actor:send_actor_message( DownstreamBlockPid, Oneway, State ),

	% As standard ports are used here (notably: no downstream iteration), we can
	% directly record that channel creation (the downstream block will thus
	% directly notify the listener, without having to use this upstream block as
	% an intermediate):

	InputPortId = { DownstreamBlockPid, InputPortBinName },

	FedPorts = OutputPort#output_port.fed_ports,

	case lists:member( InputPortId, FedPorts ) of

		true ->
			throw( { input_port_already_connected, InputPortId, FedPorts } );

		false ->
			ok

	end,

	NewFedPorts = [ InputPortId | FedPorts ],

	?void_fmt( "Now this (local) output port '~ts' is feeding following "
		"~B port identifiers: ~ts", [ OutputPortBinName, length( NewFedPorts ),
		text_utils:strings_to_string(
		  [ dataflow_support:port_id_to_string( PortId )
			|| PortId <- NewFedPorts ] ) ] ),

	NewOutputPort = OutputPort#output_port{ fed_ports=NewFedPorts },

	NewOutputPortTable = table:update_entry( OutputPortBinName, NewOutputPort,
											 OutputPortTable ),

	FinalState = setAttribute( SentState, output_ports, NewOutputPortTable ),

	actor:return_state( FinalState ).



% Connects (thanks to an actor message, hence when the simulation is running)
% the specified (local, output) standard ports and iterations of this block to
% the specified (remote, input) standard ports and iterations of the specified
% downstream block.
%
% Typically called by a unit manager, in order to connect a unit to a dataflow
% object, or vice-versa.
%
% Will trigger back (from the ultimately called onInboundConnectionsCreated/5
% method) the onConnectionsCreated/4 actor oneway of the caller of this first
% oneway, once done.
%
-spec connectToDownstreamBlock( wooper:state(), [ canonical_connection_spec() ],
		downstream_block_pid(), action_id(), sending_actor_pid() ) ->
									actor_oneway_return().
connectToDownstreamBlock( State, ConnectionSpecs, DownstreamBlockPid, ActionId,
						  SendingActorPid ) ->

	?info_fmt( "Connecting to downstream block ~w, on behalf of actor ~w, "
		"for action #~B, based on ~ts",
		[ DownstreamBlockPid, SendingActorPid, ActionId,
		  class_DataflowUnitManager:connection_specs_to_string(
			ConnectionSpecs ) ] ),

	% For all specified output port iterations, we create a local, iterated
	% (output) port first, so that from here we deal only with standard output
	% ports afterwards.

	{ IterConnectionSpecs, IterState } =
		instantiate_output_iterated_ports( ConnectionSpecs, State ),

	% As a result, in all cases we ultimately connect only local, (now) standard
	% output ports to the downstream block.

	% First, let's check that all output ports exist locally, and gather their
	% descriptions in a list of inbound_connection_info() elements to be sent to
	% the downstream unit:
	%
	InboundConnectionInfos =
		get_inbound_connection_infos( IterConnectionSpecs, IterState ),

	% We do not update here the local output ports (ex: regarding fed ports)
	% from their downstream counterparts as, when connecting to input port
	% iterations, the name of the actual input ports cannot be anticipated here
	% (they will be only known when the downstream block replies with an
	% onInboundConnectionsCreated/5 call).

	ListenerActorPid = SendingActorPid,

	% So will trigger back an onInboundConnectionsCreated/5 call:
	?debug_fmt( "Requesting inbound connection to the downstream block ~w "
		"regarding ~ts", [ DownstreamBlockPid,
			inbound_connection_infos_to_string( InboundConnectionInfos ) ] ),

	Oneway = { requestInboundConnections,
			   [ InboundConnectionInfos, ListenerActorPid, ActionId ] },

	SentState = class_Actor:send_actor_message( DownstreamBlockPid, Oneway,
												IterState ),

	actor:return_state( SentState ).



% Instantiates the output iterated ports from the output port iteration found in
% specified connection specification.
%
% Returns an updated connection specification where said iterations have been
% replaced by the corresponding instantiated output port, and a state updated
% accordingly.
%
% (helper)
%
-spec instantiate_output_iterated_ports( [ canonical_connection_spec() ],
		wooper:state() ) -> { [ canonical_connection_spec() ], wooper:state() }.
instantiate_output_iterated_ports( ConnectionSpecs, State ) ->

	OutputPortTable = ?getAttr(output_ports),
	OutputIterationTable = ?getAttr(output_iterations),

	instantiate_output_iterated_ports( ConnectionSpecs, OutputPortTable,
							OutputIterationTable, _AccConnSpec=[], State ).


% (helper)
instantiate_output_iterated_ports( _ConnectionSpecs=[], OutputPortTable,
								   OutputIterationTable, AccConnSpec, State ) ->

	OutputState = setAttributes( State, [
		{ output_ports, OutputPortTable },
		{ output_iterations, OutputIterationTable } ] ),

	{ AccConnSpec, OutputState };


% Output iteration found, thus must be instantiated:
instantiate_output_iterated_ports( _ConnectionSpecs=[
		{ { output_iteration_name, OutputIterationBinName }, InputPair } | T ],
		OutputPortTable, OutputIterationTable, AccConnSpec, State ) ->

	% Let's create an (output) iterated port from this iteration:
	OutputIteration = case table:lookup_entry( OutputIterationBinName,
											   OutputIterationTable ) of

		{ value, Iter } ->
			Iter;

		key_not_found ->
			?error_fmt( "Request to instantiate a non-existing output "
				"iteration '~ts', whereas ~ts", [ OutputIterationBinName,
				list_output_iterations( OutputIterationTable ) ] ),

			throw( { output_iteration_not_found,
					 text_utils:binary_to_string( OutputIterationBinName ) } )

	end,

	% A single output port is needed from this iteration:
	{ [ NewIteratedOutputPortName ], NewOutputPortTable, NewOutputIteration } =
		create_output_iterated_ports( _PortCount=1, OutputIteration,
									  OutputPortTable ),

	?void_fmt( "Instantiated output port iteration '~ts': new (output) "
		"iterated port '~ts' created.",
		[ OutputIterationBinName, NewIteratedOutputPortName ] ),

	NewOutputIterationTable = table:update_entry( OutputIterationBinName,
									NewOutputIteration, OutputIterationTable ),

	OutputPair = { output_port_name, NewIteratedOutputPortName },

	NewAccConnSpec = [ { OutputPair, InputPair } | AccConnSpec ],

	instantiate_output_iterated_ports( T, NewOutputPortTable,
		NewOutputIterationTable, NewAccConnSpec, State );


% By design just a standard port, leave it as it is:
instantiate_output_iterated_ports( _ConnectionSpecs=[
  ConnSpec={ { output_port_name, _OutputIterationBinName }, _InputPair } | T ],
  OutputPortTable, OutputIterationTable, ConnSpecAcc, State ) ->

	instantiate_output_iterated_ports( T, OutputPortTable,
		OutputIterationTable, [ ConnSpec | ConnSpecAcc ], State ).




% Returns the inbound connection information suitable for the downstream block,
% from the specified overall, more general connection specs.
%
% Checks that the specified local output ports exist, and gathers their port
% description.
%
-spec get_inbound_connection_infos( [ canonical_connection_spec() ],
					wooper:state() ) -> [ inbound_connection_info() ].
get_inbound_connection_infos( ConnectionSpecs, State ) ->

	OutputPortTable = ?getAttr(output_ports),

	get_inbound_connection_infos( ConnectionSpecs, _AccConnInfo=[],
								  OutputPortTable, State ).


% (helper)
get_inbound_connection_infos( _ConnectionSpecs=[], AccConnInfo,
							  _OutputPortTable, _State ) ->
	% Order does not matter:
	AccConnInfo;

get_inbound_connection_infos( _ConnectionSpecs=[
		  { { output_port_name, OutputPortBinName }, DownstreamPortSpec } | T ],
							  AccConnInfo, OutputPortTable, State ) ->

	% Will be the source endpoint of the channel:
	OutputPort = case table:lookup_entry( OutputPortBinName,
										  OutputPortTable ) of

		{ value, OPort } ->
			OPort;

		key_not_found ->
			?error_fmt( "Connection request from (local) output port '~ts' "
				"(to downstream ~ts) rejected, as this local port "
				"does not exist, whereas ~ts",
				[ OutputPortBinName,
				  class_DataflowUnitManager:downstream_spec_to_string(
					DownstreamPortSpec ),
				  list_output_ports( OutputPortTable ) ] ),

			throw( { output_port_not_found,
					 text_utils:binary_to_string( OutputPortBinName ) } )

	end,

	OutputPortDescription = get_port_description( OutputPort, State ),

	% Forges a new inbound_connection_info():
	NewInboundConnInfo = { OutputPortBinName, OutputPortDescription,
						   DownstreamPortSpec },

	get_inbound_connection_infos( T, [ NewInboundConnInfo | AccConnInfo ],
								  OutputPortTable, State ).




% Connects (thanks to an actor message, hence when the simulation is running)
% the specified output port of the caller (an upstream dataflow block) to the
% specified (local) input port of this dataflow block, checking thanks to their
% description whether they are compliant (i.e. whether they can form a channel).
%
% Typically called by the connectOutputPort/5 actor oneway of an upstream block.
%
% The specified listening actor (typically the one that initiated the channel
% creation) will be notified of the availability of that channel: its
% onChannelCreated/4 actor oneway will be called.
%
-spec requestConnectionToInputPort( wooper:state(), input_port_name(),
		output_port_name(), port_description(), actor_pid(),
		action_id(), sending_actor_pid() ) -> actor_oneway_return().
requestConnectionToInputPort( State, InputPortBinName, OutputPortBinName,
		OutputPortDescription, ListenerActorPid, ActionId, UpstreamBlockPid ) ->

	% Note: to keep in sync with the significantly similar operations performed
	% by requestConnectionToInputPortInitially/5 above.

	?void_fmt( "Received from upstream block ~w a connection request from "
		"its '~ts' output port to (local) input port '~ts', in the context "
		"of action #~B; this upstream output port is a ~ts",
		[ UpstreamBlockPid, OutputPortBinName, InputPortBinName, ActionId,
		  port_description_to_string( OutputPortDescription ) ] ),

	InputPortTable = ?getAttr(input_ports),

	ConnectedState = connect_to_input_port( InputPortBinName, InputPortTable,
		UpstreamBlockPid, OutputPortBinName, OutputPortDescription,
		_InitiatorInfo=direct, State ),

	% Notifies then the specified listener:
	OutputPortId = { UpstreamBlockPid, OutputPortBinName },
	InputPortId = { self(), InputPortBinName },

	Oneway = { onChannelCreated, [ OutputPortId, InputPortId, ActionId ] },

	SentState = class_Actor:send_actor_message( ListenerActorPid, Oneway,
												ConnectedState ),

	actor:return_state( SentState ).



% Requests (thanks to an actor message, hence when the simulation is running)
% inbound port connections to be made on this (downstream) block.
%
% Typically called from the connectToDownstreamBlock/5 oneway of an upstream
% block.
%
% ListenerActorPid and ActionId are just received and passed along.
%
-spec requestInboundConnections( wooper:state(), [ inbound_connection_info() ],
	   actor_pid(), action_id(), sending_actor_pid() ) -> actor_oneway_return().
requestInboundConnections( State, InboundConnectionInfos, ListenerActorPid,
						   ActionId, UpstreamBlockPid ) ->

	% So we have in InboundConnectionInfos the descriptions of the channels to
	% create. Let's apply this here, and notify back the caller.

	?debug_fmt( "Received from upstream block ~w a connection request, "
		"in the context of action #~B, as ~ts",
		[ UpstreamBlockPid, ActionId,
		  inbound_connection_infos_to_string( InboundConnectionInfos ) ] ),

	% Gets the newer, corresponding pairs of standard ports:
	{ PortPairs, AppliedState } =
		apply_connections( UpstreamBlockPid, InboundConnectionInfos, State ),

	% Allows notably to notify the upstream block of the actual input ports
	% instantiated (if any):
	%
	Oneway = { onInboundConnectionsCreated,
			   [ PortPairs, ListenerActorPid, ActionId ] },

	SentState = class_Actor:send_actor_message( UpstreamBlockPid, Oneway,
												AppliedState ),

	actor:return_state( SentState ).



% (helper)
-spec apply_connections( upstream_block_pid(), [ inbound_connection_info() ],
					wooper:state() ) -> { [ port_pair() ], wooper:state() }.
apply_connections( UpstreamBlockPid, InboundConnectionInfo, State ) ->

	InputPortTable = ?getAttr(input_ports),
	InputIterationTable = ?getAttr(input_iterations),

	apply_connections( UpstreamBlockPid, InboundConnectionInfo, InputPortTable,
					   InputIterationTable, _AccPortPairs=[], State ).



% (helper)
-spec apply_connections( upstream_block_pid(), [ inbound_connection_info() ],
			input_port_table(), input_iteration_table(),
			[ port_pair() ], wooper:state() ) ->
	   { [ port_pair() ], input_port_table(), input_iteration_table() }.
apply_connections( _UpstreamBlockPid, _InboundConnectionInfo=[], InputPortTable,
				   InputIterationTable, AccPortPairs, State ) ->

	SetState = setAttributes( State, [
					{ input_ports, InputPortTable },
					{ input_iterations, InputIterationTable } ] ),

	{ AccPortPairs, SetState };


% Targeting a standard input port here:
apply_connections( UpstreamBlockPid, _InboundConnectionInfo=[
		{ OutputPortBinName, OutputPortDescription,
		  { input_port_name, InputPortBinName } } | T ],
				   InputPortTable, InputIterationTable, AccPortPairs, State ) ->

	?void_fmt( "Received from upstream block ~w a connection request from "
		"its '~ts' output port to (local) input port '~ts'; this "
		"upstream output port is a ~ts",
		[ UpstreamBlockPid, OutputPortBinName, InputPortBinName,
		  port_description_to_string( OutputPortDescription ) ] ),

	ConnectedState = connect_to_input_port( InputPortBinName, InputPortTable,
		UpstreamBlockPid, OutputPortBinName, OutputPortDescription,
		_InitiatorInfo=direct, State ),

	NewInputPortTable = getAttribute( ConnectedState, input_ports ),

	NewAccPortPairs =
		[ { OutputPortBinName, InputPortBinName } | AccPortPairs ],

	apply_connections( UpstreamBlockPid, T, NewInputPortTable,
					   InputIterationTable, NewAccPortPairs, ConnectedState );


% Targeting an input iteration port here:
apply_connections( UpstreamBlockPid, _InboundConnectionInfo=[
		 { OutputPortBinName, OutputPortDescription,
			{ input_iteration_name, InputIterationBinName } } | T ],
				   InputPortTable, InputIterationTable, AccPortPairs, State ) ->

	?void_fmt( "Received from upstream block ~w a connection request from "
		"its '~ts' output port to (local) input iteration '~ts'; this "
		"upstream output port is a ~ts", [ UpstreamBlockPid,
			OutputPortBinName, InputIterationBinName,
			port_description_to_string( OutputPortDescription ) ] ),

	% An iterated port is created from said port iteration, so that it can be
	% the target endpoint of the channel:
	%
	InputIteration = case table:lookup_entry( InputIterationBinName,
											 InputIterationTable ) of

		{ value, Iter } ->
			Iter;

		key_not_found ->
			?error_fmt( "Request to connect a non-existing input iteration "
				"'~ts', whereas ~ts", [ InputIterationBinName,
				list_input_iterations( InputIterationTable ) ] ),

			throw( { input_iteration_not_found,
					 text_utils:binary_to_string( InputIterationBinName ) } )

	end,

	% Now we create a (single) standard port out of it, and we update that
	% iteration accordingly:
	%
	OutputPortId = { UpstreamBlockPid, OutputPortBinName },

	{ [ NewIteratedInputPortName ], NewInputPortTable,
	  NewInputIteration } = create_input_iterated_ports( _PortCount=1,
								InputIteration, InputPortTable, OutputPortId ),

	?void_fmt( "Instantiated input port iteration '~ts': new (input) "
		"iterated port '~ts' created.",
		[ InputIterationBinName, NewIteratedInputPortName ] ),

	NewInputIterationTable = table:update_entry( InputIterationBinName,
									NewInputIteration, InputIterationTable ),

	NewAccPortPairs = [ { OutputPortBinName, NewIteratedInputPortName }
						| AccPortPairs ],

	apply_connections( UpstreamBlockPid, T, NewInputPortTable,
					   NewInputIterationTable, NewAccPortPairs, State ).



% Called by a downstream block (typically from its requestInboundConnections/5
% method) when its inbound connections have been created.
%
-spec onInboundConnectionsCreated( wooper:state(), [ connection_info() ],
	   actor_pid(), action_id(), sending_actor_pid() ) -> actor_oneway_return().
onInboundConnectionsCreated( State, PortPairs, ListenerActorPid,
							 ActionId, DownstreamBlockPid ) ->

	?info_fmt( "Notified by downstream block ~w, for action #~B, "
		"that ~B channels have been created, corresponding to "
		"following 'local output'/'remote input' port pairs: ~ts",
		[ DownstreamBlockPid, ActionId, length( PortPairs ),
		  port_pairs_to_string( PortPairs ) ] ),

	% Note that the validity of connections (i.e. the compliance of each
	% output/input port pair involved) has been already checked by the
	% downstream block.

	% First, let's record the consequences onto the local output ports; it boils
	% down to recording to which input ports each of them is now connected:
	%
	ConnectedState = record_downstream_channels( DownstreamBlockPid,
												 PortPairs, State ),

	% Then this upstream bock notifies back the listener:
	Oneway = { onConnectionsCreated,
			   [ PortPairs, DownstreamBlockPid, ActionId ] },

	SentState = class_Actor:send_actor_message( ListenerActorPid, Oneway,
												ConnectedState ),

	?info_fmt( "Connection to downstream block ~w, for action #~B, granted "
		"for following local output/remote input port pairs: ~ts",
		[ DownstreamBlockPid, ActionId, port_pairs_to_string( PortPairs ) ] ),

	actor:return_state( SentState ).



% Records the specified downstream channels.
-spec record_downstream_channels( downstream_block_pid(), [ connection_info() ],
								  wooper:state() ) -> wooper:state().
record_downstream_channels( DownstreamBlockPid, PortPairs, State ) ->

	OutputPortTable = ?getAttr(output_ports),

	record_downstream_channels( DownstreamBlockPid, PortPairs,
								OutputPortTable, State ).


% (helper)
record_downstream_channels( _DownstreamBlockPid, _PortPairs=[],
							OutputPortTable, State ) ->
	setAttribute( State, output_ports, OutputPortTable );

record_downstream_channels( DownstreamBlockPid,
		_PortPairs=[ { OutputPortBinName, InputPortBinName } | T ],
		OutputPortTable, State ) ->

	OutputPort = case table:lookup_entry( OutputPortBinName,
										  OutputPortTable ) of

		{ value, OPort } ->
			OPort;

		key_not_found ->
			?error_fmt( "Connection notification from (local) output port "
				"'~ts' (to ~w:~ts) rejected, as this port does not exist, "
				"knowing that ~ts.",
				[ OutputPortBinName, DownstreamBlockPid, InputPortBinName,
				  list_output_ports( OutputPortTable ) ] ),

			throw( { output_port_not_found,
					 text_utils:binary_to_string( OutputPortBinName ) } )

	end,

	?void_fmt( "Now (local) output port '~ts' is connected to input port "
		"~w:~ts.",
		[ OutputPortBinName, DownstreamBlockPid, InputPortBinName ] ),

	InputPortId = { DownstreamBlockPid, InputPortBinName },

	NewFedPorts = [ InputPortId | OutputPort#output_port.fed_ports ],

	NewOutputPort = OutputPort#output_port{ fed_ports=NewFedPorts },

	NewOutputPortTable = table:update_entry( OutputPortBinName, NewOutputPort,
											 OutputPortTable ),

	record_downstream_channels( DownstreamBlockPid, T, NewOutputPortTable,
								State ).



% Creates the specified number of input iterated ports, based on the specified
% iteration, designated by its name (as a binary string).
%
% Returns a list of the names of the created iterated input ports, and a
% corresponding updated state for that dataflow block.
%
-spec create_input_iterated_ports( port_count(), input_port_iteration(),
								   input_port_table(), output_port_id() ) ->
	   { [ input_port_name() ], input_port_table(), input_port_iteration() }.
create_input_iterated_ports( CreationCount,
							 InputIteration=#input_port_iteration{
											   multiplicity=Multiplicity,
											   port_indexes=Indexes },
							 InputPortTable,
							 OutputPortId ) ->

	{ NewPortCount, _Bounds } = NewMultiplicity =
		update_multiplicity( CreationCount, Multiplicity ),

	{ NewIndexes, AllIndexes } = insert_indexes( CreationCount, Indexes ),

	% Just a basic checking:
	NewPortCount = length( AllIndexes ),

	% List of {Name, InputPort} pairs:
	NewPortEntries = [ create_input_iterated_entry( I, InputIteration,
									OutputPortId ) || I <- NewIndexes ],

	NewInputPortTable = table:add_new_entries( NewPortEntries, InputPortTable ),

	NewInputIteration = InputIteration#input_port_iteration{
							multiplicity=NewMultiplicity,
							port_indexes=AllIndexes },

	CreatedPortNames = [ Name || { Name, _Port } <- NewPortEntries ],

	{ CreatedPortNames, NewInputPortTable, NewInputIteration }.



% Returns an input port entry created from specified iterated index and input
% port specification.
%
-spec create_input_iterated_entry( iterated_index(), input_port_iteration(),
					output_port_id() ) -> { input_port_name(), input_port() }.
create_input_iterated_entry( Index, #input_port_iteration{
									   base_name=BaseName,
									   comment=Comment,
									   value_semantics=Semantics,
									   value_unit=Unit,
									   value_type=Type,
									   value_constraints=Constraints },
							 OutputPortId ) ->

	PortName = get_iterated_port_name( BaseName, Index ),

	% Direct copy, since already validated at iteration definition:
	NewIteratedInputPort = #input_port{ comment=Comment,
										value_semantics=Semantics,
										value_unit=Unit,
										value_type=Type,
										value_constraints=Constraints,
										feeder_port=OutputPortId },

	{ PortName, NewIteratedInputPort }.



% Creates the specified number of output iterated ports, based on the specified
% iteration, designated by its name (as a binary string).
%
% Returns a list of the names of the created iterated output ports, and a
% corresponding updated state for that dataflow block.
%
-spec create_output_iterated_ports( port_count(), output_port_iteration(),
									output_port_table() ) ->
	   { [ output_port_name() ], output_port_table(), output_port_iteration() }.
create_output_iterated_ports( CreationCount,
							  OutputIteration=#output_port_iteration{
												 multiplicity=Multiplicity,
												 port_indexes=Indexes },
							  OutputPortTable ) ->

	{ NewPortCount, _Bounds } = NewMultiplicity =
		update_multiplicity( CreationCount, Multiplicity ),

	{ NewIndexes, AllIndexes } = insert_indexes( CreationCount, Indexes ),

	% Just a basic checking:
	NewPortCount = length( AllIndexes ),

	% List of {Name,OutputPort} pairs::
	NewPortEntries = [ create_output_iterated_entry( I, OutputIteration )
					   || I <- NewIndexes ],

	NewOutputPortTable = table:add_new_entries( NewPortEntries,
												OutputPortTable ),

	NewOutputIteration = OutputIteration#output_port_iteration{
						   multiplicity=NewMultiplicity,
						   port_indexes=AllIndexes },

	CreatedPortNames = [ Name || { Name, _Port } <- NewPortEntries ],

	{ CreatedPortNames, NewOutputPortTable, NewOutputIteration }.



% Returns an output port entry created from specified iterated index and output
% port specification.
%
-spec create_output_iterated_entry( iterated_index(),
			output_port_iteration() ) -> { output_port_name(), output_port() }.
create_output_iterated_entry( Index, #output_port_iteration{
										base_name=BaseName,
										comment=Comment,
										produces_result=ResultSettings,
										value_semantics=Semantics,
										value_unit=Unit,
										value_type=Type,
										value_constraints=Constraints } ) ->

	PortName = get_iterated_port_name( BaseName, Index ),

	% Direct copy, since already validated at iteration definition:
	NewIteratedOutputPort = #output_port{ comment=Comment,
										  produces_result=ResultSettings,
										  value_semantics=Semantics,
										  value_unit=Unit,
										  value_type=Type,
										  value_constraints=Constraints },

	{ PortName, NewIteratedOutputPort }.



% Returns the name of the iterated port having specified base name and port
% index.
%
-spec get_iterated_port_name( iteration_name(), iterated_index() ) ->
									port_name().
get_iterated_port_name( IterationName, Index ) ->

	PortName = text_utils:format( "~ts~ts~B",
						[ IterationName, ?iterated_port_token, Index ] ),

	text_utils:string_to_binary( PortName ).



% Updates the specified iteration multiplicity with the specified number of
% iterated ports to be created.
%
-spec update_multiplicity( port_count(), iteration_multiplicity() ) ->
									iteration_multiplicity().
update_multiplicity( PortCreationCount,
					 { Current, Bounds={ _Min, _Max=unbounded } } ) ->
	{ Current + PortCreationCount, Bounds };

update_multiplicity( PortCreationCount, { Current, Bounds={ _Min, Max } } ) ->

	NewCount = Current + PortCreationCount,

	case NewCount > Max of

		true ->
			throw( { too_many_iterated_ports_requested, NewCount, Max } );

		false ->
			{ NewCount, Bounds }

	end.



% Defines the specified number of new indexes in the specified list of indexes,
% and returns a pair of two ordered lists of indexes: just the newly introduced
% ones, and all indexes.
%
% Note: we could have tried to fill the gaps that may have been induced by port
% destructions, however port names would have been reused, which could be the
% entry point of a possible bug. Instead we just continuously expand the indexes
% (anyway they are unlikely to reach extremely high values), and thus only fresh
% names are used.
%
-spec insert_indexes( basic_utils:count(), [ iterated_index() ] ) ->
							{ [ iterated_index() ], [ iterated_index() ] }.
insert_indexes( CreationCount, Indexes ) ->

	% Just a sanity check regarding order:
	case lists:sort( Indexes ) of

		Indexes ->
			ok;

		Other ->
			throw( { invalid_index_order, Indexes, Other } )

	end,

	{ RevIndexes, FirstNewIndex } = case lists:reverse( Indexes ) of

		[] ->
			{ [], 1 };

		L=[ Last | _T ] ->
			{ L, Last + 1 }

	end,

	% {NewIndexes, AllIndexes} shall be returned, mminimising reversing:
	RevNewIndexes =
		get_rev_new_indexes( CreationCount, FirstNewIndex, _Acc=[] ),

	AllIndexes = lists:reverse( RevNewIndexes ++ RevIndexes ),

	{ lists:reverse( RevNewIndexes ), AllIndexes }.



% (helper)
get_rev_new_indexes( _CreationCount=0, _CurrentIndex, Acc ) ->
	Acc;

get_rev_new_indexes( CreationCount, CurrentIndex, Acc ) ->
	get_rev_new_indexes( CreationCount-1, CurrentIndex+1,
						 [ CurrentIndex | Acc ] ).



% Sets explicitly the specified input port to the specified fully-specified
% value.
%
% Note: calling this method bypasses the (channel-based) dataflow system; it is
% mostly useful in order to feed source blocks from outside of the dataflow
% (typically from the experiment entry point). Such an explicit setting will
% perform activations exactly like a standard setting.
%
-spec setInputPortValue( wooper:state(),
			input_port_name() | input_port_string_name(), channel_value(),
			class_Actor:actor_pid() ) -> oneway_return().
setInputPortValue( _State, _InputPortName, _Value, _SenderPid ) ->

	% Mere placeholder, meant to be overridden, as its implementation depends on
	% the type of that block:
	%
	throw( not_overridden ).



% Notifies this dataflow block that, for specified input port, one of its
% upstream blocks just emitted a new (channel) value.
%
% Note: an immediate value (with no specific metadata) could have sufficed, as
% ports are already connected.
%
-spec notifyNewInput( wooper:state(), input_port_name(), channel_value(),
					  block_pid() ) -> actor_oneway_return().
notifyNewInput( _State, InputPortName, ChannelValue, _UpstreamBlockPid )
  when is_binary( InputPortName )
	   andalso is_record( ChannelValue, channel_value ) ->

	% Mere placeholder, meant to be overridden as its implementation depends on
	% the type of that block:
	%
	throw( not_overridden ).



% Informs back the calling actor regarding the status of the specified local
% output port, by calling its notifyOutputPortStatus/3 actor oneway.
%
% Note: calling this method bypasses the (channel-based) dataflow system; it is
% mostly useful in order that blocks from outside of the dataflow are able to
% read statuses of various dataflow blocks (typically useful for the experiment
% exit point).
%
-spec requestOutputPortStatus( wooper:state(),
			output_port_name() | output_port_string_name(),
			sending_actor_pid() ) -> actor_oneway_return().
requestOutputPortStatus( State, BinOutputPortName, ActorPid )
  when is_binary( BinOutputPortName ) ->

	?void_fmt( "Received a request about the status of output port '~ts' "
			   "from ~w.", [ BinOutputPortName, ActorPid ] ),

	Status = get_output_port_status( BinOutputPortName, State ),

	SentState = class_Actor:send_actor_message( ActorPid,
					{ notifyOutputPortStatus, [ Status ] }, State ),

	actor:return_state( SentState );


requestOutputPortStatus( State, OutputPortName, SenderPid )
  when is_list( OutputPortName ) ->
	BinOutputPortName = text_utils:string_to_binary( OutputPortName ),
	NewState = requestOutputPortStatus( State, BinOutputPortName, SenderPid ),
	actor:return_state( NewState ).



% Unsets specified (supposedly set) input port.
%
% (exported helper)
%
-spec unset_input_port( input_port(), input_port_name() ) -> input_port().
unset_input_port( #input_port{ value_status=unset }, InputPortName ) ->
	throw( { input_port_name_already_unset, InputPortName } );

unset_input_port( InputPort, _InputPortName ) ->
	InputPort#input_port{ value_status=unset }.



% Unsets specified (supposedly set) output port.
%
% (exported helper)
%
-spec unset_output_port( output_port(), output_port_name() ) -> output_port().
unset_output_port( #output_port{ value_status=unset }, OutputPortName ) ->
	throw( { output_port_name_already_unset, OutputPortName } );

unset_output_port( OutputPort, _OutputPortName ) ->
	OutputPort#output_port{ value_status=unset }.



% Resumes that block, supposedly having been suspended beforehand.
-spec resume( wooper:state(), sending_actor_pid() ) -> actor_oneway_return().
resume( State, _SendingActorPid ) ->

	% Checking:
	suspended = ?getAttr(run_status),

	?void_fmt( "Resuming now ~ts.", [ to_string( State ) ] ),

	OutputPortTable = ?getAttr(output_ports),

	{ NewOutputPortTable, NewState } =
		send_suspended_values( OutputPortTable, State ),

	ResumeState = setAttributes( NewState, [
					{ run_status, active },
					{ output_ports, NewOutputPortTable } ] ),

	actor:return_state( ResumeState ).



% Sends now the values that were assigned to output ports while this block was
% suspended.
%
% (helper)
%
-spec send_suspended_values( output_port_table(), wooper:state() ) ->
								{ output_port_table(), wooper:state() }.
send_suspended_values( OutputPortTable, State ) ->

	OutputPorts = table:enumerate( OutputPortTable ),

	PortTimestamp = class_Actor:get_current_logical_timestamp( State ),

	send_suspended_values( OutputPorts, PortTimestamp,
						   _AccPortTable=table:new(), State ).


send_suspended_values( _OutputPorts=[], _PortTimestamp, AccPortTable, State ) ->
	{ AccPortTable, State };

send_suspended_values( _OutputPorts=[ { OutputPortName,
										OutputPort=#output_port{
						  value_semantics=Semantics,
						  value_unit=Unit,
						  value_type=Type,
						  value_status={ set, Value },
						  last_sending=send_on_resume,
						  fed_ports=FedPorts } } | T ],
					   PortTimestamp, AccPortTable, State ) ->

	% We can directly reuse the stored, raw value, as it has been supposedly
	% checked at setting time.

	ChannelValue = class_Dataflow:create_direct_channel_value( Value,
													Semantics, Unit, Type ),

	?info_fmt( "Exiting suspension, notifying of ~ts following remote input "
		"ports:~n~ts",
		[ value_to_string( ChannelValue ), text_utils:strings_to_string(
				  [ dataflow_support:port_id_to_string( PortId )
					|| PortId <- FedPorts ] ) ] ),

	SentState = notify_fed_input_ports( ChannelValue, FedPorts, State ),

	NewOutputPort = OutputPort#output_port{ last_sending=PortTimestamp },

	NewPortTable = table:add_new_entry( _K=OutputPortName, _V=NewOutputPort,
										AccPortTable ),

	send_suspended_values( T, PortTimestamp, NewPortTable, SentState );


% Either a non-set and/or non-suspended port, we keep it as is:
send_suspended_values( _OutputPorts=[ { OutputPortName, OutputPort } | T ],
					   PortTimestamp, AccPortTable, State ) ->

	NewPortTable = table:add_new_entry( _K=OutputPortName, _V=OutputPort,
										AccPortTable ),

	send_suspended_values( T, PortTimestamp, NewPortTable, State ).



% Requests this block to disconnect from specified one, knowing the latter
% already forgot this former block.
%
-spec disconnectFromBlock( wooper:state(), sending_actor_pid() ) ->
									actor_oneway_return().
disconnectFromBlock( State, BlockPid ) ->

	% Note: maybe iterated ports becoming empty could/should be removed.

	?debug_fmt( "Disconnection from block ~w.", [ BlockPid ] ),

	NewInputPortTable = disconnect_input_ports_from( BlockPid,
													 ?getAttr(input_ports) ),

	NewOutputPortTable = disconnect_output_ports_from( BlockPid,
													   ?getAttr(output_ports) ),

	FinalState = setAttributes( State, [
						{ input_ports, NewInputPortTable },
						{ output_ports, NewOutputPortTable } ] ),

	actor:return_state( FinalState ).



% (helper)
disconnect_input_ports_from( BlockPid, InputPortTable ) ->

	InputPorts = table:enumerate( InputPortTable ),

	NewInputPorts = [ { PortName, disconnect_input_port( BlockPid, InputPort ) }
					  || { PortName, InputPort } <- InputPorts ],

	table:new( NewInputPorts ).



% (helper)
disconnect_input_port( BlockPid,
	   InputPort=#input_port{ feeder_port={ BlockPid, _OutputPortName } } ) ->
	InputPort#input_port{ feeder_port=undefined };

disconnect_input_port( _BlockPid, InputPort ) ->
	InputPort.



% (helper)
disconnect_output_ports_from( BlockPid, OutputPortTable ) ->

	OutputPorts = table:enumerate( OutputPortTable ),

	NewOutputPorts = [ { PortName,
						 disconnect_output_port( BlockPid, OutputPort ) }
					   || { PortName, OutputPort } <- OutputPorts ],

	table:new( NewOutputPorts ).



% (helper)
disconnect_output_port( BlockPid,
						OutputPort=#output_port{ fed_ports=PortIds } ) ->

	% We have a list of {SomeBlockPid, PortName} pairs, we remove all pairs
	% (possibly more than one) whose first element matches BlockPid:

	NewPortIds = filter_block_from_ids( BlockPid, PortIds, _Acc=[] ),

	OutputPort#output_port{ fed_ports=NewPortIds }.



% (helper)
filter_block_from_ids( _BlockPid, _PortIds=[], Acc ) ->
	Acc;

filter_block_from_ids( BlockPid, _PortIds=[ { BlockPid, _PortName } | T ],
					   Acc ) ->
	% Matching, thus dropped:
	filter_block_from_ids( BlockPid, T, Acc );

filter_block_from_ids( BlockPid, _PortIds=[ Id | T ], Acc ) ->
	% Not matching, hence kept:
	filter_block_from_ids( BlockPid, T, [ Id | Acc ] ).




% Section to the exported helpers, introduced to ease the dataflow uses.



% Returns the PID of the block corresponding to the specified external
% identifier.
%
% (exported helper)
%
-spec get_block_pid( external_id(), wooper:state() ) -> block_pid().
get_block_pid( ExternalId, State ) ->

	% Note: using for this read-only need a standard, unsynchronized WOOPER
	% call:

	?getAttr(identification_server_pid) !
		{ getBlockPID, [ ExternalId ], self() },

	receive

		{ wooper_result, BlockPid } when is_pid( BlockPid ) ->
			BlockPid

	end.



% Section dedicated to the internal use of ports.


% Returns the specified input port.
-spec get_input_port( input_port_name(), input_port_table(),
					  wooper:state() ) -> input_port().
get_input_port( InputPortName, InputPortTable, State )
  when is_binary( InputPortName ) ->

	case table:lookup_entry( _K=InputPortName, InputPortTable ) of

		{ value, IPort } ->
			IPort;

		key_not_found ->

			?error_fmt( "Error, specified input port named '~ts' does "
				"not exist, knowing that ~ts",
				[ InputPortName, list_input_ports( InputPortTable ) ] ),

			throw( { unknown_input_port,
					 text_utils:binary_to_string( InputPortName ) } )

	end;

get_input_port( InputPortName, _InputPortTable, State ) ->
	?error_fmt( "The input port name '~p' is not a binary string.",
				[ InputPortName ] ),
	throw( { invalid_port_name_type, InputPortName } ).



% Returns the status of specified input port, i.e. either 'unset' or
% {'set',Value}, and leaves it unchanged.
%
% Typically used for blocks wanting that the corresponding input value remains
% as long as it is not explicitly changed (or, previously, relying on the
% 'activate_when_all_set activation' policy that used to reset all input ports
% after activation anyway).
%
-spec get_input_port_status( input_port_string_name() | input_port_name(),
							 wooper:state() ) -> value_status().
get_input_port_status( InputPortName, State )
  when is_list( InputPortName ) ->
	BinPortName = text_utils:string_to_binary( InputPortName ),
	get_input_port_status( BinPortName, State );

get_input_port_status( InputBinPortName, State )
  when is_binary( InputBinPortName ) ->

	InputPortTable = ?getAttr(input_ports),

	InputPort = get_input_port( InputBinPortName, InputPortTable, State ),

	ValueStatus = InputPort#input_port.value_status,

	%?debug_fmt( "Reading for input port '~ts' value status ~p.",
	%			[ InputPortName, ValueStatus ] ),

	ValueStatus.



% Returns the (supposedly set) actual, raw value (not a full channel value) of
% specified input port, and leaves it unchanged.
%
% Throws an exception if the port is not set.
%
% Typically used for blocks wanting that the corresponding input value remains
% as long as it is not explicitly changed (or, previously, relying on the
% 'activate_when_all_set activation' policy that used to reset all input ports
% after activation anyway).
%
% Note: the port name specified as string for convenience (intra-process
% service).
%
-spec get_input_port_value( input_port_string_name(), wooper:state() ) ->
									actual_value().
get_input_port_value( InputPortName, State ) ->
	{ set, V } = get_input_port_status( InputPortName, State ),
	V.



% Returns the value of the specified input port if it is set, and leaves it in
% 'unset' state in all cases.
%
% As a result, returns:
%  - either 'port_already_unset'
%  - or {InputPortValue, UpdatedInputPortTable}
%
% Typically used for blocks relying on the 'activate_on_new_set' policy, to
% unset an input port when it is read.
%
% Note: the port name is specified as string for convenience (intra-process
% service).
%
-spec extract_input_port_value( input_port_string_name(), input_port_table(),
								wooper:state() ) ->
			'port_already_unset' | { actual_value(), input_port_table() }.
extract_input_port_value( InputPortName, InputPortTable, State )
  when is_list( InputPortName ) ->

	BinPortName = text_utils:string_to_binary( InputPortName ),

	InputPort = get_input_port( BinPortName, InputPortTable, State ),

	case InputPort#input_port.value_status of

		unset ->
			port_already_unset;

		{ set, InputPortValue } ->
			ResetInputPort = InputPort#input_port{ value_status=unset },

			UpdatedInputPortTable = table:update_entry( BinPortName,
											ResetInputPort, InputPortTable ),

			{ InputPortValue, UpdatedInputPortTable }

	end.



% Returns the specified output port.
-spec get_output_port( output_port_name(), output_port_table(),
					   wooper:state() ) -> output_port().
get_output_port( OutputPortName, OutputPortTable, State )
  when is_binary( OutputPortName ) ->

	case table:lookup_entry( _K=OutputPortName, OutputPortTable ) of

		{ value, OPort } ->
			OPort;

		key_not_found ->

			?error_fmt( "Error, specified output port named '~ts' does "
				"not exist, knowing that ~ts", [ OutputPortName,
					list_output_ports( OutputPortTable ) ] ),

			throw( { unknown_output_port,
					 text_utils:binary_to_string( OutputPortName ) } )

	end;

get_output_port( OutputPortName, _OutputPortTable, State ) ->

	?error_fmt( "The output port name '~p' is not a binary string.",
				[ OutputPortName ] ),

	throw( { invalid_port_name_type, OutputPortName } ).



% Returns the status of specified output port, i.e. either 'unset' or
% {'set',Value}, and leaves it unchanged.
%
-spec get_output_port_status( output_port_string_name() | output_port_name(),
							  wooper:state() ) -> value_status().
get_output_port_status( BinOutputPortName, State )
  when is_binary( BinOutputPortName ) ->

	OutputPortTable = ?getAttr(output_ports),

	OutputPort = get_output_port( BinOutputPortName, OutputPortTable, State ),

	ValueStatus = OutputPort#output_port.value_status,

	%?debug_fmt( "Reading for output port '~ts' value status ~p.",
	%			[ BinOutputPortName, ValueStatus ] ),

	ValueStatus;


get_output_port_status( OutputPortName, State )
  when is_list( OutputPortName ) ->

	BinPortName = text_utils:string_to_binary( OutputPortName ),

	get_output_port_status( BinPortName, State ).



% Returns the SUTC metadata associated to an output port, in a form typically
% suitable for class_Dataflow:create_channel_value/4.
%
% (helper)
%
-spec get_output_port_metadata( output_port_string_name() | output_port_name(),
								wooper:state() ) -> port_metadata().
get_output_port_metadata( OutputPortName, State )
  when is_binary( OutputPortName ) ->

	OutputPortTable = ?getAttr(output_ports),

	OutputPort = get_output_port( OutputPortName, OutputPortTable, State ),

	{ OutputPort#output_port.value_semantics,
	  OutputPort#output_port.value_unit,
	  OutputPort#output_port.value_type,
	  OutputPort#output_port.value_constraints };


get_output_port_metadata( OutputPortName, State )
  when is_list( OutputPortName ) ->

	BinPortName = text_utils:string_to_binary( OutputPortName ),

	get_output_port_metadata( BinPortName, State ).



% Section specific to port iterations.


% Returns specified input port iteration.
-spec get_input_port_iteration( input_iteration_name(), wooper:state() ) ->
										input_port_iteration().
get_input_port_iteration( InputIterationName, State )
  when is_binary( InputIterationName ) ->

	InputIterationTable = ?getAttr(input_iterations),

	case table:lookup_entry( InputIterationName, InputIterationTable ) of

		{ value, Iter } ->
			Iter;

		key_not_found ->

			?error_fmt( "Error, specified input port iteration '~ts' does "
				"not exist, knowing that ~ts.",
				[ InputIterationName,
				  list_input_iterations( InputIterationTable ) ] ),

			throw( { unknown_input_port_iteration,
					 text_utils:binary_to_string( InputIterationName ) } )

	end.



% Returns specified output port iteration.
-spec get_output_port_iteration( output_iteration_name(), wooper:state() ) ->
										output_port_iteration().
get_output_port_iteration( OutputIterationName, State )
  when is_binary( OutputIterationName )->

	OutputIterationTable = ?getAttr(output_iterations),

	case table:lookup_entry( OutputIterationName, OutputIterationTable ) of

		{ value, Iter } ->
			Iter;

		key_not_found ->

			?error_fmt( "Error, specified output port iteration '~ts' does "
				"not exist, knowing that ~ts.", [ OutputIterationName,
					list_output_iterations( OutputIterationTable ) ] ),

			throw( { unknown_output_port_iteration,
					 text_utils:binary_to_string( OutputIterationName ) } )

	end.



% Returns the port description corresponding to the specified port.
-spec get_port_description( input_port() | output_port(), wooper:state() ) ->
									port_description().
get_port_description( #output_port{ value_semantics=Semantics,
									value_unit=Unit,
									value_type=Type,
									value_constraints=Constraints,
									value_status=Status },
					  State ) ->

	% If this upstream block is suspended, we do not want to report a value, as
	% it shall not be used before it is resumed:
	%
	ActualStatus = case ?getAttr(run_status) of

		suspended ->
			unset;

		_ ->
			Status

   end,

	% Does not depend on a port being an input or output one:
	#port_description{ semantics=Semantics,
					   unit=Unit,
					   type=Type,
					   constraints=Constraints,
					   status=ActualStatus }.



% Returns the set of the PID of the upstream blocks of this block.
-spec get_upstream_blocks( wooper:state() ) -> set_utils:set( block_pid() ).
get_upstream_blocks( State ) ->

	InputPorts = table:values( ?getAttr(input_ports) ),

	% Duplicates may exist:
	UpstreamBlockPids = [ UpstreamBlockPid ||
		 #input_port{ feeder_port={ UpstreamBlockPid, _OutputPortName } }
								<- InputPorts ],

	set_utils:from_list( UpstreamBlockPids ).



% Returns the set of the PID of the downstream blocks of this block.
-spec get_downstream_blocks( wooper:state() ) -> set_utils:set( block_pid() ).
get_downstream_blocks( State ) ->

	OutputPorts = table:values( ?getAttr(output_ports) ),

	% Compared to the upstream variant, we have here *lists* of port
	% identifiers:

	InputPortIdLists = [ InputPortIdList
		   || #output_port{ fed_ports=InputPortIdList } <- OutputPorts ],

	% Duplicates may exist:
	DownstreamBlockPids =
		[ DownstreamBlockPid || { DownstreamBlockPid, _InputPortName }
				<- lists:flatten( InputPortIdLists ) ],

	set_utils:from_list( DownstreamBlockPids ).



% Returns the set of the PIDs of all blocks directly connected to this block
% (whether they are downstream, upstream or both).
%
-spec get_directly_connected_blocks( wooper:state() ) ->
											set_utils:set( block_pid() ).
get_directly_connected_blocks( State ) ->
	set_utils:union( get_upstream_blocks( State ),
					 get_downstream_blocks( State ) ).



% Validates that the specified channel value can be accepted by the specified
% input port.
%
% The last two parameters allow to better report errors.
%
-spec validate_value_for_input_port( channel_value(), input_port(),
			input_port_name(), wooper:state() ) -> void().
validate_value_for_input_port( ChannelValue=#channel_value{
								  actual_value=Value,
								  semantics=ValueSemantics,
								  unit={ ValueUnitBinString, ValueUnit },
								  type=ValueType },
							   #input_port{
								  value_semantics=PortSemantics,
								  value_unit={ PortUnitBinString, PortUnit },
								  value_type=PortType,
								  value_constraints=PortConstraints },
							   PortName, State ) ->

	case rdf_utils:implies( ValueSemantics, PortSemantics ) of

		true ->
			ok;

		false ->

			LackingSem = set_utils:difference( PortSemantics, ValueSemantics ),

			LackingSemList = set_utils:to_list( LackingSem ),

			% Temporarily silenced a bit:
			%?warning_fmt
			?debug_fmt( "Trying to feed the input port '~ts' with a value "
				"(~ts) whose semantics does not comply with the one of "
				"this port, as the value lacks following ~B semantic "
				"elements: ~p.~n~n"
				"Indeed the value relies on ~tswhile the port relies on ~ts"  ,
				[ PortName, value_to_string( ChannelValue ),
				  length( LackingSemList ), LackingSemList,
				  rdf_utils:vocabulary_to_string( ValueSemantics ),
				  rdf_utils:vocabulary_to_string( PortSemantics ) ] ),

			% We currently do not consider that it is a blocking issue:
			ok

			% ValueSemList = set_utils:to_list( ValueSemantics ),
			% PortSemList = set_utils:to_list( PortSemantics ),
			%
			% throw( { incompatible_semantics, { { value, ValueSemList },
			%							 { input_port, PortSemList } } } )

	end,

	% In future versions, provided of course that units are compatible,
	% transparent conversions could be done (should it be considered desirable -
	% which is not sure):
	%
	case unit_utils:are_units_identical( ValueUnit, PortUnit ) of

		true ->
			ok;

		false ->
			ValueUnitString = text_utils:binary_to_string( ValueUnitBinString ),
			PortUnitString = text_utils:binary_to_string( PortUnitBinString ),

			?error_fmt( "Trying to feed the input port '~ts' with a "
				"value (~ts) whose unit (~ts) does not match the one of "
				"this port (~ts).",
				[ PortName, value_to_string( ChannelValue ),
				  ValueUnitString, PortUnitString ] ),

			throw( { unmatching_units, { { value, ValueUnitString },
										 { input_port, PortUnitString } } } )

	end,

	% The same applies here, as types may be distinct yet potentially compatible
	% (ex: floats and integers):
	%
	case type_utils:are_types_identical( ValueType, PortType ) of

		true ->
			ok;

		false ->
			?error_fmt( "Trying to feed the input port '~ts' with a "
				"value (~ts) whose type (~ts) does not match the one of "
				"this port (~ts).",
				[ PortName, value_to_string( ChannelValue ),
				  type_utils:type_to_string( ValueType ),
				  type_utils:type_to_string( PortType ) ] ),

			throw( { unmatching_types, { { value, ValueType },
										 { input_port, PortType } } } )

	end,

	case satisfies_constraints( Value, PortConstraints ) of

		true ->
			ok;

		{ false, Constraint } ->
			?error_fmt( "Trying to feed the input port '~ts' with a "
				"value (~ts) that violates the following constraint of "
				"that port: ~w.",
				[ PortName, value_to_string( ChannelValue ), Constraint ] ),
			throw( { unsatisfied_constraint, Constraint, Value } )

	end;

validate_value_for_input_port( Value, _InputPort, _PortName, _State )
  when not is_record( Value, channel_value ) ->
	throw( { not_a_channel_value, Value } ).




% Validates that the specified channel value can be accepted by the specified
% output port.
%
% The last two parameters allow to better report errors.
%
% Note: extremely similar to validate_value_for_input_port/4.
%
-spec validate_value_for_output_port( channel_value(), output_port(),
			output_port_name(), wooper:state() ) -> void().
validate_value_for_output_port( ChannelValue=#channel_value{
								  actual_value=Value,
								  semantics=ValueSemantics,
								  unit={ ValueUnitBinString, ValueUnit },
								  type=ValueType },
								#output_port{
								  value_semantics=PortSemantics,
								  value_unit={ PortUnitBinString, PortUnit },
								  value_type=PortType,
								  value_constraints=PortConstraints },
								PortName, State ) ->

	case rdf_utils:implies( ValueSemantics, PortSemantics ) of

		true ->
			ok;

		false ->

			LackingSem = set_utils:difference( PortSemantics, ValueSemantics ),

			LackingSemList = set_utils:to_list( LackingSem ),

			?warning_fmt( "Trying to feed the output port '~ts' with a value "
				"(~ts) whose semantics does not comply with the one of this "
				"port, as the value lacks following ~B semantic elements: ~p.~n"
				"Indeed the value relies on ~tswhile the port relies on ~ts.",
				[ PortName, value_to_string( ChannelValue ),
				  length( LackingSemList ), LackingSemList,
				  rdf_utils:vocabulary_to_string( ValueSemantics ),
				  rdf_utils:vocabulary_to_string( PortSemantics ) ] ),

			% We currently do not consider it is a blocking issue:
			ok

			% ValueSemList = set_utils:to_list( ValueSemantics ),
			% PortSemList = set_utils:to_list( PortSemantics ),
			%
			% throw( { incompatible_semantics, { { value, ValueSemList },
			%							{ output_port, PortSemList } } } )

	end,

	% In future versions, provided of course that units are compatible,
	% transparent conversions could be done (should it be considered desirable -
	% which is not sure at all):
	%
	case unit_utils:are_units_identical( ValueUnit, PortUnit ) of

		true ->
			ok;

		false ->
			ValueUnitString = text_utils:binary_to_string( ValueUnitBinString ),
			PortUnitString = text_utils:binary_to_string( PortUnitBinString ),

			?error_fmt( "Trying to feed the output port '~ts' with a "
				"value (~ts) whose unit (~ts) does not match the one of "
				"this port (~ts).",
				[ PortName, value_to_string( ChannelValue ),
				  ValueUnitString, PortUnitString ] ),

			throw( { unmatching_units, { { value, ValueUnitString },
										 { output_port, PortUnitString } } } )

	end,

	% The same applies here, as types may be distinct yet potentially compatible
	% (ex: floats and integers):
	%
	case type_utils:are_types_identical( ValueType, PortType ) of

		true ->
			ok;

		false ->
			?error_fmt( "Trying to feed the output port '~ts' with a "
				"value (~ts) whose type (~ts) does not match the one of "
				"this port (~ts).",
				[ PortName, value_to_string( ChannelValue ),
				  type_utils:type_to_string( ValueType ),
				  type_utils:type_to_string( PortType ) ] ),

			throw( { unmatching_types, { { value, ValueType },
										 { output_port, PortType } } } )

	end,

	case satisfies_constraints( Value, PortConstraints ) of

		true ->
			ok;

		{ false, Constraint } ->
			?error_fmt( "Trying to feed the output port '~ts' with a value "
				"(~ts) that violates the following constraint of that "
				"port: ~w.",
				[ PortName, value_to_string( ChannelValue ), Constraint ] ),

			throw( { unsatisfied_constraint, Constraint, Value } )

	end;

validate_value_for_output_port( Value, _OutputPort, _PortName, _State )
  when not is_record( Value, channel_value ) ->
	throw( { not_a_channel_value, Value } ).



% Assigns the specified value to the specified input port, with (almost) no
% specific value-level checking (see validate_value_for_input_port/4) for that,
% and returns the corresponding updated input port.
%
% Note:
% - the port name is specified only to provide better error diagnoses.
% - the state is const
%
-spec assign_input_value( channel_value(), input_port(), port_name(),
						  wooper:state() ) -> input_port().
assign_input_value( ChannelValue=#channel_value{ actual_value=Value },
		InputPort=#input_port{ value_type=ExpectedType }, BinPortName,
		State ) ->

	case type_utils:is_of_type( Value, ExpectedType ) of

		true ->
			PortTimestamp = class_Actor:get_current_logical_timestamp( State ),

			InputPort#input_port{ value_status={ set, Value },
								  last_receiving=PortTimestamp };

		false ->
			DetectedType = type_utils:get_type_of( Value ),

			?error_fmt( "The value '~p' (corresponding to ~ts) is "
				"determined as being of type '~ts' whereas the expected "
				"type was '~ts' for ~ts",
				[ Value, value_to_string( ChannelValue ),
				  type_utils:type_to_string( DetectedType ),
				  type_utils:type_to_string( ExpectedType ),
				  input_port_to_string( BinPortName, InputPort ) ] ),

			PortName = text_utils:binary_to_string( BinPortName ),

			throw( { unmatching_input_type, PortName, Value, ExpectedType,
					 DetectedType } )

	end.



% Assigns the specified (channel) value to the specified output port, with
% (almost) no specific value-level checking (see
% validate_value_for_output_port/4) for that, and returns a pair made of the
% corresponding updated output port and a new state, just updated regarding the
% sending of a message to notify the associated target dataflow blocks.
%
% (exported helper)
%
-spec assign_output_value( channel_value(), output_port(), port_name(),
					wooper:state() ) -> { output_port(), wooper:state() }.
assign_output_value( ChannelValue=#channel_value{ actual_value=Value },
					 OutputPort=#output_port{ value_type=ExpectedType,
											  fed_ports=FedPorts },
					 BinPortName, State ) ->

	case type_utils:is_of_type( Value, ExpectedType ) of

		true ->

			case ?getAttr(run_status) of

				active ->
					PortTimestamp =
						class_Actor:get_current_logical_timestamp( State ),

					% Setting output ports is not that useful:
					NewOutputPort = OutputPort#output_port{
										value_status={ set, Value },
										last_sending=PortTimestamp },

					SentState = case FedPorts of

						[] ->
							?info_fmt( "Assigning output ~ts, yet no remote "
								"input port is currently registered.",
								[ value_to_string( ChannelValue ) ] ),
							State;

						_ ->

							?info_fmt( "Assigning output ~ts to following "
								"remote input port(s):~n~ts",
								[ value_to_string( ChannelValue ),
								  text_utils:strings_to_string(
									[ dataflow_support:port_id_to_string(
										PortId ) || PortId <- FedPorts ] ) ] ),

							notify_fed_input_ports( ChannelValue, FedPorts,
													State )

					end,

					{ NewOutputPort, SentState };


				suspended ->
					NewOutputPort = OutputPort#output_port{
										value_status={ set, Value },
										last_sending=send_on_resume },

					{ NewOutputPort, State }

			end;


		false ->

			DetectedType = type_utils:get_type_of( Value ),

			?error_fmt( "The value '~p' (corresponding to ~ts) is "
				"determined as being of type '~ts', whereas the expected "
				"type was '~ts' for ~ts",
				[ Value, value_to_string( ChannelValue ),
				  type_utils:type_to_string( DetectedType ),
				  type_utils:type_to_string( ExpectedType ),
				  output_port_to_string( BinPortName, OutputPort ) ] ),

			PortName = text_utils:binary_to_string( BinPortName ),

			throw( { unmatching_output_type, PortName, Value, ExpectedType,
					 DetectedType } )

	end.



% Notifies the specified downstream input ports of the specified value, fed by
% our specified output port.
%
% Note: specifying the source output port name could be considered, to add debug
% information.
%
-spec notify_fed_input_ports( channel_value(), [ input_port_id() ],
							  wooper:state() ) -> wooper:state().
notify_fed_input_ports( _ChannelValue, _InputPortId=[], State ) ->
	State;

notify_fed_input_ports( ChannelValue,
		_InputPortId=[ { DownstreamBlockPid, InputPortName } | T ], State ) ->

	?void_fmt( "Feeding the input port '~ts' of downstream block ~w with "
		"value ~ts.", [ InputPortName, DownstreamBlockPid,
					   value_to_string( ChannelValue ) ] ),

	SentState = class_Actor:send_actor_message( DownstreamBlockPid,
				  { notifyNewInput, [ InputPortName, ChannelValue ] }, State ),

	notify_fed_input_ports( ChannelValue, T, SentState ).



% Returns a list of the identifiers of the iterated ports that correspond to
% specified input port iteration.
%
-spec get_input_iterated_ports( input_port_iteration(), wooper:state() ) ->
										[ input_port_id() ].
get_input_iterated_ports( #input_port_iteration{
							 base_name=BaseName,
							 port_indexes=Indexes }, State ) ->

	InputPortTable = ?getAttr(input_ports),

	[ begin
		  PortName = get_iterated_port_name( BaseName, I ),
		  InputPort = get_input_port( PortName, InputPortTable, State ),
		  { PortName, InputPort }
	  end || I <- Indexes ].



% Returns a list of all the actual values that the specified input port
% iteration stores, knowing that all of its iterated ports must be set
% (otherwise an exception is raised).
%
-spec get_all_input_iteration_values( string_iteration_name(),
									  wooper:state() ) -> [ actual_value() ].
get_all_input_iteration_values( InputIterationName, State )
  when is_list( InputIterationName ) ->

	BinIterationName = text_utils:string_to_binary( InputIterationName ),

	Iteration = #input_port_iteration{ base_name=BaseName,
									   port_indexes=Indexes }
		= get_input_port_iteration( BinIterationName, State ),

	?void_fmt( "Getting all values from ~ts.",
			   [ input_port_iteration_to_string( Iteration ) ] ),

	InputPortTable = ?getAttr(input_ports),

	IteratedPorts = [ begin
							PortName = get_iterated_port_name( BaseName, I ),
							get_input_port( PortName, InputPortTable, State )
					  end || I <- Indexes ],

	ChannelStatuses = [ IP#input_port.value_status || IP <- IteratedPorts ],

	% Returns ChannelValues, or raises an exception if ever a port was not set:
	lists:foldl( fun( { set, V }, Acc ) ->
						 [ V | Acc ];

					( unset, _Acc ) ->
						 throw( { unset_iterated_port_in, InputIterationName } )

				 end,
				 _Acc0=[],
				 ChannelStatuses ).



% Assigns the specified list of actual (raw) values to the iterated ports of the
% specified output port iteration.
%
% Fails if at least a value is specified and the number of values does not match
% the current number of iterated ports (as obviously there is a mismatch);
% writing (any number of) values to an empty iteration is allowed (as for
% standard output ports, which may or may not be connected).
%
-spec set_all_output_iteration_values( string_iteration_name(),
					 [ actual_value() ], wooper:state() ) -> wooper:state().
set_all_output_iteration_values( OutputIterationName, Values, State )
  when is_list( OutputIterationName ) ->

	ValueCount = length( Values ),

	?debug_fmt( "Assigning ~B values to output iteration '~ts'.",
				[ ValueCount, OutputIterationName ] ),

	BinIterationName = text_utils:string_to_binary( OutputIterationName ),

	#output_port_iteration{ base_name=BaseName, port_indexes=Indexes } =
		get_output_port_iteration( BinIterationName, State ),

	IteratedPortCount = length( Indexes ),

	case IteratedPortCount of

		0 ->
			% If the (output) port iteration is empty (not connected at all),
			% the setting of any number of values is tolerated, yet ignored:
			State;

		ValueCount ->
			% Counts match, we rely now on their respective order to match as
			% well:

			% In order:
			IteratedPortNames =
				[ get_iterated_port_name( BaseName, I ) || I <- Indexes ],

			% List of { StringPortName, ActualValue }:
			PortPairs = lists:zip( IteratedPortNames, ValueCount ),

			set_output_port_direct_values( PortPairs, State );

		PortCount when PortCount > ValueCount ->
			MissingCount = PortCount - ValueCount,

			?error_fmt( "When setting the values of output port iteration "
				"'~ts', ~B of them were missing: "
				"~B values were specified against ~B iterated ports.~n"
				"Specified values: ~ts",
				[ OutputIterationName, MissingCount, ValueCount,
				  IteratedPortCount,
				  text_utils:terms_to_enumerated_string( Values ) ] ),

			throw( { missing_values_for, OutputIterationName,
						{ expected, PortCount }, { got, ValueCount } } );

		PortCount when PortCount < ValueCount ->
			ExtraCount = ValueCount - PortCount,

			?error_fmt( "When setting the values of output port iteration "
				"'~ts', ~B extraneous values were specified: "
				"~B values were specified, against ~B iterated ports.~n"
				"Specified values: ~ts",
				[ OutputIterationName, ExtraCount, ValueCount,
				  IteratedPortCount,
				  text_utils:terms_to_enumerated_string( Values ) ] ),

			throw( { extraneous_values_for, OutputIterationName,
					 { expected, PortCount }, { got, ValueCount } } )

	end.



% Callback executed automatically whenever the block is activated.
%
% Meant to be overridden.
%
-spec activate( wooper:state() ) -> const_oneway_return().
activate( State ) ->

	?warning_fmt( "Default, do-nothing activation triggered for ~ts.",
				  to_string( State ) ),

	wooper:const_return().



% Sets the specified input ports to their respective specified (channel) values,
% internally (i.e. directly from this block).
%
% (exported helper)
%
-spec set_input_port_values( [ { input_port_string_name(), channel_value() } ],
							 wooper:state() ) -> wooper:state().
set_input_port_values( PortPairs, State ) ->
	InputPortTable = ?getAttr(input_ports),
	set_input_port_values_helper( PortPairs, InputPortTable, State ).



% (helper)
set_input_port_values_helper( _PortPairs=[], InputPortTable, State ) ->
	setAttribute( State, input_ports, InputPortTable );

set_input_port_values_helper( _PortPairs=[ { PortName, ChannelValue } | T ],
							  InputPortTable, State ) ->

	BinPortName = text_utils:string_to_binary( PortName ),

	InputPort = get_input_port( BinPortName, InputPortTable, State ),

	validate_value_for_input_port( ChannelValue, InputPort, BinPortName,
								   State ),

	NewInputPort = assign_input_value( ChannelValue, InputPort, BinPortName,
									   State ),

	NewInputPortTable = table:update_entry( BinPortName, NewInputPort,
											InputPortTable ),

	set_input_port_values_helper( T, NewInputPortTable, State ).



% Sets the specified input port to the specified direct (i.e. plain,
% non-channel) values.
%
% Typically useful at times like initialisation, where direct values (with no
% associated metadata), are available, and must be assigned to ports.
%
-spec set_input_port_direct_values(
		[ { input_port_string_name(), actual_value() } ], wooper:state() ) ->
											wooper:state().
set_input_port_direct_values( PortPairs, State ) ->
	InputPortTable = ?getAttr(input_ports),
	set_input_port_direct_values_helper( PortPairs, InputPortTable, State ).



% (helper)
set_input_port_direct_values_helper( _PortPairs=[], InputPortTable, State ) ->
	setAttribute( State, input_ports, InputPortTable );

set_input_port_direct_values_helper(
  _PortPairs=[ { InputPortName, DirectValue } | T ], InputPortTable, State ) ->

	InputPortBinName = text_utils:string_to_binary( InputPortName ),

	% As the channel value passed to assign_input_value/4 is only used to fetch
	% the actual value that it contains, we only create here a (bogus) empty
	% channel value, and set its value to the direct one specified as argument.
	%
	% We could have however preferred reading from the input port the various
	% expected metadata, and used class_Dataflow:create_channel_value/4 to
	% obtain a proper, corresponding channel value, but useless transformations
	% (ex: regarding types) would then have be performed.
	%
	InputPort = get_input_port( InputPortBinName, InputPortTable, State ),

	% The argument was not a channel value, hence no real validation can be
	% performed, so we simply reuse the port metadata to directly transform this
	% direct value into a channel one:
	%
	ChannelValue = class_Dataflow:create_channel_value( DirectValue ),

	% Not relevant by design:
	%validate_value_for_input_port( ChannelValue, InputPort, BinPortName,
	%							   State ),

	% Minimal type-based checking done:
	NewInputPort = assign_input_value( ChannelValue, InputPort, InputPortName,
									   State ),

	NewInputPortTable = table:update_entry( InputPortBinName, NewInputPort,
											InputPortTable ),

	set_input_port_direct_values_helper( T, NewInputPortTable, State ).




% Returns a textual description of the known input ports of this block.
-spec list_input_ports( input_port_table() ) -> ustring().
list_input_ports( InputPortTable ) ->

	case table:keys( InputPortTable ) of

		[] ->
			"no input port at all is defined for that block.";

		PortNames ->
			SortedString = text_utils:binaries_to_sorted_string( PortNames ),
			text_utils:format( "the ~B existing input ports are "
				"(alphabetically): ~ts", [ length( PortNames ), SortedString ] )

	end.



% Returns a textual description of the known output ports of this block.
-spec list_output_ports( output_port_table() ) -> ustring().
list_output_ports( OutputPortTable ) ->

	case table:keys( OutputPortTable ) of

		[] ->
			"no output port at all is defined for that block";

		PortNames ->
			SortedString = text_utils:binaries_to_sorted_string( PortNames ),
			text_utils:format( "the ~B existing output ports are "
				"(alphabetically): ~ts", [ length( PortNames ), SortedString ] )

	end.



% Returns a textual description of the known input iterations of this block.
-spec list_input_iterations( input_iteration_table() ) -> ustring().
list_input_iterations( InputIterationTable ) ->

	case table:keys( InputIterationTable ) of

		[] ->
			"no input iteration at all is defined for that block";

		IterationNames ->
			SortedString =
				text_utils:binaries_to_sorted_string( IterationNames ),
			text_utils:format( "the ~B existing input iterations are "
				"(alphabetically): ~ts",
				[ length( IterationNames ), SortedString ] )

	end.



% Returns a textual description of the known output iterations of this block.
-spec list_output_iterations( output_iteration_table() ) -> ustring().
list_output_iterations( OutputIterationTable ) ->

	case table:keys( OutputIterationTable ) of

		[] ->
			"no output iteration at all is defined for that block";

		IterationNames ->
			SortedString = text_utils:binaries_to_sorted_string(
								IterationNames ),
			text_utils:format( "the ~B existing output iterations are "
				"(alphabetically): ~ts",
				[ length( IterationNames ), SortedString ] )

	end.



% Sets the specified output port to the specified (channel) value.
%
% (exported helper)
%
-spec set_output_port_value( output_port_string_name(), channel_value(),
							 wooper:state() ) -> wooper:state().
set_output_port_value( OutputPortName, ChannelValue, State ) ->

	?void_fmt( "Setting output port '~ts' to ~ts",
			   [ OutputPortName, value_to_string( ChannelValue ) ] ),

	OutputPortTable = ?getAttr(output_ports),

	{ NewOutputPortTable, NewState } = set_output_ports_internal(
			OutputPortName, ChannelValue, OutputPortTable, State ),

	setAttribute( NewState, output_ports, NewOutputPortTable ).



% Sets the specified output ports to their respective specified (channel) value.
%
% (exported helper)
%
-spec set_output_port_values(
		[ { output_port_string_name(), channel_value() } ], wooper:state() ) ->
									wooper:state().
set_output_port_values( PortPairs, State ) ->

	OutputPortTable = ?getAttr(output_ports),

	set_output_ports_helper( PortPairs, OutputPortTable, State ).



% (helper)
% State used mainly for the traces.
set_output_ports_helper( _PortPairs=[], OutputPortTable, State ) ->
	setAttribute( State, output_ports, OutputPortTable );

set_output_ports_helper( _PortPairs=[ { PortName, ChannelValue } | T ],
						 OutputPortTable, State )
  when is_record( ChannelValue, channel_value ) ->

	{ NewOutputPortTable, NewState } = set_output_ports_internal( PortName,
								ChannelValue, OutputPortTable, State ),

	set_output_ports_helper( T, NewOutputPortTable, NewState ).



% Helper used both by single/multi-ports versions:
%
% (note: the returned state does not include yet the change in the port table)
%
-spec set_output_ports_internal( output_port_string_name(), channel_value(),
								 output_port_table(), wooper:state() ) ->
									 { output_port_table(), wooper:state() }.
set_output_ports_internal( OutputPortName, ChannelValue, OutputPortTable,
						   State ) when is_list( OutputPortName ) ->

	OutputPortBinName = text_utils:string_to_binary( OutputPortName ),

	set_output_ports_internal( OutputPortBinName, ChannelValue, OutputPortTable,
							   State );


set_output_ports_internal( OutputPortBinName, ChannelValue, OutputPortTable,
						   State )
  when is_binary( OutputPortBinName ) ->

	OutputPort = case table:lookup_entry( OutputPortBinName,
										  OutputPortTable ) of

		{ value, OPort } ->
			OPort;

		key_not_found ->
			?error_fmt( "Output port '~ts' not found; ~ts",
				[ OutputPortBinName, list_output_ports( OutputPortTable ) ] ),

			throw( { output_port_not_found,
					 text_utils:binary_to_string( OutputPortBinName ) } )

	end,

	validate_value_for_output_port( ChannelValue, OutputPort, OutputPortBinName,
									State ),

	{ NewOutputPort, NewState } = assign_output_value( ChannelValue,
										OutputPort, OutputPortBinName, State ),

	NewPortTable = table:update_entry( OutputPortBinName, NewOutputPort,
									   OutputPortTable ),

	{ NewPortTable, NewState }.



% Sets the specified output ports to the specified direct (i.e. plain,
% non-channel) values.
%
% Typically useful at times like initialisation, where direct values (with no
% associated metadata) are available, and must be assigned to (output) ports.
%
-spec set_output_port_direct_values(
		[ { output_port_string_name(), actual_value() } ], wooper:state() ) ->
											wooper:state().
set_output_port_direct_values( PortPairs, State ) ->
	OutputPortTable = ?getAttr(output_ports),
	set_output_port_direct_values_helper( PortPairs, OutputPortTable, State ).



% (helper)
set_output_port_direct_values_helper( _PortPairs=[], OutputPortTable, State ) ->
	setAttribute( State, output_ports, OutputPortTable );

set_output_port_direct_values_helper(
  _PortPairs=[ { PortName, DirectValue } | T ], OutputPortTable, State ) ->

	BinPortName = text_utils:string_to_binary( PortName ),

	% Here, as opposed to the input counterpart, we cannot create a bogus
	% (mostly uninitialized) channel value, as it will have to be sent to the
	% downstream blocks.
	%
	#output_port{ value_semantics=Semantics,
				  value_unit=Unit,
				  value_type=Type } =
		get_output_port( BinPortName, OutputPortTable, State ),

	% The argument was not a channel value, hence no real validation can be
	% performed, so we simply reuse the port metadata to directly transform this
	% direct value into a (valid by design) channel one:
	%
	ChannelValue = class_Dataflow:create_channel_value( DirectValue, Semantics,
														Unit, Type ),

	% Would be useless, as valid by design:
	%validate_value_for_output_port( ChannelValue, OutputPort, BinPortName,
	%								 State ),


	% As the channel value passed to assign_output_value/4 is only used to fetch
	% the actual value that it contains, we only create here a (bogus) empty
	% channel value, and set its value to the direct one specified as argument.
	%
	% We could have however preferred reading from the output port the various
	% expected metadata, and used class_Dataflow:create_channel_value/4 to
	% obtain a proper, corresponding channel value, but useless transformations
	% (ex: regarding types) would then have be performed.
	%
	OutputPort = table:get_value( BinPortName, OutputPortTable ),

	% The argument was not a channel value, hence no real validation can be
	% performed, so we simply reuse the port metadata to directly transform this
	% direct value into a channel one:
	%
	ChannelValue = class_Dataflow:create_channel_value( DirectValue ),

	% Not relevant by design:
	%validate_value_for_output_port( ChannelValue, OutputPort, BinPortName,
	%								 State ),

	% Minimal type-based checking done:
	NewOutputPort = assign_output_value( ChannelValue, OutputPort, BinPortName,
										 State ),

	NewOutputPortTable = table:update_entry( BinPortName, NewOutputPort,
											 OutputPortTable ),

	set_output_port_direct_values_helper( T, NewOutputPortTable, State ).



% Static section.


% Declares statically (i.e. exactly once per type of dataflow block) the
% associated semantics and types operations.
%
-spec declare_static_information_for(
		[ block_type() | managed_unit_spec() ] ) ->
									static_return( basic_utils:base_status() ).
declare_static_information_for( DataflowBlockTypes ) ->

	% Semantics and types are each aggregated per block first, then sent to
	% their respective server:
	%
	SemanticServerPid = class_SemanticServer:get_server(),
	TypeServerPid = class_TypeServer:get_server(),

	Status = declare_static_information_for( DataflowBlockTypes,
											 SemanticServerPid, TypeServerPid ),

	wooper:return_static( Status ).


% (helper)
-spec declare_static_information_for( [ block_type() | managed_unit_spec() ],
		semantic_server_pid(), type_server_pid() ) ->
											basic_utils:base_status().
declare_static_information_for( _DataflowBlockTypes=[], _SemanticServerPid,
								_TypeServerPid ) ->
	% No semantics to be declared here:
	ok;

declare_static_information_for( _DataflowBlockTypes=[ BlockTypeModule | T ],
								SemanticServerPid, TypeServerPid ) ->

	% A small attempt of simple interleaving between semantics and typing:

	IsSemanticWaited = get_and_trigger_semantics( BlockTypeModule,
												  SemanticServerPid ),

	IsTypeWaited = get_and_trigger_types( BlockTypeModule, TypeServerPid ),

	% Now, let's wait for the results and return them:
	case wait_for( IsSemanticWaited, IsTypeWaited ) of

		ok ->
			% Recurses on next block type then:
			declare_static_information_for( T, SemanticServerPid,
											TypeServerPid );

		{ error, Error } ->
			{ error, { static_declaration_failed, BlockTypeModule, Error } }

	end.




% Collects the semantics for specified dataflow block and, if obtained, declares
% them.
%
% Returns whether an acknowledgement is waited for.
%
-spec get_and_trigger_semantics( block_type() | managed_unit_spec(),
		semantic_server_pid() ) -> static_return( boolean() ).
get_and_trigger_semantics( BlockTypeModule, SemanticServerPid ) ->

	% Returns false or the actual semantics:
	SemToRequest = case get_declared_semantics( BlockTypeModule ) of

		no_semantics_declared ->

			% Then maybe we can infer these semantics from declared port
			% specifications?
			%
			case get_port_specifications( BlockTypeModule ) of

				no_port_specifications_declared ->
					false;

				{ InputPortSpecs, OutputPortSpecs } ->
					get_referenced_semantics( InputPortSpecs, OutputPortSpecs );

				false ->
					false

			end;

		_Semantics=[] ->
			false;

		_Semantics ->
			% Now deactivated, since we want to accept all semantics, even if
			% they are arbitrarily close:

			%Semantics
			false

	end,

	case SemToRequest of

		false ->
			% Nothing to wait for then:
			wooper:return_static( false );

		ActualSemantics ->
			%trace_utils:debug_fmt( "Requesting validation for semantics ~p.",
			%		   [ ActualSemantics ] ),

			% Will then trigger back a validation_outcome() as result:
			SemanticServerPid ! { validateSemantics, [ ActualSemantics ],
								  self() },
			wooper:return_static( true )

	end.



% Collects the types for specified dataflow block and, if obtained, declares
% them.
%
% Returns whether an acknowledgement is waited for.
%
-spec get_and_trigger_types( block_type() | managed_unit_spec(),
							 type_server_pid() ) -> static_return( boolean() ).
get_and_trigger_types( BlockTypeModule, TypeServerPid ) ->

	% Returns false or the actual types:
	TypeToRequest = case get_declared_types( BlockTypeModule ) of

		no_types_declared ->

			% Here we could only infer, thanks to port specifications, what are
			% the types that are *used*, not how they are *defined*. So there is
			% little that can be done here, statically.
			%
			false;

		_Types=[] ->
			false;

		Types ->
			Types

	end,

	case TypeToRequest of

		false ->
			% Nothing to check, just recurse:
			wooper:return_static( false );

		ActualTypes ->
			%io:format( "Requesting validation for types ~p.~n",
			%		   [ ActualTypes ] ),
			TypeServerPid ! { validateTypes, [ ActualTypes ], self() },
			wooper:return_static( true )

	end.



% Waits for the outcome of any semantic or type declaration.
wait_for( _IsSemanticWaited=false, _IsTypeWaited=false ) ->
	%trace_utils:debug( "Waiting over." ),
	ok;

wait_for( _IsSemanticWaited=true, IsTypeWaited ) ->
	%trace_utils:debug( "Semantics waited." ),
	receive

		{ wooper_result, semantics_accepted } ->
			%trace_utils:debug( "Semantics accepted." ),
			wait_for( false, IsTypeWaited );

		{ wooper_result, Reject={ semantics_rejected, _Reason } } ->
			{ error, Reject }

	end;

wait_for( IsSemanticWaited, _IsTypeWaited=true ) ->
	%trace_utils:debug( "Types waited." ),
	receive

		{ wooper_result, type_accepted } ->
			%trace_utils:debug( "Type(s) accepted." ),
			wait_for( IsSemanticWaited, false );

		{ wooper_result, Reject={ type_rejected, _Reason } } ->
			{ error, Reject }

	end.


% Returns all semantics used by specified ports.
-spec get_referenced_semantics( [ input_port_spec() ],
				[ output_port_spec() ] ) -> static_return( vocabulary() ).
get_referenced_semantics( InputPortSpecs, OutputPortSpecs ) ->

	InputSemantics = [ IS#input_port_spec.value_semantics
					   || IS <- InputPortSpecs ],

	OutputSemantics = [ OS#output_port_spec.value_semantics
						|| OS <- OutputPortSpecs ],

	FlatBinList = text_utils:strings_to_binaries(
				list_utils:flatten_once( InputSemantics ++ OutputSemantics ) ),

	wooper:return_static( list_utils:uniquify( FlatBinList ) ).



% Returns all types used by specified ports.
-spec get_referenced_types( [ input_port_spec() ], [ output_port_spec() ] ) ->
						static_return( [ type_entry() ] ).
get_referenced_types( InputPortSpecs, OutputPortSpecs ) ->

	InputTypes = [ IS#input_port_spec.value_type_description
				   || IS <- InputPortSpecs ],

	OutputTypes = [ OS#output_port_spec.value_type_description
					|| OS <- OutputPortSpecs ],

	wooper:return_static( list_utils:uniquify( InputTypes ++ OutputTypes ) ).




% Helper section.


% Returns whether the specified dataflow block class declared its semantics and,
% if yes, what they are.
%
-spec get_declared_semantics( block_type() | managed_unit_spec() ) ->
				static_return( 'no_semantics_declared' | vocabulary() ).
get_declared_semantics( { DataflowBlockClassname, erlang } ) ->
	Sem = get_declared_semantics( DataflowBlockClassname ),
	wooper:return_static( Sem );


get_declared_semantics( { DataflowBlockClassname, Language } ) ->

	ActualClassname = dataflow_binding_utils:get_erlang_unit_type( Language ),

	SemFunName = get_declared_semantics,

	Sem = case meta_utils:is_function_exported( ActualClassname, SemFunName,
												_Arity=1 ) of

		true ->
			ActualClassname:SemFunName( DataflowBlockClassname );

		false ->
			no_semantics_declared

	end,
	wooper:return_static( Sem );


get_declared_semantics( DataflowBlockClassname ) ->

	SemFunName = get_declared_semantics,

	Sem = case meta_utils:is_function_exported( DataflowBlockClassname,
												SemFunName, _Arity=0 ) of

		true ->
			DataflowBlockClassname:SemFunName();

		false ->
			no_semantics_declared

	end,
	wooper:return_static( Sem ).



% Returns whether the specified dataflow block class declared its types and,
% if yes, what they are.
%
-spec get_declared_types( block_type() | managed_unit_spec() ) ->
						static_return( 'no_types_declared' | type_entries() ).
get_declared_types( { DataflowBlockClassname, erlang } ) ->
	Types = get_declared_types( DataflowBlockClassname ),
	wooper:return_static( Types );


get_declared_types( { DataflowBlockClassname, Language } ) ->

	ActualClassname = dataflow_binding_utils:get_erlang_unit_type( Language ),

	TypeFunName = get_declared_types,

	Types = case meta_utils:is_function_exported( ActualClassname, TypeFunName,
												  _Arity=1 ) of

		true ->
			ActualClassname:TypeFunName( _TypeArgs=[ DataflowBlockClassname ] );

		false ->
			no_types_declared

	end,
	wooper:return_static( Types );


get_declared_types( DataflowBlockClassname ) ->

	TypeFunName = get_declared_types,

	Types = case meta_utils:is_function_exported( DataflowBlockClassname,
												  TypeFunName, _Arity=0 ) of

		true ->
			DataflowBlockClassname:TypeFunName();

		false ->
			no_types_declared

	end,
	wooper:return_static( Types ).



% Returns all static (class-level) information that can be collected regarding
% the actual dataflow block at hand.
%
% The specified state is expected to correspond to a (direct or not) instance of
% class_TraceEmitter.
%
% (exported helper, for convenience)
%
-spec get_static_information( wooper:state() ) ->
		{ vocabulary(), type_entries() }.
get_static_information( State ) ->

	BasicClassname = wooper:get_classname( State ),

	ActualUnitSpec = case BasicClassname of

		class_DataflowPythonProcessingUnit ->
			WOOPERPythonClass =
			  wooper_utils:pep8_class_to_wooper_class( ?getAttr(python_class) ),
			{ WOOPERPythonClass, python };

		%class_DataflowJavaProcessingUnit ->
		%	WOOPERJavaClass = wooper_utils:java_class_to_wooper_class(
		%						?getAttr(java_class) ),
		%	{ WOOPERJavaClass, java };

		AnyOtherClassname ->
			AnyOtherClassname

	end,

	get_static_information( ActualUnitSpec, State ).



-spec get_static_information( managed_unit_spec(), wooper:state() ) ->
									{ vocabulary(), type_entries() }.
get_static_information( UnitSpec, State ) ->

	Vocabulary = case get_declared_semantics( UnitSpec ) of

		no_semantics_declared ->
			% Warning would be overkill, yet...
			?notice( "This dataflow block type does not declare its "
					 "semantics." ),
			[];

		Semantics ->
			Semantics

	end,

	TypeEntries = case get_declared_types( UnitSpec ) of

		no_types_declared ->
			% Warning would be overkill, yet...
			?notice( "This dataflow block type does not declare its typing "
					 "information." ),
			[];

		Types ->
			Types

	end,

	{ Vocabulary, TypeEntries }.



% Returns the static (class-level) port specifications that can be collected
% regarding the actual dataflow block at hand.
%
% The specified state is expected to correspond to a (direct or not) instance of
% class_TraceEmitter.
%
% (exported helper, for convenience)
%
-spec get_port_specifications( block_type() | managed_unit_spec() ) ->
	'no_port_specifications_declared' |
		{ [ input_port_spec() ], [ output_port_spec() ] }.
get_port_specifications( { DataflowBlockClassname, erlang } ) ->
	get_port_specifications( DataflowBlockClassname );

get_port_specifications( { DataflowBlockClassname, Language } ) ->

	ActualClassname = dataflow_binding_utils:get_erlang_unit_type( Language ),

	PortFunctionName = get_port_specifications,

	case meta_utils:is_function_exported( ActualClassname, PortFunctionName,
										  _Arity=1 ) of

		true ->
			ActualClassname:PortFunctionName( DataflowBlockClassname );

		false ->
			no_port_specifications_declared

	end;

get_port_specifications( DataflowBlockClassname ) ->

	PortFunctionName = get_port_specifications,

	case meta_utils:is_function_exported( DataflowBlockClassname,
										  PortFunctionName, _Arity=0 ) of

		true ->
			DataflowBlockClassname:PortFunctionName();

		false ->
			no_port_specifications_declared

	end.



% Creates dynamically a set of output ports.
-spec createOutputPorts( wooper:state(), [ output_port_spec() ] ) ->
								request_return( 'output_ports_created' ).
createOutputPorts( State, OutputPortSpecs ) ->

	{ NewOutputTable, NewOutputIterationTable } = register_output_ports(
		OutputPortSpecs, ?getAttr(output_ports), ?getAttr(output_iterations),
		?getAttr(semantic_server_pid), ?getAttr(type_server_pid), State ),

	OutputState = setAttributes( State, [
			{ output_ports, NewOutputTable },
			{ output_iterations, NewOutputIterationTable } ] ),

	wooper:return_state_result( OutputState, output_ports_created ).



% Included for following sections:
%  - validations
%  - checking
%  - textual descriptions (to_string)
%
-include("class_DataflowBlock_functions.hrl").
