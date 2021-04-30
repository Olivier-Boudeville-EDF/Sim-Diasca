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

% Author: Robin Huart (robin-externe.huart@edf.fr)


-module(class_DataflowPythonProcessingUnit).


-define( class_description,
		 "Base class for all the Python-based processing units, i.e. units "
		 "that rely on a Python implementation through the Sim-Diasca "
		 "binding." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowProcessingUnit ] ).



% Designates the Python reference corresponding to a processing unit instance
% (relatively to its interpreter):
%
-type python_ref() :: basic_utils:count().


-export_type([ python_ref/0 ]).


% Helpers:
-export([ get_python_module_and_class_for/1, to_string/1 ]).





% The attributes specific to a Python processing unit are:
-define( class_attributes, [

	{ python_interpreter_pid, python_utils:interpreter_pid(),
	  "PID of the Python interpreter in which the Python processing unit has "
	  "been instantiated" },

	% Not exactly python_utils:pep8_class_module():
	{ python_module, file_utils:bin_file_name(),
	  "name of the Python module, as a filename including the '.py' extension, "
	  "in which this processing unit is implemented" },

	% Not exactly python_utils:pep8_classname():
	{ python_class, text_utils:bin_string(),
	  "name of the Python class providing the actual implementation of this "
	  "processing unit (as a Python PEP8 classname - yet not as an atom)" },

	{ python_instance_ref, python_ref(),
	  "reference that identifies the Python processing unit in its "
	  "interpreter" } ] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Bindings.PythonProcessingUnit" ).


% For types and shorthands related to dataflows:
-include("dataflow_defines.hrl").

% For types and shorthands related to bindings:
-include("bindings.hrl").

% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").

% Allows to use macros for sending standalone traces:
-include_lib("traces/include/traces.hrl").

% For input_port and all:
-include("class_DataflowBlock_defines.hrl").


% Shorthands:
-type ustring() :: text_utils:ustring().



% Design notes:
%
% Each of these processing units uses the Python binding API in order to rely on
% a Python-based implementation: this type of processing unit is an Erlang
% lightweight, mostly empty shell that simply defers its actual processing to a
% Python interpreter, and reinjects back its results in the simulation by
% modifying its usual unit state accordingly.


% Implementation notes:
%
% Operations (such as construction or activation) are thus deferred to the
% related Python interpreter, and their result is then waited for by this Erlang
% unit counterpart.




% Constructs a new dataflow Python processing unit:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as automatically assigned by the load balancer
%
% - UnitClassname is the (WOOPER) classname of this unit (its Python counterpart
% must be available); ex: 'class_TransportationDemandUnit'
%
% - PythonConstructionParameters is the list of the construction parameters that
% will be used to instantiate the corresponding Python processing unit instance
% in its interpreter; the first argument must be the name of this unit, which is
% also stored in the (Erlang) actor state
%
% - DataflowPid is the PID identifying the dataflow to which this processing
% unit belongs
%
% - PythonBindingManagerPid is the PID of the Python runtime manager in charge
% of that actor
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		 dataflow_unit_type(), construction_parameters(),
		 dataflow_pid(), python_binding_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, UnitClassname,
		   PythonConstructionParameters=[ UnitName | OtherParams ],
		   DataflowPid, PythonBindingManagerPid ) ->

	% Deduces from the specified (WOOPER) classname the name (as an atom) of
	% the:
	%
	%  - Python package (directory)
	%  - module (filename)
	%  - class of the instance being constructed, from the declared classname
	%
	{ PythonUnitModule, PythonUnitClassname } =
		get_python_module_and_class_for( UnitClassname ),

	% Gets the Python interpreter in charge of that unit:
	InterpreterPid = class_PythonBindingManager:get_interpreter(
						PythonBindingManagerPid ),

	% Instantiates the Python processing unit in its interpreter, and gets back
	% the initial data (attributes or not) needed in the Erlang world:
	%
	PythonInitialData = [ PythonUnitModule, PythonUnitClassname ]
		++ PythonConstructionParameters,

	TraceEmitterInfo = ?trace_categorize(UnitName),

	ErlangInitialData = python_binding_utils:execute_request( InterpreterPid,
		instantiate_unit, PythonInitialData, TraceEmitterInfo ),

	% Separates and decodes the attributes received from the Python world, in
	% standard (Erlang) dataflow terms:
	%
	% (activation policy downcased in the encode/1 method of the
	% AutoHidingEnumprocess)
	%
	{ PythonInstanceRef, BinActivationPolicy, EncodedInputPortSpecs,
	  EncodedOutputPortSpecs } = ErlangInitialData,

	%trace_utils:debug_fmt( "Instance of the Python-based unit of class "
	%    "'~ts' (defined in module '~p') created by its "
	%    "interpreter: its reference is ~p, "
	%    "its activation policy is ~p.",
	%    [ PythonUnitClassname, PythonUnitModule,
	%      PythonInstanceRef, BinActivationPolicy ] ),

	ActivationPolicy = text_utils:binary_to_atom( BinActivationPolicy ),

	InputPortSpecs = [ begin
	  DecodedIPS = class_LanguageBindingManager:decode_input_port_specs( EIPS ),
	  class_DataflowBlock:parse_raw_input_port_spec( DecodedIPS )
					   end || EIPS <- EncodedInputPortSpecs ],

	OutputPortSpecs = [ begin
	  DecodedOPS = class_LanguageBindingManager:decode_output_port_specs(
					 EOPS ),
		class_DataflowBlock:parse_raw_output_port_spec( DecodedOPS )
						end || EOPS <- EncodedOutputPortSpecs ],

	% Constructs then the corresponding direct mother class:
	UnitState = class_DataflowProcessingUnit:construct( State, ActorSettings,
					TraceEmitterInfo, ActivationPolicy,
					InputPortSpecs, OutputPortSpecs, DataflowPid ),

	?send_debug_fmt( UnitState, "Created a Python-based processing unit "
		"instance named '~ts' (extra construction parameters: ~p) "
		"of type '~ts' (corresponding in Python to classname '~ts' "
		"of module '~ts', relying on the (Python) binding manager "
		"~w), in the context of dataflow ~p.",
		[ UnitName, OtherParams, UnitClassname, PythonUnitClassname,
		  PythonUnitModule, PythonBindingManagerPid, DataflowPid ] ),

	% Sets the class-specific attributes:
	setAttributes( UnitState, [
		{ python_interpreter_pid, InterpreterPid },
		{ python_module, PythonUnitModule },
		{ python_class, PythonUnitClassname },
		{ python_instance_ref, PythonInstanceRef } ] ).




% Methods section.


% Callback executed automatically whenever the processing unit is activated.
%
% Meant to be overridden.
%
-spec activate( wooper:state() ) -> oneway_return().
activate( State ) ->

	PythonClass = ?getAttr(python_class),

	?info_fmt( "Activating and evaluating now the unit named '~ts' "
		"of type '~ts' (implemented as Python class '~ts').",
		[ ?getAttr(name),
		  wooper_utils:pep8_class_to_wooper_class( PythonClass ),
		  PythonClass ] ),

	% Gathers the pieces of simulation context that could be of any use during
	% the computations involved in the Python activate/1 method:
	%
	SimulationDate = executeConstRequest( State, getSimulationDate ),

	SimulationData = { ?getAttr(current_tick_offset), SimulationDate },

	% Builds the exhaustive list of all port statuses (with their names):
	InputPortStatuses =
		class_LanguageBindingManager:get_encoded_input_ports_data( State ),

	?debug_fmt( "Just before activation, the ~B input ports are in "
		"following state: ~ts",
		[ length( InputPortStatuses ),
		  text_utils:strings_to_sorted_string(
			[ text_utils:format( "'~ts' is ~ts", [ IPName,
						dataflow_support:value_status_to_string( ValStatus ) ] )
			  || { IPName, ValStatus } <- InputPortStatuses ] ) ] ),

	% Builds the list of encoded data relative to input port iterations that
	% might be necessary for computations involved in the Python activate/1
	% method:
	%
	InputPortIterationPieces =
		class_LanguageBindingManager:get_encoded_input_port_iterations_data(
		  State ),

	% Builds the list of encoded data relative to output port iterations that
	% might be necessary for the unit to know how many output ports are actually
	% instantiated:
	%
	OutputPortIterationPieces =
		class_LanguageBindingManager:get_encoded_output_port_iterations_data(
		  State ),

	% Gathers all relevant input data, calls the activate/1 method of the
	% associated Python processing unit and gets back its computation results:

	InterpreterPid = ?getAttr(python_interpreter_pid),

	PythonActivationData = [ ?getAttr(python_instance_ref), SimulationData,
							 InputPortStatuses, InputPortIterationPieces,
							 OutputPortIterationPieces ],

	ActivationResults = python_binding_utils:execute_request( InterpreterPid,
							activate_unit, PythonActivationData, State ),

	?debug_fmt( "Just after activation, following ~B output ports are "
		"to be set: ~ts",
		[ length( ActivationResults ),
		  text_utils:strings_to_sorted_string(
			[ text_utils:format( "'~ts' set to: ~p", [ OPName,
					%class_DataflowBlock:value_to_string( ChValue ) ] )
													   ChValue ] )
			  || { OPName, ChValue } <- ActivationResults ] ) ] ),

	% Interprets then ActivationResults as a list of tasks to achieve on the
	% output ports, then performs them:
	%
	FinalState = class_LanguageBindingManager:apply_activation_results(
				   ActivationResults, State ),

	wooper:return_state( FinalState ).




% Helpers section.



% Returns the Python module and class that correspond to the specified (Erlang)
% unit type, i.e. a WOOPER classname, like
% 'class_BigPackage__MyPackage__MyExample', resulting in:
% { 'big_package.my_package.my_example', 'MyExample' }.
%
-spec get_python_module_and_class_for( dataflow_unit_type() ) ->
		{ python_utils:pep8_class_module(), python_utils:pep8_classname() }.
get_python_module_and_class_for( UnitType ) ->

	% For instance, let's suppose UnitType = "class_Package__Module".

	% Then PythonPackageAndClass = 'Package__Module':
	PythonPackageAndClass = wooper_utils:wooper_class_to_pep8_class( UnitType ),

	StrPythonPackageAndClass =
		text_utils:atom_to_string( PythonPackageAndClass ),

	% ["Package", "Module"]:
	SplitElems =
		string:split( StrPythonPackageAndClass, _Pattern="__", _Where=all ),

	% Then PythonClass = 'Module':
	PythonClass = text_utils:string_to_atom( lists:last( SplitElems ) ),

	% Then PythonModule = "package_.module"
	PythonModule =
		python_utils:pep8_class_to_pep8_module( PythonPackageAndClass ),

	{ PythonModule, PythonClass }.




% Textual helpers section.


% Returns a textual description of this processing unit.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	{ InputDetailed, OutputDetailed } =
		class_DataflowBlock:io_to_string( State ),

	UnitClassname =
		wooper_utils:pep8_class_to_wooper_class( ?getAttr(python_class) ),

	text_utils:format( "Python processing unit named '~ts' of type ~p, "
		"implemented in ~p and ruled by the activation policy '~p'.~n "
		"Inputs: ~ts~nOutputs: ~ts~n",
		[ ?getAttr(name), UnitClassname, ?getAttr(python_module),
		  ?getAttr(activation_policy), InputDetailed, OutputDetailed ] ).
