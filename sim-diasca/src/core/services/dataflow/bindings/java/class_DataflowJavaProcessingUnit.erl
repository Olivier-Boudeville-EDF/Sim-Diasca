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


-module(class_DataflowJavaProcessingUnit).


-define( class_description,
		 "Base class for all the Java-based processing units, i.e. units that "
		 "rely on a Java implementation through the Sim-Diasca binding." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowProcessingUnit ] ).



% Designates the Java reference corresponding to a processing unit instance
% (relatively to its worker thread):
%
-type java_ref() :: basic_utils:count().


-export_type([ java_ref/0 ]).


% Helpers:
-export([ to_string/1 ]).



% The attributes specific to a Java processing unit are:
-define( class_attributes, [

	{ java_worker_mbox, java_utils:java_mbox_pid(),
	  "the PID of the (worker) mailbox in charge of this Java processing "
	  "unit" },

	{ java_full_class, java_utils:java_fully_qualified_classname(),
	  "the full name of the Java class providing the actual implementation of "
	  "this processing unit" },

	{ java_instance_ref, java_ref(),
	  "reference that identifies a Java processing unit, relatively to its "
	  "worker thread" } ] ).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Bindings.JavaProcessingUnit" ).


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
% Each of these processing units uses the Java binding API in order to rely on a
% Java-based implementation: this type of processing unit is an Erlang
% lightweight, mostly empty shell (a mere process) that simply defers its actual
% processing to a Java interpreter (a node-local JVM; more precisely: the worker
% thread in charge of this unit), and reinjects back its results in the
% simulation by modifying its usual unit state accordingly.


% Implementation notes:
%
% Operations (such as construction or activation) are thus deferred to the
% related Java interpreter, and their result is then waited for by this Erlang
% unit counterpart.




% Constructs a new dataflow Java processing unit:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as automatically assigned by the load balancer
%
% - UnitClassname is the (WOOPER) classname of this unit (of course its Java
% counterpart must have been defined and be available); ex:
% 'class_TransportationDemandUnit'
%
% - JavaConstructionParameters is the list of the construction parameters that
% will be used to instantiate the corresponding Java processing unit instance
% in its interpreter; the first argument must be the name of this unit, which is
% also stored in the (Erlang) actor state
%
% - DataflowPid is the PID identifying the dataflow to which this processing
% unit belongs
%
% - JavaBindingManagerPid is the PID of the Java binding manager in charge of
% that actor
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 dataflow_unit_type(), construction_parameters(),
				 dataflow_pid(), java_binding_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, UnitClassname,
		   JavaConstructionParameters=[ UnitName | OtherParams ],
		   DataflowPid, JavaBindingManagerPid ) ->

	% Deduces from the specified (WOOPER) classname the name (as an atom) of
	% the:
	%
	% - Java package (directory)
	% - package (filename)
	% - class of the instance being constructed, from the declared classname
	%
	% (note that a package name may or may not be returned in addition to the
	% classname)
	%
	JavaFullyQualifiedClassname =
		wooper_utils:get_java_package_and_class_for( UnitClassname ),

	% We used to rely on the controller mailbox of the local JVM for instance
	% creations, now we prefer selecting any worker mailbox for that.

	% Instantiates the Java processing unit in its associated worker thread, and
	% gets back the initial data (attributes or not) needed in the Erlang world:

	JavaInitialData = [ JavaFullyQualifiedClassname,
						JavaConstructionParameters ],

	TraceEmitterInfo = ?trace_categorize(UnitName),

	% The worker mailbox used for the instance creation will be the one used
	% afterwards for all Java-side processings for this instance:
	%
	Res = java_binding_utils:execute_request_locally( instantiate_unit,
					JavaInitialData, JavaBindingManagerPid, TraceEmitterInfo ),

	?debug_fmt( "Received from Java:~n~p", [ Res ] ),

	{ ErlangInitialData, WorkerMailboxPid } = Res,

	?debug_fmt( "ErlangInitialData:~n~p", [ ErlangInitialData ] ),

	% Separates and decodes the attributes received from the Java world, in
	% standard (Erlang) dataflow terms:
	%
	% (activation policy downcased in the encode/1 method of the
	% AutoHidingEnumprocess)
	%
	{ JavaInstanceRef, BinActivationPolicy, EncodedInputPortSpecs,
	  EncodedOutputPortSpecs } = ErlangInitialData,

	?debug_fmt( "JavaInstanceRef:~p", [ JavaInstanceRef ] ),
	?debug_fmt( "BinActivationPolicy:~p", [ BinActivationPolicy ] ),
	?debug_fmt( "EncodedInputPortSpecs:~n~p", [ EncodedInputPortSpecs ] ),
	?debug_fmt( "EncodedOutputPortSpecs:~n~p", [ EncodedOutputPortSpecs ] ),

	ActivationPolicy = text_utils:binary_to_atom( BinActivationPolicy ),

	InputPortSpecs = [ begin
		DecodedIPS =
			class_LanguageBindingManager:decode_input_port_specs( EIPS ),
		class_DataflowBlock:parse_raw_input_port_spec( DecodedIPS )
					   end || EIPS <- EncodedInputPortSpecs ],

	OutputPortSpecs = [ begin
		DecodedOPS =
			class_LanguageBindingManager:decode_output_port_specs( EOPS ),
		class_DataflowBlock:parse_raw_output_port_spec( DecodedOPS )
						end || EOPS <- EncodedOutputPortSpecs ],

	% Constructs then the corresponding direct mother class:
	UnitState = class_DataflowProcessingUnit:construct( State, ActorSettings,
		TraceEmitterInfo, ActivationPolicy, InputPortSpecs, OutputPortSpecs,
		DataflowPid ),

	?send_debug_fmt( UnitState, "Created a Java-based processing unit "
		"instance named '~ts' (extra construction parameters: ~p) "
		"of type '~ts' (corresponding in Java ~ts, relying on the "
		"Java worker mailbox ~w), in the context of dataflow ~p.",
		[ UnitName, OtherParams, UnitClassname, JavaFullyQualifiedClassname,
		  WorkerMailboxPid, DataflowPid ] ),

	% Sets the class-specific attributes:
	setAttributes( UnitState, [
		{ java_worker_mbox, WorkerMailboxPid },
		{ java_full_class, JavaFullyQualifiedClassname },
		{ java_instance_ref, JavaInstanceRef } ] ).




% Methods section.


% Callback executed automatically whenever the processing unit is activated.
%
% Meant to be overridden.
%
-spec activate( wooper:state() ) -> oneway_return().
activate( State ) ->

	JavaFullClass = ?getAttr(java_full_class),

	?info_fmt( "Activating and evaluating now the unit named '~ts' "
		"implemented through Java ~ts.",
		[ ?getAttr(name),
		  java_utils:fully_qualified_classname_to_string( JavaFullClass ) ] ),

	% Gathers the pieces of simulation context that could be of any use during
	% the computations involved in the Java activate/1 method:
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
	% might be necessary for computations involved in the Java activate/1
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
	% associated Java processing unit and gets back its computation results:

	MboxPid = ?getAttr(java_worker_mbox),

	JavaActivationData = [ ?getAttr(java_instance_ref), SimulationData,
		InputPortStatuses, InputPortIterationPieces,
		OutputPortIterationPieces ],

	ActivationResults = java_binding_utils:execute_request( MboxPid,
		activate_unit, JavaActivationData, State ),

	?debug_fmt( "Just after activation, following ~B output ports are "
		"to be set: ~ts", [ length( ActivationResults ),
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




% Static section.


% To denote traces (typically error ones) that are sent from a Java unit yet
% in a static context (hence with no sensible trace emitter name).
%
-spec get_static_trace_info( dataflow_unit_type() ) ->
									static_return( traces:emitter_info() ).
get_static_trace_info( UnitType ) ->

	EmitterName = text_utils:atom_to_string( UnitType )
		% ++ " (in static context)",
		++ "-static",

	wooper:return_static( { EmitterName, ?trace_emitter_categorization } ).



% Returns the specifications for the input and output ports of that dataflow
% processing unit.
%
-spec get_port_specifications( dataflow_unit_type() ) ->
	static_return( { [ input_port_spec() ], [ output_port_spec() ] }
				   | 'no_port_specifications_declared' ).
get_port_specifications( UnitType ) ->

	JavaFullClass =
		wooper_utils:get_java_package_and_class_for( UnitType ),

	% Requests the encoded port specifications from this Java package:
	RequestResult = java_binding_utils:execute_request_locally(
		get_port_specifications, [ JavaFullClass ],
		get_static_trace_info( UnitType ) ),

	trace_utils:debug_fmt( "get_port_specifications returned '~ts' type (~ts).",
				[ type_utils:get_type_of( RequestResult ), RequestResult ] ),

	% Returns the 'no_port_specifications_declared' atom if no static port
	% declaration has been found in Java, otherwise decodes the specifications
	% received:

	NoSpecAtom = no_port_specifications_declared,

	Res = case RequestResult of

		NoSpecAtom ->
			NoSpecAtom;

		"no_port_specifications_declared" ->
			NoSpecAtom;

		<<"no_port_specifications_declared">> ->
			NoSpecAtom;

		{ EncodedInputPortSpecs, EncodedOutputPortSpecs } ->

			InputPortSpecs = [ begin
			  DecodedIPS = class_LanguageBindingManager:decode_input_port_specs(
							 EIPS ),
			  class_DataflowBlock:parse_raw_input_port_spec( DecodedIPS )
							   end || EIPS <- EncodedInputPortSpecs ],

			OutputPortSpecs = [ begin
				DecodedOPS =
				  class_LanguageBindingManager:decode_output_port_specs( EOPS ),
				class_DataflowBlock:parse_raw_output_port_spec( DecodedOPS )
								end || EOPS <- EncodedOutputPortSpecs ],

			{ InputPortSpecs, OutputPortSpecs }

	end,

	wooper:return_static( Res ).



% Returns the semantics statically declared by this unit.
%
% Defining this method allows to ensure that all the ports ever created by this
% unit will use semantics among this explicitly stated list.
%
% Otherwise the list would be deduced from the initial port specifications, with
% no specific control.
%
-spec get_declared_semantics( dataflow_unit_type() ) -> static_return(
		class_SemanticServer:vocabulary() | 'no_semantics_declared' ).
get_declared_semantics( UnitType ) ->

	JavaFullClass =
		wooper_utils:get_java_package_and_class_for( UnitType ),

	% Requests the encoded semantics usable for dataflow ports and channels from
	% this Java package:
	%
	RequestResult = java_binding_utils:execute_request_locally(
		get_declared_semantics, [ JavaFullClass ],
		get_static_trace_info( UnitType ) ),

	trace_utils:debug_fmt( "get_declared_semantics returned '~ts' type (~p).",
		[ type_utils:get_type_of( RequestResult ), RequestResult ] ),

	% Returns the 'no_semantics_declared' atom if no static declaration of
	% semantics has been found in Java, or decodes the received one otherwise:

	NoSemAtom = no_semantics_declared,

	Res = case RequestResult of

		NoSemAtom ->
			NoSemAtom;

		% Mimics the behaviour of the 'get_and_trigger' methods in
		% class_DataflowBlock.erl, for which empty declarations are equivalent
		% to no declaration at all:
		%
		[] ->
			NoSemAtom;

		"no_semantics_declared" ->
			NoSemAtom;


		<<"no_semantics_declared">> ->
			NoSemAtom;

		EncodedSemantics when is_list( EncodedSemantics ) ->
			[ text_utils:binary_to_atom( BinSem )
			  || BinSem <- EncodedSemantics ]

	end,

	wooper:return_static( Res ).



% Returns the types statically declared by this unit.
-spec get_declared_types( dataflow_unit_type() ) -> static_return(
		'no_types_declared' | class_TypeServer:type_entries() ).
get_declared_types( UnitType ) ->

	{ JavaPackage, JavaClass } =
		wooper_utils:get_java_package_and_class_for( UnitType ),

	% Requests the encoded strings describing the types accepted by each port,
	% from this Java package:
	%
	RequestResult = java_binding_utils:execute_request_locally(
		get_declared_types, [ JavaPackage, JavaClass ],
		get_static_trace_info( UnitType ) ),

	% Returns the 'no_types_declared' atom if no static declaration of value
	% types has been found in Java, or decodes the received one otherwise:

	NoTypeAtom = no_types_declared,

	Res = case RequestResult of

		NoTypeAtom ->
			NoTypeAtom;

		% Mimics the behaviour of the 'get_and_trigger' methods in
		% class_DataflowBlock.erl, for which empty declarations are equivalent
		% to no declaration at all:
		[] ->
			NoTypeAtom;

		"no_types_declared" ->
			no_types_declared;

		<<"no_types_declared">> ->
			no_types_declared;

		EncodedTypes when is_list( EncodedTypes ) ->
			[ { text_utils:binary_to_atom( BinTypeName ),
				text_utils:binary_to_string( BinExplicitType ) }
			  || { BinTypeName, BinExplicitType } <- EncodedTypes ]

	end,

	wooper:return_static( Res ).




% Helpers section.



% Textual helpers section.


% Returns a textual description of this processing unit.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	{ InputDetailed, OutputDetailed } =
		class_DataflowBlock:io_to_string( State ),

	UnitClassname =
		wooper_utils:pep8_class_to_wooper_class( ?getAttr(java_class) ),

	PackString = case ?getAttr(java_package) of

		"" ->
			"no package used";

		P ->
			text_utils:format( "whose package is '~ts'", [ P ] )

	end,

	text_utils:format( "Java processing unit named '~ts' of type ~p, "
		"implemented in class '~ts' (~ts), ruled by the activation policy '~p' "
		"and associated to worker mailbox ~w.~n Inputs: ~ts~nOutputs: ~ts~n",
		[ ?getAttr(name), UnitClassname, ?getAttr(java_class), PackString,
		  ?getAttr(activation_policy), ?getAttr(java_worker_mbox),
		  InputDetailed, OutputDetailed ] ).
