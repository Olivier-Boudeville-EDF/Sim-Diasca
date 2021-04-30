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

% Authors: Olivier Boudeville (olivier.boudeville@edf.fr)
%		   Samuel Thiriot (samuel.thiriot@edf.fr)


-module(class_DataflowObject).


-define( class_description,
		 "Dataflow object class, corresponding to the implementation of the "
		 "state-related components of a dataflow. "
		 "This is a specialization of the generic dataflow block. "
		 "Dataflow objects may be direct instances of the current class "
		 "(with then no specific overriding needed). "
		 "Please refer to the 'Sim-Diasca Dataflow HOWTO' for further "
		 "information." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_DataflowBlock ] ).



% First-stage, optional initialisation:
-export([ pre_init/2 ]).


% Convenience peer-related exports:
-export([ register_peer/3, unregister_peer/3,
		  get_unique_peer_for/2, set_unique_peer_for/3,
		  get_multiple_peers_for/2 ]).


% Exported helpers:
-export([ unset_attribute/2 ]).


% For the input_port record and all:
-include("class_DataflowBlock_defines.hrl").



% Design notes:
%
% Each actual dataflow object (inheriting from this class) shall of course
% declare the specifications of its dataflow attributes.
%
% The get_declared_semantics/0 and get_declared_types/0 static methods may also
% be defined, respectively to declare the semantics and types a given dataflow
% object may rely upon.



% The attributes that are specific to a dataflow object are:
-define( class_attributes, [

	{ attribute_specs, [ dataflow_attribute_spec() ],
	  "a list of the attribute specifications used to create this dataflow "
	  "object; useful whenever having to set a proper channel value to a given "
	  "attribute" },

	{ unique_peer_table, unique_peer_table(),
	  "a table that associates, to each (single) peer associated to a unique "
	  "peer type, its value (if any)" },

	{ multiple_peer_table, multiple_peer_table(),
	  "a table that associates, to each peer associated to a multiple peer "
	  "type, its value (if any)" },

	{ peer_to_name_table, peer_to_attribute_table(),
	  "a table allowing to determine, for a given (unique) peer, the "
	  "associated dataflow attribute name publishing its external identifier" },

	{ accept_multiple_valued_attrs, boolean(),
	  "tells whether having more than one initial value defined per attribute "
	  "is accepted (generally set to false)" },

	{ accept_non_valued_attrs, boolean(),
	  "tells whether having no initial value defined for an attribute is "
	  "accepted; generally set to true, as no initial values are usually set "
	  "for output ports" },

	{ accept_extra_attr_values, boolean(),
	  "tells whether initial values provided for non-existing dataflow "
	  "attributes are allowed" },

	{ identification_server_pid, maybe( identification_server_pid() ),
	  "PID of the identification server (if any)" } ] ).



% Two small templates for child classes in comments:


% Returns the semantics statically declared by this dataflow object.
%
% Defining this method allows to ensure that all the attributes ever added by
% this dataflow block will rely on user-level semantics among this explicitly
% stated list.
%
% Otherwise the list would be deduced from the initial attribute specifications,
% with no specific control.
%
%-spec get_declared_semantics() -> static_return( user_vocabulary() ).
%get_declared_semantics() ->
%    wooper:return_static( [ "an example" ] ).


% Returns the types statically declared by this dataflow object.
%-spec get_declared_types() -> static_return( class_TypeServer:type_entries() ).
%get_declared_types() ->
%    wooper:return_static( [ { 'my_type', "'something'|'other thing'" } ] ).



% About attributes, ports and suspension.

% Each attribute of a dataflow object is thus embodied by exactly two ports: an
% input one (to set it), and an output one (to notify downstream blocks of a new
% value thereof).
%
% When a dataflow object is constructed, it starts in suspended mode; otherwise,
% being then by design not connected yet, assigning its output ports at creation
% time to their respective initial values would not trigger any downstream
% block.
%
% Instead, the dataflow object being initially suspended, all the initial
% settings of its (output) ports will be taken into account when it will be
% resumed (most probably by its dataflow, on behalf of the experiment manager),
% so that their initial values may propagate directly during the resulting
% dataflow evaluation.


% About peers.

% A dataflow object may declare possible types of peers (i.e. dataflow objects
% that are referenced by this one), each of these types being described by an
% atom (ex: 'parent', 'children', 'components', 'wheels', 'rooms', etc.).
%
% There are two sorts of peer types:
%
% - unique peer types, which can only be linked up to once, i.e. the given
% dataflow object shall be associated to zero or one designated instance for
% that peer type (ex: if a dataflow object has a single 'parent' object); the
% corresponding value is then either 'undefined' or the PID of that (parent,
% here) instance; additionally, a unique peer may have its external identifier
% published as a dataflow port of that object (ex: as a port named "parent_id"),
% so that this information can be available to the rest of the dataflow
%
% - multiple peer types, any number of them being possibly associated to the
% dataflow object of interest: it then stores a list of the PID of instances of
% that peer type; ex: 'children')
%
% Unless specified otherwise, a peer is designated by a PID (i.e. the value held
% by a peer type is of type 'pid').
%
% Should a (unique) peer declare an associated dataflow attribute, then the
% dataflow values held by the corresponding ports are expected to be external
% identifiers, assumed here to be strings (at worse non-string identifiers may
% be serialised); indeed domain-specific values shall preferably be exchanged
% over the dataflow rather than technical, transient identifiers like PIDs.


% Name of a dataflow object:
-type object_name() :: string().


% Describes the type of a peer (ex: 'parent', 'neighbours', 'potential_link',
% etc.):
%
-type peer_type() :: atom().


% PID of a peer dataflow object:
-type peer_pid() :: object_pid().



% The name (as a binary string) of the dataflow attribute (if any) associated to
% a given type of (unique) peer:
%
-type peer_attribute_name() :: dataflow_attribute_bin_name().


% Designates whether a given peer type is unique or not:
-type peer_multiplicity() :: 'unique' | 'multiple'.


% Describes how a unique peer type is to be declared:
-type unique_peer_spec() :: peer_type()
						  | { peer_type(), peer_attribute_name() }.


% Describes how a multiple peer type is to be declared:
-type multiple_peer_spec() :: peer_type().


% Table storing a potential value for the single peer attached to each of the
% known unique peer types.
%
% Default values are the 'undefined' atom.
%
-type unique_peer_table() :: table( peer_type(), maybe( peer_pid() ) ).



% Table storing a potential value for each peer attached to each of the known
% multiple peer types.
%
-type multiple_peer_table() :: table( peer_type(), [ peer_pid() ] ).



% Associates, to a (unique) peer type, the corresponding dataflow attribute
% (hence port) name (as a plain string).
%
-type peer_to_name_table() :: table( peer_type(), peer_attribute_name() ).


% Describes an attribute update in the form of { AttributeName,
% AttributeNewValue }:
%
-type attribute_update() :: { dataflow_attribute_name(), actual_value() }.



% A dimension is a collection of symbols specialising a logical attribute into
% a set of actual ones (one per value in the dimension).
%
% Ex: [ "2017", "2018", "2019" ].
%
-type dimension() :: [ text_utils:string_like() ].


-export_type([ object_name/0, dataflow_attribute_spec/0, attribute_update/0 ]).



% Helpers:
-export([ to_string/1, attribute_to_string/2, attributes_to_string/1,
		  create_attribute_specs_with_dimension/6,
		  create_attribute_specs_with_dimensions/6,
		  decode_initial_attribute_values/3, set_attributes/3 ]).



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Object" ).


% For port_timestamp(), value_status() and all:
-include("dataflow_defines.hrl").


% For WOOPER, actor types, etc.:
-include("sim_diasca_for_actors.hrl").


% Shorthands:
-type ustring() :: text_utils:ustring().



% Implementation notes:
%
% Each of the dataflow attributes leads to the creation of two associated ports:
% an input port to set it, an output port to read it.




% First, optional stage of the construction of a dataflow object, so that it can
% send traces and decode attributes early (before the real, second stage
% construct/8):
%
% (requires the caller to inherit twice (one directly, one indirectly) from
% TraceEmitter)
%
% (helper)
%
-spec pre_init( object_name(), wooper:state() ) -> wooper:state().
pre_init( TraceEmitterName, State ) ->

	% So that we can send traces even before even being constructed for good:
	TraceState = class_TraceEmitter:construct( State, TraceEmitterName ),

	% Default settings:
	setAttributes( TraceState, [
			{ accept_multiple_valued_attrs, false },
			{ accept_non_valued_attrs, true },
			{ accept_extra_attr_values, true } ] ).



% Constructs (second stage) a new dataflow object, whose role is to represent in
% the dataflow a part of the state of the target system:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as automatically assigned by the load balancer
%
% - DataflowObjectName is a human-readable name for that dataflow object (as a
% plain, non-empty string)
%
% - DataflowAttributeSpecs is an (ordered) list describing the dataflow
% attributes of this dataflow object
%
% - InitialAttributeValues is a tuple listing the initial, direct values for
% each of the previously defined dataflow attributes, in the same order; using a
% tuple rather than a list allows the proper types for constructors to be
% specified (i.e. {t1(), t2(), t3()}, not [t1() | t2() | t3()])
%
% - PossibleUniquePeersTypes contains a list of atoms for which one might later
% define a unique link with another object; typically one might pass
% [class_City] to define that this dataflow object may be contained into a City.
%
% - SpecForMultiplePeers contains a list of atoms for which one might later
% define multiple links with other objects; typically one might pass
% [class_Building] to indicate that this object can contain several buildings
%
% - DataflowPid is the PID of the dataflow that will own this object
%
-spec construct( wooper:state(), class_Actor:actor_settings(), object_name(),
		[ dataflow_attribute_spec() ], [ actual_value() ], unique_peer_spec(),
		multiple_peer_spec(), dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, DataflowObjectName, DataflowAttributeSpecs,
		   InitialAttributeValues, SpecForUniquePeers, SpecForMultiplePeers,
		   DataflowPid ) ->

	% When some unique peers are specified (for instance, the parent), we can
	% create attributes that publish their identifiers:
	%
	% (additionally checks the types of UniquePeerSpec)
	%
	{ AttrSpecsForUniquePeers, InitValuesForUniquePeers } =
		create_attribute_specs_for_unique_peers( SpecForUniquePeers ),

	% The base dataflow attributes are then enriched with the ones for the
	% unique peers:
	%
	AllAttrSpecs = DataflowAttributeSpecs ++ AttrSpecsForUniquePeers,

	% Same for their initial values (same order):
	AllInitialAttrValues = InitialAttributeValues ++ InitValuesForUniquePeers,

	% Transforms the attribute specifications into a list of input and output
	% port specifications:
	%
	{ InputPortSpecs, OutputPortSpecs } = get_port_specs_for( AllAttrSpecs ),

	% Constructs the object by calling our parent class:
	ElemState = class_DataflowBlock:construct( State, ActorSettings,
		?trace_categorize(DataflowObjectName),
		InputPortSpecs, OutputPortSpecs, DataflowPid ),

	% Determines the initial table of unique (ex: parent) and multiple (ex:
	% children) peers:

	UniquePeerTypeTable = get_unique_peer_table( SpecForUniquePeers ),

	MultiplePeerTypeTable = get_multiple_peer_table( SpecForMultiplePeers ),

	% If specified, unique peers can be published as dataflow ports (ex:
	% "parent_id"), as stored by this table:
	%
	UniquePeersToNameTable = get_peer_to_name_table( SpecForUniquePeers ),

	MaybeIdServer = class_IdentificationServer:get_any_server(),

	% Do not override any pre-established settings:
	% (not a good practice to conditionally defined attributes...)

	MultAttr = case hasAttribute( State, accept_multiple_valued_attrs ) of

		true ->
			?getAttr(accept_multiple_valued_attrs);

		false ->
			false

	end,

	NonAttr = case hasAttribute( State, accept_non_valued_attrs ) of

		true ->
			?getAttr(accept_non_valued_attrs);

		false ->
			true

	end,

	ExtraAttr = case hasAttribute( State, accept_extra_attr_values ) of

		true ->
			?getAttr(accept_extra_attr_values);

		false ->
			true

	end,

	SuspendedState = setAttributes( ElemState, [
			{ run_status, suspended },
			{ attribute_specs, AllAttrSpecs },
			{ unique_peer_table, UniquePeerTypeTable },
			{ multiple_peer_table, MultiplePeerTypeTable },
			{ peer_to_name_table, UniquePeersToNameTable },

			% Defaults:
			{ accept_multiple_valued_attrs, MultAttr },
			{ accept_non_valued_attrs, NonAttr },
			{ accept_extra_attr_values, ExtraAttr },

			{ identification_server_pid, MaybeIdServer } ] ),

	% Suspension will be declared to the dataflow at the first diasca of this
	% object (as it is an actor oneway), as a side-effect of its registration as
	% object.

	% We set the initial values for the "standard" dataflow attributes only, as
	% the unique peer associations would be undefined.

	% PeerAssignedState = set_attributes( AttrSpecsForUniquePeers,
	%   InitValuesForUniquePeers, SuspendedState ),

	AssignedState = set_attributes( DataflowAttributeSpecs,
									InitialAttributeValues, SuspendedState ),

	trace_initial_state( AllAttrSpecs, AllInitialAttrValues, AssignedState ),

	AssignedState.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Checks that no peer is still registered:

	UniquePeerPairs = table:enumerate( ?getAttr(unique_peer_table) ),

	% Filters out 'undefined' values:
	case [ UniquePair || UniquePair={ _PeerType, PeerPid } <- UniquePeerPairs,
						 is_pid( PeerPid ) ] of

		[] ->
			ok;

		StillUniques ->
			UniqueStrings = [ text_utils:format( "peer type '~ts' to ~w",
				   [ Type, Pid ] ) || { Type, Pid } <- StillUniques ],

			?warning_fmt(
			   "At destruction, still ~B unique peers associated: ~ts",
			   [ length( UniqueStrings ),
				 text_utils:strings_to_string( UniqueStrings ) ] )

	end,

	MultiplePeerPairs = table:enumerate( ?getAttr(multiple_peer_table) ),

	% Filters out 'undefined' values:
	case [ MultiplePair || MultiplePair={ _PeerType, PeerPidList }
							   <- MultiplePeerPairs, is_list( PeerPidList ) ] of

		[] ->
			ok;

		StillMultiples ->
			MultipleStrings = [ text_utils:format( "peer type '~ts' to ~w",
				[ Type, Pids ] ) || { Type, Pids } <- StillMultiples ],

			?warning_fmt( "At destruction, still ~B multiple peers "
				"associated: ~ts", [ length( MultipleStrings ),
					text_utils:strings_to_string( MultipleStrings ) ] )

	end,


	case table:enumerate( ?getAttr(peer_to_name_table) ) of

		[] ->
			ok;

		PeerNames ->
			NameStrings = [
				text_utils:format( "peer ~w associated to attribute '~ts'",
								   [ Peer, AttrName ] )
							|| { Peer, AttrName } <- PeerNames ],

			?warning_fmt( "At destruction, still ~B associations between "
				"(unique) peer types and dataflow attributes: ~ts",
				[ length( PeerNames ),
				  text_utils:strings_to_string( NameStrings ) ] )

	end,

	% Then allow chaining:
	State.





% Returns a table whose keys are the (unique) peer types that have, as values,
% an associated dataflow attribute name.
%
-spec get_peer_to_name_table( unique_peer_spec() ) -> peer_to_name_table().
get_peer_to_name_table( SpecForUniquePeers ) ->
	% Skips peer types with no associated attribute name:
	table:new( [ { PeerType, text_utils:string_to_binary( AttrName ) }
				 || { PeerType, AttrName } <- SpecForUniquePeers ] ).



% Returns a basic attribute specification, defaulting to the string type
% (assuming it is the type of the external identifiers), and corresponding to
% the specified peer.
%
-spec create_attribute_specs_for_peer( dataflow_attribute_name() ) ->
												dataflow_attribute_spec().
create_attribute_specs_for_peer( PeerName ) ->
	create_attribute_specs_for_peer( PeerName, _TypeName=string,
									 _Multiplicity=unique ).



% Returns a basic attribute specification corresponding to the specified peer.
-spec create_attribute_specs_for_peer( dataflow_attribute_name(),
   type_utils:type_name(), peer_multiplicity() ) -> dataflow_attribute_spec().
create_attribute_specs_for_peer( PeerName, _Typename=string,
								 _PeerMultiplicity=unique ) ->
	#dataflow_attribute_spec{
	   attribute_name=PeerName,
	   semantics=[],
	   unit="dimensionless",
	   type_description="string",
	   % Add 'non_empty', once available:
	   constraints=[] };

create_attribute_specs_for_peer( PeerName, _Typename=pid,
								 _PeerMultiplicity=unique ) ->
	#dataflow_attribute_spec{
	   attribute_name=PeerName,
	   semantics=[],
	   unit="dimensionless",
	   type_description="pid",
	   constraints=[] };

create_attribute_specs_for_peer( PeerName, _Typename=string,
								 _PeerMultiplicity=multiple ) ->
	#dataflow_attribute_spec{
	   attribute_name=PeerName,
	   semantics=[],
	   unit="dimensionless",
	   type_description="[string]",
	   constraints=[] };

create_attribute_specs_for_peer( PeerName, _Typename=pid,
								 _PeerMultiplicity=multiple ) ->
	#dataflow_attribute_spec{
	   attribute_name=PeerName,
	   semantics=[],
	   unit="dimensionless",
	   type_description="[pid]",
	   constraints=[] }.




% Creates the specifications for the dataflow attributes corresponding to the
% specified peer specification.
%
% In this example, a string attribute of declared type "string" will be created.
%
% Also returns a list of tuples that can conveniently be used to initialize
% these attributes to 'undefined'.
%
% Note: also enforces basic type verification.
%
-spec create_attribute_specs_for_unique_peers( unique_peer_spec() ) ->
						{ [ dataflow_attribute_spec() ], [ 'undefined' ] }.
create_attribute_specs_for_unique_peers( _SpecForUniquePeers=[] ) ->
	{ _AttrSpecs=[], _UndefinedList=[] };

create_attribute_specs_for_unique_peers(
  _SpecForUniquePeers=[ { PeerType, PortName } | T ] )
  when is_atom( PeerType ) andalso is_list( PortName ) ->

	{ RemainingAttributesSpecs, RemainingInitValues } =
		create_attribute_specs_for_unique_peers( T ),

	PeerAttrSpec = create_attribute_specs_for_peer( PortName ),

	AttributesSpecs = [ PeerAttrSpec | RemainingAttributesSpecs ],

	InitValues = [ undefined | RemainingInitValues ],

	{ AttributesSpecs, InitValues };


% Skip peer declarations with no associated port name:
create_attribute_specs_for_unique_peers(
  _SpecForUniquePeers=[ PeerType | T ] ) when is_atom( PeerType ) ->
	create_attribute_specs_for_unique_peers( T ).



% Returns, for the specified unique peer specification, the corresponding unique
% peer table, associating to each (unique) peer type the 'undefined' value (no
% initial peer link).
%
-spec get_unique_peer_table( unique_peer_spec() ) -> unique_peer_table().
get_unique_peer_table( SpecForUniquePeers ) ->
	PeerEntries = get_unique_peer_entries( SpecForUniquePeers, _Acc=[] ),
	table:new( PeerEntries ).


% (helper)
get_unique_peer_entries( _SpecForUniquePeers=[], Acc ) ->
	Acc;


get_unique_peer_entries( _SpecForUniquePeers=[ { PeerType, _AttrName } | T ],
						 Acc ) ->
	PeerEntry = { PeerType, undefined },
	get_unique_peer_entries( T, [ PeerEntry | Acc ] );


get_unique_peer_entries( _SpecForUniquePeers=[ PeerType | T ], Acc )
  when is_atom( PeerType ) ->
	PeerEntry = { PeerType, undefined },
	get_unique_peer_entries( T, [ PeerEntry | Acc ] ).



% Returns, for the specified multiple peer specification, the corresponding
% multiple peer table, each (multiple) peer type being associated to an
% (initially empty) list of peer links.
%
% Note: also enforces basic type verification.
%
-spec get_multiple_peer_table( multiple_peer_spec() ) -> multiple_peer_table().
get_multiple_peer_table( SpecForMultiplePeers ) ->
	PeerEntries = get_multiple_peer_table( SpecForMultiplePeers, _Acc=[] ),
	table:new( PeerEntries ).


% (helper)
get_multiple_peer_table( _SpecForMultiplePeers=[], Acc ) ->
	Acc;


get_multiple_peer_table( _SpecForMultiplePeers=[ { PeerType, AttrName } | T ],
						 Acc ) when is_atom( PeerType )
									andalso	is_list( AttrName ) ->
	PeerEntry = { PeerType, [] },
	get_multiple_peer_table( T, [ PeerEntry | Acc ] );


get_multiple_peer_table( _SpecForMultiplePeers=[ PeerType | T ], Acc )
  when is_atom( PeerType ) ->
	PeerEntry = { PeerType, [] },
	get_multiple_peer_table( T, [ PeerEntry | Acc ] ).



% Translates specified attribute specs into their corresponding input and output
% ports.
%
-spec get_port_specs_for( [ dataflow_attribute_spec() ] ) ->
							{ [ input_port_spec() ], [ output_port_spec() ] }.
get_port_specs_for( DataflowAttributeSpecs ) ->
	get_port_specs_for( DataflowAttributeSpecs, _InputAcc=[], _OutputAcc=[] ).


% (helper)
get_port_specs_for( _DataflowAttributeSpecs=[], InputAcc, OutputAcc ) ->

	% Preserve declaration order (even if they will end up in an associative
	% table and be listed alphabetically afterwards):
	%
	{ lists:reverse( InputAcc ), lists:reverse( OutputAcc ) };


get_port_specs_for( _DataflowAttributeSpecs=[ #dataflow_attribute_spec{
												attribute_name=AttrName,
												comment=Comment,
												semantics=Semantics,
												unit=Unit,
												type_description=Type,
												constraints=Constraints } | T ],
					InputAcc, OutputAcc ) ->

	NewInputSpec = #input_port_spec{ name=AttrName,
									 comment=Comment,
									 is_iteration=false,
									 value_semantics=Semantics,
									 value_unit=Unit,
									 value_type_description=Type,
									 value_constraints=Constraints },

	NewOutputSpec = #output_port_spec{ name=AttrName,
									   comment=Comment,
									   is_iteration=false,
									   produces_result=false,
									   value_semantics=Semantics,
									   value_unit=Unit,
									   value_type_description=Type,
									   value_constraints=Constraints },

	get_port_specs_for( T, [ NewInputSpec | InputAcc ],
						[ NewOutputSpec | OutputAcc ] ).



% Sets (assigns) the attributes (port pairs) corresponding to specified
% attribute specs to the specified (raw, not channel) values.
%
% Manages the case where this object is suspended (typically at creation or
% update time): then output ports set their timestamp to 'send_on_resume',
% rather than propagating directly their values.
%
% Ex: called in the context of the construction of this dataflow object.
%
-spec set_attributes( [ dataflow_attribute_spec() ], [ actual_value() ],
					  wooper:state() ) -> wooper:state().
set_attributes( DataflowAttributeSpecs, AttributeValues, State ) ->

	SpecLen = length( DataflowAttributeSpecs ),

	% These two lists shall correspond (early check for better user
	% notification):
	%
	case length( AttributeValues ) of

		SpecLen ->

			% Instead of rebuilding the metadata so that the initial values
			% become channel ones, we could have reused the metadata of either
			% the input or output ports.

			InputPortTable = ?getAttr(input_ports),
			OutputPortTable = ?getAttr(output_ports),

			set_attributes( DataflowAttributeSpecs, AttributeValues,
							InputPortTable, OutputPortTable, State );


		OtherLen ->
			?error_fmt( "~B attributes specifications were provided, whereas "
				"~B values to be set have been specified: ~p, ~p",
				[ SpecLen, OtherLen, DataflowAttributeSpecs,
				  AttributeValues ] ),
			throw( { invalid_attribute_assignment, SpecLen, OtherLen } )

	end.



% (helper)
%
% No more value to assign, store them in our state:
set_attributes( _DataflowAttributeSpecs=[], _AttributeValues=[],
				InputPortTable, OutputPortTable, State ) ->
	setAttributes( State, [ { input_ports, InputPortTable },
							{ output_ports, OutputPortTable } ] );

% There is an attribute to declare, but no value defined for it, so do not
% initialise specifically that attribute:
%
set_attributes( _DataflowAttributeSpecs=[ _AttributeSpec | TS ],
				_AttributeValues=[ _RawValue=undefined | TV ],
				InputPortTable, OutputPortTable, State ) ->
	set_attributes( TS, TV, InputPortTable, OutputPortTable, State );


% There is an attribute to declare with an initial value; let's define that
% attribute and assign the right value to use when the object is resumed:
%
set_attributes( _DataflowAttributeSpecs=[ AttrSpec=#dataflow_attribute_spec{
							   attribute_name=AttrName } | TS ],
				_AttributeValues=[ RawValue | TV ], InputPortTable,
				OutputPortTable, State ) ->

	BinAttrName = text_utils:ensure_binary( AttrName ),

	% Channel value acceptable by design, assuming that the raw value is
	% consistent with these attribute metadata:
	%
	ChannelValue = create_channel_value_for_attribute( RawValue, AttrSpec ),

	{ NewInputPortTable, NewOutputPortTable, AssignState } =
		assign_channel_value_to_attribute( ChannelValue, BinAttrName,
								InputPortTable, OutputPortTable, State ),

	set_attributes( TS, TV, NewInputPortTable, NewOutputPortTable,
					AssignState ).




% Assigns a raw value to the specified dataflow attribute, assuming this value
% is compliant with it.
%
% (helper)
%
-spec assign_raw_value_to_attribute( actual_value(),
			dataflow_attribute_bin_name(), wooper:state() ) -> wooper:state().
assign_raw_value_to_attribute( RawValue, BinAttrName, State ) ->

	InputPortTable = ?getAttr(input_ports),
	OutputPortTable = ?getAttr(output_ports),

	{ NewInputPortTable, NewOutputPortTable, NewState } =
		assign_raw_value_to_attribute( RawValue, BinAttrName, InputPortTable,
									   OutputPortTable, State ),

	setAttributes( NewState, [ { input_ports, NewInputPortTable },
							   { output_ports, NewOutputPortTable } ] ).



% Assigns a raw value to the specified dataflow attribute, assuming this value
% is compliant with it.
%
-spec assign_raw_value_to_attribute( actual_value(),
		dataflow_attribute_bin_name(), input_port_table(),
		output_port_table(), wooper:state() ) ->
					{ input_port_table(), output_port_table(), wooper:state() }.
assign_raw_value_to_attribute( RawValue, BinAttrName, InputPortTable,
							   OutputPortTable, State ) ->

	AttributeSpec = get_attribute_spec_from_name( BinAttrName, State ),

	ChannelValue = create_channel_value_for_attribute( RawValue,
													   AttributeSpec ),

	assign_channel_value_to_attribute( ChannelValue, BinAttrName,
									   InputPortTable, OutputPortTable, State ).




% Assigns a channel value to the specified dataflow attribute.
-spec assign_channel_value_to_attribute( channel_value(),
			dataflow_attribute_bin_name(), wooper:state() ) -> wooper:state().
assign_channel_value_to_attribute( ChannelValue, BinAttrName, State ) ->

	InputPortTable = ?getAttr(input_ports),
	OutputPortTable = ?getAttr(output_ports),

	{ NewInputPortTable, NewOutputPortTable, NewState } =
		assign_channel_value_to_attribute( ChannelValue, BinAttrName,
								 InputPortTable, OutputPortTable, State ),

	setAttributes( NewState, [ { input_ports, NewInputPortTable },
							   { output_ports, NewOutputPortTable } ] ).



% Lower-level function to assign a channel value to a dataflow attribute.
%
% Note: the port tables are not set yet in the returned state.
%
-spec assign_channel_value_to_attribute( channel_value(),
			dataflow_attribute_bin_name(), input_port_table(),
			output_port_table(), wooper:state() ) ->
				   { input_port_table(), output_port_table(), wooper:state() }.
assign_channel_value_to_attribute( ChannelValue, BinAttrName, InputPortTable,
								   OutputPortTable, State ) ->

	% First let's update the input port:
	InputPort = class_DataflowBlock:get_input_port( BinAttrName,
													InputPortTable, State ),

	NewInputPort = class_DataflowBlock:assign_input_value( ChannelValue,
							InputPort, BinAttrName, State ),

	NewInputPortTable = table:add_entry( BinAttrName, NewInputPort,
										 InputPortTable ),

	% Then the output one:
	OutputPort = class_DataflowBlock:get_output_port( BinAttrName,
													  OutputPortTable, State ),

	% The run status of this dataflow object (telling whether it is suspended or
	% not) is managed by this helper:
	%
	{ NewOutputPort, AssignState } = class_DataflowBlock:assign_output_value(
						ChannelValue, OutputPort, BinAttrName, State ),

	NewOutputPortTable = table:add_entry( BinAttrName, NewOutputPort,
										  OutputPortTable ),

	{ NewInputPortTable, NewOutputPortTable, AssignState }.



% Creates, from a raw value and an attribute specification, a dataflow value
% that can be assigned to any corresponding attribute.
%
-spec create_channel_value_for_attribute( actual_value(),
			dataflow_attribute_spec() ) -> static_return( channel_value() ).
create_channel_value_for_attribute( ActualValue, #dataflow_attribute_spec{
									  semantics=Semantics,
									  unit=Unit,
									  type_description=TypeDescription } ) ->
	ChannelValue = class_Dataflow:create_channel_value( ActualValue, Semantics,
														Unit, TypeDescription ),

	wooper:return_static( ChannelValue ).



% Unsets specified dataflow attribute.
%
% (exported helper)
%
-spec unset_attribute( dataflow_attribute_bin_name(), wooper:state() ) ->
							wooper:state().
unset_attribute( BinAttrName, State ) ->

	% Silent update (not triggering downstream blocks):
	assign_raw_value_to_attribute( _RawValue=undefined, BinAttrName, State ).



% Member methods section.



% Callback executed on the first diasca of existence of this dataflow object,
% overridden in order to register specifically as an object.
%
% Note: should this method be overridden in a child class, this version should
% be called from there as well (as must be called in all cases).
%
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
							actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?debug_fmt( "Created ~ts.", [ to_string( State ) ] ),

	DataflowPid = ?getAttr(dataflow_pid),

	% As this class may have been specialised:
	ActualClassname = wooper:get_classname( State ),

	SentState = class_Actor:send_actor_message( DataflowPid,
		{ registerDataflowObject, [ ActualClassname ] }, State ),

	actor:return_state( SentState ).



% Sets explicitly the specified input port to the specified fully-specified
% channel value.
%
% Note: calling this method bypasses the (channel-based) dataflow system; it is
% mostly useful in order to feed source units from outside of the dataflow
% (typically from the experiment entry point). Such an explicit setting will
% perform activations exactly like a standard setting.
%
% Counterpart, for dataflow objects, of the setInputValue/4 oneway for
% processing units.
%
-spec setAttributeValue( wooper:state(), dataflow_attribute_name(),
	channel_value(), sending_actor_pid() ) -> actor_oneway_return().
setAttributeValue( State, BinAttrName, ChannelValue, SendingActorPid )
  when is_binary( BinAttrName )
	   andalso is_record( ChannelValue, channel_value ) ->

	?debug_fmt( "Explicit setting of attribute '~ts' to the value '~ts' "
		"for this dataflow object, as requested by process ~w.",
		[ BinAttrName, class_DataflowBlock:value_to_string( ChannelValue ),
		  SendingActorPid ] ),

	% Values conveyed by channels obey by design some rules - here we have to
	% validate them from scratch as they are introduced with no control:
	%
	% (only lightweight checking done currently)

	SetState = assign_channel_value_to_attribute( ChannelValue, BinAttrName,
												  State ),

	actor:return_state( SetState );


setAttributeValue( State, AttributeName, ChannelValue, SendingActorPid )
  when is_list( AttributeName ) ->

	SetState = setAttributeValue( State,
		text_utils:string_to_binary( AttributeName ),
		ChannelValue, SendingActorPid ),

	actor:return_state( SetState );


setAttributeValue( _State, Unexpected, _ChannelValue, _SendingActorPid ) ->
	% Probably an atom here:
	throw( { invalid_type_for_attribute_name, Unexpected } ).



% Associates this dataflow object to the specified (unique or multiple) peer, of
% the specified peer type.
%
-spec registerPeerAs( wooper:state(), peer_type(), peer_pid(),
					  sending_actor_pid() ) -> actor_oneway_return().
registerPeerAs( State, PeerPid, PeerType, _SendingActorPid ) ->

	NewState = register_peer( PeerPid, PeerType, State ),

	actor:return_state( NewState ).



% Registers specified peer, of specified type.
%
% Note the parameter order, different from registerPeerAs/4.
%
% (exported helper)
%
-spec register_peer( peer_pid(), peer_type(), wooper:state() ) ->
							wooper:state().
register_peer( PeerPid, PeerType, State ) when is_pid( PeerPid ) ->

	%?info_fmt( "Request to register a peer of type '~ts': ~w.",
	%			[ PeerType, PeerPid ] ),

	% Searching first in the multiple peer table, as it is the most common type
	% of peers:
	%
	MultiplePeerTypeTable = ?getAttr(multiple_peer_table),

	case table:lookup_entry( PeerType, MultiplePeerTypeTable ) of


		{ value, PeerPidList } ->

			case lists:member( PeerPid, PeerPidList ) of

				true ->
					?debug_fmt( "The ~w '~ts' peer instance belongs to the "
						"already registered ones, hence nothing done.",
						[ PeerType, PeerPid ] ),

					State;

				false ->
					?debug_fmt( "Registering ~w among the '~ts' peers.",
						[ PeerPid, PeerType ] ),

					% Update by design:
					NewTable = table:add_entry( PeerType,
						[ PeerPid | PeerPidList ], MultiplePeerTypeTable ),

					setAttribute( State, multiple_peer_table, NewTable )

			end;


		key_not_found ->

			% This peer type is not managed as a multiple association; maybe
			% then as a unique one?

			UniquePeerTable = ?getAttr(unique_peer_table),

			case table:lookup_entry( PeerType, UniquePeerTable ) of

				% Unique peer type known, but not set yet:
				{ value, undefined } ->
					link_with_unique( PeerType, PeerPid, UniquePeerTable,
									  State );


				{ value, AlreadySetPeerPid } ->
					?error_fmt( "Cannot register peer ~w as a unique '~ts' "
						"peer, as peer ~w is already registered.",
						[ PeerPid, PeerType, AlreadySetPeerPid ] ),

					throw( { unique_peer_already_set, PeerType,
							 AlreadySetPeerPid, PeerPid } );


				key_not_found ->

					UniqueStr = text_utils:atoms_to_string(
									table:keys( UniquePeerTable ) ),

					MultiplePeerTable = ?getAttr(multiple_peer_table),

					MultipleStr = text_utils:atoms_to_string(
									table:keys( MultiplePeerTable ) ),

					?error_fmt( "Cannot register ~w as a '~ts' peer, as "
						"this peer type is neither declared as a unique or "
						"multiple one. Unique peer types are: ~ts~n"
						"Multiple peer types are: ~ts",
						[ PeerPid, PeerType, UniqueStr, MultipleStr ] ),

					throw( { undeclared_peer_type, PeerType } )

			end

	end.



% Links with specified unique peer, supposing none is already linked.
%
% (sanity checks assumed already done)
%
-spec link_with_unique( peer_type(), peer_pid(), unique_peer_table(),
						wooper:state() ) -> wooper:state().
link_with_unique( PeerType, PeerPid, UniquePeerTable, State ) ->

	?debug_fmt( "Linking with a unique '~ts' peer: ~w.",
				[ PeerType, PeerPid ] ),

	% Update by design:
	NewPeerTable = table:add_entry( PeerType, PeerPid, UniquePeerTable ),

	RegisteredState = setAttribute( State, unique_peer_table, NewPeerTable ),

	% If an attribute was declared for this peer, then set its value with the
	% external identifier of this peer:

	PeerToNameTable = ?getAttr(peer_to_name_table),

	case table:lookup_entry( PeerType, PeerToNameTable ) of

		{ value, BinAttrName } ->

			PeerId = get_external_id_for( PeerPid, State ),

			?debug_fmt( "Setting the dataflow attribute '~ts' to the external "
				"identifier of '~ts' (unique) peer ~w, ~p.",
				[ BinAttrName, PeerType, PeerPid, PeerId ] ),

			% Returns a new state:
			assign_raw_value_to_attribute( _RawValue=PeerId, BinAttrName,
										   RegisteredState );

		key_not_found ->
			% Here no attribute name was registered for that peer type, the peer
			% is already recorded, hence nothing more to do:
			%
			RegisteredState

	end.



% Disassociates this dataflow object from the specified peer, for specified
% type.
%
-spec unregisterPeer( wooper:state(), peer_type(), peer_pid(),
					  sending_actor_pid() ) -> actor_oneway_return().
unregisterPeer( State, PeerPid, PeerType, _SendingActorPid ) ->

	NewState = unregister_peer( PeerPid, PeerType, State ),

	actor:return_state( NewState ).



% Unregisters specified peer, of specified type.
%
% Note the parameter order, different from unregisterPeer/4.
%
% (exported helper)
%
-spec unregister_peer( peer_pid(), peer_type(), wooper:state() ) ->
							wooper:state().
unregister_peer( PeerPid, PeerType, State ) when is_pid( PeerPid ) ->

	%?info_fmt( "Request to unregister a peer of type '~ts': ~w.",
	%			[ PeerType, PeerPid ] ),

	% Searching first in the multiple peer table, as it is the most common type
	% of peers:
	%
	MultiplePeerTypeTable = ?getAttr(multiple_peer_table),

	case table:lookup_entry( PeerType, MultiplePeerTypeTable ) of

		{ value, PeerPidList } ->

			case list_utils:delete_if_existing( PeerPid, PeerPidList ) of

				not_found ->
					?error_fmt( "Attempt to unregister peer ~w from the "
						"(multiple) peer type '~ts' failed, as it does "
						"pertain to the peer list, which is: ~w.",
						[ PeerPid, PeerType, PeerPidList ] ),
					throw( { multiple_peer_not_registered, PeerPid,
							 PeerType } );

				ShrunkPidList ->
					?debug_fmt( "Unregistering peer ~w from the multiple "
								"peer type '~ts'.", [ PeerPid, PeerType ] ),

					% Update by design:
					NewTable = table:add_entry( PeerType, ShrunkPidList,
												MultiplePeerTypeTable ),

					setAttribute( State, multiple_peer_table, NewTable )

			end;


		key_not_found ->

			% This peer type is not managed as a multiple association; maybe
			% then as a unique one?

			UniquePeerTable = ?getAttr(unique_peer_table),

			case table:lookup_entry( PeerType, UniquePeerTable ) of

				% Peer matching:
				{ value, PeerPid } ->
					unlink_from_unique( PeerType, PeerPid, UniquePeerTable,
										State );


				% May be 'undefined':
				{ value, OtherMaybePeerPid } ->
					?error_fmt( "Cannot unregister peer ~w from unique peer "
						"peer type '~ts', as it is not registered "
						"(current peer: ~w).",
						[ PeerPid, PeerType, OtherMaybePeerPid ] ),

					throw( { unique_peer_not_set, PeerPid, PeerType,
							 OtherMaybePeerPid } );


				key_not_found ->

					UniqueStr = text_utils:atoms_to_string(
								  table:keys( UniquePeerTable ) ),

					MultiplePeerTable = ?getAttr(multiple_peer_table),

					MultipleStr = text_utils:atoms_to_string(
									table:keys( MultiplePeerTable ) ),

					?error_fmt( "Cannot unregister peer ~w from "
						"peer type '~ts', as this peer type is neither "
						"declared as a unique or multiple one. "
						"Unique peer types are: ~ts~n"
						"Multiple peer types are: ~ts",
						[ PeerPid, PeerType, UniqueStr, MultipleStr ] ),

					throw( { undeclared_peer_type, PeerType } )

			end

	end.



% Unlinks from specified unique peer.
%
% (sanity checks assumed already done)
%
-spec unlink_from_unique( peer_type(), peer_pid(), unique_peer_table(),
						  wooper:state() ) -> wooper:state().
unlink_from_unique( PeerType, PeerPid, UniquePeerTable, State ) ->

	?debug_fmt( "Unlinking from a unique '~ts' peer: ~w.",
				[ PeerType, PeerPid ] ),

	% Update by design:
	NewPeerTable = table:add_entry( PeerType, undefined, UniquePeerTable ),

	UnregisteredState = setAttribute( State, unique_peer_table, NewPeerTable ),

	% If an attribute was declared for this peer, then its value shall be unset:

	PeerToNameTable = ?getAttr(peer_to_name_table),

	case table:lookup_entry( PeerType, PeerToNameTable ) of

		{ value, BinAttrName } ->

			?debug_fmt( "Unsetting the dataflow attribute '~ts' corresponding "
				"to '~ts' (unique) peer ~w.",
				[ BinAttrName, PeerType, PeerPid ] ),

			% Returns a new state:
			unset_attribute( BinAttrName, UnregisteredState );


		key_not_found ->
			% Here no attribute name was registered for that peer type, hence
			% nothing more to do:
			%
			UnregisteredState

	end.




% Returns the external identifier associated to specified peer.
-spec get_external_id_for( peer_pid(), wooper:state() ) -> external_id().
get_external_id_for( PeerPid, State ) ->

	IdentificationServerPid = ?getAttr(identification_server_pid),

	IdentificationServerPid ! { getExternalIdentifier, [ PeerPid ], self() },

	receive

		% No guard defined, as there is no real constraint on the type of
		% external identifiers:
		%
		{ wooper_result, Id } ->
			Id

	end.



% Returns the PID of the unique peer associated to this peer type (if any),
% otherwise undefined.
%
% Will fail in case this type is not registered.
%
-spec getUniquePeerFor( wooper:state(), peer_type() ) ->
								const_request_return( maybe( peer_pid() ) ).
getUniquePeerFor( State, PeerType ) ->

	MaybePeerPid = get_unique_peer_for( PeerType, State ),

	wooper:const_return_result( MaybePeerPid ).



% Returns the PID of the unique peer associated to this peer type (if any),
% otherwise undefined.
%
% Will fail in case this type is not registered.
%
% (exported helper)
%
-spec get_unique_peer_for( peer_type(), wooper:state() ) -> maybe( peer_pid() ).
get_unique_peer_for( PeerType, State ) ->

	case table:lookup_entry( PeerType, ?getAttr(unique_peer_table) ) of

		{ value, MaybePeerPid } ->
			MaybePeerPid;

		key_not_found ->
			throw( { unknown_unique_peer_type, PeerType } )

	end.



% Sets the specified unique peer type to the specified value (PID or
% 'undefined'), regardless of the current value.
%
% (exported helper)
%
-spec set_unique_peer_for( peer_type(), maybe( peer_pid() ),
						   wooper:state() ) -> wooper:state().
set_unique_peer_for( PeerType, MaybePeerPid, State ) ->

	NewUniqueTable = table:add_entry( PeerType, MaybePeerPid,
									  ?getAttr(unique_peer_table) ),

	setAttribute( State, unique_peer_table, NewUniqueTable ).



% Returns a list of the PID of the multiple peers associated to this peer type.
%
% Will fail in case this type is not registered.
%
-spec getMultiplePeersFor( wooper:state(), peer_type() ) ->
									const_request_return( [ peer_pid() ] ).
getMultiplePeersFor( State, PeerType ) ->

	PeerPidList = get_multiple_peers_for( PeerType, State ),

	wooper:const_return_result( PeerPidList ).



% (exported helper)
-spec get_multiple_peers_for( peer_type(), wooper:state() ) -> [ peer_pid() ].
get_multiple_peers_for( PeerType, State ) ->

	case table:lookup_entry( PeerType, ?getAttr(multiple_peer_table) ) of

		{ value, PeerPidList } ->
			PeerPidList;

		key_not_found ->
			throw( { unknown_multiple_peer_type, PeerType } )

	end.



% Returns the status of the specified dataflow attribute.
-spec getAttributeStatus( wooper:state(), dataflow_attribute_bin_name() ) ->
								const_request_return( value_status() ).
getAttributeStatus( State, BinAttributeName )
  when is_binary( BinAttributeName ) ->

	% We rely on the output port associated to specified attribute for that
	% status:

	OutputPortTable = ?getAttr(output_ports),

	OutputPort = class_DataflowBlock:get_output_port( BinAttributeName,
													  OutputPortTable, State ),

	ValueStatus = OutputPort#output_port.value_status,

	?debug_fmt( "Reading, for dataflow attribute '~ts', value status ~p.",
				[ BinAttributeName, ValueStatus ] ),

	wooper:const_return_result( ValueStatus ).



% Updates the specified attributes of this dataflow object, implicity suspending
% it.
%
% Typically called from its object manager when processing an update world
% event.
%
-spec updateAttributes( wooper:state(), [ attribute_update() ], event_id(),
						sending_actor_pid() ) -> actor_oneway_return().
updateAttributes( State, AttributeUpdates, EventId, SendingActorPid ) ->

	?debug_fmt( "Updating following ~B attributes: ~ts",
		[ length( AttributeUpdates ),
		  text_utils:strings_to_string( [ attribute_update_to_string( N, V )
			  ||	{ N, V } <- AttributeUpdates ] ) ] ),

	SuspendedState = setAttribute( State, run_status, suspended ),

	% We have a list of {AttributeName, AttributeValue} pairs, and
	% set_attributes/3 expects to zip two lists, one of attribute specs, and the
	% other of attribute values.
	%
	% So we have first to convert each of our attribute names into its
	% corresponding attribute spec:

	{ AttrNames, AttrRawValues } = lists:unzip( AttributeUpdates ),

	AttrSpecs = [ get_attribute_spec_from_name( Name, SuspendedState )
				  || Name <- AttrNames ],

	% Maybe some validity checks shall be added:
	UpdatedState = set_attributes( AttrSpecs, AttrRawValues, SuspendedState ),

	SentState = class_Actor:send_actor_message( SendingActorPid,
					{ onAttributeUpdatePerformed, [ EventId ] }, UpdatedState ),

	actor:return_state( SentState ).



% Triggers the destruction of this dataflow object.
%
% Typically called from its object manager when processing a destruction world
% event.
%
-spec triggerDestruction( wooper:state(), event_id(), sending_actor_pid() ) ->
								actor_oneway_return().
triggerDestruction( State, EventId, SendingActorPid ) ->

	?debug_fmt( "Destruction triggered by ~w, in the context of event #~B",
				[ SendingActorPid, EventId ] ),

	% Regardless of upstream or downstream:
	ConnectedBlocks = set_utils:to_list(
			class_DataflowBlock:get_directly_connected_blocks( State ) ),

	ConnectState = class_Actor:send_actor_messages( ConnectedBlocks,
									_Oneway=disconnectFromBlock, State ),

	ActualClassname = wooper:get_classname( ConnectState ),

	UnregisterState = class_Actor:send_actor_message( ?getAttr(dataflow_pid),
		{ unregisterDataflowObject, [ ActualClassname ] }, ConnectState ),

	DestructState = class_Actor:send_actor_message( SendingActorPid,
					{ onDestructionTriggered, [ EventId ] }, UnregisterState ),

	DeclaredState = executeOneway( DestructState, declareTermination ),

	EmptyPortTable = table:new(),

	FinalState = setAttributes( DeclaredState, [
					{ input_ports, EmptyPortTable },
					{ output_ports, EmptyPortTable },
					{ run_status, terminating } ] ),

	actor:return_state( FinalState ).



% Notifies this dataflow object that, for specified input port, its upstream
% block just emitted a new (channel) value.
%
% Note: an immediate value (with no specific metadata) could have sufficed.
%
-spec notifyNewInput( wooper:state(), input_port_name(), channel_value(),
					  block_pid() ) -> actor_oneway_return().
notifyNewInput( State, InputPortName, ChannelValue, UpstreamBlockPid )
  when is_binary( InputPortName )
	   andalso is_record( ChannelValue, channel_value ) ->

	AttributeName = InputPortName,

	?debug_fmt( "Dataflow-based setting of attribute '~ts' "
		"(through the input port of the same name) to the value '~ts' "
		"for this dataflow object, as requested by upstream block ~w.",
		[ AttributeName, class_DataflowBlock:value_to_string( ChannelValue ),
		  UpstreamBlockPid ] ),

	AssignState = assign_channel_value_to_attribute( ChannelValue,
													 AttributeName, State ),

	actor:return_state( AssignState );


notifyNewInput( _State, InputPortName, _ChannelValue, _UpstreamBlockPid )
  when is_list( InputPortName ) ->
	throw( { non_binary_port_name, InputPortName } );


notifyNewInput( _State, InputPortName, _ChannelValue, _UpstreamBlockPid ) ->
	% Probably an atom here:
	throw( { invalid_type_for_port_name, InputPortName } ).



% Helper functions.


% Returns the attribute specification corresponding to the specified attribute
% name.
%
-spec get_attribute_spec_from_name( dataflow_attribute_name(),
								wooper:state() ) -> dataflow_attribute_spec().
get_attribute_spec_from_name( BinAttributeName, State )
  when is_binary( BinAttributeName ) ->

	StringAttrName = text_utils:binary_to_string( BinAttributeName ),

	get_attribute_spec_from_name( StringAttrName, State );


get_attribute_spec_from_name( AttributeName, State ) ->

	AttrSpecs = ?getAttr(attribute_specs),

	% We rely on the fact that 'attribute_name' is the first field of the
	% 'dataflow_attribute_spec' record, hence is the second element of the
	% corresponding tuple:
	%
	case lists:keyfind( _K=AttributeName, _N=2, AttrSpecs ) of

		false ->

			AttrStrings = [ attribute_spec_to_string( Attr )
							|| Attr <- AttrSpecs ],

			?error_fmt( "No attribute specification found for attribute "
				"'~ts'; known attribute specifications: ~ts",
				[ AttributeName,
				  text_utils:strings_to_string( AttrStrings ) ] ),

			throw( { unknown_attribute, AttributeName } );

		AttrSpec ->
			AttrSpec

	end.



% Returns a textual description of this dataflow attribute specification.
-spec attribute_spec_to_string( dataflow_attribute_spec() ) -> ustring().
attribute_spec_to_string( DataflowAttributeSpec ) ->
	attribute_spec_to_string( DataflowAttributeSpec, _IndentationLevel=0 ).



% Returns a textual description of this dataflow attribute specification, at
% specified indentation level.
%
-spec attribute_spec_to_string( dataflow_attribute_spec(),
								text_utils:indentation_level() ) -> ustring().
attribute_spec_to_string( #dataflow_attribute_spec{
							 attribute_name=Name,
							 comment=Comment,
							 semantics=Semantics,
							 unit=Unit,
							 type_description=TypeDescription,
							 constraints=Constraints },
						  IndentationLevel ) ->

	text_utils:format( "dataflow attribute named '~ts', ~ts, ~ts, ~ts, ~ts "
		"and ~ts",
		[ Name, dataflow_support:comment_to_string( Comment),
		  dataflow_support:user_semantics_to_string( Semantics ),
		  dataflow_support:string_unit_to_string( Unit ),
		  dataflow_support:type_description_to_string( TypeDescription ),
		  dataflow_support:value_constraint_to_string( Constraints,
													   IndentationLevel ) ] ).



% Returns a textual description of this dataflow object.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	{ InputDetailed, OutputDetailed } =
		class_DataflowBlock:io_to_string( State ),

	AttrSpecs = ?getAttr(attribute_specs),

	AttrStrings = [ attribute_spec_to_string( Attr ) || Attr <- AttrSpecs ],

	AttrString = text_utils:format(
		"~B attribute specifications were defined: ~ts",
		[ length( AttrSpecs ), text_utils:strings_to_string( AttrStrings ) ] ),

	UniqueString = unique_peer_table_to_string( ?getAttr(unique_peer_table) ),

	MultipleString = multiple_peer_table_to_string(
					   ?getAttr(multiple_peer_table) ),

	NameString = peer_to_name_table_to_string( ?getAttr(peer_to_name_table) ),

	text_utils:format( "Dataflow Object named '~ts', being ~ts, "
		"having ~ts and ~ts; ~ts; ~ts; ~ts and ~ts",
		[ ?getAttr(name), ?getAttr(run_status), InputDetailed, OutputDetailed,
		  AttrString, UniqueString, MultipleString, NameString ] ).



% Returns a textual representation of specified unique peer table.
-spec unique_peer_table_to_string( unique_peer_table() ) -> ustring().
unique_peer_table_to_string( Table ) ->

	case table:enumerate( Table ) of

		[] ->
			"no unique peer defined";

		Entries ->

			Strings = [ unique_peer_type_to_string( Peer, MaybePid )
							|| { Peer, MaybePid } <- Entries ],

			text_utils:format( "~B unique peers defined: ~ts",
				[ length( Entries ), text_utils:strings_to_string( Strings ) ] )

	end.



% Returns a textual representation of specified unique peer type.
-spec unique_peer_type_to_string( peer_type(), maybe( peer_pid() ) ) ->
										ustring().
unique_peer_type_to_string( PeerType, undefined ) ->
	text_utils:format( "peer type '~ts' not associated to any peer",
					   [ PeerType ] );

unique_peer_type_to_string( PeerType, PeerPid ) ->
	text_utils:format( "peer type '~ts' associated to peer ~w",
					   [ PeerType, PeerPid ] ).



% Returns a textual representation of specified multiple peer table.
-spec multiple_peer_table_to_string( multiple_peer_table() ) -> ustring().
multiple_peer_table_to_string( Table ) ->

	case table:enumerate( Table ) of

		[] ->
			"no multiple peer defined";

		Entries ->

			Strings = [ multiple_peer_type_to_string( Peer, PidList )
						|| { Peer, PidList } <- Entries ],

			text_utils:format( "~B multiple peers defined: ~ts",
				[ length( Entries ), text_utils:strings_to_string( Strings ) ] )

	end.



% Returns a textual representation of specified multiple peer type.
-spec multiple_peer_type_to_string( peer_type(), [ peer_pid() ] ) -> ustring().
multiple_peer_type_to_string( PeerType, _PidList=[] ) ->
	text_utils:format( "peer type '~ts' not associated to any peer",
					   [ PeerType ] );

multiple_peer_type_to_string( PeerType, PidList ) ->
	text_utils:format( "peer type '~ts' associated to peers ~w",
					   [ PeerType, PidList ] ).



% Returns a textual representation of specified peer type to name table.
-spec peer_to_name_table_to_string( peer_to_name_table() ) -> ustring().
peer_to_name_table_to_string( PeerToNameTable ) ->

	case table:enumerate( PeerToNameTable ) of

		[] ->
			"no (unique) peer defined an attribute";

		Entries ->

			Strings = [ peer_attribute_name_to_string( PeerType, AttrName )
						|| { PeerType, AttrName } <- Entries ],

			text_utils:format( "~B (unique) peers defined an attribute: ~ts",
				[ length( Entries ), text_utils:strings_to_string( Strings ) ] )

	end.



% Returns a textual representation of specified peer to name association.
-spec peer_attribute_name_to_string( peer_type(), peer_attribute_name() ) ->
											ustring().
peer_attribute_name_to_string( PeerType, AttributeName ) ->
	text_utils:format( "peer type '~ts' associated to dataflow attribute name "
					   "'~ts'", [ PeerType, AttributeName ] ).




% Returns a textual description of the specified dataflow attribute, based on
% its associated input port.
%
-spec attribute_to_string( input_port_name(), input_port()  ) -> ustring().
attribute_to_string( InputPortName, #input_port{ value_status={ set, V } } ) ->
	text_utils:format( "attribute '~ts' is set to value '~p'",
					   [ InputPortName, V ] );

attribute_to_string( InputPortName, #input_port{ value_status=unset } ) ->
	text_utils:format( "attribute '~ts' is unset", [ InputPortName ] ).



% Returns a textual description of the specified attribute update.
-spec attribute_update_to_string( dataflow_attribute_name(), actual_value() ) ->
										ustring().
attribute_update_to_string( AttributeName, AttributeValue ) ->
 text_utils:format( "attribute '~ts' being set to value '~p'",
					[ AttributeName, AttributeValue ] ).



% Returns a textual description of the dataflow attributes of this dataflow
% object.
%
-spec attributes_to_string( wooper:state() ) -> ustring().
attributes_to_string( State ) ->

	% We rely on input ports (we could have used the output ones instead):
	InputPortPairs = table:enumerate( ?getAttr(input_ports) ),

	AttrStrings = [ attribute_to_string( Name, InputPort )
					|| { Name, InputPort } <- InputPortPairs ],

	text_utils:format( "~B attributes: ~ts", [ length( AttrStrings ),
				text_utils:strings_to_string( AttrStrings ) ] ).



% Traces the specified initial state of this object.
-spec trace_initial_state( [ dataflow_attribute_spec() ], [ actual_value() ],
						   wooper:state() ) -> void().
trace_initial_state( DataflowAttributeSpecs, InitialAttributeValues, State ) ->

	AttrStrings = [ attribute_spec_to_string( Attr, _IndentationLevel=1 )
					|| Attr <- DataflowAttributeSpecs ],

	ValueStrings = [ text_utils:format( "'~p'", [ V ] )
					 || V <- InitialAttributeValues ],

	?notice_fmt( "Registering following dataflow attributes: ~ts~n"
		"with following respective initial values: ~ts",
		[ text_utils:strings_to_enumerated_string( AttrStrings ),
		  text_utils:strings_to_enumerated_string( ValueStrings ) ] ),

	?debug_fmt( "Full details at the creation of this dataflow object: ~ts",
				[ class_DataflowBlock:to_string( State ) ] ).



% Helps to create attributes with different modalities / dimensions.
%
% Based on the required properties to build an attribute, creates as many
% attribute specificiations [foo_A, foo_B, foo_C] for a dimension [A, B, C].
%
-spec create_attribute_specs_with_dimension( dataflow_attribute_name(),
		value_semantics(), unit_utils:unit_string(), value_type_description(),
		value_constraints(), dimension() ) -> [ dataflow_attribute_spec() ].
create_attribute_specs_with_dimension( AttributeName, Semantics, Unit,
								TypeDescription, Constraints, Dimension ) ->

	% Comment field ignored here.

	[ #dataflow_attribute_spec{
			attribute_name=
			 text_utils:format( "~ts[~ts]", [ AttributeName, D ] ),
			semantics=Semantics,
			unit=Unit,
			type_description=TypeDescription,
			constraints=Constraints } || D <- Dimension ].


% Provided all the required properties to build an attribute, creates as many
% attributes [foo_A_X, foo_A_Y, foo_B_X, foo_B_Y, ...] as there are dimensions:
% [[A,B], [X,Y]].
%
-spec create_attribute_specs_with_dimensions( dataflow_attribute_name(),
		value_semantics(), unit_utils:unit_string(), value_type_description(),
		[ value_constraints() ], [ dimension() ] ) ->
											[ dataflow_attribute_spec() ].
create_attribute_specs_with_dimensions( AttributeName, Semantics, Unit,
							TypeDescription, Constraints, Dimensions ) ->

	% Comment field ignored here.

	% First flattens all the dimensions as a list of non-nested lists, before
	% actually creating attributes for these dimensions:
	%
	create_attribute_specs_with_dimension( AttributeName, Semantics, Unit,
		TypeDescription, Constraints,
		  [ text_utils:join( _Separator=$,, DimComponents )
			|| DimComponents <- list_utils:cartesian_product( Dimensions ) ] ).



% Returns, from:
% - the attribute specifications of the current dataflow object
% - their initial values (provided at construction time)
%
% a list containing, in the right order, the relevant initial values for each
% attribute if this value was provided, or 'undefined' otherwise
%
% (helper)
%
-spec decode_initial_attribute_values( [ dataflow_attribute_spec() ],
			dataflow_object_initial_values(), wooper:state() ) -> [ any() ].
decode_initial_attribute_values( AttributeSpecs, NameValueAttrPairs, State ) ->

	% Determine the (ordered) list of the expected attribute names, and check
	% for duplications of attribute names from their spec:

	%?debug_fmt( "NameValueAttrPairs = ~p~n", [ NameValueAttrPairs ] ),

	ExpectedAttrNames = [
				RecordAttribute#dataflow_attribute_spec.attribute_name
				|| RecordAttribute <- AttributeSpecs ],

	ExpectedAttrCount = length( ExpectedAttrNames ),

	ExpectedAttrNamesSet = set_utils:from_list( ExpectedAttrNames ),

	check_expected_attributes( ExpectedAttrNamesSet, ExpectedAttrNames,
							   ExpectedAttrCount, State ),


	ProvidedAttrNames = [ Name || { Name, _RawValue } <- NameValueAttrPairs ],

	ProvidedAttrCount = length( ProvidedAttrNames ),

	?debug_fmt( "Expected ~B attribute names: ~ts and received ~B initial "
		"values, for attribute names: ~ts",
		[ ExpectedAttrCount, text_utils:strings_to_string( ExpectedAttrNames ),
		  ProvidedAttrCount,
		  text_utils:strings_to_string( ProvidedAttrNames ) ] ),


	% Check that the provided attributes are not duplicated either (that up to
	% one value is specified per attribute):

	ProvidedAttrNamesSet = set_utils:from_list( ProvidedAttrNames ),

	check_provided_attributes( ProvidedAttrNamesSet, ProvidedAttrNames,
							   ProvidedAttrCount, State ),


	% Then compares the expected attributes to the provided ones:

	MissingElements = set_utils:difference( ExpectedAttrNamesSet,
											ProvidedAttrNamesSet ),

	case set_utils:is_empty( MissingElements ) of

		true ->
			ok;

		false ->

			% Output ports may be allowed not to have initial values, for
			% example.

			MissingStrings = [ text_utils:format( "~ts", [ S ] )
							   || S <- set_utils:to_list( MissingElements ) ],

			MissingCount = length( MissingStrings ),

			MissingMessage = text_utils:strings_to_string( MissingStrings ),

			case ?getAttr(accept_non_valued_attrs) of

				true ->
					?notice_fmt( "Following ~B attributes have no initial "
						"value, hence will start as unset: ~ts",
						[ MissingCount, MissingMessage ] );

				false ->
					?error_fmt( "Following ~B attributes have no initial value "
						"(which is not accepted here): ~ts",
						[ MissingCount, MissingMessage ] ),

					throw( { no_initial_value_for_attributes, MissingCount,
							 MissingStrings } )

			end

	end,

	% Finally, ensure that no initial value is provided for a non-existing
	% attribute:

	check_no_extra_initial_values( ProvidedAttrNamesSet, ExpectedAttrNamesSet,
								   State ),

	% We have now acceptable initial values; we reorder them according to the
	% order of the specified attributes, knowing some attributes may not have
	% values to assign:

	InitialValues = [ list_table:get_value_with_defaults( _K=AttrName,
						_DefaultValue=undefined, _Table=NameValueAttrPairs )
					  || AttrName <- ExpectedAttrNames ],

	?debug_fmt( "Initial value setting: ~ts", [ text_utils:strings_to_string(
		 [ text_utils:format( "attribute '~ts' set to ~p", [ N, V ] )
		   || { N, V } <- lists:zip( ExpectedAttrNames, InitialValues ) ] ) ] ),

	InitialValues.



% Checks that the expected attributes are legit.
-spec check_expected_attributes( set_utils:set( dataflow_attribute_name() ),
		 [ dataflow_attribute_name() ], basic_utils:count(), wooper:state() ) ->
										void().
check_expected_attributes( ExpectedAttrNamesSet, ExpectedAttrNames,
						   ExpectedAttrCount, State ) ->

	case set_utils:size( ExpectedAttrNamesSet ) < ExpectedAttrCount of

		true ->

			% Not expected to happen due to the uniqueness of attribute names:

			DuplicatePairs = list_utils:get_duplicates( ExpectedAttrNames ),

			DuplicateStrings = [ text_utils:format(
				"attribute '~ts' listed ~B times", [ AttrName, Count ] )
								 || { AttrName, Count } <- DuplicatePairs ],

			?error_fmt( "At least one attribute name is expected more than "
				"once: ~ts",
				[ text_utils:strings_to_string( DuplicateStrings ) ] ),

			ExpectedDupNames = [ Name || { Name, _Count } <- DuplicatePairs ],

			throw( { expected_attribute_names_non_unique, ExpectedDupNames } );

		false ->
			ok

	end.


% Checks that the provided attributes are legit.
-spec check_provided_attributes( set_utils:set( dataflow_attribute_name() ),
		[ dataflow_attribute_name() ], basic_utils:count(), wooper:state() ) ->
										void().
check_provided_attributes( ProvidedAttrNamesSet, ProvidedAttrNames,
						   ProvidedAttrCount, State ) ->

	case set_utils:size( ProvidedAttrNamesSet ) < ProvidedAttrCount of

		true ->

			DuplicatePairs = list_utils:get_duplicates( ProvidedAttrNames ),

			DuplicateStrings = [ text_utils:format(
						"attribute '~ts' set ~B times", [ AttrName, Count ] )
								 || { AttrName, Count } <-  DuplicatePairs ],

			Message = text_utils:format( "At least one attribute received "
						"more than one initial value: ~ts",
						[ text_utils:strings_to_string( DuplicateStrings ) ] ),

			case ?getAttr(accept_multiple_valued_attrs) of

				true ->
					?warning( Message );

				false ->
					?error( Message ),

					DuplicateNames =
						[ Name || { Name, _Count } <- DuplicatePairs ],

					DuplicateCount = length( DuplicateNames ),

					throw( { provided_attribute_names_non_unique,
							 DuplicateCount, DuplicateNames } )
			end;

		false ->
			ok

	end.



-spec check_no_extra_initial_values( set_utils:set( dataflow_attribute_name() ),
		set_utils:set( dataflow_attribute_name() ), wooper:state() ) -> void().
check_no_extra_initial_values( ProvidedAttrNamesSet, ExpectedAttrNamesSet,
							   State ) ->

	ExtraAttrNames = set_utils:difference( ProvidedAttrNamesSet,
										   ExpectedAttrNamesSet ),

	case set_utils:is_empty( ExtraAttrNames ) of

		true ->
			ok;

		false ->

			AttrList = set_utils:to_list( ExtraAttrNames ),

			AttrNames = [ text_utils:format( "~ts", [ N ] ) || N <- AttrList ],

			case ?getAttr(accept_extra_attr_values) of

				true ->
					?warning_fmt( "~B initial value(s) have been provided for "
						"non-existing dataflow attributes, and thus will "
						"be ignored: ~ts", [ length( AttrNames ),
								text_utils:strings_to_string( AttrNames ) ] );

				false ->
					?error_fmt( "~B initial value(s) have been provided for "
						"non-existing dataflow attributes: ~ts",
						[ length( AttrNames ),
						  text_utils:strings_to_string( AttrNames ) ] ),

					throw( { multiple_initial_values_for_attributes,
							 set_utils:to_list( AttrList ) } )

			end

	end.
