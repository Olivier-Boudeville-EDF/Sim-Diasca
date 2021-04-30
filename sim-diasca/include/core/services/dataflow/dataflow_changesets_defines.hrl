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


% Centralisation of defines, type definitions, etc. to manage changesets between
% a dataflow and an external counterpart (ex: maintained by a more global
% platform) with whom a (possibly bidirectional) synchronisation shall be
% performed.

% Note: not expected to be directly included, user code should rely on
% sim_diasca*.hrl instead.



% The synchronisation discussed here attempts to ensure that the current
% dataflow (namely its objects and units) matches an external reference
% describing the state of the simulation world and the operations to be applied
% on it.


% An external identifier, typically used by an external platform to designate a
% simulation object (namely a dataflow block):
%
-type external_id() :: any().



% A reference to a block can be either an external identifier or an internal one
% (i.e. the PID of a dataflow block):
%
-type block_ref() :: external_id() | block_pid().



%% Section dedicated to the description of the synchronisation events that may
%% impact a dataflow.


% The actual synchronisation events that may apply to the state of the
% simulation world (forward declarations):
%
-type world_event() :: creation_event()       | destruction_event()
					 | association_event()    | binary_association_event()
					 | disassociation_event()
					 | connection_event()     | disconnection_event()
					 | update_event().



% A changeset is an (ordered) list of world events:
%
-type changeset() :: [ world_event() ].


% Allows to identify a world event (expected to be strictly positive):
-type event_id() :: basic_utils:count().



% Note: the first field of each type of *_event record shall be the event
% identifier, and the second one shall be the timestamp (as we scan the event
% tuples for their second and third elements irrespective of their actual event
% type).


% The event timestamp records the first time at which the world manager
% schedules that event (ex: induced events scheduled later than at their
% arrival).


% An event describing the creation of an object in the simulation, which shall
% happen in the dataflow as well.
%
-record( creation_event, {


		   % The identifier of this world event:
		   id = undefined :: maybe( event_id() ),


		   % A timestamp associated to the scheduling of this world event:
		   timestamp = undefined ::
			 maybe( class_TimeManager:logical_timestamp() ),


		   % The type (classname) of the dataflow object to which this event
		   % applies:
		   %
		   object_type :: dataflow_object_type(),


		   % The external identifier (if any) of the simulation object to be
		   % created in the dataflow:
		   %
		   external_id = undefined :: maybe( external_id() ),


		   % The PID (if any, i.e. if already created) of the dataflow object
		   % corresponding to this creation:
		   %
		   object_pid = undefined :: maybe( object_pid() ),


		   % The construction parameters that define this creation:
		   %
		   % (name omitted: this is the first construction parameter, and it
		   % corresponds to a (stringified version) of the external_id field)
		   %
		   construction_parameters :: wooper:construction_parameters(),


		   % The dataflow in which this creation is to happen:
		   %
		   dataflow_pid :: dataflow_pid(),


		   % The other events (if any) induced by this creation:
		   %
		   induced_events = [] :: [ world_event() ]

} ).


-type creation_event() :: #creation_event{}.



% An event describing the destruction of an object in the simulation, which
% shall happen in the dataflow as well.
%
-record( destruction_event, {


		   % The identifier of this world event:
		   id = undefined :: maybe( event_id() ),


		   % A timestamp associated to the scheduling of this world event:
		   timestamp = undefined ::
			 maybe( class_TimeManager:logical_timestamp() ),


		   % The type (classname) of the dataflow object to which this event
		   % applies:
		   %
		   object_type :: dataflow_object_type(),


		   % The external identifier (if any) of the simulation object to be
		   % created in the dataflow:
		   %
		   external_id = undefined :: maybe( external_id() ),


		   % The PID (if any, i.e. if not yet deleted) of the dataflow object
		   % corresponding to this destruction:
		   %
		   object_pid = undefined :: maybe( object_pid() ),


		   % The dataflow in which this destruction is to happen:
		   % (at least useful to filter matches)
		   %
		   dataflow_pid :: dataflow_pid(),


		   % The other events (if any) induced by this destruction:
		   %
		   induced_events = [] :: [ world_event() ]

} ).


-type destruction_event() :: #destruction_event{}.



% Describes the type of that association, which is generally user-defined (ex:
% 'located_in_district').
%
% The 'peer_type_association' atom is a reserved value that designates a special
% type of (binary) association, allowing to define unique or multiple peers that
% are automatically managed by a given type of dataflow object.
%
% The 'any_association_type' atom is a reserved value (type wildcard), no actual
% association type shall be named as such.
%
-type association_type() :: 'peer_type_association' | 'any_association_type'
						  | user_defined_association_type().


% A type of association introduced by the user:
%
-type user_defined_association_type() :: atom().


% Any extra information relevant to describe a (non-binary) association (ex: the
% other objects it may apply to).
%%
-type association_info() :: any().



% An event describing the addition of an association (any kind of relationship)
% of a (source) object involved in the simulation, which shall happen in the
% dataflow as well.
%
% Note: binary associations should rely on a binary_association_event.
%
% The point of view for the description of this association is the "source"
% object.
%
-record( association_event, {


		   % The identifier of this world event:
		   id = undefined :: maybe( event_id() ),


		   % A timestamp associated to the scheduling of this world event:
		   timestamp = undefined ::
			 maybe( class_TimeManager:logical_timestamp() ),


		   % The type of this association (ex: 'located_in_areas'):
		   association_type :: association_type(),


		   % The type (classname) of the (source) dataflow object to which this
		   % event applies:
		   %
		   object_type :: dataflow_object_type(),


		   % The external identifier (if any) of the (source) simulation object
		   % that is to be associated:
		   %
		   external_id = undefined :: maybe( external_id() ),


		   % The PID of the (source) dataflow object that is to be associated:
		   %
		   object_pid = undefined :: maybe( object_pid() ),


		   % Any information applying to this association (ex: to which other
		   % target dataflow objects it applies, etc.)
		   %
		   association_information :: association_info(),


		   % The dataflow in which this association is to happen:
		   %
		   dataflow_pid :: dataflow_pid(),


		   % The other events (if any) induced by this association event:
		   %
		   induced_events = [] :: [ world_event() ]

} ).


-type association_event() :: #association_event{}.



% An event describing the addition of a binary association between a source
% dataflow object and a target one.
%
-record( binary_association_event, {


		   % The identifier of this world event:
		   id = undefined :: maybe( event_id() ),


		   % A timestamp associated to the scheduling of this world event:
		   timestamp = undefined ::
			 maybe( class_TimeManager:logical_timestamp() ),


		   % The type of this association (ex: 'located_in_district'):
		   association_type :: association_type(),


		   % The type (classname) of the (source) dataflow object to which this
		   % event applies:
		   %
		   source_object_type :: dataflow_object_type(),


		   % The type (classname) of the (target) dataflow object to which this
		   % event applies:
		   %
		   target_object_type :: dataflow_object_type(),


		   % The external identifier (if any) of the (source) simulation object
		   % that is to be associated:
		   %
		   source_external_id = undefined :: maybe( external_id() ),


		   % The external identifier (if any) of the (target) simulation object
		   % that is to be associated:
		   %
		   target_external_id = undefined :: maybe( external_id() ),


		   % The PID of the (source) dataflow object that is to be associated:
		   %
		   source_object_pid = undefined :: maybe( object_pid() ),


		   % The PID of the (target) dataflow object that is to be associated:
		   %
		   target_object_pid = undefined :: maybe( object_pid() ),


		   % Any information applying to this association.
		   %
		   association_information = undefined ::
			 maybe( association_info() ),


		   % The dataflow in which this association is to happen:
		   %
		   dataflow_pid :: dataflow_pid(),


		   % The other events (if any) induced by this association event:
		   %
		   induced_events = [] :: [ world_event() ]

} ).


-type binary_association_event() :: #binary_association_event{}.



% Any information relevant to describe a disassociation, typically a tuple made
% of the name of the disassociation and any parameter(s) thereof (ex: the other
% objects to which the association that shall be removed used to apply).
%
% Ex: { located_in_district, DistrictExternalId } or { living_in_building,
% BuildingExternalId }.
%
-type disassociation_info() :: any().


% An event describing a change in the disassociation (any kind of relationship)
% of an object involved in the simulation, which shall happen in the dataflow as
% well.
%
-record( disassociation_event, {


		   % The identifier of this world event:
		   id = undefined :: maybe( event_id() ),


		   % A timestamp associated to the scheduling of this world event:
		   timestamp = undefined ::
			 maybe( class_TimeManager:logical_timestamp() ),


		   % The type (classname) of the dataflow object to which this event
		   % applies:
		   %
		   object_type :: dataflow_object_type(),


		   % The external identifier (if any) of the simulation object that is
		   % to be disassociated:
		   %
		   external_id = undefined :: maybe( external_id() ),


		   % The PID of the dataflow object that is to be disassociated:
		   %
		   object_pid = undefined :: maybe( object_pid() ),


		   % Any information applying to this disassociation (ex: the type/name
		   % of the former association, to which other dataflow objects it
		   % applied, etc.)
		   %
		   disassociation_information :: disassociation_info(),


		   % The dataflow in which this disassociation is to happen:
		   %
		   dataflow_pid :: dataflow_pid(),


		   % The other events (if any) induced by this disassociation event:
		   %
		   induced_events = [] :: [ world_event() ]

} ).


-type disassociation_event() :: #disassociation_event{}.



% An event describing the creation of a dataflow channel, i.e. the connection of
% an output port of a dataflow block to the input port of a dataflow block.
%
-record( connection_event, {


		   % The identifier of this world event:
		   id = undefined :: maybe( event_id() ),


		   % A timestamp associated to the scheduling of this world event:
		   timestamp = undefined ::
			 maybe( class_TimeManager:logical_timestamp() ),


		   % The type (classname) of the dataflow block which is at the source
		   % endpoint of the channel.
		   %
		   source_block_type :: block_type(),


		   % The type (classname) of the dataflow block which is at the target
		   % endpoint of the channel.
		   %
		   target_block_type :: block_type(),


		   % The external identifier (if any) of the dataflow block which is at
		   % the source endpoint of the channel.
		   %
		   source_external_id = undefined :: maybe( external_id() ),


		   % The external identifier (if any) of the dataflow block which is at
		   % the target endpoint of the channel.
		   %
		   target_external_id = undefined :: maybe( external_id() ),



		   % The PID of the dataflow block which is at the source endpoint of
		   % the channel.
		   %
		   source_block_pid = undefined :: maybe( object_pid() ),


		   % The PID of the dataflow block which is at the target endpoint of
		   % the channel.
		   %
		   target_block_pid = undefined :: maybe( object_pid() ),


		   % The name of the output port of the source dataflow block from which
		   % the channel will be created.
		   %
		   output_port_name :: output_port_string_name(),


		   % The name of the input port of the target dataflow block to which
		   % the channel will be created.
		   %
		   input_port_name :: input_port_string_name(),


		   % The dataflow in which this connection is to happen:
		   %
		   dataflow_pid :: dataflow_pid(),


		   % The other events (if any) induced by this connection:
		   %
		   induced_events = [] :: [ world_event() ]

} ).


-type connection_event() :: #connection_event{}.



% An event describing the destruction of a dataflow channel, i.e. the
% disconnection of an output port of a dataflow block from the input port of a
% dataflow block.
%
-record( disconnection_event, {


		   % The identifier of this world event:
		   id = undefined :: maybe( event_id() ),


		   % A timestamp associated to the scheduling of this world event:
		   timestamp = undefined ::
			 maybe( class_TimeManager:logical_timestamp() ),


		   % The type (classname) of the dataflow block which was at the source
		   % endpoint of the channel.
		   %
		   source_block_type :: block_type(),


		   % The type (classname) of the dataflow block which was at the target
		   % endpoint of the channel.
		   %
		   target_block_type :: block_type(),


		   % The external identifier (if any) of the dataflow block which was at
		   % the source endpoint of the channel.
		   %
		   source_external_id = undefined :: maybe( external_id() ),


		   % The external identifier (if any) of the dataflow block which was at
		   % the target endpoint of the channel.
		   %
		   target_external_id = undefined :: maybe( external_id() ),



		   % The PID of the dataflow block which was at the source endpoint of
		   % the channel.
		   %
		   source_block_pid = undefined :: maybe( object_pid() ),


		   % The PID of the dataflow block which was at the target endpoint of
		   % the channel.
		   %
		   target_block_pid = undefined :: maybe( object_pid() ),


		   % The name of the output port of the source dataflow block from which
		   % the channel will be deleted.
		   %
		   output_port_name :: output_port_string_name(),


		   % The name of the input port of the target dataflow block to which
		   % the channel will be deleted.
		   %
		   input_port_name :: input_port_string_name(),


		   % The dataflow in which this disconnection is to happen:
		   %
		   dataflow_pid :: dataflow_pid(),


		   % The other events (if any) induced by this disconnection:
		   %
		   induced_events = [] :: [ world_event() ]

} ).


-type disconnection_event() :: #disconnection_event{}.



% An event describing the update (in terms of dataflow attributes) of an object
% in the simulation, which shall happen in the dataflow as well.
%
-record( update_event, {


		   % The identifier of this world event:
		   id = undefined :: maybe( event_id() ),


		   % A timestamp associated to the scheduling of this world event:
		   timestamp = undefined ::
			 maybe( class_TimeManager:logical_timestamp() ),


		   % The type (classname) of the dataflow object to which this event
		   % applies:
		   %
		   object_type :: dataflow_object_type(),


		   % The external identifier (if any) of the simulation object to be
		   % created in the dataflow:
		   %
		   external_id = undefined :: maybe( external_id() ),


		   % The PID (if any, i.e. if already created) of the dataflow object
		   % corresponding to this update:
		   %
		   object_pid = undefined :: maybe( object_pid() ),


		   % A list of attribute update pairs, i.e. a list containing
		   % {AttributeName,NewAttributeValue} pairs.
		   %
		   updates = [] :: [ class_DataflowObject:attribute_update() ],


		   % The dataflow in which this update is to happen:
		   % (at least useful to filter matches)
		   %
		   dataflow_pid :: dataflow_pid(),


		   % The other events (if any) induced by this update:
		   %
		   induced_events = [] :: [ world_event() ]

} ).


-type update_event() :: #update_event{}.





%% Section dedicated to the descriptions of the event matches regarding
%% changesets that may apply to a dataflow.


% Allows to match events based on the type of block that they designate:
%
-type block_type_match() :: block_type() | 'any_block_type'.


% Allows to match events based on the type of object that they designate:
%
-type object_type_match() :: dataflow_object_type() | 'any_object_type'.


% Allows to match events based on the external identifier that they embed:
%
-type external_id_match() :: external_id() | 'any_external_id'.


% Allows to match events based on the block (identified by its PID) that they
% embed:
%
-type block_pid_match() :: block_pid() | 'any_block_pid'.


% Allows to match events based on the object (identified by its PID) that they
% embed:
%
-type object_pid_match() :: object_pid() | 'any_object_pid'.


% Allows to match events based on the dataflow in which they happen:
%
-type dataflow_pid_match() :: dataflow_pid() | 'any_dataflow_pid'.



% Allows to match (creation) events based on the construction parameters they
% embed:
%
-type construction_parameters_match() :: wooper:construction_parameters()
									   | 'any_construction_parameters'.


% Allows to match (association) events based on the association type they
% embed:
%
-type association_type_match() :: association_type() | 'any_association_type'.


% Allows to match (association) events based on the association information they
% embed:
%
-type association_info_match() :: association_info() | 'any_association_info'.


% Allows to match (disassociation) events based on the disassociation
% information they embed:
%
-type disassociation_info_match() ::
		disassociation_info() | 'any_disassociation_info'.



% Allows to match (connection/disconnection) events based on the name of the
% port they operate on:
%
-type port_name_match() :: port_string_name() | 'any_port_name'.


% Allows to match (update) events based on the (ordered) updates that they
% embed:
%
-type attribute_update_match() ::
		[ class_DataflowObject:attribute_update() ] | 'any_attribute_update'.



% Allows the unit managers to define to which synchronisation events they may be
% receptive.
%
-type event_match() :: creation_event_match()           |
					   destruction_event_match()        |
					   association_event_match()        |
					   binary_association_event_match() |
					   disassociation_event_match()     |
					   connection_event_match()         |
					   disconnection_event_match()      |
					   update_event_match()             |
					   'any_event_type'.


% Shorthand for list of event matches:
%
-type event_matches() :: [ event_match() ].


% Specifications of event matches.

% We do not believe events shall be matched against their ID (too brittle).
%
% By design changesets transmitted to the experiment manager have been
% flattened, hence there are no induced events that can be possibly matched.



% The description of a possible match in terms of creation events.
%
-record( creation_event_match, {


		   % If the type of the created object matters for the match:
		   object_type_match = any_object_type :: object_type_match(),


		   % If the external identifier of the created object matters for the
		   % match:
		   %
		   external_id_match = any_external_id :: external_id_match(),


		   % If the (Erlang) PID of the created object matters for the match:
		   object_pid_match = any_object_pid :: object_pid_match(),


		   % If the construction parameters matter for the match:
		   construction_parameters_match =
			   any_construction_parameters :: construction_parameters_match(),


		   % If the (Erlang) PID of the associated dataflow matters for the
		   % match:
		   %
		   dataflow_pid_match = any_dataflow_pid :: dataflow_pid_match()

} ).

-type creation_event_match() :: #creation_event_match{}.



% The description of a possible match in terms of destruction events.
%
-record( destruction_event_match, {


		   % If the type of the deleted object matters for the match:
		   object_type_match = any_object_type :: object_type_match(),


		   % If the external identifier of the deleted object matters for the
		   % match:
		   %
		   external_id_match = any_external_id :: external_id_match(),


		   % If the (Erlang) PID of the deleted object matters for the match:
		   object_pid_match = any_object_pid :: object_pid_match(),


		   % If the (Erlang) PID of the associated dataflow matters for the
		   % match:
		   %
		   dataflow_pid_match = any_dataflow_pid :: dataflow_pid_match()

} ).

-type destruction_event_match() :: #destruction_event_match{}.



% The description of a possible match in terms of association events.
%
-record( association_event_match, {


		   % If the type of the associated object matters for the match:
		   object_type_match = any_object_type :: object_type_match(),


		   % If the external identifier of the associated object matters for the
		   % match:
		   %
		   external_id_match = any_external_id :: external_id_match(),


		   % If the (Erlang) PID of the associated object matters for the match:
		   object_pid_match = any_object_pid :: object_pid_match(),


		   % If the information applying to this association matters for the
		   % match:
		   %
		   association_info_match =
			   any_association_info :: association_info_match(),


		   % If the (Erlang) PID of the associated dataflow matters for the
		   % match:
		   %
		   dataflow_pid_match = any_dataflow_pid :: dataflow_pid_match()

} ).

-type association_event_match() :: #association_event_match{}.



% The description of a possible match in terms of binary association events.
%
-record( binary_association_event_match, {


		   % If the type of the association matters for the match:
		   association_type_match = any_association_type
			  :: association_type_match(),



		   % If the type of the source associated object matters for the match:
		   source_object_type_match = any_object_type :: object_type_match(),


		   % If the type of the target associated object matters for the match:
		   target_object_type_match = any_object_type :: object_type_match(),



		   % If the external identifier of the source associated object matters
		   % for the match:
		   %
		   source_external_id_match = any_external_id :: external_id_match(),


		   % If the external identifier of the target associated object matters
		   % for the match:
		   %
		   target_external_id_match = any_external_id :: external_id_match(),



		   % If the (Erlang) PID of the source associated object matters for the
		   % match:
		   %
		   source_object_pid_match = any_object_pid :: object_pid_match(),


		   % If the (Erlang) PID of the target associated object matters for the
		   % match:
		   %
		   target_object_pid_match = any_object_pid :: object_pid_match(),



		   % If the information applying to this association matters for the
		   % match:
		   %
		   association_info_match =
			   any_association_info :: association_info_match(),


		   % If the (Erlang) PID of the associated dataflow matters for the
		   % match:
		   %
		   dataflow_pid_match = any_dataflow_pid :: dataflow_pid_match()

} ).

-type binary_association_event_match() :: #binary_association_event_match{}.



% The description of a possible match in terms of disassociation events.
%
-record( disassociation_event_match, {


		   % If the type of the associated object matters for the match:
		   object_type_match = any_object_type :: object_type_match(),


		   % If the external identifier of the associated object matters for the
		   % match:
		   %
		   external_id_match = any_external_id :: external_id_match(),


		   % If the (Erlang) PID of the associated object matters for the match:
		   object_pid_match = any_object_pid :: object_pid_match(),


		   % If the information applying to this disassociation matters for the
		   % match:
		   %
		   disassociation_info_match =
			   any_disassociation_info :: disassociation_info_match(),


		   % If the (Erlang) PID of the associated dataflow matters for the
		   % match:
		   %
		   dataflow_pid_match = any_dataflow_pid :: dataflow_pid_match()

} ).

-type disassociation_event_match() :: #disassociation_event_match{}.



% The description of a possible match in terms of connection events.
%
-record( connection_event_match, {


		   % If the type of the source block matters for the connection:
		   source_block_type_match = any_block_type :: block_type_match(),


		   % If the type of the target block matters for the connection:
		   target_block_type_match = any_block_type :: block_type_match(),



		   % If the external identifier of the source block matters for the
		   % match:
		   %
		   source_external_id_match = any_external_id :: external_id_match(),


		   % If the external identifier of the target block matters for the
		   % match:
		   %
		   target_external_id_match = any_external_id :: external_id_match(),



		   % If the (Erlang) PID of the source block matters for the match:
		   source_block_pid_match = any_block_pid :: block_pid_match(),


		   % If the (Erlang) PID of the target block matters for the match:
		   target_block_pid_match = any_block_pid :: block_pid_match(),



		   % If the name of the output port of the source block matters for the
		   % match:
		   %
		   output_port_name_match = any_port_name :: port_name_match(),


		   % If the name of the input port of the target block matters for the
		   % match:
		   %
		   input_port_name_match = any_port_name :: port_name_match(),



		   % If the (Erlang) PID of the associated dataflow matters for the
		   % match:
		   %
		   dataflow_pid_match = any_dataflow_pid :: dataflow_pid_match()

} ).

-type connection_event_match() :: #connection_event_match{}.



% The description of a possible match in terms of disconnection events.
%
-record( disconnection_event_match, {


		   % If the type of the source block matters for the disconnection:
		   source_block_type_match = any_block_type :: block_type_match(),


		   % If the type of the target block matters for the disconnection:
		   target_block_type_match = any_block_type :: block_type_match(),



		   % If the external identifier of the source block matters for the
		   % match:
		   %
		   source_external_id_match = any_external_id :: external_id_match(),


		   % If the external identifier of the target block matters for the
		   % match:
		   %
		   target_external_id_match = any_external_id :: external_id_match(),



		   % If the (Erlang) PID of the source block matters for the match:
		   source_block_pid_match = any_block_pid :: block_pid_match(),


		   % If the (Erlang) PID of the target block matters for the match:
		   target_block_pid_match = any_block_pid :: block_pid_match(),



		   % If the name of the output port of the source block matters for the
		   % match:
		   %
		   output_port_name_match = any_port_name :: port_name_match(),


		   % If the name of the input port of the target block matters for the
		   % match:
		   %
		   input_port_name_match = any_port_name :: port_name_match(),


		   % If the (Erlang) PID of the associated dataflow matters for the
		   % match:
		   %
		   dataflow_pid_match = any_dataflow_pid :: dataflow_pid_match()

} ).

-type disconnection_event_match() :: #disconnection_event_match{}.



% The description of a possible match in terms of update events.
%
-record( update_event_match, {


		   % If the type of the updated object matters for the match:
		   object_type_match = any_object_type :: object_type_match(),


		   % If the external identifier of the updated object matters for the
		   % match:
		   %
		   external_id_match = any_external_id :: external_id_match(),


		   % If the (Erlang) PID of the updated object matters for the match:
		   object_pid_match = any_object_pid :: object_pid_match(),


		   % If the actual attribute updates matter for the match:
		   attribute_update_match =
			   any_attribute_update :: attribute_update_match(),


		   % If the (Erlang) PID of the associated dataflow matters for the
		   % match:
		   %
		   dataflow_pid_match :: dataflow_pid_match()

} ).

-type update_event_match() :: #update_event_match{}.
