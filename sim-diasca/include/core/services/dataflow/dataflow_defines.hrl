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


% Centralisation of defines, type definitions, etc. regarding dataflows in
% general.

% Note: not expected to be directly included, user code should rely on
% sim_diasca.hrl instead.


% To avoid too many agent-level includes:
% (commented-out as already included in sim_diasca_for_actors.hrl)
% For agent_pid():
%-include("engine_common_defines.hrl").


-define( dataflow_version, "0.0.6" ).



% Section about the overall state of the dataflow infrastructure.


% Publicly, directly available state of the dataflow subsystem (mainly the PIDs
% of the main various services involved):
%
-record( dataflow_state, {

	world_manager_pid :: world_manager_pid(),

	experiment_manager_pid :: experiment_manager_pid(),

	identification_manager_pid :: identification_manager_pid(),

	semantic_server_pid :: semantic_server_pid(),

	type_server_pid :: type_server_pid()} ).

-type dataflow_state() :: #dataflow_state{}.



% Port-related section.


% Minimum lexicographic (Levenshtein) distance between semantics:
-define( min_lexicographic_distance, 1 ).


% List of common types defined for the Dataflow support, to share them or avoid
% having to prefix them with a module name.
%
% A few component names are also defined.


% Name of a port: an arbitrary non-empty binary string, unique among all ports
% and iterations of the same direction - input or output - of a dataflow
% element, and containing the ?iterated_port_token substring iff it is an
% iterated port.
%
-type port_name() :: text_utils:bin_string().


% String-based port name, typically for user input (converted then internally to
% a binary string); an arbitrary non-empty string, unique among all ports and
% iterations of the same direction - input or output - of a dataflow element,
% and containing the ?iterated_port_token substring iff it is an iterated port.
%
-type port_string_name() :: text_utils:ustring().



% Name of a port iteration (arbitrary non-empty string, unique among all ports
% and iterations of the same direction - input or output - of a dataflow
% element, and not containing the ?iterated_port_token substring.
%
-type iteration_name() :: text_utils:bin_string().


% User-level counterpart:
-type string_iteration_name() :: text_utils:ustring().


% To count port instances:
-type port_count() :: basic_utils:count().

-type min_port_count() :: port_count().
-type max_port_count() :: port_count() | 'unbounded'.


% Both endpoints of a channel, in the context of two blocks:
-type port_pair() :: { output_port_name(), input_port_name() }.



% The canonical form describing the initial, minimum and maximum number of
% iterated ports.
%
-type iteration_multiplicity() ::
		{ port_count(), { min_port_count(), max_port_count() } }.



% Describes whether a given port is an iterated one.
%
% Note: once validated, this specification is put in canonical form, i.e. set to
% its third, expanded form (with the three port counts)
%
-type iteration_spec() ::

		% Values not possible here (as expected to be already translated
		% beforehand): 'true' or 'false'

		% Iterated with specified initial instance count:
		%
		% (min:0, max: unbounded)
		%
		port_count()

		% Iterated with specified initial and maximum instance count (min:0):
	  | { port_count(), max_port_count() }

		% Iterated with specified initial, minimum and maximum instance count:
		% (canonical form)
		%
	  | iteration_multiplicity().



% Name of an input port:
-type input_port_name() :: port_name().


% User-level counterpart:
-type input_port_string_name() :: port_string_name().


% Name of an output port:
-type output_port_name() :: port_name().


% User-level counterpart:
-type output_port_string_name() :: port_string_name().


% Type of a (standard) port (direction), either input or output.
-type port_type() :: 'input_port' | 'output_port'.



% Type of a port specification.
-type port_spec_type() ::
		'input_port_iteration' | 'output_port_iteration' | port_type().


% Any type of comment:
-type comment() :: string().


% Allows to comment on the role and purpose of a given port (to ease the
% understanding, for human consumption only yet possibly reused by user
% interfaces):
%
-type port_comment() :: comment().


% A comment, once stored for good in any kind of actor:
-type internal_comment() :: text_utils:bin_string().



% A context-free identifier of a port:
-type full_port_id() :: { block_pid(), port_type(), port_name() }.


% Port source, to designate the source endpoint of a channel, i.e. the output
% port of the upstream block:
%
-type port_source() :: { block_pid(), output_port_name() }.


% Port target, to designate the target endpoint of a channel, i.e. the input
% port of the downstream block:
%
-type port_target() :: { block_pid(), input_port_name() }.



% Name of an input (port) iteration:
-type input_iteration_name() :: iteration_name().


% User-level counterpart:
-type input_iteration_string_name() :: string_iteration_name().


% Name of an output (port) iteration:
-type output_iteration_name() :: iteration_name().


% User-level counterpart:
-type output_iteration_string_name() :: string_iteration_name().



% Iteration port source, to designate a port iteration that is the source
% endpoint for a set of channels: defines the iterated output ports meant to
% target upstream blocks.
%
-type iteration_port_source() :: { block_pid(), output_iteration_name() }.


% User-level counterpart:
-type iteration_port_string_source() :: { block_pid(),
										  output_iteration_string_name() }.


% Iteration port target, to designate a port iteration that is the target
% endpoint of a set of channels: defines the iterated input ports meant to
% target downstream blocks.
%
% (used internally)
%
-type iteration_port_target() :: { block_pid(), input_iteration_name() }.


% User-level counterpart:
-type iteration_port_string_target() ::
		{ block_pid(), input_iteration_string_name() }.



% Semantics of a corresponding value, i.e. its associated meaning (the 'S' in
% SUTC), as specified by the user (as a list of RDF subjects, i.e. IRIs; a
% possibly empty list, possibly with duplicates).
%
-type user_value_semantics() :: class_SemanticServer:user_vocabulary().


% User-level vocabulary (same as user_value_semantics/0, yet having a slightly
% different meaning, referring for example to statically declared vocabularies,
% with no special link with values).
%
-type user_vocabulary() :: class_SemanticServer:user_vocabulary().


% Unitary element (concept) comprised in a semantics, as expressed by the user;
% it shall be a RDF subject, i.e. an IRI ('International Resource Identifier').
%
% Ex: "http://dbpedia.org/resource/Leonardo_da_Vinci",
% "http://xmlns.com/foaf/0.1/knows", to be preferred to "energy_demand",
% "unitary cost", "Number of tracks elapsed", etc.
%
% (note that the actual type of such an element is plain string)
%
-type user_value_semantical_element() :: class_SemanticServer:user_semantics().




% Semantics of a corresponding value, i.e. its associated meaning (the 'S' in
% SUTC), when used internally by the engine.
%
% It shall be a set of RDF subjects, i.e. a set of IRIs.
%
-type value_semantics() :: class_SemanticServer:vocabulary().


% Unitary element (concept) comprised in a semantics, when used internally by
% the engine; it is expressed as a RDF subject, i.e. an IRI ('International
% Resource Identifier').
%
% (note that the actual type of such an element is binary string)
%
-type value_semantical_element() :: class_SemanticServer:semantics().




% Unit of a corresponding value (the 'U' in SUTC), as a pair made of:
%
% - the user-specified unit binary string, for user-interaction purposes
% (typically display), in our standard form; ex: "km/h" or "mW.K^2.m^-3" (in the
% general case differs from the canonical, internal representation)
%
% - its actual counterpart, as a canonical unit, for computation purposes
%
% Please refer to the section about units in the technical manual of the
% 'Myriad' layer, and to unit_utils.erl for more details.
%
-type value_unit() :: { unit_utils:unit_bin_string(),
						unit_utils:canonical_unit() }.


% Type of a corresponding value (the 'T' in SUTC).



% The (textual) description of the type of a given value ("type-as-a-string").
%
% Ex: "[{float(),boolean()}]" for a list containing pairs made of a float and a
% boolean.
%
-type value_type_description() :: type_utils:type_description().


% If a given type may be initially specified as a type-as-a-string, i.e. as a
% value_type_description() such as for example "[{float(),boolean()}]", it
% has to be translated here to a type-as-a-term, like in this corresponding
% form: { list, [ { tuple, [ {float,[]}, {boolean,[]} ] } ] }.
%
% Doing so allows to check a given value against its expected type, for
% verification purposes.
%
-type value_type() :: type_utils:type().


% A constraint that may apply to a value.
%
-type value_constraint() :: { 'greater_than', number() }
						  | { 'lower_than', number() }
						  | { 'between', number(), number() }
						  | { 'in', list() }
						  | 'positive'
						  | 'strictly_positive'
						  | 'negative'
						  | 'strictly_negative'
						  | 'non_null'.



% Constraints applying to a corresponding value (the 'C' in SUTC).
%
-type value_constraints() :: [ value_constraint() ].


% Describes the SUTC metadata held by a port.
%
% See also: the port_description/0 type.
%
-type port_metadata() :: { value_semantics(), value_unit(),
						   value_type_description(), value_constraints() }.


% The actual value possibly held by a port (expected to comply with its declared
% type):
%
-type actual_value() :: any().


% Current value-related status of a port ("readiness"):
%
% (as a pair, so that actual values like the 'unset' atom can be held as well;
% even though this would be quite confusing)
%
-type value_status() :: 'unset' | { 'set', actual_value() }.


% An (absolute) identifier of an input port (no port type useful in this
% context):
%
-type input_port_id() :: { downstream_block_pid(), input_port_name() }.


% An (absolute) identifier of an output port (no port type useful in this
% context).
%
-type output_port_id() :: { upstream_block_pid(), output_port_name() }.


% Identifier of any kind of port (input or output).
%
-type port_id() :: input_port_id() | output_port_id().



% Timestamp for port-related events.
%
% Can be:
%
% - a pair made of a tick offset and a diasca
%
% - the 'none' atom if no event occurred yet for this port
%
% - the 'send_on_resume' atom in the context of an output port that was set
% whereas its associated block was suspended (thus the set value is to be sent
% to the upstream blocks as soon as the block resumes)
%
-type port_timestamp() :: class_TimeManager:logical_timestamp()
						| 'none' | 'send_on_resume'.



% Concentrates all information (mandatory or not) that may be provided by the
% user when defining an input port:
%
-record( input_port_spec, {


		   % Name of that input port (mandatory):
		   name :: port_string_name(),


		   % Comment (if any) associated to this input port:
		   comment = undefined :: maybe( port_comment() ),


		   % Tells whether this is a port iteration.
		   %
		   % If false, this is not a port iteration; if true, it is an unbounded
		   % port iteration with no initial port; otherwise the iteration is
		   % explicitly specified.
		   %
		   is_iteration = 'false' :: boolean() | iteration_spec(),


		   % SUTC information:


		   % Semantics of the information carried by this port (as a list of
		   % IRIs expressed as plain strings)
		   %
		   value_semantics :: user_value_semantics(),


		   % Unit of the values that this port may receive (mandatory):
		   %
		   value_unit :: unit_utils:unit_string(),


		   % Textual description of the type of the values that this port may
		   % receive (mandatory):
		   %
		   value_type_description :: value_type_description(),


		   % Constraints that apply to the values that this port may receive
		   % (may not be defined):
		   %
		   value_constraints = [] :: value_constraints()

}).

-type input_port_spec() :: #input_port_spec{}.




% Concentrates all information (mandatory or not) that may be provided by the
% user when defining an output port:
%
-record( output_port_spec, {


		   % Name of that output port (mandatory):
		   name :: port_string_name(),


		   % Comment (if any) associated to this output port:
		   comment = undefined :: maybe( port_comment() ),


		   % Tells whether this is a port iteration.
		   %
		   % If false, this is not a port iteration; if true, it is an unbounded
		   % port iteration with no initial port; otherwise the iteration is
		   % explicitly specified.
		   %
		   is_iteration = 'false' :: boolean() | iteration_spec(),


		   % Tells whether this output port shall be seen as a result producer,
		   % i.e. if its values over time are results of interest for the
		   % simulation.
		   %
		   produces_result = false :: boolean(),


		   % SUTC information:


		   % Semantics of the information carried by this port (as a list of
		   % IRIs expressed as plain strings)
		   %
		   value_semantics :: user_value_semantics(),


		   % Unit of the values that this port may send (mandatory):
		   %
		   value_unit :: unit_utils:unit_string(),


		   % Textual description of the type of the values that this port may
		   % send (mandatory):
		   %
		   value_type_description :: value_type_description(),


		   % Constraints that apply to the values that this port may send (may
		   % not be defined):
		   %
		   value_constraints = [] :: value_constraints()

}).

-type output_port_spec() :: #output_port_spec{}.



% Value exchanged over a dataflow channel, typically emitted by an output port
% and received by an input port.
%
-record( channel_value, {


		% The actual value carried by this element of information:
		%
		actual_value :: actual_value(),


		% Semantics of that value, a vocabulary (i.e. a set of binaries):
		%
		semantics :: value_semantics(),


		% Unit of that value:
		%
		unit :: value_unit(),


		% Type of that value:
		type :: value_type()

		% Constraints are attached to ports, not to values as such.

}).

-type channel_value() :: #channel_value{}.




% Name of a dataflow attribute, i.e. a dataflow-visible attribute of a dataflow
% object.
%
% Such an attribute should be defined as a string (and will be converted in a
% binary string as soon as processed by a dataflow object).
%
% Note that it has to translate nicely to its two associated port names: it must
% comply with the rules listed for port_name/0.
%
-type dataflow_attribute_name() :: dataflow_attribute_string_name()
								 | dataflow_attribute_bin_name().


-type dataflow_attribute_string_name() :: string().

-type dataflow_attribute_bin_name() :: port_name().


% Comment associated to a dataflow attribute:
%
-type dataflow_attribute_comment() :: comment().



% Specification of a dataflow attribute.
%
-record( dataflow_attribute_spec, {


		% Name of this dataflow attribute:
		%
		attribute_name :: dataflow_attribute_name(),


		% Comment (if any) associated to this attribute:
		%
		comment :: maybe( dataflow_attribute_comment() ),


		% Semantics associated to this attribute (as a list of IRIs expressed as
		% plain strings)
		%
		semantics :: user_value_semantics(),


		% Textual description of the type of the values that this attribute is
		% to hold (mandatory):
		%
		unit :: unit_utils:unit_string(),


		% Textual description of the type of the values that this port may
		% hold (mandatory):
		%
		type_description :: value_type_description(),


		% Constraints that apply to the values that this attribute may hold (may
		% not be defined):
		%
		constraints = [] :: value_constraints()

}).


-type dataflow_attribute_spec() :: #dataflow_attribute_spec{}.



% Describes the initial values of the attributes of a dataflow object.
%
-type dataflow_object_initial_values() :: [ { dataflow_attribute_name(),
											  actual_value() } ].


% Type (class) of a dataflow block (generally a non-abstract class):
%
-type block_type() :: wooper:classname().



% Type of a dataflow object, i.e. its classname (ex: 'class_FoobarObject').
%
-type dataflow_object_type() :: block_type().


% Type of a processing unit, i.e. its classname (ex: 'class_FoobarUnit'):
%
-type dataflow_unit_type() :: block_type().



% Name of a dataflow object manager, i.e. its classname (ex:
% 'class_DistrictObjectManager').
%
-type object_manager_name() :: wooper:classname().


% Name of a dataflow unit manager, i.e. its classname (ex:
% 'class_UrbanUnitManager').
%
-type unit_manager_name() :: wooper:classname().



% Definition of the valid formats for mockup clauses:
%
-type clause_time_spec() :: integer() | 'any_time'.


-type clause_input_match_spec() ::
		{ port_string_name(), 'any_state' }
	  | { port_string_name(), 'unset' }
	  | { port_string_name(), 'set' }
	  | { port_string_name(), { 'set', actual_value() } }
	  | { port_string_name(), { 'between', number(), number() } }
	  | { port_string_name(), { 'around', float(), float() } }
	  | { port_string_name(), { 'around', float() } }
	  | { port_string_name(), { 'among', [ actual_value() ] }
}.


-type clause_output_match_spec() ::
		{ port_string_name(), 'reassign' }
	  | { port_string_name(), 'unset' }
	  | { port_string_name(), { 'set', actual_value() } }
	  | { port_string_name(), { 'state_of', port_string_name() }
}.


-type mockup_clause() :: { clause_time_spec(), [ clause_input_match_spec() ],
						   [ clause_output_match_spec() ] }.


% Specification record defining a class of mockup units to be instantiated:
%
-record( mockup_unit_spec, {

		   unit_type = class_DefaultMockupUnit :: dataflow_unit_type(),

		   activation_policy ::
						 class_DataflowProcessingUnit:activation_policy(),

		   input_port_specs :: [ input_port_spec() ],

		   output_port_specs :: [ output_port_spec() ],

		   mockup_clauses :: [ mockup_clause() ]

} ).

-type mockup_unit_spec() :: #mockup_unit_spec{}.




% PID shorthands:


% PID of the identification manager:
-type identification_manager_pid() :: sim_diasca:agent_pid().


% PID of the world manager:
-type world_manager_pid() :: class_Actor:actor_pid().

% PID of the experiment manager:
-type experiment_manager_pid() :: class_Actor:actor_pid().


% PID of a experiment entry point:
-type experiment_entry_point_pid() :: class_Actor:actor_pid().

% PID of a experiment exit point:
-type experiment_exit_point_pid() :: class_Actor:actor_pid().



% PID of a dataflow:
-type dataflow_pid() :: class_Actor:actor_pid().



% PID of a Sim-Diasca actor corresponding to a dataflow block:
-type block_pid() :: class_Actor:actor_pid().



% PID of an object manager:
-type object_manager_pid() :: class_Actor:actor_pid().


% PID of a dataflow block that happens to be more specifically a dataflow
% object:
%
-type object_pid() :: block_pid().



% PID of a processing unit manager:
%
-type unit_manager_pid() :: class_Actor:actor_pid().


% PID of a processing unit:
%
-type processing_unit_pid() :: block_pid().


% Shorthand thereof:
-type unit_pid() :: processing_unit_pid().


% Upstream block, i.e. regarded as having output ports involved in a channel:
-type upstream_block_pid() :: block_pid().


% Downstream block, i.e. regarded as having input ports involved in a channel:
-type downstream_block_pid() :: block_pid().


% PID of a dataflow mockup unit:
-type mockup_unit_pid() :: unit_pid().


% PID of the semantic server:
-type semantic_server_pid() :: class_SemanticServer:semantic_server_pid().


% PID of the type server:
-type type_server_pid() :: class_TypeServer:type_server_pid().


% PID of the identification server:
-type identification_server_pid() :: pid().



% Registered names of components:

-define( world_manager_name, sim_diasca_world_manager ).
-define( experiment_manager_name, sim_diasca_experiment_manager ).


% To support the synchronisation of a dataflow against an external counterpart:
-include("dataflow_changesets_defines.hrl").
