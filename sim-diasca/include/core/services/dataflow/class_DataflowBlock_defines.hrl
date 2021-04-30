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


% This header file is private to the class_DataflowBlock module, and defined to
% make it a bit more tractable and modular.
%
% It focuses mainly on the definition of datastructures and on the dedicated
% functions to manage them.



% Describes an input port of a dataflow block (internal datastructure).
-record( input_port, {


		   % Name of that input port not stored here, as this name is the key
		   % associated to this record:
		   % name :: port_name(),


		   % Internal storage of the associated comment (if any):
		   comment = undefined :: basic_utils:maybe( internal_comment() ),


		   % SUTC information first:


		   % Semantics of the information carried by this port (i.e. the minimal
		   % concepts demanded by this port in order to accept a channel value):
		   %
		   value_semantics :: value_semantics(),


		   % Unit of the values that this port may receive (mandatory):
		   value_unit :: value_unit(),


		   % Type of the values that this port may receive (mandatory):
		   value_type :: value_type(),


		   % Constraints that apply to the values that this port may receive
		   % (may not be defined):
		   %
		   value_constraints = [] :: value_constraints(),


		   % Value (if any) being currently held by this port, with status
		   % information:
		   %
		   value_status = 'unset' :: value_status(),


		   % Timestamp of the last value receiving (if any) performed by this
		   % port:
		   %
		   last_receiving = 'none' :: port_timestamp(),


		   % The output port (if any) feeding this input port:
		   feeder_port = undefined :: basic_utils:maybe( output_port_id() )


}).

-type input_port() :: #input_port{}.



% Describes an output port of a dataflow block (internal datastructure).
%
-record( output_port, {


		   % Name of that output port not stored here, as this name is the key
		   % associated to this record:
		   % name :: port_name(),


		   % Internal storage of the associated comment (if any):
		   %
		   comment = undefined :: basic_utils:maybe( internal_comment() ),


		   % Tells whether this output port shall be seen as a result producer,
		   % i.e. if its values over time are results of interest for the
		   % simulation.
		   %
		   produces_result = false :: boolean(),


		   % SUTC information first:


		   % Semantics of the information carried by this port (i.e. the minimal
		   % concepts demanded by this port in order to accept a channel value):
		   %
		   value_semantics :: value_semantics(),


		   % Unit of the values that this port may send (mandatory):
		   value_unit :: value_unit(),


		   % Type of the values that this port may send (mandatory):
		   value_type :: value_type(),


		   % Constraints that apply to the values that this port may send (may
		   % not be defined):
		   %
		   value_constraints = [] :: value_constraints(),


		   % Value (if any) being currently held by this port, with status
		   % information:
		   %
		   value_status = 'unset' :: value_status(),


		   % Timestamp of the last value sending (if any) performed by this
		   % port:
		   %
		   last_sending = 'none' :: port_timestamp(),


		   % The input ports (if any) fed by this output port:
		   fed_ports = [] :: [ input_port_id() ]


}).

-type output_port() :: #output_port{}.



% Describes a port, notably to check compliance when creating a channel,
% i.e. when connecting another port.
%
% Corresponds roughly to the maximal subset of the fields that are common to an
% input port and an output port; please refer to their counterpart fields for
% documentation.
%
-record( port_description, {
		   semantics :: value_semantics(),
		   unit = dimensionless :: value_unit(),
		   type :: value_type(),
		   constraints = [] :: value_constraints(),
		   status = 'unset' :: value_status()
}).

-type port_description() :: #port_description{}.




% The index (starting at 1) of an actual port obtained from an iterated one.
%
-type iterated_index() :: basic_utils:count().


% A count of iterated ports:
%
-type iterated_count() :: basic_utils:count().



% An iterated port is named according to this token (that is included in its
% actual port name, like in "my_iteration_iterated_4"):
%
-define( iterated_port_token, "_iterated_" ).





% Describes an input port iteration of a dataflow block (internal
% datastructure).
%
% We were initially using a 'spec :: input_port_spec()' field here, where the
% corresponding specification of the input port template could be stored
% directly in this iteration as it is; however some of its fields are to be
% pre-processed from the start and once of all (notably, the user-defined unit
% was translated into a canonical unit) - and thus these elements should not be
% lost.
%
% As it was not logical to allow an input port spec to store such a
% (transformed) canonical unit in some cases, we finally defined specific,
% dedicated fields in order to store all relevant internal information for a
% port iteration.
%
-record( input_port_iteration, {


		   % In this section we store all relevant information coming from the
		   % input port specification (some of which, like the unit, being
		   % already pre-processed).


		   % Base name for that iteration (used as a prefix to name each
		   % iterated port); this name shall not contain ?iterated_port_token.
		   %
		   base_name :: input_port_name(),


		   % Comment (if any) associated to the corresponding iterated input
		   % ports:
		   %
		   comment = undefined :: maybe( internal_comment() ),


		   % Tells about the supported multiplicities in terms of iterated
		   % ports:
		   %
		   multiplicity :: iteration_multiplicity(),



		   % SUTC information for the iterated ports created from this
		   % iteration:


		   % Semantics of the information carried by this iteration (i.e. the
		   % minimal concepts demanded by any of its iterator ports in order to
		   % accept a channel value):
		   %
		   value_semantics :: value_semantics(),


		   % Unit of the values that iterated ports may receive (mandatory):
		   value_unit :: value_unit(),


		   % Actual type (obtained from its textual description) of the values
		   % that the iterated ports may receive (mandatory):
		   %
		   value_type :: value_type(),


		   % Constraints that apply to the values that the iterated ports may
		   % receive (may not be defined):
		   %
		   value_constraints = [] :: value_constraints(),


		   % The ordered list of currently existing iterated ports created from
		   % this port iteration; as any port may be created or destroyed,
		   % indexes are strictly increasing, yet possibly with gaps.
		   %
		   port_indexes = [] :: [ iterated_index() ]

}).

-type input_port_iteration() :: #input_port_iteration{}.



% Describes an output port iteration of a dataflow block (internal
% datastructure).
%
% See input_port_iteration for design comments.
%
-record( output_port_iteration, {


		   % In this section we store all relevant information coming from the
		   % output port specification (some of which, like the unit, being
		   % already pre-processed).


		   % Base name for that iteration (used as a prefix to name each
		   % iterated port); this name shall not contain ?iterated_port_token.
		   %
		   base_name :: output_port_name(),


		   % Comment (if any) associated to the corresponding iterated output
		   % ports:
		   %
		   comment = undefined :: maybe( internal_comment() ),


		   % Tells whether the iterated output ports shall be seen as a result
		   % producers, i.e. if their values over time are results of interest
		   % for the simulation.
		   %
		   produces_result = false :: boolean(),


		   % Tells about the supported multiplicities in terms of iterated
		   % ports:
		   %
		   multiplicity :: iteration_multiplicity(),



		   % SUTC information for the iterated ports created from this
		   % iteration:


		   % Semantics of the information carried by this iteration (i.e. the
		   % minimal concepts demanded by any of its iterator ports in order to
		   % accept a channel value):
		   %
		   value_semantics :: value_semantics(),


		   % Unit of the values that iterated ports may receive (mandatory):
		   value_unit :: value_unit(),


		   % Actual type (obtained from its textual description) of the values
		   % that the iterated ports may receive (mandatory):
		   %
		   value_type :: value_type(),


		   % Constraints that apply to the values that the iterated ports may
		   % receive (may not be defined):
		   %
		   value_constraints = [] :: value_constraints(),


		   % The ordered list of currently existing iterated ports created from
		   % this port iteration (any port may be created or destroyed):
		   %
		   port_indexes = [] :: [ iterated_index() ]

}).

-type output_port_iteration() :: #output_port_iteration{}.



% Associative table to hold input ports:
-type input_port_table() :: table( input_port_name(), input_port() ).

% Associative table to hold output ports:
-type output_port_table() :: table( output_port_name(), output_port() ).



% Associative table to hold input port iterations:
-type input_iteration_table() ::
		table( input_iteration_name(), input_port_iteration() ).

% Associative table to hold output port iterations:
-type output_iteration_table() ::
		table( output_iteration_name(), output_port_iteration() ).
