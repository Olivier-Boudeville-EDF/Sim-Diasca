% Copyright (C) 2016-2025 EDF R&D
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
% Creation date: 2016.


% This header file is private to the class_DataflowBlock module, and defined to
% make it a bit more tractable and modular.
%
% It focuses mainly on the definition of datastructures and on the dedicated
% functions to manage them.



% Describes an input port of a dataflow block (internal datastructure):
-record( input_port, {


	% Name of that input port not stored here, as this name is the key
	% associated to this record:
	% name :: port_name(),


	% Internal storage of the associated comment (if any):
	comment = undefined :: option( internal_comment() ),


	% SUTC information first:


	% Semantics of the information carried by this port (i.e. the minimal
	% concepts demanded by this port in order to accept a channel value):
	%
	value_semantics :: value_semantics(),


	% Unit of the values that this port may receive (mandatory):
	value_unit :: value_unit(),


	% Type of the values that this port may receive (mandatory):
	value_type :: value_type(),


	% Constraints that apply to the values that this port may receive (may not
	% be defined):
	%
	value_constraints = [] :: value_constraints(),


	% Value (if any) being currently held by this port, with status information:
	value_status = 'unset' :: value_status(),


	% Timestamp of the last value receiving (if any) performed by this port:
	last_receiving = 'none' :: port_timestamp(),


	% The output port (if any) feeding this input port:
	feeder_port = undefined :: option( output_port_id() ) } ).





% Describes an output port of a dataflow block (internal datastructure):
-record( output_port, {


	% Name of that output port not stored here, as this name is the key
	% associated to this record:
	% name :: port_name(),


	% Internal storage of the associated comment (if any):
	comment = undefined :: option( internal_comment() ),


	% Tells whether this output port shall be seen as a result producer, i.e. if
	% its values over time are results of interest for the simulation.
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


	% Constraints that apply to the values that this port may send (may not be
	% defined):
	%
	value_constraints = [] :: value_constraints(),


	% Value (if any) being currently held by this port, with status information:
	value_status = 'unset' :: value_status(),


	% Timestamp of the last value sending (if any) performed by this port:
	last_sending = 'none' :: port_timestamp(),


	% The input ports (if any) fed by this output port:
	fed_ports = [] :: [ input_port_id() ] } ).




% Describes a port, notably to check compliance when creating a channel, that is
% when connecting another port.
%
-record( port_description, {
	semantics :: value_semantics(),
	unit = dimensionless :: value_unit(),
	type :: value_type(),
	constraints = [] :: value_constraints(),
	status = 'unset' :: value_status() } ).



% An iterated port is named according to this token (that is included in its
% actual port name, like in "my_iteration_iterated_4"):
%
-define( iterated_port_token, "_iterated_" ).





% Describes an input port iteration of a dataflow block (internal
% datastructure).
%
-record( input_port_iteration, {


	% In this section we store all relevant information coming from the input
	% port specification (some of which, like the unit, being already
	% pre-processed).


	% Base name for that iteration (used as a prefix to name each iterated
	% port); this name shall not contain ?iterated_port_token.
	%
	base_name :: input_port_name(),


	% Comment (if any) associated to the corresponding iterated input ports:
	comment = undefined :: option( internal_comment() ),


	% Tells about the supported multiplicities in terms of iterated ports:
	multiplicity :: iteration_multiplicity(),



	% SUTC information for the iterated ports created from this iteration:


	% Semantics of the information carried by this iteration (i.e. the minimal
	% concepts demanded by any of its iterator ports in order to accept a
	% channel value):
	%
	value_semantics :: value_semantics(),


	% Unit of the values that iterated ports may receive (mandatory):
	value_unit :: value_unit(),


	% Actual type (obtained from its textual description) of the values that the
	% iterated ports may receive (mandatory):
	%
	value_type :: value_type(),


	% Constraints that apply to the values that the iterated ports may receive
	% (may not be defined):
	%
	value_constraints = [] :: value_constraints(),


	% The ordered list of currently existing iterated ports created from this
	% port iteration; as any port may be created or destroyed, indexes are
	% strictly increasing, yet possibly with gaps.
	%
	port_indexes = [] :: [ class_DataflowBlock:iterated_index() ] } ).




% Describes an output port iteration of a dataflow block (internal
% datastructure).
%
% See input_port_iteration for design comments.
%
-record( output_port_iteration, {


	% In this section we store all relevant information coming from the output
	% port specification (some of which, like the unit, being already
	% pre-processed).


	% Base name for that iteration (used as a prefix to name each iterated
	% port); this name shall not contain ?iterated_port_token.
	%
	base_name :: output_port_name(),


	% Comment (if any) associated to the corresponding iterated output ports:
	comment = undefined :: option( internal_comment() ),


	% Tells whether the iterated output ports shall be seen as a result
	% producers, i.e. if their values over time are results of interest for the
	% simulation.
	%
	produces_result = false :: boolean(),


	% Tells about the supported multiplicities in terms of iterated ports:
	multiplicity :: iteration_multiplicity(),



	% SUTC information for the iterated ports created from this iteration:


	% Semantics of the information carried by this iteration (i.e. the minimal
	% concepts demanded by any of its iterator ports in order to accept a
	% channel value):
	%
	value_semantics :: value_semantics(),


	% Unit of the values that iterated ports may receive (mandatory):
	value_unit :: value_unit(),


	% Actual type (obtained from its textual description) of the values that the
	% iterated ports may receive (mandatory):
	%
	value_type :: value_type(),


	% Constraints that apply to the values that the iterated ports may receive
	% (may not be defined):
	%
	value_constraints = [] :: value_constraints(),


	% The ordered list of currently existing iterated ports created from this
	% port iteration (any port may be created or destroyed):
	%
	port_indexes = [] :: [ class_DataflowBlock:iterated_index() ] } ).
