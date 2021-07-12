% Copyright (C) 2018-2021 EDF R&D

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



% Stores and centralises SimDiasca-level information gathered about a given
% actor oneway.
%
% This record is to strictly supersede the WOOPER-level oneway_info one.
%
% See also: this counterpart oneway_info WOOPER record, defined in
% wooper_info.hrl.
%
-record( actor_oneway_info, {


		   name = undefined :: class_Actor:actor_oneway_name(),
		   % The name of that actor oneway.


		   arity = undefined :: wooper:method_arity(),
		   % The arity of that actor oneway.


		   qualifiers = [] :: wooper:method_qualifiers(),
		   % Qualifiers applying to this actor oneway.


		   ast_location = undefined :: maybe( ast_info:location() ),
		   % Corresponds to (our view of) the in-AST sortable location of the
		   % full form for the definition (first clause) of this actor oneway
		   % (not of its spec, which has its specific field below).


		   file_location = undefined :: maybe( ast_base:file_loc() ),
		   % Corresponds to the in-file location of the first defined clause (in
		   % its source file) of this actor oneway:
		   %
		   % (this information is not redundant with the previous field, as they
		   % are different types of locations; however it is a priori redundant
		   % with the one in the first clause, yet present in the forms, thus
		   % kept here; note that the linter will not accept an 'undefined'
		   % value)


		   clauses = [] :: [ meta_utils:clause_def() ],
		   % Actor oneway actual definition, a (non-located) list of the
		   % abstract forms of its clauses.


		   spec = undefined :: maybe( wooper_info:located_method_spec() )
		   % The type specification (if any) of that actor oneway, as an
		   % abstract form.

} ).


-type actor_oneway_info() :: #actor_oneway_info{}.






% Stores and centralises SimDiasca-level information gathered about a given
% actor class.
%
% This record is to strictly supersede the WOOPER-level class_info one.
%
% See also: this class_info counterpart WOOPER record, defined in
% wooper_info.hrl.
%
-record( actor_class_info, {


		class :: wooper_info:class_entry(),
		% Name of that class.


		superclasses :: [ wooper:classname() ],
		% Ordered list of the superclasses of this class.


		attributes :: wooper_info:attribute_table(),
		% All the class-specific attributes of the instances (hence not
		% comprising inherited ones).


		inherited_attributes :: wooper_info:attribute_table(),
		% All inherited attribute definitions for this class.


		compilation_options :: ast_info:compile_option_table(),
		% A table, whose keys are compilation options (ex: no_auto_import,
		% inline, etc.) and whose values are aggregated lists of their
		% associated values (ex: [{size,1}] and [{get_bucket_index,2},{f/1}]).
		%
		% Note: for the 'inline' key, if full inlining is enabled ( '-compile(
		% inline ).'), then its associated key is not a list of function
		% identifiers, but 'all'.


		compilation_option_defs = [] :: [ ast_info:located_form() ],
		% We merely touch compilation options (ex: '{compile, {inline, [{
		% FunName, Arity}]}}').


		parse_attributes :: ast_info:attribute_table(),
		% Other (unrecognised, not corresponding to other fields of interest)
		% parse-level attributes (ex: '-my_attribute( my_value ).'), as a table
		% associating, to an attribute name (an atom key), a list of pairs
		% comprising each a value and an AST form.
		%
		% Such attributes, also named "wild attributes", mostly correspond to
		% user-defined ones.


		remote_spec_defs = [] :: [ ast_info:located_form() ],
		% As remote function specifications can be defined, like:
		% -spec Mod:Name(...) -> ...


		includes = [] :: [ file_utils:file_name() ],
		% Include files (typically *.hrl files, but also includes the .erl
		% source module):
		%
		% (expected to remain empty, as the preprocessor is supposed to have
		% already been run)


		include_defs = [] :: [ ast_info:located_form() ],
		% Include definitions.


		type_exports :: ast_info:type_export_table(),
		% Whether a type (possibly any kind of it; ex: opaque or not) is
		% exported is recorded primarily in its own type_info record through a
		% list of locations, while the information sufficient to reconstruct the
		% actual forms for the exports of all types are recorded here.
		%
		% Note: it is better that way, as a type export attribute may define any
		% number of exports, and we need to record its definition line.
		%
		% (this field must be kept synchronised with the table in the
		% 'types' field)


		types :: ast_info:type_table(),
		% All information, indexed by type identifiers, about all the types
		% defined in that module:


		records :: ast_info:record_table(),
		% All information (notably: field descriptions), indexed by record
		% names, about all the records known of that module.


		function_imports :: ast_info:function_import_table(),
		% Lists the functions imported by that module, per-module.


		function_imports_defs = [] :: [ ast_info:located_form() ],
		% The definitions of the function imports.


		function_exports :: ast_info:function_export_table(),
		% Whether a plain function (i.e. not a method) is exported is recorded
		% primarily in its own record through a list of locations, while the
		% information sufficient to reconstruct the actual forms for the exports
		% of all functions are recorded here.
		%
		% Note: it is better that way, as a function export attribute may define
		% any number of exports, and we need to record its definition line.
		%
		% (this field must be kept synchronised with the table in the
		% corresponding 'functions' field)


		functions :: ast_info:function_table(),
		% All information about the other, plain functions defined for that
		% class.
		%
		% (this field must be kept synchronised with the table in the
		% 'function_exports' field)


		constructors :: wooper_info:constructor_table(),
		% All information about the constructor(s) of that class.


		new_operators :: ast_info:function_table(),
		% All information about the new operators (ex: remote_new_link/N) of
		% that class.


		destructor = undefined :: maybe( ast_info:function_info() ),
		% All information about the destructor (if any) of that class.


		request_exports :: wooper_info:request_export_table(),
		% Whether a request is exported is recorded primarily in its own
		% request_info record through a list of locations, while the information
		% sufficient to reconstruct the actual forms for the exports of all are
		% recorded here.
		%
		% Note: it is better that way, as a request export attribute may define
		% any number of exports, and we need to record its definition line.
		%
		% (this field must be kept synchronised with the table in the
		% corresponding 'requests' field)


		requests :: wooper_info:request_table(),
		% All information about the class-specific (member) request methods
		% defined for that class.
		%
		% (this field must be kept synchronised with the table in the
		% 'request_exports' field)


		oneway_exports :: wooper_info:oneway_export_table(),
		% Whether a oneway is exported is recorded primarily in its own
		% oneway_info record through a list of locations, while the information
		% sufficient to reconstruct the actual forms for the exports of all are
		% recorded here.
		%
		% Note: it is better that way, as a oneway export attribute may define
		% any number of exports, and we need to record its definition line.
		%
		% (this field must be kept synchronised with the table in the
		% corresponding 'oneways' field)


		oneways :: wooper_info:oneway_table(),
		% All information about the class-specific (member) oneway methods
		% defined for that class and that are not actor oneways.
		%
		% (this field must be kept synchronised with the table in the
		% 'oneway_exports' field)


		actor_oneway_exports :: actor_info:actor_oneway_export_table(),
		% Whether an actor oneway is exported is recorded primarily in its own
		% actor_oneway_info record through a list of locations, while the
		% information sufficient to reconstruct the actual forms for the exports
		% of all are recorded here.
		%
		% Note: it is better that way, as an actor oneway export attribute may
		% define any number of exports, and we need to record its definition
		% line.
		%
		% (this field must be kept synchronised with the table in the
		% corresponding 'actor_oneways' field)


		actor_oneways :: wooper_info:oneway_table(),
		% All information about the class-specific (member) oneway methods
		% defined for that class.
		%
		% (this field must be kept synchronised with the table in the
		% 'oneway_exports' field)


		static_exports :: wooper_info:static_export_table(),
		% Whether a static method is exported is recorded primarily in its own
		% static_info record through a list of locations, while the information
		% sufficient to reconstruct the actual forms for the exports of all are
		% recorded here.
		%
		% Note: it is better that way, as a static method export attribute may
		% define any number of exports, and we need to record its definition
		% line.
		%
		% (this field must be kept synchronised with the table in the
		% corresponding 'statics' field)



		statics :: wooper_info:static_table(),
		% All information about the class-specific (member) static methods
		% defined for that class:
		%
		% (this field must be kept synchronised with the table in the
		% 'static_exports' field)



		optional_callbacks_defs = [] :: [ ast_info:located_form() ],
		% The definitions of the list of optional callbacks.



		debug_mode = true :: boolean(),
		% Tells whether this class shall be compiled and run in debug mode,
		% hence with extended checks for example (slower, safer, easier to
		% debug).



		last_file_location :: ast_info:located_form(),
		% The definition of the last in-file location in the original source
		% file:
		%
		% (we keep it as a located form rather than as a simple
		% ast_utils:file_loc() to avoid a costly addition in last position)
		%
		% (any added code will be put afterwards)



		markers :: ast_info:section_marker_table(),
		% Section markers, offering reference locations to AST transformations.



		errors = [] :: [ ast_info:error() ],
		% Error information collected when traversing the AST.



		unhandled_forms = [] :: [ ast_info:located_form() ]
		% List of all the located forms that are unhandled, which might be
		% typically errors, like:
		%
		% '{error,{LineNumber,erl_parse, ["syntax error before: ","')'"]}}''.


} ).
