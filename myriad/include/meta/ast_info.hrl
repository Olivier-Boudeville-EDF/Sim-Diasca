% Copyright (C) 2014-2021 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Saturday, February 3, 2018.


% This header file gathers mostly following info records:
% - module_info
% - type_info
% - function_info



% Description of a module name:
-type module_entry() :: basic_utils:maybe( { basic_utils:module_name(),
											 ast_info:located_form() } ).


% A record to store and centralise information gathered about an Erlang
% (compiled) module.
%
% Allows to perform checkings, and to reorder and transform the returned version
% of it.
%
% For that, ASTs and forms are used, in a located version of them, as the form
% order matters (to the actual compilation) and is a different, complementary
% information to the original line numbers, which are kept for source reference
% purposes.
%
% We store the located forms verbatim whenever possible (depending on the case,
% either directly in the same field, or in *_def* counterpart fields), notably
% to preserve the line numbers within.
%
% In the former case, a single field (ex: 'module') contains a pair of
% information, whose first element is the higher level, syntax-free information
% that was gathered, and whose second element is a lower level, form-based,
% information (a bit more informative in terms of syntax and source layout, yet
% less tractable from a user point of view).
%
% In the latter case, there are thus two fields per theme:
%
% - the high-level, developer-friendly one (ex: 'compilation_options')
% - the raw one in AST form (ex: 'compilation_option_defs')
%
% It is up to the user to ensure that, if either field is modified, its
% counterpart one is updated accordingly. Note that there may not be a
% one-to-one mapping between the elements of two associated fields.
%
-record( module_info, {


		% Name (if any) of that module, together with its definition (as a
		% located form):
		%
		module = undefined :: module_entry(),



		% A table, whose keys are compilation options (ex: no_auto_import,
		% inline, etc.) and whose values are aggregated *lists* of their
		% associated values (ex: [{size,1}] and [{get_bucket_index,2},{f/1}]).
		%
		% Note:
		%
		% - for this aggregated, higher-level field, the specific way that was
		% used in order to specify these various information (ex: separately or
		% in groups, at which actual file locations, etc.) is abstracted out
		%
		% - options can also be specified through the command line with the -D
		% selector (ex: see ERLANG_COMPILER_TOKEN_OPT); as the in-source ones,
		% they are gathered in this table (yet do not have of course counterpart
		% forms in the compilation_option_defs field below); no specific
		% precedence between command-line options applies, as to a given option
		% name is associated a list of values (so they can be all aggregated in
		% that entry)
		%
		% - the 'inline' key is special-cased to account for its variants: if
		% full inlining is enabled ( '-compile( inline ).'), then its associated
		% key is not a list of function identifiers, but the 'all' atom
		%
		% - this field is to be kept in sync with its 'compilation_option_defs'
		% counterpart lower-level field
		%
		compilation_options :: ast_info:compile_option_table(),


		% The actual, lower-level definitions of the compilation options, like
		% in:
		%
		% [...]
		% {attribute,61,compile,{no_auto_import,[{size,1}]}},
		% {attribute,63,compile,{inline,[{get_bucket_index,2}]}},
		% {attribute,64,compile,{no_auto_import,[{foo,1},{bar,2}]}},
		% [...]
		%
		% Note: to be kept in sync with its 'compilation_options' counterpart
		% higher-level field
		%
		compilation_option_defs = [] :: [ ast_info:located_form() ],



		% Parse-level attributes (ex: '-my_attribute( my_value ).'), as a table
		% associating, to an attribute name (an atom key such as
		% 'my_attribute'), a list of pairs comprising each a value (ex:
		% my_value) and the corresponding AST form (knowing of course that a
		% given attribute name may be used multiple times).
		%
		% Such attributes, also named "wild attributes", mostly correspond to
		% user-defined ones.
		%
		parse_attributes :: ast_info:attribute_table(),


		% Remote function specifications may be defined, like in:
		% -spec Mod:Name(...) -> ...
		%
		% Note: no counterpart field.
		%
		remote_spec_defs = [] :: [ ast_info:located_form() ],



		% Include files (typically *.hrl files).
		%
		% Unlike the raw definitions (in the 'include_defs' field), this field
		% does not include the filename of the module being compiled (ex:
		% "foobar.erl").
		%
		% (there is no duplicate either in that include list; we do not use a
		% set here though, as set_utils is not a bootstrapped module)
		%
		% Note: to be kept in sync with its 'include_defs' counterpart
		% lower-level field.
		%
		includes = [] :: [ file_utils:bin_file_path() ],


		% Include definitions:
		%
		% (possibly a given file might be included more than once; it is
		% generally the case for the module being currently compiled)
		%
		% Note: to be kept in sync with its 'includes' counterpart higher-level
		% field.
		%
		include_defs = [] :: [ ast_info:located_form() ],



		% Whether a type (possibly any kind of it; ex: opaque or not) is
		% exported is stored primarily in its own type_info record (see the
		% 'types' field) through a list of locations, while the information
		% sufficient to reconstruct the actual forms for the exports of all
		% types are recorded here.
		%
		% Note: it is better that way, as a type export attribute may define any
		% number of exports, and we need to record its definition line.
		%
		% Note: this field must be kept synchronised with the table in the
		% 'types' field.
		%
		type_exports :: ast_info:type_export_table(),


		% All information, indexed by type identifiers, about all the types
		% defined in that module.
		%
		% Note: this field must be kept synchronised with the table in the
		% 'type_exports' field.
		%
		types :: ast_info:type_table(),



		% All information (notably: field descriptions), indexed by record
		% names, about all the records known of that module:
		%
		records :: ast_info:record_table(),



		% Lists the functions imported by that module, indexed per
		% implementation module (each being a key of that table).
		%
		% Note: to be kept in sync with its 'function_imports_defs' counterpart
		% lower-level field.
		%
		function_imports :: ast_info:function_import_table(),


		% The definitions of the function imports:
		%
		% Note: to be kept in sync with its 'function_imports' counterpart
		% higher-level field.
		%
		function_imports_defs = [] :: [ ast_info:located_form() ],



		% Whether a function (possibly any kind of it) is exported is recorded
		% primarily in its own function_info record through a list of locations,
		% while the information sufficient to reconstruct the actual forms for
		% the exports of all functions are recorded here.
		%
		% Note: it is better that way, as a function export attribute may define
		% any number of exports, and we need to record its definition line.
		%
		% As empty exports are allowed (i.e. '-export([]).'), by default at the
		% 'export_functions_marker' marker such an empty export is automatically
		% defined, so that the parse transforms are able to populate it whenever
		% they decide to introduce new functions that shall be exported.
		%
		% This field must be kept synchronised with the table in the 'functions'
		% field; note however that, should a function_info record there declare
		% among its locations (in its 'exported' field) the location of an
		% actual export form (typically the one of the previous marker) in this
		% current field (function_exports), the corresponding export will be
		% added here automatically, if not already defined.
		%
		function_exports :: ast_info:function_export_table(),


		% All information, indexed by function identifiers, about all the
		% functions defined in that module:
		%
		% Note: this field must be kept synchronised with the table in the
		% 'function_exports' field.
		%
		functions :: ast_info:function_table(),



		% The definitions of the list of optional callbacks:
		%
		% Note: no counterpart field.
		%
		optional_callbacks_defs = [] :: [ ast_info:located_form() ],


		% The definition of the last line in the original source file:
		%
		% (we keep it as a located form rather than a simple meta_utils:line()
		% to avoid a costly addition in last position)
		%
		last_line :: ast_info:located_form(),


		% Section markers, offering reference locations to AST transformations.
		markers :: ast_info:section_marker_table(),


		% Error information collected when traversing the AST
		errors = [] :: [ ast_info:error() ],


		% List of all the located forms that are unhandled, which are typically
		% errors, like:
		%
		% '{error,{LineNumber,erl_parse, ["syntax error before: ","')'"]}}''.
		%
		% or forms meant to trigger errors (ex: if resulting in defining a
		% function more than once)
		%
		unhandled_forms = [] :: [ ast_info:located_form() ]


} ).




% Describes a type (generally extracted from a module).
-record( type_info, {


		   % The name of that type:
		   name = undefined :: type_utils:type_name(),


		   % The (ordered) list of variable definitions (ex: [ { var, Line, 'X'
		   % } ]) of this type:
		   %
		   % Note: set purposedly not to an empty list by default (to convey a
		   % different meaning)
		   %
		   variables = undefined ::
			 basic_utils:maybe( [ ast_type:ast_variable_pattern() ] ),


		   % Tells whether this type is defined as opaque:
		   opaque = undefined :: boolean(),


		   % Corresponds to the location of the full form for the definition of
		   % this type:
		   %
		   location = undefined :: basic_utils:maybe( ast_info:location() ),


		   % Corresponds to the line where this type is defined (in its source
		   % file):
		   %
		   line = undefined :: basic_utils:maybe( ast_base:line() ),


		   % Type actual definition, a (non-located) abstract form:
		   definition = undefined :: ast_type:ast_type(),


		   % Tells whether this type has been exported, as a (possibly
		   % empty) list of the location(s) of its actual export(s), knowing
		   % that a type can be exported more than once, or never:
		   %
		   exported = [] :: [ ast_info:location() ]

} ).




% Describes a function (generally extracted from a module, or possibly
% automatically generated).
%
-record( function_info, {


		   % The name of that function:
		   name = undefined :: meta_utils:function_name(),


		   % The arity of that function:
		   arity = undefined :: arity(),


		   % Corresponds to the location of the full form for the definition
		   % (first clause) of this function (not of the spec, which has its
		   % specific field below):
		   %
		   location = undefined :: basic_utils:maybe( ast_info:location() ),


		   % Corresponds to the line of the first defined clause (in its source
		   % file):
		   %
		   % (this information is a priori redundant with the one in the first
		   % clause, yet present in the forms, thus kept here; note that the
		   % linter will not accept an 'undefined' value)
		   %
		   line = undefined :: basic_utils:maybe( ast_base:line() ),


		   % Function actual definition, a (non-located) list of the abstract
		   % forms of its clauses:
		   %
		   clauses = [] :: [ meta_utils:clause_def() ],


		   % The type specification (if any) of that function, as an abstract
		   % form:
		   %
		   spec = undefined ::
			 basic_utils:maybe( ast_info:located_function_spec() ),


		   % Tells whether the function is a callback (hence declared as such
		   % with a -callback attribute) or not.
		   %
		   % The latter case is the default (whether this function is declared
		   % with a -spec attribute, or with no related attribute at all).
		   %
		   callback = false :: boolean(),


		   % Tells whether this function has been exported, as a (possibly
		   % empty) list of the location(s) of its actual export(s), knowing
		   % that a function can be exported more than once or never.
		   %
		   % Note that, provided a legit location is specified here (i.e. the
		   % location of an actual export form), this function will be
		   % automatically declared in that export form, should it be not already)
		   %
		   exported = [] :: [ ast_info:location() ]


} ).
