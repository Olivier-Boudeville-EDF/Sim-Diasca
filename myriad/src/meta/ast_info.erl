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


% Module centralising the management of all information that can be extracted
% from ASTs.
%
-module(ast_info).


% For table type:
-include("meta_utils.hrl").


% For the corresponding records:
-include("ast_info.hrl").


-type module_info() :: #module_info{}.

-type type_info() :: #type_info{}.

-type function_info() :: #function_info{}.



% Location of a form in an AST, so that the order of forms can be recreated.
%
% We use sortable identifiers so that any number of new forms can be introduced
% between any two of them, if needed.
%
% Location is relative to the position of a form in a given AST, while the line
% information embedded in forms is relative to the file in which they are
% defined.
%
% Thanks to locations (which order forms appropriately, including the ones
% regarding file references), once forms have been recomposed, by design a
% stored line is always relative to the current file.
%
% 'locate_at' means that the corresponding form is yet to be located just after
% the location pointed at by the specified marker in the AST stream (thus a
% transformation pass is still to be applied before it becomes an actual
% sortable identifier). Such a relative location allows for example to place
% forms just after (and never, for example, just before) a module declaration.
%
% We try to translate section markers in actual locations rather sooner than
% later, as markers are not expected to move, and it would probably be more
% difficult to recreate a consistency at later stages.
%
% Similarly, 'locate_after' means that a form is yet to be located after the
% specified form.
%
% There is no specific order enforced between the possibly several forms located
% at a given marker or after a given form.
%
-type location() :: ast_base:form_location()
				  | { 'locate_at', section_marker() }
				  | { 'locate_after', ast_base:form_location() }.


% When processing an AST (ex: read from a BEAM file), the order of the forms
% matters (for example to report compile errors, which are relative to a context
% defined by the last '-file' attribute previously encountered, i.e. like
% {attribute,40,file,{"foo.erl",40}}). So even if we store forms in tables
% according to their type, when (re)generating the AST we have to recreate the
% same order.
%
% To do so, instead of managing a list of forms, we manage any sets of located
% forms by including in each form an identifier allowing to recreate the form
% order in the original AST.
%
-type located_form() :: { location(), form() }.


% An AST including location information:
-type located_ast() :: [ located_form() ].


% Located type specification of a function:
-type located_function_spec() :: { location(), meta_utils:function_spec() }.



% Known section markers (insertion points), listed in their expected order of
% appearance in an AST stream (ex: a source file). All markers are expected to
% be set (located) as soon as the scan of an AST into a module_info has been
% done.
%
% These are, more specifically, the points, the lower locations from which
% corresponding elements would be inserted (hence not necessarily in the exact
% same order as inferred from an AST).
%
% Multiple markers may point to the same location.
%
% There may or may not be actual forms at such locations in the corresponding
% AST: to preserve their order, markers may point to locations that have been
% generated, i.e. that have not been directly obtained that the initial scan
% (ex: inserted between an actual one and the logical end of the AST).
%
-type section_marker() ::

		% Marker designating the beginning of the AST stream / source file:
		'begin_marker'


		% Marker designating a section starting just after the module
		% declaration (i.e. '-module(my_module_name).'):
		%
	  | 'module_marker'


		% Marker designating a section dedicated to the export of types
		% (i.e. where '-import([ bar/n, ...]).' declarations may be gathered):
		%
	  | 'export_types_marker'


		% Marker designating a section dedicated to the export of functions
		% (i.e. where '-export([ foo/n, ...]).' declarations may be gathered):
		%
	  | 'export_functions_marker'


		% Marker designating a section dedicated to the import of functions
		% (i.e. where -import( Module, Funcitons ) declarations may be
		% gathered):
		%
	  | 'import_functions_marker'

	  % Not relevant, as includes have already be inlined in an input AST:
	  %| 'include_marker'    % Include declarations


		% Marker designating a section dedicated to the definition of records:
	  | 'definition_records_marker'


		% Marker designating a section dedicated to the definition of types:
	  | 'definition_types_marker'


		% Marker designating a section dedicated to the definition of functions:
	  | 'definition_functions_marker'

	  | 'end_marker'.       % End of the AST stream / source file



% Tables to be found in the module_info record:


% Ex: inline, export_all, etc.
-type compile_option_name() :: atom().

% In some cases (at least when it is specified from the command-line), a
% compilation option is a triplet (ex: -Dmy_other_test_token=51 is translated,
% in terms of a parse-transform option, as: {d,my_other_test_token,51}).
%
% The value associated to the option name ('d') is then:
% {my_other_test_token,51}.
%
-type compile_option_value() :: term().


% As typically obtained from options fed to a parse-transform:
-type compile_option_entry() :: compile_option_name()
					  | { compile_option_name(), compile_option_value() }.


% For easy access to compilation information:
%
% Note that an option specified without a value (ex: -Dmy_token on the command
% line) will be associated to the 'undefined' value.
%
-type compile_option_table() :: ?table:?table( compile_option_name(),
							basic_utils:maybe( [ compile_option_value() ] ) ).



% The name of a (parse-level) attribute (ex: '-my_attribute( my_value ).').
-type attribute_name() :: atom().


% The value of a (parse-level) attribute (ex: '-my_attribute( my_value ).').
-type attribute_value() :: term().



% Parse-level attribute:
-type attribute() :: { attribute_name(), attribute_value() }.


% For easy access to the values and AST forms associated to a parse attribute,
% from its name.
%
% For example, to the 'dialyzer' key will be associated the values and located
% forms of the various '-dialyzer( XXX ).' found in a source file.
%
-type attribute_table() :: ?table:?table( attribute_name(),
					   [ { attribute_value(), located_form() } ] ).




% A table associating, to a given location, the corresponding line in the source
% file (to recreate the corresponding export form) and a list of the identifiers
% of the types to declare exported there.
%
% Note:
%
% - this table must be explicitly updated whenever adding or removing a type
% in a module_info'types' field; see: add_type/2 and remove_type/2
%
% - [ type_id() ] used, not a set, to better preserve order
%
-type type_export_table() :: ?table:?table( location(),
										{ line(), [ type_id() ] } ).



% A table associating to each type identifier a full type information.
-type type_table() :: ?table:?table( type_id(), type_info() ).




% A table associating to each record name the description of the corresponding
% record.
%
-type record_table() :: ?table:?table( basic_utils:record_name(),
									   record_definition() ).


% The full definition of a record.
-type record_definition() :: { field_table(), location(), line() }.



% A table associating to a given field of a record its description.
%
% The ?table type (usually map_hashtable) cannot be used, as it does not
% preserve the order of its entries, whereas the fields are indexed in
% tuple-records according to their rank in the corresponding list.

% Best solution here is not a list_table (which does not strictly preserve
% element order either), but a plain (ordered) list (of pairs).
%
-type field_table() :: [ { basic_utils:field_name(),
						   ast_record:field_definition() } ].



% A table referencing, for each module listed, a list of the functions that are
% imported from it by the current module:
%
-type function_import_table() :: ?table:?table( basic_utils:module_name(),
												[ function_id() ] ).




% A table associating, to a given location, the corresponding line in the source
% file (to recreate the corresponding export form) and a list of the identifiers
% of the functions to declare exported there.
%
% Note:
% - this table must be explicitly updated whenever adding or removing a function
% in a module_info 'functions' field; see: meta_utils:add_function/2 and
% meta_utils:remove_function/2
% - a list of function_id() is used, not a set, to better preserve order
% - the 'export_all' compile attribute may also have been set in compile options
%
-type function_export_table() ::
		?table:?table( location(), { line(), [ function_id() ] } ).



% A table associating to each function identifier a full function information.
-type function_table() :: ?table:?table( function_id(), function_info() ).


% A table storing the actual locations corresponding to the standard section
% markers:
%
-type section_marker_table() :: ?table:?table( section_marker(), location() ).



% All relevant information about an error found in an AST:
%
% (note: includes warnings)
%
-type error() :: { ast_scan:scan_context(), ast_scan:error_report() }.


-export_type([ module_info/0, type_info/0, function_info/0,

			   location/0, located_form/0, located_ast/0,
			   located_function_spec/0,

			   section_marker/0,

			   compile_option_name/0, compile_option_value/0,
			   compile_option_entry/0, compile_option_table/0,

			   attribute_name/0, attribute_value/0, attribute/0,
			   attribute_table/0,

			   type_export_table/0, type_table/0,

			   record_table/0, record_definition/0, field_table/0,

			   function_import_table/0, function_export_table/0,
			   function_table/0, section_marker_table/0,

			   error/0 ]).


% General module-info helpers:
-export([ ensure_function_exported/4, ensure_function_not_exported/3,
		  located_ast_to_string/1 ]).


% Module-info section:
-export([ extract_module_info_from_ast/1, init_module_info/0,
		  check_module_info/1, interpret_options/2,
		  recompose_ast_from_module_info/1,
		  write_module_info_to_file/2,

		  get_default_module_location/0, get_default_module_location/1,

		  get_default_export_type_location/0,
		  get_default_export_type_location/1,

		  get_default_export_function_location/0,
		  get_default_export_function_location/1,

		  get_default_import_function_location/0,
		  get_default_import_function_location/1,

		  get_default_definition_record_location/0,
		  get_default_definition_record_location/1,

		  get_default_definition_type_location/0,
		  get_default_definition_type_location/1,

		  get_default_definition_function_location/0,
		  get_default_definition_function_location/1,

		  module_info_to_string/1, module_info_to_string/2,
		  module_info_to_string/3 ]).


% Elements for textual descriptions:
-export([ forms_to_string/3, location_to_string/1,
		  module_entry_to_string/2,
		  compilation_options_to_string/3, compilation_options_to_string/4,
		  optional_callbacks_to_string/3,
		  parse_attribute_table_to_string/2, parse_attribute_table_to_string/3,
		  remote_spec_definitions_to_string/3,
		  includes_to_string/3, includes_to_string/4,
		  type_exports_to_string/2, type_exports_to_string/3,
		  types_to_string/3,
		  records_to_string/1, records_to_string/2,
		  function_imports_to_string/4,
		  functions_to_string/3,
		  last_line_to_string/1,
		  markers_to_string/1, markers_to_string/2,
		  errors_to_string/2,
		  unhandled_forms_to_string/3,
		  fields_to_strings/1, field_to_string/3,
		  function_id_to_string/1,
		  function_info_to_string/1, function_info_to_string/2,
		  function_info_to_string/3 ]).


% General type-info helpers:
-export([ ensure_type_exported/4, ensure_type_not_exported/3,
		  type_id_to_string/1,
		  type_info_to_string/1, type_info_to_string/3 ]).


% Local shorthands:

-type ast() :: ast_base:ast().
-type line() :: ast_base:line().
-type form() :: ast_base:form().

-type type_id() :: type_utils:type_id().
-type function_id() :: meta_utils:function_id().

-type ustring() :: text_utils:ustring().
-type indentation_level() :: text_utils:indentation_level().




% Section for general helpers.



% Ensures that the specified function is exported at the specified location(s).
-spec ensure_function_exported( function_id(), [ location() ], module_info(),
						function_export_table() ) -> function_export_table().
ensure_function_exported( _FunId, _ExportLocs=[], _ModuleInfo, ExportTable ) ->
	ExportTable;

% Any kind of location, either direct or marker-based (they will be translated
% later, when generating back an AST):
%
ensure_function_exported( FunId, _ExportLocs=[ Loc | T ], ModuleInfo,
						  ExportTable ) ->

	% Here we have an immediate location:
	case ?table:lookup_entry( Loc, ExportTable ) of

		{ value, { Line, FunIds } } ->

			case lists:member( FunId, FunIds ) of

				true ->
					% Already registered, perfect as is, continues with the next
					% locations:
					%
					ensure_function_exported( FunId, T, ModuleInfo,
											  ExportTable );

				false ->

					% Adding it then:
					NewEntry = { Line, [ FunId | FunIds ] },

					NewExportTable =
						?table:add_entry( Loc, NewEntry, ExportTable ),

					ensure_function_exported( FunId, T, ModuleInfo,
											  NewExportTable )

			end;

		key_not_found ->
			% Not even a registered location:
			throw( { invalid_export_location, Loc, FunId } )

	end.



% Ensures that specified function is not exported at the specified location(s),
% in the specified function export table (un-export said function).
%
-spec ensure_function_not_exported( meta_utils:function_id(), [ location() ],
						function_export_table() ) -> function_export_table().
ensure_function_not_exported( _FunId, _ExportLocs=[], ExportTable ) ->
	ExportTable;

ensure_function_not_exported( FunId, _ExportLocs=[ Loc | T ], ExportTable ) ->

	case ?table:lookup_entry( Loc, ExportTable ) of

		{ value, { Line, FunIds } } ->

			% 0 or 1 reference expected, which is handled the same by:
			NewExportTable = case lists:delete( FunId, FunIds ) of

				[] ->
					?table:remove_entry( Loc, ExportTable );

				ShrunkFunIds ->
					?table:add_entry( Loc, { Line, ShrunkFunIds }, ExportTable )

			end,

			ensure_function_not_exported( FunId, T, NewExportTable );

		key_not_found ->
			throw( { inconsistent_export_location, Loc, FunId } )

	end.




% Returns a textual description of the specified located AST.
%
% Note: relies on text_utils.
%
-spec located_ast_to_string( located_ast() ) -> ustring().
located_ast_to_string( AST ) ->

	% Raw, not sorted on purpose:
	Strings = [ text_utils:format( "at ~ts: ~p",
					[ id_utils:sortable_id_to_string( Loc ), Form ] )
				|| { Loc, Form } <- AST ],

	text_utils:strings_to_string( Strings ).




% Processes the specified AST relative to a whole module, and returns the
% corresponding information gathered.
%
% Note: the extraction will probably fail (and stop any underlying parse
% transform) should the corresponding, specified code not be able to compile (as
% a rather precise linting is done).
%
-spec extract_module_info_from_ast( ast() ) -> module_info().
extract_module_info_from_ast( AST ) ->

	%ast_utils:display_debug( "Processing following AST:~n~p",
	%  [ AST ] ),

	%ast_utils:display_debug( "Processing AST..." ),

	%ast_utils:write_ast_to_file( AST, "original-extracted-ast.txt" ),

	% First we check whether the corresponding code compiles:

	% We could define specific compile options, yet they could be too
	% restrictive (more than the ones of the compiler) and moreover it would
	% force us to specify a filename.

	% We cannot simply count errors and warnings, as we have in each case a list
	% of per-file elements (a list of lists), in the order of the specified AST
	% (thus additionally a given file may happen multiple times); a count is not
	% useful here anyway.

	% Finally we have not real freedom in terms of output, as we prefer to
	% respect the native display format of the error messages so that tools (ex:
	% emacs, possible erlide and all) are still able to manage them.

	% Useless: would report pre-transform errors that would be solved after
	% transformation (ex: void() not existing)
	%pre_check_ast( AST ),

	ModuleInfo = ast_scan:scan( AST ),

	% Uncomment with care, as must ultimately depend *only* on non-bootstrapped
	% modules (like {meta,text}_utils) - this should be the case here:
	%
	%ast_utils:display_debug( "Resulting module information:~n~ts",
	%						 [ module_info_to_string( ModuleInfo ) ] ),

	case ModuleInfo#module_info.unhandled_forms of

		[] ->
			ok;

		UnhandledForms ->

			UnHandledStrings = [ text_utils:format( "~p", [ Form ] )
								 || { _Loc, Form } <- UnhandledForms ],

			ast_utils:display_warning( "~B forms have not been handled: ~ts",
				[ length( UnhandledForms ),
				  text_utils:strings_to_string( UnHandledStrings ) ] )

	end,

	% Additional linting, just after extraction:
	check_module_info( ModuleInfo ),

	ModuleInfo.




% Returns a new, blank instance of the module_info record, typically to be fed
% with an input AST afterwards.
%
-spec init_module_info() -> module_info().
init_module_info() ->

	EmptyTable = ?table:new(),

	#module_info{ compilation_options=EmptyTable,
				  parse_attributes=EmptyTable,
				  type_exports=EmptyTable,
				  types=EmptyTable,
				  records=EmptyTable,
				  function_imports=EmptyTable,
				  function_exports=EmptyTable,
				  functions=EmptyTable,
				  markers=EmptyTable }.



% Checks the correctness of specified module information.
-spec check_module_info( module_info() ) -> basic_utils:void().
check_module_info( #module_info{ module=undefined } ) ->
	ast_utils:raise_error( "no '-module' define found" );

check_module_info( #module_info{ last_line=undefined } ) ->
	ast_utils:raise_error( "no last line found" );


check_module_info( ModuleInfo=#module_info{
								 module={ ModuleName, _LocForm },
								 unhandled_forms=[] } ) ->
	%ast_utils:display_debug( "Checking AST." ),
	check_module_include( ModuleInfo, ModuleName ),
	check_module_types( ModuleInfo, ModuleName ),
	check_module_functions( ModuleInfo, ModuleName );

check_module_info( #module_info{ unhandled_forms=_UnhandledForms } ) ->

	%Forms = [ F || { _Loc, F } <- UnhandledForms ],

	%ast_utils:raise_error( [ unhandled_forms, UnhandledForms ] ).
	%trace_utils:warning_fmt( "Unhandled forms: ~p", [ UnhandledForms ] ),

	% A warning may have already been issued, we let these unexpected forms flow
	% through and be caught by the compiler:
	ok.



% Helper to check module includes.
check_module_include( #module_info{ includes=Includes,
									include_defs=IncludeDefs },
					  ModuleName ) ->

	Len = length( Includes ),

	case length( IncludeDefs ) of

		% Includes are filtered (ex: for duplicates):
		L when L < Len ->
			ast_utils:raise_usage_error( "mismatch regarding includes, "
				"having ~B includes (~p) and ~B include definitions (~p).",
				[ Len, Includes, L, IncludeDefs ], ModuleName );

		_ ->
			ok

	end.




% Helper to check module types.
check_module_types( #module_info{ types=Types }, Filename ) ->

	TypeInfos = ?table:enumerate( Types ),

	[ check_type( TypeId, TypeInfo, Filename )
	  || { TypeId, TypeInfo } <- TypeInfos ].



% Nothing to check for 'spec' or 'exported':
check_type( TypeId, _TypeInfo=#type_info{ line=Line, definition=[] },
			Filename ) ->
	ast_utils:raise_usage_error( "no definition found for type ~ts/~B.",
								 pair:to_list( TypeId ), Filename, Line );

check_type( _TypeId={ Name, Arity },
			_TypeInfo=#type_info{ line=Line, name=Name, variables=undefined },
			Filename ) ->
	ast_utils:raise_usage_error( "type ~ts/~B is exported yet not defined.",
				[ Name, Arity ], Filename, Line );

check_type( _TypeId={ Name, Arity },
			_TypeInfo=#type_info{ line=Line, name=Name,
								  variables=TypeVars },
			Filename ) ->

	case length( TypeVars ) of

		Arity ->
			ok;

		OtherArity ->
			ast_utils:raise_usage_error( "mismatch between the definition of "
				"type ~ts and its export; respective arities are ~B and ~B",
				[ Name, Arity, OtherArity ], Filename, Line )

	end;

check_type( TypeId, _TypeInfo=#type_info{ name=SecondName,
										  variables=TypeVars,
										  line=Line },
			Filename ) ->

	SecondArity = length( TypeVars ),

	ast_utils:raise_usage_error(
		"mismatch in terms of type definition, between ~ts/~B and ~ts/~B.",
		pair:to_list( TypeId ) ++ [ SecondName, SecondArity ],
		Filename, Line ).



% Helper to check module functions.
check_module_functions( #module_info{ functions=Functions }, ModuleName ) ->

	FunInfos = ?table:enumerate( Functions ),

	[ check_function( FunId, FunInfo, ModuleName )
	  || { FunId, FunInfo } <- FunInfos ].


% No definition, and neither with a spec nor exported, yet registered (strange):
check_function( FunId, _FunInfo=#function_info{ clauses=[],
												spec=undefined,
												exported=[] },
				ModuleName ) ->
	ast_utils:raise_usage_error( "no clause found for function ~ts/~B",
								 pair:to_list( FunId ), ModuleName );


% No definition, no spec, hence exported:
check_function( _FunId,
				_FunInfo=#function_info{ clauses=[], spec=undefined },
				_ModuleName ) ->

	% Silenced, as we prefer this error to be reported through the toolchain
	% itself, for a better integration in error handling:
	%
	%ast_utils:raise_usage_error( "function ~ts/~B exported yet not defined",
	%    pair:to_list( FunId ), ModuleName ] );
	ok;

% No definition, not exported, hence just a spec:
check_function( _FunId, _FunInfo=#function_info{ clauses=[], exported=[] },
				_ModuleName ) ->

	% Silenced, as we prefer this error to be reported through the toolchain
	% itself, for a better integration in error handling:

	%ast_utils:raise_usage_error( "function spec without definition for ~ts/~B",
	%                             pair:to_list( FunId ), ModuleName );

	%trace_utils:warning_fmt( "Function spec without a definition for ~ts/~B.",
	%						  pair:to_list( FunId ) );

	ok;

check_function( _FunId={ Name, Arity },
				_FunInfo=#function_info{ name=Name, arity=Arity },
				_ModuleName ) ->
	% Match:
	ok;

check_function( FunId, _FunInfo=#function_info{ name=SecondName,
												arity=SecondArity },
				ModuleName ) ->
	ast_utils:raise_usage_error(
	  "function definition mismatch for ~ts/~B vs ~ts/~B",
	  pair:to_list( FunId ) ++ [ { SecondName, SecondArity } ],
	  ModuleName ).



% Recomposes an AST from specified module information.
-spec recompose_ast_from_module_info( module_info() ) -> ast().
recompose_ast_from_module_info( #module_info{

			% Between parentheses: fields unused here, hence not bound.

			% Note: one should regularly check that all relevant fields of
			% module_info() are indeed read here, so that they are reinjected
			% as expected in the output AST.

			module={ _ModuleName, ModuleLocDef },

			% (compilation_options)
			compilation_option_defs=CompileOptLocDefs,

			parse_attributes=ParseAttributeTable,

			remote_spec_defs=RemoteSpecLocDefs,

			% (includes)
			include_defs=IncludeLocDefs,

			type_exports=TypeExportTable,

			types=TypeTable,

			records=RecordTable,

			% (function_imports)
			function_imports_defs=ImportLocDefs,

			function_exports=FunctionExportTable,

			% The main part of the AST:
			functions=FunctionTable,

			optional_callbacks_defs=OptCallbacksLocDefs,

			last_line=LastLineLocDef,

			markers=MarkerTable,

			errors=[],

			unhandled_forms=UnhandledLocForms } ) ->

	ParseAttributeLocDefs = [ LocForm
			   || { _Value, LocForm } <- ?table:values( ParseAttributeTable ) ],

	{ TypeExportLocDefs, TypeLocDefs } =
		ast_type:get_located_forms_for( TypeExportTable, TypeTable ),

	RecordLocDefs = ast_record:get_located_forms_for( RecordTable ),

	% Auto-exports functions if relevant:
	{ FunExportLocDefs, FunctionLocDefs } = ast_function:get_located_forms_for(
										FunctionExportTable, FunctionTable ),


	% We used to start from a sensible order so that inserted forms do not end
	% up in corner cases, yet all these definitions are located, so their order
	% does not really matter once we have only explicit locations.
	%
	UnorderedLocatedAST = [ ModuleLocDef
							| ParseAttributeLocDefs
							++ RemoteSpecLocDefs
							++ FunExportLocDefs
							++ IncludeLocDefs
							++ ImportLocDefs
							++ OptCallbacksLocDefs
							++ CompileOptLocDefs
							++ RecordLocDefs
							++ TypeExportLocDefs
							++ TypeLocDefs
							++ FunctionLocDefs
							++ [ LastLineLocDef | UnhandledLocForms ] ],


	%ast_utils:display_debug( "Unordered located AST:~n~p~n",
	%						 [ UnorderedLocatedAST ] ),

	OrderedAST = get_ordered_ast_from( UnorderedLocatedAST, MarkerTable ),

	%ast_utils:display_debug( "Recomposed AST:~n~p~n",
	%						 [ OrderedAST ] ),

	OrderedAST;


% If there was at least one error reported:
recompose_ast_from_module_info( #module_info{ errors=Errors } ) ->

	ErrorStrings = [ text_utils:term_to_string( E ) || E <- Errors ],

	trace_utils:error_fmt( "~B errors spotted in AST: ~ts",
		[ length( Errors ), text_utils:strings_to_string( ErrorStrings ) ] ),

	throw( { errors_in_ast, Errors } );


recompose_ast_from_module_info( Unexpected ) ->

	throw( { unexpected_term_as_module_info, Unexpected } ).




% Returns an (ordered, with no location information) AST from the specified
% unordered, located AST.
%
-spec get_ordered_ast_from( located_ast(), section_marker_table() ) -> ast().
get_ordered_ast_from( UnorderedLocatedAST, MarkerTable ) ->

	%ast_utils:display_debug( "Unordered, non-immediate located input AST:~n~p",
	%						 [ UnorderedLocatedAST ] ),

	% We replace any non-immediate location by an immediate one, and have the
	% whole sorted, then unlocated:
	%
	ReorderedAST = reorder_forms_in( UnorderedLocatedAST, MarkerTable ),

	% One of the most useful view of output:
	%ast_utils:display_debug( "Ordered, unlocated output AST:~n~p",
	%						 [ ReorderedAST ] ),

	ReorderedAST.



% Reorders specified located AST: returns an (unlocated) AST, whose form order
% has been determined thanks to an intermediary step fully based on immediate
% locations.
%
-spec reorder_forms_in( located_ast(), section_marker_table() ) -> ast().
reorder_forms_in( LocatedForms, MarkerTable ) ->

	%trace_utils:debug_fmt( "Reording following forms:~n~p~n"
	%					   "while there are ~ts", [ LocatedForms,
	%					   markers_to_string( MarkerTable ) ] ),

	% Separate immediate locations from others, which are all converted in a
	% locate_after form and then correctly inserted:
	%
	reorder_forms_in( LocatedForms, MarkerTable, _AccLoc=[], _AccLocAfter=[] ).


% locate_at are first transformed into locate_after locations, then all
% locate_after are properly inserted into the AST stream.
%
% (helper)
%
reorder_forms_in( _LocatedForms=[], _MarkerTable, AccLoc, AccLocAfter ) ->

	% Locations are the first elements of the pairs:
	LocIndex=1,

	% All forms inspected, we want in-order immediate located and located_at
	% forms:
	OrderedLocForms = lists:keysort( LocIndex, AccLoc ),

	OrderedLocAfter = lists:keysort( LocIndex, AccLocAfter ),

	insert_after_located_forms( OrderedLocAfter, OrderedLocForms );


% Manages locate_at information (transforms them in locate_after):
reorder_forms_in( _LocatedForms=[ { { locate_at, MarkerName }, Form } | T ],
				  MarkerTable, AccLoc, AccLocAfter ) ->

	MarkerLoc = ?table:get_value( MarkerName, MarkerTable ),

	NewAccLocAfter = [ { MarkerLoc, Form } | AccLocAfter ],

	reorder_forms_in( T, MarkerTable, AccLoc, NewAccLocAfter );


% Manages locate_after information (aggregates them):
reorder_forms_in( _LocatedForms=[ { { locate_after, Loc }, Form } | T ],
				  MarkerTable, AccLoc, AccLocAfter ) ->
	reorder_forms_in( T, MarkerTable, AccLoc, [ { Loc, Form } | AccLocAfter ] );


% E={Loc, Form} expected:
reorder_forms_in( _LocatedForms=[ E | T ], MarkerTable, AccLoc, AccLocAfter ) ->
	reorder_forms_in( T, MarkerTable, [ E | AccLoc ], AccLocAfter ).




% Merges the specified immediate and located-after ordered lists of forms:
% inserts the located-after forms in a right position in the immediate-located
% AST stream, and returns the resulting AST, once properly ordered and fully
% unlocated.
%
% (helper)
%
-spec insert_after_located_forms( located_ast(), located_ast() ) ->
										located_ast().
insert_after_located_forms( LocAfterForms, LocForms ) ->
	%trace_utils:debug_fmt( "Merging located-after forms: ~n~p~n  in:~n~p.",
	%					   [ LocAfterForms, LocForms ] ),
	insert_after_located_forms( LocAfterForms, LocForms,
								_CurrentLocInfo=undefined, _AccForms=[] ).



% No more located-after forms, and no current aggregation:
%
% (AccForms is a reversed, unlocated AST)
%
insert_after_located_forms( _LocAfter=[], LocForms, _CurrentLocInfo=undefined,
							AccForms ) ->

	% The remaining immediate-located forms, once unlocated:
	RemainingImmediateForms = [ F || { _Loc, F } <- LocForms ],

	lists:reverse( AccForms ) ++ RemainingImmediateForms;


% The case with LocAfter=[] and CurrentLocInfo defined is managed in the clause
% below that is commented as "First non-matching...".


% A new located-after form is found (wheras there is no current one):
insert_after_located_forms( _LocAfter=[ { TargetLoc, TargetForm } | T ],
							LocForms, _CurrentLocInfo=undefined, AccForms ) ->

	% Preparing the regrouping:
	NewLocInfo = { TargetLoc, [ TargetForm ] },

	insert_after_located_forms( T, LocForms, NewLocInfo, AccForms );


% Still matching the current location (TargetLoc), hence aggregating forms:
insert_after_located_forms( _LocAfter=[ { TargetLoc, TargetForm } | T ],
			LocForms, _CurrentLocInfo={ TargetLoc, CurrentForms }, AccForms ) ->

	NewLocInfo = { TargetLoc, [ TargetForm | CurrentForms ] },

	insert_after_located_forms( T, LocForms, NewLocInfo, AccForms );


% First non-matching after-location found, storing the information that has been
% aggregated beforehand.
%
% Here LocAfter does not match { TargetLoc, TargetForm } - so it is either [ {
% _AnotherLoc, _TargetForm } | _T ] or [].
%
insert_after_located_forms( LocAfter, LocForms,
				_CurrentLocInfo={ TargetLoc, CurrentForms }, AccForms ) ->

	% We remove the location information; once reversed, we will have, by
	% increasing locations, for a given location, the base form followed by all
	% associated located_after forms.

	% For example, here:
	%   TargetLoc=6
	%   CurrentForms=[ Fa, Fb ]
	%   LocForms = [ {2,F2}, {5,F5}, {6,F6}, {8,F8}, {9,F9} ]
	% Then, after split_at_location/2:
	%   RevUnlocPrefix = [ F5, F2 ]
	%   BaseForm = F6
	%   LocSuffix = [ {8,F8}, {9,F9} ]
	%
	% Then we can have:
	% NewAccLocForms = [ Fa, Fb ] ++ [ F6 | [ F5, F2 ] ] ++ AccForms

	% The target location may or may not point to an actual form:
	{ NewAccLocForms, LocSuffix } =
			case split_at_location( TargetLoc, LocForms ) of

		{ RevUnlocPrefix, _BaseForm=undefined, LocationSuffix } ->
		   { CurrentForms ++ RevUnlocPrefix ++ AccForms, LocationSuffix };

		{ RevUnlocPrefix, BaseForm, LocationSuffix } ->
			{ CurrentForms ++ [ BaseForm | RevUnlocPrefix ] ++ AccForms,
			  LocationSuffix }

	end,

	% Next step will process this new location:
	insert_after_located_forms( LocAfter, LocSuffix,
								_NewCurrentLocInfo=undefined, NewAccLocForms ).




% Returns {RevUnlocPrefix, BaseForm, LocSuffix} so that:
%
% InputLocForms = (reversed, located version of RevUnlocPrefix) ++ [
% {Loc,BaseForm} | LocSuffix ]
%
% The specified location is expected to be found in the specified (ordered) AST.
%
% (helper)
%
-spec split_at_location( location(), located_ast() ) ->
							{ located_ast(), form(), located_ast() }.
split_at_location( Loc, InputLocForms ) ->
	%trace_utils:debug_fmt( "Splitting at location ~p following forms:~n~p",
	%					   [ Loc, InputLocForms ] ),
	split_at_location( Loc, InputLocForms, _Acc=[] ).


% (helper)
split_at_location( Loc, _InputLocForms=[], _Acc ) ->
	throw( { base_location_not_found, Loc } );

% Found:
split_at_location( Loc, _LocForms=[ { Loc, BaseForm } | T ], Acc ) ->
	% Acc already in the right order:
	{ Acc, BaseForm, T };

% Skips base located forms until matching (Loc not reached yet):
split_at_location( Loc, _LocForms=[ { OtherLoc, Form } | T ], Acc )
  when OtherLoc < Loc ->
	split_at_location( Loc, T, [ Form | Acc ] );

% Here, implicitly, OtherLoc just went past Loc, so:
split_at_location( _Loc, LocForms, Acc ) ->
	% Acc already in the right order:
	{ Acc, _BaseForm=undefined, LocForms }.





% Writes specified module_info record into specified (text) file.
%
% Useful for example to determine faulty transformations.
%
-spec write_module_info_to_file( module_info(), file_utils:file_path() ) ->
										basic_utils:void().
write_module_info_to_file( ModuleInfo, FilePath ) ->

	% Note: we cannot actually use file_utils, which is not a prerequisite of
	% the 'Myriad' parse transform:

	% We overwrite any pre-existing file:
	{ ok, File } = file:open( FilePath, [ write, raw ] ),

	ok = file:write( File, module_info_to_string( ModuleInfo ) ),

	ok = file:close( File ).




% Returns the conventional location designating where forms can be be added just
% after the declaration of the module name, as an indirect location.
%
-spec get_default_module_location() -> location().
get_default_module_location() ->
	{ locate_at, module_marker }.


% Returns the conventional location designating where forms can be be added just
% after the declaration of the module name, as an immediate location.
%
-spec get_default_module_location( section_marker_table() ) -> location().
get_default_module_location( MarkerTable ) ->
	?table:get_value( module_marker, MarkerTable ).



% Returns the conventional location at which new types may be exported, as an
% indirect location.
%
-spec get_default_export_type_location() -> location().
get_default_export_type_location() ->
	{ locate_at, export_types_marker }.


% Returns the conventional location at which new types may be exported, as an
% immediate location.
%
-spec get_default_export_type_location( section_marker_table() ) -> location().
get_default_export_type_location( MarkerTable)  ->
	?table:get_value( export_types_marker, MarkerTable ).



% Returns the conventional location at which new functions may be exported, as
% an indirect location.
%
-spec get_default_export_function_location() -> location().
get_default_export_function_location() ->
	{ locate_at, export_functions_marker }.


% Returns the conventional location at which new functions may be exported, as
% an immediate location.
%
-spec get_default_export_function_location( section_marker_table() ) ->
													location().
get_default_export_function_location( MarkerTable ) ->
	?table:get_value( export_functions_marker, MarkerTable ).



% Returns the conventional location at which functions may be imported, as an
% indirect location.
%
-spec get_default_import_function_location() -> location().
get_default_import_function_location() ->
	{ locate_at, import_functions_marker }.


% Returns the conventional location at which functions may be imported, as an
% immediate location.
%
-spec get_default_import_function_location( section_marker_table() ) ->
													location().
get_default_import_function_location( MarkerTable ) ->
	?table:get_value( import_functions_marker, MarkerTable ).




% Returns the conventional location at which new records may be defined, as an
% indirect location.
%
-spec get_default_definition_record_location() -> location().
get_default_definition_record_location() ->
	{ locate_at, definition_records_marker }.


% Returns the conventional location at which new records may be defined, as an
% immediate location.
%
-spec get_default_definition_record_location( section_marker_table() ) ->
													location().
get_default_definition_record_location( MarkerTable ) ->
	?table:get_value( definition_records_marker, MarkerTable ).



% Returns the conventional location at which new types may be defined, as an
% indirect location.
%
-spec get_default_definition_type_location() -> location().
get_default_definition_type_location() ->
	{ locate_at, definition_types_marker }.


% Returns the conventional location at which new types may be defined, as an
% immediate location.
%
-spec get_default_definition_type_location( section_marker_table() ) ->
													location().
get_default_definition_type_location( MarkerTable ) ->
	?table:get_value( definition_types_marker, MarkerTable ).



% Returns the conventional location at which new functions may be defined, as an
% indirect location.
%
-spec get_default_definition_function_location() -> location().
get_default_definition_function_location() ->
	{ locate_at, definition_functions_marker }.


% Returns the conventional location at which new functions may be defined, as an
% immediate location.
%
-spec get_default_definition_function_location( section_marker_table() ) ->
														location().
get_default_definition_function_location( MarkerTable ) ->
	?table:get_value( definition_functions_marker, MarkerTable ).




% Returns a textual description of specified module information, not including
% forms, and based on a default indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec module_info_to_string( module_info() ) -> ustring().
module_info_to_string( ModuleInfo ) ->
	module_info_to_string( ModuleInfo, _DoIncludeForms=false ).


% Returns a textual description of specified module information, including forms
% if requested, and based on a default indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec module_info_to_string( module_info(), boolean() ) -> ustring().
module_info_to_string( ModuleInfo, DoIncludeForms ) ->
	module_info_to_string( ModuleInfo, DoIncludeForms, _IndentationLevel=0 ).


% Returns a textual description of specified module information, including forms
% if requested, and with specified indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec module_info_to_string( module_info(), boolean(), indentation_level() ) ->
									ustring().
module_info_to_string( #module_info{
						 module=ModuleEntry,
						 compilation_options=CompileTable,
						 compilation_option_defs=CompileOptDefs,
						 parse_attributes=ParseAttributeTable,
						 remote_spec_defs=RemoteSpecDefs,
						 includes=Includes,
						 include_defs=IncludeDefs,
						 type_exports=TypeExportTable,
						 types=TypeTable,
						 records=RecordTable,
						 function_imports=FunctionImportTable,
						 function_imports_defs=FunctionImportDefs,
						 function_exports=_FunctionExports,
						 functions=FunctionTable,
						 optional_callbacks_defs=OptCallbacksDefs,
						 last_line=LastLineLocDef,
						 markers=MarkerTable,
						 errors=Errors,
						 unhandled_forms=UnhandledForms },
					   DoIncludeForms,
					   IndentationLevel ) ->


	% For this textual description, we mostly rely on the higher-level
	% information available.

	% As the next strings will be collected at a level of their own:
	NextIndentationLevel = IndentationLevel + 1,

	% Information gathered in the order of the fields:

	ModuleString = module_entry_to_string( ModuleEntry, DoIncludeForms ),

	Infos = [ compilation_options_to_string( CompileTable, CompileOptDefs,
									DoIncludeForms, NextIndentationLevel ),

			  optional_callbacks_to_string( OptCallbacksDefs, DoIncludeForms,
											NextIndentationLevel ),

			  parse_attribute_table_to_string( ParseAttributeTable,
								DoIncludeForms, NextIndentationLevel ),

			  remote_spec_definitions_to_string( RemoteSpecDefs, DoIncludeForms,
												 NextIndentationLevel ),

			  includes_to_string( Includes, IncludeDefs, DoIncludeForms,
								  NextIndentationLevel ),

			  % No form to manage:
			  type_exports_to_string( TypeExportTable, NextIndentationLevel ),

			  types_to_string( TypeTable, DoIncludeForms,
							   NextIndentationLevel ),

			  records_to_string( RecordTable, NextIndentationLevel ),

			  function_imports_to_string( FunctionImportTable,
				  FunctionImportDefs, DoIncludeForms, NextIndentationLevel ),

			  functions_to_string( FunctionTable, DoIncludeForms,
								   NextIndentationLevel ),

			  last_line_to_string( LastLineLocDef ),

			  markers_to_string( MarkerTable, NextIndentationLevel ),

			  errors_to_string( Errors, NextIndentationLevel ),

			  unhandled_forms_to_string( UnhandledForms, DoIncludeForms,
										 NextIndentationLevel ) ],

	text_utils:format( "Information about module ~ts: ~ts", [ ModuleString,
			text_utils:strings_to_string( Infos, IndentationLevel ) ] ).



% Returns a textual representation of the specified forms, if requested, and
% using specified indentation level.
%
% (helper used by the various *_to_string functions)
%
-spec forms_to_string( [ located_form() ], boolean(), indentation_level() ) ->
							 ustring().
forms_to_string( _LocForms, _DoIncludeForms=false, _IndentationLevel ) ->
	% No representation wanted here:
	"";

% As a result, DoIncludeForms known to be true below:
forms_to_string( _LocForms=[], _DoIncludeForms, _IndentationLevel ) ->
	"(there are no corresponding forms)";


forms_to_string( LocForms, _DoIncludeForms, IndentationLevel ) ->

	FormStrings = [ text_utils:format( "~p", [ F ] )
					|| { _Loc, F } <- LocForms ],

	FormString = text_utils:strings_to_string( FormStrings,
											   IndentationLevel ),

	text_utils:format( "~nThe corresponding forms are: ~ts", [ FormString ] ).



% Returns a textual representation of the specified location.
-spec location_to_string( location() ) -> ustring().
location_to_string( { locate_at, MarkerName } ) ->
	text_utils:format( "at marker ~ts", [ MarkerName ] );

location_to_string( { locate_after, Location } ) ->
	text_utils:format( "after ~ts",
					   [ id_utils:sortable_id_to_string( Location ) ] );

location_to_string( Location ) ->
	id_utils:sortable_id_to_string( Location ).



% Returns a textual representation of the name of the module corresponding to
% specified entry, possibly with forms.
%
-spec module_entry_to_string( module_entry(), boolean() ) -> ustring().
module_entry_to_string( _ModuleEntry=undefined, _DoIncludeForms ) ->
	"(unnamed module)";

module_entry_to_string( _ModuleEntry={ ThisModName, _ModuleLocDef },
						_DoIncludeForms=false ) ->
	text_utils:atom_to_string( ThisModName );

module_entry_to_string( _ModuleEntry={ ThisModName,
									   _ModuleLocDef={ _Loc, Form } },
						_DoIncludeForms=true ) ->
	text_utils:format( "~ts (represented as form '~p')",
					   [ ThisModName, Form ] ).



% Returns a textual representation of compilation options, based on a default
% indentation level.
%
-spec compilation_options_to_string( compile_option_table(), [ located_form() ],
									 boolean() ) -> ustring().
compilation_options_to_string( CompileTable, CompileOptDefs, DoIncludeForms ) ->
	compilation_options_to_string( CompileTable, CompileOptDefs, DoIncludeForms,
								   _IndentationLevel=0 ).



% Returns a textual representation of compilation options, with specified
% indentation level.
%
-spec compilation_options_to_string( compile_option_table(), [ located_form() ],
								 boolean(), indentation_level() ) -> ustring().
compilation_options_to_string( CompileTable, CompileOptDefs, DoIncludeForms,
							   IndentationLevel ) ->

	case ?table:enumerate( CompileTable ) of

		[] ->
			"no compile option defined";


		CompileOpts ->

			CompStrings = [ text_utils:format( "for option '~ts': ~p",
											   [ OptName, OptValue ] )
							|| { OptName, OptValue } <- CompileOpts ],

			OptString = text_utils:format( "~B compile option(s) defined: ~ts",
							   [ length( CompileOpts ),
								 text_utils:strings_to_string( CompStrings,
												   IndentationLevel ) ] ),

			OptString ++ forms_to_string( CompileOptDefs, DoIncludeForms,
										  IndentationLevel + 1 )

	end.



% Returns a textual representation of the specified optional callbacks, based on
% a default indentation level.
%
-spec optional_callbacks_to_string( [ located_form() ], boolean(),
									indentation_level() ) -> ustring().
optional_callbacks_to_string( _OptCallbacksDefs=[], _DoIncludeForms,
							  _IndentationLevel ) ->
	"no optional callback defined";

optional_callbacks_to_string( OptCallbacksDefs, _DoIncludeForms=false,
							 _IndentationLevel ) ->
	text_utils:format( "~B lists of optional callbacks defined",
					   [ length( OptCallbacksDefs ) ] );

optional_callbacks_to_string( OptCallbacksDefs, _DoIncludeForms=true,
							  IndentationLevel ) ->
	optional_callbacks_to_string( OptCallbacksDefs, _DoIncForms=false,
								  IndentationLevel )
		++ forms_to_string( OptCallbacksDefs, _DoIncludeForms=true,
							IndentationLevel + 1 ).



% Returns a textual representation of the specified parse-attribute table, based
% on a default indentation level.
%
-spec parse_attribute_table_to_string( attribute_table(), boolean() ) ->
												ustring().
parse_attribute_table_to_string( ParseAttributeTable, DoIncludeForms ) ->
	parse_attribute_table_to_string( ParseAttributeTable, DoIncludeForms,
									 _IndentationLevel=0 ).



% Returns a textual representation of the specified parse-attribute table, with
% specified indentation level.
%
-spec parse_attribute_table_to_string( attribute_table(), boolean(),
									   indentation_level() ) -> ustring().
parse_attribute_table_to_string( ParseAttributeTable, DoIncludeForms,
								 IndentationLevel ) ->

	% Parse attributes are like: '-foo( bar ).':
	case ?table:enumerate( ParseAttributeTable ) of

		[] ->
			"no parse attribute defined";

		ParseAttributes ->
			ParseAttrString = text_utils:strings_to_sorted_string( [
				begin

					AttrValues = [ V || { V, _LocForm } <- AttrEntries ],

					text_utils:format( "attribute '~ts', set to: ~ts",
						[ AttrName,
						  text_utils:terms_to_listed_string( AttrValues ) ] )

				end || { AttrName, AttrEntries } <- ParseAttributes ] ),

			BaseString = text_utils:format(
						   "~B parse attribute(s) defined: ~ts",
						   [ length( ParseAttributes ), ParseAttrString ] ),

			% To avoid collecting forms uselessly:
			case DoIncludeForms of

				true ->
					Forms =
						[ F || { _AName, { _AValue, F } } <- ParseAttributes ],

					BaseString ++ forms_to_string( Forms, _DoIncludeForms=true,
												   IndentationLevel + 1 );

				false ->
					BaseString

			end

	end.




% Returns a textual representation of the specified definitions of remote
% specifications, with specified indentation level.
%
-spec remote_spec_definitions_to_string( [ located_form() ], boolean(),
										 indentation_level() ) -> ustring().
remote_spec_definitions_to_string( _RemoteSpecDefs=[], _DoIncludeForms,
								   _IndentationLevel ) ->
	"no remote spec definition";

remote_spec_definitions_to_string( RemoteSpecDefs, DoIncludeForms,
								   IndentationLevel ) ->

	BaseString = text_utils:format( "~B remote spec definition(s)",
									[ length( RemoteSpecDefs ) ] ),

	BaseString ++ forms_to_string( RemoteSpecDefs, DoIncludeForms,
								   IndentationLevel + 1 ).



% Returns a textual representation of the specified includes, based on a default
% indentation level.
%
-spec includes_to_string( [ file_utils:bin_file_path() ], [ located_form() ],
						  boolean() ) -> ustring().
includes_to_string( Includes, IncludeDefs, DoIncludeForms ) ->
	includes_to_string( Includes, IncludeDefs, DoIncludeForms,
						_IndentationLevel=0 ).


% Returns a textual representation of the specified includes, with specified
% indentation level.
%
-spec includes_to_string( [ file_utils:bin_file_path() ],
		  [ located_form() ], boolean(),
		  indentation_level() ) -> ustring().
includes_to_string( _Includes=[], _IncludeDefs, _DoIncludeForms,
					_IndentationLevel ) ->
	"no file included";

includes_to_string( Includes, IncludeDefs, DoIncludeForms,
					IndentationLevel ) ->

	IncludeString = text_utils:strings_to_sorted_string(
				[ text_utils:format( "~ts", [ Inc ] ) || Inc <- Includes ],
				IndentationLevel ),

	% Possibly with duplicates:
	text_utils:format( "~B file include(s): ~ts",
					   [ length( Includes ), IncludeString ] )
		++ forms_to_string( IncludeDefs, DoIncludeForms, IndentationLevel + 1 ).



% Returns a textual representation of the specified type exports, based on a
% default indentation level.
%
-spec type_exports_to_string( type_export_table(), boolean() ) -> ustring().
type_exports_to_string( TypeExportTable, DoIncludeForms ) ->
	type_exports_to_string( TypeExportTable, DoIncludeForms,
							_IndentationLevel=0 ).


% Returns a textual representation of the specified type exports, with specified
% indentation level.
%
-spec type_exports_to_string( type_export_table(), boolean(),
							  indentation_level() ) -> ustring().
type_exports_to_string( TypeExportTable, _DoIncludeForms, IndentationLevel ) ->

	case ?table:enumerate( TypeExportTable ) of

		[] ->
			"no type export defined";

		TypeExportEntries ->

			% The indentation is 2 below, to account for the case where there
			% are multiple exports; should there be only one, we have to live
			% with an extra indentation level then:
			%
			TypeExpString = text_utils:strings_to_sorted_string(
				[ text_utils:format( "at line #~B, export of: ~ts",
									 [ Line, text_utils:strings_to_string(
											   [ type_id_to_string( TypeId )
												 || TypeId <- TypeIds ],
											   IndentationLevel + 2 ) ] )
				  || { _Loc, { Line, TypeIds } } <- TypeExportEntries ],
				IndentationLevel + 1 ),

			% No form to represent, as none stored:
			text_utils:format( "~B type export(s) defined: ~ts",
							   [ length( TypeExportEntries ), TypeExpString ] )

	end.



% Returns a textual representation of the specified type table, with specified
% indentation level.
%
-spec types_to_string( type_table(), boolean(), indentation_level() ) ->
								ustring().
types_to_string( TypeTable, DoIncludeForms, IndentationLevel ) ->

	case ?table:values( TypeTable ) of

		[] ->
			"no type definition defined";

		TypeInfos ->

			TypeStrings = [ type_info_to_string( TypeInfo, DoIncludeForms,
												 IndentationLevel )
							|| TypeInfo <- TypeInfos ],

			text_utils:format( "~B type definition(s) specified: ~ts",
				[ length( TypeInfos ),
				  text_utils:strings_to_string( TypeStrings,
												IndentationLevel ) ] )

	end.



% Returns a textual representation of the specified records, based on a default
% indentation level.
%
-spec records_to_string( record_table() ) -> ustring().
records_to_string( RecordTable ) ->
	records_to_string( RecordTable, _IndentationLevel=0 ).



% Returns a textual representation of the specified records, with specified
% indentation level.
%
% Note: no available form to display.
%
-spec records_to_string( record_table(), indentation_level() ) -> ustring().
records_to_string( RecordTable, IndentationLevel ) ->

	case ?table:enumerate( RecordTable ) of

		[] ->
			"no record defined";

		RecordEntries ->
			RecordString = text_utils:strings_to_sorted_string( [

				begin

					FieldStrings = fields_to_strings( FieldTable ),

					% Indentation level 2, supposing we have more than one field
					% generally:
					%
					FieldString = text_utils:strings_to_enumerated_string(
									FieldStrings, IndentationLevel + 2 ),

					text_utils:format( "record '~ts' having ~B fields: ~ts",
						[ RecordName, length( FieldStrings ), FieldString ] )


				end || { RecordName, { FieldTable, _NextLocation, _Line } }
						   <- RecordEntries ],
				IndentationLevel ),

			text_utils:format( "~B record(s) defined: ~ts",
							   [ length( RecordEntries ), RecordString ] )

	end.



% Returns a textual representation of the specified function imports, with
% specified indentation level.
%
-spec function_imports_to_string( function_import_table(), [ located_form() ],
								  boolean(), indentation_level() ) -> ustring().
function_imports_to_string( FunctionImportTable, FunctionImportDefs,
							DoIncludeForms, IndentationLevel ) ->

	case ?table:enumerate( FunctionImportTable ) of

		[] ->
			"no function imported";

		ImportPairs ->

			ModuleString = text_utils:strings_to_sorted_string( [

				begin

					IdStrings = [ function_id_to_string( FunId )
								  || FunId <- FunIds ],

					text_utils:format( "from module '~ts': ~ts", [ ModuleName,
						text_utils:strings_to_enumerated_string( IdStrings ) ] )


				end || { ModuleName, FunIds } <- ImportPairs ],
				IndentationLevel ),

			text_utils:format( "function imports from ~B modules: ~ts",
							   [ length( ImportPairs ), ModuleString ] )
				++ forms_to_string( FunctionImportDefs, DoIncludeForms,
									IndentationLevel + 1 )

	end.



% Returns a textual representation of the specified functions, with specified
% indentation level.
%
-spec functions_to_string( function_table(), boolean(), indentation_level() ) ->
								 ustring().
functions_to_string( FunctionTable, DoIncludeForms, IndentationLevel ) ->

	case ?table:values( FunctionTable ) of

		[] ->
			"no function information";

		FunInfos ->

			FunctionStrings = [
				function_info_to_string( FunInfo, DoIncludeForms )
					|| FunInfo <- FunInfos ],

			case FunctionStrings of

				[] ->
					"no function defined";

				_ ->
					text_utils:format( "~B function(s) defined: ~ts",
						[ length( FunctionStrings ),
						  text_utils:strings_to_sorted_string( FunctionStrings,
											IndentationLevel ) ] )

			 end

	end.



% Returns a textual representation of a module last line / line count.
-spec last_line_to_string( basic_utils:maybe( located_form() ) ) -> ustring().
last_line_to_string( _LastLine=undefined ) ->
	"unknown line count";

last_line_to_string( _LastLine={ _Loc, { eof, Count } } ) ->
	text_utils:format( "~B lines of source code", [ Count ] ).



% Returns a textual representation of the known section markers.
-spec markers_to_string( section_marker_table() ) -> ustring().
markers_to_string( MarkerTable ) ->
	markers_to_string( MarkerTable, _IndentationLevel=0 ).



% Returns a textual representation of the known section markers.
-spec markers_to_string( section_marker_table(), indentation_level() ) ->
							   ustring().
markers_to_string( MarkerTable, IndentationLevel ) ->


	case ?table:enumerate( MarkerTable ) of

		[] ->
			"no known section marker";

		MarkPairs ->

			SortedMarkPairs = lists:keysort( _Index=2, MarkPairs ),

			text_utils:format( "~B known section marker(s), sorted by "
				"increasing locations: ~ts",
				[ length( MarkPairs ), text_utils:strings_to_string(
			[ text_utils:format( "marker '~ts' pointing to ~ts",
						 [ Marker, id_utils:sortable_id_to_string( Loc ) ] )
			  || { Marker, Loc } <- SortedMarkPairs ],
								   IndentationLevel ) ] )

	end.



% Returns a textual representation of specified errors.
-spec errors_to_string( [ error() ], indentation_level() ) -> ustring().
errors_to_string( _Errors=[], _IndentationLevel ) ->
	"no error to report";

errors_to_string( Errors, IndentationLevel ) ->
	text_utils:format( "~B error(s) to report: ~ts",
		[ length( Errors ), text_utils:strings_to_string(
			[ text_utils:format( "in ~ts, line ~B: ~p",
								 [ Filename, Line, Reason ] )
			  || { Filename, Line, Reason } <- Errors ],
						   IndentationLevel ) ] ).



% Returns a textual representation of specified unhandled forms.
-spec unhandled_forms_to_string( [ located_form() ], boolean(),
								 indentation_level() ) -> ustring().
unhandled_forms_to_string( _UnhandledForms=[], _DoIncludeForms=false,
						   _IndentationLevel ) ->
	"no unhandled form to report";

unhandled_forms_to_string( UnhandledForms, _DoIncludeForms=false,
						   _IndentationLevel ) ->
	text_utils:format( "~B unhandled form(s)", [ length( UnhandledForms ) ] );

unhandled_forms_to_string( UnhandledForms, _DoIncludeForms=true,
						   IndentationLevel ) ->
	text_utils:format( "~ts: ~ts", [
		unhandled_forms_to_string( UnhandledForms, _DoIncludeForms=false,
								   IndentationLevel ),
		text_utils:strings_to_string( [ text_utils:format( "~p", [ F ] )
										|| { _Loc, F } <- UnhandledForms ],
									  IndentationLevel + 1 ) ] ).





% Returns a list of textual representations for each of the record fields in
% specified table.
%
-spec fields_to_strings( field_table() ) -> [ ustring() ].
fields_to_strings( FieldTable ) ->

	[ field_to_string( FieldName, FieldType, DefaultValue )
	  || { FieldName, { FieldType, DefaultValue, _FirstLine, _SecondLine } }
			 <- FieldTable ].



% Returns a textual representation of specified record field.
%
% (helper)
%
field_to_string( FieldName, FieldType, DefaultValue ) ->

	TypeString = case FieldType of

		undefined ->
			"no type";

		_ ->
			text_utils:format( "following type '~p'", [ FieldType ] )

	end,

	DefaultValueString = case DefaultValue of

		undefined ->
			"no default value";

		_ ->
			text_utils:format( "following default value '~p'",
							   [ DefaultValue ] )

	end,

	text_utils:format( "field '~ts' with ~ts and ~ts defined",
					   [ FieldName, TypeString, DefaultValueString ] ).



% Returns a textual description of the specified function identifier.
-spec function_id_to_string( function_id() ) -> ustring().
function_id_to_string( { FunctionName, FunctionArity } ) ->
	text_utils:format( "~ts/~B", [ FunctionName, FunctionArity ] ).



% Returns a textual description of the specified function information, not
% including its forms (clauses), with a default indentation level.
%
-spec function_info_to_string( function_info() ) -> ustring().
function_info_to_string( FunctionInfo ) ->
	function_info_to_string( FunctionInfo, _DoIncludeForms=false ).



% Returns a textual description of the specified function information, including
% its forms (clauses) if specified, with a default indentation level.
%
-spec function_info_to_string( function_info(), boolean() ) -> ustring().
function_info_to_string( FunctionInfo, DoIncludeForms ) ->
	function_info_to_string( FunctionInfo, DoIncludeForms,
							 _IndentationLevel=0 ).



% Returns a textual description of the specified function information.
-spec function_info_to_string( function_info(), boolean(),
							   indentation_level() ) -> ustring().
function_info_to_string( #function_info{ name=Name,
										 arity=Arity,
										 location=_Location,
										 line=Line,
										 clauses=Clauses,
										 spec=LocatedSpec,
										 callback=IsCallback,
										 exported=Exported },
						 DoIncludeForms,
						 IndentationLevel ) ->

	ExportString = case Exported of

		[] ->
			%"local";
			"not exported";

		ExportLocs ->
			Strings = [ location_to_string( L ) || L <- ExportLocs ],

			text_utils:format( "exported in following ~B location(s): ~ts",
				  [ length( ExportLocs ),
					text_utils:strings_to_string( Strings ) ] )

	end,

	DefString = case Line of

		undefined ->
			text_utils:format( "with ~B clause(s) defined",
							   [ length( Clauses ) ] );

		_ ->
			text_utils:format( "defined from line #~B, with "
			   "~B clause(s) specified", [ Line, length( Clauses ) ] )

	end,

	SpecString = case LocatedSpec of

		undefined ->
			"no type specification";

		_ ->
			case IsCallback of

				true ->
					"a callback type specification";

				false ->
					"a (standard) type specification"

			end

	end,

	BaseString = text_utils:format( "function ~ts/~B, ~ts, ~ts and ~ts",
						[ Name, Arity, ExportString, DefString, SpecString ] ),

	case DoIncludeForms of

		true ->
			text_utils:format( "~ts~n~ts",
				[ BaseString, ast_function:clauses_to_string( Clauses,
												  IndentationLevel + 1 ) ] );

		false ->
			BaseString

	end.



% Ensures that specified type is exported at the specified location(s).
-spec ensure_type_exported( type_id(), [ location() ], module_info(),
							type_export_table() ) -> type_export_table().
ensure_type_exported( _TypeId, _ExportLocs=[], _ModuleInfo, ExportTable ) ->
	ExportTable;

ensure_type_exported( TypeId, _ExportLocs=[ _Loc=auto_locate_after | T ],
					  ModuleInfo, ExportTable ) ->

	% When a type export is to be auto-located, we attach it just after the
	% module definition, to avoid possibly placing an export after a type
	% definition:

	ModuleLoc = case ModuleInfo#module_info.module of

		{ _ModuleName, _ModuleLocForm=undefined } ->
			throw( { auto_locate_after_whereas_no_module_defined, TypeId } );

		{ _ModuleName, { MLoc, _Form } } ->
			MLoc

	end,

	TypeExportLoc = id_utils:get_higher_next_depth_sortable_id( ModuleLoc ),

	% This location may have already been used, thus:
	case ?table:lookup_entry( TypeExportLoc, ExportTable ) of

		{ value, { Line, TypeIds } } ->

			case lists:member( TypeId, TypeIds ) of

				true ->
					% Already registered, perfect as is, continues with the next
					% locations:
					%
					ensure_type_exported( TypeId, T, ModuleInfo, ExportTable );

				false ->
					% Adding it then:
					NewEntry = { Line, [ TypeId | TypeIds ] },
					NewExportTable = ?table:add_entry( TypeExportLoc, NewEntry,
													   ExportTable ),
					ensure_type_exported( TypeId, T, ModuleInfo,
										  NewExportTable )

			end;


		key_not_found ->
			% We create a new location entry then:
			NewEntry = { _DefaultLine=0, [ TypeId ] },

			NewExportTable = ?table:add_entry( TypeExportLoc, NewEntry,
											  ExportTable ),

			ensure_type_exported( TypeId, T, ModuleInfo, NewExportTable )

	end;


ensure_type_exported( TypeId, _ExportLocs=[ Loc | T ], ModuleInfo,
					  ExportTable ) ->

	% Here we have an immediate location:
	case ?table:lookup_entry( Loc, ExportTable ) of

		{ value, { Line, TypeIds } } ->

			case lists:member( TypeId, TypeIds ) of

				true ->
					% Already registered, perfect as is, continues with the next
					% locations:
					ensure_type_exported( TypeId, T, ModuleInfo, ExportTable );

				false ->
					% Adding it then:
					NewEntry = { Line, [ TypeId | TypeIds ] },
					NewExportTable =
						?table:add_entry( Loc, NewEntry, ExportTable ),
					ensure_type_exported( TypeId, T, ModuleInfo,
										  NewExportTable )

			end;


		key_not_found ->
			% Not even a registered location:
			throw( { invalid_export_location, Loc, TypeId } )

	end.



% Ensures that specified type is not exported at the specified location(s).
-spec ensure_type_not_exported( type_id(), [ location() ],
								type_export_table() ) -> type_export_table().
ensure_type_not_exported( _TypeId, _ExportLocs=[], ExportTable ) ->
	ExportTable;

ensure_type_not_exported( TypeId, _ExportLocs=[ Loc | T ], ExportTable ) ->

	case ?table:lookup_entry( Loc, ExportTable ) of

		{ value, { Line, TypeIds } } ->

			% 0 or 1 reference expected, which is handled the same by:
			NewExportTable = case lists:delete( TypeId, TypeIds ) of

				[] ->
					?table:remove_entry( Loc, ExportTable );

				ShrunkTypeIds ->
					?table:add_entry( Loc, { Line, ShrunkTypeIds },
									 ExportTable )

			end,

			ensure_type_not_exported( TypeId, T, NewExportTable );

		key_not_found ->
			throw( { inconsistent_export_location, Loc, TypeId } )

	end.



% Returns a textual description of the specified type identifier.
-spec type_id_to_string( type_id() ) -> ustring().
type_id_to_string( { TypeName, TypeArity } ) ->
	text_utils:format( "~ts/~B", [ TypeName, TypeArity ] ).



% Returns a textual description of the specified type information.
-spec type_info_to_string( type_info() ) -> ustring().
type_info_to_string( TypeInfo ) ->
	type_info_to_string( TypeInfo, _DoIncludeForms=false,
						 _IndentationLevel=0 ).



% Returns a textual description of the specified type information.
-spec type_info_to_string( type_info(), boolean(), indentation_level() ) ->
								 ustring().
type_info_to_string( #type_info{ name=TypeName,
								 variables=TypeVariables,
								 opaque=IsOpaque,
								 location=_Location,
								 line=_Line,
								 definition=Definition,
								 exported=Exported },
					 DoIncludeForms,
					 _IndentationLevel ) ->

	ExportString = case Exported of

		[] ->
			"local";

		ExportLocs ->
			text_utils:format( "exported in ~ts",
					   [ id_utils:sortable_ids_to_string( ExportLocs ) ] )

	end,

	% In theory, we expect opaque types to be exported ones, yet we prefer to
	% handle all possible cases:
	%
	OpaqueString = case IsOpaque of

		true ->
			"opaque";

		false ->
			"non-opaque"

	end,

	Arity = length( TypeVariables ),

	TypeIdString = text_utils:format( "for type ~ts: ",
						[ type_id_to_string( { TypeName, Arity } ) ] ),

	BaseStrings = [ OpaqueString, ExportString ],

	AllStrings = case DoIncludeForms of

		true ->
			FormString = text_utils:format( "defined by: ~p", [ Definition ] ),
			list_utils:append_at_end( FormString, BaseStrings );

		false ->
			BaseStrings

	end,

	TypeIdString ++ text_utils:strings_to_listed_string( AllStrings ).



% Interprets the specified compilation options, that were typically specified
% through the command-line.
%
-spec interpret_options( [ compile_option_entry() ], module_info() ) ->
								module_info().
interpret_options( OptionList,
				   ModuleInfo=#module_info{
								 compilation_options=OptionTable } ) ->

	NewOptionTable = scan_options( OptionList, OptionTable ),

	%ast_utils:display_debug( "Scanned compilation options:~n~ts",
	%						  [ ?table:to_string( NewOptionTable ) ] ),

	ModuleInfo#module_info{ compilation_options=NewOptionTable }.



% Note that values may accumulate for various options, like for a 'd' key to
% which the following value could be associated: [ [ {my_second_test_token,200},
% {my_second_test_token,201} ].


% (helper)
scan_options( _OptionList=[], OptionTable ) ->
	OptionTable;

% Ex: {d,myriad_debug_mode}
scan_options( _OptionList=[ { Name, Value } | T ], OptionTable ) ->
	NewOptionTable = ?table:append_to_entry( _K=Name, Value, OptionTable ),
	scan_options( T, NewOptionTable );

% Ex: {d,my_second_test_token,200}
scan_options( _OptionList=[ { Name, BaseValue, OtherValue } | T ],
			  OptionTable ) ->
	NewOptionTable = ?table:append_to_entry( _K=Name, { BaseValue, OtherValue },
										   OptionTable ),
	scan_options( T, NewOptionTable );

% Ex: report_errors
scan_options( _OptionList=[ Name | T ], OptionTable ) when is_atom( Name ) ->

	% No clash wanted, throws an exception is the same key appears more than
	% once:
	%
	NewOptionTable = ?table:add_new_entry( _K=Name, _V=undefined, OptionTable ),

	scan_options( T, NewOptionTable );

scan_options( _OptionList=[ Unexpected | _T ], _OptionTable ) ->
	throw( { unexpected_compilation_option, Unexpected } ).
