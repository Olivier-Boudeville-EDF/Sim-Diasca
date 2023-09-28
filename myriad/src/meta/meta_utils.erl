% Copyright (C) 2014-2023 Olivier Boudeville
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
% Creation date: Friday, December 19, 2014.


% @doc Gathering of various higher-level, convenient <b>meta-related
% facilities</b>, notably regarding metaprogramming, types and parse transforms.
%
% See meta_utils_test.erl for the corresponding test, and ast_info.erl for the
% more basic services used by this module.
%
% Note that this module is a prerequisite of at least most of our parse
% transforms, hence it must be bootstrapped *before* they are built, and cannot
% use them.
%
% So, to compile it, just go to the root of this layer and execute for example
% 'make all'.
%
% To determine the other bootstrapped modules (i.e. the subset of our modules
% that this module can use), see the BOOTSTRAP_MODULES variable in
% GNUmakevars.inc.
%
% See also: the type_utils module, about the management of datatypes themselves,
% and the ast* modules for lower-level operations.
%
-module(meta_utils).



% For table macro, etc.:
-include("meta_utils.hrl").


% For *_info records:
-include("ast_info.hrl").


% For ast_transforms record:
-include("ast_transform.hrl").



% Key implementation notes:
%
% - again: any exported function meant to be used by parse transforms shall rely
% exclusively (through all its code paths) on bootstrapped modules, as listed in
% the BOOTSTRAP_MODULES variable of GNUmakevars.inc
%
% - see type_utils about how to handle datatypes




% Implementation notes about parse transforms:

% Here are some resources to better understand parse transforms (PT, here):
%
% - generic information about PT: in http://www.erlang-factory.com/ :
% upload/presentations/521/yrashk_parse_transformations_sf12.pdf
%
% - Abstract Format: http://www.erlang.org/doc/apps/erts/absform.html (full spec
% of the AST format)
%
% - http://chlorophil.blogspot.fr/2007/04/erlang-macro-processor-v1-part-i.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-ii.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-iii.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-iv.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-v.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-vi.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-vii.html


% We consider here that an AST is an ordered list of forms.
%
% We often use located counterparts of the standard elements (e.g. forms, ASTs)
% so that we can recreate and modify the order of (possibly transformed, added
% or removed) forms in an AST (that order matters, notably for its compilation),
% knowing that the embedded source-level line numbers are considerably less
% tractable and refer to another view onto the program at hand (and that the
% original forms are aggregated internally on a per-category basis rather than
% on a source-level one).
%
% See the definition of the location/0 type for further information.


% Standard modules of interest:
%
% - erl_scan ('The Erlang Token Scanner'): functions for tokenizing characters
% into Erlang tokens
%
% - epp ('An Erlang Code Preprocessor'): functions which are used by compile to
% preprocess macros and include files before the actual parsing
%
% - erl_parse ('The Erlang Parser'): basic Erlang parser
%
% - erl_eval ('The Erlang Meta Interpreter'): interpreter for Erlang
% expressions, in the abstract syntax
%
% - erl_pp ('The Erlang Pretty Printer'): to display abstract forms
%
% - erl_lint ('The Erlang Code Linter'): to check Erlang code for illegal
% syntax, bugs, unrecommended coding practices, etc.
%
% - compile ('The Erlang Compiler'): interface to the standard Erlang compiler

% Example of PT: http://www.erlang.org/doc/man/erl_id_trans.html


% Third-party libraries of interest:
% - https://github.com/uwiger/parse_trans
% - https://github.com/uwiger/toker


% Useful information about how to convert source code into actual code: on
% http://stackoverflow.com/questions/,
% 2160660/how-to-compile-erlang-code-loaded-into-a-string


% Use -P to see the code generated by a parse-transform; e.g. 'erlc -P' or in
% the shell as 'c("X.erl", ['P'])'.


% ast_utils:string_to_form/1 is useful to easily obtain an AST from a source
% code (as a string).
%
% For example:
%   ast_utils:string_to_form( "f(X) -> throw({hello,X})." ).
% is to return:
% {function,{0,1}, f,1, [{clause,{0,1}, [{var,{0,3},'X'}], [],
%   [{call,{0,9}, {atom,{0,9},throw}, ...



-type parse_transform_options() :: proplists:proplist().
% Options specified to a parse transform at runtime, like: report_warnings,
% beam,report_errors, {cwd,"X"}, {outdir,"Y"}, {i,"Z"}, {parse_transform,P},
% debug_info, warnings_as_errors, etc.
%
% (hence not a list_table, anyway not available here)



%% Module subsection.

-type module_name() :: basic_utils:module_name().
% The name of a module.


%% Function subsection.

-type function_name() :: basic_utils:function_name().
% The name of a function.


-type function_arity() :: arity().
% The arity of a function.


-type function_id() :: { function_name(), function_arity() }.
% Declaration of a function based on a name with an arity (unique function
% signature within a module).


-type function_type() :: any().
% The type of a function (currenty: unclear semantics).


-type clause_def() :: form().
% The form corresponding to the definition of a clause of a function, typically
% {clause, LINE, Rep(Ps), Rep(Gs), Rep(B)} for '( Ps ) when Gs -> B'.


-type function_spec() :: form().
% The full type specification (if any) of that function, as an abstract form;
% typically:
%
% {attribute, L, spec, { {foobar,Arity}, [{type,L,'fun', [{type,L,...


-type variable_name() :: atom().
% The name of a variable (e.g. 'X', or '_' in some cases).


-type term_transformer() :: fun( ( term(), user_data() ) ->
										{ term(), user_data() } ).
% Type of functions to transform terms during a recursive traversal (see
% transform_term/4).
%
% Such a transformer can operate on ASTs, but more generally on any kind of
% terms.
%
% Note: apparently we cannot use the 'when' notation here (InputTerm ... when
% InputTerm :: term()).


-export_type([ parse_transform_options/0,
			   module_name/0,
			   function_name/0, function_arity/0, function_id/0,
			   function_type/0,
			   clause_def/0, function_spec/0, variable_name/0,
			   term_transformer/0 ]).



% Shorthands:

-type user_data() :: basic_utils:user_data().

-type primitive_type_description() :: type_utils:primitive_type_description().

-type form() :: ast_base:form().

-type module_info() :: ast_info:module_info().
-type type_info() :: ast_info:type_info().
-type function_info() :: ast_info:function_info().

-type ast_transforms() :: ast_transform:ast_transforms().



% Parse-transform related functions:
-export([ apply_ast_transforms/2,
		  add_function/3, add_function/4, remove_function/2,
		  add_type/2, remove_type/2 ]).



% General functions about functions, not operating on ASTs:
-export([ list_exported_functions/1, get_arities_for/2,
		  is_function_exported/3, check_potential_call/3 ]).


% More general-purpose, term-oriented functions:
-export([ transform_term/4 ]).

% Common settings:
-export([ get_compile_base_opts/0, get_debug_info_settings/0 ]).



% Function addition/removal section.


% @doc Registers (includes exporting) the specified (spec-less) function in the
% specified module.
%
-spec add_function( function_id(), [ clause_def() ], module_info() ) ->
							module_info().
add_function( _FunId={ FunctionName, FunctionArity }, Clauses, ModuleInfo ) ->
	add_function( FunctionName, FunctionArity, Clauses, ModuleInfo ).


% @doc Registers (includes exporting) the specified (spec-less) function in the
% specified module.
%
-spec add_function( basic_utils:function_name(), meta_utils:function_arity(),
					[ clause_def() ], module_info() ) -> module_info().
add_function( FunctionName, FunctionArity, Clauses,
			  ModuleInfo=#module_info{ %function_exports=ExportTable,
									   functions=FunTable,
									   markers=MarkerTable } ) ->

	% Let's check first that the function is not already defined:
	FunId = { FunctionName, FunctionArity },

	?table:has_entry( FunId, FunTable ) andalso
		begin
			CurrentFunInfo = ?table:get_value( FunId, FunTable ),
			CurrentFunString =
				ast_info:function_info_to_string( CurrentFunInfo ),

			ast_utils:display_error( "Function ~p already defined, as ~ts.",
									 [ FunId, CurrentFunString ] ),

			throw( { function_already_defined, FunId } )
		end,

	DefASTLoc = ?table:get_value( definition_functions_marker, MarkerTable ),

	ExportASTLoc = ast_info:get_default_export_function_location(),

	FunInfo = #function_info{ name=FunctionName,
							  arity=FunctionArity,
							  ast_location=DefASTLoc,
							  file_location=0,
							  clauses=Clauses,
							  spec=undefined,
							  callback=false,

							  % Will be auto-exported once module is recomposed:
							  exported=[ ExportASTLoc ] },

	NewFunTable = ?table:add_entry( FunId, FunInfo, FunTable ),

	% It is not strictly needed anymore to update accordingly the overall export
	% table, as would be done automatically when recomposing the AST:
	%
	%NewExportTable = ast_info:ensure_function_exported( FunId, [ ExportLoc ],
	%                                   ModuleInfo, ExportTable ),

	ModuleInfo#module_info{ %function_exports=NewExportTable,
							functions=NewFunTable }.



% @doc Unregisters the specified function from the specified module.
-spec remove_function( function_info(), module_info() ) -> module_info().
remove_function( FunInfo=#function_info{ exported=ExportLocs },
				 ModuleInfo=#module_info{ function_exports=ExportTable,
										  functions=FunTable } ) ->

	FunId = { FunInfo#function_info.name, FunInfo#function_info.arity },

	% First forget its description:
	NewFunTable = case ?table:has_entry( FunId, FunTable ) of

		true ->
			?table:remove_entry( FunId, FunTable );

		false ->
			throw( { non_existing_function_to_remove, FunId } )

	end,

	% Then its exports:
	NewExportTable = ast_info:ensure_function_not_exported( FunId, ExportLocs,
															ExportTable ),

	ModuleInfo#module_info{ function_exports=NewExportTable,
							functions=NewFunTable }.



% Type addition/removal section.


% @doc Registers the specified, fully-described type in the specified module.
-spec add_type( type_info(), module_info() ) -> module_info().
add_type( TypeInfo=#type_info{ variables=TypeVariables,
							   exported=ExportLocs },
		  ModuleInfo=#module_info{ type_exports=ExportTable,
								   types=TypeTable } ) ->

	Arity = length( TypeVariables ),

	% Let's check first that the type is not already defined:
	TypeId = { TypeInfo#type_info.name, Arity },

	?table:has_entry( TypeId, TypeTable ) andalso
		begin
			CurrentTypeInfo = ?table:get_value( TypeId, TypeTable ),
			CurrentTypeString = ast_info:type_info_to_string( CurrentTypeInfo ),

			AddedTypeString = ast_info:type_info_to_string( TypeInfo ),

			ast_utils:display_error( "Type ~p already defined, as ~ts, "
				"whereas to be added, as ~ts.",
				[ TypeId, CurrentTypeString, AddedTypeString ] ),

			throw( { type_already_defined, TypeId } )

		end,

	NewTypeTable = ?table:add_entry( TypeId, TypeInfo, TypeTable ),

	% Now updating the exports:
	NewExportTable = ast_info:ensure_type_exported( TypeId, ExportLocs,
													ModuleInfo, ExportTable ),

	ModuleInfo#module_info{ type_exports=NewExportTable,
							types=NewTypeTable }.



% @doc Unregisters specified type from specified module.
-spec remove_type( type_info(), module_info() ) -> module_info().
remove_type( TypeInfo=#type_info{ variables=TypeVariables,
								  exported=ExportLocs },
			 ModuleInfo=#module_info{ type_exports=ExportTable,
									  types=TypeTable } ) ->

	Arity = length( TypeVariables ),

	TypeId = { TypeInfo#type_info.name, Arity },

	% First forget its description:
	NewTypeTable = case ?table:has_entry( TypeId, TypeTable ) of

		true ->
			?table:remove_entry( TypeId, TypeTable );

		false ->
			throw( { non_existing_type_to_remove, TypeId } )

	end,

	% Then its exports:
	NewExportTable =
		ast_info:ensure_type_not_exported( TypeId, ExportLocs, ExportTable ),

	ModuleInfo#module_info{ type_exports=NewExportTable,
							types=NewTypeTable }.



% @doc Applies the specified AST transformations (mostly depth-first) to the
% specified module information.
%
% (helper)
%
-spec apply_ast_transforms( module_info(), ast_transforms() ) ->
						{ module_info(), ast_transforms() }.
apply_ast_transforms( ModuleInfo=#module_info{ types=TypeTable,
											   records=RecordTable,
											   functions=FunctionTable },
					  Transforms ) ->

	% Note: we consider that no transformation state is to be carried from a
	% top-level transformation to another (so we consider that Transforms is
	% immutable here)

	% First, update the type definitions accordingly (including in records):

	%ast_utils:display_trace( "[Myriad] Transforming known types." ),
	{ NewTypeTable, TypeTransforms } =
		ast_type:transform_type_table( TypeTable, Transforms ),

	%ast_utils:display_trace( "[Myriad] Transforming known types in records." ),
	{ NewRecordTable, RecTransforms } =
		ast_type:transform_types_in_record_table( RecordTable, TypeTransforms ),

	%ast_utils:display_trace( "[Myriad] Transforming all functions." ),
	{ NewFunctionTable, FunTransforms } =
		ast_function:transform_functions( FunctionTable, RecTransforms ),

	% Updated module_info returned:
	{ ModuleInfo#module_info{ types=NewTypeTable,
							  records=NewRecordTable,
							  functions=NewFunctionTable },
	  FunTransforms }.



% @doc Lists (in the order of their definition) all the functions ({Name,Arity})
% that are exported by the specified module, expected to be found in the code
% path.
%
-spec list_exported_functions( module_name() ) -> [ function_id() ].
list_exported_functions( ModuleName ) ->

	% To avoid a unclear message like 'undefined function XXX:module_info/1':
	code_utils:is_beam_in_path( ModuleName ) =:= not_found andalso
		begin

			FilteredCodePath = [
				case file_utils:is_existing_directory_or_link( P ) of

					true ->
						P;

					false ->
						text_utils:format( "(~ts)", [ P ] )

				end || P <- code_utils:get_code_path(), P =/= "" ],

			trace_utils:error_fmt( "Module '~ts' not found in code path "
				"(from '~ts'), which is (sorted alphabetically, "
				"non-existing directories being parenthesized): ~ts",
				[ ModuleName, file_utils:get_current_directory(),
				  code_utils:code_path_to_string( FilteredCodePath ) ] ),
			throw( { module_not_found_in_path, ModuleName } )

	end,

	% Supposedly relevant:
	{ module, ModuleName } = code:ensure_loaded( ModuleName ),

	try

		ModuleName:module_info( exports )

	catch error:undef ->

		trace_utils:error_fmt( "Unable to get the exports for module '~ts'; "
			"this may happen for example if this module was added to an OTP "
			"release that was not rebuilt since then.", [ ModuleName ] ),

		throw( { no_exports_for, ModuleName } )

	end.



% @doc Returns a list of the arities for which the specified function of the
% specified module is exported.
%
-spec get_arities_for( module_name(), function_name() ) -> [ arity() ].
get_arities_for( ModuleName, FunctionName ) ->

	ExportedFuns = list_exported_functions( ModuleName ),

	% Match on FunctionName:
	[ Arity || { Name, Arity } <- ExportedFuns, Name =:= FunctionName ].



% @doc Tells whether the specified function (name with arity) is exported by the
% specified module.
%
-spec is_function_exported( module_name(), function_name(), arity() ) ->
									boolean().
is_function_exported( ModuleName, FunctionName, Arity ) ->

	% Could have relied on erlang:function_exported/3 after a
	% code:ensure_loaded/1:
	%
	lists:member( { FunctionName, Arity },
				  list_exported_functions( ModuleName ) ).



% @doc Checks whether a potential upcoming call to the specified MFA
% (Module,Function,Arguments) has a chance of succeeding.
%
-spec check_potential_call( module_name(), function_name(),
							[ basic_utils:argument() ] ) ->
				'ok' | 'module_not_found' | 'function_not_exported'.
check_potential_call( ModuleName, FunctionName, Arguments )
				when is_atom( ModuleName ) andalso is_atom( FunctionName )
					 andalso is_list( Arguments ) ->

	case code_utils:is_beam_in_path( ModuleName ) of

		not_found ->
			module_not_found;

		_ ->
			Arity = length( Arguments ),
			case is_function_exported( ModuleName, FunctionName, Arity ) of

				true ->
					ok;

				false ->
					function_not_exported

			end

	end;

check_potential_call( ModuleName, FunctionName, Arguments ) ->

	is_atom( ModuleName ) orelse
		throw( { non_atom_module_name, ModuleName } ),

	is_atom( FunctionName ) orelse
		throw( { non_atom_function_name, FunctionName } ),

	% Only remaining possibility:
	throw( { non_list_arguments, Arguments } ).




% @doc Transforms "blindly" (that is with no a-priori knowledge about its
% structure) the specified arbitrary term (possibly with nested subterms, as the
% function recurses in lists, tuples and maps), calling specified transformer
% function on each instance of the specified type, in order to replace that
% instance by the result of that function.
%
% Note that specifying 'undefined' as type description leads to transform
% (exactly) all non-container types.
%
% Returns an updated term, with these replacements made.
%
% For example the input term could be `T={a, ["foo", {c, [2.0, 45]}]}' and the
% function might replace, for example, floats by `<<bar>>'; then `{a, ["foo",
% {c, [<<bar>>, 45]}]}' would be returned.
%
% Note: the transformed terms are themselves recursively transformed, to ensure
% nesting is managed. Of course this implies that the term transform should not
% result in iterating the transformation infinitely.
%
% As a result it may appear that a term of the targeted type is transformed
% almost systematically twice: it is first transformed as such, and the result
% is transformed in turn. If the transformed term is the same as the original
% one, then that content will be shown as analysed twice.
%
-spec transform_term( term(), basic_utils:maybe( primitive_type_description() ),
				term_transformer(), user_data() ) -> { term(), user_data() }.
% Here the term is a list and this is the type we want to intercept:
transform_term( TargetTerm, TypeDescription=list, TermTransformer, UserData )
								when is_list( TargetTerm ) ->

	{ TransformedTerm, NewUserData } = TermTransformer( TargetTerm, UserData ),

	transform_transformed_term( TransformedTerm, TypeDescription,
								TermTransformer, NewUserData );


% Here the term is a list and we are not interested in them:
transform_term( TargetTerm, TypeDescription, TermTransformer, UserData )
								when is_list( TargetTerm ) ->
	transform_list( TargetTerm, TypeDescription, TermTransformer, UserData );


% Here the term is a map and this is the type we want to intercept:
transform_term( TargetTerm, TypeDescription=map, TermTransformer, UserData )
								when is_map( TargetTerm ) ->

	{ TransformedTerm, NewUserData } = TermTransformer( TargetTerm, UserData ),

	transform_transformed_term( TransformedTerm, TypeDescription,
								TermTransformer, NewUserData );


% Here the term is a map and we are not interested in them:
transform_term( TargetTerm, TypeDescription, TermTransformer, UserData )
								when is_map( TargetTerm ) ->
	transform_map( TargetTerm, TypeDescription, TermTransformer, UserData );



% Here the term is a tuple (or a record...), and we want to intercept them:
transform_term( TargetTerm, TypeDescription, TermTransformer, UserData )
		when is_tuple( TargetTerm ) andalso (
			TypeDescription =:= tuple orelse TypeDescription =:= record ) ->

	{ TransformedTerm, NewUserData } = TermTransformer( TargetTerm, UserData ),

	transform_transformed_term( TransformedTerm, TypeDescription,
								TermTransformer, NewUserData );


% Here the term is a tuple (or a record...), and we are not interested in them:
transform_term( TargetTerm, TypeDescription, TermTransformer, UserData )
								when is_tuple( TargetTerm ) ->
	transform_tuple( TargetTerm, TypeDescription, TermTransformer, UserData );


% Base case (current term is not a binding structure, it is a leaf of the
% underlying syntax tree):
%
transform_term( TargetTerm, _TypeDescription=undefined, TermTransformer,
				UserData ) ->
	% All non-container types selected:
	TermTransformer( TargetTerm, UserData );

transform_term( TargetTerm, TypeDescription, TermTransformer, UserData ) ->

	case type_utils:get_type_of( TargetTerm ) of

		TypeDescription ->
			TermTransformer( TargetTerm, UserData );

		_ ->
			% Unchanged:
			{ TargetTerm, UserData }

	end.



% @doc Transforms the elements of a list (helper).
transform_list( TargetList, TypeDescription, TermTransformer, UserData ) ->

	{ NewList, NewUserData } = lists:foldl(
		fun( Elem, { AccList, AccData } ) ->

			{ TransformedElem, UpdatedData } = transform_term( Elem,
				TypeDescription, TermTransformer, AccData ),

			% New accumulator, produces a reversed element list:
			{ [ TransformedElem | AccList ], UpdatedData }

		end,

		_Acc0={ _Elems=[], UserData },

		TargetList ),

	{ lists:reverse( NewList ), NewUserData }.



% @doc Transforms the entries of a map (helper).
%
% The transformation is applied entry by entry and, for each of them, first on
% the key, then on the value.
%
transform_map( TargetMap, TypeDescription, TermTransformer, UserData ) ->

	{ NewMap, NewUserData } = maps:fold(
		fun( K, V, _Acc={ AccMap, AccData } ) ->

			{ TransformedK, UpdatedKData } = transform_term( K,
				TypeDescription, TermTransformer, AccData ),

			{ TransformedV, UpdatedVData } = transform_term( V,
				TypeDescription, TermTransformer, UpdatedKData ),

			NewAccMap = AccMap#{ TransformedK => TransformedV },

			% New accumulator, produces a reversed element map:
			{ NewAccMap, UpdatedVData }

		end,

		_Acc0={ _EmptyMap=#{}, UserData },

		TargetMap ),

	{ NewMap, NewUserData }.



% @doc Transforms the elements of a tuple (helper).
transform_tuple( TargetTuple, TypeDescription, TermTransformer, UserData ) ->

	% We do exactly as with lists:
	TermAsList = tuple_to_list( TargetTuple ),

	{ NewList, NewUserData } = transform_list( TermAsList, TypeDescription,
											   TermTransformer, UserData ),

	{ list_to_tuple( NewList ), NewUserData }.



% @doc Transforms any term by traversing it (helper).
%
% Helper to traverse a transformed term (e.g. if looking for a {user_id, String}
% pair, we must recurse in nested tuples like: {3, {user_id, "Hello"}, 1}.
%
transform_transformed_term( TargetTerm, TypeDescription, TermTransformer,
							UserData ) ->

	case TermTransformer( TargetTerm, UserData ) of

		{ TransformedTerm, NewUserData } when is_list( TransformedTerm ) ->
			transform_list( TransformedTerm, TypeDescription, TermTransformer,
							NewUserData );

		{ TransformedTerm, NewUserData } when is_map( TransformedTerm ) ->
			transform_map( TransformedTerm, TypeDescription, TermTransformer,
							NewUserData );

		{ TransformedTerm, NewUserData } when is_tuple( TransformedTerm ) ->
			transform_tuple( TransformedTerm, TypeDescription, TermTransformer,
							 NewUserData );

		% {ImmediateTerm, NewUserData} ->
		Other ->
			Other

	end.



% @doc Returns the recommended base compilation options to be used for
% code generation (that is the compilation of forms).
%
-spec get_compile_base_opts() -> [ compile:option() ].
get_compile_base_opts() ->

	% We want here all generated functions to be exported:
	%
	% (a priori not interesting: 'compressed')
	%
	[ verbose,report_errors,report_warnings, warnings_as_errors,
	  export_all, no_line_info, debug_info ]
		++ get_debug_info_settings().


% @doc Returns suitable settings for the debug_info chunk in generated code.
-spec get_debug_info_settings() -> [ tuple() ].

% See DEBUG_INFO_KEY in GNUmakevars.inc:
-ifdef(myriad_debug_info_key).

get_debug_info_settings() ->
	Key = ?myriad_debug_info_key,
	%trace_utils:debug_fmt( "Using debug info key '~ts'.", [ Key ] ),
	[ { debug_info, des3_cbc, [], Key } ].

-else. % myriad_debug_info_key

get_debug_info_settings() ->
	%trace_utils:debug( "Not using any debug info key." ),
	[].

-endif. % myriad_debug_info_key
