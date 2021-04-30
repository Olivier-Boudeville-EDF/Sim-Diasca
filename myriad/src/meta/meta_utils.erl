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
% Creation date: Friday, December 19, 2014.




% Gathering of various higher-level, convenient meta-related facilities, notably
% regarding metaprogramming, types and parse transforms.
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
% We often use located counterparts of the standard elements (ex: forms, ASTs)
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
%
% - https://github.com/uwiger/parse_trans
% - https://github.com/uwiger/toker


% Useful information about how to convert source code into actual code: on
% http://stackoverflow.com/questions/,
% 2160660/how-to-compile-erlang-code-loaded-into-a-string


% Use -P to see the code generated by a parse-transform; ex: 'erlc -P' or in the
% shell as 'c( "X.erl", [ 'P' ] )'.





% Options specified to a parse transform at runtime, like: report_warnings,
% beam,report_errors, {cwd,"X"}, {outdir,"Y"}, {i,"Z"}, {parse_transform,P},
% debug_info, warnings_as_errors, etc.
%
% (hence not a list_table, anyway not available here)
%
-type parse_transform_options() :: proplists:proplist().



%% Module subsection.

% The name of a module:
-type module_name() :: basic_utils:module_name().



%% Function subsection.


% The name of a function:
-type function_name() :: basic_utils:function_name().


% The arity of a function:
-type function_arity() :: arity().


% Declaration of a function based on a name with an arity (unique function
% signature within a module):
%
-type function_id() :: { function_name(), function_arity() }.


% The type of a function (currenty: unclear semantics).
-type function_type() :: any().


% The form corresponding to the definition of a clause of a function, typically
% { clause, LINE, Rep(Ps), Rep(Gs), Rep(B) } for '( Ps ) when Gs -> B':
%
-type clause_def() :: form().



% The full type specification (if any) of that function, as an abstract form;
% typically:
%
% { attribute, L, spec, { {foobar,Arity}, [{type,L,'fun', [{type,L,...
%
-type function_spec() :: form().


% The name of a variable (ex: 'X', or '_' in some cases):
-type variable_name() :: atom().


-export_type([ parse_transform_options/0,
			   module_name/0,
			   function_name/0, function_arity/0, function_id/0,
			   function_type/0,
			   clause_def/0, function_spec/0, variable_name/0 ]).



% Local shorthands:

-type form() :: ast_base:form().

-type module_info() :: ast_info:module_info().
-type type_info() :: ast_info:type_info().
-type function_info() :: ast_info:function_info().




% Parse-transform related functions:
-export([ apply_ast_transforms/2,
		  add_function/3, add_function/4, remove_function/2,
		  add_type/2, remove_type/2 ]).



% General functions, not operating an ASTs:
-export([ list_exported_functions/1, get_arities_for/2,
		  is_function_exported/3, check_potential_call/3 ]).




% Function addition/removal section.


% Registers (includes exporting) specified (spec-less) function in specified
% module.
%
-spec add_function( function_id(), [ clause_def() ], module_info() ) ->
							module_info().
add_function( _FunId={ FunctionName, FunctionArity }, Clauses, ModuleInfo ) ->
	add_function( FunctionName, FunctionArity, Clauses, ModuleInfo ).


% Registers (includes exporting) specified (spec-less) function in specified
% module.
%
-spec add_function( basic_utils:function_name(), meta_utils:function_arity(),
					[ clause_def() ], module_info() ) -> module_info().
add_function( FunctionName, FunctionArity, Clauses,
			  ModuleInfo=#module_info{ %function_exports=ExportTable,
									   functions=FunTable,
									   markers=MarkerTable } ) ->

	% Let's check first that the function is not already defined:
	FunId = { FunctionName, FunctionArity },

	case ?table:has_entry( FunId, FunTable ) of

		true ->
			CurrentFunInfo = ?table:get_value( FunId, FunTable ),
			CurrentFunString = ast_info:function_info_to_string(
								 CurrentFunInfo ),

			ast_utils:display_error( "Function ~p already defined, as ~ts.",
									 [ FunId, CurrentFunString ] ),

			throw( { function_already_defined, FunId } );

		false ->
			ok

	end,

	DefLoc = ?table:get_value( definition_functions_marker, MarkerTable ),

	ExportLoc = ast_info:get_default_export_function_location(),

	FunInfo = #function_info{ name=FunctionName,
							  arity=FunctionArity,
							  location=DefLoc,
							  line=0,
							  clauses=Clauses,
							  spec=undefined,
							  callback=false,

							  % Will be auto-exported once module is recomposed:
							  exported=[ ExportLoc ] },

	NewFunTable = ?table:add_entry( FunId, FunInfo, FunTable ),

	% It is not strictly needed anymore to update accordingly the overall export
	% table, as would be done automatically when recomposing the AST:
	%
	%NewExportTable = ast_info:ensure_function_exported( FunId, [ ExportLoc ],
	%										  ModuleInfo, ExportTable ),

	ModuleInfo#module_info{ %function_exports=NewExportTable,
							functions=NewFunTable }.




% Unregisters specified function from specified module.
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



% Registers the specified, fully-described type in specified module.
-spec add_type( type_info(), module_info() ) -> module_info().
add_type( TypeInfo=#type_info{ variables=TypeVariables,
							   exported=ExportLocs },
		  ModuleInfo=#module_info{ type_exports=ExportTable,
								   types=TypeTable } ) ->

	Arity = length( TypeVariables ),

	% Let's check first that the type is not already defined:
	TypeId = { TypeInfo#type_info.name, Arity },

	case ?table:has_entry( TypeId, TypeTable ) of

		true ->
			CurrentTypeInfo = ?table:get_value( TypeId, TypeTable ),
			CurrentTypeString = ast_info:type_info_to_string( CurrentTypeInfo ),

			AddedTypeString = ast_info:type_info_to_string( TypeInfo ),

			ast_utils:display_error( "Type ~p already defined, as ~ts, "
				"whereas to be added, as ~ts.",
				[ TypeId, CurrentTypeString, AddedTypeString ] ),

			throw( { type_already_defined, TypeId } );

		false ->
			ok

	end,

	NewTypeTable = ?table:add_entry( TypeId, TypeInfo, TypeTable ),

	% Now updating the exports:
	NewExportTable = ast_info:ensure_type_exported( TypeId, ExportLocs,
													ModuleInfo, ExportTable ),

	ModuleInfo#module_info{ type_exports=NewExportTable,
							types=NewTypeTable }.




% Unregisters specified type from specified module.
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
	NewExportTable = ast_info:ensure_type_not_exported( TypeId, ExportLocs,
														ExportTable ),

	ModuleInfo#module_info{ type_exports=NewExportTable,
							types=NewTypeTable }.



% Applies specified AST transformations (mostly depth-first) to the specified
% module information.
%
% (helper)
%
-spec apply_ast_transforms( module_info(), ast_transform:ast_transforms() ) ->
						{ module_info(), ast_transform:ast_transforms() }.
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



% Lists (in the order of their definition) all the functions ({Name,Arity}) that
% are exported by the specified module, expected to be found in the code path.
%
-spec list_exported_functions( module_name() ) -> [ function_id() ].
list_exported_functions( ModuleName ) ->

	% To avoid a unclear message like 'undefined function XXX:module_info/1':
	case code_utils:is_beam_in_path( ModuleName ) of

		not_found ->
			throw( { module_not_found_in_path, ModuleName } );

		_ ->
			ok

	end,

	try

		ModuleName:module_info( exports )

	catch error:undef ->

		trace_utils:error_fmt( "Unable to get the exports for module '~ts'; "
			"this may happen for example if this module was added to an OTP "
			"release that was not rebuilt since then.", [ ModuleName ] ),

		throw( { no_exports_for, ModuleName } )

	end.



% Returns a list of the arities for which the specified function of the
% specified module is exported.
%
-spec get_arities_for( module_name(), function_name() ) -> [ arity() ].
get_arities_for( ModuleName, FunctionName ) ->

	ExportedFuns = list_exported_functions( ModuleName ),

	% Match on FunctionName:
	[ Arity || { Name, Arity } <- ExportedFuns, Name =:= FunctionName ].



% Tells whether the specified function (name with arity) is exported by the
% specified module.
%
-spec is_function_exported( module_name(), function_name(), arity() ) ->
								  boolean().
is_function_exported( ModuleName, FunctionName, Arity ) ->
	lists:member( { FunctionName, Arity },
				  list_exported_functions( ModuleName ) ).



% Checks whether a potential upcoming call to the specified MFA
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

	case is_atom( ModuleName ) of

		true ->
			ok;

		false ->
			throw( { non_atom_module_name, ModuleName } )

	end,

	case is_atom( FunctionName ) of

		true ->
			ok;

		false ->
			throw( { non_atom_function_name, FunctionName } )

	end,

	% Only remaining possibility:
	throw( { non_list_arguments, Arguments } ).
