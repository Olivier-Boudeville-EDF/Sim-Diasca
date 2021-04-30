% Copyright (C) 2014-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER library.
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
% Creation date: Wednesday, December 24, 2014.


% Defined now here, as the rebar-based build system would not allow us to define
% per-module rules (ex: this module shall itself be compiled by the Myriad parse
% transform).
%
-compile({parse_transform, myriad_parse_transform}).


% Overall parse transform for the WOOPER layer.
%
% It is meant to be applied to ASTs describing (WOOPER) classes (not standard
% modules).
%
-module(wooper_parse_transform).



% Implementation notes:

% Calls in turn the Myriad parse transform, before and after the WOOPER-level
% operations have been completed (respectively to obtain a module_info as input
% for WOOPER, and to transform adequately, as standard Erlang code, any
% WOOPER-injected code that would rely on Myriad conventions).
%
% One will get: 'undefined parse transform 'wooper_parse_transform'' as soon as
% a compiled module called by the parse transform (ex: text_utils.beam) will not
% be found (hence even if the transform itself is available) or a non-exported
% (or even not existing) function is called (ex: text_utils:format/1).

% We must discriminate here between methods and functions, and identify, among
% detected methods: the requests, the oneways and the static ones.
%
% For that we can rely either on the type specs (if any, as technically they
% remain optional - but we decided that, conventionally, they should better be
% mandatory) or on the function definition itself (relying then on the WOOPER
% return primitives).
%
% More precisely, both for the type spec and the actual code (all clauses):
%
% - a request shall return its state and value thanks to a call to
% wooper:request_return/2
%
% - a oneway shall return its state thanks to a call to wooper:oneway_return/1
%
% - a static method (as opposed to the previous two member methods) shall return
% this value thanks to a call to wooper:static_return/1


% We consider here that the ?table type (defined in meta_utils.hrl) is actually
% map_hashtable, and thus can be designated as just table.


% Regarding the WOOPER parse transform.

% All WOOPER-related symbols (ex: atoms, functions, etc.) are to be prefixed by
% 'wooper_'. This prefix shall be considered as reserved for WOOPER internals
% (all wooper_* symbols are forbidden to the user).

% Previously, for simplicity, some values (ex: the superclasses) were defined
% thanks to macro defines (ex: '-define( wooper_foo, 42 )'). Now they are
% specified thanks to attributes -ex: '-wooper_foo( 42 ).' and when there was
% previously ?wooper_foo, we replaced that with the definition of a
% wooper_get_foo() function.



% Regarding function/method exports:
%
% We preferred that methods are auto-exported (defining them is sufficient, no
% particular export declaration needed):
%
% - a pseudo-export line (i.e. '-export([setColor/2, ...]).') is generated and
% appropriately placed in the AST
%
% - knowing that we want -spec lines (even in the form '-oneway setColor(...,
% ... ) -> ...') to remain optional, the kind of a method
% (oneway/request/static) is inferred at compilation-time, based on method
% terminators (ex: scanning for wooper:oneway_return/1 through all "leaves" of
% the call graph)
%
% Constructor(s) and destructor (if any) are also auto-exported (i.e. all
% construct/N and destruct/1 functions).


% Regarding function/method type specifications:
%
% - example for a (plain) function:
%       -spec f(float()) -> integer().
%
% - example for a oneway method:
%       -oneway_spec setColor(wooper:state(), color()) -> void().
%
% - example for a request method:
%       -request_spec getColor(wooper:state()) -> color().
%
% - example for a static method:
%       -static_spec get_mean_count(foo()) -> count().


% Used for iterated (re)composition of class information:
-type compose_pair() :: { ast_info:function_table(), class_info() }.


% For clarity:
-type operator_table() :: function_table().


-export_type([ compose_pair/0, operator_table/0 ]).



-export([ run_standalone/1, run_standalone/2,
		  parse_transform/2, apply_wooper_transform/2,
		  generate_class_info_from/1, create_class_info_from/1,
		  check_class_info/1, generate_module_info_from/1 ]).



% For class_info, attribute_info, etc.:
-include("wooper_info.hrl").

% For display_trace/{1,2}:
-include("wooper_debug.hrl").

% For the function_info record:
-include_lib("myriad/include/ast_info.hrl").


% Shorthands:

-type file_name() :: file_utils:file_name().

-type ast() :: ast_base:ast().
-type form() :: ast_base:form().

-type preprocessor_option() :: ast_utils:preprocessor_option().

-type module_info() :: ast_info:module_info().
-type function_info() :: ast_info:function_info().
-type function_table() :: ast_info:function_table().

-type parse_transform_options() :: meta_utils:parse_transform_options().

-type request_table() :: wooper_info:request_table().
-type oneway_table() :: wooper_info:oneway_table().
-type static_table() :: wooper_info:static_table().

-type class_info() :: wooper_info:class_info().



% Currently not used:
-export([ add_function/4, add_request/4, add_oneway/4, add_static_method/4,
		  get_new_variation_names/0 ]).



% Implementation notes:

% For log output, even if io:format/{1,2} and ast_utils:display_*/* work, we
% recommend using trace_utils:*/*.



% Runs the WOOPER parse transform defined here in a standalone way (i.e. without
% being triggered by the usual, integrated compilation process), with no
% specific preprocessor option.
%
% This allows to benefit from all compilation error and warning messages,
% whereas they are seldom available from a code directly run as a parse
% transform (ex: 'undefined parse transform 'foobar'' as soon as a function or a
% module is not found).
%
-spec run_standalone( file_name() ) -> { ast(), class_info() }.
run_standalone( FileToTransform ) ->
	run_standalone( FileToTransform, _PreprocessorOptions=[] ).



% Runs the WOOPER parse transform defined here in a standalone way (i.e. without
% being triggered by the usual, integrated compilation process), with specified
% preprocessor options.
%
% This allows to benefit from all compilation error and warning messages,
% whereas they are seldom available from a code directly run as a parse
% transform (ex: 'undefined parse transform 'foobar'' as soon as a function or a
% module is not found).
%
-spec run_standalone( file_name(), [ preprocessor_option() ] ) ->
							{ ast(), class_info() }.
run_standalone( FileToTransform, PreprocessorOptions ) ->

	InputAST = ast_utils:erl_to_ast( FileToTransform, PreprocessorOptions ),

	% Returns { WOOPERAST, ClassInfo }:
	apply_wooper_transform( InputAST, _Options=[] ).



% The parse transform itself, transforming the specified (WOOPER-based) Abstract
% Format code first into a Myriad-based information being itself converted in
% turn into an Erlang-compliant Abstract Format code.
%
-spec parse_transform( ast(), parse_transform_options() ) -> ast().
parse_transform( InputAST, Options ) ->

	%trace_utils:info_fmt( "WOOPER input AST:~n~p~n", [ InputAST ] ),

	%trace_utils:info_fmt( "WOOPER options:~n~p~n", [ Options ] ),

	%ast_utils:write_ast_to_file( InputAST, "WOOPER-input-AST.txt" ),

	% In the context of this direct parse transform, the class_info is of no
	% use afterwards and thus can be dropped:
	%
	{ WOOPERAST, _ClassModInfo } = apply_wooper_transform( InputAST, Options ),

	%trace_utils:info_fmt( "WOOPER output AST:~n~p~n", [ WOOPERAST ] ),

	%ast_utils:write_ast_to_file( WOOPERAST, "WOOPER-output-AST.txt" ),

	WOOPERAST.



% Transforms specified AST for WOOPER.
%
% Depending on the nature of the AST (WOOPER class or mere module), returns a
% class information or a module information.
%
-spec apply_wooper_transform( ast(), parse_transform_options() ) ->
									{ ast(), class_info() | module_info() }.
apply_wooper_transform( InputAST, Options ) ->

	%trace_utils:debug_fmt( "  (applying parse transform '~p')", [ ?MODULE ] ),

	%trace_utils:debug_fmt( "~n## INPUT ####################################" ),
	%trace_utils:debug_fmt( "WOOPER input AST:~n~p~n~n", [ InputAST ] ),

	%ast_utils:write_ast_to_file( InputAST, "WOOPER-input-AST.txt" ),

	% This allows to compare input and output ASTs more easily:
	%ast_utils:write_ast_to_file( lists:sort( InputAST ),
	%							 "WOOPER-input-AST-sorted.txt" ),

	% First preprocesses the AST based on the Myriad parse transform, in order
	% to benefit from its corresponding module_info record:
	%
	% (however no Myriad-level transformation performed yet, will be done just
	% before recomposing the module_info)
	%
	InputModuleInfo = ast_info:extract_module_info_from_ast( InputAST ),

	WithOptsModuleInfo = ast_info:interpret_options( Options, InputModuleInfo ),

	?display_trace( "Module information extracted." ),

	%ast_utils:display_debug( "Module information, directly as obtained "
	%	"from Myriad and command-line options: ~ts",
	%	[ ast_info:module_info_to_string( WithOptsModuleInfo ) ] ),

	{ ModInfo, MaybeClassInfo } = case is_wooper_class( WithOptsModuleInfo ) of

		true ->
			% Then promote this Myriad-level information into a WOOPER one:
			% (here is the real WOOPER magic, if any)
			%
			ClassInfo = generate_class_info_from( WithOptsModuleInfo ),

			?display_trace( "Class information generated, transforming it." ),

			% Finally perform WOOPER-specific transformation:
			NewClassInfo = transform_class_info( ClassInfo ),

			%trace_utils:debug_fmt( "Transformed class information: ~ts",
			%    [ wooper_info:class_info_to_string( NewClassInfo ) ] ),

			?display_trace( "Generating back module information." ),

			% Then translates back this class information in module information:
			{ generate_module_info_from( NewClassInfo ), NewClassInfo };

		false ->
			% Not a WOOPER class, hence only the Myriad module-level
			% transformations will apply:
			%
			%trace_utils:debug( "Standard module detected (not a class)." ),
			{ WithOptsModuleInfo, undefined }

	end,

	%trace_utils:debug_fmt(
	%  "Module information just prior to Myriad transformation: ~ts",
	%  [ ast_info:module_info_to_string( ModInfo ) ] ),

	% And finally obtain the corresponding updated AST thanks to Myriad:
	%
	% (should be done as a final step as WOOPER may of course rely on
	% Myriad-introduced facilities such as void, maybe, table, etc.)

	?display_trace( "Performing Myriad-level transformation." ),

	{ TransformedModuleInfo, _MyriadTransforms } =
		myriad_parse_transform:transform_module_info( ModInfo ),

	%trace_utils:debug_fmt(
	%  "Module information after Myriad transformation: ~ts",
	%  [ ast_info:module_info_to_string( TransformedModuleInfo ) ] ),

	OutputAST = ast_info:recompose_ast_from_module_info(
				  TransformedModuleInfo ),

	?display_trace( "Recomposing corresponding AST." ),

	%trace_utils:debug_fmt( "WOOPER output AST:~n~p", [ OutputAST ] ),

	%OutputASTFilename = text_utils:format(
	%   "WOOPER-output-AST-for-module-~ts.txt",
	%	[ element( 1, TransformedModuleInfo#module_info.module ) ] ),

	%ast_utils:write_ast_to_file( OutputAST, OutputASTFilename ),

	%ast_utils:write_ast_to_file( lists:sort( OutputAST ),
	%							 "WOOPER-output-AST-sorted.txt" ),

	case MaybeClassInfo of

		undefined ->
			% Preferring, at least for the moment, returning the untransformed
			% module_info:
			%
			{ OutputAST, WithOptsModuleInfo };

		SomeClassInfo ->
			{ OutputAST, SomeClassInfo }

	end.



% Tells whether the specified module_info corresponds to a WOOPER class or to a
% standard module.
%
-spec is_wooper_class( module_info() ) -> boolean().
is_wooper_class( #module_info{  module={ ModuleName, _LocForm } } ) ->

	case text_utils:atom_to_string( ModuleName ) of

		"class_" ++ _ ->
			true;

		_ ->
			false

	end.



% Returns the class-level information that were gathered from the specified
% module-level ones.
%
% (reciprocal of generate_module_info_from/1)
%
-spec generate_class_info_from( module_info() ) -> class_info().
generate_class_info_from( ModuleInfo ) ->

	% We handle there only WOOPER-specific needs:

	ExtractedClassInfo = create_class_info_from( ModuleInfo ),

	% Optional:
	check_class_info( ExtractedClassInfo ),

	ExtractedClassInfo.



% Recomposes (WOOPER) class information from (Myriad) module-level ones.
%
% The goal is to pick the relevant WOOPER-level information (from the module
% info), to transform them and to populate the specified class information with
% the result.
%
-spec create_class_info_from( module_info() ) -> class_info().
create_class_info_from(
  % We basically reuse (as they are, or after relevant transformations) all
  % information gathered from the module:
  %
  _ModuleInfo=#module_info{ module=ModuleEntry,
							compilation_options=CompileOptTable,
							compilation_option_defs=CompileOptDefs,
							parse_attributes=ParseAttrTable,
							remote_spec_defs=RemoteSpecDefs,
							includes=Includes,
							include_defs=IncludeDefs,
							type_exports=TypeExportTable,
							types=TypeTable,
							records=RecordTable,
							function_imports=FunctionImportTable,
							function_imports_defs=FunctionImportDefs,
							function_exports=FunctionExportTable,
							functions=FunctionTable,
							optional_callbacks_defs=OptCallbacksDefs,
							last_line=LastLine,
							markers=MarkerTable,
							errors=Errors,
							unhandled_forms=UnhandledForms } ) ->

	% TO-DO: check for debug_info being defined either in parse_attributes or in
	% the command-line, and set module_info.debug_mode accordingly.

	BlankClassInfo = wooper_info:init_class_info(),

	% For a starting basis, let's init first all the fields that we do not plan
	% to update, as they are:
	%
	% (the fields that will be updated afterwards are commented out, to be able
	% to check for completeness more easily)
	%
	VerbatimClassInfo = BlankClassInfo#class_info{
						  %class
						  %superclasses
						  %attributes
						  %inherited_attributes
						  compilation_options=CompileOptTable,
						  compilation_option_defs=CompileOptDefs,
						  parse_attributes=ParseAttrTable,
						  remote_spec_defs=RemoteSpecDefs,
						  includes=Includes,
						  include_defs=IncludeDefs,
						  type_exports=TypeExportTable,
						  types=TypeTable,
						  records=RecordTable,
						  function_imports=FunctionImportTable,
						  function_imports_defs=FunctionImportDefs,
						  function_exports=FunctionExportTable,
						  functions=FunctionTable,
						  %constructors
						  %destructor
						  %request_exports
						  %requests
						  %oneway_exports
						  %oneways
						  %static_exports
						  %statics
						  optional_callbacks_defs=OptCallbacksDefs,
						  last_line=LastLine,
						  markers=MarkerTable,
						  errors=Errors,
						  unhandled_forms=UnhandledForms },


	% Then taking care of the missing fields, roughly in their original order:

	ClassInClassInfo = wooper_class_management:manage_classname( ModuleEntry,
															VerbatimClassInfo ),

	SuperClassInfo =
		wooper_class_management:manage_superclasses( ClassInClassInfo ),


	AttrClassInfo = wooper_state_management:manage_attributes( SuperClassInfo ),

	% We extract elements (ex: constructors) from the function table, yet we do
	% not modify specifically the other related information (ex: exports).

	% We manage here { FunctionTable, ClassInfo } pairs, in which the first
	% element is the reference, most up-to-date version of the function table
	% that shall be used (extracted-out for convenience) - not any counterpart
	% that could be found in the second element and that will be updated later
	% from the first:
	%
	InitialFunctionTable = AttrClassInfo#class_info.functions,

	InitialPair = { InitialFunctionTable, AttrClassInfo },


	ConstructPair =
		wooper_instance_construction:manage_constructors( InitialPair ),

	DestructPair =
		wooper_instance_destruction:manage_destructor( ConstructPair ),

	MethodPair = wooper_method_management:manage_methods( DestructPair ),

	% ...

	_FinalPair = { FinalFunctionTable, FinalClassInfo } = MethodPair,

	ReturnedClassInfo = FinalClassInfo#class_info{
							functions=FinalFunctionTable },

	%trace_utils:debug_fmt( "Recomposed class information: ~ts",
	%	   [ wooper_info:class_info_to_string( ReturnedClassInfo ) ] ),

	ReturnedClassInfo.



% Class/module section:

% Any invalid or duplicated module declaration will be caught by the compiler
% anyway.


% We wanted the users to rely on a define such as '-classname(class_MyName)'
% instead of '-module(class_MyName)', yet apparently -module should be found
% *before* the parse-transform is ever triggered (we collected the very first
% InputAST we can get to check, it is already unusable if a module declaration
% was lacking), so that the preprocessor can rely on the ?MODULE macro
% afterwards; otherwise the input AST contains forms such as
% '{error,{L,epp,{undefined,'MODULE',none}}}' instead of the forms that referred
% to ?MODULE (as a result these are lost, unrecoverable information).
%
% Only possible work-around: have the actual modules compiled by a specific
% program, driving the compilation by itself, instead of being inserted as a
% mere parse transform. Later maybe!
%
% For the moment, we stick to requiring a
% -module(class_XXX) declaration.
%
%% get_info( _AST=[ { 'attribute', Line, 'classname', Classname } | T ],
%%		  C=#class_info{ class=undefined, class_def=undefined } ) ->

%%	trace_utils:debug_fmt( "Intercepting WOOPER classname declaration for "
%%						   "'~ts'.", [ Classname ] ),

%%	check_classname( Classname ),

%%	% Transforms that in a standard module definition:
%%	NewDef = { 'attribute', Line, 'module', Classname },

%%	get_info( T, C#class_info{ class=Classname, class_def=NewDef } );


%% % We accept (only) the Erlang-standard, direct '-module(XXX).' declaration
%% for % now:

%% get_info( _AST=[ F={ 'attribute', _Line, 'module', Classname } | T ],
%%		  C=#class_info{ class=undefined, class_def=undefined } ) ->

%%	%trace_utils:debug_fmt( "Intercepting module-based classname declaration "
%%	%					   "for '~ts'.", [ Classname ] ),

%%	check_classname( Classname ),

%%	get_info( T, C#class_info{ class=Classname, class_def=F } );


%% % The fact that no '-module(XXX).' can be found in the source file results in
%% % forms such as {error,{85,epp,{undefined,'MODULE',none}}} that we want to
%% % filter-out, as we will introduce a relevant module form afterwards:
%% %
%% get_info( _AST=[ F={ 'error',{ _Line, 'epp',
%%								 { 'undefined', 'MODULE', 'none' } } } | T ],
%%		  C ) ->

%%	% Problems ahead:
%%	trace_utils:debug_fmt( "Dropping module-related error form ~p.", [ F ] ),

%%	get_info( T, C );



% Adds specified function into the corresponding table.
-spec add_function( meta_utils:function_name(), arity(), form(),
					function_table() ) -> function_table().
add_function( Name, Arity, Form, FunctionTable ) ->

	FunId = { Name, Arity },

	% Its spec might have been found before its definition:

	FunInfo = case table:lookup_entry( FunId, FunctionTable ) of

		key_not_found ->
					  % New entry then:
					  #function_info{ name=Name,
									  arity=Arity,
									  location=undefined,
									  line=undefined,
									  clauses=Form
									  % Implicit:
									  %spec=undefined
									  %callback=undefined
									  %exported=[]
									 };

		{ value, F=#function_info{ clauses=undefined } } ->
			% Just add the form then:
			F#function_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			wooper_internals:raise_usage_error(
			  "multiple definition for ~ts/~B.", pair:to_list( FunId ) )

	end,

	table:add_entry( _K=FunId, _V=FunInfo, FunctionTable ).



% Adds specified request into the corresponding table.
-spec add_request( wooper:request_name(), arity(), form(), request_table() ) ->
							request_table().
add_request( Name, Arity, Form, RequestTable ) ->

	RequestId = { Name, Arity },

	% Its spec might have been found before its definition:

	RequestInfo = case table:lookup_entry( RequestId, RequestTable ) of

		key_not_found ->
			% New entry then:
			#request_info{ name=Name,
						   arity=Arity,
						   qualifiers=[],
						   location=undefined,
						   line=undefined,
						   clauses=Form
							% Implicit:
							%spec=undefined
						 };

		{ value, F=#request_info{ clauses=undefined } } ->
			% Just add the form then:
			F#request_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			wooper_internals:raise_usage_error( "multiple definitions for "
				"request ~ts/~B.", pair:to_list( RequestId ) )

	end,

	table:add_entry( _K=RequestId, _V=RequestInfo, RequestTable ).



% Adds specified oneway into the corresponding table.
-spec add_oneway( wooper:oneway_name(), arity(), form(), oneway_table() ) ->
						oneway_table().
add_oneway( Name, Arity, Form, OnewayTable ) ->

	OnewayId = { Name, Arity },

	% Its spec might have been found before its definition:

	OnewayInfo = case table:lookup_entry( OnewayId, OnewayTable ) of

		key_not_found ->
			% New entry then:
			#oneway_info{ name=Name,
						  arity=Arity,
						  qualifiers=[],
						  location=undefined,
						  line=undefined,
						  clauses=Form
						  % Implicit:
						  %spec=undefined
						};

		{ value, F=#oneway_info{ clauses=undefined } } ->
			% Just add the form then:
			F#oneway_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			wooper_internals:raise_usage_error( "multiple definitions for "
				"oneway ~ts/~B.", pair:to_list( OnewayId ) )

	end,

	table:add_entry( _K=OnewayId, _V=OnewayInfo, OnewayTable ).



% Adds specified static method into the corresponding table.
-spec add_static_method( wooper:static_name(), arity(), form(),
						 static_table() ) -> static_table().
add_static_method( Name, Arity, Form, StaticTable ) ->

	StaticId = { Name, Arity },

	% Its spec might have been found before its definition:

	StaticInfo = case table:lookup_entry( StaticId, StaticTable ) of

		key_not_found ->
			% New entry then:
			#static_info{ name=Name,
						  arity=Arity,
						  clauses=Form
						  % Implicit:
						  %spec=undefined
						};

		{ value, F=#static_info{ clauses=undefined } } ->
			% Just add the form then:
			F#static_info{ clauses=Form };

		% Here a definition was already set:
		_ ->
			wooper_internals:raise_usage_error( "multiple definitions for "
				"static method ~ts/~B.", pair:to_list( StaticId ) )

	end,

	table:add_entry( _K=StaticId, _V=StaticInfo, StaticTable ).



% Ensures that the described class respects appropriate constraints for WOOPER
% generation, besides the ones checked during the AST exploration and the ones
% that will be checked by the compiler.
%
-spec check_class_info( class_info() ) -> void().
check_class_info( #class_info{ class={ Classname, _LocForm },
							   constructors=Constructors } ) ->

	case table:is_empty( Constructors ) of

		true ->
			wooper_internals:raise_usage_error( "no constructor defined "
				"(expecting at least one construct/N defined).", [],
				Classname );

		false ->
			ok

	end.

	% For each clause of each constructor, we should check that the constructors
	% of direct superclasses have all a fair chance of being called.



% Returns a list of the names of the class_X:*new* operators that are generated
% by WOOPER to branch on the construct/N and thus shall not be defined by the
% user.
%
get_new_variation_names() ->
	[ new_link, synchronous_new, synchronous_new_link, synchronous_timed_new,
	  synchronous_timed_new_link, remote_new, remote_new_link,
	  remote_synchronous_new, remote_synchronous_new_link,
	  remote_synchronisable_new_link, remote_synchronous_timed_new,
	  remote_synchronous_timed_new_link ].



% Transforms (at the WOOPER level) specified class information.
-spec transform_class_info( class_info() ) -> class_info().
transform_class_info( ClassInfo ) ->
	% Nothing specific done currently!
	ClassInfo.



% Generates back (Myriad-level) module-level information from specified
% class-level information.
%
% (reciprocal of generate_class_info_from/1)
%
-spec generate_module_info_from( class_info() ) -> module_info().
generate_module_info_from( #class_info{
				 class=ClassEntry,
				 %superclasses

				 attributes=_AttributeTable,

				 % No impact onto the class-related module itself:
				 inherited_attributes=_InheritedAttributeTable,

				 compilation_options=CompileOptTable,
				 compilation_option_defs=CompileOptDefs,

				 parse_attributes=ParseAttrTable,

				 remote_spec_defs=RemoteSpecDefs,

				 includes=Includes,
				 include_defs=IncludeDefs,

				 type_exports=TypeExportTable,
				 types=TypeTable,

				 records=RecordTable,

				 function_imports=FunctionImportTable,
				 function_imports_defs=FunctionImportDefs,

				 function_exports=FunctionExportTable,
				 functions=FunctionTable,

				 constructors=ConstructorTable,
				 new_operators=OperatorTable,
				 destructor=MaybeDestructor,

				 request_exports=_RequestExportTable,
				 requests=RequestTable,

				 oneway_exports=_OnewayExportTable,
				 oneways=OnewayTable,

				 static_exports=_StaticExportTable,
				 statics=StaticTable,

				 optional_callbacks_defs=OptCallbackDefs,

				 last_line=LastLine,

				 markers=MarkerTable,

				 errors=Errors,

				 unhandled_forms=UnhandledForms } ) ->

	% In addition to the plain, classical functions already in
	% FunctionExportTable and Functions, we have to add back constructors,
	% destructor and methods in the function-related fields, i.e. regarding
	% export and definition:

	% For constructors:
	WithConstrFunTable = lists:foldl(

		fun( { ConstructArity, ConstructFunInfo }, AccFunTable ) ->
			ConstructId = { construct, ConstructArity },
			% Expected to have already been appropriately exported.
			table:add_new_entry( ConstructId, ConstructFunInfo,
								 AccFunTable )
		end,
		_Acc0=FunctionTable,
		_List=table:enumerate( ConstructorTable ) ),

	% For new operators:

	% (we do not use merge/2, as we want to detect any clash between
	% user-defined functions and these automatically-generated new operators):
	%

	%trace_utils:debug_fmt( "Integrating the new operators: ~p",
	%					   [ table:keys( OperatorTable ) ] ),

	WithNewOpFunTable = register_functions( table:enumerate( OperatorTable ),
											WithConstrFunTable ),

	% For destructor:
	WithDestrFunTable = case MaybeDestructor of

		undefined ->
			WithNewOpFunTable;

		DestructFunInfo ->
			DestructId = { destruct, 1 },
			% Expected to have already been appropriately exported.
			table:add_new_entry( DestructId, DestructFunInfo,
								 WithNewOpFunTable )

	end,

	% For methods:

	% Probably useless now that locations are determined directly if implicit:
	WithMthdExpTable = FunctionExportTable,
	AllExportTable = WithMthdExpTable,

	WithMthdFunTable = wooper_method_management:methods_to_functions(
		RequestTable, OnewayTable, StaticTable, WithDestrFunTable,
		MarkerTable ),

	AllFunctionTable = WithMthdFunTable,

	%trace_utils:debug_fmt( "Complete function table: ~ts",
	%					   [ table:to_string( AllFunctionTable ) ] ),

	% Directly returned (many fields can be copied verbatim):
	#module_info{

		% Untouched:
		module=ClassEntry,
		compilation_options=CompileOptTable,
		compilation_option_defs=CompileOptDefs,

		parse_attributes=ParseAttrTable,

		% Untouched:
		remote_spec_defs=RemoteSpecDefs,
		includes=Includes,
		include_defs=IncludeDefs,
		type_exports=TypeExportTable,
		types=TypeTable,
		records=RecordTable,
		function_imports=FunctionImportTable,
		function_imports_defs=FunctionImportDefs,
		function_exports=AllExportTable,
		functions=AllFunctionTable,
		optional_callbacks_defs=OptCallbackDefs,
		last_line=LastLine,
		markers=MarkerTable,
		errors=Errors,
		unhandled_forms=UnhandledForms }.



% Registers specified functions in specified (function) table, detecting
% properly any clash.
%
-spec register_functions( [ { meta_utils:function_id(), function_info() } ],
							function_table() ) -> function_table().
register_functions( [], FunctionTable ) ->
	FunctionTable;

register_functions( [ { FunId, FunInfo } | T ], FunctionTable ) ->
	case table:lookup_entry( FunId, FunctionTable ) of

		key_not_found ->
			NewFunctionTable = table:add_entry( FunId, FunInfo, FunctionTable ),
			register_functions( T, NewFunctionTable );

		{ value, OtherFunInfo } ->
			{ FunName, FunArity } = FunId,
			ast_utils:display_error( "Attempt to declare ~ts/~B more than "
				"once; whereas already registered as:~n  ~ts~n"
				"this function has been declared again, as:~n  ~ts~n",
				[ FunName, FunArity,
				  ast_info:function_info_to_string( OtherFunInfo ),
				  ast_info:function_info_to_string( FunInfo ) ] ),
			throw( { multiple_declarations_for, FunId } )

	end.
