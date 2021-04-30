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


% Overall parse transform for the Sim-Diasca engine.
%
% It is meant to be applied to ASTs describing Sim-Diasca classes (not standard
% modules), i.e. specific WOOPER classes, targeting especially classes
% inheriting (directly or not) from class_Actor.
%
-module(sim_diasca_parse_transform).



% Implementation notes:

% Calls in turn parts of the WOOPER parse transform (itself calling the Myriad
% parse transform), before and after the Sim-Diasca-level operations have been
% completed.
%
% Note than most of the WOOPER functions cannot be reused as are, as they expect
% class_info() records, whereas we have actor_class_info() ones; not a serious
% drawback as thus any future customisation is made easier.
%
% One will get: 'undefined parse transform 'sim_diasca_parse_transform'' as soon
% as a compiled module called by the parse transform will not be found (hence
% even if the transform itself is available) or a non-exported (or even not
% existing) function is called.

% We must discriminate here between "standard" (WOOPER) classes and the more
% specific ones that implement models (i.e. direct or indirect children of
% class_Actor).



% Regarding the Sim-Diasca parse transform.

% All Sim-Diasca-related internal symbols (ex: atoms, functions, etc.) are to be
% prefixed by 'sim_diasca_'. This prefix shall be considered as reserved for the
% engine internals (hence all sim_diasca_* symbols are forbidden to the user).


% To better report errors
-define( origin_layer, "Sim-Diasca" ).


% Natures as known of the engine:
-type function_extended_nature() :: wooper_method_management:function_nature()
								  | 'actor_oneway'.


% Local shorthands:

-type ast() :: ast_base:ast().
-type line() :: ast_base:line().
-type function_info() :: ast_info:function_info().
-type function_id() :: meta_utils:function_id().
-type module_info() :: ast_info:module_info().
-type ast_transforms() :: ast_transform:ast_transforms().

-type class_info() :: wooper_info:class_info().
-type function_nature() :: wooper_method_management:function_nature().
-type method_qualifiers() :: wooper:method_qualifiers().

-type actor_class_info() :: actor_info:actor_class_info().


-export([ run_standalone/1, parse_transform/2, engine_call_transformer/4,
		  apply_sim_diasca_transform/2 ]).


% For the actor_info record:
-include("actor_info.hrl").

% For display_trace/{1,2}:
-include("actor_debug.hrl").


% For the class_info record:
-include_lib("wooper/include/wooper_info.hrl").


% For the function_info record:
-include_lib("myriad/include/ast_info.hrl").

% For the ast_transforms record:
-include_lib("myriad/include/ast_transform.hrl").



% Runs the Sim-Diasca parse transform defined here in a standalone way
% (i.e. without being triggered by the usual, integrated compilation process).
%
% This allows to benefit from all compilation error and warning messages,
% whereas they are seldom available from a code directly run as a parse
% transform (ex: 'undefined parse transform 'foobar'' as soon as a function or a
% module is not found).
%
-spec run_standalone( file_utils:file_name() ) -> { ast(), class_info() }.
run_standalone( FileToTransform ) ->

	InputAST = ast_utils:erl_to_ast( FileToTransform ),

	% Returns { SimDiascaAST, ClassInfo }:
	apply_sim_diasca_transform( InputAST, _Options=[] ).



% The parse transform itself, transforming the specified (Sim-Diasca-based)
% Abstract Format code first into a WOOPER-based one (which itself will be
% translated into Myriad-based information being itself converted in turn into
% an Erlang-compliant Abstract Format code).
%
-spec parse_transform( ast(), meta_utils:parse_transform_options() ) -> ast().
parse_transform( InputAST, Options ) ->

	%trace_utils:notice_fmt( "(applying parse transform '~p')", [ ?MODULE ] ),

	%trace_utils:info_fmt( "Sim-Diasca input AST:~n~p~n", [ InputAST ] ),

	%trace_utils:info_fmt( "Sim-Diasca options:~n~p~n", [ Options ] ),

	%ast_utils:write_ast_to_file( InputAST, "Sim-Diasca-input-AST.txt" ),

	% In the context of this direct parse transform, the class_info is of no
	% use afterwards and thus can be dropped:
	%
	{ SimDiascaAST, _ClassInfo } =
		apply_sim_diasca_transform( InputAST, Options ),

	%trace_utils:info_fmt( "Sim-Diasca output AST:~n~p~n", [ SimDiascaAST ] ),

	SimDiascaAST.



% Transforms specified AST for Sim-Diasca.
%
-spec apply_sim_diasca_transform( ast(),
   meta_utils:parse_transform_options() ) -> { ast(), actor_class_info() }.
apply_sim_diasca_transform( InputAST, Options ) ->

	%trace_utils:debug_fmt( "  (applying parse transform '~p')", [ ?MODULE ] ),

	%trace_utils:debug_fmt( "~n## INPUT ####################################" ),
	%trace_utils:debug_fmt( "Sim-Diasca input AST:~n~p~n~n", [ InputAST ] ),

	%ast_utils:write_ast_to_file( InputAST, "Sim-Diasca-input-AST.txt" ),

	% This allows to compare input and output ASTs more easily:
	%ast_utils:write_ast_to_file( lists:sort( InputAST ),
	%							 "Sim-Diasca-input-AST-sorted.txt" ),

	% We will do here mostly as wooper_parse_transform:apply_wooper_transform/1
	% does, except that we have to special-case the actor oneways.

	% First preprocesses the AST based on the Myriad parse transform, in order
	% to benefit from its corresponding module_info record:
	% (however no Myriad-level transformation performed yet)
	%
	InputModuleInfo = ast_info:extract_module_info_from_ast( InputAST ),

	WithOptsModuleInfo = ast_info:interpret_options( Options, InputModuleInfo ),

	?display_trace( "Module information extracted." ),

	%ast_utils:display_debug( "Module information, directly as obtained "
	%				"from Myriad and command-line options: ~s",
	%				[ ast_info:module_info_to_string( WithOptsModuleInfo ) ] ),

	% Then promote this Myriad-level information into a Sim-Diasca one:
	% (here is the real magic, including WOOPER one)
	%
	ActorClassInfo = generate_actor_class_info_from( WithOptsModuleInfo ),

	% No specific transformation to insert here.

	%trace_utils:debug_fmt( "~s",
	%	[ actor_info:actor_class_info_to_string( ActorClassInfo ) ] ),

	?display_trace( "Generating back module information." ),

	% Then translates back this actor class information in module information
	% (through an intermediate WOOPER stage):
	NewModuleInfo = generate_module_info_from( ActorClassInfo ),

	%trace_utils:debug_fmt(
	%  "Module information just prior to Myriad transformation: ~s",
	%  [ ast_info:module_info_to_string( ModuleInfoOfInterest ) ] ),

	% And finally obtain the corresponding updated AST thanks to Myriad:
	%
	% (should be done as a final step as Sim-Diasca and WOOPER may of course
	% rely on Myriad-introducted facilities such as void, maybe, table, etc.)

	?display_trace( "Performing Myriad-level transformation." ),

	{ TransformedModuleInfo, _MyriadTransforms } =
		myriad_parse_transform:transform_module_info( NewModuleInfo  ),

	%trace_utils:debug_fmt(
	%  "Module information after Myriad transformation: ~s",
	%  [ ast_info:module_info_to_string( TransformedModuleInfo ) ] ),

	OutputAST = ast_info:recompose_ast_from_module_info(
				  TransformedModuleInfo ),

	?display_trace( "Recomposing corresponding AST." ),

	%trace_utils:debug_fmt( "Sim-Diasca output AST:~n~p", [ OutputAST ] ),

	%OutputASTFilename = text_utils:format(
	%           "Sim-Diasca-output-AST-for-module-~s.txt",
	%			[ element( 1, TransformedModuleInfo#module_info.module ) ] ),

	%ast_utils:write_ast_to_file( OutputAST, OutputASTFilename ),

	%ast_utils:write_ast_to_file( lists:sort( OutputAST ),
	%							 "Sim-Diasca-output-AST-sorted.txt" ),

	{ OutputAST, ActorClassInfo }.




% Returns the "actor class"-level information that were gathered from the
% specified module-level ones.
%
% (reciprocal of generate_module_info_from/1)
%
-spec generate_actor_class_info_from( module_info() ) -> actor_class_info().
generate_actor_class_info_from( ModuleInfo ) ->

	ExtractedActorClassInfo = create_actor_class_info_from( ModuleInfo ),

	% Optional:
	check_actor_class_info( ExtractedActorClassInfo ),

	ExtractedActorClassInfo.



% Ensures that the described class respects appropriate constraints for
% Sim-Diasca (including WOOPER) generation, besides the ones checked during the
% AST exploration and the ones that will be checked by the compiler.
%
% Note: the WOOPER counterpart check_class_info/1 cannot be used due to the
% different record types.
%
-spec check_actor_class_info( actor_class_info() ) -> void().
check_actor_class_info( #actor_class_info{ class={ Classname, _LocForm },
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



% Recomposes Sim-Diasca actor class information from (Myriad) module-level ones.
%
% Directly derived from wooper_parse_transform:create_class_info_from/1; it
% cannot be reused directly because of info record mismatches, and because
% Sim-Diasca must sort by itself the functions, knowing that WOOPER would
% consider actor oneways as invalid (due to their unknown method terminators).
%
% The goal is to pick the relevant SimDiasca-level information (from the module
% info), especially about actor oneways, and to transform them and to populate
% the specified actor class information with thresult.
%
-spec create_actor_class_info_from( module_info() ) -> actor_class_info().
create_actor_class_info_from(
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

	% We start with a (WOOPER-level) class_info that we will promote when
	% necessary into a (SimDiasca-level) actor_class_info.

	BlankClassInfo = wooper_info:init_class_info(),

	% We basically do as wooper_parse_transform:create_class_info_from/1, except
	% regarding methods, so that we can intercept specifically actor oneways:

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

	% Let's start as WOOPER does:

	% Then taking care of the missing fields, roughly in their original order:

	ClassInClassInfo = wooper_class_management:manage_classname( ModuleEntry,
															VerbatimClassInfo ),

	SuperClassInfo = wooper_class_management:manage_superclasses(
					   ClassInClassInfo ),

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


	ConstructPair = wooper_instance_construction:manage_constructors(
					  InitialPair ),

	DestructPair = wooper_instance_destruction:manage_destructor(
					 ConstructPair ),

	% Returns a promoted pair, where the class_info() is replaced by an
	% actor_class_info():
	%
	ActorClassInfo = manage_methods_for_engine( DestructPair ),

	%trace_utils:debug_fmt( "Recomposed actor class information: ~s",
	%	   [ actor_info:actor_class_info_to_string( ReturnedClassInfo ) ] ),

	ActorClassInfo.



% Corresponds to a derived version of wooper_method_management:manage_methods/2,
% whose role is to sort functions according to their nature, one of them being
% here, in addition to the WOOPER ones, "actor oneway" (not just "oneway").
%
% So we have to read all fields of a (WOOPER-level) class_info(), sort functions
% accordingly, and from these elements assign all fields of a (SimDiasca-level)
% actor_class_info():
%
-spec manage_methods_for_engine( { ast_info:function_table(),
								   class_info() } ) -> actor_class_info().
manage_methods_for_engine( { CompleteFunctionTable,  #class_info{
				class=ClassEntry={ Classname, _ClassForm },
				superclasses=SuperclassEntry,
				attributes=AttributeTable,
				inherited_attributes=InheritedAttributeTable,
				compilation_options=CompileOptions,
				compilation_option_defs=CompileOptionDefs,
				parse_attributes=ParseAttributes,
				remote_spec_defs=RemoteSpecsDefs,
				includes=Includes,
				include_defs=IncludeDefs,
				type_exports=TypeExportTable,
				types=TypeTable,
				records=RecordTable,
				function_imports=FunctionImportTable,
				function_imports_defs=FunctionImportDefs,
				function_exports=FunctionExportTable,

				% Ignored as the up-to-date reference is CompleteFunctionTable:
				functions=_IgnoredFunctionTable,

				constructors=ConstructorTable,
				new_operators=NewOpTable,
				destructor=DestructorInfo,
				request_exports=RequestExportTable,
				requests=RequestTable,
				oneway_exports=OnewayExportTable,
				oneways=OnewayTable,
				static_exports=StaticExportTable,
				statics=StaticTable,
				optional_callbacks_defs=OptcallbackDefs,
				debug_mode=DebugMode,
				last_line=LastLine,
				markers=MarkerTable,
				errors=Errors,
				unhandled_forms=UnhandledForms } } ) ->

	AllFunEntries = table:enumerate( CompleteFunctionTable ),

	EmptyTable = table:new(),

	ExportLoc = ast_info:get_default_export_function_location( MarkerTable ),

	% Determined once for all:
	WOOPERExportSet = wooper:get_exported_functions_set(),

	% Sim-Diasca way of sorting:
	{ NewFunctionTable, NewRequestTable, NewOnewayTable, NewActorOnewayTable,
	  NewStaticTable } = sort_out_functions( AllFunEntries,
			_FunctionTable=EmptyTable, RequestTable, OnewayTable,
			_ActorOnewayTable=EmptyTable, StaticTable, Classname, ExportLoc,
			WOOPERExportSet ),

	#actor_class_info{ class=ClassEntry,
					   superclasses=SuperclassEntry,
					   attributes=AttributeTable,
					   inherited_attributes=InheritedAttributeTable,
					   compilation_options=CompileOptions,
					   compilation_option_defs=CompileOptionDefs,
					   parse_attributes=ParseAttributes,
					   remote_spec_defs=RemoteSpecsDefs,
					   includes=Includes,
					   include_defs=IncludeDefs,
					   type_exports=TypeExportTable,
					   types=TypeTable,
					   records=RecordTable,
					   function_imports=FunctionImportTable,
					   function_imports_defs=FunctionImportDefs,
					   function_exports=FunctionExportTable,
					   functions=NewFunctionTable,
					   constructors=ConstructorTable,
					   new_operators=NewOpTable,
					   destructor=DestructorInfo,
					   request_exports=RequestExportTable,
					   requests=NewRequestTable,
					   oneway_exports=OnewayExportTable,
					   oneways=NewOnewayTable,

					   % The major difference with class_info():
					   actor_oneway_exports=EmptyTable,
					   actor_oneways=NewActorOnewayTable,

					   static_exports=StaticExportTable,
					   statics=NewStaticTable,
					   optional_callbacks_defs=OptcallbackDefs,
					   debug_mode=DebugMode,
					   last_line=LastLine,
					   markers=MarkerTable,
					   errors=Errors,
					   unhandled_forms=UnhandledForms }.




% Transforms and categorises each of the specified functions according to its
% real nature (ex: a given Erlang function may actually be a WOOPER oneway, or a
% Sim-Diasca actor oneway, etc.).
%
sort_out_functions( _FunEntries=[], FunctionTable, RequestTable, OnewayTable,
					ActorOnewayTable, StaticTable, _Classname, _ExportLoc,
					_WOOPERExportSet ) ->
	{ FunctionTable, RequestTable, OnewayTable, ActorOnewayTable, StaticTable };


% Checks that all sorted functions have an actual implementation:
sort_out_functions( _FunEntries=[ { _FunId={ FName, Arity },
							#function_info{ %line=undefined,
											clauses=[],
											spec=Spec } } | T ],
	FunctionTable, RequestTable, OnewayTable, ActorOnewayTable, StaticTable,
	Classname, _ExportLoc, _WOOPERExportSet ) when Spec =/= undefined ->

	AllTables = [ FunctionTable, RequestTable, OnewayTable, ActorOnewayTable,
				  StaticTable ],

	SpecLine = case Spec of

		{ _Loc, { attribute, Line, spec, _Tuple } } ->
			Line;

		_ ->
			undefined

	end,

	wooper_method_management:raise_no_implementation_error( Classname, FName,
		Arity, SpecLine, _RemainingFunEntries=T, AllTables );


sort_out_functions( _FunEntries=[ { FunId, FunInfo=#function_info{
											clauses=OriginalClauses,
											spec=Spec } } | T ],
					FunctionTable, RequestTable, OnewayTable, ActorOnewayTable,
					StaticTable, Classname, ExportLoc, WOOPERExportSet ) ->

	%trace_utils:debug_fmt( "Examining Erlang function ~s/~B",
	%                       pair:to_list( FunId ) ),

	% We used to infer the function nature based on its first clause, and then
	% to make a custom full traversal to transform method terminators.
	%
	% Now we reuse the Myriad ast_transforms instead, and perform everything
	% (guessing/checking/transforming) in one pass:
	%
	{ NewClauses, ExtFunNature, Qualifiers } =
		manage_method_terminators( OriginalClauses, FunId, Classname,
								   WOOPERExportSet ),

	%trace_utils:debug_fmt( "~p is a ~s whose qualifiers are ~p.",
	%					   [ FunId, function_nature_to_string( ExtFunNature ),
	%						 Qualifiers ] ),

	NewFunInfo = FunInfo#function_info{ clauses=NewClauses },

	% Last chance to determine the actual nature of a function reported as
	% 'throw': it may have a type spec to remove ambiguity; any spec used also
	% to check consistency with the guessed elements:
	%
	{ FinalExtNature, FinalQualifiers } = take_spec_into_account( Spec, FunId,
							  ExtFunNature, Qualifiers, Classname, NewFunInfo ),

	% Stores the result in the right category and recurses:
	case FinalExtNature of

		% Only SimDiasca-specific case:
		actor_oneway ->

			wooper_method_management:check_state_argument( NewClauses, FunId,
														   Classname ),

			ExportedFunInfo = wooper_method_management:ensure_exported_at(
								NewFunInfo, ExportLoc ),

			ActorOnewayInfo = function_to_actor_oneway_info( ExportedFunInfo,
															 FinalQualifiers ),

			NewActorOnewayTable = table:add_new_entry( FunId, ActorOnewayInfo,
													 ActorOnewayTable ),

			sort_out_functions( T, FunctionTable, RequestTable, OnewayTable,
								NewActorOnewayTable, StaticTable, Classname,
								ExportLoc, WOOPERExportSet );

		% Cannot recurse with wooper_method_management:sort_out_functions/7
		% (different arities), but for the non-actor natures the processing is
		% basically the same:

		function ->

			NewFunctionTable =
				table:add_new_entry( FunId, NewFunInfo, FunctionTable ),

			sort_out_functions( T, NewFunctionTable, RequestTable, OnewayTable,
								ActorOnewayTable, StaticTable, Classname,
								ExportLoc, WOOPERExportSet );

		request ->

			wooper_method_management:check_state_argument( NewClauses, FunId,
														   Classname ),

			ExportedFunInfo = wooper_method_management:ensure_exported_at(
								NewFunInfo, ExportLoc ),

			RequestInfo = wooper_method_management:function_to_request_info(
							ExportedFunInfo, FinalQualifiers ),

			NewRequestTable = table:add_new_entry( FunId, RequestInfo,
												 RequestTable ),

			sort_out_functions( T, FunctionTable, NewRequestTable, OnewayTable,
								ActorOnewayTable, StaticTable, Classname,
								ExportLoc, WOOPERExportSet );

		oneway ->

			wooper_method_management:check_state_argument( NewClauses, FunId,
														   Classname ),

			ExportedFunInfo = wooper_method_management:ensure_exported_at(
								NewFunInfo, ExportLoc ),

			OnewayInfo = wooper_method_management:function_to_oneway_info(
						   ExportedFunInfo, FinalQualifiers ),

			NewOnewayTable = table:add_new_entry( FunId, OnewayInfo,
												OnewayTable ),

			sort_out_functions( T, FunctionTable, RequestTable, NewOnewayTable,
								ActorOnewayTable, StaticTable, Classname,
								ExportLoc, WOOPERExportSet );

		static ->

			ExportedFunInfo = wooper_method_management:ensure_exported_at(
								NewFunInfo, ExportLoc ),

			StaticInfo = wooper_method_management:function_to_static_info(
						   ExportedFunInfo, FinalQualifiers ),

			NewStaticTable = table:add_new_entry( FunId, StaticInfo,
												StaticTable ),

			sort_out_functions( T, FunctionTable, RequestTable, OnewayTable,
								ActorOnewayTable, NewStaticTable, Classname,
								ExportLoc, WOOPERExportSet )

	end;


sort_out_functions( _Functions=[ #function_info{ name=FunName,
												 arity=Arity } | _T ],
					_FunctionTable, _RequestTable, _OnewayTable,
					_ActorOnewayTable, _StaticTable, Classname, _ExportLoc,
					_WOOPERExportSet ) ->

	% Error raised directly, could be appended to the class_info.errors:
	wooper_internals:raise_usage_error( "no clause found for ~s/~B; "
			"function exported yet not defined?",
			[ FunName, Arity ], Classname ).




% Checks that the method spec (if any) corresponds indeed to the right nature of
% function, i.e. relies on the right method terminators with the right
% qualifiers, and returns a corresponding pair.
%
% (helper)
%
-spec take_spec_into_account( maybe( ast_info:located_form() ), function_id(),
		function_nature(), method_qualifiers(), wooper:classname(),
		function_info() ) -> { function_nature(), method_qualifiers() }.
% We intercept only the SimDiasca-specific (actor) specs:

% Special case for a function nature detected as 'throw': any information
% collected from its spec is accepted as is (provided there is only one spec).
%
take_spec_into_account( _LocSpec={ _Loc,
							{ attribute, _, spec, { FunId, [ ClauseSpec ] } } },
		FunId, _FunNature=throw, _Qualifiers, Classname, _FunInfo ) ->
	%trace_utils:notice_fmt( "A1 for ~p~n", [ FunId ] ),
	get_info_from_clause_spec( ClauseSpec, FunId, Classname );


take_spec_into_account( _LocSpec=undefined, FunId, _FunNature=throw,
						_Qualifiers, Classname, FunInfo ) ->

	Line = case FunInfo#function_info.line of

		undefined ->
			0;

		L ->
			L

	end,

	wooper_internals:raise_usage_error(
	  "all clauses of ~s/~B throw an exception; as a result, this "
	  "function can be of any nature. Please define a type specification for "
	  "that function in order to remove this ambiguity "
	  "(ex: use const_actor_oneway_return/0 to mark it as a (const) actor "
	  "oneway).",
	  pair:to_list( FunId ), Classname, Line );


% Spec available for a non-throw actor oneway:
take_spec_into_account( _LocSpec={ _Loc,
					   { attribute, _, spec, { FunId, ClauseSpecs } } },
		_FunId, FunNature=actor_oneway, Qualifiers, Classname, _FunInfo  ) ->
	[ check_clause_spec( C, FunNature, Qualifiers, FunId, Classname )
	  || C <- ClauseSpecs ],
	% If check not failed, approved, so:
	{ FunNature, Qualifiers };


% We rely on WOOPER for the other cases (precisely: if throw-only and no spec,
% if throw with multi-clause spec, if non-throw and no spec)
%
take_spec_into_account( LocSpec, FunId, FunNature, Qualifiers, Classname,
						FunInfo  ) ->
	wooper_method_management:take_spec_into_account( LocSpec, FunId, FunNature,
											 Qualifiers, Classname, FunInfo ).




% Returns a { FunctionNature, Qualifiers } pair deduced from specified clause
% spec.
%
get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, _, actor_oneway_return, [] } ] },
						   _FunId, _Classname ) ->
	{ actor_oneway, [] };


get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, Line, actor_oneway_return, RTypes } ] },
						   FunId, Classname ) ->
	wooper_internals:raise_usage_error(
	  "wrong arity of the specified Sim-Diasca return type for the spec "
	  "of ~s/~B: it should be actor_oneway_return/0 "
	  "(not actor_oneway_return/~B).",
	  pair:to_list( FunId ) ++ [ length( RTypes ) ], Classname, Line );


get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, _, const_actor_oneway_return, [] } ] },
						   _FunId, _Classname ) ->
	{ actor_oneway, [ const ] };


get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun', _Seqs=[
	   _TypeProductForArgs,
	   _ResultType={ user_type, Line, const_actor_oneway_return, RTypes } ] },
						   FunId, Classname ) ->
	wooper_internals:raise_usage_error(
	  "wrong arity of the specified Sim-Diasca return type for the spec "
	  "of ~s/~B: it should be const_actor_oneway_return/0 "
	  "(not const_actor_oneway_return/~B).",
	  pair:to_list( FunId ) ++ length( RTypes ), Classname, Line );


% For the non-SimDiasca return types, rely on WOOPER:
get_info_from_clause_spec( ClauseSpec, FunId, Classname ) ->
	wooper_method_management:get_info_from_clause_spec( ClauseSpec, FunId,
														Classname ).



% Checks the specified method clause spec.
%
% (helper)


%% For actor oneways:

% Spec implies non-const actor oneway:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, actor_oneway_return, [] } ] },
	 _FunNature=actor_oneway, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			wooper_internals:raise_usage_error(
			  "the ~s/~B actor oneway has been detected as const, however its "
			  "spec uses actor_oneway_return/0 instead of "
			  "const_actor_oneway_return/0.",
			  pair:to_list( FunId ), Classname, Line );

		false ->
			ok

	end;


% Spec implies const actor oneway:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_actor_oneway_return, [] } ] },
	 _FunNature=actor_oneway, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			ok;

		false ->
			wooper_internals:raise_usage_error(
			  "the ~s/~B actor oneway has been detected as non-const, however "
			  "its spec uses const_actor_oneway_return/0 instead of "
			  "actor_oneway_return/0.", pair:to_list( FunId ), Classname, Line )

	end;


% Spec of this actor oneway uses thus an unexpected terminator:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, OtherSpecTerminator, Types } ] },
	 _FunNature=actor_oneway, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			wooper_internals:raise_usage_error( "the spec of the ~s/~B const "
				"actor oneway uses an unexpected method terminator, ~s/~B. "
				"Hint: 'const_actor_oneway_return()' might be more relevant.",
				pair:to_list( FunId ) ++ [ OtherSpecTerminator,
										   length( Types ) ],
				Classname, Line );

		false ->
			wooper_internals:raise_usage_error( "the spec of the ~s/~B "
				"actor oneway uses an unexpected method terminator, ~s/~B. "
				"Hint: 'actor_oneway_return()' might be more relevant.",
				pair:to_list( FunId ) ++ [ OtherSpecTerminator,
										   length( Types ) ],
				Classname, Line )

	end;


% Spec implies (non-const) actor oneway whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, actor_oneway_return, [] } ] },
	 NonActFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B has been detected as a ~s, "
		"not as a (non-const) actor oneway, however its spec uses "
		"actor_oneway_return/0.", pair:to_list( FunId )
			++ [ function_nature_to_string( NonActFunNature ) ],
										Classname, Line );


% Spec implies (const) actor oneway whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_actor_oneway_return, [] } ] },
	 NonActFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B has been detected as a ~s, "
		"not as a (const) actor oneway, however its spec uses "
		"const_actor_oneway_return/0.", pair:to_list( FunId )
			++ [ function_nature_to_string( NonActFunNature ) ],
		Classname, Line );


% Wrong arity for actor_oneway_return/0:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, actor_oneway_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses actor_oneway_return/~B, "
		"which does not exist; its correct arity is 0.",
		[ length( Types ) | pair:to_list( FunId ) ], Classname, Line );


% Wrong arity for const_actor_oneway_return/0:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_actor_oneway_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses "
		"const_actor_oneway_return/~B, which does not exist; "
		"its correct arity is 0.",
		pair:to_list( FunId ) ++ [ length( Types ) ], Classname, Line );


% *_result used instead of *_return:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_actor_oneway_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses the (unknown) "
		"const_actor_oneway_result type: probably that "
		"const_actor_oneway_return/0 was meant instead.",
		pair:to_list( FunId ), Classname, Line );


check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, actor_oneway_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~s/~B uses the (unknown) "
		"actor_oneway_result type: probably that "
		"actor_oneway_return/0 was meant instead.",
		pair:to_list( FunId ), Classname, Line );


check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ remote_type, _,
				   [ {atom,_,ModuleName},
					 {atom,Line,FunctionName}, Params ] } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error(
	  "~s/~B uses an unexpected result type, ~s:~s/~B.",
		pair:to_list( FunId ) ++ [ ModuleName, FunctionName, length( Params ) ],
	  Classname, Line );


check_clause_spec( { type, Line, 'fun', _Seqs=[ _TypeProductForArgs,
											 ResultType ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error(
	  "~s/~B uses an unexpected result type:~n  ~p",
		pair:to_list( FunId ) ++ [ ResultType ], Classname, Line ).


% No catch-all clause branching to WOOPER, as by design this function is to be
% called only with actor oneways:

% For the rest:
%check_clause_spec( Clause, FunNature, Qualifiers, FunId, Classname ) ->
	% Rely on WOOPER:
	%wooper_method_management:check_clause_spec( Clause, FunNature, Qualifiers,
	%											FunId, Classname ).



% Infers the nature of the corresponding function and any relevant method
% qualifier(s), ensures that all method terminators correspond, and transforms
% them appropriately, in one pass.
%
% We consider that no method is to be explicitly exported and that all actual
% clauses of a method must explicitly terminate with a relevant method
% terminator (the same for all clauses, except regarding constness), rather than
% calling an helper function that would use such a terminator (otherwise the
% nature of methods could not be auto-detected, as there would be no way to
% determine whether said helper should be considered as a method or not).
%
-spec manage_method_terminators( meta_utils:clause_def(),
				   meta_utils:function_id(), wooper:classname(),
								 wooper:function_export_set() ) ->
		{ meta_utils:clause_def(), function_extended_nature(),
		  wooper:method_qualifiers() }.
manage_method_terminators( _Clauses=[], FunId, Classname, _WOOPERExportSet ) ->
	wooper_internals:raise_usage_error(
	  "the function ~s/~B is exported yet not defined.", pair:to_list( FunId ),
	  Classname );

manage_method_terminators( Clauses, FunId, Classname, WOOPERExportSet ) ->

	% SimDiasca-specific call transformer, to manage specifically types and
	% terminators for actors, and which relies on its WOOPER counterpart for the
	% rest:
	%
	EngineCallTransformFun = fun engine_call_transformer/4,

	% We retain all WOOPER transformers except the call one, that we override:
	TransformTable = table:update_entry( 'call', EngineCallTransformFun,
						wooper_method_management:get_wooper_transform_table() ),

	BlankTransfState =
		wooper_method_management:get_blank_transformation_state(
		  WOOPERExportSet ),

	Transforms = #ast_transforms{ transformed_module_name=Classname,
								  transform_table=TransformTable,
								  transformed_function_identifier=FunId,
								  transformation_state=BlankTransfState },

	%trace_utils:debug_fmt( "transforming now ~p.", [ FunId ] ),

	{ NewClauses, NewTransforms } =
		ast_clause:transform_function_clauses( Clauses, Transforms ),

	% Unless found different, a function is a (plain) function:
	{ FunExtNature, Qualifiers } =
			  case NewTransforms#ast_transforms.transformation_state of

		{ undefined, NoQualifier=[], _WOOPERExportSet } ->
			%trace_utils:debug_fmt( "~s/~B detected as a plain function.",
			%					   pair:to_list( FunId ) ),
			{ function, NoQualifier };

		% Ex: { request, [ const ], _ }
		{ OtherNature, SomeQualifiers, _WOOPERExportSet } ->
			%trace_utils:debug_fmt( "~s/~B detected as: ~p (qualifiers: ~p)",
			%   pair:to_list( FunId ) ++ [ OtherNature, SomeQualifiers ] ),
			{ OtherNature, SomeQualifiers }

	end,

	{ NewClauses, FunExtNature, Qualifiers }.



% In charge of detecting the method terminators and qualifiers, in a Sim-Diasca
% context.
%
% (anonymous mute variables correspond to line numbers)
%
-spec engine_call_transformer( line(), ast_expression:function_ref_expression(),
			ast_expression:params_expression(), ast_transforms() ) ->
					  { [ ast_expression:ast_expression() ], ast_transforms() }.
% We intercept only calls in link with actor oneways; for the rest, we rely on
% the WOOPER counterpart:

% Better use the most precise return pseudo-function if this clause is const
% (since returning the initial State):
%
engine_call_transformer( _LineCall,
		_FunctionRef={ remote, _, {atom,_,actor}, {atom,_,return_state} },
		_Params=[ _StateExpr={ var, Line, 'State' } ],
		Transforms=#ast_transforms{
					  transformed_function_identifier=FunId } ) ->
	wooper_internals:raise_usage_error( "this const clause of actor oneway "
		"~s/~B shall use actor:const_return/0 "
		"(instead of actor:return_state/1).",
		pair:to_list( FunId ), Transforms, Line );


% First (correct, non-const) actor oneway detection:
engine_call_transformer( _LineCall,
	 _FunctionRef={ remote, _, {atom,_,actor}, {atom,_,return_state} },
	 _Params=[ StateExpr ],
	 Transforms=#ast_transforms{
			%transformed_function_identifier=FunId,
			transformation_state={ undefined, _, WOOPERExportSet } } ) ->

	%trace_utils:debug_fmt( "~s/~B detected as a non-const actor oneway.",
	%					   pair:to_list( FunId ) ),

	% So that actor:return_state( S ) becomes simply S:
	NewExpr = StateExpr,
	NewTransforms = Transforms#ast_transforms{
				  transformation_state={ actor_oneway, [], WOOPERExportSet } },
	{ [ NewExpr ], NewTransforms };


% Already detected as an actor oneway, checking qualifiers:
engine_call_transformer( _LineCall,
	  _FunctionRef={ remote, _, {atom,_,actor}, {atom,_,return_state} },
	  _Params=[ StateExpr ],
	  Transforms=#ast_transforms{ transformation_state={ actor_oneway,
										Qualifiers, WOOPERExportSet } } ) ->

	% 'const' may or may not be still there, and will surely not:
	NewQualifiers = lists:delete( const, Qualifiers ),

	NewExpr = StateExpr,

	NewTransforms = Transforms#ast_transforms{ transformation_state={
					   actor_oneway, NewQualifiers, WOOPERExportSet } },

	{ [ NewExpr ], NewTransforms };


% Faulty return_state/1 arity:
engine_call_transformer( LineCall,
		_FunctionRef={ remote, _, {atom,_,actor}, {atom,_,return_state} },
		Params,
		Transforms=#ast_transforms{ transformed_function_identifier=FunId } )
  when length( Params ) =/= 1 ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for actor:return_state/1, when defining actor oneway ~s/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
engine_call_transformer( LineCall,
		_FunctionRef={ remote, _, {atom,_,actor}, {atom,_,return_state} },
		_Params,
		Transforms=#ast_transforms{
		  transformed_function_identifier=FunId,
		  transformation_state={ OtherNature, _Qualifiers,
								 _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~s/~B: actor:return_state/1 implies "
		"actor oneway, whereas was detected as a ~s.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );


% First (correct, a priori const) actor oneway detection:
engine_call_transformer( LineCall,
		_FunctionRef={ remote, _, {atom,_,actor}, {atom,_,const_return} },
		_Params=[],
		Transforms=#ast_transforms{
			%transformed_function_identifier=FunId,
			transformation_state={ undefined, _, WOOPERExportSet } } ) ->

	%trace_utils:debug_fmt( "~s/~B detected as a const actor oneway.",
	%					   pair:to_list( FunId ) ),

	% So that actor:const_return() becomes simply S:
	NewExpr = { var, LineCall, 'State' },
	NewTransforms = Transforms#ast_transforms{
		  transformation_state={ actor_oneway, [ const ], WOOPERExportSet } },
	{ [ NewExpr ], NewTransforms };


% Already detected as an actor oneway, this clause is const, this will not
% change overall constness status:
%
engine_call_transformer( LineCall,
	_FunctionRef={ remote, _, {atom,_,actor}, {atom,_,const_return} },
	_Params=[],
	Transforms=#ast_transforms{ transformation_state={ actor_oneway,
										_Qualifiers, _WOOPERExportSet } } ) ->

	NewExpr = { var, LineCall, 'State' },

	{ [ NewExpr ], Transforms };


% Faulty const_return/0 arity:
engine_call_transformer( LineCall,
	_FunctionRef={ remote, _, {atom,_,actor}, {atom,_,const_return} },
	Params,
	Transforms=#ast_transforms{ transformed_function_identifier=FunId } )
  when Params =/= [] ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for actor:const_return/0, when defining actor oneway ~s/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
engine_call_transformer( LineCall,
	_FunctionRef={ remote, _, {atom,_,actor}, {atom,_,const_return} },
	_Params,
	Transforms=#ast_transforms{
		  transformed_function_identifier=FunId,
		  transformation_state={ OtherNature, _Qualifiers,
								 _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~s/~B: actor:const_return/0 implies "
		"actor oneway, whereas was detected as a ~s.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );


% 'actor' pseudo-module used, yet with no known terminator (ex: a faulty
% 'actor:return( SomeState ).'):
%
engine_call_transformer( LineCall,
	  _FunctionRef={ remote, _, {atom,_,actor}, {atom,_,UnexpectedTerminator} },
	  Params,
	  Transforms=#ast_transforms{ transformed_function_identifier=FunId } ) ->
	wooper_internals:raise_usage_error( "method ~s/~B is using an invalid "
		"actor-related terminator, actor:~s/~B (it shall be either "
		"actor:return_state/1 or actor:const_return/0).",
		pair:to_list( FunId ) ++ [ UnexpectedTerminator, length( Params ) ],
		Transforms, LineCall );


% Anything not having matched is forwarded to the WOOPER counterpart:
%
engine_call_transformer( LineCall, FunctionRef, Params, Transforms ) ->
	wooper_method_management:call_transformer( LineCall, FunctionRef, Params,
											   Transforms ).



% Returns a textual description of the specified function nature.
%
-spec function_nature_to_string( function_extended_nature() ) ->
									   text_utils:ustring().
function_nature_to_string( actor_oneway ) ->
	"actor oneway";

function_nature_to_string( Other ) ->
	wooper_method_management:function_nature_to_string( Other ).






% Converts specified (Myriad-level) function information into a
% (SimDiasca-level) actor oneway information.
%
-spec function_to_actor_oneway_info( function_info(), method_qualifiers() ) ->
										   actor_oneway_info().
function_to_actor_oneway_info( #function_info{ name=Name,
											   arity=Arity,
											   location=Loc,
											   line=Line,
											   clauses=Clauses,
											   spec=Spec,
											   callback=false,
											   exported=[] },
							   Qualifiers ) ->
	#actor_oneway_info{ name=Name,
						arity=Arity,
						qualifiers=Qualifiers,
						location=Loc,
						line=Line,
						clauses=Clauses,
						spec=Spec };


function_to_actor_oneway_info( #function_info{ name=Name,
											   arity=Arity,
											   location=Loc,
											   line=Line,
											   clauses=Clauses,
											   spec=Spec,
											   callback=false
											   %exported
											 },
							   Qualifiers ) ->
	#actor_oneway_info{ name=Name,
						arity=Arity,
						qualifiers=Qualifiers,
						location=Loc,
						line=Line,
						clauses=Clauses,
						spec=Spec };


function_to_actor_oneway_info( Other, _Qualifiers ) ->
	throw( { unexpected_function_info, Other, actor_oneway } ).



% Converts specified (SimDiasca-level) actor oneway information into a
% (WOOPER-level) oneway information.
%
-spec actor_oneway_to_oneway_info( actor_oneway_info() ) ->
										 wooper_info:oneway_info().
actor_oneway_to_oneway_info( #actor_oneway_info{ name=Name,
												 arity=Arity,
												 qualifiers=Qualifiers,
												 location=Loc,
												 line=Line,
												 clauses=Clauses,
												 spec=Spec } ) ->
	% Basically the same:
	#oneway_info{ name=Name,
				  arity=Arity,
				  qualifiers=Qualifiers,
				  location=Loc,
				  line=Line,
				  clauses=Clauses,
				  spec=Spec };

actor_oneway_to_oneway_info( Other ) ->
	throw( { unexpected_actor_oneway_info, Other, actor_oneway } ).



% Generates back (Myriad-level) module-level information from specified
% actor-level information.
%
% (reciprocal of generate_class_info_from/1)
%
-spec generate_module_info_from( actor_class_info() ) -> module_info().
generate_module_info_from( #actor_class_info{
				 class=ClassEntry,
				 superclasses=SuperclassEntry,

				 attributes=AttributeTable,

				 inherited_attributes=InheritedAttributeTable,

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

				 request_exports=RequestExportTable,
				 requests=RequestTable,

				 oneway_exports=OnewayExportTable,
				 oneways=OnewayTable,

				 actor_oneway_exports=ActorOnewayExportTable,
				 actor_oneways=ActorOnewayTable,

				 static_exports=StaticExportTable,
				 statics=StaticTable,

				 optional_callbacks_defs=OptCallbackDefs,

				 last_line=LastLine,

				 markers=MarkerTable,

				 errors=Errors,

				 unhandled_forms=UnhandledForms } ) ->

	% We just ride piggyback with WOOPER, once actor oneways have been
	% special-cased:
	%
	FullOnewayExportTable =
		table:merge_unique( ActorOnewayExportTable, OnewayExportTable ),

	% We have to convert actor_oneway_info records into oneway_info ones:
	ConvertedOnewayTable = table:new(
		 [ { OnwId, actor_oneway_to_oneway_info( ActorOnewayInfo ) }
		   || { OnwId, ActorOnewayInfo } <-
				  table:enumerate( ActorOnewayTable ) ] ),

	FullOnewayTable = table:merge_unique( ConvertedOnewayTable, OnewayTable ),

	WOOPERClassInfo = #class_info{
				 class=ClassEntry,
				 superclasses=SuperclassEntry,

				 attributes=AttributeTable,

				 inherited_attributes=InheritedAttributeTable,

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

				 request_exports=RequestExportTable,
				 requests=RequestTable,

				 oneway_exports=FullOnewayExportTable,
				 oneways=FullOnewayTable,

				 static_exports=StaticExportTable,
				 statics=StaticTable,

				 optional_callbacks_defs=OptCallbackDefs,

				 last_line=LastLine,

				 markers=MarkerTable,

				 errors=Errors,

				 unhandled_forms=UnhandledForms },

	wooper_parse_transform:generate_module_info_from( WOOPERClassInfo ).
