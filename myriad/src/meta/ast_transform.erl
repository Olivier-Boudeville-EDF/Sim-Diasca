% Copyright (C) 2018-2021 Olivier Boudeville
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
% Creation date: Sunday, February 4, 2018.



% Module in charge of transforming AST elements, typically by operating on a
% module_info record obtained after the transforming of an AST.
%
% Note that the transform relies on a rather complex and complete traversal of
% the abstract syntax of the AST, inspired from the spec (in
% http://erlang.org/doc/apps/erts/absform.html) and also checked against the
% Erlang 'id' parse transformation (see lib/stdlib/examples/erl_id_trans.erl).
%
-module(ast_transform).


% For table macro, etc.:
-include("meta_utils.hrl").


% For *_info records:
-include("ast_info.hrl").


% For ast_transforms record:
-include("ast_transform.hrl").




% Facilities to express transformations.


% All information regarding AST replacements:
-type ast_transforms() :: #ast_transforms{}.


% Not expected to be legit symbols:
-define( any_module_name, '_' ).

-type module_name_match() :: module_name() | ?any_module_name.



%% Type replacement section.

-define( any_type_name,  '_' ).
-define( any_type_arity, '_' ).


-type type_name_match()  :: type_name()  | ?any_type_name.
-type type_arity_match() :: type_arity() | ?any_type_arity.


% The same arity is kept, and just specifying the module name means that the
% type name is not to change.
%
% Note that this implies that a (local or remote) type can only be replaced by a
% remote type (a priori not a problematic limitation).
%
-type type_replacement() :: { module_name(), type_name() } | module_name().


% Local subsection:

-type local_type_id_match() :: { type_name_match(), type_arity_match() }.

% Either we directly set the target module and type names (using same arity), or
% we apply an anonymous function to determine the corresponding information,
% based on context:
-type local_type_replacement() :: type_replacement()
			| fun( ( type_name(), type_arity(), transformation_state() ) ->
						 { type_replacement(), transformation_state() } ).


% Table defining replacements of local types:
-type local_type_transform_table() ::
		?table:?table( local_type_id_match(), local_type_replacement() ).


% Remote subsection:

-type remote_type_id_match() :: { module_name_match(), type_name_match(),
								  type_arity_match() }.


% Either we directly set the target module and type names (using same arity), or
% we apply an anonymous function to determine the corresponding information,
% based on context:
-type remote_type_replacement() :: type_replacement()
			 | fun( ( module_name(), type_name(), type_arity(),
					  transformation_state() ) ->
						  { type_replacement(), transformation_state() } ).


% Table defining replacements of remote types:
-type remote_type_transform_table() ::
		?table:?table( remote_type_id_match(), remote_type_replacement() ).




%% Call replacement section.

-define( any_function_name,  '_' ).
-define( any_function_arity, '_' ).


-type function_name_match()  :: function_name() | ?any_function_name.
-type function_arity_match() :: arity()         | ?any_function_arity.


% The same arity is kept, and just specifying the module name means that the
% function name of the call is not to change.
%
% Note that this implies that a (local or remote) call can only be replaced by a
% remote call (a priori not a problematic limitation).
%
-type call_replacement() :: { module_name(), function_name() } | module_name().


% Local subsection:

-type local_call_match() :: { function_name_match(), function_arity_match() }.

% Either we directly set the target module and function names (using same
% arity), or we apply an anonymous function to determine the corresponding
% information, based on context:
%
-type local_call_replacement() :: call_replacement()
			 | fun( ( function_name(), arity(), transformation_state() ) ->
						  { call_replacement(), transformation_state() } ) .


% Table defining replacements of local calls:
-type local_call_transform_table() ::
		?table:?table( local_call_match(), local_call_replacement() ).


% Remote subsection:

-type remote_call_match() :: { module_name_match(), function_name_match(),
							   function_arity_match() }.

% Either we directly set the target module and function names (using same
% arity), or we apply an anonymous function to determine the corresponding
% information, based on context:
%
-type remote_call_replacement() :: call_replacement()
			 | fun( ( module_name(), function_name(), arity(),
					  transformation_state() ) ->
						  { call_replacement(), transformation_state() } ).


% Table defining replacements of remote calls:
-type remote_call_transform_table() ::
		?table:?table( remote_call_match(), remote_call_replacement() ).



%% AST subtree replacement section.


% Lists the contexts that may trigger a transformation function:
%
% Note that not all triggers are supported, but that adding any lacking one is
% not especially difficult.
%
-type transform_trigger() :: ast_expression:expression_kind()
							 | 'clause'
							 | 'body'.


% User-supplied function to define how AST clauses shall be transformed:
-type clause_transform_function() ::
		fun( ( ast_clause(), ast_transforms() ) ->
				   { ast_clause(), ast_transforms() } ).


% User-supplied function to define how AST bodies shall be transformed:
-type body_transform_function() :: fun( ( ast_body(), ast_transforms() ) ->
											{ ast_body(), ast_transforms() } ).





% User-supplied function to define how expressions shall be replaced:
%
% (currently describing only call replacements)
%
-type expression_replacement_function() :: fun(
  ( line(), ast_expression:function_ref_expression(),
	ast_expression:params_expression(), ast_transforms() ) ->
					{ [ ast_expression() ], ast_transforms() } ).


% All the kinds of functions able to transform at least a part of an AST:
-type ast_transform_function() :: clause_transform_function()
								| body_transform_function()
								| expression_replacement_function().


% Table defining replacements of parts of an input AST:
%
% Note: a full ast_transforms record (not a mere transformation state) is used
% as input (and output) of these transformation functions so that they can
% trigger in turn recursive transformation calls (ex: to
% ast_expression:transform_expressions/2) by themselves.
%
-type ast_transform_table() ::
		?table:?table( transform_trigger(), ast_transform_function() ).



% Any state that is to be preserved in the course of a transformation (so that
% it may have a memory) and that may be ultimately read (i.e. to be used for its
% inner mode of operation and possibly for the caller's sake as well).
%
-type transformation_state() :: any().


% Designates a function able to properly format typically the output of
% expression transformation (ex: when exiting an
% ast_expression:transform_expression/2 clause).
%
-type transform_formatter() :: fun( ( format_string(), format_values() ) ->
										  ustring() ).




% For all kinds of replacements:
-export_type([ ast_transforms/0, module_name_match/0 ]).


% For type replacements:
-export_type([ type_name_match/0, type_arity_match/0, type_replacement/0,
			   local_type_id_match/0, local_type_replacement/0,
			   local_type_transform_table/0,
			   remote_type_id_match/0, remote_type_replacement/0,
			   remote_type_transform_table/0 ]).

% For clause replacements:
-export_type([ clause_transform_function/0 ]).


% For body replacements:
-export_type([ body_transform_function/0 ]).


% For call replacements:
-export_type([ function_name_match/0, function_arity_match/0,
			   call_replacement/0,
			   local_call_match/0, local_call_replacement/0,
			   local_call_transform_table/0,
			   remote_call_match/0, remote_call_replacement/0,
			   remote_call_transform_table/0 ]).


% For expression replacements:
-export_type([ expression_replacement_function/0,
			   ast_transform_function/0,
			   ast_transform_table/0 ]).

-export_type([ transformation_state/0, transform_formatter/0 ]).



% Another (more basic) way of performing transformations is to operate directly
% on raw AST forms, with no particular knowledge about their structure:



% Type of functions to transform terms during a recursive traversal (see
% transform_term/4).
%
% Note: apparently we cannot use the 'when' notation here (InputTerm ... when
% InputTerm :: term()).
%
-type term_transformer() :: fun( ( term(), user_data() ) ->
										{ term(), user_data() } ).


% Designates the transformation functions that are used to transform differently
% a kind of form (ex: the one of a bistring, a record, etc.) depending on the
% context (ex: in a guard, in an expression, etc.).
%
-type transform_fun() :: transform_fun( ast_base:ast_element() ).


% Designates the transformation functions that are used to transform differently
% a kind of form (ex: the one of a bistring, a record, etc.) depending on the
% context (ex: in a guard, in an expression, etc.).
%
-type transform_fun( TargetType ) :: fun( ( TargetType, ast_transforms() ) ->
											{ TargetType, ast_transforms() } ).


-export_type([ term_transformer/0, transform_fun/0, transform_fun/1 ]).


-export([ get_local_type_transform_table/1, get_remote_type_transform_table/1,
		  get_local_call_transform_table/1, get_remote_call_transform_table/1,
		  transform_term/4, ast_transforms_to_string/1, default_formatter/2 ]).


% Shorthands:

-type user_data() :: basic_utils:user_data().

-type ustring() :: text_utils:ustring().
-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type type_arity() :: type_utils:type_arity().
-type type_name() :: type_utils:type_name().

-type function_name() :: meta_utils:function_name().
-type module_name() :: meta_utils:module_name().

-type line() :: ast_base:line().
-type ast_expression() :: ast_expression:ast_expression().
-type ast_body() :: ast_clause:ast_body().
-type ast_clause() :: ast_clause:ast_clause().



%% Type replacement section.


% Returns a table describing local type replacements.
%
% Ex: [ { { void, 0 }, basic_utils },
%       { { my_maybe, 1 }, { basic_utils, maybe } },
%       % First clause will never match due to arity:
%       { { '_', 3 }, fun( other_void, 0 ) ->
%                                    other_utils;
%                        ( _, '_' ) ->
%                                   {foo_utils,some_type}
%                     end }
% ]
%
% will return a description of the transformation of:
%
%  - void() into basic_utils:void(), as the same type name is implied there; it
%  is just the addition (prefix) of a module, as a remote type
%
%  - my_maybe(T) into basic_utils:maybe(T)
%
%  - other_void() into other_utils:other_void()
%
%  - any type depending on three others by foo_utils:some_type/3
%
-spec get_local_type_transform_table(
		[ { local_type_id_match(), type_replacement() } ] ) ->
				local_type_transform_table().
get_local_type_transform_table( Replacements ) ->
	EmptyTable = ?table:new(),
	get_local_type_repl_helper( Replacements, EmptyTable ).



% (helper)
get_local_type_repl_helper( _Replacements=[], Table ) ->
	Table;

% Replacement can be either { TargetModule, TargetType } or TargetModule:
get_local_type_repl_helper( _Replacements=[
		{ Src={ _SourceTypeMatch, _ArityMatch },
		  Replacement={ _TargetModule, _TargetType } } | T ], Table ) ->

	% Up to one transformation per source type:
	NewTable = ?table:add_new_entry( Src, Replacement, Table ),
	get_local_type_repl_helper( T, NewTable );

% Same target type here:
get_local_type_repl_helper( _Replacements=[
		{ Src={ SourceTypeMatch, _ArityMatch }, TargetModule } | T ], Table )
  when is_atom( TargetModule ) ->

	Replacement = { TargetModule, SourceTypeMatch },

	% Up to one transformation per source type:
	NewTable = ?table:add_new_entry( Src, Replacement, Table ),
	get_local_type_repl_helper( T, NewTable );


get_local_type_repl_helper(_Replacements=[
		{ Src={ _SourceTypeMatch, _ArityMatch }, ReplaceFun } | T ], Table )
  when is_function( ReplaceFun ) ->

	% Up to one transformation per source type:
	NewTable = ?table:add_new_entry( Src, ReplaceFun, Table ),
	get_local_type_repl_helper( T, NewTable ).



% Returns a table describing remote type replacements.
%
% Ex: [ { { a_module, void, 0 }, basic_utils },
%       { { a_module, my_maybe, 1 }, { basic_utils, maybe } },
%       % First clause will never match due to arity:
%       { { '_', '_', 3 }, fun( other_void, 0 ) ->
%                                    other_utils;
%                             ( _, '_' ) ->
%                                   {foo_utils,some_type}
%                     end }
% ]
%
% will return a description of the transformation of:
%
%  - a_module:void() into basic_utils:void(), as the same type name is implied
%  there; it is just the modification of the module used by a remote type
%  - a_module:my_maybe(T) into basic_utils:maybe(T)
%  - M:other_void() into M:other_utils()
%  - any type of any module depending on three other types by
%  foo_utils:some_type/3
%
-spec get_remote_type_transform_table(
		[ { remote_type_id_match(), type_replacement() } ] ) ->
				remote_type_transform_table().
get_remote_type_transform_table( Replacements ) ->
	EmptyTable = ?table:new(),
	get_remote_type_repl_helper( Replacements, EmptyTable ).



% (helper)
get_remote_type_repl_helper( _Replacements=[], Table ) ->
	Table;

% Replacement can be either { TargetModule, TargetType } or TargetModule:
get_remote_type_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, _SourceTypeMatch, _ArityMatch },
			Replacement={ _TargetModule, _TargetType } } | T ], Table ) ->

	% Up to one transformation per source type:
	NewTable = ?table:add_new_entry( Src, Replacement, Table ),
	get_remote_type_repl_helper( T, NewTable );

% Same target type here:
get_remote_type_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, SourceTypeMatch, _ArityMatch }, TargetModule } | T ],
							 Table )
  when is_atom( TargetModule ) ->

	Replacement = { TargetModule, SourceTypeMatch },

	% Up to one transformation per source type:
	NewTable = ?table:add_new_entry( Src, Replacement, Table ),
	get_remote_type_repl_helper( T, NewTable );


get_remote_type_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, _SourceTypeMatch, _ArityMatch }, ReplaceFun } | T ],
							Table )
  when is_function( ReplaceFun ) ->

	% Up to one transformation per source type:
	NewTable = ?table:add_new_entry( Src, ReplaceFun, Table ),
	get_remote_type_repl_helper( T, NewTable ).







%% Call replacement section.


% Returns a table describing local call replacements.
%
% Ex: [ { { halt, 0 }, basic_utils },
%       { { setAttributes, 1 }, { some_utils, set_attr } },
%       % First clause will never match due to arity:
%       { { '_', 3 }, fun( my_fun, 0 ) ->
%                                   other_utils;
%                        ( _, '_' ) ->
%                                   {foo_utils,some_fun}
%                     end }
% ]
%
% will return a description of the transformation of:
%
%  - halt/0 into basic_utils:halt/0, as the same function name is implied there;
%  it is just the addition (prefix) of a module, as a remote call
%
%  - setAttributes/1 into some_utils:set_attr/1
%
%  - my_fun/0 into other_utils:my_fun/0
%
%  - any call to a function of arity 3 by foo_utils:some_fun/3
%
-spec get_local_call_transform_table(
		[ { local_call_match(), call_replacement() } ] ) ->
				local_call_transform_table().
get_local_call_transform_table( Replacements ) ->
	EmptyTable = ?table:new(),
	get_local_call_repl_helper( Replacements, EmptyTable ).



% (helper)
get_local_call_repl_helper( _Replacements=[], Table ) ->
	Table;

% Replacement can be either { TargetModule, TargetFunctionName } or
% TargetModule:
%
get_local_call_repl_helper( _Replacements=[
		{ Src={ _SourceFunctionNameMatch, _ArityMatch },
		  Replacement={ _TargetModule, _TargetFunctionName } } | T ], Table ) ->

	% Up to one transformation per source function:
	NewTable = ?table:add_new_entry( Src, Replacement, Table ),
	get_local_call_repl_helper( T, NewTable );

% Same target function name here:
get_local_call_repl_helper( _Replacements=[
		{ Src={ SourceFunctionNameMatch, _ArityMatch }, TargetModule } | T ],
							Table ) when is_atom( TargetModule ) ->

	Replacement = { TargetModule, SourceFunctionNameMatch },

	% Up to one transformation per source function:
	NewTable = ?table:add_new_entry( Src, Replacement, Table ),
	get_local_call_repl_helper( T, NewTable );


get_local_call_repl_helper(_Replacements=[
		{ Src={ _SourceFunctionNameMatch, _ArityMatch }, ReplaceFun } | T ],
						   Table ) when is_function( ReplaceFun ) ->

	% Up to one transformation per source function:
	NewTable = ?table:add_new_entry( Src, ReplaceFun, Table ),
	get_local_call_repl_helper( T, NewTable ).



% Returns a table describing remote call replacements.
%
% Ex: [ { { a_module, void, 0 }, basic_utils },
%       { { a_module, my_maybe, 1 }, { basic_utils, maybe } },
%       % First clause will never match due to arity:
%       { { '_', '_', 3 }, fun( other_void, 0 ) ->
%                                    other_utils;
%                             ( _, '_' ) ->
%                                   {foo_utils,some_type}
%                     end }
% ]
%
% will return a description of the transformation of:
%
%  - a_module:void() into basic_utils:void(), as the same type name is implied
%  there; it is just the modification of the module used by a remote type
%  - a_module:my_maybe(T) into basic_utils:maybe(T)
%  - M:other_void() into M:other_utils()
%  - any type of any module depending on three other types by
%  foo_utils:some_type/3
%
-spec get_remote_call_transform_table(
		[ { remote_call_match(), call_replacement() } ] ) ->
				remote_call_transform_table().
get_remote_call_transform_table( Replacements ) ->
	EmptyTable = ?table:new(),
	get_remote_call_repl_helper( Replacements, EmptyTable ).


% (helper)
get_remote_call_repl_helper( _Replacements=[], Table ) ->
	Table;

% Replacement can be either { TargetModule, TargetFunctionName } or
% TargetModule:
%
get_remote_call_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, _SourceFunctionNameMatch, _ArityMatch },
			Replacement={ _TargetModule, _TargetFunctionName } } | T ],
							 Table ) ->

	% Up to one transformation per source function:
	NewTable = ?table:add_new_entry( Src, Replacement, Table ),
	get_remote_call_repl_helper( T, NewTable );

% Same target function name here:
get_remote_call_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, SourceFunctionNameMatch, _ArityMatch },
	  TargetModule } | T ], Table ) when is_atom( TargetModule ) ->

	Replacement = { TargetModule, SourceFunctionNameMatch },

	% Up to one transformation per source function:
	NewTable = ?table:add_new_entry( Src, Replacement, Table ),
	get_remote_call_repl_helper( T, NewTable );


get_remote_call_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, _SourceFunctionNameMatch, _ArityMatch },
	  ReplaceFun } | T ], Table ) when is_function( ReplaceFun ) ->

	% Up to one transformation per source function:
	NewTable = ?table:add_new_entry( Src, ReplaceFun, Table ),
	get_remote_call_repl_helper( T, NewTable ).






% In this section, a term is traversed generically: as opposed to the transforms
% above, no assumption is made about the underlying structure of the term to
% transform.


% Transforms "blindly" (i.e. with no a-priori knowledge about its structure) the
% specified arbitrary term (possibly with nested subterms, as the function
% recurses in lists and tuples), calling specified transformer function on each
% instance of the specified type, in order to replace that instance by the
% result of that function.
%
% Returns an updated term, with these replacements made.
%
% Ex: the input term could be T={ a, [ "foo", {c, [2.0, 45]} ] } and the
% function might replace, for example, floats by <<bar>>; then T'={ a, [ "foo",
% { c, [<<bar>>, 45] } ] } would be returned.
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
-spec transform_term( term(), type_utils:primitive_type_description(),
				term_transformer(), user_data() ) -> { term(), user_data() }.

% Here the term is a list and this is the type we want to intercept:
transform_term( TargetTerm, _TypeDescription=list, TermTransformer, UserData )
  when is_list( TargetTerm ) ->

	{ TransformedTerm, NewUserData } = TermTransformer( TargetTerm, UserData ),

	transform_transformed_term( TransformedTerm, _TypeDescription=list,
							TermTransformer, NewUserData );


% Here the term is a list and we are not interested in them:
transform_term( TargetTerm, TypeDescription, TermTransformer, UserData )
  when is_list( TargetTerm ) ->

	transform_list( TargetTerm, TypeDescription, TermTransformer, UserData );


% Here the term is a tuple (or a record...), and we want to intercept them:
transform_term( TargetTerm, TypeDescription, TermTransformer, UserData )
  when is_tuple( TargetTerm )
	andalso ( TypeDescription =:= tuple orelse TypeDescription =:= record ) ->

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
transform_term( TargetTerm, TypeDescription, TermTransformer, UserData ) ->

	case type_utils:get_type_of( TargetTerm ) of

		TypeDescription ->
			TermTransformer( TargetTerm, UserData );

		_ ->
			% Unchanged:
			{ TargetTerm, UserData }

	end.



% Helper to traverse a list.
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



% Helper to traverse a tuple.
transform_tuple( TargetTuple, TypeDescription, TermTransformer, UserData ) ->

	% We do exactly as with lists:
	TermAsList = tuple_to_list( TargetTuple ),

	{ NewList, NewUserData } = transform_list( TermAsList, TypeDescription,
											   TermTransformer, UserData ),

	{ list_to_tuple( NewList ), NewUserData }.



% Helper to traverse a transformed term (ex: if looking for a {user_id, String}
% pair, we must recurse in nested tuples like: {3, {user_id, "Hello"}, 1}.
%
transform_transformed_term( TargetTerm, TypeDescription, TermTransformer,
							UserData ) ->

	case TermTransformer( TargetTerm, UserData ) of

		{ TransformedTerm, NewUserData } when is_list( TransformedTerm ) ->
			transform_list( TransformedTerm, TypeDescription, TermTransformer,
							NewUserData );

		{ TransformedTerm, NewUserData } when is_tuple( TransformedTerm ) ->
			transform_tuple( TransformedTerm, TypeDescription, TermTransformer,
							 NewUserData );

		% { ImmediateTerm, NewUserData } ->
		Other ->
			Other

	end.



% Returns a textual description of specified AST transforms.
-spec ast_transforms_to_string( ast_transforms() ) -> ustring().
ast_transforms_to_string( #ast_transforms{
							 local_types=MaybeLocalTypeTable,
							 remote_types=MaybeRemoteTypeTable,
							 local_calls=MaybeLocalCallTable,
							 remote_calls=MaybeRemoteCallTable,
							 transformed_module_name=ModuleName,
							 transformed_function_identifier=MaybeFunId,
							 transform_table=MaybeTransformTable,
							 transformation_state=TransfoState } ) ->

	Bullet = "  - ",

	LocalTypeStr = case MaybeLocalTypeTable of

		undefined ->
			"no transformation regarding local types";

		_ ->
			text_utils:format( "local types transformed based on ~ts",
				   [ ?table:to_string( MaybeLocalTypeTable, Bullet ) ] )

	end,

	RemoteTypeStr = case MaybeRemoteTypeTable of

		undefined ->
			"no transformation regarding remote types";

		_ ->
			text_utils:format( "remote types transformed based on ~ts",
				   [ ?table:to_string( MaybeRemoteTypeTable, Bullet ) ] )

	end,

	LocalCallStr = case MaybeLocalCallTable of

		undefined ->
			"no transformation regarding local calls";

		_ ->
			text_utils:format( "local calls transformed based on ~ts",
				   [ ?table:to_string( MaybeLocalCallTable, Bullet ) ] )

	end,

	RemoteCallStr = case MaybeRemoteCallTable of

		undefined ->
			"no transformation regarding remote calls";

		_ ->
			text_utils:format( "remote calls transformed based on ~ts",
				   [ ?table:to_string( MaybeRemoteCallTable, Bullet ) ] )

	end,

	ModuleString = case ModuleName of

		undefined ->
			"no target module specified";

		_ ->
			text_utils:format( "being applied to module '~ts'",
							   [ ModuleName ] )

	end,

	FunIdString = case MaybeFunId of

		undefined ->
			"no transformed function specified";

		{ FunName, Arity } ->
			text_utils:format( "applied to function ~ts/~B",
							   [ FunName, Arity ] )

	end,

	TransfoTableStr = case MaybeTransformTable of

		undefined ->
			"no AST transformation defined";

		TransfoTable ->
			text_utils:format( "AST transformations defined, "
							   "for following ~B triggers: ~w; "
							   "transformation state is:~n  ~p",
							   [ ?table:size( TransfoTable ),
								 ?table:keys( TransfoTable ),
								 TransfoState ] )

	end,

	TableString = text_utils:strings_to_string( [ LocalTypeStr, RemoteTypeStr,
			LocalCallStr, RemoteCallStr, ModuleString, FunIdString,
			TransfoTableStr ] ),

	text_utils:format( "AST transformations: ~ts", [ TableString ] ).



% The default transform_formatter() to be used:
-spec default_formatter( format_string(), format_values() ) -> ustring().
default_formatter( _FormatString, _FormatValue ) ->
	%text_utils:format( "[Myriad-Transforms] " ++ FormatString, FormatValue ).
	ok.
