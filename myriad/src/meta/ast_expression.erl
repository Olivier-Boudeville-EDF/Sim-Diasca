% Copyright (C) 2018-2022 Olivier Boudeville
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


% @doc Module in charge of handling <b>expressions defined with an AST</b>.
%
% See [http://erlang.org/doc/apps/erts/absform.html] for more information.
%
-module(ast_expression).


-type ast_expression() :: ast_base:ast_element().
% The description of an expression in an AST, with in-file location information.
%
% Ex: '{integer, {97,1}, 2}' or '{match, 117, {var,117,'A'}, {atom, 117,
% foobar}}', etc.
%
% Note that an expression is different from a pattern: even if they share at
% least some types of forms, they are to be interpreted differently (ex: their
% sub-elements are of the same kind as they are, and at least some rules
% differ).


-type ast_integer_expression() :: ast_expression().
% An expression that can be evaluated to an integer.


-type ast_field_init() :: ast_record:ast_untyped_record_field_definition().

-type ast_expressions() :: [ ast_expression() ].


-export_type([ ast_expression/0, ast_integer_expression/0,
			   ast_expressions/0 ]).


-export([ transform_expression/2, transform_expressions/2 ]).



% For the table macro:
-include("meta_utils.hrl").

% For the ast_transforms record:
-include("ast_transform.hrl").

% For the rec_guard define:
-include("ast_utils.hrl").


% Implementation notes:

% Note that any code transformation (typically of an expression) is to transform
% a given form into a (possibly empty) list of forms (rather than a single
% form).

% Allowing the definition of transformation functions allows to give full
% control to the user-specified transformations (ex: w.r.t. to recursion in
% parameters).



% Shorthands:

-type file_loc() :: ast_base:file_loc().

-type ast_case_clause() :: ast_clause:ast_case_clause().
-type ast_if_clause() :: ast_clause:ast_if_clause().

-type ast_pattern() :: ast_pattern:ast_pattern().

-type ast_body() :: ast_clause:ast_body().

-type ast_transforms() :: ast_transform:ast_transforms().

-type form() :: ast_base:form().


-type lc_generator_qualifier() ::
		{ 'generate', file_loc(), ast_pattern(), ast_expression() }.
% List-comprehension generator.


-type bitstring_generator_qualifier() ::
		{ 'b_generate', file_loc(), ast_pattern(), ast_expression() }.
% Bitstring generator.


-type ast_qualifier() :: ast_expression() | lc_generator_qualifier()
							| bitstring_generator_qualifier().
% A qualifier is one of the following: an expression-based filter, a
% list-comprehension generator or a bitstring generator.


-type expression_kind() :: 'call' | 'if' | 'case' | 'match' | 'bin'
		| 'unary_op' | 'binary_op' | 'simple_receive' | 'receive_with_after'
		| 'try' | 'remote' | 'catch' | 'cons' | 'lc' | 'bc' | 'tuple'
		| 'map_creation' | 'map_field_assoc' | 'map_field_exact'
		| 'record_creation' | 'record_index' | 'record_field'
		| 'record_field_other' | 'record_update' | 'block' | 'fun_definition'
		| 'fun_local' | 'fun_mfa_old' | 'fun_mfa' | 'var' | 'nil' | 'named_fun'
		| 'atomic_literal'.
% Allows to designate any kind of AST expression.


-type function_ref_expression() :: ast_expression().
% Expression designating a reference to a function (local or remote).


-type params_expression() :: [ ast_expression() ].
% List of expressions corresponding to function parameters.


-export_type([ expression_kind/0, function_ref_expression/0,
			   params_expression/0 ]).



% Conditional logging.
%
% Note: awfully verbose. Best option is to leave it disabled and to enable it
% selectively when recompiling specific target modules.

% Comment to disable logging (too detailed, almost untractable even to display):
%-define( log_traversal, ).


-ifdef(log_traversal).

 % To manage unused expressions:
 -define( e, E ).

 -define( log_enter(S,V), ast_utils:display_debug( S, V ) ).


 %-define( log_exit(S,V), ast_utils:display_debug( S, V ) ).
 -define( log_exit(S,V),
		  (Transforms#ast_transforms.transform_formatter)( S, V ) ).

-else. % log_traversal

 % Syntax error because of final comma:
 %-define( log_enter(S,V), ).

 % Terms unused:
 %-define( log_enter(S,V), ).

 % A term is constructed, but never used:
 %-define( log_enter(S,V), {S,V} ).

 -define( e, _E ).

 -define( log_enter(S,V), no_log ).
 -define( log_exit(S,V), no_log ).

-endif. % log_traversal


% @doc Transforms specified expression into a list of expressions.
%
% See section "7.4 Expressions" in
% [http://erlang.org/doc/apps/erts/absform.html].
%
-spec transform_expression( ast_expression(), ast_transforms() ) ->
									{ [ ast_expression() ], ast_transforms() }.


% Function call found:
%
% Once it is transformed, expected to fall within:
%
% "If E is a function call E_0(E_1, ..., E_k), then Rep(E) = {call, FILE_LOC,
% Rep(E_0), [Rep(E_1), ..., Rep(E_k)]}."
%
% or
%
% "If E is a function call E_m:E_0(E_1, ..., E_k), then Rep(E) = {call,
% FILE_LOC, {remote, FILE_LOC, Rep(E_m), Rep(E_0)}, [Rep(E_1), ..., Rep(E_k)]}.
%
transform_expression( ?e={ 'call', FileLoc, FunctionRef, Params },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming call expression ~p...", [ E ] ),

	% Maybe call expressions have to be transformed as a whole?
	Res = case Transforms#ast_transforms.transform_table of

		undefined ->
			transform_call( FileLoc, FunctionRef, Params, Transforms );

		TransformTable ->
			case ?table:lookup_entry( 'call', TransformTable ) of

				key_not_found ->
					transform_call( FileLoc, FunctionRef, Params, Transforms );

				{ value, CallTransformFun } ->
					% Returns directly {NewExprs, NewTransforms}:
					%
					% (note that this transform function is responsible for
					% recursing in the parameters if needed - which is probably
					% the case)
					%
					CallTransformFun( FileLoc, FunctionRef, Params,
									  Transforms )

			end

	end,

	?log_exit( "... returning call-originating expressions and state ~p",
			   [ Res ] ),

	Res;


% If expression found:
%
% "If E is an if expression if Ic_1 ; ... ; Ic_k end, where each Ic_i is an if
% clause, then Rep(E) = {'if', FILE_LOC, [Rep(Ic_1), ..., Rep(Ic_k)]}."
%
transform_expression( ?e={ 'if', FileLoc, Clauses }, Transforms ) ?rec_guard ->

	?log_enter( "Transforming if expression ~p...", [ E ] ),

	% Maybe if expressions have to be transformed as a whole?
	Res = case Transforms#ast_transforms.transform_table of

		undefined ->
			transform_if( FileLoc, Clauses, Transforms );

		TransformTable ->
			case ?table:lookup_entry( 'if', TransformTable ) of

				key_not_found ->
					transform_if( FileLoc, Clauses, Transforms );

				{ value, IfTransformFun } ->
					% Returns directly {NewExprs, NewTransforms}:
					%
					% (note that this transform function is responsible for
					% recursing in the parameters if needed - which is probably
					% the case)
					%
					IfTransformFun( FileLoc, Clauses, Transforms )

			end

	end,

	?log_exit( "... returning if-originating expressions and state ~p",
			   [ Res ] ),

	Res;




% Case expression found:
%
% "If E is a case expression case E_0 of Cc_1 ; ... ; Cc_k end, where E_0 is an
% expression and each Cc_i is a case clause, then Rep(E) =
% {'case', FILE_LOC, Rep(E_0), [Rep(Cc_1), ..., Rep(Cc_k)]}."
%
transform_expression( ?e={ 'case', FileLoc, TestExpression, CaseClauses },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming case expression ~p...", [ E ] ),

	% Maybe case expressions have to be transformed as a whole?
	Res = case Transforms#ast_transforms.transform_table of

		undefined ->
			transform_case( FileLoc, TestExpression, CaseClauses, Transforms );

		TransformTable ->
			case ?table:lookup_entry( 'case', TransformTable ) of

				key_not_found ->
					transform_case( FileLoc, TestExpression, CaseClauses,
									Transforms );

				{ value, CaseTransformFun } ->
					% Returns directly {NewExprs, NewTransforms}:
					%
					% (note that this transform function is responsible for
					% recursing in the parameters if needed - which is probably
					% the case)
					%
					CaseTransformFun( FileLoc, TestExpression, CaseClauses,
									  Transforms )

			end

	end,

	?log_exit( "... returning case-originating expressions and state ~p",
			   [ Res ] ),

	Res;



% Match expression found:
%
% "If E is a match operator expression P = E_0, where P is a pattern, then
% Rep(E) = {match, FILE_LOC, Rep(P), Rep(E_0)}."
%
transform_expression( ?e={ 'match', FileLoc, MatchPattern, MatchExpression },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming match expression ~p...", [ E ] ),

	% Maybe match expressions have to be transformed as a whole?
	Res = case Transforms#ast_transforms.transform_table of

		undefined ->
			transform_match( FileLoc, MatchPattern, MatchExpression,
							 Transforms );

		TransformTable ->
			case ?table:lookup_entry( 'match', TransformTable ) of

				key_not_found ->
					transform_match( FileLoc, MatchPattern, MatchExpression,
									 Transforms );

				{ value, MatchTransformFun } ->
					% Returns directly {NewExprs, NewTransforms}:
					%
					% (note that this transform function is responsible for
					% recursing in the parameters if needed - which is probably
					% the case)
					%
					MatchTransformFun( FileLoc, MatchPattern, MatchExpression,
									   Transforms )

			end

	end,

	?log_exit( "... returning match-originating expressions and state ~p",
			   [ Res ] ),

	Res;



% Bin expression found:
%
% "If E is a bitstring constructor <<E_1:Size_1/TSL_1, ..., E_k:Size_k/TSL_k>>,
% where each Size_i is an expression and each TSL_i is a type specificer list,
% then Rep(E) = {bin, FILE_LOC, [{bin_element, FILE_LOC, Rep(E_1), Rep(Size_1),
% Rep(TSL_1)}, ..., {bin_element, FILE_LOC, Rep(E_k), Rep(Size_k),
% Rep(TSL_k)}]}.
%
% For Rep(TSL), see below. An omitted Size_i is represented by default. An
% omitted TSL_i is represented by default."
%
transform_expression( ?e={ 'bin', FileLoc, BinElemPatterns },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming bin expression ~p...", [ E ] ),

	% TO-DO: add a 'bin' transform trigger.

	{ NewBinElemPattern, NewTransforms } =
		ast_bitstring:transform_bin_elements( BinElemPatterns, Transforms ),

	NewExpr = { 'bin', FileLoc, NewBinElemPattern },

	Res = { [ NewExpr ], NewTransforms },

	?log_exit( "... returning bin-originating expressions and state ~p",
			   [ Res ] ),

	Res;


% Unary operation expression found:
%
% "If E is an operator expression Op E_0, where Op is a unary operator, then
% Rep(E) = {op, FILE_LOC, Op, Rep(E_0)}."
%
transform_expression( ?e={ 'op', FileLoc, Operator, Operand },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming unary operation expression ~p...", [ E ] ),

	% TO-DO: add a 'unary_op' transform trigger.

	{ [ NewOperand ], NewTransforms } =
		transform_expression( Operand, Transforms ),

	NewExpr = { 'op', FileLoc, Operator, NewOperand },

	Res = { [ NewExpr ], NewTransforms },

	?log_exit( "... returning unary operation expressions and state ~p",
			   [ Res ] ),

	Res;


% Binary operation expression found:
%
% "If E is an operator expression E_1 Op E_2, where Op is a binary operator
% other than match operator =, then Rep(E) = {op, FILE_LOC, Op, Rep(E_1),
% Rep(E_2)}."
%
transform_expression( ?e={ 'op', FileLoc, Operator, LeftOperand, RightOperand },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming binary operation expression ~p...", [ E ] ),

	% TO-DO: add a 'binary_op' transform trigger.

	{ [ NewLeftOperand ], LeftTransforms } =
		transform_expression( LeftOperand, Transforms ),

	{ [ NewRightOperand ], RightTransforms } =
		transform_expression( RightOperand, LeftTransforms ),

	NewExpr = { 'op', FileLoc, Operator, NewLeftOperand, NewRightOperand },

	Res = { [ NewExpr ], RightTransforms },

	?log_exit( "... returning binary operation expressions and state ~p",
			   [ Res ] ),

	Res;



% Receive "simple" (with no 'after' clause) expression found:
%
% "If E is a receive expression receive Cc_1 ; ... ; Cc_k end, where each Cc_i
% is a case clause, then Rep(E) = {'receive', FILE_LOC, [Rep(Cc_1), ...,
% Rep(Cc_k)]}."
%
transform_expression( ?e={ 'receive', FileLoc, ReceiveClauses },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming simple receive expression ~p...", [ E ] ),

	% Maybe simple receive expressions have to be transformed as a whole?
	Res = case Transforms#ast_transforms.transform_table of

		undefined ->
			transform_simple_receive( FileLoc, ReceiveClauses, Transforms );

		TransformTable ->
			case ?table:lookup_entry( 'simple_receive', TransformTable ) of

				key_not_found ->
					transform_simple_receive( FileLoc, ReceiveClauses,
											  Transforms );

				{ value, ReceiveTransformFun } ->
					% Returns directly {NewExprs, NewTransforms}:
					%
					% (note that this transform function is responsible for
					% recursing in the parameters if needed - which is probably
					% the case)
					%
					ReceiveTransformFun( FileLoc, ReceiveClauses, Transforms )

			end

	end,

	?log_exit( "... returning simple receive expressions and state ~p",
			   [ Res ] ),

	Res;


% Receive expression with 'after' found:
%
% "If E is a receive expression receive Cc_1 ; ... ; Cc_k after E_0 -> B_t end,
% where each Cc_i is a case clause, E_0 is an expression, and B_t is a body,
% then Rep(E) = { 'receive', FILE_LOC, [Rep(Cc_1), ..., Rep(Cc_k)], Rep(E_0),
% Rep(B_t)}.
%
transform_expression( ?e={ 'receive', FileLoc, ReceiveClauses, AfterTest,
						   AfterExpressions }, Transforms ) ?rec_guard ->

	?log_enter( "Transforming receive expression with after ~p...", [ E ] ),

	% Maybe receive-with-after expressions have to be transformed as a whole?
	Res = case Transforms#ast_transforms.transform_table of

		undefined ->
			transform_receive_with_after( FileLoc, ReceiveClauses, AfterTest,
										  AfterExpressions, Transforms );

		TransformTable ->
			case ?table:lookup_entry( 'receive_with_after', TransformTable ) of

				key_not_found ->
					transform_receive_with_after( FileLoc, ReceiveClauses,
								AfterTest, AfterExpressions, Transforms );

				{ value, ReceiveTransformFun } ->
					% Returns directly {NewExprs, NewTransforms}:
					%
					% (note that this transform function is responsible for
					% recursing in the parameters if needed - which is probably
					% the case)
					%
					ReceiveTransformFun( FileLoc, ReceiveClauses, AfterTest,
										 AfterExpressions, Transforms )

			end

	end,

	?log_exit( "... returning receive-with-after expressions and state ~p",
			   [ Res ] ),

	Res;



% Try expression found (6 different forms managed in this single clause):
%
% - "If E is a try expression try B catch Tc_1 ; ... ; Tc_k end, where B is a
% body and each Tc_i is a catch clause, then Rep(E) = {'try', FILE_LOC, Rep(B),
% [], [Rep(Tc_1), ..., Rep(Tc_k)], []}."
%
% - "If E is a try expression try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n
% end, where B is a body, each Cc_i is a case clause, and each Tc_j is a catch
% clause, then Rep(E) = {'try', FILE_LOC, Rep(B), [Rep(Cc_1), ..., Rep(Cc_k)],
% [Rep(Tc_1), ..., Rep(Tc_n)], []}."
%
% - "If E is a try expression try B after A end, where B and A are bodies, then
% Rep(E) = {'try', FILE_LOC, Rep(B), [], [], Rep(A)}."
%
% - "If E is a try expression try B of Cc_1 ; ... ; Cc_k after A end, where B
% and A are a bodies, and each Cc_i is a case clause, then Rep(E) = {'try',
% FILE_LOC, Rep(B), [Rep(Cc_1), ..., Rep(Cc_k)], [], Rep(A)}."
%
% - "If E is a try expression try B catch Tc_1 ; ... ; Tc_k after A end, where B
% and A are bodies, and each Tc_i is a catch clause, then Rep(E) = {'try',
% FILE_LOC, Rep(B), [], [Rep(Tc_1), ..., Rep(Tc_k)], Rep(A)}."
%
% - "If E is a try expression try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n
% after A end, where B and A are a bodies, each Cc_i is a case clause, and each
% Tc_j is a catch clause, then Rep(E) = {'try', FILE_LOC, Rep(B), [Rep(Cc_1),
% ..., Rep(Cc_k)], [Rep(Tc_1), ..., Rep(Tc_n)], Rep(A)}."
%
transform_expression( ?e={ 'try', FileLoc, TryBody, TryClauses, CatchClauses,
						   AfterBody }, Transforms ) ?rec_guard ->

	?log_enter( "Transforming try expression ~p...", [ E ] ),

	% Maybe try expressions have to be transformed as a whole?
	Res = case Transforms#ast_transforms.transform_table of

		undefined ->
			transform_try( FileLoc, TryBody, TryClauses, CatchClauses,
						   AfterBody, Transforms );

		TransformTable ->
			case ?table:lookup_entry( 'try', TransformTable ) of

				key_not_found ->
					transform_try( FileLoc, TryBody, TryClauses, CatchClauses,
								   AfterBody, Transforms );

				{ value, TryTransformFun } ->
					% Returns directly {NewExprs, NewTransforms}:
					%
					% (note that this transform function is responsible for
					% recursing in the parameters if needed - which is probably
					% the case)
					%
					TryTransformFun( FileLoc, TryBody, TryClauses, CatchClauses,
									 AfterBody, Transforms )

			end

	end,

	?log_exit( "... returning try expressions and state ~p", [ Res ] ),

	Res;


% Useful indeed, typically should a define be replaced by
% module_name:function_name (see the myriad_spawn define for an example)
%
transform_expression( ?e={ 'remote', FileLoc, ModuleExpr, FunctionExpr },
					  Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Remote transform expression, with module "
	%   "expression '~p' and function one '~p'.",
	%   [ ModuleExpr, FunctionExpr ] ),

	% TO-DO: add a 'remote' transform trigger.

	?log_enter( "Transforming remote expression ~p...", [ E ] ),

	{ [ NewModuleExpr ], ModTransforms } =
		transform_expression( ModuleExpr, Transforms ),

	{ [ NewFunctionExpr ], FunTransforms } =
		transform_expression( FunctionExpr, ModTransforms ),

	NewExpr = { 'remote', FileLoc, NewModuleExpr, NewFunctionExpr },

	Res = { [ NewExpr ], FunTransforms },

	?log_exit( "... returning remote expressions and state ~p", [ Res ] ),

	Res;


% Catch expression found:
%
% "If E is a catch expression catch E_0, then Rep(E) = {'catch', FILE_LOC,
% Rep(E_0)}."
%
transform_expression( ?e={ 'catch', FileLoc, Expression },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming catch expression ~p...", [ E ] ),

	% Maybe catch expressions have to be transformed as a whole?
	Res = case Transforms#ast_transforms.transform_table of

		undefined ->
			transform_catch( FileLoc, Expression, Transforms );

		TransformTable ->
			case ?table:lookup_entry( 'catch', TransformTable ) of

				key_not_found ->
					transform_catch( FileLoc, Expression, Transforms );

				{ value, CatchTransformFun } ->
					% Returns directly {NewExprs, NewTransforms}:
					%
					% (note that this transform function is responsible for
					% recursing in the parameters if needed - which is probably
					% the case)
					%
					CatchTransformFun( FileLoc, Expression, Transforms )

			end

	end,

	?log_exit( "... returning catch expressions and state ~p", [ Res ] ),

	Res;


% Cons expression found:
%
% "If E is a cons skeleton [E_h | E_t], then Rep(E) = {cons, FILE_LOC, Rep(E_h),
% Rep(E_t)}."
%
% Head and Tail members are expressions (not just patterns), as a member can
% for example be : {call,56, {remote, ...
%
transform_expression( ?e={ 'cons', FileLoc, HeadExpression, TailExpression },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming cons expression ~p...", [ E ] ),

	% TO-DO: add a 'cons' transform trigger.

	{ [ NewHeadExpression ], HeadTranforms } =
		transform_expression( HeadExpression, Transforms ),


	{ [ NewTailExpression ], TailTransforms } =
		transform_expression( TailExpression, HeadTranforms ),

	NewExpr = { 'cons', FileLoc, NewHeadExpression, NewTailExpression },

	Res = { [ NewExpr ], TailTransforms },

	?log_exit( "... returning cons expressions and state ~p", [ Res ] ),

	Res;


% List comprehension found:
%
% "If E is a list comprehension [E_0 || Q_1, ..., Q_k], where each Q_i is a
% qualifier, then Rep(E) = {lc, FILE_LOC, Rep(E_0), [Rep(Q_1), ...,
% Rep(Q_k)]}. For Rep(Q), see below."
%
transform_expression( ?e={ 'lc', FileLoc, Expression, Qualifiers },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming list comprehension ~p...", [ E ] ),

	% TO-DO: add a 'lc' transform trigger.

	{ [ NewExpression ], ExprTransforms } =
		transform_expression( Expression, Transforms ),

	{ NewQualifiers, QualTransforms } =
		transform_qualifiers( Qualifiers, ExprTransforms ),

	NewExpr = { 'lc', FileLoc, NewExpression, NewQualifiers },

	Res = { [ NewExpr ], QualTransforms },

	?log_exit( "... returning list comprehension ~p and state ", [ Res ] ),

	Res;


% Bitstring comprehension found:
%
% "If E is a bitstring comprehension <<E_0 || Q_1, ..., Q_k>>, where each Q_i is
% a qualifier, then Rep(E) = {bc, FILE_LOC, Rep(E_0), [Rep(Q_1), ...,
% Rep(Q_k)]}."
%
transform_expression( ?e={ 'bc', FileLoc, Expression, Qualifiers },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming bitstring comprehension ~p...", [ E ] ),

	% TO-DO: add a 'bc' transform trigger.

	{ [ NewExpression ], ExprTransforms } =
		transform_expression( Expression, Transforms ),

	{ NewQualifiers, QualTransforms } =
		transform_qualifiers( Qualifiers, ExprTransforms ),

	NewExpr = { 'bc', FileLoc, NewExpression, NewQualifiers },

	Res = { [ NewExpr ], QualTransforms },

	?log_exit( "... returning bitstring comprehension ~p and state ", [ Res ] ),

	Res;


% Tuple skeleton found:
%
% "If E is a tuple skeleton {E_1, ..., E_k}, then Rep(E) = {tuple, FILE_LOC,
% [Rep(E_1), ..., Rep(E_k)]}."
%
transform_expression( ?e={ 'tuple', FileLoc, Expressions },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming tuple skeleton ~p...", [ E ] ),

	% TO-DO: add a 'tuple' transform trigger.

	{ NewExpressions, NewTransforms } =
		transform_expressions( Expressions, Transforms ),

	NewExpr = { 'tuple', FileLoc, NewExpressions },

	Res = { [ NewExpr ], NewTransforms },

	?log_exit( "... returning tuple skeleton and state ~p", [ Res ] ),

	Res;



% Map creation found:
%
% "If E is a map creation #{A_1, ..., A_k}, where each A_i is an association
% E_i_1 => E_i_2 or E_i_1 := E_i_2, then Rep(E) = {map, FILE_LOC, [Rep(A_1),
% ..., Rep(A_k)]}."
%
transform_expression( ?e={ 'map', FileLoc, Expressions },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming map creation ~p...", [ E ] ),

	% TO-DO: add a 'map_creation' transform trigger.

	{ NewExpressions, NewTransforms } =
		transform_expressions( Expressions, Transforms ),

	NewExpr = { 'map', FileLoc, NewExpressions },

	Res = { [ NewExpr ], NewTransforms },

	?log_exit( "... returning map creation and state ~p", [ Res ] ),

	Res;


% Map update found:
%
% "If E is a map update E_0#{A_1, ..., A_k}, where each A_i is an association
% E_i_1 => E_i_2 or E_i_1 := E_i_2, then Rep(E) = {map, FILE_LOC, Rep(E_0),
% [Rep(A_1), ..., Rep(A_k)]}."
%
transform_expression( ?e={ 'map', FileLoc, MapRefExpression, AssocExpressions },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming map update ~p...", [ E ] ),

	% TO-DO: add a 'map_update' transform trigger.

	{ [ NewMapRefExpression | NewAssocExpressions ], NewTransforms } =
		transform_expressions( [ MapRefExpression | AssocExpressions ],
							   Transforms ),

	NewExpr = { 'map', FileLoc, NewMapRefExpression, NewAssocExpressions },

	Res = { [ NewExpr ], NewTransforms },

	?log_exit( "... returning map update and state ~p", [ Res ] ),

	Res;


% Map field association found:
%
% "If A is an association K => V, then Rep(A) = {map_field_assoc, FILE_LOC,
% Rep(K), Rep(V)}."
%
transform_expression( ?e={ 'map_field_assoc', FileLoc, KeyExpression,
						   ValueExpression }, Transforms ) ?rec_guard ->

	?log_enter( "Transforming map association ~p...", [ E ] ),

	% TO-DO: add a 'map_field_assoc' transform trigger.

	{ [ NewKeyExpression ], KeyTransforms } =
		transform_expression( KeyExpression, Transforms ),

	{ [ NewValueExpression ], ValueTransforms } =
		transform_expression( ValueExpression, KeyTransforms ),

	NewExpr = { 'map_field_assoc', FileLoc, NewKeyExpression,
				NewValueExpression },

	Res = { [ NewExpr ], ValueTransforms },

	?log_exit( "... returning map association and state ~p", [ Res ] ),

	Res;


% Map exact field association found:
%
% "If A is an association K := V, then Rep(A) = {map_field_exact, FILE_LOC,
% Rep(K), Rep(V)}."
%
transform_expression( ?e={ 'map_field_exact', FileLoc, KeyExpression,
						   ValueExpression }, Transforms ) ?rec_guard ->

	?log_enter( "Transforming map exact association ~p...", [ E ] ),

	% TO-DO: add a 'map_field_exact' transform trigger.

	{ [ NewKeyExpression ], KeyTransforms } =
		transform_expression( KeyExpression, Transforms ),

	{ [ NewValueExpression ], ValueTransforms } =
		transform_expression( ValueExpression, KeyTransforms ),

	NewExpr = { 'map_field_exact', FileLoc, NewKeyExpression,
				NewValueExpression },

	Res = { [ NewExpr ], ValueTransforms },

	?log_exit( "... returning map exact association and state ~p", [ Res ] ),

	Res;



% No 'struct' to be managed (cf. erl_id_trans, commented-out).



% Record creation expression found:
%
% "If E is a record creation #Name{Field_1=E_1, ..., Field_k=E_k}, where each
% Field_i is an atom or _, then Rep(E) = {record, FILE_LOC, Name,
% [{record_field, FILE_LOC, Rep(Field_1), Rep(E_1)}, ..., {record_field,
% FILE_LOC, Rep(Field_k), Rep(E_k)}]}."
%
transform_expression( ?e={ 'record', FileLoc, RecordName, FieldInits },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming record creation expression ~p...", [ E ] ),

	% TO-DO: add a 'record_creation' transform trigger.

	{ NewFieldInits, NewTransforms } =
		transform_record_field_inits( FieldInits, Transforms ),

	NewExpr = { 'record', FileLoc, RecordName, NewFieldInits },

	Res = { [ NewExpr ], NewTransforms },

	?log_exit( "... returning record creation expression and state ~p",
			   [ Res ] ),

	Res;


% Record index expression found:
%
% "If E is a record field index #Name.Field, where Field is an atom, then Rep(E)
% = {record_index, FILE_LOC, Name, Rep(Field)}."
%
transform_expression( ?e={ 'record_index', FileLoc, RecordName, FieldName },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming record index expression ~p...", [ E ] ),

	% TO-DO: add a 'record_index' transform trigger.

	{ [ NewFieldName ], NewTransforms } =
		transform_expression( FieldName, Transforms ),

	NewExpr = { 'record_index', FileLoc, RecordName, NewFieldName },

	Res = { [ NewExpr ], NewTransforms },

	?log_exit( "... returning record index expression and state ~p", [ Res ] ),

	Res;


% Record field access found:
%
% "If E is a record field access E_0#Name.Field, where Field is an atom, then
% Rep(E) = {record_field, FILE_LOC, Rep(E_0), Name, Rep(Field)}."
%
transform_expression( ?e={ 'record_field', FileLoc, RecordRef, RecordName,
						   FieldName }, Transforms ) ?rec_guard ->

	?log_enter( "Transforming record field access expression ~p...", [ E ] ),

	% TO-DO: add a 'record_field' transform trigger.

	{ [ NewRecordRef ], RefTransforms } =
		transform_expression( RecordRef, Transforms ),

	{ [ NewFieldName ], NameTransforms } =
		transform_expression( FieldName, RefTransforms ),

	NewExpr = { 'record_field', FileLoc, NewRecordRef, RecordName,
				NewFieldName },

	Res = { [ NewExpr ], NameTransforms },

	?log_exit( "... returning record field access expression and state ~p",
			   [ Res ] ),

	Res;


% Record field found:
%
% (not found apparently in http://erlang.org/doc/apps/erts/absform.html)
%
transform_expression( ?e={ 'record_field', FileLoc, RecordRef, Field },
					  Transforms ) ?rec_guard ->

	% Expected never to be displayed:
	ast_utils:display_warning( "Clause about record field expression "
							   "actually triggered." ),

	?log_enter( "Transforming record field expression ~p...", [ E ] ),

	% TO-DO: add a 'record_field_other' transform trigger.

	{ [ NewRecordRef ], RefTransforms } =
		transform_expression( RecordRef, Transforms ),

	{ [ NewField ], FieldTransforms } =
		transform_expression( Field, RefTransforms ),

	NewExpr = { 'record_field', FileLoc, NewRecordRef, NewField },

	Res = { [ NewExpr ], FieldTransforms },

	?log_exit( "... returning record field expression and state ~p", [ Res ] ),

	Res;


% Record update found:
%
% "If E is a record update E_0#Name{Field_1=E_1, ..., Field_k=E_k}, where each
% Field_i is an atom, then Rep(E) = {record, FILE_LOC, Rep(E_0), Name,
% [{record_field, FILE_LOC, Rep(Field_1), Rep(E_1)}, ...,
% {record_field, FILE_LOC, Rep(Field_k), Rep(E_k)}]}."
%
transform_expression( ?e={ 'record', FileLoc, RecordRef, RecordName,
						   FieldUpdates }, Transforms ) ?rec_guard ->

	?log_enter( "Transforming record update expression ~p...", [ E ] ),

	% TO-DO: add a 'record_update' transform trigger.

	{ [ NewRecordRef ], RefTransforms } =
		transform_expression( RecordRef, Transforms ),

	{ NewFieldUpdates, UpTransforms } =
		transform_record_field_updates( FieldUpdates, RefTransforms ),

	NewExpr = { 'record', FileLoc, NewRecordRef, RecordName, NewFieldUpdates },

	Res = { [ NewExpr ], UpTransforms },

	?log_exit( "... returning record update expression and state ~p", [ Res ] ),

	Res;



% Block expression found:
%
% "If E is a block expression begin B end, where B is a body, then Rep(E) =
% {block, FILE_LOC, Rep(B)}."
%
transform_expression( ?e={ 'block', FileLoc, Expressions },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming block expression ~p...", [ E ] ),

	% TO-DO: add a 'block' transform trigger.

	% Unfolds this block into a sequence of expressions:
	{ NewExpressions, NewTransforms } =
		transform_expressions( Expressions, Transforms ),

	NewExpr = { 'block', FileLoc, NewExpressions },

	Res = { [ NewExpr ], NewTransforms },

	?log_exit( "... returning block expression and state ~p", [ Res ] ),

	Res;



% Fun expression found:


% "If E is a fun expression fun Fc_1 ; ... ; Fc_k end, where each Fc_i is a
% function clause, then Rep(E) = {'fun', FILE_LOC, {clauses, [Rep(Fc_1), ...,
% Rep(Fc_k)]}}."
%
transform_expression( ?e={ 'fun', FileLoc, { 'clauses', FunctionClauses } },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming clause-based fun expression ~p...", [ E ] ),

	% TO-DO: add a 'fun_definition' transform trigger.

	{ NewFunctionClauses, NewTransforms } =
		ast_clause:transform_function_clauses( FunctionClauses, Transforms ),

	NewExpr = { 'fun', FileLoc, { 'clauses', NewFunctionClauses } },

	Res = { [ NewExpr ], NewTransforms },

	?log_exit( "... returning clause-based fun expression and state ~p",
			   [ Res ] ),

	Res;


% "If E is a fun expression fun Name/Arity, then Rep(E) = {'fun', FILE_LOC,
% {function, Name, Arity}}."
%
transform_expression( E={ 'fun', _FileLoc, { 'function', _Name, _Arity } },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming local fun expression ~p...", [ E ] ),

	% TO-DO: add a 'fun_local' transform trigger.

	%NewName = transform_expression( Name, Transforms ),

	%NewArity = transform_expression( Arity, Transforms ),

	% Apparently no possible transformation, already fully resolved:
	% (see expr/1 in erl_id_trans)
	%
	%NewExpr = { 'fun', FileLoc, { function, NewName, NewArity } },
	NewExpr = E,

	Res = { [ NewExpr ], Transforms },

	?log_exit( "... returning local fun expression and state ~p", [ Res ] ),

	Res;


% Managing specifically the fact that, before Erlang/OTP R15, Rep(E) =
% {'fun', FILE_LOC, {function, Module, Name, Arity}}.
%
transform_expression( E={ 'fun', _FileLoc,
						  _F={ 'function', Module, Name, Arity } },
					  Transforms ) when is_atom( Module )
		andalso is_atom( Name ) andalso is_integer( Arity )
		?andalso_rec_guard ->

	%ast_utils:display_warning( "Pre-R15 fun expression '~p' detected, "
	%                           "this warning should be silenced.", [ E ] ),

	?log_enter( "Transforming pre-R15 fun expression ~p...", [ E ] ),

	% TO-DO: add a 'fun_mfa_old' transform trigger.

	Res = { [ E ], Transforms },

	?log_exit( "... returning pre-R15 fun expression and state ~p", [ Res ] ),

	Res;


% "If E is a fun expression fun Module:Name/Arity, then Rep(E) = {'fun',
% FILE_LOC, {function, Rep(Module), Rep(Name), Rep(Arity)}}."
%
% Since R15, fun M:F/A can be obtained through variables.
%
transform_expression( ?e={ 'fun', FileLoc,
				_F={ 'function', ModuleName, FunctionName, FunctionArity } },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming remote fun expression ~p...", [ E ] ),

	% TO-DO: add a 'fun_mfa' transform trigger.

	{ [ NewModuleName ], ModTransforms } =
		transform_expression( ModuleName, Transforms ),

	{ [ NewFunctionName ], NameTransforms } =
		transform_expression( FunctionName, ModTransforms ),

	{ [ NewFunctionArity ], ArityTransforms } =
		transform_expression( FunctionArity, NameTransforms ),

	NewExpr = { 'fun', FileLoc,
			{ 'function', NewModuleName, NewFunctionName, NewFunctionArity } },

	Res = { [ NewExpr ], ArityTransforms },

	?log_exit( "... returning remote fun expression and state ~p", [ Res ] ),

	Res;


% "If E is a variable V, then Rep(E) = {var, FILE_LOC, A}, where A is an atom
% with a printname consisting of the same characters as V."
%
transform_expression( E={ 'var', _FileLoc, _VarAtomName },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming var expression with clauses ~p...", [ E ] ),

	% TO-DO: add a 'var' transform trigger.

	% Currently names not transformed:
	%NewVarAtomName = VarAtomName,

	%NewExpr = { 'var', FileLoc, NewVarAtomName },
	NewExpr = E,

	Res= { [ NewExpr ], Transforms },

	?log_exit( "... returning var expression with clauses and state ~p",
			   [ Res ] ),

	Res;


% "If E is nil, [], then Rep(E) = {nil, FILE_LOC}."
transform_expression( E={ 'nil', _FileLoc }, Transforms ) ?rec_guard ->

	?log_enter( "Transforming nil expression with clauses ~p...", [ E ] ),

	% TO-DO: add a 'nil' transform trigger.

	% Currently not transformed:
	NewExpr = E,

	Res= { [ NewExpr ], Transforms },

	?log_exit( "... returning nil expression with clauses and state ~p",
			   [ Res ] ),

	Res;



% "If E is a fun expression fun Name Fc_1 ; ... ; Name Fc_k end, where Name is a
% variable and each Fc_i is a function clause, then Rep(E) =
% {named_fun, FILE_LOC, Name, [Rep(Fc_1), ..., Rep(Fc_k)]}."
%
transform_expression( ?e={ 'named_fun', FileLoc, Name, FunctionClauses },
					  Transforms ) ?rec_guard ->

	?log_enter( "Transforming named fun expression ~p...", [ E ] ),

	% TO-DO: add a 'named_fun' transform trigger.

	{ NewFunctionClauses, NewTransforms } =
		ast_clause:transform_function_clauses( FunctionClauses, Transforms ),

	NewExpr = { 'named_fun', FileLoc, Name, NewFunctionClauses },

	Res = { [ NewExpr ], NewTransforms },

	?log_exit( "... returning named fun expression and state ~p", [ Res ] ),

	Res;


% "If E is an atomic literal L, then Rep(E) = Rep(L)."
%
% Wish type_utils:get_immediate_types/0 could be used in a guard.
%
transform_expression( E={ AtomicLiteralType, _FileLoc, _Value },
					  Transforms ) when ( AtomicLiteralType =:= 'atom' orelse
		AtomicLiteralType =:= 'char' orelse AtomicLiteralType =:= 'float' orelse
		AtomicLiteralType =:= 'integer' orelse AtomicLiteralType =:= 'string' )
		?andalso_rec_guard ->

	% TO-DO: add a 'atomic_literal' transform trigger.

	{ NewExpr, NewTransforms } = ast_value:transform_value( E, Transforms ),

	{ [ NewExpr ], NewTransforms };


% Partial catch-all:
transform_expression( Expression, Transforms )
  when is_record( Transforms, ast_transforms ) ->

	% Was incorrect, as patterns are not a special case of expressions:

	% None of the expressions above matched, this expression must be a pattern
	% then:
	%
	%ast_pattern:transform_pattern( Expression, Transforms ).

	ast_utils:raise_error( [ unexpected_expression, Expression ] );


% Final catch-all:
transform_expression( Expression, Transforms ) ->
	ast_utils:raise_error( [ transforms_expected, Transforms, Expression ] ).





% Section centralising the transformations that are specific to a kind of
% expression.



% @doc Transforms an expression corresponding to a function call into another
% one (exactly).
%
% (default traversal implementation)
%
-spec transform_call( file_loc(), function_ref_expression(),
					  params_expression(), ast_transforms() ) ->
							{ [ ast_expression() ], ast_transforms() }.
transform_call( FileLoc, FunctionRef, Params, Transforms ) ?rec_guard ->

	?log_enter( "Transforming 'call', to function reference ~p",
				[ FunctionRef ] ),

	{ [ TransformedFunctionRef ], FuncTransforms } =
		transform_expression( FunctionRef, Transforms ),

	%?log_enter( "Transforming call parameters ~p",
	%			[ Params ] ),

	% First recurses, knowing that function parameters are expressions:
	{ [ NewParams ], ParamsTransforms } =
		transform_expressions( Params, FuncTransforms ),

	NewArity = length( NewParams ),

	{ [ FinalFunctionRef ], FinalTransforms } = transform_call_expression(
						TransformedFunctionRef, NewArity, ParamsTransforms ),

	NewExpr = { 'call', FileLoc, FinalFunctionRef, NewParams },

	{ [ NewExpr ], FinalTransforms }.



% @doc Transforms an expression corresponding to an 'if' into another one
% (exactly).
%
% (default traversal implementation)
%
-spec transform_if( file_loc(), [ ast_if_clause() ], ast_transforms() ) ->
							{ [ ast_expression() ], ast_transforms() }.
transform_if( FileLoc, Clauses, Transforms ) ?rec_guard ->

	{ NewClauses, NewTransforms } =
		ast_clause:transform_if_clauses( Clauses, Transforms ),

	NewExpr = { 'if', FileLoc, NewClauses },

	{ [ NewExpr ], NewTransforms }.



% @doc Transforms an expression corresponding to a 'case' into another one
% (exactly).
%
% (default traversal implementation)
%
-spec transform_case( file_loc(), ast_expression(), [ ast_case_clause() ],
			ast_transforms() ) -> { [ ast_expression() ], ast_transforms() }.
transform_case( FileLoc, TestExpression, CaseClauses, Transforms ) ?rec_guard ->

	{ [ NewTestExpression ], TestTransforms } =
		transform_expression( TestExpression, Transforms ),

	{ NewCaseClauses, CaseTransforms } =
		ast_clause:transform_case_clauses( CaseClauses, TestTransforms ),

	NewExpr = { 'case', FileLoc, NewTestExpression, NewCaseClauses },

	{ [ NewExpr ], CaseTransforms }.




% @doc Transforms an expression corresponding to a 'match' into another one
% (exactly).
%
% (default traversal implementation)
%
-spec transform_match( file_loc(), ast_pattern(), ast_expression(),
			ast_transforms() ) -> { [ ast_expression() ], ast_transforms() }.
transform_match( FileLoc, MatchPattern, MatchExpression,
				 Transforms ) ?rec_guard ->

	{ NewMatchPattern, PatternTransforms } =
		ast_pattern:transform_pattern( MatchPattern, Transforms ),

	%?log_enter( "Transforming match expression: ~p", [ MatchExpression ] ),

	{ [ NewMatchExpression ], ExprTransforms } =
		transform_expression( MatchExpression, PatternTransforms ),

	%ast_utils:display_debug( "New match expression:~p",
	%						  [ NewMatchExpression ] ),

	NewExpr = { 'match', FileLoc, NewMatchPattern, NewMatchExpression },

	{ [ NewExpr ], ExprTransforms }.



% @doc Transforms an expression corresponding to a simple 'receive' into another
% one (exactly).
%
% (default traversal implementation)
%
-spec transform_simple_receive( file_loc(), [ ast_case_clause() ],
			ast_transforms() ) -> { [ ast_expression() ], ast_transforms() }.
transform_simple_receive( FileLoc, ReceiveClauses, Transforms ) ?rec_guard ->

	% 'case' clauses relevant here:
	{ NewReceiveClauses, NewTransforms } =
		ast_clause:transform_case_clauses( ReceiveClauses, Transforms ),

	NewExpr = { 'receive', FileLoc, NewReceiveClauses },

	{ [ NewExpr ], NewTransforms }.



% @doc Transforms an expression corresponding to a simple 'receive' into another
% one (exactly).
%
% (default traversal implementation)
%
-spec transform_receive_with_after( file_loc(), [ ast_case_clause() ],
		ast_expression(), ast_body(), ast_transforms() ) ->
								{ [ ast_expression() ], ast_transforms() }.
transform_receive_with_after( FileLoc, ReceiveClauses, AfterTest,
							  AfterBody, Transforms ) ?rec_guard ->

	% 'case' clauses relevant here:
	{ NewReceiveClauses, CaseTransforms } =
		ast_clause:transform_case_clauses( ReceiveClauses, Transforms ),

	{ [ NewAfterTest ], AfterTestTransforms } =
		transform_expression( AfterTest, CaseTransforms ),

	% Not exactly, as this is a body:
	%
	%{ NewAfterExpressions, AfterTransforms } =
	%    transform_expressions( AfterExpressions, AfterTestTransforms ),
	%
	{ NewAfterBody, AfterTransforms } =
		ast_clause:transform_body( AfterBody, AfterTestTransforms ),

	NewExpr = { 'receive', FileLoc, NewReceiveClauses, NewAfterTest,
				 NewAfterBody },

	{ [ NewExpr ], AfterTransforms }.



% @doc Transforms an expression corresponding to a 'try' into another one
% (exactly).
%
% (default traversal implementation)
%
-spec transform_try( file_loc(), ast_body(), [ ast_case_clause() ],
					 [ ast_case_clause() ], ast_body(), ast_transforms() ) ->
							{ [ ast_expression() ], ast_transforms() }.
transform_try( FileLoc, TryBody, TryClauses, CatchClauses, AfterBody,
			   Transforms ) ?rec_guard ->

	{ NewTryBody, TryBodyTranforms } =
		ast_clause:transform_body( TryBody, Transforms ),

	{ NewTryClauses, TryTransforms } =
		ast_clause:transform_try_clauses( TryClauses, TryBodyTranforms ),

	{ NewCatchClauses, CatchTransforms } =
		ast_clause:transform_catch_clauses( CatchClauses, TryTransforms ),

	{ NewAfterBody, AfterTransforms } =
		ast_clause:transform_body( AfterBody, CatchTransforms ),

	NewExpr = { 'try', FileLoc, NewTryBody, NewTryClauses, NewCatchClauses,
				NewAfterBody },

	{ [ NewExpr ], AfterTransforms }.



% @doc Transforms an expression corresponding to a 'catch' into another one
% (exactly).
%
% (default traversal implementation)
%
-spec transform_catch( file_loc(), ast_expression(), ast_transforms() ) ->
							{ [ ast_expression() ], ast_transforms() }.
transform_catch( FileLoc, Expression, Transforms ) ?rec_guard ->

	{ [ NewExpression ], NewTransforms } =
		transform_expression( Expression, Transforms ),

	NewExpr = { 'catch', FileLoc, NewExpression },

	{ [ NewExpr ], NewTransforms }.




% @doc Transforms specified list of expressions.
%
% Defined for convenience.
%
-spec transform_expressions( [ ast_expression() ], ast_transforms() ) ->
									{ [ ast_expression() ], ast_transforms() }.
transform_expressions( Expressions, Transforms ) ?rec_guard ->

	% An expression is transformed into a *list* of expressions: (probably
	% lists:mapfoldl/3 should be replaced by ad-hoc code, to ease debugging)
	%
	{ ExprLists, NewTransforms } = lists:mapfoldl(
			fun transform_expression/2, _Acc0=Transforms, _List=Expressions ),

	% We do not want expressions to remain nested over two levels:
	OneLevelExprList = merge_expression_lists( ExprLists ),

	{ OneLevelExprList, NewTransforms }.



% @doc Removes a single depth of nesting (not an arbitrary flattening) regarding
% expressions.
%
% (helper)
%
% Note: directly inspired from list_utils:flatten_once/1, yet we do not want to
% bootstrap the full list_utils module just for that.
%
merge_expression_lists( List ) ->
	%ast_utils:display_trace( "merging expression list ~p", [ List ] ),
	merge_expression_lists( List, _Acc=[] ).



% (helper)
%
% Note: not using simply 'lists:reverse( Acc );' and a (more efficient) 'L ++
% Acc', as we would end up with [1,[3,4],2] - whereas we want to preserve order.
%
merge_expression_lists( [], Acc ) ->
	Acc;

merge_expression_lists( [ L | T ], Acc ) when is_list( L ) ->
	merge_expression_lists( T, Acc ++ L );

merge_expression_lists( [ Unexpected | _T ], _Acc ) ->
	throw( { not_a_list, Unexpected } ).



% @doc Transforms specified qualifiers.
%
% Allows filters to be both guard tests and general expressions.
%
% See also: lc_bc_quals/1 in erl_id_trans
%
-spec transform_qualifiers( [ ast_qualifier() ], ast_transforms() ) ->
									{ [ ast_qualifier() ], ast_transforms() }.
transform_qualifiers( Qualifiers, Transforms ) ?rec_guard ->
	lists:mapfoldl( fun transform_qualifier/2, _Acc0=Transforms,
					_List=Qualifiers ).



% @doc Transforms specified qualifier.
-spec transform_qualifier( ast_qualifier(), ast_transforms() ) ->
									{ ast_qualifier(), ast_transforms() }.

% "If Q is a (lc) generator P <- E, where P is a pattern and E is an expression,
% then Rep(Q) = {generate, FILE_LOC, Rep(P), Rep(E)}."
%
transform_qualifier( _Qualifier={ 'generate', FileLoc, Pattern, Expression },
					 Transforms ) ?rec_guard ->

	{ NewPattern, PatTransforms } =
		ast_pattern:transform_pattern( Pattern, Transforms ),

	{ [ NewExpression ], ExpTransforms } =
		transform_expression( Expression, PatTransforms ),

	NewExpr = { 'generate', FileLoc, NewPattern, NewExpression },

	{ NewExpr, ExpTransforms };


% "If Q is a bitstring generator P <= E, where P is a pattern and E is an
% expression, then Rep(Q) = {b_generate, FILE_LOC, Rep(P), Rep(E)}."
%
transform_qualifier( _Qualifier={ 'b_generate', FileLoc, Pattern, Expression },
					 Transforms ) ?rec_guard ->

	{ NewPattern, PatTransforms } =
		ast_pattern:transform_pattern( Pattern, Transforms ),

	{ [ NewExpression ], ExpTransforms } =
		transform_expression( Expression, PatTransforms ),

	NewExpr = { 'b_generate', FileLoc, NewPattern, NewExpression },

	{ NewExpr, ExpTransforms };


% "If Q is a filter E, where E is an expression, then Rep(Q) = Rep(E)."
transform_qualifier( _Qualifier=Expression, Transforms ) ?rec_guard ->
	{ [ E ], NewTransforms } = transform_expression( Expression, Transforms ),
	{ E, NewTransforms }.




% (corresponds to record_inits/1 in erl_id_trans)
%
% Field names are full expressions here, but only atoms are allowed by the
% linter.
%
% (helper)
%
-spec transform_record_field_inits( [ ast_field_init() ], ast_transforms() ) ->
				{ [ ast_field_init() ], ast_transforms() }.
transform_record_field_inits( RecordFieldInits, Transforms ) ?rec_guard ->

	%ast_utils:display_trace( "Transforming record field init ~p.",
	%						  [ RecordFieldInits ] ),

	% An expression is transformed into a *list* of expressions:
	{ ExprLists, NewTransforms } = lists:mapfoldl(
		fun transform_record_field_init/2, _Acc0=Transforms,
									_List=RecordFieldInits ),

	% We do not want expressions to remain nested over two levels:
	OneLevelExprList = merge_expression_lists( ExprLists ),

	%ast_utils:display_trace( "record field inits ~n~p transformed as:~n~p",
	%						  [ RecordFieldInits, OneLevelExprList ] ),

	{ OneLevelExprList, NewTransforms }.



% Includes the case where FieldName is '_':
transform_record_field_init( { 'record_field', FileLocField,
		FieldNameASTAtom={ atom, _FileLocAtom, _FieldName }, FieldValue },
							 Transforms ) ?rec_guard ->

	{ [ NewFieldValue ], NewTransforms } =
		transform_expression( FieldValue, Transforms ),

	NewExpr = { 'record_field', FileLocField, FieldNameASTAtom, NewFieldValue },

	{ [ NewExpr ], NewTransforms };

transform_record_field_init( { 'record_field', FileLocField, OtherForm,
							   _FieldValue }, _Transforms ) ?rec_guard ->

	trace_utils:error_fmt( "Unexpected record field initialisation "
		"at ~ts:~n ~p",
		[ ast_utils:file_loc_to_string( FileLocField ), OtherForm ] ),

	ast_utils:raise_error( [ unexpected_record_field_init, OtherForm ] ).



% (corresponds to record_updates/1 in erl_id_trans)
%
% Field names are full expressions here, but only atoms are allowed by the
% linter.
%
% (helper)
%
transform_record_field_updates( RecordFieldUpdates, Transforms ) ?rec_guard ->

	?log_enter( "Transforming record field updates ~p",
				[ RecordFieldUpdates ] ),

	_Res = lists:mapfoldl( fun transform_record_field_update/2,
						   _Acc0=Transforms, _List=RecordFieldUpdates ).

	%ast_utils:display_debug( "transformed record field updates: ~p",
	%						 [ element( 1, Res ) ] ),

	%Res.


transform_record_field_update( { 'record_field', FileLocField,
		FieldNameASTAtom={ atom, _FileLocAtom, _FieldName }, FieldValue },
							   Transforms ) ?rec_guard ->

	{ [ NewFieldValue ], NewTransforms } =
		transform_expression( FieldValue, Transforms ),

	NewExpr = { record_field, FileLocField, FieldNameASTAtom, NewFieldValue },

	% Single expression here by design:
	{ NewExpr, NewTransforms }.





% @doc Remote call expression found.
%
% "If E is a function call E_m:E_0(E_1, ..., E_k), then Rep(E) = {call,
% FILE_LOC, {remote, FILE_LOC, Rep(E_m), Rep(E_0)}, [Rep(E_1), ..., Rep(E_k)]}.
%
% Remote call expression found, with an immediate name for both the module and
% the function:
%
% (parameters already transformed)
%
-spec transform_call_expression( form(), arity(), ast_transforms() ) ->
										{ form(), ast_transforms() }.
transform_call_expression( OriginalExpr={ 'remote', FileLocRemote,
									_M={ atom, FileLocMod, ModuleName },
									_F={ atom, FileLocFun, FunctionName } },
						   Arity, Transforms ) ?rec_guard ->

	?log_enter( "Transforming remote call expression to ~ts:~ts/~B...",
				[ ModuleName, FunctionName, Arity ] ),

	Outcome = case Transforms#ast_transforms.remote_calls of

		undefined ->
			unchanged;

		RemoteReplaceTable ->

			case ?table:lookup_entry( { ModuleName, FunctionName, Arity },
									  RemoteReplaceTable ) of

				{ value, E={ _NewModuleName, _NewFunctionName } } ->
					E;

				{ value, TransformFun } when is_function( TransformFun ) ->
					TransformFun( FunctionName, Arity );

				key_not_found ->

					AnyArity = '_',

					% Maybe a wildcard arity was defined then?
					case ?table:lookup_entry(
							{ ModuleName, FunctionName, AnyArity },
							RemoteReplaceTable ) of

						{ value, E={ _NewModuleName, _NewFunctionName } } ->
							E;

						% Same function name, only module overridden:
						% (never happens)
						%{ value, NewModuleName }
						%       when is_atom( NewModuleName ) ->
						%   { NewModuleName, FunName };

						{ value, TransformFun }
						  when is_function( TransformFun ) ->
							TransformFun( FunctionName, Arity );

						key_not_found ->
							% Maybe a wildcard function name was defined then?

							% (note: the case of a wildcard function name and a
							% set, actual arity is not deemed relevant)

							case ?table:lookup_entry( { ModuleName,
									   _AnyFunctionName='_', AnyArity },
													  RemoteReplaceTable ) of

								{ value,
								  { NewModuleName, _NewFunctionName='_' } } ->
									{ NewModuleName, FunctionName } ;

								{ value,
								  E={ _NewModuleName, _NewFunctionName } } ->
									E;

									% Same function name, only module
									% overridden: (never happens)
									%
									%{ value, NewModuleName }
									%       when is_atom( NewModuleName ) ->
									%    { NewModuleName, FunName };

								{ value, TransformFun }
								  when is_function( TransformFun ) ->
									TransformFun( FunctionName, Arity );

								key_not_found ->
									unchanged

							end

					end

			end

	end,

	NewExpr = case Outcome of

		unchanged ->
			?log_exit( "... returning original remote call expression "
					   "(case R1) ~p", [ OriginalExpr ] ),
			OriginalExpr;

		{ SetModuleName, SetFunctionName } ->
			TransfExpr = { 'remote', FileLocRemote,
							{ atom, FileLocMod, SetModuleName },
							{ atom, FileLocFun, SetFunctionName } },
			?log_exit( "... returning remote call expression "
					   "(case R2) ~p", [ TransfExpr ] ),
			TransfExpr

	end,

	{ [ NewExpr ], Transforms };


% Here, at least one name (module and/or function) is not immediate in that
% remote call expression:
%
% (note: we do not manage yet the case where for example the function name
% results from an expression yet a wildcard has been defined for it)
%
transform_call_expression( ?e={ 'remote', FileLocRemote, ModuleExpr,
								FunctionExpr },
						   _Arity, Transforms ) ?rec_guard ->

	?log_enter( "Transforming non-immediate remote call expression ~p...",
				[ E ] ),

	{ [ NewModuleExpr ], ModTransforms } =
		transform_expression( ModuleExpr, Transforms ),

	{ [ NewFunctionExpr ], FunTransforms } =
		transform_expression( FunctionExpr, ModTransforms ),

	NewExpr = { 'remote', FileLocRemote, NewModuleExpr, NewFunctionExpr },

	Res = { [ NewExpr ], FunTransforms },

	?log_exit( "... returning non-immediate remote call expression "
			   "(case R3) and state ~p", [ Res ] ),

	Res;


% Local call expression found:
%
% "If E is a function call E_0(E_1, ..., E_k), then Rep(E) = {call, FILE_LOC,
% Rep(E_0), [Rep(E_1), ..., Rep(E_k)]}."
%
transform_call_expression( CallExpr={ 'atom', FileLocFun, FunName }, Arity,
						   Transforms ) ?rec_guard ->

	?log_enter( "Transforming local call expression ~p...", [ CallExpr ] ),

	Outcome = case Transforms#ast_transforms.local_calls of

		undefined ->
			unchanged;

		LocalReplaceTable ->

			case ?table:lookup_entry( { FunName, Arity }, LocalReplaceTable ) of

				{ value, E={ _NewModuleName, _NewFunName } } ->
					E;

				{ value, TransformFun } when is_function( TransformFun ) ->
					TransformFun( FunName, Arity );

				key_not_found ->

					% Maybe a wildcard arity was defined then?
					case ?table:lookup_entry( { FunName, _AnyArity='_' },
											  LocalReplaceTable ) of

						{ value, E={ _NewModuleName, _NewFunName } } ->
							E;

						% Same function name, only module overridden: (never
						% happens)
						%{ value, NewModuleName }
						%       when is_atom( NewModuleName ) ->
						%   { NewModuleName, FunName };

						{ value, TransformFun }
						  when is_function( TransformFun ) ->
							TransformFun( FunName, Arity );

						key_not_found ->
							% Nope, let it as it is:
							unchanged

					end

			end

	end,

	NewExpr = case Outcome of

		unchanged ->
			Expr = CallExpr,
			?log_exit( "... returning local call expression ~p", [ Expr ] ),
			Expr;

		{ SetModuleName, SetFunctionName } ->
			Expr = { 'remote', FileLocFun, SetModuleName, SetFunctionName },
			?log_exit( "... returning remote call expression ~p", [ Expr ] ),
			Expr

	end,
	{ [ NewExpr ], Transforms };


% Ex: happens with a line like: 'MyNode = MyContentFun( Content, "hello" )'.
transform_call_expression( CallExpr, _Arity, Transforms ) ?rec_guard ->
	transform_expression( CallExpr, Transforms ).
