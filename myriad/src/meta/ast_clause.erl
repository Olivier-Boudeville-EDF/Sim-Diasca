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



% Module in charge of handling clauses defined within an AST.
%
% Refer to the "7.5 Clauses" section of
% http://erlang.org/doc/apps/erts/absform.html for more information.
%
-module(ast_clause).



% There are 5 different kinds of clauses in an AST:
% - function clauses
% - if clauses
% - case clauses
% - try clauses
% - catch clauses
%
% One may note they actually all obey the same structure (same quintuplet).
%
-type ast_clause() :: ast_function_clause() | ast_if_clause()
					| ast_case_clause() | ast_try_clause()
					| ast_catch_clause().


% Describes a generic (most general) clause in an AST:
%
-type ast_generic_clause() :: { 'clause', line(),
								ast_pattern:ast_pattern_sequence(),
								ast_guard:ast_guard_sequence(), ast_body() }.


% Describes a function clause in an AST:
%
% "If C is a function clause ( Ps ) -> B, where Ps is a pattern sequence and B
% is a body, then Rep(C) = {clause,LINE,Rep(Ps),[],Rep(B)}.
%
% If C is a function clause ( Ps ) when Gs -> B, where Ps is a pattern sequence,
% Gs is a guard sequence and B is a body, then Rep(C) =
% {clause,LINE,Rep(Ps),Rep(Gs),Rep(B)}."
%
-type ast_function_clause() :: ast_generic_clause().



% Describes an if clause in an AST:
%
% "If C is an if clause Gs -> B, where Gs is a guard sequence and B is a body,
% then Rep(C) = {clause,LINE,[],Rep(Gs),Rep(B)}."
%
% (special case of ast_generic_clause/0, no pattern sequence)
%
-type ast_if_clause() :: { 'clause', line(), [], ast_guard:ast_guard_sequence(),
						   ast_body() }.



% Describes a case clause in an AST:
%
-type ast_case_clause() :: ast_generic_clause().


% Describes a try clause in an AST:
%
-type ast_try_clause() :: ast_generic_clause().


% Describes a catch clause in an AST:
%
-type ast_catch_clause() :: ast_generic_clause().


% The description of a body (ex: of a function clause) in an AST.
%
% "A body B is a non-empty sequence of expressions E_1, ..., E_k, and Rep(B) =
% [Rep(E_1), ..., Rep(E_k)]."
%
-type ast_body() :: nonempty_list( ast_expression() ).


-export_type([ ast_clause/0, ast_function_clause/0, ast_if_clause/0,
			   ast_case_clause/0, ast_try_clause/0, ast_catch_clause/0,
			   ast_body/0 ]).



% Forging:
-export([ forge_local_call/3, forge_local_call/4,
		  forge_remote_call/4, forge_remote_call/5 ]).


% Checking:
-export([ check_function_clauses/2, check_function_clauses/3 ]).




-export([ transform_clause_default/2,
		  transform_function_clauses/2, transform_function_clause/2,

		  transform_try_clauses/2, transform_try_clause/2,
		  transform_catch_clauses/2, transform_catch_clause/2,

		  transform_if_clauses/2, transform_if_clause/2,
		  transform_case_clauses/2, transform_case_clause/2,

		  transform_body/2 ]).



% Shorthands:

-type module_name() :: meta_utils:module_name().
-type function_name() :: meta_utils:function_name().
-type function_arity() :: meta_utils:function_arity().
-type ast_expression() :: ast_expression:ast_expression().
-type line() :: ast_base:line().
-type form_context() :: ast_base:form_context().
-type ast_transforms() :: ast_transform:ast_transforms().


% For the table macro:
-include("meta_utils.hrl").

% For the ast_transform record:
-include("ast_transform.hrl").

% For the rec_guard-related defines:
-include("ast_utils.hrl").



% Transformations section.


% Apparently, for all (4) kinds of clauses, according to erl_id_trans the same
% structure applies: {clause,Line,H,G,B}, where:
%
% - H is Head, a list of patterns (a pattern sequence)
% - G is Guard, a list of guard tests (a guard sequence)
% - B is Body, a list of expressions
%
% However, depending of the actual kind, more specific rules apply (ex: a list
% having a single element), which are enforced here.



% Generic clause section.
%
% In quite a few occasions, clauses can be managed generically, regardless of
% whether they belong to a 'if', a 'catch', etc. (see icr_clauses/1 in
% erl_id_trans).
%
% Here is the corresponding generic clause transformation.
%
% (helper)
%
-spec transform_clauses_generic( [ ast_clause() ], ast_transforms() ) ->
									   { [ ast_clause() ], ast_transforms() }.
transform_clauses_generic( Clauses, Transforms ) ?rec_guard ->

	% As a result, a given clause when transformed will benefit from the
	% processing of any previous one (of the same function of course):
	%
	lists:mapfoldl( fun transform_clause_generic/2, _Acc0=Transforms,
					_List=Clauses ).



% Transforms a single clause, generic version.
-spec transform_clause_generic( ast_clause(), ast_transforms() ) ->
									  { ast_clause(), ast_transforms() }.
transform_clause_generic( Clause, Transforms ) ?rec_guard ->

	?display_trace( "Transforming clause:~n~p~n", [ Clause ] ),

	% Maybe a clause replacement function has been defined?
	case Transforms#ast_transforms.transform_table of

		undefined ->

			NewClausePair = transform_clause_default( Clause, Transforms ),

			%ast_utils:display_debug(
			%  "returning directly transformed clause pair (case 1):~n~p",
			%  [ NewClausePair ] ),

			NewClausePair;


		TransformTable ->
			case ?table:lookup_entry( 'clause', TransformTable ) of

				key_not_found ->
					NewClausePair = transform_clause_default( Clause,
															  Transforms ),

					%ast_utils:display_debug( "returning directly transformed "
					%                         "clause pair (case 2):~n~p",
					%                         [ NewClausePair ] ),

					NewClausePair;

				{ value, ClauseTransformFun } ->

					% Returns directly { NewClause, NewTransforms }:
					NewClausePair = ClauseTransformFun( Clause, Transforms ),

					%ast_utils:display_debug(
					%  "returning fun-transformed clause pair (case 3):~n~p",
					%  [ NewClausePair ] ),

					NewClausePair

			end

	end.



% Default transformation applied to function clauses.
transform_clause_default(
  _Clause={ 'clause', Line, HeadPatternSequence, GuardSequence, BodyExprs },
  Transforms ) ->

	?display_trace( "Transforming head patterns." ),

	{ NewHeadPatternSequence, HeadTransforms } =
		ast_pattern:transform_pattern_sequence( HeadPatternSequence,
												Transforms ),


	?display_trace( "Transforming guards." ),

	% Possibly empty guard list:
	{ NewGuardSequence, GuardTransforms } =
		ast_guard:transform_guard_sequence( GuardSequence, HeadTransforms ),


	%?display_trace( "Transforming body." ),
	?display_trace( "Transforming body:~n~p", [ BodyExprs ] ),

	{ NewBodyExprs, BodyTransforms } =
		transform_body( BodyExprs, GuardTransforms ),

	?display_trace( "Transformed body:~n~p", [ NewBodyExprs ] ),

	NewExpr = { 'clause', Line, NewHeadPatternSequence, NewGuardSequence,
				NewBodyExprs },

	%ast_utils:display_debug( "... returning generic clause:~n~p~n~n"
	%						 "and state:~n~p", [ NewExpr, BodyTransforms ] ),

	{ NewExpr, BodyTransforms }.




% Function clause section.


% Transforms specified list of function clauses.
-spec transform_function_clauses( [ ast_function_clause() ],
		 ast_transforms() ) -> { [ ast_function_clause() ], ast_transforms() }.
transform_function_clauses( FunctionClauses, Transforms ) ?rec_guard ->
	transform_clauses_generic( FunctionClauses, Transforms ).



% Transforms specified function clause.
%
% Handled the same, with or without guard(s), as a guard sequence may be empty:
%
%  - without: "If C is a function clause ( Ps ) -> B, where Ps is a pattern
%  sequence and B is a body, then Rep(C) = {clause,LINE,Rep(Ps),[],Rep(B)}."
%
%  - with: "If C is a function clause ( Ps ) when Gs -> B, where Ps is a pattern
%  sequence, Gs is a guard sequence and B is a body, then Rep(C) =
%  {clause,LINE,Rep(Ps),Rep(Gs),Rep(B)}."
%
-spec transform_function_clause( ast_function_clause(), ast_transforms() ) ->
								   { ast_function_clause(), ast_transforms() }.
transform_function_clause( Clause, Transforms ) ?rec_guard ->
	transform_clause_generic( Clause, Transforms ).



% Try clause section.


% Transforms specified list of try clauses.
%
-spec transform_try_clauses( [ ast_try_clause() ], ast_transforms() ) ->
								   { [ ast_try_clause() ], ast_transforms() }.
transform_try_clauses( TryClauses, Transforms ) ?rec_guard ->
	transform_clauses_generic( TryClauses, Transforms ).


% Transforms specified try clause.
%
-spec transform_try_clause( ast_try_clause(), ast_transforms() ) ->
								   { [ ast_try_clause() ], ast_transforms() }.
transform_try_clause( TryClause, Transforms ) ?rec_guard ->
	transform_clause_generic( TryClause, Transforms ).





% Catch clause section.
%
% (with both possibilities, having an empty guard sequence is just a special
% case of a more general rule)


% Transforms specified list of 'catch' clauses.
%
-spec transform_catch_clauses( [ ast_catch_clause() ], ast_transforms() ) ->
								 { [ ast_catch_clause() ], ast_transforms() }.
transform_catch_clauses( CatchClauses, Transforms ) ?rec_guard ->
	lists:mapfoldl( fun transform_catch_clause/2, _Acc0=Transforms,
					_List=CatchClauses ).


% Catch clause with no variable, with or without a guard sequence (1/4 and 3/4):
%
% - "If C is a catch clause P -> B, where P is a pattern and B is a body, then
% Rep(C) = {clause,LINE,[Rep({throw,P,_})],[],Rep(B)}."
%
% - "If C is a catch clause P when Gs -> B, where P is a pattern, Gs is a guard
% sequence, and B is a body, then Rep(C) =
% {clause,LINE,[Rep({throw,P,_})],Rep(Gs),Rep(B)}."
%
-spec transform_catch_clause( ast_catch_clause(), ast_transforms() ) ->
									{ ast_catch_clause(), ast_transforms() }.
transform_catch_clause(
  _Clause={ 'clause', Line, [ { throw, Pattern, Any } ], GuardSequence,
			BodyExprs },
  Transforms ) ?rec_guard ->

	ast_utils:display_warning( "transform_catch_clause: Any= ~p", [ Any ] ),

	%ast_utils:display_debug( "Intercepting catch clause ~p...", [ Clause ] ),

	{ NewPattern, PatTransforms } =
		ast_pattern:transform_pattern( Pattern, Transforms ),

	{ NewGuardSequence, GuardTransforms } = ast_guard:transform_guard_sequence(
											  GuardSequence, PatTransforms ),

	{ NewBodyExprs, BodyTransforms } =
		transform_body( BodyExprs, GuardTransforms ),

	NewExpr = { 'clause', Line, [ { 'throw', NewPattern, Any } ],
				NewGuardSequence, NewBodyExprs },

	Res = { NewExpr, BodyTransforms },

	%ast_utils:display_debug( "... returning catch clause and state ~p",
	%                         [ Res ] ),

	Res;


% Catch clause with X variable, with or without a guard sequence (2/4 and 4/4):
%
% - "If C is a catch clause X : P -> B, where X is an atomic literal or a
% variable pattern, P is a pattern, and B is a body, then Rep(C) =
% {clause,LINE,[Rep({X,P,_})],[],Rep(B)}."
%
% - "If C is a catch clause X : P when Gs -> B, where X is an atomic literal or
% a variable pattern, P is a pattern, Gs is a guard sequence, and B is a body,
% then Rep(C) = {clause,LINE,[Rep({X,P,_})],Rep(Gs),Rep(B)}."
%
transform_catch_clause(
  _Clause={ 'clause', Line, [ HeadPattern={ _X, _P, _Any } ], GuardSequence,
			BodyExprs },
  Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "transform_catch_clause: X=~p, P=~p, Any= ~p",
	%						  [ X, P, Any ] ),

	%ast_utils:display_debug( "Intercepting catch clause with variable ~p...",
	%						 [ Clause ] ),

	% Includes atomic literals:
	{ NewHeadPattern, HeadTransforms } =
		ast_pattern:transform_pattern( HeadPattern, Transforms ),

	{ NewGuardSequence, GuardTransforms } =
		ast_guard:transform_guard_sequence( GuardSequence, HeadTransforms ),

	{ NewBodyExprs, BodyTransforms } =
		transform_body( BodyExprs, GuardTransforms ),

	NewExpr = { 'clause', Line, [ NewHeadPattern ], NewGuardSequence,
				NewBodyExprs },

	Res = { NewExpr, BodyTransforms },

	%ast_utils:display_debug( "... returning catch clause with variable "
	%                         "and state ~p", [ Res ] ),

	Res.





% If clause section.


% Transforms specified list of 'if' clauses.
%
-spec transform_if_clauses( [ ast_if_clause() ], ast_transforms() ) ->
								  { [ ast_if_clause() ], ast_transforms() }.
transform_if_clauses( IfClauses, Transforms ) ?rec_guard ->
	lists:mapfoldl( fun transform_if_clause/2, _Acc0=Transforms,
					_List=IfClauses ).



% Transforms specified 'if' clause.
%
% "If C is an if clause Gs -> B, where Gs is a guard sequence and B is a body,
% then Rep(C) = {clause,LINE,[],Rep(Gs),Rep(B)}."
%
% (no pattern sequence allowed)
%
-spec transform_if_clause( ast_if_clause(), ast_transforms() ) ->
								{ ast_if_clause(), ast_transforms() }.
transform_if_clause(
  _Clause={ 'clause', Line, HeadPatternSequence=[], GuardSequence, BodyExprs },
  Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting if clause ~p...", [ Clause ] ),

	{ NewGuardSequence, GuardTransforms } =
		ast_guard:transform_guard_sequence( GuardSequence, Transforms ),

	{ NewBodyExprs, BodyTransforms } =
		transform_body( BodyExprs, GuardTransforms ),

	NewExpr = { 'clause', Line, HeadPatternSequence, NewGuardSequence,
				NewBodyExprs },

	Res = { NewExpr, BodyTransforms },

	%ast_utils:display_debug( "... returning if clause and state ~p", [ Res ] ),

	Res.




% Case clause section.


% Transforms specified list of 'case' clauses.
%
-spec transform_case_clauses( [ ast_case_clause() ], ast_transforms() ) ->
									{ [ ast_case_clause() ], ast_transforms() }.
transform_case_clauses( CaseClauses, Transforms ) ?rec_guard ->
	lists:mapfoldl( fun transform_case_clause/2, _Acc0=Transforms,
					_List=CaseClauses ).


% Transforms specified 'case' clause.
%
% "If C is a case clause P -> B, where P is a pattern and B is a body, then
% Rep(C) = {clause,LINE,[Rep(P)],[],Rep(B)}.
%
% If C is a case clause P when Gs -> B, where P is a pattern, Gs is a guard
% sequence, and B is a body, then Rep(C) =
% {clause,LINE,[Rep(P)],Rep(Gs),Rep(B)}."
%
% (a single pattern allowed)
%
-spec transform_case_clause( ast_case_clause(), ast_transforms() ) ->
									{ [ ast_case_clause() ], ast_transforms() }.
transform_case_clause(
  _Clause={ 'clause', Line, [ Pattern ], GuardSequence, BodyExprs },
  Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting case clause ~p...", [ Clause ] ),

	{ NewPattern, PatTransforms } =
		ast_pattern:transform_pattern( Pattern, Transforms ),

	{ NewGuardSequence, GuardTransforms } =
		ast_guard:transform_guard_sequence( GuardSequence, PatTransforms ),

	{ NewBodyExprs, BodyTransforms } =
		transform_body( BodyExprs, GuardTransforms ),

	NewExpr = { 'clause', Line, [ NewPattern ], NewGuardSequence,
				NewBodyExprs },

	Res = { NewExpr, BodyTransforms },

	%ast_utils:display_debug( "... returning case clause and state ~p",
	%                         [ Res ] ),

	Res.



% Transforms the specified AST body.
%
% "A body B is a non-empty sequence of expressions E_1, ..., E_k, and Rep(B) =
% [Rep(E_1), ..., Rep(E_k)]."
%
-spec transform_body( ast_body(), ast_transforms() ) ->
							{ ast_body(), ast_transforms() }.

% Actually bodies can be empty lists (ex: if a try/catch does not have an
% 'after' clause, its associated body will be empty).
%
%transform_body( _BodyExprs=[], _Transforms ) ->
%	ast_utils:raise_error( invalid_empty_body );

transform_body( BodyExprs, Transforms )
  when is_list( BodyExprs ) ?andalso_rec_guard ->

	%?display_trace( "transforming body: ~p...", [ BodyExprs ] ),

	case Transforms#ast_transforms.transform_table of

		undefined ->

			NewBodyPair =
				ast_expression:transform_expressions( BodyExprs, Transforms ),

			%ast_utils:display_debug(
			%  "returning directly transformed body pair (case 1):~n~p",
			%  [ NewBodyPair ] ),

			NewBodyPair;


		TransformTable ->
			case ?table:lookup_entry( 'body', TransformTable ) of

				key_not_found ->

					NewBodyPair = ast_expression:transform_expressions(
									 BodyExprs, Transforms ),

					%ast_utils:display_debug(
					%  "returning directly transformed body pair (case 2):~n~p",
					%  [ NewBodyPair ] ),

					NewBodyPair;


				{ value, BodyTransformFun } ->

					% Returns directly { NewBody, NewTransforms }:
					NewBodyPair = BodyTransformFun( BodyExprs, Transforms ),

					%ast_utils:display_debug(
					%  "returning fun-transformed body pair (case 3):~n~p",
					%  [ NewBodyPair ] ),

					NewBodyPair

			end

	end;

transform_body( Other, _Transforms ) ->
	ast_utils:raise_error( [ invalid_body, Other ] ).




% Forging section.



% Returns an AST-compliant representation of specified local call.
%
% Ex: to designate 'some_fun( a, b )' at line 102, use;
% forge_local_call( some_fun, ParamDefs, 102 ) - which returns:
% {call,102,{atom,102,some_fun},[{atom,102,a},{atom,102,b}]}.
%
-spec forge_local_call( function_name(), [ ast_expression() ],
						line() ) -> ast_expression().
forge_local_call( FunctionName, Params, Line ) ->
	forge_local_call( FunctionName, Params, Line, Line ).


% Returns an AST-compliant representation of specified local call.
%
% Ex: to designate 'some_fun( a, b )' at line 102, use;
% forge_local_call( some_fun, ParamDefs, 102 ) - which returns:
% {call,102,{atom,102,some_fun},[{atom,102,a},{atom,102,b}]}.
%
-spec forge_local_call( function_name(), [ ast_expression() ], line(),
						line() ) -> ast_expression().
forge_local_call( FunctionName, Params, Line1, Line2 ) ->
	{ 'call', Line1, ast_value:forge_atom_value( FunctionName, Line2 ),
	  Params }.




% Returns an AST-compliant representation of specified remote call.
%
% Ex: to designate 'some_module:some_fun( a, b )' at line 102, use;
% forge_remote_call( some_module, some_fun, ParamDefs, 102 ) - which returns:
% {{remote,102, {atom,102,some_module}, {atom,102,some_fun},
%              [{atom,102,a},{atom,102,b}]}.
%
-spec forge_remote_call( module_name(), function_name(), [ ast_expression() ],
						 line() ) -> ast_expression().
forge_remote_call( ModuleName, FunctionName, Params, Line ) ->
	forge_remote_call( ModuleName, FunctionName, Params, Line, Line ).



% Returns an AST-compliant representation of specified (immediate, in terms of
% name of module and function) remote call.
%
% Ex: to designate 'some_module:some_fun( a, b )' at lines 101 and 102, use;
% forge_remote_call( some_module, some_fun, ParamDefs, 102 ) - which returns:
% {{remote,102, {atom,102,some_module}, {atom,102,some_fun},
%              [{atom,102,a},{atom,102,b}]}.
%
-spec forge_remote_call( module_name(), function_name(), [ ast_expression() ],
						 line(), line() ) -> ast_expression().
forge_remote_call( ModuleName, FunctionName, Params, Line1, Line2 ) ->
	{ 'call', Line1, { remote, Line2,
					   ast_value:forge_atom_value( ModuleName, Line2 ),
					   ast_value:forge_atom_value( FunctionName, Line2 ) },
	  Params }.



% Checking section.


% Checks that specified function clauses are legit.
%
-spec check_function_clauses( term(), function_arity() ) ->
									[ ast_function_clause() ].
check_function_clauses( Clauses, FunctionArity ) ->
	check_function_clauses( Clauses, FunctionArity, _Context=undefined ).


% Checks that specified function clauses are legit.
%
-spec check_function_clauses( term(), function_arity(), form_context() ) ->
									[ ast_function_clause() ].
check_function_clauses( Clauses, FunctionArity, Context )
  when is_list( Clauses ) ->
	ast_utils:check_arity( FunctionArity, Context ),
	Clauses;

check_function_clauses( Other, _FunctionArity, Context ) ->
	ast_utils:raise_error( [ invalid_function_clauses, Other ], Context ).
