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



% Module in charge of handling patterns defined with an AST.
%
% See the "7.3 Patterns" section of http://erlang.org/doc/apps/erts/absform.html
% for more information.
%
-module(ast_pattern).


% The description of a pattern in an AST, with line information.
%
% Ex: {match,12,X,Y}.
%
% Too many patterns to fully specify all (see "7.3 Patterns").
%
% Note: a pattern is different from an expression: even if they share at least
% some types of forms, they are to be interpreted differently (ex: their
% sub-elements are of the same kind as they are, and at least some rules
% differ).
%
-type ast_pattern() :: ast_base:ast_element().


% The description of a sequence of patterns in an AST, with line information.
%
% A pattern sequence is simply an (ordered) list of patterns.
%
% Ex: [{match,12,X,Y}].
%
-type ast_pattern_sequence() :: [ ast_pattern() ].


-export_type([ ast_pattern/0, ast_pattern_sequence/0 ]).



-export([ transform_pattern/2, transform_pattern_sequence/2 ]).


% Shorthands:

-type line() :: ast_base:line().

-type ast_transforms() :: ast_transform:ast_transforms().
-type ast_element() :: ast_base:ast_element().
-type ast_pattern_field() :: ast_record:ast_pattern_field().


% For the ast_transforms record:
-include("ast_transform.hrl").

% For rec_guard-related defines:
-include("ast_utils.hrl").


% Regarding patterns vs expressions:
%
% "Notice that every pattern has the same source form as some expression, and is
% represented in the same way as the corresponding expression."
%
% Thus patterns are special cases of (richer) expressions.




% Transforms specified pattern, operating relevant AST transformations onto it
% (e.g. call ones).


% List of patterns found:
%
% (note: this clause may be removed in the future, once all AST elements will
% have been specifically intercepted by a dedicated clause, and when the nature
% of their elements will be established and thus traversed specifically, rather
% than opening the possibility that each element may be a list)
%
% See section "7.3  Patterns" in http://erlang.org/doc/apps/erts/absform.html.
%
% "If Ps is a sequence of patterns P_1, ..., P_k, then Rep(Ps) = [Rep(P_1), ...,
% Rep(P_k)]."
%
% "Such sequences occur as the list of arguments to a function or fun."
%
-spec transform_pattern( ast_pattern(), ast_transforms() ) ->
								{ ast_pattern(), ast_transforms() }.
% A list of patterns should have already been iterated over upstream:
%transform_pattern( PatternList, Transforms ) when is_list( PatternList ) ->

	%ast_utils:display_debug( "Intercepting pattern list ~p...",
	%						 [ PatternList ] ),

	%%Res = { NewPatternList, NewTransforms }:
	%Res = lists:mapfoldl( fun transform_pattern/2, _Acc0=Transforms,
	%                       _List=PatternList ).

	%ast_utils:display_debug( "... returning pattern list and state ~p",
	%						  [ Res ] ),

	%Res;


% Match pattern found:
%
% "If P is a compound pattern P_1 = P_2, then Rep(P) =
% {match,LINE,Rep(P_1),Rep(P_2)}."
%
% Left and Right members are patterns here (not general expressions).
%
transform_pattern( _E={ 'match', Line, LeftPattern, RightPattern },
				   Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting match pattern ~p...", [ E ] ),

	{ NewLeftPattern, LeftTransforms } = transform_pattern( LeftPattern,
															Transforms ),

	{ NewRightPattern, RightTransforms } = transform_pattern( RightPattern,
															  LeftTransforms ),

	NewExpr = { 'match', Line, NewLeftPattern, NewRightPattern },
	Res = { NewExpr, RightTransforms },

	%ast_utils:display_debug( "... returning match pattern and state ~p",
	%                         [ Res ] ),

	Res;


% Cons pattern found:
%
% "If P is a cons pattern [P_h | P_t], then Rep(P) =
% {cons,LINE,Rep(P_h),Rep(P_t)}."
%
transform_pattern( _E={ 'cons', Line, HeadPattern, TailPattern },
				   Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting cons pattern ~p...", [ E ] ),

	% As, for trees, we tend to be depth-first, we prefer this order:

	{ NewTailPattern, TailTransforms } = ast_pattern:transform_pattern(
										   TailPattern, Transforms ),

	{ NewHeadPattern, HeadTransforms } = ast_pattern:transform_pattern(
										   HeadPattern, TailTransforms ),

	NewExpr = { cons, Line, NewHeadPattern, NewTailPattern },

	Res = { NewExpr, HeadTransforms },

	%ast_utils:display_debug( "... returning cons pattern and state ~p",
	%                         [ Res ] ),

	Res;


% Nil pattern found:
%
% "If P is a nil pattern [], then Rep(P) = {nil,LINE}."
%
transform_pattern( E={ 'nil', _Line }, Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting nil pattern ~p...", [ E ] ),

	Res = { E, Transforms },

	%ast_utils:display_debug( "... returning nil pattern and state ~p",
	%                         [ Res ] ),

	Res;



% Receive pattern found:

% Note: commented-out as is an expression, not a pattern apparently, according
% to http://erlang.org/doc/apps/erts/absform.html and erl_id_trans.


% "If E is a receive expression receive Cc_1 ; ... ; Cc_k end, where each Cc_i
% is a case clause, then Rep(E) = {'receive',LINE,[Rep(Cc_1), ..., Rep(Cc_k)]}."
%
%transform_pattern( E={ 'receive', Line, Clauses }, Transforms ) ->

%	ast_utils:display_debug( "Intercepting receive pattern ~p...", [ E ] ),

%   { NewClauses, NewTransforms } = lists:mapfoldl(
%       fun transform_case_clause/2, _Acc0=Transforms, _List=Clauses ),

%	NewExpr = { 'receive', Line, NewClauses },
%
%   Res = { NewExpr, NewClauses },
%
%	ast_utils:display_debug( "... returning receive pattern and state ~p",
%                            [ Res ] ),

%	Res;


% "If E is a receive expression receive Cc_1 ; ... ; Cc_k after E_0 -> B_t end,
% where each Cc_i is a case clause, E_0 is an expression, and B_t is a body,
% then Rep(E) = {'receive',LINE,[Rep(Cc_1), ..., Rep(Cc_k)],Rep(E_0),Rep(B_t)}."
%
%transform_pattern( E={ 'receive', Line, Clauses, Expression, Body },
%					Transforms ) ->

%	ast_utils:display_debug( "Intercepting receive pattern with after ~p...",
%							 [ E ] ),

%   { NewClauses, ClauseTransforms } = lists:mapfoldl(
%       fun transform_case_clause/2, _Acc0=Transforms, _List=Clauses ),

%	{ NewExpression, ExprTransforms } = ast_expression:transform_expression(
%                                            Expression, ClauseTransforms ),

%	{ NewBody, BodyTransforms } = ast_clause:transform_body( Body,
%                                                            ExprTransforms ),

%	NewExpr = { 'receive', Line, NewClauses, NewExpression, NewBody },
%
%   Res = { NewExpr, BodyTransforms },

%	ast_utils:display_debug( "... returning receive pattern with after "
%                            "and state ~p", [ Res ] ),

%	 Res;



% Map pattern found:
%
% "If P is a map pattern #{A_1, ..., A_k}, where each A_i is an association
% P_i_1 := P_i_2, then Rep(P) = {map,LINE,[Rep(A_1), ..., Rep(A_k)]}."
%
transform_pattern( _E={ 'map', Line, Associations }, Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting map pattern ~p...", [ E ] ),

	{ NewAssociations, NewTransforms } = lists:mapfoldl(
		   fun transform_pattern/2, _Acc0=Transforms, _List=Associations ),


	NewExp = { 'map', Line, NewAssociations },

	Res = { NewExp, NewTransforms },

	%ast_utils:display_debug( "... returning map pattern and state ~p",
	%                         [ Res ] ),

	Res;


% "If A is an association K := V, then Rep(A) =
% {map_field_exact,LINE,Rep(K),Rep(V)}."
%
% Detected thanks to erl_id_trans (and not so clear in
% http://erlang.org/doc/apps/erts/absform.html); apparently, no map_field_assoc
% to expect here, according to the same source.
%
transform_pattern( _E={ 'map_field_exact', Line, Key, Value },
				   Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting map exact association ~p...",
	%						  [ E ] ),

	{ [ NewKey ], KeyTransforms } =
		ast_expression:transform_expression( Key, Transforms ),

	{ NewValue, ValueTransforms } = transform_pattern( Value, KeyTransforms ),

	NewExpr = { 'map_field_exact', Line, NewKey, NewValue },

	Res = { NewExpr, ValueTransforms },

	%ast_utils:display_debug( "... returning map exact association and state "
	%                         "~p", [ Res ] ),

	Res;


% ('struct' tuple commented-out in erl_id_trans, and not found in
% http://erlang.org/doc/apps/erts/absform.html)



% Bitstring pattern found:
%
% "If P is a bitstring pattern <<P_1:Size_1/TSL_1, ..., P_k:Size_k/TSL_k>>,
% where each Size_i is an pattern that can be evaluated to an integer, and
% each TSL_i is a type specifier list, then Rep(P) =
% {bin,LINE,[{bin_element,LINE,Rep(P_1),Rep(Size_1),Rep(TSL_1)}, ...,
% {bin_element,LINE,Rep(P_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see
% below. An omitted Size_i is represented by default. An omitted TSL_i is
% represented by default."
%
transform_pattern( _Clause={ 'bin', Line, BinElements },
				   Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting bitstring pattern ~p...",
	%						 [ Clause ] ),

	% Actually no need to introduce a pattern-specific way of transforming a
	% bitstring:
	%
	%NewBinElements = ast_bitstring:transform_bin_elements( BinElements,
	%					   Transforms, fun transform_pattern/2 ),

	{ NewBinElements, NewTransforms } = ast_bitstring:transform_bin_elements(
										  BinElements, Transforms ),

	NewExpr = { 'bin', Line, NewBinElements },

	Res = { NewExpr, NewTransforms },

	%ast_utils:display_debug( "... returning bitstring pattern and state ~p",
	%                         [ Res ] ),

	Res;


% Tuple pattern found:
%
% "If P is a tuple pattern {P_1, ..., P_k}, then Rep(P) = {tuple,LINE,[Rep(P_1),
% ..., Rep(P_k)]}."
%
% Note: patterns, not expressions here, as shown by erl_id_trans.
%
transform_pattern( _Clause={ 'tuple', Line, Patterns },
				   Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting tuple pattern ~p...",
	%						 [ Clause ] ),

	{ NewPatterns, NewTransforms } = lists:mapfoldl( fun transform_pattern/2,
									 _Acc0=Transforms, _List=Patterns ),

	NewExpr = { 'tuple', Line, NewPatterns },

	Res = { NewExpr, NewTransforms },

	%ast_utils:display_debug( "... returning tuple pattern and state ~p",
	%                         [ Res ] ),

	Res;


% Variable pattern found:
%
% "If P is a universal pattern _, then Rep(P) = {var,LINE,'_'}."
%  - and also -
% "If P is a variable pattern V, then Rep(P) = {var,LINE,A}, where A is an atom
% with a printname consisting of the same characters as V."
%
transform_pattern( _Clause={ 'var', Line, VariableName },
				   Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting variable pattern ~p...",
	%						 [ Clause ] ),

	{ NewVariableName, NewTransforms } =
		transform_variable( VariableName, Line, Transforms ),

	NewExpr = { 'var', Line, NewVariableName },

	Res = { NewExpr, NewTransforms },

	%ast_utils:display_debug( "... returning variable pattern and state ~p",
	%                         [ Res ] ),

	Res;


% Atomic literal value found:
%
% (difficult to discriminate more at this level)
%
transform_pattern( Clause={ LiteralType, _Line, _Value }, Transforms )
  when is_atom( LiteralType ) ?andalso_rec_guard ->

	% Maybe Value could just be sent (or no transformation be considered):
	ast_value:transform_value( Clause, Transforms );



% Record found:
%
% "If P is a record pattern #Name{Field_1=P_1, ..., Field_k=P_k}, where each
% Field_i is an atom or _, then Rep(P) =
% {record,LINE,Name,[{record_field,LINE,Rep(Field_1),Rep(P_1)}, ...,
% {record_field,LINE,Rep(Field_k),Rep(P_k)}]}."
%
transform_pattern( _Clause={ 'record', Line, RecordName, PatternFields },
				   Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Transforming record '~ts' at line #~B, "
	%	"fields being described as following patterns: ~p",
	%	[ RecordName, Line, PatternFields ] ),


	{ NewPatternFields, NewTransforms } =
		transform_pattern_fields( PatternFields, Transforms ),

	NewExpr = { 'record', Line, RecordName, NewPatternFields },

	{ NewExpr, NewTransforms };


% Access to a record field found (see previous clause):
transform_pattern( _Clause={ 'record_field', Line, RecordName, FieldName,
							 FieldValue }, Transforms ) ?rec_guard ->

	{ NewRecordName, RecTransforms } = ast_expression:transform_expression(
										 RecordName, Transforms ),

	% (FieldName not specifically inspected by erl_trans_id for some reason)

	{ NewFieldValue, FieldTransforms } =
		ast_expression:transform_expression( FieldValue, RecTransforms ),

	NewExpr = { 'record_field', Line, NewRecordName, FieldName, NewFieldValue },

	{ NewExpr, FieldTransforms };


% Update of a record field found (see 'record' clause):
%
transform_pattern( _Clause={ 'record_field', Line, FieldName, FieldValue },
				   Transforms ) ?rec_guard ->

	{ NewFieldName, FieldTransforms } =
		ast_expression:transform_expression( FieldName, Transforms ),

	{ NewFieldValue, ValueTransforms } =
		ast_expression:transform_expression( FieldValue, FieldTransforms ),

	NewExpr = { 'record_field', Line, NewFieldName, NewFieldValue },

	{ NewExpr, ValueTransforms };


% Record index found:
%
% "If P is a record field index pattern #Name.Field, where Field is an atom,
% then Rep(P) = {record_index,LINE,Name,Rep(Field)}."
%
transform_pattern( _Clause={ 'record_index', Line, RecordName, PatternField },
				   Transforms ) ?rec_guard ->

	{ NewPatternField, NewTransforms } =
		transform_pattern( PatternField, Transforms ),

	NewExpr = { 'record_index', Line, RecordName, NewPatternField },

	{ NewExpr, NewTransforms };


% "If P is an operator pattern P_1 Op P_2, where Op is a binary operator (this
% is either an occurrence of ++ applied to a literal string or character list,
% or an occurrence of an expression that can be evaluated to a number at compile
% time), then Rep(P) = {op,LINE,Op,Rep(P_1),Rep(P_2)}."
%
% (as shown in erl_id_trans, no transformation applies, as evaluated otherwise
% by the compiler)
%
transform_pattern( Clause={ 'op', _Line, _BinaryOperator, _LeftOperand,
							_RightOperand }, Transforms ) ?rec_guard ->
	{ Clause, Transforms };

transform_pattern( Clause={ 'op', _Line, _UnaryOperator, _Operand },
				   Transforms ) ?rec_guard ->
	{ Clause, Transforms };




% "If P is an operator pattern Op P_0, where Op is a unary operator (this is an
% occurrence of an expression that can be evaluated to a number at compile
% time), then Rep(P) = {op,LINE,Op,Rep(P_0)}."
%


% "If P is a parenthesized pattern ( P_0 ), then Rep(P) = Rep(P_0), that is,
% parenthesized patterns cannot be distinguished from their bodies." (nothing to
% do then)

transform_pattern( E, Transforms )
  when is_record( Transforms, ast_transforms ) ->

	% Record classical misuse?
	case is_tuple( E ) andalso element( 1, E ) =:= 'record'
		% implicit due to previous match: andalso size( E ) =/= 4 of
		andalso size( E ) > 4 of % for element( 3, E )
			% Here the record notation was probably misused (typically
			% MyVar#my_record{... was used where MyVar=#my_record{... was
			% wanted):
			%
			true ->
				ast_utils:display_error( "Invalid record pattern "
					"at line #~p; maybe using MyVar#my_record where "
					"MyVar=#my_record was meant?~n", [ element( 2, E ) ] ),
				ast_utils:raise_error( [ invalid_record_use, E ] );

			false ->
				%ast_utils:display_warning( "Letting unhandled pattern ~p as "
				%  "is.", [ E ] ),
				% E.
				ast_utils:display_error( "Illegal pattern found at line #~p "
					"(not supported by the current version of the "
					"Erlang syntax).~n",
					[ element( 2, E ) ] ),
				ast_utils:raise_error( [ unexpected_pattern_to_transform, E ] )

	end.



% Transforms specified pattern sequence, operating relevant AST transformations.
%
% Note: the case where the sequence is empty is managed here as well.
%
-spec transform_pattern_sequence( ast_pattern_sequence(), ast_transforms() ) ->
							{ ast_pattern_sequence(), ast_transforms() }.
transform_pattern_sequence( Patterns, Transforms ) ?rec_guard ->
	lists:mapfoldl( fun transform_pattern/2, _Acc0=Transforms,
					_List=Patterns ).



% Transforms specified variable (possibly the universal one, '_'), operating
% relevant AST transformations.
%
-spec transform_variable( meta_utils:variable_name(), line(),
				ast_transforms() ) -> { ast_element(), ast_transforms() }.
transform_variable( VariableName, _Line, Transforms ) ?rec_guard ->
	% Currently no transformation done:
	{ VariableName, Transforms }.


% Transforms specified pattern fields.
%
% (note: better here than in ast_record)
%
-spec transform_pattern_fields( [ ast_pattern_field() ],
				ast_transforms() ) -> { [ ast_element() ], ast_transforms() }.
transform_pattern_fields( PatternFields, Transforms ) ?rec_guard ->
	lists:mapfoldl( fun transform_pattern_field/2, _Acc0=Transforms,
					_List=PatternFields ).



% Transforms specified pattern field.
%
% Note: according to erl_id_trans, field names are full expressions here, but
% only atoms are allowed by the linter.
%
-spec transform_pattern_field( ast_pattern_field(), ast_transforms() ) ->
									{ ast_element(), ast_transforms() }.
transform_pattern_field( { 'record_field', Line1,
		   N={ atom, _Line2, _FieldName }, Pattern }, Transforms ) ?rec_guard ->

	{ NewPattern, NewTransforms } = transform_pattern( Pattern, Transforms ),

	NewExpr = { 'record_field', Line1, N, NewPattern },

	{ NewExpr, NewTransforms };


transform_pattern_field( { 'record_field', Line1,
	  N={ 'var', _Line2, _FieldName='_' }, Pattern }, Transforms ) ?rec_guard ->

	{ NewPattern, NewTransforms } = transform_pattern( Pattern, Transforms ),

	NewExpr = { 'record_field', Line1, N, NewPattern },

	{ NewExpr, NewTransforms }.
