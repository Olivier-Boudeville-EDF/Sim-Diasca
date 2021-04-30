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



% Module in charge of handling guards defined with an AST.
%
% See the "7.6 Guards" section of http://erlang.org/doc/apps/erts/absform.html
% for more information.
%
-module(ast_guard).



% The description of a sequence of guards in an AST.
%
% "A guard sequence Gs is a sequence of guards G_1; ...; G_k, and Rep(Gs) =
% [Rep(G_1), ..., Rep(G_k)]. If the guard sequence is empty, then Rep(Gs) = []."
%
% Ex: {call,102, {atom,102,is_integer}, [{var,102,'X'}]}
%
-type ast_guard_sequence() :: [ ast_guard() ].



% The description of a guard in an AST.
%
% "A guard G is a non-empty sequence of guard tests Gt_1, ..., Gt_k, and Rep(G)
% = [Rep(Gt_1), ..., Rep(Gt_k)]."
%
%
-type ast_guard() :: nonempty_list( ast_guard_test() ).



% The description of a guard test in an AST.
%
-type ast_guard_test() ::
		ast_bitstring:constructor( ast_guard_test() )
	  | { 'cons', line(), ast_guard_test(), ast_guard_test() }
	  | { 'call', line(),
		  { 'remote', line(), ast_base:ast_element(), ast_base:ast_atom() }
			| ast_base:ast_atom(),
		  [ ast_guard_test() ] }
	  %| ast_map:ast_map_form( ast_guard_test() ).
	  | ast_map:ast_map_form().



% "If Gt is a bitstring constructor <<Gt_1:Size_1/TSL_1, ...,
% Gt_k:Size_k/TSL_k>>, where each Size_i is a guard test and each TSL_i is a
% type specificer list, then Rep(Gt) =
% {bin,LINE,[{bin_element,LINE,Rep(Gt_1),Rep(Size_1),Rep(TSL_1)}, ...,
% {bin_element,LINE,Rep(Gt_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see
% above.
%
% An omitted Size_i is represented by default. An omitted TSL_i is
% represented by default."
%
% So apparently a guard test is a recursive type:
%
-type ast_bitstring_constructor() ::
		ast_bitstring:constructor( ast_guard_test() ).


% Defined in the context of a guard:
-type ast_bitstring_bin_element() ::
		ast_bitstring:bin_element( ast_guard_test() ).



-export_type([ ast_guard_sequence/0, ast_guard/0, ast_guard_test/0,
			   ast_bitstring_constructor/0, ast_bitstring_bin_element/0 ]).


-export([ transform_guard_test/2, transform_guard/2,
		  transform_guard_sequence/2 ]).


% Shorthands:

-type line() :: ast_base:line().
-type ast_transforms() :: ast_transform:ast_transforms().



% For the ast_transforms record:
-include("ast_transform.hrl").

% For rec_guard-related defines:
-include("ast_utils.hrl").



% Transforms specified guard sequence, operating relevant AST transformations.
%
% "A guard sequence Gs is a sequence of guards G_1; ...; G_k, and Rep(Gs) =
% [Rep(G_1), ..., Rep(G_k)]. If the guard sequence is empty, then Rep(Gs) = []."
%
% Note: the cases where the sequence is empty is managed here as well.
%
-spec transform_guard_sequence( ast_guard_sequence(), ast_transforms() ) ->
									{ ast_guard_sequence(), ast_transforms() }.
transform_guard_sequence( Guards, Transforms ) ?rec_guard ->
	lists:mapfoldl( fun transform_guard/2, _Acc0=Transforms, _List=Guards ).




% Transforms specified guard, operating relevant AST transformations.
%
% "A guard G is a non-empty sequence of guard tests Gt_1, ..., Gt_k, and Rep(G)
% = [Rep(Gt_1), ..., Rep(Gt_k)]."
%
-spec transform_guard( ast_guard(), ast_transforms() ) ->
								{ ast_guard(), ast_transforms() }.
transform_guard( _GuardTests=[], _Transforms ) ->
	throw( invalid_empty_guard );

transform_guard( GuardTests, Transforms )
  when is_list( GuardTests ) ?andalso_rec_guard ->
	lists:mapfoldl( fun transform_guard_test/2, _Acc0=Transforms,
					_List=GuardTests );

transform_guard( Other, Transforms )
  when is_record( Transforms, ast_transforms ) ->
	ast_utils:raise_usage_error( "invalid guard '~p'.", [ Other ],
		Transforms#ast_transforms.transformed_module_name ).



% Transforms specified list of guard tests.
%
% Note: unlike transform_guard/2, the list may be empty, and a direct
% transformation is now expected to be performed.
%
% (helper)
%
-spec direct_transform_guard_tests( [ ast_guard_test() ], ast_transforms() ) ->
								  { [ ast_guard_test() ], ast_transforms() }.
direct_transform_guard_tests( GuardTests, Transforms ) ?rec_guard ->
	lists:mapfoldl( fun direct_transform_guard_test/2, _Acc0=Transforms,
					_List=GuardTests ).



% (corresponds to grecord_inits/1 in erl_id_trans)
%
% (helper)
%
transform_record_field_inits( RecordFieldInits, Transforms ) ?rec_guard ->
	lists:mapfoldl( fun transform_record_field_init/2, _Acc0=Transforms,
					_List=RecordFieldInits ).



% Field names are full expressions here, but only atoms are allowed by the
% linter.
%
% Note: includes the case where FieldName is '_'.
%
transform_record_field_init( { 'record_field', LineField,
		FieldNameASTAtom={ atom, _LineAtom, _FieldName }, FieldValue },
							 Transforms ) ?rec_guard ->

	{ NewFieldValue, NewTransforms } =
		direct_transform_guard_test( FieldValue, Transforms ),

	NewExpr = { 'record_field', LineField, FieldNameASTAtom, NewFieldValue },

	{ NewExpr, NewTransforms }.



% Local call (to a builtin-only):
%
% "If Gt is a function call A(Gt_1, ..., Gt_k), where A is an atom, then Rep(Gt)
% = {call,LINE,Rep(A),[Rep(Gt_1), ..., Rep(Gt_k)]}."
%
% Ex: {call,102, {atom,102,is_integer}, [{var,102,'X'}]}
%
% Note: the subject of a special case in erl_id_trans (guard_test/1), delegated
% appropriately to the direct counterpart.
%
transform_guard_test( GuardTest={ 'call', Line, FunctionASTName, GuardTests },
					  Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test local call ~p...",
	%						 [ GuardTest ] ),

	{ atom, _Line, FunctionName } = ast_type:check_ast_atom( FunctionASTName,
															 Line ),

	% Here we check whether FunctionName designates an Erlang BIF that is
	% allowed in guards:
	%
	% (see also: guard_test/1 in erl_id_trans)
	%
	FunctionArity = length( GuardTests ),

	case erl_internal:type_test( FunctionName, FunctionArity ) of

		true ->
			{ NewGuardTests, NewTransforms } =
				direct_transform_guard_tests( GuardTests, Transforms ),

			NewExpr = { 'call', Line, FunctionASTName, NewGuardTests },

			{ NewExpr, NewTransforms };

		false ->
			direct_transform_guard_test( GuardTest, Transforms )

	end;

transform_guard_test( AnyOtherGuardTest, Transforms ) ?rec_guard ->
	direct_transform_guard_test( AnyOtherGuardTest, Transforms ).





% Transforms specified guard test, operating relevant AST transformations.
%
% (see section 7.6 for complete detail)
%
% Note that we should not consider that guard tests are simply AST expressions
% (they are only a very specific subset thereof, with guard-specific rules -
% hence not plugging here to any expression-generic code).
%
% If the guard test is a bitstring constructor:
%
% "If Gt is a bitstring constructor <<Gt_1:Size_1/TSL_1, ...,
% Gt_k:Size_k/TSL_k>>, where each Size_i is a guard test and each TSL_i is a
% type specificer list, then Rep(Gt) =
% {bin,LINE,[{bin_element,LINE,Rep(Gt_1),Rep(Size_1),Rep(TSL_1)}, ...,
% {bin_element,LINE,Rep(Gt_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see
% above. An omitted Size_i is represented by default. An omitted TSL_i is
% represented by default."
%
-spec direct_transform_guard_test( ast_guard_test(), ast_transforms() ) ->
									{ ast_guard_test(), ast_transforms() }.
direct_transform_guard_test( _GuardTest={ 'bin', Line, BinElements },
							 Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test bitstring "
	%						 "constructor ~p...", [ GuardTest ] ),

	% Actually we are not sure Gt_1 is a guard test (maybe is more globally an
	% expression):
	%
	%NewBinElements = ast_bitstring:transform_bin_elements( BinElements,
	%					   Transforms, fun direct_transform_guard_test/2 ),

	{ NewBinElements, NewTransforms } =
		ast_bitstring:transform_bin_elements( BinElements, Transforms ),

	NewExpr = { 'bin', Line, NewBinElements },

	Res = { NewExpr, NewTransforms },

	%ast_utils:display_debug( "... returning guard test bitstring "
	%						 "constructor and state ~p", [ Res ] ),

	Res;


% "If Gt is a cons skeleton [Gt_h | Gt_t], then Rep(Gt) =
% {cons,LINE,Rep(Gt_h),Rep(Gt_t)}."
%
direct_transform_guard_test( _GuardTest={ 'cons', Line, HeadGuardTest,
										  TailGuardTest },
							 Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test cons skeleton ~p...",
	%						 [ GuardTest ] ),

	% We do not try to generalise this code (ex: by defining and using a
	% ast_transform:transform_cons( H, T, Transforms)) as, depending on the
	% context (ex: for guards), different AST structures are expected.

	% So, not ast_expression:transform_expression/2 here:
	{ NewHeadGuardTest, HeadTransforms } =
		direct_transform_guard_test( HeadGuardTest, Transforms ),

	% Expecting a list for tail?
	{ NewTailGuardTest, TailTransforms } =
		direct_transform_guard_test( TailGuardTest, HeadTransforms ),

	NewExpr = { 'cons', Line, NewHeadGuardTest, NewTailGuardTest },

	Res = { NewExpr, TailTransforms },

	%ast_utils:display_debug( "... returning guard test cons skeleton "
	%                         "and state ~p", [ Res ] ),

	Res;


% Local call:
%
% (already listed in transform_guard_test/2)
%
direct_transform_guard_test( _GuardTest={ 'call', LineCall,
			FunASTAtom={ atom, _LineFun, FunctionName }, SubGuardTests },
							 Transforms ) ?rec_guard ->

	FunctionArity = length( SubGuardTests ),

	case erl_internal:guard_bif( FunctionName, FunctionArity ) of

		true ->
			{ NewSubGuardTests, NewTransforms } =
				direct_transform_guard_tests( SubGuardTests, Transforms ),
			NewExpr = { 'call', LineCall, FunASTAtom, NewSubGuardTests },


			{ NewExpr, NewTransforms };

		false ->
			ast_utils:raise_usage_error( "call to invalid guard ~ts/~B.",
				[ FunctionName, FunctionArity ],
				Transforms#ast_transforms.transformed_module_name,
				LineCall )

	end;


% Remote call (only to the 'erlang' module):
%
% "If Gt is a function call A_m:A(Gt_1, ..., Gt_k), where A_m is the atom erlang
% and A is an atom or an operator, then Rep(Gt) =
% {call,LINE,{remote,LINE,Rep(A_m),Rep(A)},[Rep(Gt_1), ..., Rep(Gt_k)]}.
%
direct_transform_guard_test( _GuardTest={ 'call', LineCall,
			R={ remote, LineRemote, { atom, _LineMod, _Module=erlang },
				{ atom, _LineAtom, FunctionName } }, SubGuardTests },
							 Transforms ) ?rec_guard ->

	% Here, Module can only be 'erlang', and FunctionName could be checked, yet
	% we are not (re)implementing the compiler.

	%ast_utils:display_debug( "Intercepting guard test remote call ~p...",
	%						 [ GuardTest ] ),

	% Waiting for an operator to be met:
	%ast_type:check_ast_atom( FunctionName, Line2 ),

	FunctionArity = length( SubGuardTests ),

	case erl_internal:guard_bif( FunctionName, FunctionArity )
		orelse erl_internal:arith_op( FunctionName, FunctionArity )
		orelse erl_internal:comp_op( FunctionName, FunctionArity )
		orelse erl_internal:bool_op( FunctionName, FunctionArity ) of

		true ->
			{ NewSubGuardTests, NewTransforms } =
				direct_transform_guard_tests( SubGuardTests, Transforms ),

			NewGuardTest = { 'call', LineCall, R, NewSubGuardTests },

			Res = { NewGuardTest, NewTransforms },

			%ast_utils:display_debug( "... returning guard test remote call "
			%                         "and state ~p", [ Res ] ),

			Res;

		false ->
			ast_utils:raise_usage_error(
				"invalid remote call to an erlang:~ts/~B guard.",
				[ FunctionName, FunctionArity ],
				Transforms#ast_transforms.transformed_module_name,
				LineRemote )

	end;


% Remote call (only to the 'erlang' module):
%
% "If Gt is a function call A_m:A(Gt_1, ..., Gt_k), where A_m is the atom erlang
% and A is an atom or an operator, then Rep(Gt) =
% {call,LINE,{remote,LINE,Rep(A_m),Rep(A)},[Rep(Gt_1), ..., Rep(Gt_k)]}.
%
direct_transform_guard_test( _GuardTest={ 'call', LineCall,
			RemoteAST={ remote, LineRemote, { atom, _LineMod, _Module },
				FunctionDesignator }, SubGuardTests },
							 Transforms ) ?rec_guard ->

	% Here, Module can only be 'erlang', and FunctionDesignator could be
	% checked, yet we are not (re)implementing the compiler.

	%ast_utils:display_debug( "Intercepting guard test remote call ~p...",
	%						 [ GuardTest ] ),

	% Waiting for an operator to be met:
	ast_type:check_ast_atom( FunctionDesignator, LineRemote ),

	{ NewSubGuardTests, NewTransforms } = lists:mapfoldl(
			fun direct_transform_guard_test/2, _Acc0=Transforms,
			_List=SubGuardTests ),

	NewGuardTest = { 'call', LineCall, RemoteAST, NewSubGuardTests },

	Res = { NewGuardTest, NewTransforms },

	%ast_utils:display_debug( "... returning guard test remote call "
	%                         "and state ~p", [ Res ] ),

	Res;



% "If Gt is a map creation #{A_1, ..., A_k}, where each A_i is an association
% Gt_i_1 => Gt_i_2 or Gt_i_1 := Gt_i_2, then Rep(Gt) = {map,LINE,[Rep(A_1), ...,
% Rep(A_k)]}."
%
direct_transform_guard_test( _GuardTest={ 'map', Line, MapAssociations },
							 Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test map creation ~p...",
	%						 [ GuardTest ] ),

	%NewMapAssociations = ast_map:transform_map_associations( MapAssociations,
	%						Transforms, fun direct_transform_guard_test/2 ),

	{ NewMapAssociations, NewTransforms } =
		direct_transform_guard_tests( MapAssociations, Transforms ),

	NewGuardTest = { 'map', Line, NewMapAssociations },

	Res = { NewGuardTest, NewTransforms },

	%ast_utils:display_debug( "... returning guard test map creation "
	%                         "and state ~p", [ Res ] ),

	Res;


% "If Gt is a map update Gt_0#{A_1, ..., A_k}, where each A_i is an association
% Gt_i_1 => Gt_i_2 or Gt_i_1 := Gt_i_2, then Rep(Gt) =
% {map,LINE,Rep(Gt_0),[Rep(A_1), ..., Rep(A_k)]}."
%
direct_transform_guard_test( _GuardTest={ 'map', Line, BaseMap,
										  MapAssociations },
							 Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test map update ~p...",
	%						 [ GuardTest ] ),

	{ NewBaseMap, BaseTransforms } =
		direct_transform_guard_test( BaseMap, Transforms ),

	%NewMapAssociations = ast_map:transform_map_associations( MapAssociations,
	%						Transforms, fun direct_transform_guard_test/2 ),

	{ NewMapAssociations, GuardTransforms } =
		direct_transform_guard_tests( MapAssociations, BaseTransforms ),

	NewGuardTest = { 'map', Line, NewBaseMap, NewMapAssociations },

	Res = { NewGuardTest, GuardTransforms },

	%ast_utils:display_debug( "... returning guard test map update "
	%                         "and state ~p", [ Res ] ),

	Res;



% "If A is an association type K => V, where K and V are types, then Rep(A) =
% {type,LINE,map_field_assoc,[Rep(K),Rep(V)]}."
%
direct_transform_guard_test( _GuardTest={ 'map_field_assoc', Line, Key, Value },
							 Transforms ) ?rec_guard ->

	{ NewKey, KeyTransforms } = direct_transform_guard_test( Key, Transforms ),

	{ NewValue, ValueTransforms } =
		direct_transform_guard_test( Value, KeyTransforms ),

	NewExpr = { 'map_field_assoc', Line, NewKey, NewValue },

	{ NewExpr, ValueTransforms };


% "If A is an association type K := V, where K and V are types, then Rep(A) =
% {type,LINE,map_field_exact,[Rep(K),Rep(V)]}."
%
direct_transform_guard_test( _GuardTest={ 'map_field_exact', Line, Key, Value },
							 Transforms ) ?rec_guard ->

	{ NewKey, KeyTransforms } = direct_transform_guard_test( Key, Transforms ),

	{ NewValue, ValueTransforms } =
		direct_transform_guard_test( Value, KeyTransforms ),

	NewExpr = { 'map_field_exact', Line, NewKey, NewValue },

	{ NewExpr, ValueTransforms };


% "If Gt is nil, [], then Rep(Gt) = {nil,LINE}."
%
direct_transform_guard_test( GuardTest={ 'nil', _Line },
							 Transforms ) ?rec_guard ->
	{ GuardTest, Transforms };


% "If Gt is an operator guard test Gt_1 Op Gt_2, where Op is a binary operator
% other than match operator =, then Rep(Gt) = {op,LINE,Op,Rep(Gt_1),Rep(Gt_2)}.
%
% Since R11B, andalso/orelse are allowed in guards.
%
%direct_transform_guard_test( GuardTest={ 'op', Line, Operator, LeftOperand,
%										 RightOperand },
%							 Transforms ) when Operator =/= '=' ->
direct_transform_guard_test( _GuardTest={ 'op', Line, Operator, LeftOperand,
										  RightOperand },
							 Transforms )
  when ( Operator =:= 'andalso' orelse Operator =:= 'orelse' )
	   ?andalso_rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test andalso/orelse binary "
	%						 "operator ~p...", [ GuardTest ] ),

	{ NewLeftOperand, LeftTransforms } =
		direct_transform_guard_test( LeftOperand, Transforms ),

	{ NewRightOperand, RightTransforms } =
		direct_transform_guard_test( RightOperand, LeftTransforms ),

	NewGuardTest = { 'op', Line, Operator, NewLeftOperand, NewRightOperand },

	Res = { NewGuardTest, RightTransforms },

	%ast_utils:display_debug( "... returning guard test andalso/orelse binary "
	%						 "operator and state ~p", [ Res ] ),

	Res;


direct_transform_guard_test( _GuardTest={ 'op', Line, Operator, LeftOperand,
										  RightOperand },
							 Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test binary "
	%						 "operator ~p...", [ GuardTest ] ),

	case erl_internal:arith_op( Operator, 2 )
		orelse erl_internal:bool_op( Operator, 2 )
		orelse erl_internal:comp_op( Operator, 2 ) of

		true ->

			{ NewLeftOperand, LeftTransforms } =
				direct_transform_guard_test( LeftOperand, Transforms ),

			{ NewRightOperand, RightTransforms } =
				direct_transform_guard_test( RightOperand, LeftTransforms ),

			NewGuardTest = { 'op', Line, Operator, NewLeftOperand,
							 NewRightOperand },

			Res = { NewGuardTest, RightTransforms },

			%ast_utils:display_debug( "... returning guard test binary "
			%						 "operator and state ~p", [ Res ] ),

			Res;

		false ->
			ast_utils:raise_usage_error(
				"call to invalid binary operator '~p' in guard.", [ Operator ],
				Transforms#ast_transforms.transformed_module_name, Line )

	end;


% "If Gt is an operator guard test Op Gt_0, where Op is a unary operator, then
% Rep(Gt) = {op,LINE,Op,Rep(Gt_0)}.
%
direct_transform_guard_test( _GuardTest={ 'op', Line, Operator, Operand },
							 Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test unary operator ~p...",
	%						 [ GuardTest ] ),

	case erl_internal:arith_op( Operator, 1 )
		orelse erl_internal:bool_op( Operator, 1 ) of

		true ->
			{ NewOperand, NewTransforms } =
				direct_transform_guard_test( Operand, Transforms ),

			NewGuardTest = { 'op', Line, Operator, NewOperand },

			Res = { NewGuardTest, NewTransforms },

			%ast_utils:display_debug( "... returning guard test unary operator "
			%						  "and state ~p", [ Res ] ),

			Res;

		false ->
			ast_utils:raise_usage_error(
				"call to invalid unary operator '~p' in guard.", [ Operator ],
				Transforms#ast_transforms.transformed_module_name, Line )

	end;


% "If Gt is a parenthesized guard test ( Gt_0 ), then Rep(Gt) = Rep(Gt_0), that
% is, parenthesized guard tests cannot be distinguished from their bodies."


% "If Gt is a record creation #Name{Field_1=Gt_1, ..., Field_k=Gt_k}, where each
% Field_i is an atom or _, then Rep(Gt) =
% {record,LINE,Name,[{record_field,LINE,Rep(Field_1),Rep(Gt_1)}, ...,
% {record_field,LINE,Rep(Field_k),Rep(Gt_k)}]}.
%
direct_transform_guard_test( _GuardTest={ 'record', Line, RecordName,
										  RecordFieldInits },
							 Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test record creation ~p...",
	%						 [ GuardTest ] ),

	ast_type:check_ast_atom( RecordName, Line ),

	%NewRecordFieldInits = ast_record:transform_record_field_definitions(
	%										  RecordFieldInits, Transforms ),

	{ NewRecordFieldInits, NewTransforms } =
		transform_record_field_inits( RecordFieldInits, Transforms ),

	NewGuardTest = { 'record', Line, RecordName, NewRecordFieldInits },

	Res = { NewGuardTest, NewTransforms },

	%ast_utils:display_debug( "... returning guard test record creation "
	%                         "and state ~p", [ Res ] ),

	Res;


% "If Gt is a record field access Gt_0#Name.Field, where Field is an atom, then
% Rep(Gt) = {record_field,LINE,Rep(Gt_0),Name,Rep(Field)}."
%
direct_transform_guard_test( _GuardTest={ 'record_field', Line, RecordGuardTest,
										  RecordName, FieldGuardTest },
							 Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test record field access "
	%						 "~p...", [ GuardTest ] ),

	{ NewRecordGuardTest, RecTransforms } =
		direct_transform_guard_test( RecordGuardTest, Transforms ),

	ast_type:check_ast_atom( RecordName, Line ),

	%ast_type:check_ast_atom( FieldGuardTest, Line ),

	{ NewFieldGuardTest, FieldTransforms } =
		direct_transform_guard_test( FieldGuardTest, RecTransforms ),

	NewGuardTest = { 'record_field', Line, NewRecordGuardTest, RecordName,
					 NewFieldGuardTest },

	Res = { NewGuardTest, FieldTransforms },

	%ast_utils:display_debug( "... returning guard test record field "
	%						  "access ~p", [ Res ] ),

	Res;


% "If Gt is a record field index #Name.Field, where Field is an atom, then
% Rep(Gt) = {record_index,LINE,Name,Rep(Field)}."
%
direct_transform_guard_test( _RecordGuardTest={ 'record_index', Line,
												RecordName, FieldName },
							 Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test record field index "
	%						 "~p...", [ RecordGuardTest ] ),

	ast_type:check_ast_atom( RecordName, Line ),

	ast_type:check_ast_atom( FieldName, Line ),

	{ NewFieldName, NewTransforms } =
		direct_transform_guard_test( FieldName, Transforms ),

	NewRecordGuardTest = { 'record_index', Line, RecordName, NewFieldName },

	Res = { NewRecordGuardTest, NewTransforms },

	%ast_utils:display_debug( "... returning guard test record field "
	%						  "index and state ~p", [ Res ] ),

	Res;


% "If Gt is a tuple skeleton {Gt_1, ..., Gt_k}, then Rep(Gt) =
% {tuple,LINE,[Rep(Gt_1), ..., Rep(Gt_k)]}.
%
direct_transform_guard_test( _GuardTest={ 'tuple', Line, GuardTests },
							 Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting guard test tuple skeleton ~p...",
	%						 [ GuardTest ] ),

	{ NewGuardTests, NewTransforms } =
		direct_transform_guard_tests( GuardTests, Transforms ),

	NewGuardTest = { 'tuple', Line, NewGuardTests },

	Res = { NewGuardTest, NewTransforms },

	%ast_utils:display_debug( "... returning guard test tuple skeleton "
	%                         "and state ~p", [ Res ] ),

	Res;


% "If Gt is a variable pattern V, then Rep(Gt) = {var,LINE,A}, where A is an
% atom with a printname consisting of the same characters as V."
%
direct_transform_guard_test( GuardTest={ 'var', _Line, VarName },
							 Transforms ) ?rec_guard ->

	type_utils:check_atom( VarName ),

	{ GuardTest, Transforms };


% "If Gt is an atomic literal L, then Rep(Gt) = Rep(L)."
direct_transform_guard_test( E={ AtomicLiteralType, _Line, _Value },
							 Transforms )
  when ( AtomicLiteralType =:= 'atom'
		 orelse AtomicLiteralType =:= 'char'
		 orelse AtomicLiteralType =:= 'float'
		 orelse AtomicLiteralType =:= 'integer'
		 orelse AtomicLiteralType =:= 'string' ) ?andalso_rec_guard ->

	ast_value:transform_value( E, Transforms );


% Default, catch-all error clause:
direct_transform_guard_test( Other, Transforms )
  when is_record( Transforms, ast_transforms ) ->
	ast_utils:raise_usage_error( "call to invalid guard test '~p'.", [ Other ],
		Transforms#ast_transforms.transformed_module_name ).
