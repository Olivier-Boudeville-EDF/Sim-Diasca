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



% Module in charge of handling records defined or used within an AST.
%
% See http://erlang.org/doc/apps/erts/absform.html for more information.
%
-module(ast_record).



% Note that records have to be managed differently, depending on various
% contexts, i.e. if being:
%
% - defined (then in a top-level form)
% - or found in:
%    * a pattern
%    * a guard test
%    * an expression
%    * a type
%
% To manage these last four cases, we used to rely on higher-functions for
% transformations, in which a transform fun (ex:
% ast_expression:transform_expression/2) was provided as a context-dependent
% parameter (see transform_record_field_definitions/2 and all).
%
% Now, we prefer to rely on a more flexible per-context version of the
% transformation, located in the module corresponding to the context (ex:
% ast_expression), and only the transformation of the record definition is
% managed here.


% Shorthands:

-type line() :: ast_base:line().
-type form() :: ast_base:form().
-type located_form() :: ast_info:located_form().

-type ast_element() :: ast_base:ast_element().
-type ast_transforms() :: ast_transform:ast_transforms().


-type ast_type() :: ast_type:ast_type().
-type maybe_ast_type() :: ast_type:maybe_ast_type().

-type maybe_ast_immediate_value() :: ast_value:maybe_ast_immediate_value().

-type record_table() :: ast_info:record_table().

-type record_name() :: basic_utils:record_name().

-type record_definition() :: ast_info:record_definition().
-type record_pair() :: { record_name(), record_definition() }.

-type field_table() :: ast_info:field_table().

-type field_name() :: basic_utils:field_name().


% Defines the field of a type of record (type and default value, if specified;
% lines are also stored so that the full, actual form can be recreated - far
% clearer to interpret afterwards).
%
-type field_definition() :: { maybe_ast_type(), maybe_ast_immediate_value(),
							  line(), line() }.


-type field_pair() :: { field_name(), field_definition() }.


% Field identifier, possibly '_':
-type field_id() :: field_name() | '_'.


-type ast_field_id() :: ast_base:ast_atom().


% AST definition for a field of a record, when creating it.
%
-type ast_record_field_definition() ::
		ast_record_field_definition( ast_base:ast_element() ).



% AST definition for a field of a record, when creating it.
%
% {record_field,LINE,Rep(Field_1),Rep(ValueType)}
%
-type ast_untyped_record_field_definition( ValueType ) ::
		{ 'record_field', line(), ast_field_id(), ValueType }.


-type ast_untyped_record_field_definition() ::
		ast_untyped_record_field_definition( term() ).


-type ast_typed_record_field_definition( ValueType ) ::
		{ 'typed_record_field', line(),
		  ast_untyped_record_field_definition( ValueType ),
		  ast_type() }.


-type ast_record_field_definition( ValueType ) ::
		ast_untyped_record_field_definition( ValueType )
	  | ast_typed_record_field_definition( ValueType ).


% Typically used in ast_pattern:
-type ast_pattern_field() :: { 'record_field', line(),
							   ast_field_id() | { 'var', line(), '_' },
							   ast_pattern:ast_pattern() }.



-export_type([ record_pair/0,
			   field_definition/0, field_id/0, field_name/0, field_pair/0,
			   ast_field_id/0,
			   ast_untyped_record_field_definition/1,
			   ast_untyped_record_field_definition/0,
			   ast_typed_record_field_definition/1,
			   ast_record_field_definition/0, ast_record_field_definition/1,
			   ast_pattern_field/0 ]).


-export([ transform_record_definitions/2,
		  transform_record_field_definitions/2,
		  transform_record_field_definition/2,
		  get_located_forms_for/1 ]).


% For the table macro:
-include("meta_utils.hrl").


% For the ast_transforms record:
-include("ast_transform.hrl").

% For rec_guard-related defines:
-include("ast_utils.hrl").



% Often names (ex: of a record, or a field) are transmitted as parameters
% whereas they are usually not necessary, yet it is useful at least for error
% reporting.



% Transforms the specified record definitions (ex: coming for the 'records'
% field of a module_info record), according to specified transforms.
%
-spec transform_record_definitions( ast_info:record_table(),
		   ast_transforms() ) -> { ast_info:record_table(), ast_transforms() }.
transform_record_definitions( RecordTable, Transforms ) ?rec_guard ->

	ast_utils:display_trace( "transforming the definition of following "
							 "records: ~p", [ ?table:keys( RecordTable ) ] ),

	% { record_name(), record_definition() } pairs:
	RecordPairs = ?table:enumerate( RecordTable ),

	{ NewRecordPairs, NewTransforms } = lists:mapfoldl(
		fun transform_record_pair/2, _Acc0=Transforms,
		_List=RecordPairs ),

	NewRecordTable = ?table:new( NewRecordPairs ),

	{ NewRecordTable, NewTransforms }.



% Transforms the specified record pair: { RecordName, RecordDef }.
%
% Allows to keep around the record name, to recreate the record table more
% easily.
%
-spec transform_record_pair( record_pair(), ast_transforms() ) ->
									 { record_pair(), ast_transforms() }.
transform_record_pair(
  _RecordPair={ RecordName, _RecordDefinition={ FieldTable, Loc, Line } },
  Transforms ) ?rec_guard ->

	% { FieldName, FieldDefinition } pairs:
	FieldPairs = ?table:enumerate( FieldTable ),

	{ NewFieldPairs, NewTransforms } = lists:mapfoldl(
		fun transform_field_pair/2, _Acc0=Transforms,
		_List=FieldPairs ),

	ast_utils:display_trace( "transforming record ~p", [ RecordName ] ),

	NewFieldTable = ?table:new( NewFieldPairs ),

	NewRecordDefinition = { NewFieldTable, Loc, Line },

	{ { RecordName, NewRecordDefinition }, NewTransforms }.



% Transforms the specified field definition.
%
-spec transform_field_pair( { field_name(), field_definition() },
		ast_transforms() ) ->
					 { { field_name(), field_definition() }, ast_transforms() }.
transform_field_pair(
  { FieldName,
	_FieldDescription={ FieldType, FieldDefaultValue } },
  Transforms ) ?rec_guard ->

	{ NewFieldType, FieldTransforms } =
		transform_field_definition_type( FieldType, Transforms ),

	{ NewFieldDefaultValue, DefTransforms } =
		transform_field_definition_default_value( FieldDefaultValue,
												  FieldTransforms ),

	NewFieldDescription = { NewFieldType, NewFieldDefaultValue },

	{ { FieldName, NewFieldDescription }, DefTransforms }.



% Transforms the specified field type.
%
-spec transform_field_definition_type( maybe_ast_type(), ast_transforms() ) ->
										{ maybe_ast_type(), ast_transforms() }.
transform_field_definition_type( _FieldType=undefined,
								 Transforms ) ?rec_guard ->
	{ undefined, Transforms };

transform_field_definition_type( FieldType, Transforms ) ?rec_guard ->
	ast_type:transform_type( FieldType, Transforms ).



% Transforms the specified field default value.
%
-spec transform_field_definition_default_value( maybe_ast_immediate_value(),
	  ast_transforms() ) -> { maybe_ast_immediate_value(), ast_transforms() }.
transform_field_definition_default_value( _FieldDefaultValue=undefined,
										  Transforms ) ?rec_guard ->
	{ undefined, Transforms };

transform_field_definition_default_value( FieldDefaultValue,
										  Transforms ) ?rec_guard ->
	ast_value:transform_value( FieldDefaultValue, Transforms ).



% Transforms specified record fields, at creation, applying to each record field
% the specified function to perform the relevant transformations (that depends
% on the context; ex: if being in a guard, in an expression, etc.).
%
% (counterpart of record_inits/1 in erl_id_trans)
%
-spec transform_record_field_definitions( [ ast_record_field_definition() ],
			ast_transforms() ) -> [ ast_record_field_definition() ].
transform_record_field_definitions( RecordFields, Transforms ) ?rec_guard ->
	lists:mapfoldl( fun transform_record_field_definition/2, _Acc0=Transforms,
					_List=RecordFields ).



% Transforms specified record field definition.
%
% Ex: {record_field,LINE,Rep(Field_k),Rep(Gt_k)}.
%
-spec transform_record_field_definition( ast_record_field_definition(),
										 ast_transforms() ) ->
			{ ast_record_field_definition(), ast_transforms() }.
% With a value and no type specified here:
transform_record_field_definition(
  _RF={ 'record_field', Line, ASTFieldName, ASTValue },
  Transforms ) ?rec_guard ->

	%ast_utils:display_trace( "transforming record field '~p' of value ~p"
	%						 " (type 1).", [ ASTFieldName, ASTValue ] ),

	{ NewASTFieldName, FieldTransforms } =
		transform_record_field_name( ASTFieldName, Transforms ),

	{ NewASTValue, ValueTransforms } =
		ast_value:transform_value( ASTValue, FieldTransforms ),

	NewRF = { 'record_field', Line, NewASTFieldName, NewASTValue },

	{ NewRF, ValueTransforms };


% With no value and no type specified here:
transform_record_field_definition( _RF={ 'record_field', Line, ASTFieldName },
								   Transforms ) ?rec_guard ->

	%ast_utils:display_trace( "transforming record field '~p' (type 2).",
	%						 [ ASTFieldName ] ),

	{ NewASTFieldName, NewTransforms } =
		transform_record_field_name( ASTFieldName, Transforms ),

	{ { 'record_field', Line, NewASTFieldName }, NewTransforms };


% With a value and a type specified here:
transform_record_field_definition(
  _RF={ 'typed_record_field', { 'record_field', Line, ASTFieldName, ASTValue },
		ASTType }, Transforms ) ?rec_guard ->

	%ast_utils:display_trace( "transforming record field '~p' of value ~p and "
	%		 "type ~p (type 3).", [ ASTFieldName, ASTValue, ASTType ] ),

	{ NewASTFieldName, NameTransforms } =
		transform_record_field_name( ASTFieldName, Transforms ),

	{ NewASTValue, ValueTransforms } =
		ast_value:transform_value( ASTValue, NameTransforms ),

	{ NewASTType, TypeTransforms } =
		ast_type:transform_type( ASTType, ValueTransforms ),

	RF = { 'typed_record_field',
		   { 'record_field', Line, NewASTFieldName, NewASTValue }, NewASTType },

	{ RF, TypeTransforms };


% With no value and a type specified here:
transform_record_field_definition(
  _RF={ 'typed_record_field', { 'record_field', Line, ASTFieldName }, ASTType },
  Transforms ) ?rec_guard ->

	%ast_utils:display_trace( "transforming record field '~p' of type ~p "
	%						 "(type 4).", [ ASTFieldName, ASTType ] ),

	{ NewASTFieldName, NameTransforms } =
		transform_record_field_name( ASTFieldName, Transforms ),

	{ NewASTType, TypeTransforms } =
		ast_type:transform_type( ASTType, NameTransforms ),

	RF = { 'typed_record_field', { 'record_field', Line, NewASTFieldName },
		   NewASTType },

	{ RF, TypeTransforms }.




% Transforms the name of the specified field.
%
-spec transform_record_field_name( ast_element(), ast_transforms() ) ->
										{ ast_element(), ast_transforms() }.
transform_record_field_name( ASTFieldName, Transforms ) ?rec_guard ->

	% Note: field names are full expressions here, but only atoms are allowed
	% by the parser (dixit the id parse transform).
	% So we could expect to have ASTFieldName={atom,Line,Value} here.

	%ast_type:check_ast_atom( ASTFieldName, Line ),
	%NewASTFieldName = TransformFun( ASTFieldName, Transforms ),

	{ [ NameExpr ], NewTransforms } =
		ast_expression:transform_expression( ASTFieldName, Transforms ),

	{ NameExpr, NewTransforms }.




% Returns located forms corresponding to specified record table.
%
-spec get_located_forms_for( record_table() ) -> [ located_form() ].
get_located_forms_for( RecordTable ) ->

	RecordPairs = ?table:enumerate( RecordTable ),

	lists:foldl( fun( { RecordName, RecordDef }, Acc ) ->
				  [ get_located_form_for_record( RecordName, RecordDef ) | Acc ]
				 end,
				 _Acc0=[],
				 _List=RecordPairs ).



% Returns a located form corresponding to specified record.
%
-spec get_located_form_for_record( record_name(), record_definition() ) ->
										 located_form().
get_located_form_for_record( RecordName,
							 _RecordDef={ FieldTable, Loc, RecordLine } ) ->

	FieldDefs = recompose_field_definitions( FieldTable ),

	Form = { attribute, RecordLine, record, { RecordName, FieldDefs } },

	{ Loc, Form }.



% Recomposes the forms corresponding to the specified record fields.
%
-spec recompose_field_definitions( field_table() ) -> [ form() ].
recompose_field_definitions( FieldTable ) ->
	[ recompose_field_definition( FieldName, FieldDef )
	  || { FieldName, FieldDef } <- FieldTable ].



% Recomposes the form corresponding to the specified record field.
%
-spec recompose_field_definition( field_name(), field_definition() ) -> form().
recompose_field_definition( FieldName,
		_FieldDef={ _MaybeASTType=undefined, _MaybeASTDefaultValue=undefined,
					FirstLine, SecondLine } ) ->
	{ 'record_field', FirstLine, { atom, SecondLine, FieldName } };

recompose_field_definition( FieldName,
		_FieldDef={ _MaybeASTType=undefined, ASTDefaultValue, FirstLine,
					SecondLine } ) ->
	{ 'record_field', FirstLine, { atom, SecondLine, FieldName },
	  ASTDefaultValue };

recompose_field_definition( FieldName,
		_FieldDef={ ASTType, _MaybeASTDefaultValue=undefined, FirstLine,
					SecondLine } ) ->
	{ 'typed_record_field', { 'record_field', FirstLine,
							  { atom, SecondLine, FieldName } }, ASTType };

recompose_field_definition( FieldName, _FieldDef={ ASTType, ASTDefaultValue,
												   FirstLine, SecondLine } ) ->
	{ 'typed_record_field',
	  { 'record_field', FirstLine, { atom, SecondLine, FieldName },
		ASTDefaultValue }, ASTType }.
