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


% @doc Module in charge of handling <b>types, but also variables and values</b>
% defined with an AST.
%
% See the "7.7 Types" section of [http://erlang.org/doc/apps/erts/absform.html]
% for more information.
%
-module(ast_type).


% For the table macro:
-include("meta_utils.hrl").

% For the type_info record:
-include("ast_info.hrl").

% For the ast_transforms record:
-include("ast_transform.hrl").

% For the rec_guard and default_generation_location defines:
-include("ast_utils.hrl").


% Section for types about types.


-type ast_type_definition() :: ast_base:form().
% An in-AST definition of a type.


% Note: the order of the fields matters (not arbitrary, in order to correspond
% to the actual AST terms).
%
% Not possible: -record( builtin_type, {
-record( type, {

		   % Location of this form in the current source file:
		   file_location = ?default_generation_location :: file_loc(),

		   % Name of the target type:
		   name :: type_name(),

		   % Type variables, i.e. types on which this type depends:
		   variables = [] :: [ ast_type() ] }).


-type ast_builtin_type() :: #type{}.
% Reference to a built-in type, in an AST.
%
% Ex:
% - {type,45,atom,[]}                           -- for atom()
% - {type,44,list,[{type,{44,5},boolean,[]}]}   -- for [ boolean() ]



% Note: the order of the fields matters (not arbitrary, to correspond to the
% actual AST terms).
%
-record( user_type, {

	% Location of this form in the current source file:
	file_location = ?default_generation_location :: file_loc(),

	% Name of the target type:
	name :: type_name(),

	% Type variables, i.e. types on which this type depends:
	variables = [] :: [ ast_type() ] } ).


-type ast_user_type() :: #user_type{}.
% Reference to a user-defined (local) type, in an AST.
%
% Ex: {user_type,{45,1},foo,[{type,45,atom,[]}]}     -- for foo( atom() )



% Note: the order of the fields matters (not arbitrary, to correspond to the
% actual AST terms).
%
-record( remote_type, {

	% Location of this form in the current source file:
	file_location = ?default_generation_location :: file_loc(),

	% More precisely, a list of three elements, two atoms and a list of
	% type variables, like in:
	% [ {atom,43,basic_utils}, {atom,43,maybe}, [{type,43,float,[]}] ]
	%
	spec :: [ ast_builtin_type() | [ ast_type() ] ] } ).


-type ast_remote_type() :: #remote_type{}.
% Reference to a remote type, in an AST.
%
% Example for basic_utils:maybe(float()):
% {remote_type,43,[{atom,43,basic_utils},{atom,43,maybe},[{type,43,float,[]}]]}


-type ast_type() :: ast_builtin_type() | ast_user_type() | ast_remote_type().
% Any kind of reference onto a type.


-type maybe_ast_type() :: basic_utils:maybe( ast_type() ).


% May be constrained or not (see http://erlang.org/doc/apps/erts/absform.html):
%-type function_type().


-type ast_field_description() :: tuple().
% The description of a field of a record.
%
% Ex : {typed_record_field, {record_field, 76, {atom,76,my_index}},
%           {remote_type, 76, [{atom,76,linear}, {atom,76,coordinate}, []]}},


-type ast_variable_name() :: atom().
% Includes '_'.


-type ast_variable_pattern() :: { 'var', file_loc(), ast_variable_name() }.
% Variable pattern.



-export_type([ ast_type_definition/0,
			   ast_builtin_type/0, ast_user_type/0, ast_remote_type/0,
			   ast_type/0, maybe_ast_type/0,
			   ast_field_description/0,
			   ast_variable_name/0, ast_variable_pattern/0 ]).


% For types, we used to propagate through transformation calls only the two
% local/remote type tables, yet it was not relevant enough: for example, when
% tranforming a record type, a field may have a default value defined (ex:
% table()), in which case we must be able to transform an expression as well.
%
% As a result, for types as well, we pass around the full transforms (i.e. the
% full ast_transforms record).




% Forging AST types:
%
% Note that when using the forge_*_type/N functions, type variables are expected
% to be already forged.
%
-export([ forge_boolean_type/0, forge_boolean_type/1,
		  forge_atom_type/0, forge_atom_type/1,
		  forge_pid_type/0, forge_pid_type/1,
		  forge_integer_type/0, forge_integer_type/1,
		  forge_float_type/0, forge_float_type/1,
		  forge_tuple_type/1, forge_tuple_type/2,
		  forge_list_type/1, forge_list_type/2,
		  forge_union_type/1, forge_union_type/2,
		  forge_builtin_type/3, forge_local_type/3,
		  forge_remote_type/4, forge_remote_type/6,
		  forge_type_variable/2 ]).


% Checking:
-export([ check_type_name/1, check_type_name/2,
		  check_type_definition/1, check_type_definition/2,

		  check_record_name/1, check_record_name/2,
		  check_type_id/1, check_type_id/2,
		  check_type_ids/1, check_type_ids/2,

		  check_type_variable/1, check_type_variable/2,
		  check_type_variables/1, check_type_variables/2,

		  check_ast_atom/1, check_ast_atom/2 ]).


% Transformations:
-export([ transform_type_table/2, transform_types_in_record_table/2,
		  transform_types/2, transform_type/2,
		  transform_association_type/2,
		  transform_type_variable/3 ]).


% Recomposition:
-export([ get_located_forms_for/2 ]).



% Shorthands:

-type module_name() :: meta_utils:module_name().
-type variable_name() :: meta_utils:variable_name().

-type file_loc() :: ast_base:file_loc().
-type form_context() :: ast_base:form_context().
-type ast_element() :: ast_base:ast_element().

-type located_form() :: ast_info:located_form().

-type type_table() :: ast_info:type_table().

-type type_name() :: type_utils:type_name().
-type type_id() :: type_utils:type_id().

-type record_table() :: ast_info:record_table().

-type field_table() :: ast_info:field_table().
-type field_pair() :: ast_record:field_pair().

-type type_info() :: ast_info:type_info().
-type type_pair() :: { type_id(), type_info() }.

-type record_definition() :: ast_info:record_definition().
-type record_pair() :: ast_record:record_pair().

-type ast_transforms() :: ast_transform:ast_transforms().



% Implementation notes:
%
% The use of lists:mapfoldl/3 should preferably be replaced by
% ?table:map_on_values/2.



% Transformation section.


% @doc Transforms the types in specified type table, according to the specified
% transforms.
%
-spec transform_type_table( type_table(), ast_transforms() ) ->
									{ type_table(), ast_transforms() }.
transform_type_table( TypeTable, Transforms ) ?rec_guard ->

	% {type_id(), type_info()} pairs:
	TypePairs = ?table:enumerate( TypeTable ),

	{ NewTypePairs, NewTransforms } = lists:mapfoldl(
			fun transform_type_info_pair/2, _Acc0=Transforms,
			_List=TypePairs ),

	NewTypeTable = ?table:new( NewTypePairs ),

	{ NewTypeTable, NewTransforms }.



% @doc Transforms specified function pair: {FunId, FunInfo}.
%
% Allows to keep around the function identifier, to recreate the function table
% more easily.
%
-spec transform_type_info_pair( type_pair(), ast_transforms() ) ->
										{ type_pair(), ast_transforms() }.
transform_type_info_pair( { TypeId,
							_TypeInfo=#type_info{ file_location=MaybeFileLoc,
												  definition=undefined,
												  exported=Export } },
							Transforms )
						when Export =/= [] ?andalso_rec_guard ->

	% We cannot let this error go through, as it would remain silent.

	% A context could be recreated with the module and line, and used to raise
	% the error, yet, at least for types, it is not unlikely that they are
	% exported in an header file and thus we would possibly be pointing to a
	% wrong place.

	ErrorMessage = text_utils:format( "type ~ts/~B is exported, yet has never "
									  "been defined.", pair:to_list( TypeId ) ),

	UsedFileLoc = case MaybeFileLoc of

		undefined ->
			{ 0, 0 };

		_ ->
			MaybeFileLoc

	end,

	ast_utils:raise_error( ErrorMessage, Transforms, UsedFileLoc );

	% So not: { { TypeId, TypeInfo }, Transforms };

transform_type_info_pair( { TypeId, TypeInfo }, Transforms ) ?rec_guard ->

	{ NewTypeInfo, NewTransforms } =
		transform_type_info( TypeInfo, Transforms ),

	{ { TypeId, NewTypeInfo }, NewTransforms }.



% (helper)
-spec transform_type_info( type_info(), ast_transforms() ) ->
									{ type_info(), ast_transforms() }.
transform_type_info( TypeInfo=#type_info{ definition=TypeDef },
					 Transforms ) ?rec_guard ->

	{ NewTypeDef, NewTransforms } = transform_type( TypeDef, Transforms ),

	NewTypeInfo = TypeInfo#type_info{ definition=NewTypeDef },

	{ NewTypeInfo, NewTransforms }.



% @doc Transforms the types in specified record table, according to the
% specified transforms.
%
-spec transform_types_in_record_table( record_table(), ast_transforms() ) ->
										{ record_table(), ast_transforms() }.
transform_types_in_record_table( RecordTable, Transforms ) ?rec_guard ->

	% {record_name(), record_definition()} pairs:
	RecordPairs = ?table:enumerate( RecordTable ),

	{ NewRecordPairs, NewTransforms } = lists:mapfoldl(
			fun transform_record_pair/2, _Acc0=Transforms, _List=RecordPairs ),

	NewRecordTable = ?table:new( NewRecordPairs ),

	{ NewRecordTable, NewTransforms }.



% @doc Transforms the specified record pair: {RecordName, RecordDef}.
%
% Allows to keep around the record name, to recreate the record table more
% easily.
%
-spec transform_record_pair( record_pair(), ast_transforms() ) ->
										{ record_pair(), ast_transforms() }.
transform_record_pair( { RecordName, RecordDef }, Transforms ) ?rec_guard ->

	%ast_utils:display_trace( "transforming definition of record '~ts'.",
	%                         [ RecordName ] ),

	{ NewRecordDef, NewTransforms } =
		transform_record_definition( RecordDef, Transforms ),

	%ast_utils:display_trace( "transformed definition of record '~ts' to:~n~p.",
	%                         [ RecordName, NewRecordDef ] ),

	{ { RecordName, NewRecordDef }, NewTransforms }.



% @doc Transforms the specified record definition.
-spec transform_record_definition( record_definition(), ast_transforms() ) ->
									{ record_definition(), ast_transforms() }.
transform_record_definition( _RecordDef={ FieldTable, ASTLoc, FileLoc },
							 Transforms ) ?rec_guard ->

	{ NewFieldTable, NewTransforms } =
		transform_field_table( FieldTable, Transforms ),

	NewRecordDef = { NewFieldTable, ASTLoc, FileLoc },

	{ NewRecordDef, NewTransforms }.



% @doc Transforms the specified fields.
-spec transform_field_table( field_table(), ast_transforms() ) ->
									{ field_table(), ast_transforms() }.
transform_field_table( FieldTable, Transforms ) ?rec_guard ->

	% Is already a list directly (no key/value pairs to preserve here):
	lists:mapfoldl( fun transform_field_pair/2, _Acc0=Transforms,
					_List=FieldTable ).



% @doc Transforms the specified field pair: {FieldName, FieldInfo}.
%
% Allows to keep around the field name, to recreate the field table more easily.
%
-spec transform_field_pair( field_pair(), ast_transforms() ) ->
									{ field_pair(), ast_transforms() }.
transform_field_pair( { FieldName, FieldDef }, Transforms ) ?rec_guard ->

	{ NewFieldDef, NewTransforms } =
		transform_field_definition( FieldDef, Transforms ),

	{ { FieldName, NewFieldDef }, NewTransforms }.


% @doc Transforms the specified field definition.
transform_field_definition( FieldDef={ _AstType=undefined, _AstValue=undefined,
									   _FirstFileLoc, _SecondFileLoc },
							Transforms ) ->

	%ast_utils:display_debug( "Field definition (clause #1):~n  ~p",
	%                         [ FieldDef ] ),

	{ FieldDef, Transforms };


transform_field_definition( _FieldDef={ _AstType=undefined, AstValue,
										FirstFileLoc, SecondFileLoc },
							Transforms ) ->

	%ast_utils:display_debug( "Field definition (clause #2):~n  ~p",
	%                         [ FieldDef ] ),

	{ [ NewAstValue ], NewTransforms } =
		ast_expression:transform_expression( AstValue, Transforms ),

	NewFieldDef = { undefined, NewAstValue, FirstFileLoc, SecondFileLoc },

	{ NewFieldDef, NewTransforms };


transform_field_definition( _FieldDef={ AstType, _AstValue=undefined,
								FirstFileLoc, SecondFileLoc }, Transforms ) ->

	%ast_utils:display_debug( "Field definition (clause #3):~n  ~p",
	%                         [ FieldDef ] ),

	{ NewAstType, NewTransforms } = transform_type( AstType, Transforms ),

	NewFieldDef = { NewAstType, undefined, FirstFileLoc, SecondFileLoc },

	{ NewFieldDef, NewTransforms };


transform_field_definition(
  _FieldDef={ AstType, AstValue, FirstFileLoc, SecondFileLoc }, Transforms ) ->

	%ast_utils:display_debug( "Field definition (clause #4):~n  ~p",
	%                         [ FieldDef ] ),

	{ NewAstType, TypeTransforms } = transform_type( AstType, Transforms ),

	{ [ NewAstValue ], ExprTransforms } =
		ast_expression:transform_expression( AstValue, TypeTransforms ),

	FieldDef = { NewAstType, NewAstValue, FirstFileLoc, SecondFileLoc },

	{ FieldDef, ExprTransforms }.



% @doc Transforms the specified list of types.
-spec transform_types( [ ast_type() ], ast_transforms() ) ->
								{ [ ast_type() ], ast_transforms() }.
transform_types( Types, Transforms ) ->
	% Is already a list directly (no key/value pairs to preserve here):
	lists:mapfoldl( fun transform_type/2, _Acc0=Transforms, _List=Types ).



% @doc Transforms types: traversing them recursively according to their
% specified structure, applying on them the specified transformations.
%
% Currently not going for a fully specialised, strict and 'just sufficient'
% traversal as permitted by [http://erlang.org/doc/apps/erts/absform.html]; yet
% still getting inspiration from its section 7.7.
%
% We currently consider that all type definitions correspond to an
% ast_type(), i.e. one of:
%
% - ast_utils:ast_builtin_type(): {type, FileLoc, TypeName, TypeVars}, where
% TypeVars are often (not always) a list; ex: {type, FILE_LOC, union, [Rep(T_1),
% ..., Rep(T_k)]} or {type, FILE_LOC, map, any}; we manage specifically the most
% common type designators, and traverse generically the others
%
% - ast_utils:ast_remote_type(): {remote_type, FileLoc, [ModuleType, TypeName,
% TypeVars]}
%
% - ast_utils:ast_user_type(): {user_type, FileLoc, TypeName, TypeVars}
%
%
% Notes:
%
% - clauses ordered according to the first atom (all plain types, then all
% remote types, then all user types)
%
% - records like #type, #user_type, could be used instead
%
% (helper)
%
-spec transform_type( ast_type(), ast_transforms() ) ->
							{ ast_type(), ast_transforms() }.

% Handling tuples:

% Fully-qualified tuple type found, ex:
% {type, 42, tuple, [{type,42,integer,[]}, {type,42,float,[]}]}
%
% "If T is a tuple type {T_1, ..., T_k}, then
% Rep(T) = {type, FILE_LOC, tuple, [Rep(T_1), ..., Rep(T_k)]}."
%
transform_type( _TypeDef={ 'type', FileLoc, 'tuple', ElementTypes },
				Transforms ) when is_list( ElementTypes ) ->

	% Is already a list directly (no key/value pairs to preserve here):
	{ NewElementTypes, NewTransforms } = lists:mapfoldl( fun transform_type/2,
						_Acc0=Transforms, _List=ElementTypes ),

	NewTypeDef = { 'type', FileLoc, 'tuple', NewElementTypes },

	{ NewTypeDef, NewTransforms };


% General tuple type found (i.e. tuple()):
%
% "If T is a tuple type tuple(), then Rep(T) = {type, FILE_LOC, tuple, any}."
%
transform_type( TypeDef={ 'type', _FileLoc, 'tuple', 'any' }, Transforms ) ->
	{ TypeDef, Transforms };

transform_type( TypeDef={ 'type', FileLoc, 'tuple', _Any }, _Transforms ) ->
	ast_utils:raise_error( [ unexpected_typedef_tuple_form, TypeDef ],
						   _Context=FileLoc );



% Handling lists:


% Fully-qualified list type found, ex: {type, 43, list, [{type,43,boolean,[]}]}.
%
% Lacking specification in the doc, extrapolated to:
%
% "If T is a list of elements of type A, then Rep(T) = {type, FILE_LOC, list,
% Rep(A)}."
%
transform_type( _TypeDef={ 'type', FileLoc, 'list', [ ElementType ] },
				Transforms ) ->

	{ NewElementType, NewTransforms } =
		transform_type( ElementType, Transforms ),

	NewTypeDef = { 'type', FileLoc, 'list', [ NewElementType ] },

	{ NewTypeDef, NewTransforms };


% General list type found (i.e. list()):
%
% Lacking specification in the doc, extrapolated to:
%
% "If T is a list type list(), then Rep(T) = {type, FILE_LOC, list, any}."
%
transform_type( TypeDef={ 'type', _FileLoc, 'list', 'any' }, Transforms ) ->
	{ TypeDef, Transforms };


% Yes, at least in some cases, list() may be translated as
% {type, FILE_LOC, list, []}:
%
transform_type( TypeDef={ 'type', _FileLoc, 'list', [] }, Transforms ) ->
	{ TypeDef, Transforms };


transform_type( TypeDef={ 'type', _FileLoc, 'list', _Any }, _Transforms ) ->
	ast_utils:raise_error( [ unexpected_typedef_list_form, TypeDef ] );


% Empty list type found (i.e. []):
%
% "If T is the empty list type [], then Rep(T) = {type, FileLoc, nil, []}."
%
transform_type( TypeDef={ 'type', _FileLoc, 'nil', [] }, Transforms ) ->
	{ TypeDef, Transforms };



% Handling binaries:

% "If T is a bitstring type <<_:M,_:_*N>>, where M and N are singleton integer
% types, then Rep(T) = {type, FILE_LOC, binary, [Rep(M), Rep(N)]}."
%
transform_type( _TypeDef={ 'type', FileLoc, 'binary', [ M, N ] },
				Transforms ) ->

	% To be removed once ever seen displayed:
	%ast_utils:display_warning( "Not transforming binary elements ~p and ~p.",
	%							[ M, N ] ),

	% Finally transformed, as managed in erl_id_trans:

	{ NewM, MTransforms } = transform_type( M, Transforms ),

	{ NewN, NTransforms } = transform_type( N, MTransforms ),

	TypeDef = { 'type', FileLoc, 'binary', [ NewM, NewN ] },

	{ TypeDef, NTransforms };



% "If T is an integer range type L .. H, where L and H are singleton integer
% types, then Rep(T) = {type, FILE_LOC, range, [Rep(L), Rep(H)]}."
%
transform_type( _TypeDef={ 'type', FileLoc, 'range', [ L, H ] }, Transforms ) ->

	% To be removed once ever seen displayed:
	%ast_utils:display_warning( "Not transforming range bound ~p and ~p.",
	%                           [ L, H ] ),

	% Finally transformed, as managed in erl_id_trans:

	{ NewL, LTransforms } = transform_type( L, Transforms ),

	{ NewH, HTransforms } = transform_type( H, LTransforms ),

	NewTypeDef = { 'type', FileLoc, 'range', [ NewL, NewH ] },

	{ NewTypeDef, HTransforms };


% Handling maps:

% "If T is a map type map(), then Rep(T) = {type, FILE_LOC, map, any}."
%
transform_type( TypeDef={ 'type', _FileLoc, 'map', 'any' }, Transforms ) ->
	{ TypeDef, Transforms };


% "If T is a map type #{A_1, ..., A_k}, where each A_i is an association type,
% then Rep(T) = {type, FILE_LOC, map, [Rep(A_1), ..., Rep(A_k)]}."
%
transform_type( _TypeDef={ 'type', FileLoc, TargetType='map', AssocTypes },
				Transforms ) ->

	% Is already a list directly (no key/value pairs to preserve here):
	{ NewAssocTypes, NewTransforms } = lists:mapfoldl(
		fun transform_association_type/2, _Acc0=Transforms, _List=AssocTypes ),

	NewTypeDef = { 'type', FileLoc, TargetType, NewAssocTypes },

	{ NewTypeDef, NewTransforms };



% Handling lambda functions:


% "If T is a fun type fun(), then Rep(T) = {type, FILE_LOC, 'fun', []}."
transform_type( TypeDef={ 'type', _FileLoc, _TargetType='fun', [] },
				Transforms ) ->
	{ TypeDef, Transforms };


% "If T is a fun type fun((...) -> T_0), then Rep(T) =
% {type, FILE_LOC, 'fun', [{type, FILE_LOC, any}, Rep(T_0)]}."
%
transform_type( _TypeDef={ 'type', FileLoc1, TargetType='fun',
							[ Any={ 'type', _FileLoc2, 'any' } ], ResultType },
				Transforms ) ->

	{ NewResultType, NewTransforms } = transform_type( ResultType, Transforms ),

	NewTypeDef = { 'type', FileLoc1, TargetType, [ Any, NewResultType ] },

	{ NewTypeDef, NewTransforms };


% "If T is a fun type fun(Ft), where Ft is a function type, then Rep(T) =
% Rep(Ft)."
%
% ParamsResult corresponds to any [Params, ResultType]:
%
transform_type( TypeDef={ 'type', _FileLoc, 'fun', _ParamsResult },
				Transforms ) ->
	ast_function:transform_function_type( TypeDef, Transforms );


% Handling union types:
%
% "If T is a type union T_1 | ... | T_k, then Rep(T) =
% {type, FILE_LOC, union, [Rep(T_1), ..., Rep(T_k)]}."
%
transform_type( _TypeDef={ 'type', FileLoc, TargetType='union', UnifiedTypes },
				Transforms ) ->

	% Is already a list directly (no key/value pairs to preserve here):
	{ NewUnifiedTypes, NewTransforms } = lists:mapfoldl(
		fun transform_type/2, _Acc0=Transforms, _List=UnifiedTypes ),

	NewTypeDef = { 'type', FileLoc, TargetType, NewUnifiedTypes },

	{ NewTypeDef, NewTransforms };


% Simple built-in type, like 'boolean()', translating in '{type, 57, boolean,
% []}':
%
transform_type( TypeDef={ 'type', FileLoc, BuiltinType, _TypeVars=[] },
				Transforms ) ->

	case lists:member( BuiltinType,
					   type_utils:get_ast_simple_builtin_types() ) of

		true ->
			{ TypeDef, Transforms };

		false ->
			case BuiltinType of

				bool ->
					ast_utils:raise_error( "the bool/0 type does not exist "
						"as a builtin type; use boolean/0 instead.",
						Transforms, FileLoc );

				_ ->
					ast_utils:display_warning( "Not expecting type '~ts' "
						"(in ast_type:transform_type/2), assuming simple "
						"builtin type, in:~n  ~p", [ BuiltinType, TypeDef ] ),
					{ TypeDef, Transforms }

			end

	end;


% "If T is a record type #Name{F_1, ..., F_k}, where each F_i is a record field
% type, then Rep(T) = {type, FILE_LOC, record, [Rep(Name), Rep(F_1), ...,
% Rep(F_k)]}."
%
% Like '-type my_record() :: #my_record{}.', translating in {type, 89, record,
% [{atom, 89, my_record }]}:
%
transform_type( _TypeDef={ 'type', FileLoc, TargetType='record',
			_TypeVars=[ N={ atom, _FileLocT, _RecordName } | FieldTypes ] },
				Transforms ) ->

	% Is already a list directly (no key/value pairs to preserve here):
	{ NewFieldTypes, NewTransforms } = lists:mapfoldl(
		fun transform_field_type/2, _Acc0=Transforms,
		_List=FieldTypes ),

	NewTypeDef = { 'type', FileLoc, TargetType, [ N | NewFieldTypes ] },

	{ NewTypeDef, NewTransforms };


transform_type( _TypeDef={ 'type', FileLoc, TargetType='maybe_improper_list',
						   UnifiedTypes },
				Transforms ) ->

	% Is already a list directly (no key/value pairs to preserve here):
	{ NewUnifiedTypes, NewTransforms } = lists:mapfoldl(
		fun transform_type/2, _Acc0=Transforms, _List=UnifiedTypes ),

	NewTypeDef = { 'type', FileLoc, TargetType, NewUnifiedTypes },

	{ NewTypeDef, NewTransforms };


% Known other built-in types (catch-all for all remaining 'type'):
transform_type( TypeDef={ 'type', FileLoc, BuiltinType, TypeVars },
				Transforms ) when is_list( TypeVars ) ->

	ast_utils:display_warning( "Not expecting type '~ts', assuming unknown "
		"parametrised builtin type, in:~n  ~p", [ BuiltinType, TypeDef ] ),

	% Is already a list directly (no key/value pairs to preserve here):
	{ NewTypeVars, NewTransforms } = lists:mapfoldl(
			fun transform_type/2, _Acc0=Transforms, _List=TypeVars ),

	NewTypeDef = { 'type', FileLoc, BuiltinType, NewTypeVars },

	{ NewTypeDef, NewTransforms };



% Handling user type (necessarily a local one):


transform_type( _TypeDef={ 'user_type', FileLoc, TypeName, TypeVars },
			Transforms=#ast_transforms{ local_types=LocalTransformTable } ) ->

	% Is already a list directly (no key/value pairs to preserve here):
	{ NewTypeVars, NewTransforms } = lists:mapfoldl( fun transform_type/2,
						_Acc0=Transforms, _List=TypeVars ),

	TypeArity = length( TypeVars ),

	% Note: no user-to-local type rewriting deemed useful.

	{ Outcome, LocalTransforms } = case LocalTransformTable of

		undefined ->
			{ unchanged, NewTransforms };

		_ ->

			% Returning the new type information:
			case ?table:lookup_entry( { TypeName, TypeArity },
									  LocalTransformTable ) of

				% Module *and* type overridden:
				{ value, E={ _NewModuleName, _NewTypeName } } ->
					{ E, NewTransforms };

				% Same type, only module overridden:
				% (never happens, as module always specified in table)
				%{ value, NewModuleName } when is_atom( NewModuleName ) ->
				%	{ NewModuleName, TypeName };

				{ value, TransformFun } when is_function( TransformFun ) ->
					transform_local_type_with_fun( TransformFun, TypeName,
												   TypeArity, NewTransforms );

				key_not_found ->

					% Maybe a wildcard arity was defined then?
					case ?table:lookup_entry( { TypeName, _AnyArity='_' },
											  LocalTransformTable ) of

						{ value, E={ _NewModuleName, _NewTypeName } } ->
							{ E, NewTransforms };

						% Same type, only module overridden:
						% (was commented-out out, but may happen?)
						%
						{ value, NewModuleName }
						  when is_atom( NewModuleName ) ->
							{ { NewModuleName, TypeName }, NewTransforms };

						{ value, TransformFun }
						  when is_function( TransformFun ) ->
							transform_local_type_with_fun( TransformFun,
									TypeName, TypeArity, NewTransforms );

						key_not_found ->
							% Nope, let it as it is:
							{ unchanged, NewTransforms }

					end

			end

	end,

	NewTypeDef = case Outcome of

		unchanged ->
			% TypeDef with only updated TypeVars:
			{ 'user_type', FileLoc, TypeName, NewTypeVars };

		{ SetModuleName, SetTypeName } ->
			forge_remote_type( SetModuleName, SetTypeName, NewTypeVars,
							   FileLoc )

	end,

	{ NewTypeDef, LocalTransforms };



% Handling remote user type:


% "If T is a remote type M:N(T_1, ..., T_k), then Rep(T) =
% {remote_type, FILE_LOC, [Rep(M), Rep(N), [Rep(T_1), ..., Rep(T_k)]]}."
%
% First, the special (yet most common) case of immediate values specified for
% module and type:
%
transform_type( _TypeDef={ 'remote_type', FileLoc,
						   [ M={ atom, FileLocM, ModuleName },
							 T={ atom, FileLocT, TypeName }, TypeVars ] },
				Transforms=#ast_transforms{
								remote_types=RemoteTransformTable } ) ->

	% Is already a list directly (no key/value pairs to preserve here):
	{ NewTypeVars, NewTransforms } = lists:mapfoldl(
			fun transform_type/2, _Acc0=Transforms, _List=TypeVars ),

	TypeArity = length( TypeVars ),

	% Returning the new type information:
	{ Outcome, RemoteTransforms } = case RemoteTransformTable of

		undefined ->
			{ unchanged, NewTransforms };

		_ ->

			case ?table:lookup_entry( { ModuleName, TypeName, TypeArity },
									  RemoteTransformTable ) of

				 % Module *and* type overridden:
				{ value, E={ _NewModuleName, _NewTypeName } } ->
					{ E, NewTransforms };

				 % Same type; only the module is overridden:
				{ value, NewModuleName } when is_atom( NewModuleName ) ->
					{ { NewModuleName, TypeName }, NewTransforms };

				{ value, TransformFun } when is_function( TransformFun ) ->
					transform_remote_type_with_fun( TransformFun, ModuleName,
										TypeName, TypeArity, NewTransforms );

				key_not_found ->

					% Maybe a wildcard arity was defined for that type then?

					AnyArity = '_',

					case ?table:lookup_entry(
							{ ModuleName, TypeName, AnyArity },
							RemoteTransformTable ) of

						{ value, E={ _NewModuleName, _NewTypeName } } ->
							{ E, NewTransforms };

						 % Same type, only module overridden (never happens by
						 % design):
						 %{ value, NewModuleName }
						 %        when is_atom( NewModuleName ) ->
						 %    { NewModuleName, TypeName };

						{ value, TransformFun }
								when is_function( TransformFun ) ->
							transform_remote_type_with_fun( TransformFun,
								ModuleName, TypeName, TypeArity,
								NewTransforms );

						key_not_found ->

							% Nope; maybe a wildcard type (and arity) then?
							case ?table:lookup_entry(
									{ ModuleName, _AnyType='_', AnyArity },
									RemoteTransformTable ) of

								{ value, E={ _NewModuleName, _NewTypeName } } ->
									{ E, NewTransforms };

								% Same type, only module overridden:
								{ value, NewModuleName }
										when is_atom( NewModuleName ) ->
									{ { NewModuleName, TypeName },
									  NewTransforms };

								{ value, TransformFun }
										when is_function( TransformFun ) ->
									transform_remote_type_with_fun(
										TransformFun, ModuleName, TypeName,
										TypeArity, NewTransforms );

								key_not_found ->
									% Nope, let it as it is:
									{ unchanged, NewTransforms }

							end

					end

			end

	end,

	NewTypeDef = case Outcome of

		unchanged ->
			% TypeDef with updated TypeVars:
			{ 'remote_type', FileLoc, [ M, T, NewTypeVars ] };

		{ SetModuleName, SetTypeName } ->
			forge_remote_type( SetModuleName, SetTypeName, NewTypeVars, FileLoc,
							   FileLocM, FileLocT )

	end,

	{ NewTypeDef, RemoteTransforms };



% Second, the case where at least either the module or the type name is not
% immediate:
%
transform_type( _TypeDef={ 'remote_type', FileLoc1, [ Mod, Typ, TypeVars ] },
				Transforms ) ->

	% Wondering what these could be:
	%ast_utils:display_debug( "Transforming a remote type whose module and "
	%						  "type information are ~p and ~p.", [ Mod, Typ ] ),

	{ NewMod, ModTransforms } = transform_type( Mod, Transforms ),

	{ NewTyp, TypTransforms } = transform_type( Typ, ModTransforms ),

	% Is already a list directly (no key/value pairs to preserve here):
	{ NewTypeVars, NewTransforms } = lists:mapfoldl(
			fun transform_type/2, _Acc0=TypTransforms, _List=TypeVars ),

	NewTypeDef = { 'remote_type', FileLoc1, [ NewMod, NewTyp, NewTypeVars ] },

	{ NewTypeDef, NewTransforms };



% Variable declaration, possibly obtained through declarations like:
%
% -type my_type(T) :: other_type(T).
%
% or:
%
% -opaque tree(T) :: {T, [tree(T)]}.
%
transform_type( TypeDef={ 'var', _FileLoc, _TypeName }, Transforms ) ->

	%NewVar = transform_type_variable( TypeName, FileLoc, SomeTransform ),

	{ TypeDef, Transforms };



% Annotated type, most probably obtained from the field of a record like:
%    pointDrag :: {X::integer(), Y::integer()}}
%
% Resulting then in:
% {typed_record_field,
%		   {record_field,342,{atom,342,pointDrag}},
%		   {type,342,tuple,
%			   [{ann_type,342,[{var,342,'X'},{type,342,integer,[]}]},
%				{ann_type,342,
%					[{var,342,'Y'},{type,342,integer,[]}]} ] }}
%
transform_type( _TypeDef={ 'ann_type', FileLoc,
						   [ Var={ 'var', _FileLoc2, _VariableName },
							 InternalTypeDef ] }, Transforms ) ->

	%NewVar = transform_type_variable( VariableName, FileLoc2, _SomeTransform ),
	NewVar = Var,

	{ NewInternalTypeDef, NewTransforms } =
		transform_type( InternalTypeDef, Transforms ),

	NewTypeDef = { 'ann_type', FileLoc, [ NewVar, NewInternalTypeDef ] },

	{ NewTypeDef, NewTransforms };



% Binary operator.
%
% "If T is an operator type T_1 Op T_2, where Op is a binary operator (this is
% an occurrence of an expression that can be evaluated to an integer at compile
% time), then Rep(T) = {op, FILE_LOC, Op, Rep(T_1), Rep(T_2)}."
%
transform_type( _TypeDef={ 'op', FileLoc, Operator, LeftType, RightType },
				Transforms ) ->

	{ NewLeftType, LeftTransforms } = transform_type( LeftType, Transforms ),

	{ NewRightType, RightTransforms } =
		transform_type( RightType, LeftTransforms ),

	NewTypeDef = { 'op', FileLoc, Operator, NewLeftType, NewRightType },

	{ NewTypeDef, RightTransforms };



% Unary operator.
%
% "If T is an operator type Op T_0, where Op is a unary operator (this is an
% occurrence of an expression that can be evaluated to an integer at compile
% time), then Rep(T) = {op, FILE_LOC, Op, Rep(T_0)}."
%
transform_type( _TypeDef={ 'op', FileLoc, Operator, OperandType },
				Transforms ) ->

	{ NewOperandType, NewTransforms } =
		transform_type( OperandType, Transforms ),

	NewTypeDef = { 'op', FileLoc, Operator, NewOperandType },

	{ NewTypeDef, NewTransforms };



% Immediate values like {atom,42,foobar}, possibly obtained through
% declarations like:
%     -type my_type() :: integer() | 'foobar'.
%
% Note: this clause must remain at the end of the series, as a near-default one.
%
transform_type( TypeDef={ TypeName, _FileLoc, _Value }, Transforms ) ->

	% For some unknown reason, in erl_id_trans.erl only a subset of the
	% immediate types are managed (in type/1; ex: 'integer' but not 'float'):
	%
	%AllowedTypes = type_utils:get_immediate_types(),
	AllowedTypes = [ atom, integer ],

	case lists:member( TypeName, AllowedTypes ) of

		true ->
			%ast_value:transform_value( TypeDef, _SomeTransforms ),
			{ TypeDef, Transforms };

		false ->
			ast_utils:raise_error( [ unexpected_immediate_value, TypeDef ] )

	end;


transform_type( TypeDef, _Transforms ) ->
	ast_utils:raise_error( [ unhandled_typedef, TypeDef ] ).



% @doc Transforms specified local type with specified function.
transform_local_type_with_fun( TransformFun, TypeName, TypeArity,
		Transforms=#ast_transforms{ transformation_state=TransfoState } ) ->

	{ TypeReplacement, NewTransfoState } =
		TransformFun( TypeName, TypeArity, TransfoState ),

	NewTransforms =
		Transforms#ast_transforms{ transformation_state=NewTransfoState },

	{ TypeReplacement, NewTransforms }.



% @doc Transforms specified remote type with specified function.
transform_remote_type_with_fun( TransformFun, ModuleName, TypeName, TypeArity,
		Transforms=#ast_transforms{ transformation_state=TransfoState } ) ->

	{ TypeReplacement, NewTransfoState } =
		TransformFun( ModuleName, TypeName, TypeArity, TransfoState ),

	NewTransforms =
		Transforms#ast_transforms{ transformation_state=NewTransfoState },

	{ TypeReplacement, NewTransforms }.



% Transforming association types (from maps).


% @doc Transforms specified association type.
%
% "If A is an association type K => V, where K and V are types, then Rep(A) =
% {type, FILE_LOC, map_field_assoc, [Rep(K), Rep(V)]}."
%
-spec transform_association_type( ast_type(), ast_transforms() ) -> ast_type().
transform_association_type( { 'type', FileLoc, 'map_field_assoc',
							  Types=[ _K, _V ] }, Transforms ) ->

	% Is already a list directly (no key/value pairs to preserve here):
	{ NewTypes, NewTransforms } = lists:mapfoldl(
			fun transform_type/2, _Acc0=Transforms, _List=Types ),

	TypeDef = { 'type', FileLoc, 'map_field_assoc', NewTypes },

	{ TypeDef, NewTransforms };


% "If A is an association type K := V, where K and V are types, then Rep(A) =
% {type, FILE_LOC, map_field_exact, [Rep(K), Rep(V)]}.
%
transform_association_type( { 'type', FileLoc, 'map_field_exact',
							  Types=[ _K, _V ] }, Transforms ) ->

	% Is already a list directly (no key/value pairs to preserve here):
	{ NewTypes, NewTransforms } = lists:mapfoldl(
			fun transform_type/2, _Acc0=Transforms, _List=Types ),

	TypeDef = { 'type', FileLoc, 'map_field_exact', NewTypes },

	{ TypeDef, NewTransforms }.



% @doc Transforms specified field types (from records).
%
% "If F is a record field type Name :: Type, where Type is a type, then Rep(F) =
% {type, FILE_LOC, field_type, [Rep(Name), Rep(Type)]}."
%
transform_field_type( { 'type', FileLoc, 'field_type',
						[ N={ atom, _FileLocN, _FieldName }, FieldType ] },
					  Transforms ) ->

	{ NewFieldType, NewTransforms } = transform_type( FieldType, Transforms ),

	TypeDef = { 'type', FileLoc, 'field_type', [ N, NewFieldType ] },

	{ TypeDef, NewTransforms }.



% @doc Transforms specified AST variable.
-spec transform_type_variable( variable_name(), file_loc(),
					ast_transforms() ) -> { ast_element(), ast_transforms() }.
transform_type_variable( VariableName, _FileLoc, Transforms )
								when is_atom( VariableName ) ->
	{ VariableName, Transforms }.





% Section for type forging.


% @doc Returns an AST-compliant type description for a boolean, defined at the
% very start of the current source file.
%
% Ex: forge_boolean_type() returns: {type,{0,1},boolean,[]}.
%
-spec forge_boolean_type() -> ast_builtin_type().
forge_boolean_type() ->
	forge_boolean_type( _FileLoc=?default_generation_location ).


% @doc Returns an AST-compliant type description for a boolean, defined at the
% specified location of the current source file.
%
% Ex: forge_boolean_type(45) returns: {type,45,boolean,[]}.
%
-spec forge_boolean_type( file_loc() ) -> ast_builtin_type().
forge_boolean_type( FileLoc ) ->
	forge_builtin_type( _TypeName=boolean, _TypeVars=[], FileLoc ).



% @doc Returns an AST-compliant type description for an atom, defined at the
% very start of the current source file.
%
% Ex: forge_atom_type() returns: {type,{0,1},atom,[]}.
%
-spec forge_atom_type() -> ast_builtin_type().
forge_atom_type() ->
	forge_atom_type( _FileLoc=?default_generation_location ).



% @doc Returns an AST-compliant type description for an atom, defined at the
% specified location of the current source file.
%
% Ex: forge_atom_type(45) returns: {type,45,atom,[]}.
%
-spec forge_atom_type( file_loc() ) -> ast_builtin_type().
forge_atom_type( FileLoc ) ->
	forge_builtin_type( _TypeName=atom, _TypeVars=[], FileLoc ).



% @doc Returns an AST-compliant type description for a PID, defined at the
% very start of the current source file.
%
% Ex: forge_pid_type() returns: {type,{0,1},pid,[]}.
%
-spec forge_pid_type() -> ast_builtin_type().
forge_pid_type() ->
	forge_pid_type( _FileLoc=?default_generation_location ).



% @doc Returns an AST-compliant type description for a PID, defined at the
% specified location of the current source file.
%
% Ex: forge_pid_type(45) returns: {type,45,pid,[]}.
%
-spec forge_pid_type( file_loc() ) -> ast_builtin_type().
forge_pid_type( FileLoc ) ->
	forge_builtin_type( _TypeName=pid, _TypeVars=[], FileLoc ).



% @doc Returns an AST-compliant type description for an integer, defined at the
% very start of the current source file.
%
% Ex: forge_integer_type() returns: {type,{0,1},integer,[]}.
%
-spec forge_integer_type() -> ast_builtin_type().
forge_integer_type() ->
	forge_integer_type( _FileLoc=?default_generation_location ).



% @doc Returns an AST-compliant type description for an integer, defined at the
% specified location of the current source file.
%
% Ex: forge_integer_type(45) returns: {type,45,integer,[]}.
%
-spec forge_integer_type( file_loc() ) -> ast_builtin_type().
forge_integer_type( FileLoc ) ->
	forge_builtin_type( _TypeName=integer, _TypeVars=[], FileLoc ).



% @doc Returns an AST-compliant type description for a float, defined at the
% very start of the current source file.
%
% Ex: forge_float_type() returns: {type,{0,1},float,[]}.
%
-spec forge_float_type() -> ast_builtin_type().
forge_float_type() ->
	forge_float_type( _FileLoc=?default_generation_location ).



% @doc Returns an AST-compliant type description for a float, defined at the
% specified location of the current source file.
%
% Ex: forge_float_type(45) returns: {type,45,float,[]}.
%
-spec forge_float_type( file_loc() ) -> ast_builtin_type().
forge_float_type( FileLoc ) ->
	forge_builtin_type( _TypeName=float, _TypeVars=[], FileLoc ).



% @doc Returns an AST-compliant type description for a tuple, defined at the
% very start of the current source file.
%
-spec forge_tuple_type( [ ast_type() ] ) -> ast_builtin_type().
forge_tuple_type( ElementTypes ) ->
	forge_tuple_type( ElementTypes, _FileLoc=?default_generation_location ).


% @doc Returns an AST-compliant type description for a tuple, defined at the
% specified location of the current source file.
%
% Ex: to represent the following type defined at line 39: {integer(), float()},
% forge_tuple_type([forge_integer_type(39), forge_float_type(39)], 39)
% returns: {type, 39, tuple, [{type,39,integer,[]}, {type,39,float,[]}]}.
%
-spec forge_tuple_type( [ ast_type() ], file_loc() ) -> ast_builtin_type().
forge_tuple_type( ElementTypes, FileLoc ) ->
	forge_builtin_type( _TypeName=tuple, _TypeVars=ElementTypes, FileLoc ).



% @doc Returns an AST-compliant type description for a list, defined at the
% very start of the current source file.
%
-spec forge_list_type( ast_type() ) -> ast_builtin_type().
forge_list_type( ElementType ) ->
	forge_list_type( ElementType, _FileLoc=?default_generation_location ).



% @doc Returns an AST-compliant type description for a list, defined at the
% specified location of the current source file.
%
% Ex: to represent the following type defined at line 39: [integer()],
% forge_list_type(forge_integer_type(39), 39) returns:
% {type, 39, list, [{type,39,integer,[]}]}.
%
-spec forge_list_type( ast_type(), file_loc() ) -> ast_builtin_type().
forge_list_type( ElementType, FileLoc ) ->
	forge_builtin_type( _TypeName=list, _TypeVars=[ ElementType ], FileLoc ).



% @doc Returns an AST-compliant type description for an union, defined at the
% very start of the current source file.
%
-spec forge_union_type( [ ast_type() ] ) -> ast_builtin_type().
forge_union_type( UnitedTypes ) ->
	forge_union_type( UnitedTypes, _FileLoc=?default_generation_location ).


% @doc Returns an AST-compliant type description for an union, defined at the
% specified location of the current source file.
%
% Ex: to represent the following type defined at line 39: integer() | float(),
% forge_union_type([forge_integer_type(39), forge_float_type(39)], 39) returns:
% {type, 39, union, [{type,39,integer,[]}, {type,39,float,[]}]}.
%
-spec forge_union_type( [ ast_type() ], file_loc() ) -> ast_builtin_type().
forge_union_type( UnitedTypes, FileLoc ) ->
	forge_builtin_type( _TypeName=union, _TypeVars=UnitedTypes, FileLoc ).



% @doc Returns an AST-compliant type description for the specified built-in
% type.
%
% Ex: forge_builtin_type(atom, [], 45) returns: {type,45,atom,[]}.
%
-spec forge_builtin_type( type_name(), [ ast_type() ], file_loc() ) ->
									ast_builtin_type().
forge_builtin_type( TypeName, TypeVars, FileLoc ) ->
	#type{ file_location=FileLoc, name=TypeName, variables=TypeVars }.




% @doc Returns an AST-compliant representation of the specified local,
% user-defined type definition.
%
% Ex: to designate my_type() at line 40, forge_local_type(my_type, 40) returns:
% {user_type, 40, my_type, []}.
%
-spec forge_local_type( type_name(), [ ast_type() ], file_loc() ) ->
								ast_user_type().
forge_local_type( TypeName, TypeVars, FileLoc ) ->
	#user_type{ file_location=FileLoc, name=TypeName, variables=TypeVars }.



% @doc Returns an AST-compliant representation of the specified remote type.
%
% Ex: to designate basic_utils:some_type(float()) at line 43, use:
% forge_remote_type(basic_utils, some_type, [], 43), which returns:
% {remote_type, 43, [{atom,43,basic_utils}, {atom,43,some_type},
%   [{type,43,float,[]}]]}
%
-spec forge_remote_type( module_name(), type_name(), [ ast_type() ],
						 file_loc() ) -> ast_remote_type().
forge_remote_type( ModuleName, TypeName, TypeVars, FileLoc ) ->
	forge_remote_type( ModuleName, TypeName, TypeVars, FileLoc, FileLoc,
					   FileLoc ).


% @doc Returns an AST-compliant representation of the specified remote type.
%
% Ex: to designate basic_utils:some_type(float()) at lines 43, 44 and 45, use:
% forge_remote_type(basic_utils, some_type, [], {43, 44, 45}) - which
% returns: {remote_type, 43, [{atom,44,basic_utils}, {atom,45,some_type},
% [{type,43,float,[]}]]}.
%
-spec forge_remote_type( module_name(), type_name(), [ ast_type() ],
				file_loc(), file_loc(), file_loc() ) -> ast_remote_type().
forge_remote_type( ModuleName, TypeName, TypeVars, FileLoc1, FileLoc2,
				   FileLoc3 ) ->

	Spec = [ ast_value:forge_atom_value( ModuleName, FileLoc2 ),
			 ast_value:forge_atom_value( TypeName, FileLoc3 ), TypeVars ],

	#remote_type{ file_location=FileLoc1, spec=Spec }.


% @doc Returns an AST-compliant representation of the specified variable
% pattern.
%
-spec forge_type_variable( variable_name(), file_loc() ) ->
								ast_variable_pattern().
forge_type_variable( VariableName, FileLoc ) when is_atom( VariableName ) ->
	{ var, FileLoc, VariableName }.




% Checking section.


% @doc Checks that the specified type name is legit.
-spec check_type_name( term() ) -> type_name().
check_type_name( Name ) ->
	check_type_name( Name, _Context=undefined ).


% @doc Checks that the specified type name is legit.
-spec check_type_name( term(), form_context() ) -> type_name().
check_type_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_type_name( Other, Context ) ->
	ast_utils:raise_error( [ invalid_type_name, Other ], Context ).



% @doc Checks that the specified type definition is legit.
-spec check_type_definition( term() ) -> ast_type_definition().
check_type_definition( TypeDef ) ->
	check_type_definition( TypeDef, _Context=undefined ).


% @doc Checks that the specified type definition is legit.
-spec check_type_definition( term(), form_context() ) -> ast_type_definition().
check_type_definition( TypeDef, _Context ) when is_tuple( TypeDef ) ->
	TypeDef;

check_type_definition( Other, Context ) ->
	ast_utils:raise_error( [ invalid_type_definition, Other ], Context ).



% @doc Checks that the specified record name is legit.
-spec check_record_name( term() ) -> basic_utils:record_name().
check_record_name( Name ) ->
	check_record_name( Name, _Context=undefined ).


% @doc Checks that the specified record name is legit.
-spec check_record_name( term(), form_context() ) -> basic_utils:record_name().
check_record_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_record_name( Other, Context ) ->
	ast_utils:raise_error( [ invalid_record_name, Other ], Context ).



% @doc Checks that the specified type identifier is legit.
-spec check_type_id( term() ) -> type_id().
check_type_id( Id ) ->
	check_type_id( Id, _Context=undefined ).


% @doc Checks that the specified type identifier is legit.
-spec check_type_id( term(), form_context() ) -> type_id().
check_type_id( TypeId={ TypeName, TypeArity }, Context ) ->
	check_type_name( TypeName, Context ),
	ast_utils:check_arity( TypeArity, Context ),
	TypeId;

check_type_id( Other, Context ) ->
	ast_utils:raise_error( [ invalid_type_identifier, Other ], Context ).



% @doc Checks that the specified type identifiers are legit.
-spec check_type_ids( term() ) -> [ type_id() ].
check_type_ids( Ids ) ->
	check_type_ids( Ids, _Context=undefined ).


% @doc Checks that the specified type identifiers are legit.
-spec check_type_ids( term(), form_context() ) -> [ type_id() ].
check_type_ids( List, Context ) when is_list( List ) ->
	[ check_type_id( Id, Context ) || Id <- List ];

check_type_ids( Other, Context ) ->
	ast_utils:raise_error( [ invalid_type_identifier_list, Other ], Context ).



% @doc Checks that the specified variable is legit.
-spec check_type_variable( term() ) -> ast_variable_pattern().
check_type_variable( ASTVariable ) ->
	check_type_variable( ASTVariable, _Context=undefined ).


% @doc Checks that the specified variable is legit.
-spec check_type_variable( term(), form_context() ) -> ast_variable_pattern().
check_type_variable( ASTVariable={ 'var', FileLoc, VariableName }, Context )
  when is_atom( VariableName ) ->
	ast_utils:check_file_loc( FileLoc, Context ),
	ASTVariable;

check_type_variable( Other, Context ) ->
	ast_utils:raise_error( [ invalid_ast_variable, Other ], Context ).



% @doc Checks that the specified variables are legit.
-spec check_type_variables( term() ) -> [ ast_variable_pattern() ].
check_type_variables( ASTVariables ) ->
	check_type_variables( ASTVariables, _Context=undefined ).


% @doc Checks that the specified variables are legit.
-spec check_type_variables( term(), form_context() ) ->
									[ ast_variable_pattern() ].
check_type_variables( List, Context ) when is_list( List ) ->
	[ check_type_variable( ASTVariable, Context ) || ASTVariable <- List ];

check_type_variables( Other, Context ) ->
	ast_utils:raise_error( [ invalid_ast_variable_list, Other ], Context ).



% @doc Checks that the specified term is the AST version of an atom.
-spec check_ast_atom( term() ) -> ast_base:ast_atom().
check_ast_atom( ASTAtom ) ->
	check_ast_atom( ASTAtom, _Context=undefined ).


% @doc Checks that the specified term is the AST version of an atom.
-spec check_ast_atom( term(), form_context() ) -> ast_base:ast_atom().
check_ast_atom( ASTAtom={ atom, _FileLoc, Atom }, _Context )
  when is_atom( Atom ) ->
	ASTAtom;

check_ast_atom( Other, Context ) ->

	%trace_utils:debug_fmt( "AST non-atom: ~p (context: ~p)",
	%                       [ Other, Context ] ),

	ast_utils:raise_error( [ invalid_ast_atom, Other ], Context ).




% @doc Returns a pair made of (two) lists of located forms regarding the type
% exports.
%
% They corresponding to:
%
% - all the type export declarations that are described in the specified type
% export table
%
% - all the types definitions that are described in the specified type table
%
-spec get_located_forms_for( ast_info:type_export_table(), type_table() ) ->
									{ [ located_form() ], [ located_form() ] }.
get_located_forms_for( TypeExportTable, TypeTable ) ->

	TypeExportInfos = ?table:enumerate( TypeExportTable ),

	%ast_utils:display_debug( "TypeExportInfos = ~p", [ TypeExportInfos ] ),

	TypeExportLocDefs =
		[ { ExpASTLoc, { attribute, FileLoc, export_type, TypeIds } }
				|| { ExpASTLoc, { FileLoc, TypeIds } } <- TypeExportInfos ],

	% Dropping the keys (the type_id(), i.e. type identifiers), focusing on
	% their associated type_info()
	%
	TypeInfos = ?table:values( TypeTable ),

	TypeLocDefs = lists:foldl(

		fun( #type_info{ name=TypeName,
						 variables=TypeVariables,
						 opaque=IsOpaque,
						 ast_location=ASTLoc,
						 file_location=FileLoc,
						 definition=TypeDef
						 %exported
						}, Acc ) ->

			TypeDesignator = case IsOpaque of

				true ->
					opaque;

				false ->
					type

			end,

			Form = { attribute, FileLoc, TypeDesignator,
					 { TypeName, TypeDef, TypeVariables } },

			LocTypeForm = { ASTLoc, Form },

			[ LocTypeForm | Acc ]

		end,
		_Acc0=[],
		_List=TypeInfos ),

	{ TypeExportLocDefs, TypeLocDefs }.
