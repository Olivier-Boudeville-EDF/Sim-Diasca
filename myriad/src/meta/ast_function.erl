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
% Creation date: Wednesday, February 7, 2018.



% Module in charge of providing constructs to manage functions in an AST.
%
% Note: function clauses are managed in the ast_clause module.
%
-module(ast_function).



% Checking:
-export([ check_function_name/1, check_function_name/2,

		  check_function_id/1, check_function_id/2,
		  check_function_ids/1, check_function_ids/2,

		  check_function_type/2, check_function_type/3,
		  check_function_types/2, check_function_types/3 ]).


% Transformation:
-export([ transform_functions/2, transform_function/2,
		  transform_function_spec/2,
		  transform_spec/2, transform_function_type/2,
		  transform_function_constraints/2, transform_function_constraint/2 ]).


% Recomposition:
-export([ get_located_forms_for/2 ]).


% Helpers:
-export([ clauses_to_string/2 ]).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type function_name() :: basic_utils:function_name().

-type function_arity() :: meta_utils:function_arity().
-type function_spec() :: meta_utils:function_spec().
-type function_id() :: meta_utils:function_id().
-type function_type() :: meta_utils:function_type().


%-type line() :: ast_base:line().
-type form_context() :: ast_base:form_context().

-type located_form() :: ast_info:located_form().
-type function_info() :: ast_info:function_info().
-type function_table() :: ast_info:function_table().
-type function_pair() :: { function_id(), function_info() }.

-type function_export_table() :: ast_info:function_export_table().

-type ast_transforms() :: ast_transform:ast_transforms().


% For the table macro:
-include("meta_utils.hrl").


% For the function_info record:
-include("ast_info.hrl").

% For the ast_transform record:
-include("ast_transform.hrl").

% For the rec_guard-related defines:
-include("ast_utils.hrl").



% Checks that specified function name is legit.
-spec check_function_name( term() ) -> function_name().
check_function_name( Name ) ->
	check_function_name( Name, _Context=undefined ).



% Checks that specified function name is legit.
-spec check_function_name( term(), form_context() ) -> function_name().
check_function_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_function_name( Other, Context ) ->
	ast_utils:raise_error( [ invalid_function_name, Other ], Context ).






% Checks that specified function identifier is legit.
-spec check_function_id( term() ) -> function_id().
check_function_id( Id ) ->
	check_function_id( Id, _Context=undefined ).


% Checks that specified function identifier is legit.
-spec check_function_id( term(), form_context() ) -> function_id().
check_function_id( FunctionId={ FunctionName, FunctionArity }, Context ) ->
	check_function_name( FunctionName, Context ),
	ast_utils:check_arity( FunctionArity, Context ),
	FunctionId;

check_function_id( Other, Context ) ->
	ast_utils:raise_error( [ invalid_function_identifier, Other ], Context ).



% Checks that specified function identifiers are legit.
-spec check_function_ids( term() ) -> [ function_id() ].
check_function_ids( Ids ) ->
	check_function_ids( Ids, _Context=undefined ).


% Checks that specified function identifiers are legit.
-spec check_function_ids( term(), form_context() ) -> [ function_id() ].
check_function_ids( List, Context ) when is_list( List ) ->
	[ check_function_id( Id, Context ) || Id <- List ];

check_function_ids( Other, Context ) ->
	ast_utils:raise_error( [ invalid_function_identifier_list, Other ],
						   Context ).



% Checks that specified function type is legit.
-spec check_function_type( term(), function_arity() ) -> function_type().
check_function_type( Type, FunctionArity ) ->
	check_function_type( Type, FunctionArity, _Context=undefined ).


% Checks that specified function type is legit.
-spec check_function_type( term(), function_arity(), form_context() ) ->
									function_type().
check_function_type( _FunctionType, _FunctionArity, _Context ) ->
	%display_warning( "Function type ~p not checked (context: ~p).",
	%				 [ FunctionType, Context ] ).
	%raise_error( [ fixme_function_type ], Context ).
	ok.

%check_function_type( Other, _FunctionArity, Context ) ->
%	raise_error( [ invalid_function_type, Other ], Context ).



% Checks that specified function types are legit.
-spec check_function_types( term(), function_arity() ) -> [ function_type() ].
check_function_types( Types, FunctionArity ) ->
	check_function_types( Types, FunctionArity, _Context=undefined ).


% Checks that specified function types are legit.
-spec check_function_types( term(), function_arity(), form_context() ) ->
								[ function_type() ].
check_function_types( List, FunctionArity, Context ) when is_list( List ) ->
	[ check_function_type( Type, FunctionArity, Context ) || Type <- List ];

check_function_types( Other, _FunctionArity, Context ) ->
	ast_utils:raise_error( [ invalid_function_type_list, Other ], Context ).


% Transforms the functions in specified table, based on specified transforms.
-spec transform_functions( function_table(), ast_transforms() ) ->
									{ function_table(), ast_transforms() }.
transform_functions( FunctionTable, Transforms ) ?rec_guard ->

	%?display_trace( "Transforming all functions" ),

	FunIdInfoPairs = ?table:enumerate( FunctionTable ),

	{ NewFunIdInfoPairs, NewTransforms } = lists:mapfoldl(
			fun transform_function_pair/2, _Acc0=Transforms,
			_List=FunIdInfoPairs ),

	NewFunctionTable = ?table:new( NewFunIdInfoPairs ),

	{ NewFunctionTable, NewTransforms }.



% Transforms specified function pair: {FunId, FunInfo}.
%
% Allows to keep around the function identifier, to recreate the function table
% more easily.
%
-spec transform_function_pair( function_pair(), ast_transforms() ) ->
										{ function_pair(), ast_transforms() }.
transform_function_pair( { FunId, FunctionInfo }, Transforms ) ?rec_guard ->

	?display_trace( "Transforming function ~p.", [ FunId ] ),

	{ NewFunctionInfo, NewTransforms } =
		transform_function( FunctionInfo, Transforms ),

	{ { FunId, NewFunctionInfo }, NewTransforms }.



% Transforms specified function.
-spec transform_function( function_info(), ast_transforms() ) ->
								{ function_info(), ast_transforms() }.
transform_function( FunctionInfo=#function_info{ clauses=ClauseDefs,
												 spec=MaybeLocFunSpec },
					Transforms ) ?rec_guard ->

	% We have to transform the clauses (first) and the spec (second):

	?display_trace( "Transforming clauses." ),

	{ NewClauseDefs, ClauseTransforms } = lists:mapfoldl(
			fun ast_clause:transform_function_clause/2, _Acc0=Transforms,
			_List=ClauseDefs ),


	{ NewLocFunSpec, FunSpecTransforms } = case MaybeLocFunSpec of

		undefined ->
			{ undefined, ClauseTransforms } ;

		{ Loc, FunSpec } ->
			?display_trace( "Transforming function spec." ),
			{ NewFunSpec, NewTransforms } =
						   transform_function_spec( FunSpec, Transforms ),
			{ { Loc, NewFunSpec }, NewTransforms }

	end,

	FinalFunctionInfo = FunctionInfo#function_info{ clauses=NewClauseDefs,
													spec=NewLocFunSpec },

	{ FinalFunctionInfo, FunSpecTransforms }.



% Transforms the specified function specification.
%
% "If F is a function specification -Spec Name Ft_1; ...; Ft_k, where Spec is
% either the atom spec or the atom callback, and each Ft_i is a possibly
% constrained function type with an argument sequence of the same length Arity,
% then Rep(F) = {attribute,Line,Spec,{{Name,Arity},[Rep(Ft_1), ...,
% Rep(Ft_k)]}}.
%
-spec transform_function_spec( function_spec(), ast_transforms() ) ->
										{ function_spec(), ast_transforms() }.
transform_function_spec( { 'attribute', Line, SpecType, { FunId, SpecList } },
						 Transforms ) ?rec_guard ->

	% Ex for '-spec f( type_a() ) -> type_b().':

	% SpecList = [ {type,652,'fun',
	% [{type,652,product,[{user_type,652,type_a,[]}]},
	% {user_type,652,type_b,[]}] } ]
	%

	%?display_trace( "SpecList = ~p", [ SpecList ] ),

	{ NewSpecList, NewTransforms } =
			lists:mapfoldl( fun transform_spec/2, _Acc0=Transforms,
			_List=SpecList ),

	NewFunSpec = { 'attribute', Line, SpecType, { FunId, NewSpecList } },

	{ NewFunSpec, NewTransforms }.




% Transforms the specified function specification.
%
% (corresponds to function_type_list/1 in erl_id_trans)
%
% "If Ft is a constrained function type Ft_1 when Fc, where Ft_1 is a function
% type and Fc is a function constraint, then Rep(T) =
% {type,LINE,bounded_fun,[Rep(Ft_1),Rep(Fc)]}."
%
transform_spec( { 'type', Line, 'bounded_fun',
			[ FunctionType, FunctionConstraint ] }, Transforms ) ?rec_guard ->

	{ NewFunctionType, TypeTransforms } =
		transform_function_type( FunctionType, Transforms ),

	{ NewFunctionConstraint, ConstTransforms } =
		transform_function_constraints( FunctionConstraint, TypeTransforms ),

	NewSpec = { 'type', Line, 'bounded_fun',
				[ NewFunctionType, NewFunctionConstraint ] },

	{ NewSpec, ConstTransforms };


transform_spec( OtherSpec, Transforms ) ?rec_guard ->
	transform_function_type( OtherSpec, Transforms ).



% (helper, corresponding to function_type/1 in erl_id_trans)
%
% "If Ft is a function type (T_1, ..., T_n) -> T_0, where each T_i is a type,
% then Rep(Ft) = {type,LINE,'fun',[{type,LINE,product,[Rep(T_1), ...,
% Rep(T_n)]},Rep(T_0)]}."
%
transform_function_type( { 'type', LineFirst, 'fun',
	   [ { 'type', LineSecond, 'product', ParamTypes }, ResultType ] },
						 Transforms ) ?rec_guard ->

	{ [ NewResultType | NewParamTypes ], NewTransforms } =
		ast_type:transform_types( [ ResultType | ParamTypes ], Transforms ),

	NewTypeSpec = { 'type', LineFirst, 'fun',
	  [ { 'type', LineSecond, 'product', NewParamTypes }, NewResultType ] },

	{ NewTypeSpec, NewTransforms };

transform_function_type( UnexpectedFunType, _Transforms ) ->
	ast_utils:raise_error( [ unexpected_function_type, UnexpectedFunType ] ).




% (helper, corresponding to function_constraint/1 in erl_id_trans)
%
% "A function constraint Fc is a non-empty sequence of constraints C_1, ...,
% C_k, and Rep(Fc) = [Rep(C_1), ..., Rep(C_k)]."
%
transform_function_constraints( FunctionConstraints, Transforms ) ?rec_guard ->
	lists:mapfoldl( fun transform_function_constraint/2, _Acc0=Transforms,
					_List=FunctionConstraints ).



% "If C is a constraint V :: T, where V is a type variable and T is a type, then
% Rep(C) = {type,LINE,constraint,[{atom,LINE,is_subtype},[Rep(V),Rep(T)]]}."
%
transform_function_constraint( { 'type', Line, 'constraint',
		[ AtomConstraint={ atom, _LineAtom, _SomeAtom }, [ TypeVar, Type ] ] },
		Transforms ) ?rec_guard ->

	{ NewTypeVar, VarTransforms } =
		ast_type:transform_type( TypeVar, Transforms ),

	{ NewType, NewTransforms } = ast_type:transform_type( Type, VarTransforms ),

	NewTypeSpec = { 'type', Line, 'constraint',
					[ AtomConstraint, [ NewTypeVar, NewType ] ] },

	{ NewTypeSpec, NewTransforms }.



% Returns a pair made of (two) lists of located forms corresponding to:
%
% - all the function export declarations (possibly automatically enriched) that
% are described in the specified function export table
%
% - all the function definitions and specs that are described in the specified
% function table
%
-spec get_located_forms_for( function_export_table(), function_table() ) ->
								   { [ located_form() ], [ located_form() ] }.
get_located_forms_for( FunctionExportTable, FunctionTable ) ->

	% Dropping the keys (the function_id(), i.e. function identifiers), focusing
	% on their associated function_info():
	%
	FunInfos = ?table:values( FunctionTable ),

	{ FunctionLocDefs, NewFunctionExportTable } = lists:foldl(

						% We used to filter out/report as errors these entries,
						% as they are exported but never defined; but the
						% compiler will take care of that, with better, more
						% standard messages:
						%
						fun( #function_info{ line=undefined,
											 clauses=[],
											 spec=MaybeSpec },
							 { AccLocDefs, AccExportTable } ) ->

								NewAccLocDefs = case MaybeSpec of

									% Happens if this function is only exported,
									% with neither a spec nor a definition (will
									% be caught by the compiler and reported
									% with a relevant diagnosis:
									%
									undefined ->
										AccLocDefs;

									% We already know that compilation will
									% fail, but we prefer letting the compiler
									% report that by itself later, with its own
									% error messages, so we still include that
									% lone spec form:
									%
									LocSpecForm ->
										[ LocSpecForm | AccLocDefs ]

								end,
								{ NewAccLocDefs, AccExportTable };


						   % Only potentially correct configuration:
						   ( #function_info{ name=Name,
											 arity=Arity,
											 location=Location,
											 line=Line,
											 clauses=Clauses,
											 spec=MaybeSpec,
											 exported=ExportLocs },
							 { AccLocDefs, AccExportTable } ) ->


								case Location of

									undefined ->
										throw( { location_not_defined_for,
												 { Name, Arity } } );

									_ ->
										ok

								end,

								case Line of

									undefined ->
										throw( { line_not_defined_for,
												 { Name, Arity } } );

									L when is_integer( L ) ->
										ok

								end,

								LocFunForm = { Location,
								  { function, Line, Name, Arity, Clauses } },

								NewAccLocDefs = case MaybeSpec of

									undefined ->
										[ LocFunForm | AccLocDefs ];

									LocSpecForm ->
										[ LocSpecForm, LocFunForm | AccLocDefs ]

								end,

								% Should a function declare that it is exported
								% as a given location that happens to correspond
								% to a registered export declaration (ex: the
								% default one), we ensure that this function is
								% indeed exported there (auto-export):
								%
								NewAccExportTable = update_export_table( Name,
									Arity, ExportLocs, AccExportTable ),

								{ NewAccLocDefs, NewAccExportTable }


						end,
						_Acc0={ [], FunctionExportTable },
						_List=FunInfos ),

	FunExportLocDefs = get_function_export_forms( NewFunctionExportTable ),

	{ FunExportLocDefs, FunctionLocDefs }.



% Ensures that the specified function is as expected exported in the specified
% (supposedly export) locations.
%
-spec update_export_table( function_name(), arity(), [ ast_info:location() ],
						   function_export_table() ) -> function_export_table().
update_export_table( _FunctionName, _Arity, _ExportLocs=[], ExportTable ) ->
	ExportTable;

update_export_table( FunctionName, Arity, _ExportLocs=[ Loc | H ],
					 ExportTable ) ->

	FunId = { FunctionName, Arity },

	case ?table:lookup_entry( Loc, ExportTable ) of

		key_not_found ->
			% If there is not even an export declaration at this location, it is
			% abnormal:
			%
			throw( { no_export_declaration_at, Loc, FunId } );

		{ value, { Line, FunIds } } ->

			NewFunIds = case lists:member( FunId, FunIds ) of

				true ->
					FunIds;

				false ->
					[ FunId | FunIds ]

			end,

			NewExportTable = ?table:add_entry( Loc, { Line, NewFunIds },
											  ExportTable ),

			update_export_table( FunctionName, Arity, H, NewExportTable )

	end.



% Returns located forms corresponding to known function exports, generated from
% specified table.
%
-spec get_function_export_forms( function_export_table() ) ->
									   [ located_form() ].
get_function_export_forms( FunctionExportTable ) ->

	FunExportInfos = ?table:enumerate( FunctionExportTable ),

	%ast_utils:display_debug( "FunExportInfos = ~p",
	%  [ FunExportInfos ] ),

	[ { Loc, { attribute, Line, export, FunIds } }
	  || { Loc, { Line, FunIds } } <- FunExportInfos ].



% Returns a textual description of the specified function clauses, using
% specified indentation level.
%
-spec clauses_to_string( meta_utils:clause_def(),
						 text_utils:indentation_level() ) -> ustring().
clauses_to_string( _Clauses=[], _IndentationLevel ) ->
	"no function clause defined";

clauses_to_string( Clauses, IndentationLevel ) ->
	text_utils:format( "~B function clauses defined: ~ts", [ length( Clauses ),
		text_utils:strings_to_string(
				 [ text_utils:format( "~p", [ C ] ) || C <- Clauses ],
				 IndentationLevel ) ] ).
