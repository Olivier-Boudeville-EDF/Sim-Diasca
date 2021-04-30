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



% Module in charge of handling maps defined or used within an AST.
%
% See http://erlang.org/doc/apps/erts/absform.html for more information.
%
-module(ast_map).


-type ast_map( _KeyType, _ValueType ) :: ast_element().


-type map_field_association_type() :: 'map_field_assoc' | 'map_field_exact'.


-type ast_map_association() :: ast_map_association( ast_element(),
													ast_element() ).


-type ast_map_association( KeyType, ValueType ) ::
		{ map_field_association_type(), line(), KeyType, ValueType }.



% AST form corresponding to a map creation:
-type ast_map_creation_form() :: ast_map_creation_form( ast_element(),
														ast_element() ).


-type ast_map_creation_form( KeyType, ValueType ) ::
		{ 'map', line(), [ ast_map_association( KeyType, ValueType ) ] }.



% AST form corresponding to a map update:
%
-type ast_map_update_form() :: ast_map_update_form( ast_element(),
													ast_element() ).


-type ast_map_update_form( KeyType, ValueType ) ::
		{ 'map', line(), ast_map( KeyType, ValueType ),
		  [ ast_map_association( KeyType, ValueType ) ] }.


% Possibly not relevant:
-type ast_map_form() :: ast_map_creation_form() | ast_map_update_form().


-export_type([ ast_map/2, map_field_association_type/0,
			   ast_map_association/0, ast_map_association/2,
			   ast_map_creation_form/0, ast_map_creation_form/2,
			   ast_map_update_form/0, ast_map_update_form/2,
			   ast_map_form/0 ]).



-export([ transform_map_associations/2, transform_map_associations/3,
		  transform_map_association/3 ]).


% Shorthands:

-type line() :: ast_base:line().
-type ast_element() :: ast_base:ast_element().
-type ast_transforms() :: ast_transform:ast_transforms().


% For the ast_transform record:
-include("ast_transform.hrl").

% For the rec_guard define:
-include("ast_utils.hrl").



% Transforms specified list of map associations involved in a map operation.
%
% Note: context-insensitive function, considering that any kind of expression
% can be found for the association keys and values.
%
-spec transform_map_associations( [ ast_map_association() ],
		ast_transforms() ) -> { [ ast_map_association() ], ast_transforms() }.
transform_map_associations( Associations, Transforms ) ?rec_guard ->
	transform_map_associations( Associations, Transforms,
								fun ast_expression:transform_expression/2 ).


% Transforms specified list of map associations involved in a map operation,
% applying to each association the specified function to perform the relevant
% transformations (that depends on the context; ex: if being in a guard, in an
% expression, etc.).
%
-spec transform_map_associations( [ ast_map_association() ], ast_transforms(),
		ast_transform:transform_fun() ) ->
						   { [ ast_map_association() ], ast_transforms() }.
transform_map_associations( Associations, Transforms,
							TransformFun ) ?rec_guard ->

	% Closure, to capture TransformFun:
	ActualFun = fun( A, AccTransforms ) ->
					transform_map_association( A, AccTransforms, TransformFun )
				end,

	lists:mapfoldl( ActualFun, _Acc0=Transforms, _List=Associations ).



% Transforms specified map association involved in a map operation.
%
% "An association A is one of the following:
%    If A is an association K => V, then
%       Rep(A) = {map_field_assoc,LINE,Rep(K),Rep(V)}.
%    If A is an association K := V, then
%       Rep(A) = {map_field_exact,LINE,Rep(K),Rep(V)}."
%
-spec transform_map_association( ast_map_association(), ast_transforms(),
		 ast_transform:transform_fun() ) ->
				{ ast_map_association(), ast_transforms() }.
transform_map_association( { MapAssocType, Line, ASTKey, ASTValue }, Transforms,
						   TransformFun )
  when ( MapAssocType =:= 'map_field_assoc'
		 orelse MapAssocType =:= 'map_field_exact' ) ?andalso_rec_guard ->

	{ NewASTKey, KeyTransforms } = TransformFun( ASTKey, Transforms ),

	{ NewASTValue, ValueTransforms } = TransformFun( ASTValue, KeyTransforms ),

	Assoc = { MapAssocType, Line, NewASTKey, NewASTValue },

	{ Assoc, ValueTransforms }.
