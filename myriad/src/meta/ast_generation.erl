% Copyright (C) 2018-2024 Olivier Boudeville
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


% @doc Module in charge of <b>generating parts of an AST</b> (for example
% elements of forms).
%
-module(ast_generation).


-export([ list_to_form/1, form_to_list/1,
		  atoms_to_form/1, form_to_atoms/1,
		  enumerated_variables_to_form/1,
		  get_iterated_param_name/1, get_header_params/1 ]).


% Shorthands:

-type count() :: basic_utils:count().
-type form_element() :: ast_base:form_element().


% For the default_generation_location define:
-include("ast_utils.hrl").



% @doc Transforms the specified list (whose elements are typically themselves
% form elements already) into the AST version of a list.
%
% For example: list_to_form( [{atom,FileLoc,a}, {atom,FileLoc,b}]) = {cons,
% FileLoc, {atom,FileLoc,a}, {cons, FileLoc, {atom,FileLoc,b}, {nil,FileLoc}}}.
%
% See form_to_list/1 for the reciprocal function.
%
-spec list_to_form( list() ) -> form_element().
list_to_form( _List=[] ) ->
	{ nil, ?default_generation_location };

list_to_form( _List=[ E | T ] ) ->
	{ cons, ?default_generation_location, E, list_to_form( T ) }.



% @doc Transforms the specified AST list into the corresponding plain list.
%
% For example: form_to_list( {cons, FileLoc, {atom,FileLoc,a}, {cons, FileLoc,
% {atom,FileLoc,b}, {nil,FileLoc} }}) = [{atom,FileLoc,a}, {atom,FileLoc,b}].
%
% See list_to_form/1 for the reciprocal function.
%
-spec form_to_list( form_element() ) -> list().
form_to_list( { nil, _FileLoc } ) ->
	[];

form_to_list( { cons, _FileLoc, E, NestedForm } ) ->
	[ E | form_to_list( NestedForm ) ].



% @doc Returns the form element corresponding to the specified list of atoms.
%
% For example: {cons, FileLoc, {atom,FileLoc,a}, {cons, FileLoc,
% {atom,FileLoc,b}, {nil, FileLoc} }} = atoms_to_form(['a', 'b']).
%
-spec atoms_to_form( [ atom() ] ) -> form_element().
atoms_to_form( _AtomList=[] ) ->
	{ nil, ?default_generation_location };

atoms_to_form( _AtomList=[ Atom | H ] ) ->
	FileLoc = ?default_generation_location,
	{ cons, FileLoc, { atom, FileLoc, Atom }, atoms_to_form( H ) }.



% @doc Returns the list of atoms corresponding to the specified form element.
%
% For example ['a', 'b'] = atoms_to_form( {cons, FileLoc, {atom,FileLoc,a},
% {cons, FileLoc, {atom,FileLoc,b}, {nil,FileLoc}}}).
%
-spec form_to_atoms( form_element() ) -> [ atom() ].
form_to_atoms( { nil, _FileLoc } ) ->
	[];

form_to_atoms( { cons, _FileLoc, {atom,_,Atom}, NestedForm } ) ->
	[ Atom | form_to_atoms( NestedForm ) ].



% @doc Returns the form element corresponding to the list of variables
% corresponding to the specified number of such variables.
%
% For example: {cons, FileLoc, {var,FileLoc,'Myriad_Param_1'}, {cons, FileLoc,
% {var,FileLoc,'Myriad_Param_2'}, {nil,FileLoc}}} =
% enumerated_variables_to_form(2).
%
% See also: get_header_params/1.
%
-spec enumerated_variables_to_form( count() ) -> form_element().
enumerated_variables_to_form( Count ) ->
	enumerated_variables_to_form( Count, _Index=1 ).


enumerated_variables_to_form( _Count=0, _Index ) ->
	{ nil, ?default_generation_location };

enumerated_variables_to_form( Count, Index ) ->
	FileLoc = ?default_generation_location,
	{ cons, FileLoc, { var, FileLoc, get_iterated_param_name( Index ) },
	  enumerated_variables_to_form( Count-1, Index+1 ) }.



% @doc Returns, in AST form, a reference to the specified iterated variable.
%
% For example: 'Myriad_Param_4' = get_iterated_param_name(4).
%
-spec get_iterated_param_name( count() ) -> atom().
get_iterated_param_name( Count ) ->
	String = text_utils:format( "Myriad_Param_~B", [ Count ] ),
	text_utils:string_to_atom( String ).




% @doc Returns, as form elements, conventional call parameter names, as form
% elements, corresponding to a function of the specified arity.
%
% This is typically useful when generating a function form, to define its
% header, like in 'f(A,B)->...'.
%
% For example: [{var,FileLoc,'Myriad_Param_1'}, {var,FileLoc,'Myriad_Param_2'}]
% = get_header_params(2).
%
% See also: enumerated_variables_to_form/1.
%
-spec get_header_params( arity() ) -> [ form_element() ].
get_header_params( Arity ) ->
	get_header_params( Arity, _Acc=[] ).


get_header_params( _Arity=0, Acc ) ->
	Acc;

get_header_params( Arity, Acc ) ->

	NewAcc = [ { var, ?default_generation_location,
				 get_iterated_param_name( Arity ) } | Acc ],

	get_header_params( Arity-1, NewAcc ).
