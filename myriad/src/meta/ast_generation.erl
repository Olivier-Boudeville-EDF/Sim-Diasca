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



% Module in charge of generating parts of an AST (ex: elements of forms).
-module(ast_generation).


-export([ list_to_form/1, form_to_list/1,
		  atoms_to_form/1, form_to_atoms/1,
		  enumerated_variables_to_form/1,
		  get_iterated_param_name/1, get_header_params/1 ]).


% Shorthands:

-type count() :: basic_utils:count().
-type form_element() :: ast_base:form_element().



% Transforms specified list (whose elements are typically themselves form
% elements already) into the AST version of a list.
%
% Ex: list_to_form( [{atom,Line,a}, {atom,Line,b}]) =
% {cons,Line,{atom,Line,a}, {cons,Line,{atom,Line,b}, {nil,Line} }}.
%
% See form_to_list/1 for the reciprocal function.
%
-spec list_to_form( list() ) -> form_element().
list_to_form( _List=[] ) ->
	{ nil, _Line=0 };

list_to_form( _List=[ E | T ] ) ->
	Line = 0,
	{ cons, Line, E, list_to_form( T ) }.



% Transforms specified AST list into the corresponding plain list.
%
% Ex: form_to_list( {cons,Line,{atom,Line,a}, {cons,Line,{atom,Line,b},
% {nil,Line} } }) = [{atom,Line,a}, {atom,Line,b}].
%
% See list_to_form/1 for the reciprocal function.
%
-spec form_to_list( form_element() ) -> list().
form_to_list( { nil, _Line } ) ->
	[];

form_to_list( { cons, _Line, E, NestedForm } ) ->
	[ E | form_to_list( NestedForm ) ].



% Returns the form element corresponding to the specified list of atoms.
%
% Ex: {cons,Line,{atom,Line,a}, {cons,Line,{atom,Line,b}, {nil,Line} }} =
%         atoms_to_form(['a', 'b']).
%
-spec atoms_to_form( [ atom() ] ) -> form_element().
atoms_to_form( _AtomList=[] ) ->
	{ nil, _Line=0 };

atoms_to_form( _AtomList=[ Atom | H ] ) ->
	Line = 0,
	{ cons, Line, { atom, Line, Atom }, atoms_to_form( H ) }.



% Returns the list of atoms corresponding to the specified form element.
%
% Ex: ['a', 'b'] = atoms_to_form( {cons,Line,{atom,Line,a},
% {cons,Line,{atom,Line,b}, {nil,Line} } }).
%
-spec form_to_atoms( form_element() ) -> [ atom() ].
form_to_atoms( { nil, _Line } ) ->
	[];

form_to_atoms( { cons, _Line, {atom,_,Atom}, NestedForm } ) ->
	[ Atom | form_to_atoms( NestedForm ) ].



% Returns the form element corresponding a list of variables.
%
% Ex: {cons, Line, {var,Line,'A'}, { cons,Line,{var,Line,'B'}, {nil,Line}}} =
%         enumerated_variables_to_form(2).
%
% See also: get_header_params/1.
%
-spec enumerated_variables_to_form( count() ) -> form_element().
enumerated_variables_to_form( Count ) ->
	enumerated_variables_to_form( Count, _Index=1 ).


enumerated_variables_to_form( _Count=0, _Index ) ->
	{ nil, _Line=0 };

enumerated_variables_to_form( Count, Index ) ->
	Line = 0,
	{ cons, Line, { var, Line, get_iterated_param_name( Index ) },
	  enumerated_variables_to_form( Count-1, Index+1 ) }.


% Returns, in AST form, a reference to an iterated variable.
%
% Ex: 'Myriad_Param_4' = get_iterated_param_name( 4 ).
%
-spec get_iterated_param_name( count() ) -> atom().
get_iterated_param_name( Count ) ->
	String = text_utils:format( "Myriad_Param_~B", [ Count ] ),
	text_utils:string_to_atom( String ).




% Returns, as form elements, conventional call parameter names, as form
% elements, corresponding to a function of specified arity.
%
% This is typically useful when generating a function form, to define its
% header, like in 'f(A,B)->...'.
%
% Ex: [{var,Line,'A'}, {var,Line,'B'}] = get_header_params(2).
%
% See also: enumerated_variables_to_form/1.
%
-spec get_header_params( arity() ) -> [ form_element() ].
get_header_params( Arity ) ->
	get_header_params( Arity, _Acc=[] ).


get_header_params( _Arity=0, Acc ) ->
	Acc;

get_header_params( Arity, Acc ) ->
	NewAcc = [ { var, _Line=0, get_iterated_param_name( Arity ) } | Acc ],
	get_header_params( Arity-1, NewAcc ).
