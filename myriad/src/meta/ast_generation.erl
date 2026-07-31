% Copyright (C) 2018-2026 Olivier Boudeville
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

-module(ast_generation).

-moduledoc """
Module in charge of **generating parts of an AST** (for example elements of
forms), or recoprocally generating terms from forms.

See also the `ast_utils` module for example `ast_utils:term_to_form/1`.
""".


-export([ list_to_form/1, form_to_list/1, list_form_length/1,
          list_to_tuple_form/2,
          %tuple_to_form/1, form_to_tuple/1,
          atoms_to_form/1, form_to_atoms/1, form_to_term/1,
          enumerated_variables_to_form/1,
          get_iterated_param_name/1, get_header_params/1 ]).


% Type shorthands:

-type count() :: basic_utils:count().

-type form_element() :: ast_base:form_element().
-type file_loc() :: ast_base:file_loc().


% For the default_generation_location define:
-include("ast_utils.hrl").



-doc """
Transforms the specified list (whose elements are typically themselves form
elements already) into the AST version of a list.

This is thus not a deep transformation, only the top-level elements are
considered.

For example: `list_to_form( [{atom,FileLoc,a}, {atom,FileLoc,b}]) = {cons,
FileLoc, {atom,FileLoc,a}, {cons, FileLoc, {atom,FileLoc,b}, {nil,FileLoc}}}`.

See `form_to_list/1` for the reciprocal function.
""".
-spec list_to_form( list() ) -> form_element().
list_to_form( _List=[] ) ->
    { nil, ?default_generation_location };

list_to_form( _List=[ E | T ] ) ->
    { cons, ?default_generation_location, E, list_to_form( T ) }.



-doc """
Transforms the specified AST list into the corresponding plain list.

For example: `form_to_list( {cons, FileLoc, {atom,FileLoc,a}, {cons, FileLoc,
{atom,FileLoc,b}, {nil,FileLoc} }}) = [{atom,FileLoc,a}, {atom,FileLoc,b}]`.

See `list_to_form/1` for the reciprocal function.
""".
-spec form_to_list( form_element() ) -> list().
form_to_list( { nil, _FileLoc } ) ->
    [];

form_to_list( { cons, _FileLoc, E, NestedForm } ) ->
    [ E | form_to_list( NestedForm ) ].



-doc """
Returns the length of the list whose AST form is specified.

Cheaper than `length(form_to_list(MyForm))`.
""".
-spec list_form_length( form_element() ) -> count().
list_form_length( { nil, _FileLoc } ) ->
    0;

list_form_length( { cons, _FileLoc, _E, NestedForm } ) ->
    1 + list_form_length( NestedForm ).



-doc """
Returns the AST form corresponding to a tuple whose elements would be the ones
of the specified list.

For example, if the list is `[FormA,FormB]`, then returns, in AST form, a
`{FormA,FormB}` pair.
""".
-spec list_to_tuple_form( list(), file_loc() ) -> form_element().
list_to_tuple_form( FormList, FileLoc ) ->
    { tuple, FileLoc, FormList }.



-doc """
Returns the form element corresponding to the specified list of atoms.

For example: `{cons, FileLoc, {atom,FileLoc,a}, {cons, FileLoc,
{atom,FileLoc,b}, {nil, FileLoc} }} = atoms_to_form(['a', 'b'])`.
""".
-spec atoms_to_form( [ atom() ] ) -> form_element().
atoms_to_form( _AtomList=[] ) ->
    { nil, ?default_generation_location };

atoms_to_form( _AtomList=[ Atom | H ] ) ->
    FileLoc = ?default_generation_location,
    { cons, FileLoc, { atom, FileLoc, Atom }, atoms_to_form( H ) }.



-doc """
Returns the list of atoms corresponding to the specified form element.

For example `['a', 'b'] = atoms_to_form( {cons, FileLoc, {atom,FileLoc,a},
{cons, FileLoc, {atom,FileLoc,b}, {nil,FileLoc}}})`.
""".
-spec form_to_atoms( form_element() ) -> [ atom() ].
form_to_atoms( { nil, _FileLoc } ) ->
    [];

form_to_atoms( { cons, _FileLoc, {atom,_,Atom}, NestedForm } ) ->
    [ Atom | form_to_atoms( NestedForm ) ].



-doc """
Returns the term corresponding to the specified form element.

For example `[{float,boolean}] = form_to_term({cons,0, {tuple,0,
[{atom,0,float},{atom,0,boolean}]}, {nil,0}})`.

No variable expected in the AST (e.g. no `{var,1,'T'}`), otherwise a `badarg` is
raised.

See ast_utils:term_to_form/1` for the reciprocal operation.
""".
-spec form_to_term( form_element() ) -> term().
form_to_term( Form ) ->
   erl_parse:normalise( Form ).



-doc """
Returns the form element corresponding to the list of variables corresponding to
the specified number of such variables.

For example: `{cons, FileLoc, {var,FileLoc,'Myriad_Param_1'}, {cons, FileLoc,
{var,FileLoc,'Myriad_Param_2'}, {nil,FileLoc}}} =
enumerated_variables_to_form(2)`.

See also: `get_header_params/1`.
""".
-spec enumerated_variables_to_form( count() ) -> form_element().
enumerated_variables_to_form( Count ) ->
    enumerated_variables_to_form( Count, _Index=1 ).


enumerated_variables_to_form( _Count=0, _Index ) ->
    { nil, ?default_generation_location };

enumerated_variables_to_form( Count, Index ) ->
    FileLoc = ?default_generation_location,
    { cons, FileLoc, { var, FileLoc, get_iterated_param_name( Index ) },
      enumerated_variables_to_form( Count-1, Index+1 ) }.



-doc """
Returns, in AST form, a reference to the specified iterated variable.

For example: `'Myriad_Param_4' = get_iterated_param_name(4)`.
""".
-spec get_iterated_param_name( count() ) -> atom().
get_iterated_param_name( Count ) ->
    String = text_utils:format( "Myriad_Param_~B", [ Count ] ),
    text_utils:string_to_atom( String ).



-doc """
Returns, as form elements, conventional call parameter names, as form elements,
corresponding to a function of the specified arity.

This is typically useful when generating a function form, to define its header,
like in `f(A,B)->...`.

For example: `[{var,FileLoc,'Myriad_Param_1'}, {var,FileLoc,'Myriad_Param_2'}] =
get_header_params(2)`.

See also `enumerated_variables_to_form/1`.
""".
-spec get_header_params( arity() ) -> [ form_element() ].
get_header_params( Arity ) ->
    get_header_params( Arity, _Acc=[] ).


get_header_params( _Arity=0, Acc ) ->
    Acc;

get_header_params( Arity, Acc ) ->

    NewAcc = [ { var, ?default_generation_location,
                 get_iterated_param_name( Arity ) } | Acc ],

    get_header_params( Arity-1, NewAcc ).
