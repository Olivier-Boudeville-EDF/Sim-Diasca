% Copyright (C) 2019-2026 Olivier Boudeville
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
% Creation date: Sunday, November 17, 2019.

-module(type_utils_test).

-moduledoc """
Unit tests for the **type-related** services.

See the `type_utils` tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),


    test_facilities:display( "Testing type interpretion." ),

    FirstTerm = "Hello",

    test_facilities:display( "Interpreting '~ts': ~ts",
        [ FirstTerm, type_utils:interpret_type_of( FirstTerm ) ] ),

    SecondTerm = [ my_atom, 4,
        { a_tag, 2.0, maps:from_list( [ { self(), [ FirstTerm ] } ] ) } ],

    %MaxLevel = 0,
    %MaxLevel = 1,
    %MaxLevel = 2,
    MaxLevel = infinite,

    test_facilities:display( "Interpreting '~p': ~ts",
        [ SecondTerm, type_utils:interpret_type_of( SecondTerm, MaxLevel ) ] ),


    % To test various (valid) type strings:
    %TypeStr = "[float()]",
    %TypeStr = "[T]",
    TypeStr = "{bar,integer()}",

    % To test various invalid type strings:
    %TypeStr = "list(float())",
    %TypeStr = "list(T)",

    test_facilities:display( "Parsing the type of '~ts'.", [ TypeStr ] ),

    { ok, ParsedType } = type_utils:parse_type( TypeStr ),

    test_facilities:display( "Type parsing resulted in contextual type ~w.~n"
        "It is described as '~ts'.",
        [ ParsedType, type_utils:type_to_string( ParsedType ) ] ),


    FooTypeStr = "table(tuple(V,float()),union(bar,integer(),option(U)))",

    test_facilities:display(
        "Defining now a parametrised type foo(U,V) as ~ts.", [ FooTypeStr ] ),

    % Building this contextual type from a string:
    { ok, FooCtxtType } = type_utils:parse_type( FooTypeStr ),

    % As the order of type variables will matter when using such parametrised
    % types:
    %
    WithFooTypedefTable = type_utils:define_type( foo, [ 'U', 'V' ],
                                                  FooCtxtType, table:new() ),

    test_facilities:display( "Once foo/2 has been defined, having: ~ts.",
        [ type_utils:typedef_table_to_string( WithFooTypedefTable ) ] ),

    % Defining a partially-instantiated type using this foo/2 (with U=X and
    % Y=atom()):
    %
    WithFooTypeStr = "{buzz, foo(X,atom())}",

    { ok, WithFooCtxtType } = type_utils:parse_type( WithFooTypeStr ),

    WithFooExplType = type_utils:resolve_type( WithFooCtxtType,
                                               WithFooTypedefTable ),

    test_facilities:display( "The explicit (still parametrised) type obtained "
        "from contextual type ~ts is ~w.~nIt is described as '~ts'.",
        [ WithFooTypeStr, WithFooExplType,
          type_utils:type_to_string( WithFooExplType ) ] ),

    % Fully instantiating this type, with X=boolean():
    NonParamFooExplType = type_utils:instantiate_type( WithFooExplType,
        [ { 'X', {boolean,[]} } ] ),

    test_facilities:display( "By setting the type variable X=boolean(), "
        "obtaining explicit, non-parametrised type ~ts.",
        [ type_utils:type_to_string( NonParamFooExplType ) ] ),

    false = type_utils:is_of_type( hello, NonParamFooExplType ),
    false = type_utils:is_of_type( buzz, NonParamFooExplType ),

    MyFirstTable = table:new(),
    true = type_utils:is_of_type( { buzz, MyFirstTable }, NonParamFooExplType ),

    MySecondTable = table:singleton( _K={ hello, 1.0 }, _V=bar ),
    true = type_utils:is_of_type( { buzz, MySecondTable },
                                  NonParamFooExplType ),

    MyThirdTable = table:new( [ { { hello, 1.0 }, bar }, { { good, 0.0 }, 4 },
                                { { bye, 1.1 }, false } ] ),

    true = type_utils:is_of_type( { buzz, MyThirdTable },
                                  NonParamFooExplType ),

    MyFourthTable = table:singleton( hello, bar ),
    false = type_utils:is_of_type( { buzz, MyFourthTable },
                                   NonParamFooExplType ),

    MyFifthTable = table:singleton( { hello, 1.0 }, buzz ),
    false = type_utils:is_of_type( { buzz, MyFifthTable },
                                   NonParamFooExplType ),


    TestTypedefTable = type_utils:get_base_typedef_table(),

    TestCtxtType = {string,[]},

    TestExplType = type_utils:resolve_type( {string,[]}, TestTypedefTable ),

    test_facilities:display( "Resolving the contextual type ~w "
        "with the Myriad typedef table yields the ~w explicit type, i.e. ~ts.",
        [ TestCtxtType, TestExplType,
          type_utils:type_to_string( TestExplType ) ] ),

    ResolvedTypedefTable = type_utils:resolve_typedef_table( TestTypedefTable ),

    test_facilities:display( "Resolving the (Myriad-vanilla) ~ts "
        "results in a ~ts", [
            type_utils:typedef_table_to_string( TestTypedefTable ),
            type_utils:typedef_table_to_string( ResolvedTypedefTable ) ] ),


    test_facilities:stop().
