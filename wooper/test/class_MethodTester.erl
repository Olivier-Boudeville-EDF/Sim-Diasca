% Copyright (C) 2007-2026 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER library.
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
% Creation date: 2007.

-module(class_MethodTester).

-moduledoc """
Test of the **support of methods**.
""".


-define( class_description,
         "Class introduced notably to test the support of methods.").


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).


-define( class_attributes, [ id, name ] ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Local types:
-type id() :: integer().
-type name() :: text_utils:ustring().


% Type shorthand:
-type concurrent_result( R ) :: wooper:concurrent_result( R ).


-doc "Simplest possible signature.".
-spec construct( wooper:state(), id() ) -> wooper:state().
construct( State, Id ) ->

    trace_utils:info_fmt( "Construction of method tester of identifier #~B.",
                          [ Id ] ),

    % No mother class.
    setAttributes( State, [ { id, Id },
                            { name, "Terry" } ] ).



-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

    trace_utils:info( "Destruction." ),

    io:format( "  I am ~ts, and I am just destructed.~n", [ ?getAttr(name) ] ),
    State.



% Method implementations.


-doc "Returns the name of this instance.".
-spec getName( wooper:state() ) -> const_request_return( name() ).
getName( State ) ->
    trace_utils:info( "getName/1 called." ),
    wooper:const_return_result( ?getAttr(name) ).



-doc "Sets the name of this instance.".
-spec setName( wooper:state(), name() ) -> oneway_return().
setName( State, Name ) ->
    trace_utils:info( "setName/2 called." ),
    NewState = setAttribute( State, name, Name ),
    wooper:return_state( NewState ).



-doc "Returns the identifier of this instance.".
-spec getId( wooper:state() ) ->
                    const_request_return( concurrent_result( id() ) ).
getId( State ) ->
    ReportedId = case ?getAttr(id) of

        2 ->
            trace_utils:warning(
                "Tester instance #2 will answer too late (and wrongly)." ),
            timer:sleep( _Ms=5000 ),
            -1;

        CorrectId ->
            trace_utils:info_fmt( "Returning our (correct) identifier, #~B.",
                                  [ CorrectId ] ),
            CorrectId

    end,

    wooper:const_return_result( wooper:forge_concurrent_result( ReportedId ) ).



-doc "Returns a value established in a static context.".
-spec get_static_info( integer(), integer() ) -> static_return( integer() ).
get_static_info( A, B ) ->
    trace_utils:info( "get_static_info/2 called" ),
    wooper:return_static( A + B + 10 ).



-doc "Test of a static method returning nothing (void return).".
-spec test_static_void() -> static_void_return().
test_static_void() ->
    %trace_utils:debug( "test_static_void/0 called!" ),
    wooper:return_static_void().
