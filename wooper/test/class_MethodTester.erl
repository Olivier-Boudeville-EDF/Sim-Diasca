% Copyright (C) 2007-2021 Olivier Boudeville
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
%
-module(class_MethodTester).


-define( class_description, "Class introduced notably to test the support "
							"of methods.").


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).


-define( class_attributes, [ name ] ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-type name() :: text_utils:ustring().


% Simplest possible signature:
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	trace_utils:info( "construction" ),

	% No mother class.
	setAttribute( State, name, "Terry" ).



-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	trace_utils:info( "destruction" ),

	io:format( "  I am ~ts, and I am just destructed.~n", [ ?getAttr(name) ] ),
	State.



% Method implementations.


% Returns the name of this instance.
-spec getName( wooper:state() ) -> const_request_return( name() ).
getName( State ) ->
	trace_utils:info( "getName/1" ),
	wooper:const_return_result( ?getAttr(name) ).


% Sets the name of this instance.
-spec setName( wooper:state(), name() ) -> oneway_return().
setName( State, Name ) ->
	trace_utils:info( "setName/2" ),
	NewState = setAttribute( State, name, Name ),
	wooper:return_state( NewState ).


% Returns a value established in a static context.
-spec get_static_info( integer(), integer() ) -> static_return( integer() ).
get_static_info( A, B ) ->
	trace_utils:info( "get_static_info/2 called" ),
	wooper:return_static( A + B + 10 ).


% Test of a static method returning nothing (void return):
-spec test_static_void() -> static_void_return().
test_static_void() ->
	%trace_utils:debug( "test_static_void/0 called!" ),
	wooper:return_static_void().
