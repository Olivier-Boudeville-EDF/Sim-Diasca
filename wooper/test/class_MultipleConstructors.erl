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
-module(class_MultipleConstructors).


-define( class_description,
		 "Allows the testing the support of multiple constructors: three "
		 "different ones are defined, none exported." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).

-define( class_attributes, [ name, gender ] ).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-type name() :: text_utils:ustring().
-type gender() :: atom().


% Constructs a new instance from two construction parameters.
-spec construct( wooper:state(), name(), gender() ) -> wooper:state().
construct( State, Name, Gender ) ->
	% No mother class.
	setAttributes( State, [ { name, Name }, { gender, Gender } ] ).



% Constructs a new instance from a single construction parameter.
%
% Of course multiple clauses may exist:
%
-spec construct( wooper:state(), name() ) -> wooper:state().
construct( State, Name="Murdock" ) ->
	% No mother class.
	setAttributes( State, [ { name, Name }, { gender, undefined } ] );

construct( State, Name ) ->
	% No mother class.
	setAttributes( State, [ { name, Name }, { gender, unknown } ] ).


% Simplest possible signature:
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->
	% No mother class.
	setAttributes( State, [ { name, "Terry" }, { gender, unknown } ] ).


% Overriding the default destructor:
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	io:format( "  I am ~ts, and I am just destructed.~n", [ ?getAttr(name) ] ),
	State.



% Method implementations.


% Returns the name of this instance.
-spec getName( wooper:state() ) -> const_request_return( name() ).
getName( State ) ->
	wooper:const_return_result( ?getAttr(name) ).


% Returns the gender of this instance.
-spec getGender( wooper:state() ) -> const_request_return( gender() ).
getGender( State ) ->
	wooper:const_return_result( ?getAttr(gender) ).
