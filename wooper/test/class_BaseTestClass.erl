% Copyright (C) 2014-2021 Olivier Boudeville
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
% Creation date: Wednesday, December 24, 2014
%
-module(class_BaseTestClass).


-define( class_description,
		 "Class introduced notably to test inheritance: tests the base "
		 "services and also serves as a mother class "
		 "(see class_ChildTestClass)." ).


% Determines what are the direct mother classes of this class (if any):
% (one of these two lines may or may not be uncommented)
%-superclasses([]).
%-define( superclasses, [] ).


-type name() :: text_utils:ustring().
-type gender() :: maybe( 'male' | 'female' ).

-export_type([ name/0, gender/0 ]).


% Class-specific attributes:
-define( class_attributes, [
	{ name, name(), [ const, protected ], "Name of this creature" },
	{ gender, gender(), "Gender of this creature" },
	{ age, integer(), { initial, 0 }, "The current age of this creature" } ] ).




% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Constructs a new instance.
-spec construct( wooper:state(), name(), gender() ) -> wooper:state().
construct( State, Name, Gender ) ->
	% No mother class.
	setAttributes( State, [ { name, Name }, { gender, Gender } ] ).


% No specific destruct/1.


% Method implementations.

% One method of each nature, with or without a spec.


% Returns the name of this creature.
-spec getName( wooper:state() ) -> const_request_return( name() ).
getName( State ) ->

	Name = ?getAttr(name),

	%trace_utils:debug_fmt( "getName/1 request called by ~w.",
	%					   [ ?getSender() ] ),

	wooper:const_return_result( Name ).


% Sets the name of this creature.
setName( State, Name ) ->
	NewState = setAttribute( State, name, Name ),
	wooper:return_state( NewState ).


% A request not meant to be overridden.
-spec aRequest( wooper:state(), integer() ) ->
					  const_request_return( integer() ).
aRequest( State, Arg ) ->
	wooper:const_return_result( Arg + 5 ).


% A request meant to be overridden.
-spec someRequest( wooper:state(), integer() ) ->
						 const_request_return( integer() ).
someRequest( State, Arg ) ->
	wooper:const_return_result( Arg + 7 ).


% Used to mask to WOOPER an actual throw:
-spec my_throw_helper() -> no_return().
my_throw_helper() ->
	throw( report_exception ).


% To test wooper:throwing/1:
-spec testThrow( wooper:state() ) -> const_oneway_return().
testThrow( _State ) ->
	%wooper:const_return().
	wooper:throwing( my_throw_helper() ).


% Returns some mean count.
get_some_mean_count() ->
	wooper:return_static( 6 ).
