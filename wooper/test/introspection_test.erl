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
% Creation date: Wednesday, October 31, 2018.


% This module allows to test the support for introspection.
-module(introspection_test).


-export([ run/0 ]).


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_test(),

	test_facilities:display( "Running introspection test." ),

	TargetClass = class_ChildTestClass,

	test_facilities:display( "Superclasses of ~ts: ~p~n",
			[ TargetClass, TargetClass:get_superclasses() ] ),

	test_facilities:display( "Attribute information for ~ts:~n  ~p~n",
			[ TargetClass, TargetClass:get_class_specific_attributes() ] ),


	test_facilities:display( "Attribute names of ~ts instances: ~ts",
		[ TargetClass, text_utils:atoms_to_string(
			wooper_introspection:get_class_specific_attribute_names(
			  TargetClass ) ) ] ),

	test_facilities:display( "Test success." ),

	test_facilities:stop().
