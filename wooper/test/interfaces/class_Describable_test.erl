% Copyright (C) 2008-2023 Olivier Boudeville
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
% Creation date: 2008.


% @doc Unit tests for the <b>Describable</b> class implementation.
%
% See the class_Describable module.
%
-module(class_Describable_test).


-export([ run/0 ]).


% @doc Runs the tests.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Creating a test Describable." ),

	MyDescribable = class_Describable:new_link(),

	% Previously description was stored by the describable and thus could be
	% checked, set, etc.

	MyDescribable ! { getDescription, [], self() },

	BinDescription = text_utils:string_to_binary(
		class_Describable:to_string( undefined ) ),

	% Check:
	BinDescription = receive

		{ wooper_result, FirstDesc } ->
			FirstDesc

	end,

	test_facilities:display( "Correct description returned: '~ts'.",
							 [ BinDescription ] ),

	wooper:delete_synchronously_instance( MyDescribable ),

	test_facilities:stop().
