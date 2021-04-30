% Copyright (C) 2014-2021 Olivier Boudeville
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


% Unit tests for the type_utils services.
%
% See the type_utils.erl tested module.
%
-module(type_utils_test).


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

	test_facilities:stop().
