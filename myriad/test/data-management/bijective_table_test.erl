% Copyright (C) 2019-2021 Olivier Boudeville
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
% Creation date: Saturday, May 4, 2019


% Test of the bijective table implementation.
%
% See the bijective_table.erl tested module.
%
-module(bijective_table_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	TestTable = bijective_table:new( [ { "one", 1 }, { "two", 2 },
						{ "three", 3 }, { "four", 4 }, { "five", 5 } ] ),

	"four" = bijective_table:get_first_for( 4, TestTable ),

	3 = bijective_table:get_second_for( "three", TestTable ),

	% Failure tests:
	%FailingTable = bijective_table:new([ { "one", 1 }, { "two", 1 } ] ),
	_Table = bijective_table:new( [ { "one", 1 }, { "two", 2 } ] ),

	%bijective_table:get_first_for( 42, FailingTable ),

	%bijective_table:get_second_for( "unexpected", FailingTable ),

	test_facilities:display( "Test table: ~ts",
							 [ bijective_table:to_string( TestTable ) ] ),

	test_facilities:stop().
