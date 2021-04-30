% Copyright (C) 2015-2021 Olivier Boudeville
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

% Creation date: Tuesday, May 12, 2015
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% Unit tests for the const-table facilities.
%
% See the const_table.erl tested module.
%
-module(const_table_test).


% For run/0 export and al:
-include("test_facilities.hrl").



% Suppressing warnings due to the runtime generation of functions that Dialyzer
% cannot be aware of:
%
% (currently specifying a module is not permitted: 'bad attribute')
%
%-dialyzer( { nowarn_function,
%			 [ foobar:foo/0, foobar:baz/0, foobar:composite/0 ] } ).

% Tried also with no luck:
%-dialyzer( { no_missing_calls, run/0 } ).
%-dialyzer( { nowarn_function, run/0 } ).


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	NestedTerm = { "semper fidelis", true, [ 1, 1.0, ?MODULE ] },

	TargetTable = table:add_entries( [ { 'foo', 42.0 },
									   { 'baz', "hello" },
									   { 'composite', NestedTerm } ],
									 table:new() ),

	ModuleName = 'foobar',

	test_facilities:display(
	  "Generating pseudo-module '~ts' from following table:~n~ts",
	  [ ModuleName, table:to_string( TargetTable ) ] ),

	const_table:generate( ModuleName, TargetTable ),

	42.0 = foobar:foo(),
	"hello" = foobar:baz(),
	NestedTerm = foobar:composite(),

	test_facilities:display( "Nested term: ~p~n", [ foobar:composite() ] ),

	%will_crash = foobar:non_existing(),

	test_facilities:stop().
