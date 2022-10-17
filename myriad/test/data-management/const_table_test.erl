% Copyright (C) 2015-2022 Olivier Boudeville
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
% Creation date: Tuesday, May 12, 2015.


% @doc Unit tests for the const-table facilities, which are read-only
% associative tables whose key/value pairs can be read from any number
% (potentially extremely large) of readers very efficiently.
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
%            [ foobar:foo/0, foobar:baz/0, foobar:composite/0 ] } ).

% Tried also with no luck:
%-dialyzer( { no_missing_calls, run/0 } ).
%-dialyzer( { nowarn_function, run/0 } ).


% Generation in-memory only:
test_in_memory_generation() ->

	InMemModName = 'foobar',

	test_facilities:display( "Defining the table that will be made available "
		"directly through an ad-hoc, runtime, in-memory only module, '~ts'.",
		[ InMemModName ] ),

	NestedTerm = { "semper fidelis", true, [ 1, 1.0, ?MODULE ] },

	TargetEntries = [ { 'foo', 42.0 },
					  { 'baz', "hello" },
					  { 'composite', NestedTerm } ],

	test_facilities:display(
		"Generating pseudo-module '~ts' from following ~ts",
		[ InMemModName, list_table:to_string( TargetEntries ) ] ),

	const_table:generate_in_memory( InMemModName, TargetEntries ),


	test_facilities:display( "Using now directly that generated module "
							 "to obtain constants from it." ),

	% Clearer than using InMemModName here:
	42.0 = foobar:foo(),
	"hello" = foobar:baz(),

	ObtainedNestedTerm = foobar:composite(),

	test_facilities:display( "Nested term obtained by calling ~ts:~ts/0:~n~p",
		[ InMemModName, composite, foobar:composite() ] ),

	% Check:
	ObtainedNestedTerm = NestedTerm.

	%will_crash = foobar:non_existing().



% Persistent, in-file generation:
test_in_file_generation() ->

	InFileModName = 'frobbuz_test_generated',

	test_facilities:display( "~nDefining the table that will be made available "
		"through an ad-hoc in-file module, '~ts'.", [ InFileModName ] ),

	NestedTerm = { "si vis pacem", true, [ 3, 3.1415, ?MODULE ] },

	TargetEntries = [ { 'frob', 14.0 },
					  { 'buz', "bye" },
					  { 'another_composite', NestedTerm } ],

	ModFilename = const_table:generate_in_file( InFileModName, TargetEntries ),

	true = file_utils:is_existing_file( ModFilename ),
	false = code:is_loaded( InFileModName ),

	"bye" = InFileModName:buz(),
	NestedTerm = InFileModName:another_composite(),

	ModFullPath = file_utils:ensure_path_is_absolute( ModFilename ),
	{ file, ModFullPath } = code:is_loaded( InFileModName ),

	test_facilities:display( "Generated module has been successfully loaded "
							 "and tested, removing it now." ),

	% Test cleanup:
	file_utils:remove_file( ModFullPath ).




-spec run() -> no_return().
run() ->

	test_facilities:display( "Testing the const_table services, "
		"both in-memory and in-file." ),

	test_facilities:start( ?MODULE ),

	test_in_memory_generation(),

	test_in_file_generation(),

	test_facilities:stop().
