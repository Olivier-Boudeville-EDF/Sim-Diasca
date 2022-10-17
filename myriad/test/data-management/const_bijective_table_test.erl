% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Sunday, September 11, 2022.


% @doc Unit tests for the const-bijective table facilities, which are read-only
% two-way associative table allowing bidirectional conversions between two sets
% that can be done from any number (potentially extremely large) of callers very
% efficiently.
%
% See the const_bijective_table.erl tested module.
%
% See also bijective_table.erl for the runtime versions of such tables (as
% direct terms, not modules).
%
-module(const_bijective_table_test).


% For run/0 export and al:
-include("test_facilities.hrl").



% Generation in-memory only:
test_in_memory_generation() ->

	InMemModName = 'frobnicator',

	test_facilities:display( "Defining the constant bijective table "
		"that will be made available directly through an ad-hoc, runtime, "
		"in-memory only module, '~ts'.", [ InMemModName ] ),

	NestedTerm = { "semper fidelis", true, [ 1, 1.0, ?MODULE ] },

	% Any term can be placed as first or second element:
	TargetEntries = [ { 'foo', 42.0 },
					  { 1, <<"hello">> },
					  { "guitar", NestedTerm } ],

	test_facilities:display(
		"Generating pseudo-module '~ts' from following entries:~n ~p",
		[ InMemModName, TargetEntries ] ),

	const_bijective_table:generate_in_memory( InMemModName, TargetEntries ),


	test_facilities:display( "Using now directly that generated module "
							 "to obtain two-way constants from it." ),

	% Clearer than using InMemModName here:
	42.0 = frobnicator:get_second_for( 'foo' ),
	'foo' = frobnicator:get_first_for( 42.0 ),

	<<"hello">> = frobnicator:get_second_for( 1 ),
	1 = frobnicator:get_first_for( <<"hello">> ),

	NestedTerm = frobnicator:get_second_for( "guitar" ),
	"guitar" = frobnicator:get_first_for( NestedTerm ).

	% No get_maybe_{first,second}_for/1 tested as using here the default
	% 'strict' element look-up.

	%will_crash = frobnicator:get_second_for( 'non_existing' ).



% Persistent, in-file generation:
test_in_file_generation() ->

	InFileModName = 'darbar_test_generated',

	test_facilities:display( "~nDefining the constant bijective table "
		"that will be made available through an ad-hoc in-file module, '~ts'.",
		[ InFileModName ] ),

	NestedTerm = { "si vis pacem", true, [ 3, 3.1415, ?MODULE ] },

	SomeTerm = <<"Good">>,
	% Should not be used, as transient:
	%SomeTerm = self(),

	TargetEntries = [ { 'frob', 14.0 },
					  { "bye", 'buz' },
					  { NestedTerm, SomeTerm } ],

	ModFilename = const_bijective_table:generate_in_file( InFileModName,
						TargetEntries, _ElementLookup='maybe' ),

	true = file_utils:is_existing_file( ModFilename ),
	false = code:is_loaded( InFileModName ),

	14.0 = InFileModName:get_second_for( frob ),
	frob = InFileModName:get_first_for( 14.0 ),

	buz = InFileModName:get_second_for( "bye"),
	"bye" = InFileModName:get_first_for( buz ),

	SomeTerm = InFileModName:get_second_for( NestedTerm ),
	NestedTerm = InFileModName:get_first_for( SomeTerm ),

	% Neither among the first elements nor the second ones:
	NonExisting = 2,

	% As using here a 'maybe' element look-up:
	undefined = InFileModName:get_maybe_first_for( NonExisting ),
	undefined = InFileModName:get_maybe_second_for( NonExisting ),

	ModFullPath = file_utils:ensure_path_is_absolute( ModFilename ),
	{ file, ModFullPath } = code:is_loaded( InFileModName ),

	test_facilities:display( "Generated module has been successfully loaded "
							 "and tested, removing it now." ),

	% Test cleanup:
	file_utils:remove_file( ModFullPath ).




-spec run() -> no_return().
run() ->

	test_facilities:display( "Testing the const_bijective_table services, "
		"both in-memory and in-file." ),

	test_facilities:start( ?MODULE ),

	test_in_memory_generation(),

	test_in_file_generation(),

	test_facilities:stop().
