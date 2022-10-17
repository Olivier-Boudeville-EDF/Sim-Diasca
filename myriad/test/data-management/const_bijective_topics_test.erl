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
% Creation date: Wednesday, September 14, 2022.


% @doc Unit tests for the const-bijective topic table facilities, which are
% per-topic read-only two-way associative tables allowing bidirectional
% conversions between two sets that can be done from any number (potentially
% extremely large) of callers very efficiently.
%
% See the const_bijective_topics.erl tested module.
%
% See also:
% - const_bijective_table.erl for "single topic" const bijective tables
% - bijective_table.erl for the runtime versions of such tables (as direct
% terms, not modules)
%
-module(const_bijective_topics_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Shorthands:

-type topic_spec() :: const_bijective_topics:topic_spec().


-spec get_topic_specs() -> [ topic_spec() ].
get_topic_specs() ->

	% First, for topic alpha, which relies, by default, on strict look-up:

	NestedTerm = { "semper fidelis", true, [ 1, 1.0, ?MODULE ] },

	% Any term can be placed as first or second element:
	AlphaEntries = [ { 'foo', 42.0 },
					 { 1, <<"hello">> },
					 { "guitar", NestedTerm } ],

	AlphaTopicSpec = { alpha, AlphaEntries },

	% Second, topic beta, which relies on a maybe look-up:
	BetaEntries = [ { "beta_1", <<"train">> },
					{ <<"beta_2">>, "car" },
					{ beta_3, <<"bicycle">> } ],

	% Then there would not be any get_maybe_*/1 (like
	% frobnicator:get_maybe_second_for_beta/1):
	%
	%BetaTopicSpec = { beta, BetaEntries, strict },
	BetaTopicSpec = { beta, BetaEntries, maybe },

	[ AlphaTopicSpec, BetaTopicSpec ].



% Generation in-memory only:
test_in_memory_generation() ->

	InMemModName = 'frobnicator',

	test_facilities:display( "Defining the constant bijective topic table "
		"that will be made available directly through an ad-hoc, runtime, "
		"in-memory only module, '~ts'.", [ InMemModName ] ),

	TopicSpecs = get_topic_specs(),

	test_facilities:display(
		"Generating pseudo-module '~ts' from following topic specs:~n ~p",
		[ InMemModName, TopicSpecs ] ),

	const_bijective_topics:generate_in_memory( InMemModName, TopicSpecs ),


	test_facilities:display( "Using now directly that generated module "
							 "to obtain two-way constants from it." ),

	% Clearer than using InMemModName here:
	<<"hello">> = frobnicator:get_second_for_alpha( 1 ),
	foo = frobnicator:get_first_for_alpha( 42.0 ),

	% Function not defined, as strict lookup for alpha:
	try

		would_crash = frobnicator:get_maybe_first_for_alpha( <<"Zorblug">> )

	catch

		error:undef ->
			ok

	end,

	"car" = frobnicator:get_second_for_beta( <<"beta_2">> ),
	beta_3 = frobnicator:get_first_for_beta( <<"bicycle">> ),

	try

		would_crash = frobnicator:get_second_for_beta( 'non_existing' )

	catch

		throw:{ first_not_found, beta, non_existing } ->
			ok

	end,

	undefined = frobnicator:get_maybe_second_for_beta( 'non_existing' ),
	undefined = frobnicator:get_maybe_first_for_beta( 'non_existing' ),

	try

		would_crash = frobnicator:get_second_for_gamma( 'any' )

	catch

		error:undef ->
			ok

	end.



% Persistent, in-file generation:
test_in_file_generation() ->

	InFileModName = 'darbar_test_generated',

	test_facilities:display( "~nDefining the constant bijective topic table "
		"that will be made available through an ad-hoc in-file module, '~ts'.",
		[ InFileModName ] ),

	TopicSpecs = get_topic_specs(),

	ModFilename = const_bijective_topics:generate_in_file( InFileModName,
														   TopicSpecs ),

	true = file_utils:is_existing_file( ModFilename ),
	false = code:is_loaded( InFileModName ),


	% Clearer than using InMemModName here:
	<<"hello">> = darbar_test_generated:get_second_for_alpha( 1 ),
	foo = darbar_test_generated:get_first_for_alpha( 42.0 ),

	% Function not defined (alpha lookup being strict):
	try

		would_crash =
			darbar_test_generated:get_maybe_second_for_alpha( 'non_existing' )

	catch

		error:undef ->
			ok

	end,

	<<"train">> = darbar_test_generated:get_second_for_beta( "beta_1" ),
	beta_3 = darbar_test_generated:get_first_for_beta( <<"bicycle">> ),

	try

		would_crash =
			darbar_test_generated:get_second_for_alpha( 'non_existing' )

	catch

		throw:{ first_not_found, alpha, non_existing } ->
			ok

	end,

	% As beta lookup is maybe:
	undefined =
		darbar_test_generated:get_maybe_second_for_beta( 'non_existing' ),

	try

		would_crash = darbar_test_generated:get_second_for_gamma( 'any' )

	catch

		error:undef ->
			ok

	end,

	ModFullPath = file_utils:ensure_path_is_absolute( ModFilename ),
	{ file, ModFullPath } = code:is_loaded( InFileModName ),

	test_facilities:display( "Generated module has been successfully loaded "
							 "and tested, removing it now." ),

	% Test cleanup:
	file_utils:remove_file( ModFullPath ).




-spec run() -> no_return().
run() ->

	test_facilities:display( "Testing the const_bijective_topics services, "
		"both in-memory and in-file." ),

	test_facilities:start( ?MODULE ),

	test_in_memory_generation(),

	test_in_file_generation(),

	test_facilities:stop().
