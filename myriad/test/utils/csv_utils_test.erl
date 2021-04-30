% Copyright (C) 2016-2021 Olivier Boudeville
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


% Unit tests for the management of CVS files.
%
% See the csv_utils.erl tested module.
%
-module(csv_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	CSVFilename = "example.csv",

	test_facilities:display( "Testing CSV management, using the '~ts' "
							 "file for that.", [ CSVFilename ] ),

	Separator=$;,

	FieldCount = 5,

	% Let's suppose first that we want to interpret the content of that file and
	% we do not know a priori the field count:
	%
	case csv_utils:interpret_file( CSVFilename, Separator ) of

		{ FieldCount, MixedContent, MatchCount=5, _UnmatchCount=0, 
		  _DropCount=8 } ->
			MatchCount = length( MixedContent ),
			test_facilities:display( "Interpreted mixed content: ~p",
									 [ MixedContent ] );

		Other ->
			throw( { incorrect_interpretation, Other } )

	end,

	{ Content, RowCount, FieldCount } =
		csv_utils:read_file( CSVFilename, Separator ),

	test_facilities:display( "Read ~B rows, each with ~B fields, "
		"corresponding to a ~ts",
		[ RowCount, FieldCount, csv_utils:content_to_string( Content ) ] ),

	NewCSVFilename = "example-written.csv",

	% The internal data (i.e. comments aside) shall match exactly the one of
	% CSVFilename:
	%
	csv_utils:write_file( Content, NewCSVFilename, Separator ),

	file_utils:remove_file( NewCSVFilename ),

	test_facilities:stop().
