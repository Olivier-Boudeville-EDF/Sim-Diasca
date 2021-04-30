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
% Creation date: Saturday, May 12, 2018.



% Unit tests for the script-related facilities.
%
% See the script_utils.erl tested module.
%
-module(script_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	false = script_utils:is_running_as_escript(),

	test_facilities:display( "Root of Myriad is: ~ts",
							 [ script_utils:get_myriad_base_directory() ] ),

	test_facilities:display( "Script directory is: ~ts",
							 [ script_utils:get_script_base_directory() ] ),

	ArgString = "foo -color red white -bar baz -boom -color blue",

	% Emulated as if they were obtained in the context of an escript:
	CommandLineArgs = text_utils:split( ArgString, _Delimiters=[ $ ] ),

	CanonicalArgTables =
		shell_utils:get_argument_table_from_strings( CommandLineArgs ),

	test_facilities:display( "Command-line interpretation follows, for "
							 "command-line arguments '~ts': ~ts",
		 [ ArgString,
		   shell_utils:argument_table_to_string( CanonicalArgTables ) ] ),

	test_facilities:stop().
