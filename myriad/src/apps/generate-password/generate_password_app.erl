% Copyright (C) 2016-2025 Olivier Boudeville
%
% Released as LGPL software.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Created date: 2016.

-module(generate_password_app).

-moduledoc """
A Myriad application to **generate strong, safe passwords**.

Module defined in order to be able to test the password generation services
outside of an escript context.
""".


-export([ exec/0 ]).


% For update_code_path_for_myriad/0 and all:
-include("myriad_script_include.hrl").



-doc "Executes this password-generating application.".
-spec exec() -> void().
exec() ->

	% First, enable all possible helper code (hence to be done first of all):
	update_code_path_for_myriad_from_module(),

	% To force options for testing:
	%ArgTable = cmd_line_utils:generate_argument_table( "--interactive" ),
	%ArgTable = cmd_line_utils:generate_argument_table( "-i" ),
	%ArgTable = cmd_line_utils:generate_argument_table( "-i --unexpected" ),
	%ArgTable = cmd_line_utils:generate_argument_table( "" ),
	ArgTable = cmd_line_utils:get_argument_table(),

	password_generation:main( ArgTable ).
