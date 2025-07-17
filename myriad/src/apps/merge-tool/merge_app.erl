% Copyright (C) 2016-2025 Olivier Boudeville
%
% Released as LGPL software.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: 2016.

-module(merge_app).

-moduledoc """
Module in charge of the actual **launch of the Myriad merge tool**.

Transferred from `merge-tree.escript` in order to benefit from a more
user-friendly debugging.
""".

-export([ exec/0 ]).


% For update_code_path_for_myriad/0 and all:
-include("myriad_script_include.hrl").



-doc "Runs the merge service, directly from a module.".
-spec exec() -> void().
exec() ->

	% First, enable all possible helper code (hence to be done first of all):
	update_code_path_for_myriad_from_module(),

	% To force options for testing:
	%ArgTable = cmd_line_utils:generate_argument_table( "--help" ),

	ArgTable = cmd_line_utils:get_argument_table(),

	%trace_utils:debug_fmt( "Run as application: ~ts",
	%   [ cmd_line_utils:argument_table_to_string( ArgTable ) ] ),

	merge_utils:main( ArgTable ).
