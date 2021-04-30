% Copyright (C) 2016-2021 Olivier Boudeville
%
% Module defined in order to be able to test the merge services outside of an
% escript context.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Released as LGPL software.
%
-module(merge_app).


-export([ exec/0 ]).

% For update_code_path_for_myriad/0 and all:
-include("myriad_script_include.hrl").



% Runs the merge service, directly from a module.
-spec exec() -> void().
exec() ->

	% First, enable all possible helper code (hence to be done first of all):
	update_code_path_for_myriad_from_module(),

	% To force options for testing:
	%ArgTable = shell_utils:generate_argument_table( "--help" ),

	ArgTable = shell_utils:get_argument_table(),

	merge_utils:main( ArgTable ).
