% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Sunday, November 3, 2024.


% Like the Erlang shell:
-define( default_command_history_max_depth, 20 ).
-define( default_result_history_max_depth, 20 ).

% Thus a reserved (non-muted) name:
-define( shell_state_binding_name, "_MyriadBindingForShellState" ).


% The state of a Myriad (custom) shell instance.
-record( shell_state, {

	% The number of commands already submitted; corresponds to the number
	% (identifier) of any last command (or the one of the next command, minus
	% 1):
	%
	submission_count = 0 :: basic_utils:count(),

	% Tells whether the start of commands shall be timestamped:
	do_timestamp = false :: boolean(),


	% Tells whether the full history (inputs and outputs) shall be logged on
	% file and, if yes, in which one:
	%
	log_path = undefined :: option( file_utils:bin_file_path() ),

	% The file (if any) where logs are to be written:
	log_file = undefined :: option( file_utils:file() ),


	% Maximum number of command history elements (0: none; 1: just the last one,
	% etc.; undefined: infinite):
	%
	cmd_history_max_depth=?default_command_history_max_depth ::
		option( basic_utils:count() ),

	% Maximum number of result history elements (0: none; 1: just the last one,
	% etc.; undefined: infinite):
	%
	res_history_max_depth=?default_result_history_max_depth ::
		option( basic_utils:count() ),


	% Possibly ellipsed:
	cmd_history :: shell_utils:command_history(),

	% Possibly ellipsed:
	res_history :: shell_utils:result_history(),

	% Any file for persistent storage of past commands:
	cmd_history_file :: option( file_utils:file() ),


	% Records all current bindings:
	bindings :: erl_eval:binding_struct(),


	% The name of the module used by this shell in order to locate its built-in
	% functions:
	%
	% (made to be overridden with special-purpose modules, potentially using
	% this one as for default implementations)
	%
	callback_module='shell_default_callbacks' :: meta_utils:module_name(),

	% Any module (e.g. 'gui_shell') of reference, for example from which help
	% information can be obtained.
	%
	reference_module :: option( meta_utils:module_name() ),


	% The function that shall be called when there is a call to a local function
	% in a shell command.
	%
	local_fun_handler :: erl_eval:local_function_handler()


	% At least currently, a shell does not keep track of the process(es) using
	% it.

} ).
