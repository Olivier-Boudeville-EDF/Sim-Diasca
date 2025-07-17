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
% Creation date: Saturday, November 2, 2024.

-module(shell_default_callbacks).

-moduledoc """
This module is the default implementation module offered by Myriad for the
callbacks of its custom shell (defined in the shell_utils module).

Defined separately from shell_utils for clarity and to showcase an actual
uncoupling.

The current module provides the default shell built-in functions.

It may be called in turn by any user-supplied shell callback module, for default
implementations.
""".


% Facilities at the level of this callback module:
-export([ list_builtin_commands/0 ]).



% Shell built-in commands:
%
% (if updating this export, update list_builtin_commands/0 accordingly)
%
-export([ list_bindings/1, b/1, print_bindings/1,
		  clear_bindings/1, f/1, clear_binding/2, f/2,
		  print_command_history/1, hc/1, print_result_history/1, hr/1,
		  recall_command/2, r/2, get_result/2,
		  clear_commands/1, fc/1, clear_results/1, fr/1,
		  set_command_history_depth/2, set_result_history_depth/2,
		  clear_persistent_command_history/1,
		  help/1 ]).





% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type shell_state() :: shell_utils:shell_state().

-type command_id() :: shell_utils:command_id().
-type command_str() :: shell_utils:command_str().
-type command_result() :: shell_utils:command_result().
-type message() :: shell_utils:message().

-type binding() :: shell_utils:binding().
-type variable_string_name() :: shell_utils:variable_string_name().
-type builtin_state_only() :: shell_utils:builtin_state_only().
-type builtin_state_only(T) :: shell_utils:builtin_state_only(T).

-type function_id() :: meta_utils:function_id().



% Implementation notes:
%
% A shell built-in command is in the following form: `f(ShellState ::
% shell_state(), Param1, Param2, ...) -> {shell_state(), Result}` where Result
% (possibly 'ok' if none applies) is returned to the caller (the updated shell
% state is just extracted and reused internally, transparently).
%
% The implementation of at least some of these commands has been defined in
% shell_utils, both for an easier debugging (when called from a
% non-erl_eval:expr/* context) and re-use (for other callback modules).


-include("shell_utils.hrl").



-doc """
Lists the name and arity for each of the shell built-in commands supported by
this callback module.
""".
-spec list_builtin_commands() -> [ function_id() ].
list_builtin_commands() ->
	% See the corresponding export:
	[ { list_bindings, 1 }, { b, 1 }, { print_bindings, 1 },
	  { clear_bindings, 1 }, { f, 1 }, { clear_binding, 2 }, { f, 2 },
	  { print_command_history, 1 }, { hc, 1 },
	  { print_result_history, 1 }, { hr, 1 },
	  { recall_command, 2 }, { r, 2 }, { get_result, 2 },
	  { clear_commands, 1 }, { fc, 1 }, { clear_results, 1 }, { fr, 1 },
	  { set_command_history_depth, 2 }, { set_result_history_depth, 2 },
	  { clear_persistent_command_history, 1 },
	  { help, 1 } ].




% Implementation of the shell built-in commands.
%
% Beware, for them, because of erl_eval evaluation, the error report is less
% precise than usual.
%
% For example "undef (class: error)" / "*** Error: undef" is reported
% as soon as a function call *in* a command is not known (the command thus may
% be found defined).
%
% The best course of action is to test first each of these commands directly in
% shell_utils.



-doc "Lists (as terms) the current variable bindings.".
-spec list_bindings( shell_state() ) -> { shell_state(), [ binding() ] }.
list_bindings( ShellState=#shell_state{ bindings=BindingStruct } ) ->
	%trace_utils:debug( "Listing bindings." ),

	FilteredBindings = shell_utils:filter_bindings( BindingStruct ),

	{ ShellState, FilteredBindings }.


-doc "Shorthand for list_bindings/1.".
-spec b( shell_state() ) -> { shell_state(), [ binding() ] }.
b( ShellState ) ->
	list_bindings( ShellState ).



-doc "Prints on the console the current variable bindings.".
-spec print_bindings( shell_state() ) -> { shell_state(), ustring() }.
print_bindings( ShellState=#shell_state{ bindings=BindingStruct } ) ->

	Res = text_utils:format( "Shell has ~ts",
		[ shell_utils:bindings_to_command_string( BindingStruct ) ] ),

	{ ShellState, Res }.



-doc "Clears all variable bindings.".
-spec clear_bindings( shell_state() ) -> builtin_state_only().
clear_bindings( ShellState ) ->
	NewBindingStruct = erl_eval:new_bindings(),
	{ ShellState#shell_state{ bindings=NewBindingStruct }, ok }.


-doc "Shorthand for clear_bindings/1.".
-spec f( shell_state() ) -> builtin_state_only().
f( ShellState ) ->
	clear_bindings( ShellState ).


-doc """
Clears the binding of the specified variable.

No error is triggered if no such variable was bound.
""".
-spec clear_binding( shell_state(), variable_string_name() ) ->
										builtin_state_only().
clear_binding( ShellState=#shell_state{ bindings=BindingStruct },
			   VarName ) when is_list( VarName ) ->

	%trace_utils:debug_fmt( "Clearing binding '~ts'.", [ VarName ] ),

	NewBindingStruct = erl_eval:del_binding( list_to_atom( VarName ),
											 BindingStruct ),

	{ ShellState#shell_state{ bindings=NewBindingStruct }, ok };

clear_binding( _ShellState, VarName ) ->
	throw( { invalid_variable_name, VarName } ).



-doc "Shorthand for clear_bindings/2.".
-spec f( shell_state(), variable_string_name() ) -> builtin_state_only().
f( ShellState, VarName ) ->
	clear_binding( ShellState, VarName ).



-doc "Displays the current history of commands.".
-spec print_command_history( shell_state() ) -> { shell_state(), ustring() }.
print_command_history( ShellState ) ->
	Res = shell_utils:command_history_to_string_with_ids( ShellState ),
	{ ShellState, Res }.


-doc "Shorthand for print_command_history/1.".
-spec hc( shell_state() ) -> { shell_state(), ustring() }.
hc( ShellState ) ->
	print_command_history( ShellState ).


-doc "Displays the current history of results.".
-spec print_result_history( shell_state() ) -> { shell_state(), ustring() }.
print_result_history( ShellState ) ->
	Res = shell_utils:result_history_to_string_with_ids( ShellState ),
	{ ShellState, Res }.


-doc "Shorthand for print_result_history/1.".
-spec hr( shell_state() ) -> { shell_state(), ustring() }.
hr( ShellState ) ->
	print_result_history( ShellState ).



-doc """
Returns the command of the specified identifier (if it is in command history),
so that, if validated by the user, it can be evaluated again.
""".
-spec recall_command( shell_state(), command_id() ) ->
								{ shell_state(), command_str() }.
recall_command( ShellState, CmdId ) ->
	Res = shell_utils:recall_command( ShellState, CmdId ),
	{ ShellState, Res }.


-doc "Shorthand for recall_command/2.".
-spec r( shell_state(), command_id() ) -> { shell_state(), command_str() }.
r( ShellState, CmdId ) ->
	recall_command( ShellState, CmdId ).



-doc """
Returns the result corresponding to the command of specified identifier, if
still in result history, otherwise returns 'undefined'.
""".
% So the returned value is a bit ambiguous (not necessarily a past result), yet
% this is not a problem as it is only for interactive use:
%
-spec get_result( shell_state(), command_id() ) ->
								{ shell_state(), message() | command_result() }.
get_result( ShellState, CmdId ) ->
	Res = shell_utils:get_result( ShellState, CmdId ),
	{ ShellState, Res }.



-doc "Clears the full (live) history of commands.".
-spec clear_commands( shell_state() ) ->
								builtin_state_only( 'commands_cleared' ).
clear_commands( ShellState ) ->
	ClearedShellState = ShellState#shell_state{ cmd_history=queue:new() },
	{ ClearedShellState, commands_cleared }.


-doc "Shorthand for clear_commands/1.".
-spec fc( shell_state() ) -> builtin_state_only( 'commands_cleared' ).
fc( ShellState ) ->
	clear_commands( ShellState ).



-doc "Clears the full history of command results.".
-spec clear_results( shell_state() ) ->
								builtin_state_only( 'results_cleared' ).
clear_results( ShellState ) ->
	ClearedShellState = ShellState#shell_state{ res_history=queue:new() },
	{ ClearedShellState, results_cleared }.


-doc "Shorthand for clear_results/1.".
-spec fr( shell_state() ) -> builtin_state_only( 'results_cleared' ).
fr( ShellState ) ->
	clear_results( ShellState ).



-doc "Sets the depth of the command history.".
-spec set_command_history_depth( shell_state(), option( count() ) ) ->
											builtin_state_only().
set_command_history_depth( ShellState, NewDepth ) ->
	SetShellState = ShellState#shell_state{
		cmd_history_max_depth=shell_utils:check_history_depth( NewDepth ) },
	{ SetShellState, ok }.


-doc "Sets the depth of the result history.".
-spec set_result_history_depth( shell_state(), option( count() ) ) ->
											builtin_state_only().
set_result_history_depth( ShellState, NewDepth ) ->
	SetShellState = ShellState#shell_state{
		res_history_max_depth=shell_utils:check_history_depth( NewDepth ) },
	{ SetShellState, ok }.



-doc """
Clears the persistent history of commands (even if not currently activated), and
does not change its current activation status.
""".
-spec clear_persistent_command_history( shell_state() ) ->
			builtin_state_only( 'persistent_command_history_cleared' ).

clear_persistent_command_history( ShellState=#shell_state{
										cmd_history_file=undefined } ) ->

	file_utils:remove_file_if_existing(
		shell_utils:get_history_file_path() ),

	{ ShellState, persistent_command_history_cleared };


clear_persistent_command_history( ShellState=#shell_state{
										cmd_history_file=CmdHistFile } ) ->

	file_utils:close( CmdHistFile ),

	NewCmdHistFile = file_utils:open( shell_utils:get_history_file_path(),
									  _Opts=[ truncate ] ),

	NewShellState = ShellState#shell_state{ cmd_history_file=NewCmdHistFile },

	{ NewShellState, persistent_command_history_cleared }.




-doc "Displays help information.".
-spec help( shell_state() ) -> builtin_state_only().
help( ShellState=#shell_state{ reference_module=undefined } ) ->
	{ ShellState, "(no help information available)" };

help( ShellState=#shell_state{ reference_module=RefModule } ) ->
	{ ShellState, RefModule:get_help() }.
