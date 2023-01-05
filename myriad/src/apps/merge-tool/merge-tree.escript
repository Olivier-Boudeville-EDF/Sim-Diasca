#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable +A 16

-module(merge_tree_mod).

-mode(compile).


% @doc Prefer using directly the `merge.sh' script; this escript is mostly
% obsolete.

% Not used: -pz ../../../src/utils

% Additionally: this escript will only work when run from its current
% directory...


% Copyright (C) 2016-2022 Olivier Boudeville
% [olivier (dot) boudeville (at) esperide (dot) com]


% Released as LGPL software.

% Directly using the module-based version now, in merge_utils.erl, for an easier
% debugging (e.g. with proper stack traces, comprising line numbers); note
% though that the first lines (with module and mode) shall already help
% significantly.


% This script depends on the 'Ceylan-Myriad' layer, and only on that code (that
% shall have been compiled beforehand).

% For testing, one shall prefer using merge_utils:run/0.

% For update_code_path_for_myriad/0 and all:
%
% (ugly hack, in link with '-pz ...' above, simply to be able to include an
% header file...)
%
-include_lib("../../../include/scripts/myriad_script_include.hrl").


% Entry point of this escript.
main( ArgList ) ->

	% First, enable all possible helper code (hence to be done first of all):
	_MyriadRootDir = update_code_path_for_myriad(),

	% To be compliant with the main way of running this merge tool (see
	% merge_app.erl), the arguments specified for this escript (e.g. 'foo' and
	% 'bar') should be moved here from being (a list of) option-less arguments
	% to arguments of the -base-dir option, just after its associated path (so
	% as if run with: '--base-dir SOME_PATH/myriad foo bar'):
	%
	ArgTable = script_utils:get_arguments( ArgList ),

	%trace_utils:debug_fmt( "Run as escript: ~ts",
	%  [ shell_utils:argument_table_to_string( ArgTable ) ] ),

	merge_utils:main( ArgTable ).
