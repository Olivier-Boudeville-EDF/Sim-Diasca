#!/usr/bin/env escript
-module(merge_tree).
-mode(compile).
%% -*- erlang -*-
%%! -smp enable +A 16


% Commented out: -pa ../utils


% Copyright (C) 2016-2021 Olivier Boudeville
% [olivier (dot) boudeville (at) esperide (dot) com]


% Released as LGPL software.

% Directly using the module-based version now, in merge_utils.erl, for an easier
% debugging (ex: with proper stack traces, comprising line numbers); note though
% that the first lines (with module and mode) shall already help significantly.


% This script depends on the 'Ceylan-Myriad' layer, and only on that code (that
% shall have been compiled beforehand).

% For testing, one shall prefer using merge_utils:run/0.


% For update_code_path_for_myriad/0 and all:
-include("myriad_script_include.hrl").


% Entry point of this escript.
main( ArgList ) ->

	% First, enable all possible helper code (hence to be done first of all):
	update_code_path_for_myriad(),

	ArgTable = script_utils:get_arguments( ArgList ),
	merge_utils:main( ArgTable ).
