#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable +A 16

-module(password_generation_mod).

-mode(compile).


% @doc Prefer using directly the `generate-password.sh' script.

% Not used: -pz ../../../src/utils

% Additionally: this escript will only work when run from its current
% directory...


% Copyright (C) 2018-2022 Olivier Boudeville
% [olivier (dot) boudeville (at) esperide (dot) com]


% Released as LGPL software.

% Directly using the module-based version now, in password_generation.erl, for
% an easier debugging (e.g. with proper stack traces, comprising line numbers);
% note though that the first lines (with module and mode) shall already help
% significantly.


% This script depends on the 'Myriad' layer, and only on that code (that shall
% be recompiled beforehand).


% For update_code_path_for_myriad/0 and all:
%
% (ugly hack, in link with '-pz ...' above, simply to be able to include an
% header file...)
%
-include_lib("../../../include/scripts/myriad_script_include.hrl").


% @doc Entry point of this escript.
main( ArgList ) ->

	% First, enable all possible helper code (hence to be done first of all):
	_MyriadRootDir = update_code_path_for_myriad(),

	ArgTable = script_utils:get_arguments( ArgList ),

	password_generation:main( ArgTable ).
