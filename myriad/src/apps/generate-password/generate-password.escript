#!/usr/bin/env escript
%% -*- erlang -*-


%% Not used: ! -pz ../../../src/utils

% Additionally: this escript will only work when run from its current
% directory...


% Copyright (C) 2018-2019 Olivier Boudeville
% [olivier (dot) boudeville (at) esperide (dot) com]


% Released as LGPL software.

% Directly using the module-based version now, for an easier debugging (ex: with
% proper stack traces, comprising line numbers).

% This script depends on the 'Myriad' layer, and only on that code (that shall
% be recompiled beforehand).


% For update_code_path_for_myriad/0 and all:
%
% (ugly hack, in link with '-pz ...' above, simply to be able to include an
% header file...)
%
-include_lib("../../../include/scripts/myriad_script_include.hrl").


% Entry point of this escript.
main( ArgList ) ->

	% First, enable all possible helper code (hence to be done first of all):
	update_code_path_for_myriad(),

	ArgTable = script_utils:get_arguments( ArgList ),

	password_generation:main( ArgTable ).
