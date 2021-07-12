% Copyright (C) 2020-2021 Olivier Boudeville
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Released as LGPL software.


% @doc An experimental (not functional yet) module to <b>explore alternate build
% systems</b>.
%
% We finally stick to our make-based build system that we found more suitable
% than (for example) rebar3.
%
% Better here than in `myriad-build.escript' to benefit from a more
% user-friendly debugging.
%
% @hidden Most empty currently.
%
-module(myriad_build).


-define( exec_name, "myriad-build.escript" ).


-export([ run/0, main/1 ]).



% @doc Typically for testing.
-spec run() -> void().
run() ->
	ArgTable = shell_utils:get_argument_table(),
	main( ArgTable ).


% Defaults:


% @doc Returns the usage information of the corresponding application.
-spec get_usage() -> void().
get_usage() ->
	text_utils:format( "Usage: ~ts MBDR_PROJECT_FILE.mbdr"
		"[-h|--help]~n"
		"  Builds specified MBDR project, where:~n"
		" - MBDR_PROJECT_FILE.mbdr is a MBDR file corresponding to the project "
		"that shall be built~n", [ ?exec_name ] ).



% @doc Sole entry point for this generation service, either triggered by `run/0'
% or by the associated escript.
%
-spec main( shell_utils:argument_table() ) -> void().
main( ArgTable ) ->

	%trace_utils:debug_fmt( "Original script-specific arguments: ~ts",
	%	[ shell_utils:argument_table_to_string( ArgTable ) ] ),

	[ %InteractiveRefKey,
	  HelpRefKey ] =
		[ %'-interactive',
		  '-help' ],

	% Standardises command-line options:
	MergedTable = list_table:merge_in_keys( [
			%{ InteractiveRefKey, [ 'i' ] },
			{ HelpRefKey, [ 'h' ] } ], ArgTable ),

	%trace_utils:debug_fmt( "Canonicalized script-specific arguments: ~ts",
	%	   [ shell_utils:argument_table_to_string( MergedTable ) ] ),

	case list_table:has_entry( HelpRefKey, MergedTable ) of

		true ->
			display_usage();

		false ->
			ok

	end,

	%{ IsInteractive, InterTable } = case
	% list_table:extract_entry_with_defaults( InteractiveRefKey,
	% _DefaultInter=false, MergedTable ) of
	%
	%	{ [], ShrunkTable } ->
	%		{ true, ShrunkTable };
	%
	%	P={ false, _ShrunkTable } ->
	%		P
	%
	%end,

	%trace_utils:debug_fmt( "Interactive: ~ts", [ IsInteractive ] ),

	%ProjectFileValue = case list_table:lookup_entry( _Key=fixme, fixme ) of

	ResultingTable = MergedTable,

	case list_table:keys( ResultingTable ) of

		[] ->
			ok;

		UnexpectedOpts ->
			trace_utils:error_fmt( "Unexpected user input: ~ts~n~ts",
			  [ shell_utils:argument_table_to_string( ResultingTable ),
				get_usage() ] ),
			throw( { unexpected_command_line_options, UnexpectedOpts } )

	end,

	trace_utils:notice( "Stopping now." ),

	basic_utils:stop( _ErrorCode=0 ).


% @doc Displays the usage of this service, and stops (with no error).
display_usage() ->
	io:format( get_usage(), [] ),
	basic_utils:stop( _ErrorCode=0 ).
