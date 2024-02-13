% Copyright (C) 2020-2024 Olivier Boudeville
%
% This file is part of the Ceylan-Traces library.
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
% Creation date: Saturday, May 9, 2020.


% @doc Module gathering various <b>trace-related facilities</b>, notably in link
% with OTP.
%
% This module should not be mixed up with the trace_utils module from Myriad
% (note that the names of these modules differ, therefore their BEAM files
% cannot clash).
%
-module(traces_utils).


% Version-related functions.
-export([ get_traces_version/0, get_traces_version_string/0 ]).


-export([ get_aggregator_registration_scope/0, get_aggregator_look_up_scope/0,
		  name_trace_file_from/1 ]).


% For trace_aggregator_name:
-include("class_TraceAggregator.hrl").


% Shorthands:

-type three_digit_version() :: basic_utils:three_digit_version().
-type module_name() :: basic_utils:module_name() .

-type ustring() :: text_utils:ustring().

-type bin_file_name() :: file_utils:bin_file_name().

-type registration_scope() :: naming_utils:registration_scope().
-type look_up_scope() :: naming_utils:look_up_scope().

-type aggregator_pid() :: class_TraceAggregator:aggregator_pid().



% Version-related functions.

% @doc Returns the version of the Traces library being used.
-spec get_traces_version() -> three_digit_version().
get_traces_version() ->
	basic_utils:parse_version( get_traces_version_string() ).


% @doc Returns the version of the Traces library being used, as a string.
-spec get_traces_version_string() -> ustring().
get_traces_version_string() ->
	% As defined (uniquely) in GNUmakevars.inc:
	?traces_version.



% @doc Returns the registration scope that applies to the trace aggregator in
% the current context.
%
-spec get_aggregator_registration_scope() -> registration_scope().
get_aggregator_registration_scope() ->

	% Possibly read from any *.config file specified (e.g. refer to the
	% INTERNAL_OPTIONS make variable):
	%
	% Supporting this not deemed useful:
	%AggRegName = case application:get_env(
	%                   trace_aggregator_registration_name ) of
	%
	%   undefined ->
	%       ?trace_aggregator_name;
	%
	%   { ok, CfgRegName } when is_atom( RegName ) ->
	%       CfgRegName;
	%
	%   { ok, InvalidRegName } ->
	%       trace_utils:error_fmt( "Invalid registration name read for the "
	%           "trace aggregator: '~p'.", [ InvalidRegName ] ),
	%       throw( { invalid_trace_aggregator_registration_name,
	%                 InvalidRegName } )
	%
	%end,

	% Specifying the application is essential, as this function is to be called
	% from any process of any other application:
	%
	case application:get_env( _Application=traces,
							  trace_aggregator_registration_scope ) of

		undefined ->
			AggRegScope = ?default_trace_aggregator_registration_scope,
			cond_utils:if_defined( traces_debug_registration,
				trace_bridge:debug_fmt( "Trace aggregator (default) scope: "
										"'~ts'.", [ AggRegScope ] ) ),
			AggRegScope;


		{ ok, CfgRegScope } when is_atom( CfgRegScope ) ->
			case naming_utils:vet_registration_scope( CfgRegScope ) of

				true ->
					cond_utils:if_defined( traces_debug_registration,
						trace_bridge:debug_fmt( "Trace aggregator scope "
							"(as configured): '~ts'.", [ CfgRegScope ] ) ),
					CfgRegScope;

				false ->
					trace_utils:error_fmt( "Invalid registration scope (type) "
						"read for the trace aggregator: '~p'.",
						[ CfgRegScope ] ),
					throw( { invalid_trace_aggregator_registration_scope,
							 CfgRegScope } )

			end;


		{ ok, InvalidRegScope } ->
			trace_utils:error_fmt( "Invalid registration scope read for the "
				"trace aggregator: '~p'.", [ InvalidRegScope ] ),
			throw( { invalid_trace_aggregator_registration_scope,
					 InvalidRegScope } )

	end.



% @doc Returns the look-up scope that applies to the trace aggregator in the
% current context.
%
-spec get_aggregator_look_up_scope() -> look_up_scope().
get_aggregator_look_up_scope() ->
	naming_utils:registration_to_look_up_scope(
		get_aggregator_registration_scope() ).



% @doc Names the trace file currently managed by the trace aggregator according
% to the specified module name. Returns, if useful, the new trace filename and
% the PID of the trace aggregator.
%
% Note: the aggregator is supposed to have been launched with the 'later'
% initial supervision setting, and will be looked-up according to our
% scope conventions.
%
% Typically useful from an OTP context, where the Traces application is started
% without being able to defined programatically the resulting trace file.
%
-spec name_trace_file_from( module_name() ) ->
								{ bin_file_name(), aggregator_pid() }.
name_trace_file_from( ModName ) ->
	AggLookupScope = get_aggregator_look_up_scope(),
	name_trace_file_from( ModName, AggLookupScope ).



% @doc Names the trace file currently managed by the trace aggregator according
% to the specified module name and scope. Returns, if useful, the new trace
% filename and the PID of the trace aggregator.
%
% Note: the aggregator is supposed to have been launched with the 'later'
% initial supervision setting.
%
% Typically useful from an OTP context, where the Traces application is started
% without being able to defined programatically the resulting trace file.
%
-spec name_trace_file_from( module_name(), look_up_scope() ) ->
									{ bin_file_name(), aggregator_pid() }.
name_trace_file_from( ModName, AggLookupScope ) ->

	NewTraceFilename = traces:get_trace_filename( ModName ),

	trace_utils:info_fmt( "Requesting the renaming of trace aggregator "
		"file to '~ts'.", [ NewTraceFilename ] ),

	NewTraceFilenameBin = text_utils:string_to_binary( NewTraceFilename ),

	AggPid = naming_utils:get_registered_pid_for(
		_Name=?trace_aggregator_name, AggLookupScope ),

	% Oneway:
	AggPid ! { renameTraceFile, NewTraceFilenameBin },

	{ NewTraceFilenameBin, AggPid }.
