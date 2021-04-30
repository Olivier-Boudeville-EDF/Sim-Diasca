% Copyright (C) 2020-2021 Olivier Boudeville
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


% Module gathering various trace-related facitilies, notably in link with OTP.
%
% Note: not to be mixed up with the trace_utils module from Myriad.
%
-module(traces_utils).


-export([ name_trace_file_from/1 ]).


% For trace_aggregator_name:
-include("class_TraceAggregator.hrl").


% Names the trace file currently managed by the trace aggregator according to
% specified name.
%
% Note: the aggregator is supposed to have been launched with the 'later'
% initial supervision setting.
%
% Typically useful from an OTP context, where the Traces application is started
% without being able to defined programatically the resulting trace file.
%
-spec name_trace_file_from( basic_utils:module_name() ) -> void().
name_trace_file_from( ModName ) ->

	NewTraceFilename = traces:get_trace_filename( ModName ),

	trace_utils:info_fmt( "Requesting the renaming of trace aggregator "
		"file to '~ts'.", [ NewTraceFilename ] ),

	BinNewTraceFilename = text_utils:string_to_binary( NewTraceFilename ),

	AggPid = naming_utils:get_registered_pid_for(
				_Name=?trace_aggregator_name, _Scope=global ),

	% Oneway:
	AggPid ! { renameTraceFile, BinNewTraceFilename }.
