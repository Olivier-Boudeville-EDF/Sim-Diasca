% Copyright (C) 2010-2021 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)



% Allows to generate the plots (PNG files, usually for probe reports)
% corresponding to all the time series (*.dat and *.p) files found in the
% current directory.
%
-module(generate_plots_for_time_series).


-export([ run/1 ]).


% For the file_info record:
-include_lib("kernel/include/file.hrl").


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").


-spec run( file_utils:directory_path() ) -> void().
run( Dir ) ->

	CoreCount = system_utils:get_core_count(),

	Filenames = select_data_files( Dir ),

	io:format( "~n~nGenerating over ~B cores the plots for the ~B time series "
		"found in directory '~ts': ~ts~n",
		[ CoreCount, length( Filenames ), Dir,
		  text_utils:strings_to_string( Filenames ) ] ),

	file_utils:set_current_directory( Dir ),

	% Ensures there is up to one idle worker for maximum loading:
	MaxWorkerCount = CoreCount + 1,

	% Actually gnuplot is loading in RAM the full time-series, so the host could
	% be crashed very easily as soon as there was too many gnuplot parallel
	% instances. So currently we limit the count to 1, knowing:
	%  1. we should be disk-bound, not CPU-bound anyway
	%  2. this program thus as little interest compared to a simple shell
	% command
	%  3. a single gnuplot process might exceed 12 GB of virtual RAM
	%
	%MaxWorkerCount = 1,

	manage_workers( _DataFilenames=Filenames, _CurrentWorkers=[],
		_GeneratedFilenames=[], _ReportedErrors=[], MaxWorkerCount ),

	io:format( "End of generation script.~n" ).



% Returns a list of the filenames corresponding to time series.
select_data_files( DirectoryName ) ->

	{ RegularFiles, _Symlinks, _Directories, _OtherFiles, _Devices } =
		file_utils:list_dir_elements( DirectoryName ),

	% Order allows to re-start more easily if ever a failure happened:
	lists:sort( file_utils:filter_by_extension( RegularFiles, ".dat" ) ).



manage_workers( _DataFilenames=[], _CurrentWorkers=[], _GeneratedFilenames=[],
				_ReportedErrors=[], _MaxWorkerCount ) ->
	io:format( "Nothing was to be generated.~n" );

manage_workers( _DataFilenames=[], _CurrentWorkers=[], GeneratedFilenames,
				_ReportedErrors=[], _MaxWorkerCount ) ->
	io:format( "All ~B reports were successfully generated: ~ts~n",
		[ length( GeneratedFilenames ),
		  text_utils:strings_to_string( GeneratedFilenames ) ] );

manage_workers( _DataFilenames=[], _CurrentWorkers=[], GeneratedFilenames,
				ReportedErrors, _MaxWorkerCount ) ->

	ErrorStrings = [ io_lib:format( "for ~ts: ~ts", [ File, Error ] )
					 || { File, Error } <- ReportedErrors ],

	GenLen = length( GeneratedFilenames ),
	ErrLen = length( ReportedErrors ),

	io:format( "Out of ~B reports, ~B were successfully generated: ~ts~n"
		"Whereas ~B generations failed: ~ts~n",
		[ GenLen + ErrLen, GenLen,
		  text_utils:strings_to_string( GeneratedFilenames ), ErrLen,
		  text_utils:strings_to_string( ErrorStrings ) ] );

manage_workers( DataFilenames, CurrentWorkers, GeneratedFilenames,
				ReportedErrors, MaxWorkerCount ) ->

	{ RemainingWorkers, NewGeneratedFilenames, NewReportedErrors } = receive

		{ work_done, Pid, PNGFilename } ->
			%io:format( "Worker ~w finished.~n", [ Pid ] ),
			{ lists:delete( Pid, CurrentWorkers ),
			  [ PNGFilename | GeneratedFilenames ], ReportedErrors };

		{ work_failed, Pid, Error }->
			%io:format( "Worker ~w failed: ~ts.~n", [ Pid, Reason ] ),
			{ lists:delete( Pid, CurrentWorkers ), GeneratedFilenames,
			  [ Error | ReportedErrors ] }

	after 100 ->

		% Needed to bootstrap workers:
		{ CurrentWorkers, GeneratedFilenames, ReportedErrors }

	end,

	{ NewDataFilenames, NewWorkers } =
		update_workers( DataFilenames, RemainingWorkers, MaxWorkerCount ),

	manage_workers( NewDataFilenames, NewWorkers, NewGeneratedFilenames,
					NewReportedErrors, MaxWorkerCount ).



update_workers( _DataFilenames=[], Workers, _MaxWorkerCount ) ->
	% Last workers are still working, just wait (do nothing):
	{ [], Workers };


update_workers( _DataFilenames=[ Filename | T ], Workers, MaxWorkerCount )
  when length( Workers ) < MaxWorkerCount ->

	% There is still work to be done, and room for one more worker here:

	DispatcherPid = self(),
	F = fun() ->
				manage_plot( Filename, DispatcherPid )
		end,

	NewWorkerPid = ?myriad_spawn_link( F ),
	{ T, [ NewWorkerPid | Workers ] };

update_workers( DataFilenames, Workers, MaxWorkerCount )
  when length( Workers ) =:= MaxWorkerCount ->
	{ DataFilenames, Workers }.




% The main function of workers.

manage_plot( DataFilename, DispatcherPid ) ->

	CommandFilename =
		file_utils:replace_extension( DataFilename, ".dat",	".p" ),

	case file_utils:is_existing_file( CommandFilename ) of

		true ->
			%io:format( "Command file found.~n" ),
			case generate_report( DataFilename, CommandFilename ) of

				{ success, TargetFilename } ->
					DispatcherPid ! { work_done, self(), TargetFilename };

				{ failure, Reason } ->
					DispatcherPid ! { work_failed, self(),
									  { DataFilename, Reason } }

			end;

		false ->
			Message = text_utils:format( "command file '~ts' not found",
										 [ CommandFilename ] ),

			DispatcherPid ! { work_failed, self(), { DataFilename, Message } }

	end.



% Largely inspired from class_Probe:generate_report/2.
generate_report( DataFilename, CommandFilename ) ->

	io:format( " - generating report for ~ts~n", [ DataFilename ] ),

	Command = executable_utils:get_gnuplot_path() ++ " '" ++ CommandFilename
		++ "'",

	% Gnuplot might issue non-serious warnings. Generates a PNG:
	Succeeded = case system_utils:run_command( Command ) of

		{ _ReturnCode=0, _CmdOutput=[] } ->
			true;

		{ _ReturnCode=0, CmdOutput } ->
			% Would be ?warning_fmt:
			io:format( "[warning] Report generation succeeded for ~ts, "
				"but output following information: ~p",
				[ DataFilename, CmdOutput ] ),
			true;

		{ ReturnCode, CmdOutput } ->
			% Would be ?error_fmt:
			io:format( "[error ~B] Report generated failed for ~ts and output "
				"following information: ~p",
				[ ReturnCode, DataFilename, CmdOutput ] ),
			false

	end,

	% Hack for .png:
	TargetFilename = CommandFilename ++ "ng",

	case Succeeded of

		true ->

			case file_utils:is_existing_file( TargetFilename ) of

				true ->
					{ success, DataFilename };

				false->
					{ failure, io_lib:format( "no generated file for ~ts",
											  [ DataFilename ] ) }

			end;

		false ->
			{ failure, io_lib:format( "generation failed for ~ts",
									  [ DataFilename ] ) }

	end.
