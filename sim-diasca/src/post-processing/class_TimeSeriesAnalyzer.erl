% Copyright (C) 2011-2022 EDF R&D

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


% Analyzer tool for time series, as produced by probes, either basic or virtual.


% @doc The role of this analyzer is to <b>provide metrics and means of
% performing post-processing operations</b> on time-series produced by a
% simulation.
%
% See also class_TimeSeriesAnalyzer_test.erl.
%
-module(class_TimeSeriesAnalyzer).


-type curve_value() :: float().

-type filter_pid() :: pid().


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type file_path() :: file_utils:file_path().

-type tick() :: class_TimeManager:tick().
-type tick_offset() :: class_TimeManager:tick_offset().


% Allows to record some information about a curve in a time series.
-record( curve_metadata, {

	% The name of the curve, as a plain string:
	name :: ustring(),

	% The index of that curve in the time series (positive integer)
	index :: count(),

	% The minimum value reached by that curve, as a {MinValue,MinTick} pair,
	% MinValue being a float:
	%
	min :: { curve_value(), tick() },

	% The maximum value reached by that curve, as a {MaxValue,MaxTick} pair,
	% MaxValue being a float:
	%
	max :: { curve_value(), tick() },

	% The tick at which this curve started:
	starting_tick :: tick(),

	% The tick at which the curve stopped:
	stopping_tick :: tick(),

	% The total number of ticks defined for that curve (i.e. a tick is
	% listed, and the corresponding curve value is not 'undefined'):
	%
	tick_count :: tick_offset(),

	% A list of the PID of any optional curve-specifid curve filter
	% processes to send each {Tick,Value} samples to:
	%
	filters = [] :: [ filter_pid() ] } ).



% Allows to record some information about a time series (a set of curves).
-record( series_metadata, {

	% The name of the time series (e.g. associated probe name), as a plain
	% string:
	%
	name :: ustring(),

	% The title of the time series (ex: associated probe title), as a plain
	% string:
	%
	title :: ustring(),

	% Creation time stamp:
	creation_timestamp :: time_utils:timestamp(),

	% The tick at which the time series started:
	starting_tick :: tick(),

	% The tick at which the time series stopped:
	stopping_tick :: tick(),

	% The total number of ticks (not necessarily
	% stopping_tick - starting_tick + 1):
	%
	tick_count :: tick_offset(),

	% The list of curve meta-data (curve_metadata records):
	curves = [] :: [ #curve_metadata{} ],

	% A list of the PID of any optional series filter processes to send each
	% {Tick,Values} samples to:
	%
	filters = [] :: [ filter_pid() ] } ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_TraceEmitter ] ).


% Class-specific attributes of a time series analyzer:
-define( class_attributes, [

	{ data_filename, file_path(),
	  "the name of the time-series data file (.dat), as a plain string" },

	{ data_file, file_utils:file(), "the open file handle" },

	{ series_metadata, series_metadata(),
	  "the current version of the metadata associated to the time series" } ] ).



% Helper functions declarations.
-export([ series_to_string/1, curve_to_string/1, wait_for_filters/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "PostProcessing.TimeSeries" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").


% Implementation notes.


% Filters are processes, for example WOOPER instances, that follow at least one
% of the two API (for series or curves) below.
%
% The point is that each can keep the memory it needs of past samples,
% computations, etc. Moreover some parallel processing can be expected.



% Series filter API.
%
% A series filter is created by calling the create/3 function of its module,
% taking as parameters:
%
% - the series name (as plain string)
% - the (ordered) list of curve names (as plain strings)
% - a list of filter-specific parameters
%
% A series filter is fed for each tick defined in the data file, with the whole
% set of values defined for that tick.
%
% For each of these ticks, a {setSample,[Tick,SampleList]} message is sent by
% the analyzer to each series filter.
%
% A WOOPER-based filter will thus have its 'setSample( State, Tick, SampleList
% )' method called for each referenced tick.
%
% Once all samples have been read, a 'onEndOfSeriesData' message is sent by the
% analyzer to each series filter.
%
% Then each filter is expected to send back a {onFilterEnded,FilterPid} message
% to notify this analyzer it finished its work.
%
% Receiving the 'delete' atom should lead to the termination of the
% corresponding filter process.



% Curve filter API.
%
% A curve filter is created with two elements:
%
% - the curve name
% - a list of filter-specific parameters
%
% It is fed each time a tick is defined in the data file with a non-undefined
% value for the associated curve.
%
% For each of these tick, a {setSample,[Tick,Value]} message is sent by the
% analyzer to each curve filter.
%
% A WOOPER-based filter will thus have its 'setSample( State, Tick, Value )'
% method called.
%
% Once all samples have been read, a 'onEndOfCurveData' message is sent by the
% analyzer to each curve filter.
%
% Then each filter is expected to send back a {onFilterEnded,FilterPid} message
% to notify this analyzer it finished its work.
%
% Receiving the 'delete' atom should lead to the termination of the
% corresponding filter process.




% A filter specification (FilterSpec) is a {FilterName,FilterParameters} pair,
% with FilterName being the name of the module implementing that filter (ex:
% 'curve_selector_series_filter') and FilterParameters is either a single
% parameter or a list of parameters, to be used for the creation of that filter.
%
-type filter_name() :: atom().

% To be defined:
-type filter_parameter() :: any().

-type filter_parameters() :: filter_parameter() | [ filter_parameter() ].

-type filter_spec() :: { filter_name(),filter_parameters() }.




% @doc Constructs a time-series analyzer, from following parameters:
%
% - TimeSeriesFilename is the name of the time-series data file (generally
% *.dat), as a plain string (ex: "time_series_test.dat")
%
% - SeriesFilters is a list of FilterSpecs, to create filters that will be
% associated to the time series as a whole, thus operating on each full data
% tuple of each defined tick
%
% - CommonCurveFilters is a list of FilterSpec, to create filters that will be
% associated to each of the curves in that time series (each curve will rely on
% a filter instance per filter defined in the list)
%
% - CurveSpecificFilters is a list of {CurveName,FilterSpecList}, where
% FilterSpecList is a list of the FilterSpecs that correspond to the specific
% filters to be created for that corresponding curve
%
-spec construct( wooper:state(), file_path(), [ filter_spec() ],
				[ filter_spec() ], [ filter_spec() ] ) -> wooper:state().
construct( State, TimeSeriesFilename,
		   SeriesFilters, CommonCurveFilters, CurveSpecificFilters ) ->

	%trace_utils:debug_fmt( "Analyzing the time series in the '~ts' data file.",
	%                       [ TimeSeriesFilename ] ),

	BaseState = class_EngineBaseObject:construct( State,
						?trace_categorize("Time Series Analyzer") ),

	DataFile = open_data_file( TimeSeriesFilename, BaseState ),

	AllFilters = { SeriesFilters, CommonCurveFilters, CurveSpecificFilters },

	ReadSeries = parse_data_file( DataFile, AllFilters ),

	setAttributes( BaseState, [
		{ data_filename, TimeSeriesFilename },
		{ data_file, DataFile },
		{ series_metadata, ReadSeries } ] ).



% @doc Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state() .
destruct( State ) ->

	% Class-specific actions:
	?info( "Deleting time-series analyzer." ),

	% Not synchronous:
	[ F ! delete || F <- get_all_curve_filters( ?getAttr(series_metadata) ) ],

	case ?getAttr(data_file) of

		undefined ->
			ok;

		File ->
			close_data_file( File )

	end,

	?debug( "Time-series analyzer deleted." ),

	% Then allow chaining:
	State.



% Helper section.


% @doc Opens specified data file and prepares it for analysis.  Returns a file
% handle.
%
open_data_file( TimeSeriesFilename, State ) ->

	?notice_fmt( "Opening data file '~ts'.", [ TimeSeriesFilename ] ),

	case file_utils:is_existing_file( TimeSeriesFilename ) of

		false ->
			throw( { data_file_not_found, TimeSeriesFilename } );

		_ ->
			ok
	end,

	% Large data may have to be read (chunks of 512 kB):
	ReadOptions = [ read, raw, { read_ahead, _Size=512*1024 } ],

	file_utils:open( TimeSeriesFilename, ReadOptions ).


% @doc Closes the specified data file.
close_data_file( File ) ->
	file_utils:close( File ).



% @doc Parses the specified data file.
parse_data_file( File,
			{ SeriesFilters, CommonCurveFilters, CurveSpecificFilters } ) ->

	VanillaSeriesMetadata = parse_header( File ),

	%trace_utils:debug_fmt( "After having parsed the header:~n~ts",
	%                       [ series_to_string( VanillaSeriesMetadata ) ] ),

	SeriesFilterMetadata = add_series_filters( SeriesFilters,
											  VanillaSeriesMetadata ),

	CommonSeriesMetadata = add_common_curve_filters( CommonCurveFilters,
													 SeriesFilterMetadata ),

	SpecificSeriesMetadata = add_specific_curve_filters( CurveSpecificFilters,
														 CommonSeriesMetadata ),

	%trace_utils:debug_fmt( "After having added filters:~n~ts",
	%                       [ series_to_string( SpecificSeriesMetadata ) ] ),

	SampledMetadata = read_samples( File, SpecificSeriesMetadata ),

	%trace_utils:debug_fmt( "After having read the samples:~n~ts",
	%                       [ series_to_string( SampledMetadata ) ] ),

	SampledMetadata.



% @doc Parses the header of the data file, and returns a series metadata record.
parse_header( File ) ->

	Header = read_header( File ),
	%trace_utils:debug_fmt( "Initial header is: ~p.", [ Header ] ),

	WarningLines = jump_warning( Header ),
	%trace_utils:debug_fmt( "Warning-stripped header is: ~p.",
	%                       [ WarningLines ] ),

	{ Timestamp, TimeLines } = extract_write_timing( WarningLines ),
	%trace_utils:debug_fmt( "Timestamp: ~p, next lines: ~p.",
	%                       [ Timestamp, TimeLines ] ),

	{ ProbeName, ProbeTitle, ProbeLines } = extract_probe_info( TimeLines ),

	%trace_utils:debug_fmt(
	%   "Probe is named '~ts', its title is '~ts', next lines: ~p.",
	%   [ ProbeName, ProbeTitle, ProbeLines ] ),

	{ CurveMetadataList, _CurveLines=[] } = extract_curves_info( ProbeLines ),
	%trace_utils:debug_fmt( "Curve metadata list is ~p, next lines: ~p.",
	%                       [ CurveMetadataList, CurveLines ] ),

	#series_metadata{ name=ProbeName,
					  title=ProbeTitle,
					  creation_timestamp=Timestamp,
					  curves=CurveMetadataList }.



% @doc Parses header of specified file and returns a series_metadata record.
read_header( File ) ->
	{ ok, ReadLine } = file:read_line( File ),
	read_header( File, ReadLine, _Acc=[] ).


% Reads header, defined by all text before next empty line.
read_header( _File, _ReadLine="\n", Acc ) ->
	lists:reverse( Acc );

read_header( File, ReadLine, Acc ) ->
	{ ok, NewReadLine } = file:read_line( File ),
	%trace_utils:debug_fmt( "Read line: '~ts'.", [ NewReadLine ] ),
	read_header( File, NewReadLine, [ ReadLine | Acc ] ).



% @doc Jumps over that warning:
% # Warning: using immediate writes here, thus...
% # should subsequent curve reordering or addition...
%
jump_warning( [ "# Warning: using immediate writes here" ++ _ | T ] ) ->
	jump_warning( T );

jump_warning( [ "# should subsequent curve reordering" ++ _ | T ] ) ->
	jump_warning( T );

jump_warning( Any ) ->
	Any.



% @doc Extracts time and data of writing from header.
%
% Returns {{Year,Month,Day}, {Hour,Minute,Second}, RemainingLines}.
%
extract_write_timing( [ "# This time series data file has been written on "
						++ TimeText | T ] ) ->

	% TimeText is like L="14/4/2011, at 18:48:51.\n".

	% The objective here is to set it to a canonical form so that the timestamp
	% can be directly extracted:
	% string:tokens(L, " ") returns: ["14/4/2011,","at","18:48:51.\n"]
	%
	SplitTimestamp = lists:flatten( text_utils:format( "~ts",
								[ re:replace( TimeText, ", at ", " " ) ] ) ),

	CanonicalTimestamp = text_utils:remove_last_characters( SplitTimestamp,
															_Count=2 ),

	Timestamp = time_utils:string_to_timestamp( CanonicalTimestamp ),

	{ Timestamp, _RemainingLines=T }.



% @doc Extracts probe name and title, and advances until curve list.
%
% Returns { ProbeName, ProbeTitle, NextLines }.
%
extract_probe_info( [ "# Probe name: "  ++ Name,
					  "# Probe title: " ++ Title,
					  "# First column " ++ _,
					  "# Next columns"  ++ _ | NextLines ] ) ->

	ProbeName = text_utils:remove_ending_carriage_return( Name ),

	ProbeTitle = text_utils:remove_ending_carriage_return( Title ),

	{ ProbeName, ProbeTitle, NextLines }.



% @doc Extracts the information for all curves, returns a {CurveMetadataList,
% NextLines} pair where CurveMetadataList is a list of curve_metadata records.
%
extract_curves_info( Lines ) ->
	extract_curves_info( Lines, _Acc=[] ).

extract_curves_info( [ "# - curve #" ++ CurveInfoString | T ], Acc ) ->

	SepIndex = string:chr( CurveInfoString, $: ),

	CurveCount = text_utils:string_to_integer(
							string:substr( CurveInfoString, 1, SepIndex-1 ) ),

	CurveName = text_utils:remove_ending_carriage_return(
						string:substr( CurveInfoString, SepIndex + 2 ) ),

	NewCurveMetadata = #curve_metadata{ name=CurveName,
										index=CurveCount },

	extract_curves_info( T, [ NewCurveMetadata | Acc ] );

extract_curves_info( NextLines, Acc ) ->
	% We prefer having the curve in the declaration order:
	{ lists:reverse( Acc ), NextLines }.



% @doc Returns a textual representation of the specified time-series metadata.
-spec series_to_string( #series_metadata{} ) -> ustring().
series_to_string( #series_metadata{	name=Name,
									title=Title,
									creation_timestamp=Timestamp,
									starting_tick=StartTick,
									stopping_tick=StopTick,
									tick_count=TickCount,
									curves=Curves,
									filters=Filters } ) ->

	FilterString = case Filters of

		[] ->
			"No series filter defined.";

		_ ->
			text_utils:format( "Following time-series filters were defined: "
							   "~p.", [ Filters ] )

	end,

	CurveDescriptions = [ curve_to_string( C ) ++ "\n" || C <- Curves ],

	CurveCount = length( Curves ),

	text_utils:format( "Time series named '~ts', whose title is '~ts', "
		"created on ~ts."
		" Its first referenced tick is ~p, is last one is ~p, "
		"for a total of ~p listed ticks. " ++ FilterString
		++ " Following ~B curves were defined:~n~ts",
		[ Name, Title, time_utils:timestamp_to_string( Timestamp ),
		  StartTick, StopTick, TickCount, CurveCount,
		  text_utils:strings_to_string( CurveDescriptions ) ] ).



% @doc Returns a textual representation of the specified curve metadata.
-spec curve_to_string( #curve_metadata{} ) -> ustring().
curve_to_string( #curve_metadata{ name=Name,
								  index=Index,
								  min=Min,
								  max=Max,
								  filters=Filters,
								  starting_tick=StartTick,
								  stopping_tick=StopTick,
								  tick_count=TickCount } ) ->

	MinString = case Min of

		undefined ->
			[];

		{ MinValue, MinTick } ->
			text_utils:format( " It reached its minimum, ~f, at tick #~B.",
							   [ MinValue, MinTick ] )

	end,


	MaxString = case Max of

		undefined ->
			[];

		{ MaxValue, MaxTick } ->
			text_utils:format( " It reached its maximum, ~f, at tick #~B.",
							   [ MaxValue, MaxTick ] )

	end,


	FilterString = case Filters of

		[] ->
			" Not associated to any curve filter";

		_ ->
			text_utils:format( " Associated to following curve filters: ~w",
							   [ Filters ] )

	end,

	text_utils:format( "curve named '~ts', whose curve index is ~B. "
		"Its first referenced tick is ~p, is last one is ~p, "
		"for a total of ~p listed measures.",
		[ Name, Index, StartTick, StopTick, TickCount ] )
					++ MinString ++ MaxString ++ FilterString.



% @doc Reads all samples from specified file, and updates accordingly the
% metadata of the series.
%
read_samples( File, SeriesMetadata ) ->

	case file:read_line( File ) of

		{ ok, ReadLine } ->

			%trace_utils:debug( "Read a line" ),

			NewSeriesMetadata =
				update_series( SeriesMetadata, parse_line( ReadLine ) ),

			%trace_utils:debug( "Now reading samples" ),
			read_samples( File, NewSeriesMetadata );


		eof ->

			SeriesFilters = SeriesMetadata#series_metadata.filters,
			CurveFilters = get_all_curve_filters( SeriesMetadata ),

			% Faster, but outputs may happen in any particular order:

			%[ CurveF ! {onEndOfCurveData,self()} || CurveF <- CurveFilters ],
			%[ SeriesF ! {onEndOfSeriesData,self()}
			%  || SeriesF <- SeriesFilters ],
			%wait_for_filters( CurveFilters ++ SeriesFilters ),

			[ begin F ! { onEndOfSeriesData, self() },
				receive { onFilterEnded, F } -> ok end
			  end || F <- SeriesFilters ],

			[ begin F ! { onEndOfCurveData, self() },
				receive { onFilterEnded, F } -> ok end
			  end || F <- CurveFilters ],

			SeriesMetadata;


		{ error, Reason } ->
			throw( { error_reading_samples, Reason } )

	end.



% @doc Updates the series with the information read from this new line.
update_series( SeriesMetadata, { Tick, Values } ) ->

	%trace_utils:debug_fmt( "update_series for ~p", [ SeriesMetadata ] ),

	% Parallelizing for the overall series, hopefully with no mailbox explosion
	% of the filters:
	%
	[ SeriesF ! { setSample, [ Tick, Values ] } ||
					SeriesF <- SeriesMetadata#series_metadata.filters ],

	% Parallelizing for each curve, hopefully with no mailbox explosion of the
	% filters:
	%
	send_to_curve_filters( Tick, Values,
						   SeriesMetadata#series_metadata.curves ),

	% Samples could be unordered:

	StartTick = case SeriesMetadata#series_metadata.starting_tick of

		undefined ->
			Tick;

		StartT when StartT > Tick ->
			Tick;

		StartT ->
			StartT

	end,


	StopTick = case SeriesMetadata#series_metadata.stopping_tick of

		undefined ->
			Tick;

		StopT when StopT < Tick ->
			Tick;

		StopT ->
			StopT

	end,


	TickCount = case SeriesMetadata#series_metadata.tick_count of

		undefined ->
			1;

		T ->
			T+1

	end,

	NewCurves = update_curves( SeriesMetadata#series_metadata.curves, Values,
							   Tick ),

	SeriesMetadata#series_metadata{ starting_tick=StartTick,
									stopping_tick=StopTick,
									tick_count=TickCount,
									curves=NewCurves } .



% @doc Parses a sample line, ex: "6 0 7 0 1.0 0 0", and returns a
% {Tick, FloatList} pair.
%
parse_line( Line ) ->

	CleanedLine = text_utils:remove_ending_carriage_return(Line),

	[ StringTick | StringValues ] = string:tokens( CleanedLine, " " ),
	Tick = text_utils:string_to_integer( StringTick ),

	ParseFun = fun( V ) ->
		case V of

			   "undefined" ->
				   undefined;

			   _ ->
				   text_utils:string_to_float( V )

		end
	end,

	Values = [ ParseFun( S ) || S <- StringValues ],

	%trace_utils:debug( "Line parsed" ),

	{ Tick, Values }.



% @doc Updates the curve metadata according to specified values.
update_curves( CurvesMetadata, Values, Tick ) ->

	%trace_utils:debug_fmt( "Updating curves ~p at tick #~B with values ~p.",
	%                       [ CurvesMetadata, Tick, Values ] ),

	update_curves( CurvesMetadata, Values, Tick, _CurveAcc=[] ).



% (helper)
update_curves( _CurvesMetadata=[], _Values=[], _Tick, CurveAcc ) ->
	% Preserve curve order for next call:
	lists:reverse( CurveAcc );

update_curves( _CurvesMetadata=[ C | Curves ], _Values=[ _V=undefined | T ],
			   Tick, CurveAcc ) ->

	% Simply ignore for that curve any sample set to 'undefined':
	update_curves( Curves, T, Tick, [ C | CurveAcc ] );

update_curves( _CurvesMetadata=[ C=#curve_metadata{
									min=MinE, max=MaxE,
									starting_tick=StartTick,
									stopping_tick=StopTick,
									tick_count=TickCount } | Curves ],
			   _Values=[ V | T ], Tick, CurveAcc ) ->

	% Here V is defined.

	% Attempt of parallelizing:
	%
	% (note that if a filter happens to be slower than this reader process -
	% which should be unlikely, its mailbox may explode over time)


	NewStartTick = case StartTick of

		undefined ->
			Tick;

		StartT when StartT > Tick ->
			Tick;

		StartT ->
			StartT

	end,


	NewStopTick = case StopTick of

		undefined ->
			Tick;

		StopT when StopT < Tick ->
			Tick;

		StopT ->
			StopT

	end,


	NewTickCount = case TickCount of

		undefined ->
			1;

		_ ->
			TickCount + 1

	end,

	NewMin = case MinE of

		undefined ->
			{ V, Tick };

		{ MinValue, _MinTick } when MinValue > V ->
			{ V, Tick };

		_ ->
			MinE

	end,

	NewMax = case MaxE of

		undefined ->
			{ V, Tick };

		{ MaxValue, _MaxTick } when MaxValue < V ->
			{ V, Tick };

		_ ->
			MaxE

	end,

	NewCurve = C#curve_metadata{ min=NewMin,
								 max=NewMax,
								 starting_tick=NewStartTick,
								 stopping_tick=NewStopTick,
								 tick_count=NewTickCount },

   update_curves( Curves, T, Tick, [ NewCurve | CurveAcc ] ).




% @doc Returns the list of the PID of all curve filters involved in the time
% series.
%
get_all_curve_filters( SeriesMetadata ) ->
	CurvesMetadata = SeriesMetadata#series_metadata.curves,
	get_all_curve_filters( CurvesMetadata, _Acc=[] ).


% (helper)
get_all_curve_filters( _CurvesMetadata=[], Acc ) ->
	lists:reverse( Acc );

get_all_curve_filters( _CurvesMetadata=[ C | T ], Acc ) ->
	get_all_curve_filters( T, C#curve_metadata.filters ++ Acc ).



% @doc Returns the list of all curve names for that series.
get_all_curve_names( SeriesMetadata ) ->
	Curves = SeriesMetadata#series_metadata.curves,
	[ C#curve_metadata.name || C <- Curves ].



% @doc Waits for filters to finish their task.
%
% Use basic_utils:wait_for_acks/4 instead.
%
-spec wait_for_filters( [ pid() ] ) -> void().
wait_for_filters( _Filters=[] ) ->
	ok;

wait_for_filters( Filters ) ->

	receive

		{ onFilterEnded, FilterPid } ->
			NewFilters = lists:delete( FilterPid, Filters ),
			wait_for_filters( NewFilters )

	end.



% @doc Adds specified series filters to the series metadata.
add_series_filters( FilterSpecs, SeriesMetadata ) ->

	CurveNameList = get_all_curve_names( SeriesMetadata ),

	SeriesName = SeriesMetadata#series_metadata.name,

	NewFilters = SeriesMetadata#series_metadata.filters
		++ [ create_series_filter( F, CurveNameList, SeriesName )
				|| F <- FilterSpecs ],

	SeriesMetadata#series_metadata{ filters=NewFilters }.



% @doc Adds specified common curve filters to the series metadata.
%
% Registers an instance of each filter described in FilterSpecs (a list of
% {FilterName, FilterParameters} elements) in each curve metadata, and returns
% the overall series metadata.
%
add_common_curve_filters( FilterSpecs, SeriesMetadata ) ->

	NewCurveMetadataList = add_common_curve_filters( FilterSpecs,
					SeriesMetadata#series_metadata.curves, _Acc=[] ),

	SeriesMetadata#series_metadata{
						% Preserve law and order:
						curves=lists:reverse( NewCurveMetadataList ) }.


add_common_curve_filters( _FilterSpecs, _CurveMetadataList=[], Acc ) ->
	Acc;

add_common_curve_filters( FilterSpecs, _CurveMetadataList=[ C | T ], Acc ) ->

	NewFilters = C#curve_metadata.filters ++
		[ create_curve_filter( FSpec, C#curve_metadata.name )
			|| FSpec <- FilterSpecs ],

	add_common_curve_filters( FilterSpecs, T,
					[ C#curve_metadata{ filters=NewFilters } | Acc ] ).



% @doc Adds specified curve-specific filters to the series metadata.
%
% Registers for all each specified curve name the associated list of filters,
% nased on their FilterSpecs, and returns the overall series metadata.
%
add_specific_curve_filters( _CurveSpecificFilters=[], SeriesMetadata ) ->
	SeriesMetadata;

add_specific_curve_filters(
		_CurveSpecificFilters=[ { _CurveName, _FilterSpecList } | _T ],
		SeriesMetadata ) ->
	throw( not_implemented_yet ),
	SeriesMetadata.



% @doc Returns the PID of a new instance of a series filter, created according
% specified parameters.
%
create_series_filter( _FilterSpec={FilterName,FilterParameters}, CurveNameList,
					  SeriesName ) ->

	?myriad_spawn_link( _Mod=FilterName, _Fun=create,
						_Args=[ SeriesName, CurveNameList,
								ensure_list( FilterParameters ) ] ).



% @doc Returns the PID of a new instance of a curve filter, created according
% specified parameters.
%
create_curve_filter( _FilterSpec={ FilterName, FilterParameters },
					 CurveName ) ->

	?myriad_spawn_link( _Mod=FilterName, _Fun=create,
						_Args=[ CurveName, ensure_list( FilterParameters ) ] ).



% @doc Ensures specified parameter is returned as a list.
ensure_list( L ) when is_list( L ) ->
	L;

ensure_list( Other ) ->
	[ Other ].



% @doc Sends, if appropriate, a new value to each filter of each curve.
send_to_curve_filters( _Tick, _Values=[], _CurveMetadataList=[] ) ->
	ok;

send_to_curve_filters( Tick, _Values=[ undefined | Tv ],
					   _CurveMetadataList=[ _C | Tc ] ) ->
	% Skip undefined:
	send_to_curve_filters( Tick, Tv, Tc );

send_to_curve_filters( Tick, _Values=[ V | Tv ],
					   _CurveMetadataList=[ C | Tc ] ) ->
	[ F ! { setSample, [ Tick, V ] } || F <- C#curve_metadata.filters ],
	send_to_curve_filters( Tick, Tv, Tc ).
