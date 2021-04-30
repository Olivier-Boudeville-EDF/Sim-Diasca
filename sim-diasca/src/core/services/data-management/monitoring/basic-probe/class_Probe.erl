% Copyright (C) 2008-2021 EDF R&D

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


-module(class_Probe).


-define( class_description,
		 "Basic probe class, in charge of generating results." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ResultProducer ] ).


% See class_Probe_test.erl
%
% See http://www.gnuplot.info/docs/gnuplot.html for graph generation.
%
% Needs:
%
% - gnuplot version 4.2 or higher; see comments in get_probe_settings/4
% (key_options) to support older gnuplot versions
%
% - an image viewer, eog (eye of gnome); see the executable_utils module for
% viewers



% A (basic, plain) probe aggregates a series of values for a series of ticks and
% generates an appropriate data file for gnuplot.
%
% Note: ticks are expected to arrive correctly ordered (increasing timestamps),
% although gnuplot seems able to overcome it.
%
% A probe named 'Test probe' will result in the creation of two files:
%
% - Test_probe.p, with the relevant gnuplot commands
%
% - Test_probe.dat, with the corresponding probe data
%
% The writing of the received data samples can be either performed on-the-fly or
% deferred until a report generation is required.
%
% Note that not storing data in RAM (i.e. writing them directly on-file, which
% is the default, see the deferred_data_writes field of the probe_options
% record) allows to reduce the memory footprint.
%
% On the other hand, enabling a deferred writing of the command file allows,
% regardless of the writing of the data being itself deferred or not, to support
% the dynamic addition of new columns: additional curves and zones may be
% declared, provided that the report generation has not been requested yet.
%
% For any number of ticks, no sample at all can be sent (i.e. reported values
% may no be consecutive), and partial samples can be sent (use the atom
% 'undefined' to specify that a given data element has no known value).
%
% Maybe in the future the possibility of merging samples could be supported (as
% it is already the case with the datalogger), if deferred_data_writes is
% true. For example, if for the same tick samples S1={1, undefined, 3} and
% S2={undefined, 2, undefined} were sent, then the probe would store S={1, 2,
% 3}.
%
% The rule would be that only undefined elements could be overridden. For
% example S3={1, 2, undefined} could not be merged with S1 because they both
% defined their first element (even if it is with the same value).

% Note that samples are tuples (ex: Sample={2, 1, undefined, 4}), even if there
% is only one curve (ex: Sample={7}, not Sample=7).

% Based on the enabled_producer attribute, a probe could decide not to perform
% anything if deactivated, to avoid wasting resources.

% If the data writes are not deferred, the header will be written directly at
% probe creation, thus any subsequent change (like addition of a curve, curves
% reordering, etc.) will not be taken in account in such a probe based on an
% already-written header.

% In some cases, no sample data will be sent to the probe, whereas it will be
% requested a report (rendering). In this case we chose not to send any PNG file
% (it would be empty), yet, if requested, we will send the data file (*.dat), as
% it will not contain data, but it will provide at least metadata.


-type curve_count() :: count().


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type title() :: text_utils:title().
-type label() :: text_utils:label().

-type file_name() :: file_utils:file_name().
-type directory_path() :: file_utils:directory_path().
-type any_directory_path() :: file_utils:any_directory_path().
-type file() :: file_utils:file().


-type point() :: gui:point().
-type color() :: gui_color:color().
-type length() :: gui:length().
-type coordinate() :: gui:coordinate().

-type virtual_seconds() :: class_TimeManager:virtual_seconds().



% Probes can be used as result producers (the usual case) or, in some specific
% cases, as mere technical facilities (ex: for the tracking of the simulation
% performance).
%
% In the first case, they will be declared either from a actor (then
% class_Actor:declare_result_probe/{6,7,8} should be used for that) or directly
% from the simulation test case (then
% class_Probe:declare_{test,case}_probe/{6,8} should be used; these two latter
% forms are synonyms). In all these situations, they will be managed as results:
% created iff being requested results, and if yes they will be created in the
% directory for temporary data (typically '/tmp') and later retrieved iff the
% simulation succeeds. In terms of life cycle, such tracked basic probes are
% owned by the result manager (which will thus delete them appropriately).
%
% In the second case, probes are not result producers, just logging facilities,
% and they should be created (not declared, as their creation will not be
% decided upon by the result manager) thanks to
% class_Probe:create_facility_probe/7. In this case their files will be directly
% written into the specified directory so that, in case of crash or simulation
% failure, their files will linger there, on the local computer, already in the
% appropriate target directory.
%
% Their creator keeps their ownership and thus is responsible for their
% deallocation.



% Attributes that are specific to a (basic) probe instance are:
-define( class_attributes, [

  { settings, probe_settings(), "settings applying to that probe" },

  { command_file_up_to_date, boolean(), "tells whether the command file is "
	"considered as 'clean', i.e. as existing and up-to-date" },

  { deferred_data_writes, boolean(), "tells whether the data should be stored "
	"in memory and written just before generating the report (if true; thus "
	"increasing the memory footprint and not surviving a crash) or written "
	"over time as sample data is sent (if false; thus involving slower I/O "
	"and many writings)" },

  { is_tracked_producer, boolean(), "tells whether this probe is a tracked "
	"result producer, i.e. a producer which will be requested by the result "
	"manager to return actual simulation results" },

  { probe_dir, bin_directory_path(), "corresponds to the directory "
	"where the relevant probe files will be written; by default, it is the "
	"current working directory" },

  { curve_count, curve_count(), "the number of curves carrying the "
	"data of the probe, corresponding to the number of sample data this probe "
	"is to be fed with (cached, precomputed value)" },

  { curve_entries, [ curve_entry() ],
	"an ordered list of {CurveIndex, BinCurveName, BinPlotSuffix} triplets, "
	"with CurveIndex keeping track of the order according to which the curves "
	"were declared and fed (so that, prior to generating a report, curves can "
	"be reordered while being still associated to their values), and with "
	"curve names being binaries; the order in this list dictates the actual "
	"rendering order of curves that will be performed" },

  { zone_entries, [ zone_definition() ], "a list of {BinZoneName, "
	"{ExtendedCurveName1, ExtendedCurveName2}} entries" },

  { tick_offset, probe_tick(), "a value, by default set to zero, that will "
	"be subtracted to all the ticks sent with sample data; for example, if "
	"tick_offset is set to 1000 and samples are received for (supposedly "
	"absolute) ticks 1005 and 1007, then the corresponding samples will be "
	"associated to abscissas 5 and 7; this allows for example to be able to "
	"rely on tick offsets rather than simulation absolute ticks, which would "
	"be generally a lot larger - thus more difficult to interpret; note: we "
	"could have defined a system which would, when the first sample is "
	"received, store this tick and subtract it from all the next sample "
	"ticks; however this is not what is generally wanted, as the origin of "
	"time would then be probe-specific, whereas we want generally to use the "
	"a common origin (typically the simulation start tick); as the probe "
	"cannot guess it, a call to setTickOffset/2 seems necessary" },

  { maybe_tick_duration, maybe( virtual_seconds() ),
	"the actual duration, in floating-point seconds (in virtual time), "
	"between two simulation ticks (allows to better label the abscissa "
	"axis with actual timestamps rather than mere ticks)" },

  { sample_count, count(), "keeps track of the number of samples "
	"received (ex: useful not to attempt to generate a rendering if none was "
	"received)" },

  { data_table, [ { timestamp_bin_string(), sample_data() } ],
	"records the sample data this probe was fed with; "
	"it is not a table because gnuplot may prefer "
	"that the rows are ordered, and immediate writing might be requested. "
	"It is an ordered list (in reverse chronological order, as new samples "
	"are added at the head) that contains {TickOffset, Samples} entries, "
	"with Samples being a tuple whose size can increase over time, if "
	"updateCurveInformation/2 is called before the report generation (using "
	"default settings for graph rendering)" },

  { data_filename, text_utils:binary(), "path of the probe data file; it is "
	"a complete path (including the probe directory), stored as a binary" },

  { data_file, maybe( file() ),
	"the file object (if any) in which sample data is written" },

  { row_format_string, maybe( ustring() ),
	"a precomputed format string (if any) used to write new samples" },

  { gnuplot_version, basic_utils:two_digit_version(), "version of gnuplot "
	"that will be used on this computer during this simulation" },

  { meta_data, class_ResultManager:meta_data(), "corresponds to the meta-data "
	"to be added in probe-generated data files" } ] ).



% Probe serialisation.
%
% The state of a probe is written in a serialisation stream (file) that way:
%
% - 16-bit header telling that the following content corresponds to a probe
% (type id corresponding to ?serialised_probe_instance)
%
% - 32-bit unsigned integer telling on how many bytes the binary corresponding
% to the full state of the probe, its command file (if any) and its data file
% (if any) is spreading afterwards
%
% - then this binary itself (corresponding to a {Classname, AttributeEntries,
% BinCommand, BinData} tuple)
%
% Probes thus need specific hooks to manage the information (command, data) that
% is not stored directly in their state.
%
% See: the serialisation hooks.



% Helper functions.
-export([ wait_result_declaration_outcome/2, get_probe_settings/4,
		  format_row/4, write_row/3, write_row/4 ]).



% Export to share code with the datalogger and al:
-export([ transform_curve_names/1, transform_declared_zones/2,
		  add_probe_index_back/2, get_command_filename/1,
		  generate_command_file/6, get_data_filename/1, get_report_filename/1,
		  get_plot_command/5, check_probe_directory/1, get_basic_options/1,
		  get_label_definitions/1, get_x_range_option/1, get_y_range_option/1,
		  get_x_ticks_option/1, get_y_ticks_option/1,
		  write_header/6, forge_format_string_for/1 ]).



% Not all plot features are available in older gnuplot versions:
-define( gnuplot_reference_version, { 4, 2 } ).



% To centralize basic open flags:
%
% (exclusive used to avoid that two probes bearing the same name by mistake end
% up writing to the same files simultaneously)
%
-define( base_open_flags, [ write, exclusive ] ).


% Name information about a probe:
-type probe_name_init() :: probe_name()
						 | { probe_name(), traces:emitter_categorization() }.



% Type section of external interactions with a probe:
% (we use plain strings here, as opposed to the internal representation)

-type special_curve_names() :: 'abscissa_top' | 'abscissa_bottom'.

-type declared_curve_name() :: ustring().
-type declared_extended_curve_name() :: ustring() | special_curve_names().

-type declared_zone_name() :: ustring().

-type declared_zone() :: { declared_zone_name(),
		{ declared_extended_curve_name(), declared_extended_curve_name() } }.


% Plot style (default being 'linespoints'):
%
% (see http://gnuplot.sourceforge.net/docs_4.2/node145.html)
%
-type plot_style() :: 'linespoints'
					| 'boxes'
					| atom(). % As many others exist.


% Type section for internal data:


% Extended to allow for zone definitions:
% (knowing that the name of a curve is a binary)
%
-type extended_curve_name() :: curve_index() | special_curve_names().


% The factor by which the default point size should be multiplied:
-type point_size_factor() :: pos_integer().


-type name_options() :: probe_name() | { probe_name(), probe_options() }.


-type probe_pid() :: class_ResultProducer:producer_pid().

% A probe may not be a wanted result producer:
-type probe_ref() :: 'non_wanted_probe' | probe_pid().


% Curves are numbered internally, and correspond to the position of data in sent
% samples:
%
-type curve_index() :: curve_count().


% A (binary string) suffix (ex: <<"noenhanced with filledcurves">>, or <<"with
% boxes">>) to be added to the plot command of the corresponding curve:
%
-type curve_plot_suffix() :: bin_string().

-type zone_name() :: bin_string().


-type zone_definition() ::
		{ zone_name(), { extended_curve_name(), extended_curve_name() } }.


% Information specific to the rendering of a curve:
-type curve_entry() :: { curve_index(), curve_name(), curve_plot_suffix() }.

-type curve_entries() :: [ curve_entry() ].


% Ex: "3ab001" for lightgreen:
-type rgb_color_spec() :: ustring().

-type extra_curve_settings() :: rgb_color_spec().


% Option applying to label ticks:
-type tick_option() :: maybe( 'rotate' ).


% Option applying to ticks:
%
% (ex: see Xtics, in http://gensoft.pasteur.fr/docs/gnuplot/5.0.4/node360.html)
%
-type ticks_option() :: maybe( ustring() ).


% The display time format to use for timestamped axes:
-type timestamp_time_format() ::

		% Time then date, on a single line:
		'single_line'

		% Time, newline, date, hence on two lines:
	  | 'double_line'.


% Applies notably if using timestamps that account for more than one field in
% data:
%
-type curve_offset() :: count().


-type zone_entries()  :: [ zone_definition() ].


% Element of a gnuplot command:
-type command_element() :: ustring().


% String describing a simulated time (typically either a tick or a textual
% timestamp):
%
-type timestamp_string() :: ustring().

-type timestamp_bin_string() :: bin_string().



-type setting_id() :: 'global_plot_style' | atom().

-type setting_value() :: plot_style() | term().


% To store any extra probe settings of interest, as plenty options might be
% relevant for probes.
%
% Possible setting pairs (setting_id() -> setting_value()):
%  - global_plot_style -> plot_style()
%
-type settings_table() :: table( setting_id(), setting_value() ).


% Exported so that for example class_Actor can reference them:
-export_type([ probe_name_init/0, name_options/0,
			   declared_curve_name/0, declared_zone_name/0,
			   declared_zone/0, probe_pid/0, probe_ref/0,
			   curve_index/0, curve_plot_suffix/0,
			   curve_entry/0, curve_entries/0, curve_offset/0, zone_entries/0,
			   sample_data/0,
			   timestamp_string/0, timestamp_bin_string/0,
			   settings_table/0, setting_id/0, setting_value/0 ]).



% To have rotated tick labels:

% Rotate clockwise (hence end of text below beginning):
-define( rotate_cw_tick_label_option, "rotate by - 45" ).

% Rotate counter-clockwise (hence end of text above beginning):
% (note the right alignment here, otherwise the labels run from the base of the
% graph upwards)
%
-define( rotate_ccw_tick_label_option, "rotate by 45 right" ).



% Probes require specific (de)serialisations:
-define( wooper_serialisation_hooks,).


% Preliminary support:
-export([ deserialise/4 ]).


% For the probe settings:
-include("class_Probe.hrl").

% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.ResultManagement.Probe.Basic" ).

% For getAttr/1, etc.:
-include_lib("wooper/include/wooper.hrl").

% For app_info*:
-include_lib("traces/include/traces.hrl").



% Constructs a new (basic) probe, from following parameters: NameOptions,
% CurveNames, Title, Zones, MaybeXLabel, YLabel (no tick duration nor extra
% settings specified here), where:
%
% - NameOptions is either:
%
%  - Name :: ustring(); i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
%  - or {Name :: ustring(), ProbeOptions} where ProbeOptions is a list of
%  pairs, in:
%
%   - {create_command_file_initially, boolean()}: if true, the gnuplot command
%   file will be written at probe start-up, thus preventing the taking into
%   account of any subsequent change in the rendering parameter (default: false)
%
%   - {deferred_data_writes, boolean()}: if true, received sample data will
%   stored in memory instead of being directly written to disk (default: false,
%   as the memory footprint might become significant) where Bool is true or
%   false
%
% - CurveNames :: [ustring()] is an (ordered) list containing the names (as
% plain strings) of each curve to be drawn (hence the probe will expect
% receiving data in the form of {Tick, {V1,V2,..}} afterwards). For example,
% CurveNames=["First curve", "Second curve"] will lead to expect receiving
% samples like: {MyTick, {ValueForFirstCurve, ValueForSecondCurve}}
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of {ZoneName,
% {ExtendedCurveNameOne,ExtendedCurveNameTwo}} entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example {"My Zone", {"First curve",'abscissa_bottom'}} defines a zone named
% "My Zone" and delimited by the curve named "First curve" and the abscissa axis
% (note: the order between the two elements defining a zone does not matter)
%
% - Title will be the graph (plot) title
%
% - MaybeXLabel (if any) will be the non-default label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
% - MetaData is an option list that corresponds to extra, contextual information
% that can be taken into account in the probe-generated data files
%
-spec construct( wooper:state(),
		  probe_name_init() | { probe_name_init(), probe_options() },
		  [ declared_curve_name() ], [ declared_zone() ], title(),
		  label(), label(), class_ResultManager:meta_data() ) -> wooper:state().
construct( State, NameTerm, CurveNames, Zones, Title,
		   MaybeXLabel, YLabel, MetaData ) ->
	construct( State, NameTerm, CurveNames, Zones, Title,
			   MaybeXLabel, YLabel, MetaData, _MaybeTickDuration=undefined,
			   _MaybeExtraSettingsTable=undefined ).



% Constructs a new (basic) probe, from following parameters: NameOptions,
% CurveNames, Title, Zones, MaybeXLabel, YLabel, ExtraSettingsTable, where:
%
% - NameOptions is either:
%
%  - Name :: ustring(); i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
%  - or {Name :: ustring(), ProbeOptions} where ProbeOptions is a list of
%  pairs, in:
%
%   - {create_command_file_initially, boolean()}: if true, the gnuplot command
%   file will be written at probe start-up, thus preventing the taking into
%   account of any subsequent change in the rendering parameter (default: false)
%
%   - {deferred_data_writes, boolean()}: if true, received sample data will
%   stored in memory instead of being directly written to disk (default: false,
%   as the memory footprint might become significant) where Bool is true or
%   false
%
% - CurveNames :: [ustring()] is an (ordered) list containing the names (as
% plain strings) of each curve to be drawn (hence the probe will expect
% receiving data in the form of {Tick, {V1,V2,..}} afterwards). For example,
% CurveNames=["First curve", "Second curve"] will lead to expect receiving
% samples like: {MyTick, {ValueForFirstCurve, ValueForSecondCurve}}
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of {ZoneName,
% {ExtendedCurveNameOne,ExtendedCurveNameTwo}} entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example {"My Zone", {"First curve",'abscissa_bottom'}} defines a zone named
% "My Zone" and delimited by the curve named "First curve" and the abscissa axis
% (note: the order between the two elements defining a zone does not matter)
%
% - Title will be the graph (plot) title
%
% - MaybeXLabel (if any) will be the non-default label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
% - MetaData is an option list that corresponds to extra, contextual information
% that can be taken into account in the probe-generated data files
%
% - MaybeExtraSettingsTable is, if defined, a table holding extra probe settings
% of all sorts (allows to parameter one's probe from its creation, rather than
% sending a series of method calls to do the same once it has already been
% created)
%
% - MaybeTickDuration is, if defined, the actual duration (in simulation time)
% of a tick, for timestamped axis labels
%
-spec construct( wooper:state(),
		probe_name_init() | { probe_name_init(), probe_options() },
		[ declared_curve_name() ], [ declared_zone() ], title(),
		label(), label(), class_ResultManager:meta_data(),
		maybe( settings_table() ), maybe( virtual_seconds() ) ) ->
						wooper:state().
construct( State, { NameInit, ProbeOptions }, CurveNames, Zones, Title,
		   MaybeXLabel, YLabel, MetaData, MaybeExtraSettingsTable,
		   MaybeTickDuration ) when is_record( ProbeOptions, probe_options ) ->

	ProbeName = get_actual_probe_name( NameInit ),

	%trace_utils:debug_fmt( "Creating probe '~ts'.", [ ProbeName ] ),

	% First the direct mother classes:

	% Already trace-categorized:
	ProducerState = class_ResultProducer:construct( State, ProbeName ),

	% Then the class-specific actions:

	% Results in [ {curve_index(), curve_name(), curve_plot_suffix()} ]:
	CurveEntries = transform_curve_names( CurveNames ),

	%trace_utils:debug_fmt( "Initial curve entries: ~p.", [ CurveEntries ] ),

	% Results in [ zone_definition() ]:
	ZoneEntries = transform_declared_zones( Zones, CurveEntries ),

	%trace_utils:debug_fmt( "Initial zone entries: ~p.", [ ZoneEntries ] ),

	GnuplotVersion = executable_utils:get_current_gnuplot_version(),

	ProbeBaseSettings =
		get_probe_settings( Title, MaybeXLabel, YLabel, GnuplotVersion ),

	{ ProbeSettings, ExtraCurveSettings } = apply_extra_settings(
		MaybeExtraSettingsTable, ProbeBaseSettings, ProducerState ),

	UpdatedCurveEntries =
		update_curve_entries( CurveEntries, ExtraCurveSettings ),

	{ CreateCommandFileInitially, DeferredDataWrites, IsTrackedProducer,
	  ProbeDir, MaybeBinProbeDir } = interpret_options( ProbeOptions ),

	% For an increased interleaving:
	getAttribute( ProducerState, result_manager_pid ) ! { declareProbe,
			[ text_utils:string_to_binary( ProbeName ), IsTrackedProducer,
			  MaybeBinProbeDir ], self() },

	CurveCount = length( CurveNames ),

	DataFilename = file_utils:join( ProbeDir, get_data_filename( ProbeName ) ),


	StartState = setAttributes( ProducerState, [
		{ settings, ProbeSettings },
		{ command_file_up_to_date, false },
		{ deferred_data_writes, DeferredDataWrites },
		{ is_tracked_producer, IsTrackedProducer },
		{ probe_dir, text_utils:string_to_binary( ProbeDir ) },
		{ curve_count, CurveCount },
		{ curve_entries, UpdatedCurveEntries },
		{ zone_entries, ZoneEntries },
		{ tick_offset, 0 },
		{ maybe_tick_duration, MaybeTickDuration },
		{ sample_count, 0 },
		{ data_table, [] },
		{ data_filename, text_utils:string_to_binary( DataFilename ) },
		{ data_file, undefined },
		{ row_format_string, forge_format_string_for( CurveCount ) },
		{ gnuplot_version, GnuplotVersion },
		{ meta_data, MetaData } ] ),

	CommandState = case CreateCommandFileInitially of

		true ->
			generate_command_file( StartState );

		false ->
			StartState

	end,

	DeferredState = case DeferredDataWrites of

		 true ->
			CommandState;

		 false ->

			check_probe_directory( ProbeDir ),

			file_utils:remove_file_if_existing( DataFilename ),

			% We perform "immediate" writes here (i.e. not storing samples),
			% however we rely on the underlying delayed raw writes. Probes do
			% not have to be too much responsive (hence the 2s delay), but as
			% they may be very numerous we choose not a too big buffer (4KB), to
			% reduce the overall memory footprint:
			%
			% Note that this file creation may still fail despite the previous
			% removal, if creating multiple probes with the same name in
			% parallel (resulting in a race condition).
			%
			DataFile = file_utils:open( DataFilename,
			  [ { delayed_write, _Size=4*1024, _Delay=2000 }
				| ?base_open_flags ] ),


			% Format used merely for a portable '\n':
			file_utils:write_ustring( DataFile,
				"# Warning: using immediate writes here, thus this "
				"header might be inaccurate~n"
				"# or incomplete, should, respectively, subsequent curve "
				"reorderings~n"
				"# or additions be performed.~n"
				"# Only initially-created curves are listed below, "
				"the dynamically added~n"
				"# ones are not visible in this file.~n~n", [] ),

			write_header( DataFile, CurveEntries, ZoneEntries, ProbeSettings,
						  ProbeName, MetaData ),

			setAttribute( CommandState, data_file, DataFile )

	end,

	% After call to the declareProbe/4 request:
	wait_result_declaration_outcome( ProbeName, DeferredState );


construct( State, Name, CurveNames, Zones, Title, MaybeXLabel, YLabel, MetaData,
		   MaybeExtraSettingsTable, MaybeTickDuration ) ->
	% Will be using default settings here:
	construct( State, { Name, _DefaultOptions=#probe_options{} }, CurveNames,
			   Zones, Title, MaybeXLabel, YLabel, MetaData,
			   MaybeExtraSettingsTable, MaybeTickDuration ).




% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%trace_utils:debug_fmt( "Deleting probe '~ts'.", [ ?getAttr(name) ] ),

	% Class-specific actions:

	?info( "Deleting probe." ),

	% Closing here in all cases, whether data writes are deferred or not:
	case ?getAttr(data_file) of

		undefined ->
			ok;

		DataFile ->

			case ?getAttr(deferred_data_writes) of

				true ->
					% Simplest, direct case:
					file_utils:close( DataFile );

				false ->

					% We were performing immediate writes here, but due to the
					% delayed_write option, close may return an old write error
					% and not even try to close the file. In that case we try to
					% close it one more time:
					%
					case file:close( DataFile ) of

						{ error, Reason } ->
							?error_fmt( "Error while closing probe '~ts': ~p.",
										[ ?getAttr(name), Reason ] ),
							file:close( DataFile ) ;

						ok  ->
							ok

					end

			end

	end,

	%?debug( "Probe deleted." ),

	% Then call the direct mother class counterparts and allow chaining:
	setAttribute( State, data_file, undefined ).





% Methods section.



% Sets the tick offset that will be subtracted from the tick of all samples that
% will be received next. Then the abscissa axis will at least start with
% clearer, shorter, more tractable labels.
%
% For an example, refer to the soda vending machine test: these machines manage
% their probe so that it uses an offset to the simulation initial tick.
%
-spec setTickOffset( wooper:state(), probe_tick() ) -> oneway_return().
setTickOffset( State, Offset ) ->
	wooper:return_state( setAttribute( State, tick_offset, Offset ) ).



% Registers specified samples in the probe.
%
% Samples is a tuple that contains the value (either integer or floating-point)
% corresponding to the specified tick (not tick offset) for each known curve.
%
% Should a curve have no relevant sample to be defined, the 'undefined' atom
% should be specified instead.
%
-spec setData( wooper:state(), probe_tick(), sample_data() ) -> oneway_return().
setData( State, Tick, Samples ) ->

	%?debug_fmt( "setData called for tick ~B with samples ~p.",
	%  [ Tick, Samples ] ),

	ExpectedCount = ?getAttr(curve_count),

	case size( Samples ) of

		ExpectedCount ->
			ok;

		_Other ->
			throw( { invalid_sample_size, Samples, ExpectedCount } )

	end,

	TimestampStr = case ?getAttr(maybe_tick_duration) of

		undefined ->
			RecordedTick = Tick - ?getAttr(tick_offset),
			text_utils:integer_to_string( RecordedTick );

		TickDuration ->
			Secs = class_Actor:convert_ticks_to_seconds_explicit( Tick,
														   TickDuration ),
			IntegerSecs = round( Secs ),
			Timestamp = calendar:gregorian_seconds_to_datetime( IntegerSecs ),

			time_utils:timestamp_to_string( Timestamp )

	end,

	%trace_utils:debug_fmt( "Timestamp string: '~ts'.", [ TimestampStr ] ),

	TimestampBinStr = text_utils:string_to_binary( TimestampStr ),

	TrackedState = addToAttribute( State, sample_count, 1 ),

	case ?getAttr(deferred_data_writes) of

		true ->

			% Watch out the memory footprint in long simulations!
			wooper:return_state( appendToAttribute( TrackedState,
					data_table, { TimestampBinStr, Samples } ) );

		false ->
			RowFormatStr = case ?getAttr(row_format_string) of

				undefined ->
					forge_format_string_for( ExpectedCount );

				RowFStr ->
					RowFStr

			end,

			% Lower-level I/O will attempt a bit of deferred write nevertheless:
			write_row( ?getAttr(data_file), RowFormatStr, TimestampBinStr,
					   Samples ),

			FmtState = setAttribute( TrackedState, row_format_string,
									 RowFormatStr ),

			wooper:return_state( FmtState )

	end.




% Declares an additional curve, whose name is specified as a plain string.
%
% By default it will be rendered after the already declared curves.
%
% Note: all samples received afterwards are then expected to take it into
% account (sending one more value, or the atom 'undefined', for that curve).
%
-spec addCurve( wooper:state(), string_curve_name() ) -> oneway_return().
addCurve( State, CurveName ) ->

	%trace_utils:debug_fmt( "addCurve '~ts' for probe '~ts'.",
	%		  [ CurveName, ?getAttr(name) ] ),

	{ NewCurveCount, NewCurveEntries } =
		add_curve( CurveName, ?getAttr(curve_count), ?getAttr(curve_entries) ),

	wooper:return_state( setAttributes( State, [
		{ curve_count, NewCurveCount },
		{ curve_entries, NewCurveEntries },

		% Forcing a later re-creation:
		{ command_file_up_to_date, false },
		{ row_format_string, forge_format_string_for( NewCurveCount ) } ] ) ).



% Declares additional curves, whose names are specified as plain strings.
%
% By default they will be rendered in their specified order, after the already
% declared curves.
%
% Note: all samples received afterwards are then expected to take it into
% account (sending as many additional values, or the atom 'undefined', for these
% curves).
%
-spec addCurves( wooper:state(), [ string_curve_name() ] ) -> oneway_return().
addCurves( State, CurveNames ) ->

	%trace_utils:debug_fmt( "addCurves '~p' for probe '~ts'.",
	%						 [ CurveNames, ?getAttr(name) ] ),

	{ NewCurveCount, NewCurveEntries } = lists:foldl(
			fun( CurveName, _Acc={ CurveCount, CurveEntries } ) ->
				add_curve( CurveName, CurveCount, CurveEntries )
			end,
			_Acc0={ ?getAttr(curve_count), ?getAttr(curve_entries) },
			_List=CurveNames ),

	wooper:return_state( setAttributes( State, [
		{ curve_count, NewCurveCount },
		{ curve_entries, NewCurveEntries },

		% Forcing a later re-creation:
		{ command_file_up_to_date, false },
		{ row_format_string, forge_format_string_for( NewCurveCount ) } ] ) ).



% (helper)
-spec add_curve( string_curve_name(), curve_count(), [ curve_entry() ] ) ->
						{ curve_count(), [ curve_entry() ] }.
add_curve( CurveName, CurveCount, CurveEntries ) ->

	NewCurveCount = CurveCount + 1,

	NewCurveEntry = { NewCurveCount, text_utils:string_to_binary( CurveName ),
					  get_default_plot_suffix() },

	NewCurveEntries = list_utils:append_at_end( NewCurveEntry, CurveEntries ),

	{ NewCurveCount, NewCurveEntries }.



% Returns the list of curve names, as plain strings, sorted according to the
% current rendering order.
%
% Useful then to reorder them and then to set them back thanks to
% setCurveRenderOrder/2.
%
-spec getCurveRenderOrder( wooper:state() ) ->
								const_request_return( [ string_curve_name() ] ).
getCurveRenderOrder( State ) ->

	CurveEntries = ?getAttr(curve_entries),

	% Extract the curve name (order preserved):
	PlainNames = [ text_utils:binary_to_string( element( 2, CurveEntry ) )
				   || CurveEntry <- CurveEntries ],

	%trace_utils:debug_fmt( "Returned curve render order: ~p", [ PlainNames ] ),

	wooper:const_return_result( PlainNames ).



% Sets the list of curve names, sorted according to the desired rendering order.
%
% Names is a list of plain strings that must correspond to a permutation of the
% list which would be returned by getCurveEntries/1.
%
-spec setCurveRenderOrder( wooper:state(), [ string_curve_name() ] ) ->
									oneway_return().
setCurveRenderOrder( State, Names ) ->

	CurveEntries = ?getAttr(curve_entries),
	Len = length( CurveEntries ),

	case length( Names ) of

		Len ->

			NewCurveEntries = add_probe_index_back( Names, CurveEntries ),

			%trace_utils:debug_fmt( "Set curve render order: ~p.",
			%                       [ NewCurveEntries ] ),

			% We force the (possible re-)generation of a command file, when a
			% report will be requested:
			%
			wooper:return_state( setAttributes( State, [
					 { curve_entries, NewCurveEntries },
					 { command_file_up_to_date, false } ] ) );

		_Other ->
			throw( { invalid_name_count, Names, Len } )

	end.



% Sets the plot settings to the ones specified as a plain string (ex:
% "histograms", "linespoints"; "lines" is the default).
%
-spec setPlotStyle( wooper:state(), ustring() ) -> oneway_return().
setPlotStyle( State, NewPlotStyle ) ->

	Settings = ?getAttr(settings),

	wooper:return_state( setAttributes( State, [

		{ settings, Settings#probe_settings{
			plot_style=text_utils:string_to_binary( NewPlotStyle ) } },

		% Forcing a later re-creation:
		{ command_file_up_to_date, false } ] ) ).



% Sets the plot settings to the ones specified as a plain string (ex:
% "histograms", "linespoints"; "lines" is the default) and determines whether
% the command file shall be regenerated in order to take into account the new
% settings.
%
-spec setPlotStyle( wooper:state(), ustring(), boolean() ) -> oneway_return().
setPlotStyle( State, NewPlotStyle, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{
			  plot_style=text_utils:string_to_binary( NewPlotStyle ) } ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	wooper:return_state( CommandState ).



% Sets the fill settings, specified as a plain string (ex: "solid 1.0 border
% -1").
%
-spec setFillStyle( wooper:state(), ustring() ) -> oneway_return().
setFillStyle( State, NewFillStyle ) ->

	Settings = ?getAttr(settings),

	wooper:return_state( setAttributes( State, [

		{ settings, Settings#probe_settings{
			fill_style=text_utils:string_to_binary( NewFillStyle ) } },

		% Forcing a later re-creation:
		{ command_file_up_to_date, false } ] ) ).



% Sets the fill settings, specified as a plain string (ex: "solid 1.0 border
% -1") and determines whether the command file shall be regenerated in order to
% take into account the new settings.
%
-spec setFillStyle( wooper:state(), ustring(), boolean() ) -> oneway_return().
setFillStyle( State, NewFillStyle, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{
		  fill_style=text_utils:string_to_binary( NewFillStyle ) } ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	wooper:return_state( CommandState ).



% Sets the size of the probe reports (canvas), in pixels.
-spec setCanvasSize( wooper:state(), length(), length() ) -> oneway_return().
setCanvasSize( State, NewWidth, NewHeight ) ->

	Settings = ?getAttr(settings),

	wooper:return_state( setAttributes( State, [

			  { settings, Settings#probe_settings{ canvas_width=NewWidth,
												   canvas_height=NewHeight } },

			   % Forcing a later re-creation:
			   { command_file_up_to_date, false } ] ) ).



% Sets the size of the probe reports (canvas), in pixels and forces to
% regenerate the command file for taking into account these new settings, if
% requested.
%
-spec setCanvasSize( wooper:state(), length(), length(), boolean() ) ->
							oneway_return().
setCanvasSize( State, NewWidth, NewHeight, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{ canvas_width=NewWidth,
								 canvas_height=NewHeight } ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	wooper:return_state( CommandState ).



% Sets the size of the plot point.
%
% Point size means that the shown points will be of PointSize times the default
% point size.
%
-spec setPointSize( wooper:state(), point_size_factor() ) -> oneway_return().
setPointSize( State, PointSize ) ->

	Settings = ?getAttr(settings),

	wooper:return_state( setAttributes( State, [

			{ settings, Settings#probe_settings{ point_size=PointSize } },

			% Forcing a later re-creation:
			{ command_file_up_to_date, false } ] ) ).



% Sets the size of the plot point and forces to regenerate the command file in
% order to take into account the new settings.
%
% Point size means that the shown points will be of PointSize times the default
% point size.
%
-spec setPointSize( wooper:state(), point_size_factor(), boolean() ) ->
							oneway_return().
setPointSize( State, PointSize, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{ point_size=PointSize } ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	wooper:return_state( CommandState ).



% Sets the key (legend) settings, specified as a plain string (ex: "inside
% left", "bottom center").
%
-spec setKeyOptions( wooper:state(), ustring() ) -> oneway_return().
setKeyOptions( State, NewOptions ) ->

	Settings = ?getAttr(settings),

	wooper:return_state( setAttribute( State, settings,
		Settings#probe_settings{
				key_options=text_utils:string_to_binary( NewOptions ) } ) ).



% Sets the key (legend) settings, specified as a plain string (ex: "inside
% left", "bottom center") and forces to regenerate the command file for taking
% into account the new settings, if requested.
%
-spec setKeyOptions( wooper:state(), ustring(), boolean() ) -> oneway_return().
setKeyOptions( State, NewOptions, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{
				key_options=text_utils:string_to_binary( NewOptions ) } ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	wooper:return_state( CommandState ).



% Sets the abscissa range for the plot.
%
% MinX and MaxX are integers.
%
-spec setAbscissaRange( wooper:state(), coordinate(), coordinate() ) ->
								oneway_return().
setAbscissaRange( State, MinX, MaxX ) ->

	Settings = ?getAttr(settings),
	wooper:return_state( setAttribute( State, settings,
		Settings#probe_settings{ x_range={ MinX, MaxX } } ) ).



% Sets the abscissa range for the plot and forces to regenerate the command file
% for taking into account the new settings if requested.
%
% MinX and MaxX are integers.
%
-spec setAbscissaRange( wooper:state(), coordinate(), coordinate(),
						boolean() ) -> oneway_return().
setAbscissaRange( State, MinX, MaxX, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{ x_range={ MinX, MaxX } } ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	wooper:return_state( CommandState ).



% Sets the ordinate range for the plot.
%
%  MinY and MaxY are integers.
%
-spec setOrdinateRange( wooper:state(), coordinate(), coordinate() ) ->
								oneway_return().
setOrdinateRange( State, MinY, MaxY ) ->

	Settings = ?getAttr(settings),

	wooper:return_state( setAttribute( State, settings,
		Settings#probe_settings{ y_range={ MinY, MaxY } } ) ).



% Sets the ordinate range for the plot and forces to regenerate the command file
% for taking into account the new settings, if requested.
%
% MinY and MaxY are integers.
%
-spec setOrdinateRange( wooper:state(), coordinate(), coordinate(),
						boolean() ) -> oneway_return().
setOrdinateRange( State, MinY, MaxY, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{ y_range={ MinY, MaxY } } ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	wooper:return_state( CommandState ).



% Ensures that the generated reports will rely on rotated tick labels, so that
% labels will never overlap, however long they are.
%
-spec setRotatedTickLabels( wooper:state() ) -> oneway_return().
setRotatedTickLabels( State ) ->

	Settings = ?getAttr(settings),

	%wooper:return_state( setAttribute( State, settings,
	%	Settings#probe_settings{ x_tick=text_utils:string_to_binary(
	%		   "format \"%.0f\" border out rotate by 90 offset 0,graph 0.05"

	wooper:return_state( setAttributes( State, [

		{ settings, Settings#probe_settings{
		  x_tick=text_utils:string_to_binary(
				   ?rotate_cw_tick_label_option ) } },

		{ command_file_up_to_date, false } ] ) ).



% Ensures that the generated reports will rely on rotated tick labels, so that
% labels will never overlap, however long they are, and regenerates the command
% file if requested.
%
-spec setRotatedTickLabels( wooper:state(), boolean() ) -> oneway_return().
setRotatedTickLabels( State, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings, Settings#probe_settings{
			x_tick=text_utils:string_to_binary(
					 ?rotate_cw_tick_label_option ) } ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	wooper:return_state( CommandState ).




% Adds a specific text label, specified as a plain string, at the specified
% location ({X,Y} integer coordinates).
%
-spec addLabel( wooper:state(), ustring(), point() ) -> oneway_return().
addLabel( State, Text, Location ) ->

	Settings = ?getAttr(settings),

	% Adds default values where none was specified:
	NewLabel = #probe_label{ location=Location,
							 text=text_utils:string_to_binary( Text ),
							 color=blue,
							 position=center,
							 orientation=upright },

	Labels = [ NewLabel | Settings#probe_settings.labels ],

	wooper:return_state( setAttribute( State, settings,
			Settings#probe_settings{ labels=Labels } ) ).



% Adds a specific text label, specified as a plain string, at specified location
% ({X,Y} integer coordinates), with specified color (ex: 'magenta', or
% "#4B00820").
%
-spec addLabel( wooper:state(), ustring(), point(), color() ) ->
						oneway_return().
addLabel( State, Text, Location, Color ) ->

	Settings = ?getAttr(settings),

	% Adds default values where none was specified:
	NewLabel = #probe_label{ location=Location,
							 text=text_utils:string_to_binary( Text ),
							 color=Color,
							 position=center,
							 orientation=upright },

	Labels = [ NewLabel | Settings#probe_settings.labels ],

	wooper:return_state( setAttribute( State, settings,
			Settings#probe_settings{ labels=Labels } ) ).



% Adds a specific text label, specified as a plain string, at specified location
% ({X,Y} integer coordinates), with specified color (ex: magenta, or "#4B00820")
% and orientation, either 'upright' (the default), or {rotate, Angle}, Angle
% being an angle in degrees (as a floating-point value).
%
-spec addLabel( wooper:state(), ustring(), point(), color(),
				label_orientation() ) -> oneway_return().
addLabel( State, Text, Location, Color, Orientation ) ->

	Settings = ?getAttr(settings),

	% Adds default values where none was specified:
	NewLabel = #probe_label{ location=Location,
							 text=text_utils:string_to_binary( Text ),
							 color=Color,
							 position=center,
							 orientation=Orientation },

	Labels = [ NewLabel | Settings#probe_settings.labels ],

	wooper:return_state( setAttribute( State, settings,
			Settings#probe_settings{ labels=Labels } ) ).



% Adds a specific text label, specified as a plain string, at specified Location
% ({X,Y} integer coordinates), with specified color (ex: magenta), orientation,
% either 'upright' (the default), or {rotate, Angle}, Angle being an angle in
% degrees (as a floating-point value) and position (an atom, either left, center
% or right, the default being center).
%
-spec addLabel( wooper:state(), ustring(), point(), color(),
				label_orientation(), label_position() ) -> oneway_return().
addLabel( State, Text, Location, Color, Orientation, Position ) ->

	Settings = ?getAttr(settings),

	% Adds default values where none was specified:
	NewLabel = #probe_label{ location=Location,
							 text=text_utils:string_to_binary( Text ),
							 color=Color,
							 position=Position,
							 orientation=Orientation },

	Labels = [ NewLabel | Settings#probe_settings.labels ],

	wooper:return_state( setAttribute( State, settings,
			Settings#probe_settings{ labels=Labels } ) ).



% Sets the extra verbatim defines (ex: [ "set xlabel offset character 15" ]).
-spec setExtraDefines( wooper:state(), [ ustring() ] ) -> oneway_return().
setExtraDefines( State, ExtraDefs ) ->

	BinExtraDefs = text_utils:strings_to_binaries( ExtraDefs ),

	Settings = ?getAttr(settings),

	NewSettings = Settings#probe_settings{ extra_defines=BinExtraDefs },

	wooper:return_state( setAttribute( State, settings, NewSettings ) ).



% Adds (appends at first position) the specified extra verbatim defines (ex:
% [ "set xlabel offset character 15" ]).
%
-spec addExtraDefines( wooper:state(), [ ustring() ] ) -> oneway_return().
addExtraDefines( State, ExtraDefs ) ->

	BinExtraDefs = text_utils:strings_to_binaries( ExtraDefs ),

	Settings = ?getAttr(settings),

	NewExtraDefs = BinExtraDefs ++ Settings#probe_settings.extra_defines,

	NewSettings = Settings#probe_settings{ extra_defines=NewExtraDefs },

	wooper:return_state( setAttribute( State, settings, NewSettings ) ).



% Generates the appropriate gnuplot command file.
%
% (oneway, so that it can be overridden)
%
-spec generateCommandFile( wooper:state() ) -> oneway_return().
generateCommandFile( State ) ->
	NewState = generate_command_file( State ),
	wooper:return_state( NewState ).



% Generates a report corresponding to the current state of this probe, and
% displays the result (the image) to the user.
%
-spec generateReport( wooper:state() ) ->
							request_return( 'probe_report_generated' ).
generateReport( State ) ->
	{ NewState, Res } = generateReport( State, _DisplayWanted=true ),
	wooper:return_state_result( NewState, Res ).



% Generates a report corresponding to the current state of this probe.
%
% DisplayWanted is a boolean telling whether the generated report will be
% displayed to the user (if true).
%
% Returns the 'probe_report_generated' atom, merely for synchronisation purpose.
%
-spec generateReport( wooper:state(), boolean() ) ->
							request_return( 'probe_report_generated' ).
generateReport( State, DisplayWanted ) ->

	Name = text_utils:binary_to_string( ?getAttr(name) ),

	%trace_utils:debug_fmt( "generateReport for probe '~ts'.", [ Name ] ),

	ReportState = generate_report( Name, State ),

	case DisplayWanted of

		true ->
			ReportName = get_report_filename( Name ),
			executable_utils:display_png_file(
				file_utils:join( ?getAttr(probe_dir), ReportName ) );

		false ->
			ok

	end,

	wooper:return_state_result( ReportState, probe_report_generated ).



% Sets the probe directory: all further probe files (command, data, locally
% generated plots) will be created there.
%
-spec setDirectory( wooper:state(), any_directory_path() ) -> oneway_return().
setDirectory( State, NewProbeDirectory ) ->

	BinProbeDir = text_utils:ensure_binary( NewProbeDirectory ),

	% Expected to already exist:
	case file_utils:is_existing_directory( BinProbeDir ) of

		true ->
			ok;

		false ->
			throw( { non_existing_probe_directory, NewProbeDirectory } )

	end,

	% data_filename is the only precomputed path (thus the only one to be
	% updated):
	%
	DataFilename = file_utils:join( BinProbeDir,
			get_data_filename( class_TraceEmitter:get_plain_name( State ) ) ),

	wooper:return_state( setAttributes( State, [
		{ probe_dir, BinProbeDir },
		{ data_filename, text_utils:string_to_binary( DataFilename ) } ] ) ).



% Sends the specified type of (tracked) results to the caller (generally the
% result manager).
%
% (request, notably for synchronous operations)
%
-spec sendResults( wooper:state(), class_ResultProducer:producer_options() ) ->
				request_return( class_ResultProducer:producer_result() ).
sendResults( State, [ data_only ] ) ->

	% Here we will send an archive term containing the data and command files:
	CommandState = ensure_command_file_available( State ),
	ensure_data_file_available( CommandState ),

	% Checks:
	false = ?getAttr(result_produced),
	false = ?getAttr(result_collected),

	Name = class_TraceEmitter:get_plain_name( CommandState ),

	case ?getAttr(sample_count) of

		0 ->
			?notice_fmt( "The probe '~ts' did not receive any data sample.",
						 [ Name ] );

		_ ->
			ok

	end,

	DataFilename = text_utils:binary_to_string( ?getAttr(data_filename) ),

	PathLessDataFilename = filename:basename( DataFilename ),

	CommandFilename = get_command_filename( Name ),

	FileList = [ PathLessDataFilename, CommandFilename ],

	?info_fmt( "Creating binary data-only archive term for ~p.",
				[ FileList ] ),

	BinArchive = file_utils:files_to_zipped_term( FileList ),

	ProbeDir = ?getAttr(probe_dir),

	FilesToRemove = [ file_utils:join( ProbeDir, F ) || F <- FileList ],

	% Performing clean-up here is useful, as (basic) probes created directly
	% from the simulation case will not be created under the directory for
	% temporary data (by default '/tmp') - like it is the case for computing
	% nodes - but in the current directory.
	%
	file_utils:remove_files( FilesToRemove ),

	% We consider that these results are produced, as only data is required and
	% is already available:
	%
	FinalState = setAttributes( CommandState, [ { result_produced, true },
												{ result_collected, true } ] ),

	wooper:return_state_result( FinalState,
								{ self(), archive, BinArchive } );


sendResults( State, [ rendering_only ] ) ->

	% Checks:
	false = ?getAttr(result_produced),
	false = ?getAttr(result_collected),

	Name = class_TraceEmitter:get_plain_name( State ),

	case ?getAttr(sample_count) of

		0 ->
			?notice_fmt( "The probe '~ts' did not receive any data sample, "
				"hence does cannot yield a plot.", [ Name ] ),

			FinalState = setAttributes( State, [ { result_produced, true },
												 { result_collected, true } ] ),

			Result = { self(), no_result },

			wooper:return_state_result( FinalState, Result );


		_ ->

			ReportState = generate_report( Name, State ),

			ReportFilename = get_report_filename( Name ),

			?info_fmt( "Creating binary plot-only raw term for ~p.",
						[ ReportFilename ] ),

			ProbeDir = ?getAttr(probe_dir),

			ReportFilenameFullPath =
				file_utils:join( ProbeDir, ReportFilename ),

			BinContent = file_utils:read_whole( ReportFilenameFullPath ),

			BinReportFilename = text_utils:string_to_binary( ReportFilename ),

			DataFilename =
				text_utils:binary_to_string( ?getAttr(data_filename) ),

			CommandFilename = get_command_filename( Name ),

			FileList = [ CommandFilename, ReportFilename ],

			FilesToRemove = [ file_utils:join( ProbeDir, F ) || F <- FileList ],

			% DataFilename is already a full path:
			file_utils:remove_files( [ DataFilename | FilesToRemove ] ),

			% result_produced already set to true by generate_report/2:
			FinalState = setAttribute( ReportState, result_collected, true ),

			Result = { self(), raw, { BinReportFilename, BinContent } },

			wooper:return_state_result( FinalState, Result )

	end;


sendResults( State, [ data_and_rendering ] ) ->

	% Checks:
	false = ?getAttr(result_produced),
	false = ?getAttr(result_collected),

	% Here we will send an archive term containing the data, command and plot
	% files (if available):

	DataFilename = text_utils:binary_to_string( ?getAttr(data_filename) ),

	PathLessDataFilename = filename:basename( DataFilename ),

	Name = class_TraceEmitter:get_plain_name( State ),

	CommandFilename = get_command_filename( Name ),

	{ FileList, ListedState } = case ?getAttr(sample_count) of

		0 ->
			% We do not generate the report, yet we want the command file:
			CommandState = ensure_command_file_available( State ),

			ProducedState = setAttribute( CommandState, result_produced, true ),

			?notice_fmt( "The probe '~ts' did not receive any data sample, "
				"hence does not yield a plot.", [ Name ] ),

			{ [ PathLessDataFilename, CommandFilename ], ProducedState };

		_ ->

			% Generates the report, thus the data and command files:
			ReportState = generate_report( Name, State ),

			ReportFilename = get_report_filename( Name ),

			{ [ PathLessDataFilename, CommandFilename, ReportFilename ],
			  ReportState }

	end,

	ProbeDir = ?getAttr(probe_dir),

	?info_fmt( "Creating binary plot-and-data archive term for ~p, from '~ts'.",
				[ FileList, ProbeDir ] ),

	BinArchive = file_utils:files_to_zipped_term( FileList, ProbeDir ),

	FilesToRemove = [ file_utils:join( ProbeDir, F ) || F <- FileList ],

	file_utils:remove_files( FilesToRemove ),

	% result_produced already set to true (ex: by generate_report/2):
	FinalState = setAttribute( ListedState, result_collected, true ),

	wooper:return_state_result( FinalState, { self(), archive, BinArchive } ).




% Returns a textual description of this probe instance.
-spec toString( wooper:state() ) -> const_request_return( ustring() ).
toString( State ) ->

	CleanCommandWord = case ?getAttr(command_file_up_to_date) of

		true ->
			"";

		false ->
			"not "

	end,

	DeferredWord = case ?getAttr(deferred_data_writes) of

		true ->
			"";

		false ->
			"not "

	end,

	Text = text_utils:format( "Probe '~ts', whose command file is ~ts cleaned, "
			"and ~tsperforming deferred data writes",
			[ ?getAttr(name), CleanCommandWord, DeferredWord ] ),

	wooper:const_return_result( Text ).





% Generic interface.


% Static methods.


% Declares (synchronously) a new (basic) probe, to be seen as a result producer,
% and be created either from an actor or from a test case.
%
% - NameOptions is either:
%
%  - Name :: ustring(); i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
%  - or {Name :: ustring(), ProbeOptions} where ProbeOptions is a list of
%  pairs, in:
%
%   - {create_command_file_initially, boolean()}: if true, the gnuplot command
%   file will be written at probe start-up, thus preventing the taking into
%   account of any subsequent change in the rendering parameter (default: false)
%
%   - {deferred_data_writes, boolean()}: if true, received sample data will
%   stored in memory instead of being directly written to disk (default: false,
%   as the memory footprint might become significant) where Bool is true or
%   false
%
% - CurveNames :: [ ustring() ] is an (ordered) list containing the names (as
% plain strings) of each curve to be drawn (hence the probe will expect
% receiving data in the form of {Tick, {V1,V2,..}} afterwards). For example,
% CurveNames=["First curve", "Second curve"] will lead to expect receiving
% samples like: {MyTick, {ValueForFirstCurve, ValueForSecondCurve}}
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of { ZoneName,
% {ExtendedCurveNameOne,ExtendedCurveNameTwo} } entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example {"My Zone", {"First curve",'abscissa_bottom'} } defines a zone named
% "My Zone" and delimited by the curve named "First curve" and the abscissa axis
% (note: the order between the two elements defining a zone does not matter)
%
% - Title will be the graph (plot) title
%
% - MaybeXLabel (if any) will be the non-default label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
% Returns either the PID of this newly created result probe (if the name of that
% probe is acknowledged as a wanted result by the result manager), or the
% 'non_wanted_probe' atom.
%
-spec declare_result_probe( name_options(), [ declared_curve_name() ],
		 [ declared_zone() ], title(), label(), label() ) ->
									static_return( probe_ref() ).
declare_result_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					  MaybeXLabel, YLabel ) ->

	Res = declare_result_probe( NameOptions, CurveEntries, ZoneEntries, Title,
			MaybeXLabel, YLabel, _MaybeExtraSettingsTable=undefined,
			_MaybeTickDuration=undefined ),

	wooper:return_static( Res ).



% Declares (synchronously) a new (basic) probe, to be seen as a result producer,
% and be created either from an actor or from a test case.
%
% - NameOptions is either:
%
%  - Name :: ustring(); i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
%  - or {Name :: ustring(), ProbeOptions} where ProbeOptions is a list of
%  pairs, in:
%
%   - {create_command_file_initially, boolean()}: if true, the gnuplot command
%   file will be written at probe start-up, thus preventing the taking into
%   account of any subsequent change in the rendering parameter (default: false)
%
%   - {deferred_data_writes, boolean()}: if true, received sample data will
%   stored in memory instead of being directly written to disk (default: false,
%   as the memory footprint might become significant) where Bool is true or
%   false
%
% - CurveNames :: [ ustring() ] is an (ordered) list containing the names (as
% plain strings) of each curve to be drawn (hence the probe will expect
% receiving data in the form of {Tick, {V1,V2,..}} afterwards). For example,
% CurveNames=["First curve", "Second curve"] will lead to expect receiving
% samples like: {MyTick, {ValueForFirstCurve, ValueForSecondCurve}}
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of {ZoneName,
% {ExtendedCurveNameOne, ExtendedCurveNameTwo}} entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example {"My Zone", {"First curve", 'abscissa_bottom'}} defines a zone named
% "My Zone" and delimited by the curve named "First curve" and the abscissa axis
% (note: the order between the two elements defining a zone does not matter)
%
% - Title will be the graph (plot) title
%
% - MaybeXLabel (if any) will be the non-default label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
% - MaybeTickDuration is, if defined, the actual duration (in simulation time)
% of a tick, for timestamped axis labels
%
% Returns either the PID of this newly created result probe (if the name of that
% probe is acknowledged as a wanted result by the result manager), or the
% 'non_wanted_probe' atom.
%
-spec declare_result_probe( name_options(), [ declared_curve_name() ],
		[ declared_zone() ], title(), label(), label(),
		maybe( virtual_seconds() ), maybe( settings_table() ) ) ->
									static_return( probe_ref() ).
declare_result_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					  MaybeXLabel, YLabel, MaybeTickDuration ) ->

	Res = declare_result_probe( NameOptions, CurveEntries, ZoneEntries, Title,
		MaybeXLabel, YLabel, _MaybeExtraSettingsTable=undefined,
		MaybeTickDuration ),

	wooper:return_static( Res ).



% Declares (synchronously) a new (basic) probe, to be seen as a result producer,
% and be created either from an actor or from a test case.
%
% - NameOptions is either:
%
%  - Name :: ustring(); i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
%  - or {Name :: ustring(), ProbeOptions} where ProbeOptions is a list of
%  pairs, in:
%
%   - {create_command_file_initially, boolean()}: if true, the gnuplot command
%   file will be written at probe start-up, thus preventing the taking into
%   account of any subsequent change in the rendering parameter (default: false)
%
%   - {deferred_data_writes, boolean()}: if true, received sample data will
%   stored in memory instead of being directly written to disk (default: false,
%   as the memory footprint might become significant) where Bool is true or
%   false
%
% - CurveNames :: [ ustring() ] is an (ordered) list containing the names (as
% plain strings) of each curve to be drawn (hence the probe will expect
% receiving data in the form of {Tick, {V1,V2,..}} afterwards). For example,
% CurveNames=["First curve", "Second curve"] will lead to expect receiving
% samples like: {MyTick, {ValueForFirstCurve, ValueForSecondCurve}}
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of {ZoneName,
% {ExtendedCurveNameOne,ExtendedCurveNameTwo}} entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example {"My Zone", {"First curve",'abscissa_bottom'}} defines a zone named
% "My Zone" and delimited by the curve named "First curve" and the abscissa axis
% (note: the order between the two elements defining a zone does not matter)
%
% - Title will be the graph (plot) title
%
% - MaybeXLabel (if any) will be the non-default label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
% - MaybeExtraSettingsTable is, if defined, a table holding extra probe settings
% of all sorts
%
% - MaybeTickDuration is, if defined, the actual duration (in simulation time)
% of a tick, for timestamped axis labels
%
% Returns either the PID of this newly created result probe (if the name of that
% probe is acknowledged as a wanted result by the result manager), or the
% 'non_wanted_probe' atom.
%
-spec declare_result_probe( name_options(), [ declared_curve_name() ],
		[ declared_zone() ], title(), label(), label(),
		maybe( settings_table() ), maybe( virtual_seconds() ) ) ->
										static_return( probe_ref() ).
declare_result_probe( NameOptions, CurveEntries, ZoneEntries, Title,
		MaybeXLabel, YLabel, MaybeExtraSettingsTable, MaybeTickDuration ) ->

	ActualName = case NameOptions of

		 { Name, _ProbeOptions } ->
			Name;

		 Name when is_list( Name ) ->
			Name

	end,

	ActualBinName = text_utils:string_to_binary( ActualName ),

	ResultManagerPid = class_ResultManager:get_result_manager(),

	ResultManagerPid ! { isResultProducerWanted,
						 [ ActualBinName, _Nature=basic_probe ], self() },

	Res = receive

		{ wooper_result, { true, Metadata } } ->
			% Created in current directory (i.e. the one for temporary data):
			%
			% (note that, despite the link, apparently at least in some cases a
			% probe failing after having thrown an exception does not crash this
			% actor creator - which is not supposed trapping EXITs...)
			%
			synchronous_new_link( NameOptions, CurveEntries, ZoneEntries,
				Title, MaybeXLabel, YLabel, Metadata, MaybeExtraSettingsTable,
				MaybeTickDuration );

		{ wooper_result, false } ->
			non_wanted_probe

	end,

	wooper:return_static( Res ).




% Declares (synchronously) a new (basic) probe, to be seen as a result producer,
% and be created directly from a test case.
%
% - NameOptions is either:
%
%  - Name :: ustring(), i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
%  - or {Name :: ustring(), ProbeOptions} where ProbeOptions is a list of
%  pairs, in:
%
%   - {create_command_file_initially, boolean()}: if true, the gnuplot command
%   file will be written at probe start-up, thus preventing the taking into
%   account of any subsequent change in the rendering parameter (default: false)
%
%   - {deferred_data_writes, boolean()}: if true, received sample data will
%   stored in memory instead of being directly written to disk (default: false,
%   as the memory footprint might become significant) where Bool is true or
%   false
%
% - CurveNames :: [ustring()] is an (ordered) list containing the names (as
% plain strings) of each curve to be drawn (hence the probe will expect
% receiving data in the form of {Tick, {V1,V2,..}} afterwards). For example,
% CurveNames=["First curve", "Second curve"] will lead to expect receiving
% samples like: {MyTick, {ValueForFirstCurve, ValueForSecondCurve}}
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of { ZoneName,
% {ExtendedCurveNameOne,ExtendedCurveNameTwo} } entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example {"My Zone", {"First curve", 'abscissa_bottom'}} defines a zone named
% "My Zone" and delimited by the curve named "First curve" and the abscissa axis
% (note: the order between the two elements defining a zone does not matter)
%
% - Title will be the graph (plot) title
%
% - MaybeXLabel (if any) will be the non-default label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
% Returns either the PID of this newly created test probe (if the name of that
% probe is acknowledged as a wanted result by the result manager), or the
% 'non_wanted_probe' atom.
%
-spec declare_test_probe( name_options(), [ declared_curve_name() ],
		[ declared_zone() ], title(), label(), label() ) ->
								static_return( probe_ref() ).
declare_test_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					MaybeXLabel, YLabel ) ->

	% Test and case probes are synonyms:
	Res = declare_case_probe( NameOptions, CurveEntries, ZoneEntries, Title,
		MaybeXLabel, YLabel, _MaybeExtraSettingsTable=undefined,
		_MaybeTickDuration=undefined ),

	wooper:return_static( Res ).



% Declares (synchronously) a new (basic) probe, to be seen as a result producer,
% and be created directly from a simulation case.
%
% See declare_test_probe/6 (just above) for more information.
%
-spec declare_case_probe( name_options(), [ declared_curve_name() ],
		 [ declared_zone() ], title(), label(), label() ) ->
								static_return( probe_ref() ).
declare_case_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					MaybeXLabel, YLabel ) ->

	Res = declare_case_probe( NameOptions, CurveEntries, ZoneEntries, Title,
		MaybeXLabel, YLabel, _MaybeExtraSettingsTable=undefined,
		_MaybeTickDuration=undefined ),

	wooper:return_static( Res ).



% Declares (synchronously) a new (basic) probe, to be seen as a result producer,
% and be created directly from a simulation case.
%
% See declare_test_probe/6 (above) for more information.
%
-spec declare_test_probe( name_options(), [ declared_curve_name() ],
		[ declared_zone() ], title(), label(), label(),
		maybe( settings_table() ), maybe( virtual_seconds() ) ) ->
								static_return( probe_ref() ).
declare_test_probe( NameOptions, CurveEntries, ZoneEntries, Title,
		MaybeXLabel, YLabel, MaybeExtraSettingsTable, MaybeTickDuration ) ->

	% Test and case probes are synonyms:
	Res = declare_case_probe( NameOptions, CurveEntries, ZoneEntries, Title,
		MaybeXLabel, YLabel, MaybeExtraSettingsTable, MaybeTickDuration ),

	wooper:return_static( Res ).



% Declares (synchronously) a new (basic) probe, to be seen as a result producer,
% and be created directly from a simulation case.
%
% See declare_test_probe/6 (above) for more information.
%
-spec declare_case_probe( name_options(), [ declared_curve_name() ],
		[ declared_zone() ], title(), label(), label(),
		maybe( settings_table() ), maybe( virtual_seconds() ) ) ->
								static_return( probe_ref() ).
declare_case_probe( NameOptions, CurveEntries, ZoneEntries, Title,
		MaybeXLabel, YLabel, MaybeExtraSettingsTable, MaybeTickDuration ) ->

	Res = case declare_result_probe( NameOptions, CurveEntries, ZoneEntries,
			Title, MaybeXLabel, YLabel, MaybeExtraSettingsTable,
			MaybeTickDuration ) of

		non_wanted_probe ->
			non_wanted_probe;

		ProbePid ->

			% Directly from the simulation case, we cannot work in the current
			% case directory: (preferring a directory under /tmp):

			Nodename = atom_to_list( node() ),

			% Removes the @host trailing part:
			SimulationName = string:substr( Nodename, 1,
											string:chr( Nodename, $@ ) - 1 ),

			FinalDir = "sim-diasca-case-"
				++ file_utils:convert_to_filename( SimulationName ),

			% (initial empty string allows to forge '/tmp/..'):
			%
			% (note: for these probes, which are generally not really numerous,
			% we prefer using /tmp even if another temporary directory was
			% specified - we do not want here to depend on the deployment
			% settings)
			%
			ProbeDir = filename:join( [ "", "tmp", FinalDir ] ),

			file_utils:create_directory( ProbeDir, create_parents ),

			ProbePid ! { setDirectory,
						 [ text_utils:string_to_binary( ProbeDir ) ] },

			ProbePid

	end,

	wooper:return_static( Res ).



% Deletes specified test probe (as returned by declare_test_probe/*,
% i.e. actually created or not).
%
-spec delete_test_probe( probe_ref() ) -> static_void_return().
delete_test_probe( Any ) ->
	% Synonyms:
	delete_case_probe( Any ),
	wooper:return_static_void().



% Deletes specified case probe (as returned by declare_case_probe/*,
% i.e. actually created or not).
%
-spec delete_case_probe( probe_ref() ) -> static_void_return().
delete_case_probe( non_wanted_probe ) ->
	wooper:return_static_void();

delete_case_probe( Pid ) ->
	Pid ! delete,
	wooper:return_static_void().



% Creates in the current directory a facility probe, i.e. a lingering probe, to
% be created (unilaterally) from a test case, and that will not be considered as
% a result.
%
% - NameOptions is either:
%
%  - Name :: ustring(), i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
%  - or {Name :: ustring(), ProbeOptions} where ProbeOptions is a list of
%  pairs, in:
%
%   - {create_command_file_initially, boolean()}: if true, the gnuplot command
%   file will be written at probe start-up, thus preventing the taking into
%   account of any subsequent change in the rendering parameter (default: false)
%
%   - {deferred_data_writes, boolean()}: if true, received sample data will
%   stored in memory instead of being directly written to disk (default: false,
%   as the memory footprint might become significant) where Bool is true or
%   false
%
% - CurveNames :: [ustring()] is an (ordered) list containing the names (as
% plain strings) of each curve to be drawn (hence the probe will expect
% receiving data in the form of {Tick, {V1,V2,..}} afterwards). For example,
% CurveNames=["First curve", "Second curve"] will lead to expect receiving
% samples like: {MyTick, {ValueForFirstCurve, ValueForSecondCurve}}
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of {ZoneName,
% {ExtendedCurveNameOne,ExtendedCurveNameTwo}} entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example {"My Zone", {"First curve", 'abscissa_bottom'}} defines a zone named
% "My Zone" and delimited by the curve named "First curve" and the abscissa axis
% (note: the order between the two elements defining a zone does not matter)
%
% - Title will be the graph (plot) title
%
% - MaybeXLabel (if any) will be the non-default label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
% Returns the PID of this newly created facility probe.
%
-spec create_facility_probe( name_options(), [ declared_curve_name() ],
			[ declared_zone() ], title(), label(), label() ) ->
									static_return( probe_pid() ).
create_facility_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					   MaybeXLabel, YLabel ) ->

	ProbeDirectory = file_utils:get_current_directory(),

	Res = create_facility_probe( NameOptions, CurveEntries, ZoneEntries, Title,
								 MaybeXLabel, YLabel, ProbeDirectory ),

	wooper:return_static( Res ).



% Creates a facility probe in the specified directory, i.e. a lingering probe,
% to be created (unilaterally) from a test case, and that will not to considered
% as a result.
%
% - NameOptions is either:
%
%  - Name :: ustring(), i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
%  - or {Name :: ustring(), ProbeOptions} where ProbeOptions is a list of
%  pairs, in:
%
%   - {create_command_file_initially, boolean()}: if true, the gnuplot command
%   file will be written at probe start-up, thus preventing the taking into
%   account of any subsequent change in the rendering parameter (default: false)
%
%   - {deferred_data_writes, boolean()}: if true, received sample data will
%   stored in memory instead of being directly written to disk (default: false,
%   as the memory footprint might become significant) where Bool is true or
%   false
%
% - CurveNames :: [ustring()] is an (ordered) list containing the names (as
% plain strings) of each curve to be drawn (hence the probe will expect
% receiving data in the form of {Tick, {V1,V2,..}} afterwards). For example,
% CurveNames=["First curve", "Second curve"] will lead to expect receiving
% samples like: {MyTick, {ValueForFirstCurve, ValueForSecondCurve}}
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of {ZoneName,
% {ExtendedCurveNameOne,ExtendedCurveNameTwo}} entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example {"My Zone", {"First curve", 'abscissa_bottom'}} defines a zone named
% "My Zone" and delimited by the curve named "First curve" and the abscissa axis
% (note: the order between the two elements defining a zone does not matter)
%
% - Title will be the graph (plot) title
%
% - MaybeXLabel (if any) will be the non-default label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
% - ProbeDirectory is the directory where the files for that probe will be
% written
%
% Returns the PID of this newly created facility probe.
%
-spec create_facility_probe( name_options(), [ declared_curve_name() ],
		[ declared_zone() ], title(), label(), label(), directory_path() ) ->
									static_return( probe_pid() ).
create_facility_probe( { Name, Options }, CurveEntries, ZoneEntries, Title,
					   MaybeXLabel, YLabel, ProbeDirectory ) ->

	% Overrides any previous probe directory definition:
	NewOptions = Options#probe_options{ register_as_tracked_producer=false,
										probe_directory=ProbeDirectory },

	Pid = synchronous_new_link( { Name, NewOptions }, CurveEntries,
					ZoneEntries, Title, MaybeXLabel, YLabel, _MetaData=[] ),

	wooper:return_static( Pid );


create_facility_probe( Name, CurveEntries, ZoneEntries, Title,
					   MaybeXLabel, YLabel, ProbeDirectory ) ->

	Options = #probe_options{ register_as_tracked_producer=false,
							  probe_directory=ProbeDirectory },

	Pid = synchronous_new_link( { Name, Options }, CurveEntries,
				ZoneEntries, Title, MaybeXLabel, YLabel, _MetaData=[] ),

	wooper:return_static( Pid ).




% Deletes specified facility probe (knowing that the other kinds of probes are
% results, and thus their life cycles are managed by the result manager).
%
-spec delete_facility_probe( probe_pid() ) -> static_void_return().
delete_facility_probe( ProbePid ) when is_pid( ProbePid ) ->

	%trace_utils:debug( "Deleting this facility probe." ),

	% Disable checking for this very specific case:
	ProbePid ! { setResultCollectedStatus, true },

	% This is necessarily a PID, not a 'non_wanted_probe' atom; synchronicity is
	% better here, to detect all failures:
	%
	wooper:delete_synchronously_instance( ProbePid ),

	wooper:return_static_void().




% Sends the specified sample data for the specified tick (not tick offset) to
% the targeted probe, based on the specified probe reference (first parameter),
% which is the value returned by the result manager in answer to the initial
% creation request for that probe.
%
% This parameter is either an actual PID (then data will be sent by this method)
% or the 'non_wanted_probe' atom (as potentially sent back by the result
% manager), in which case nothing will be done.
%
% Note: this static method is to be used for basic probes, not virtual ones.
%
-spec send_data( probe_ref(), probe_tick(), sample_data() ) ->
												static_void_return().
send_data( _ProbeRef=non_wanted_probe, _Tick, _Samples )  ->
	wooper:return_static_void();

% The guard should be useless here:
send_data( ProbePid, Tick, Samples ) when is_pid( ProbePid ) ->
	ProbePid ! { setData, [ Tick, Samples ] },
	wooper:return_static_void().



% Allows to define whether the probe report should be displayed to the user,
% after generation.
%
% Now superseded by the use of the result manager.
%
-spec generate_report_for( probe_pid() ) -> static_void_return().
generate_report_for( ProbePid ) ->

	case executable_utils:is_batch() of

		true ->
			ProbePid ! { generateReport, _DisplayWanted=false, self() };

		false ->
			ProbePid ! { generateReport, _DisplayWanted=true, self() }

	end,

	receive

		{ wooper_result, probe_report_generated } ->

			% No specific collection here, without the result manager, hence we
			% have to enable a successful check:
			%
			ProbePid ! { setResultProducedStatus, true },

			?notify_info( "Probe report correctly generated." ),

			wooper:return_static_void()

	end.




% Returns an actual, suitable probe name deriving from the specified one.
-spec get_actual_probe_name( probe_name_init() ) ->
									static_return( probe_name() ).
get_actual_probe_name( { EmitterName, _EmitterCategorization } ) ->

	% A preliminary version of the trace_categorize/1 macro: we extract the base
	% probe name so that some rules (ex: no dot inside) are enforced.

	wooper:return_static( get_actual_probe_name( EmitterName ) );


get_actual_probe_name( EmitterName ) ->

	% Dots are not allowed in probe names (whereas, for example, FQDN usually
	% have such characters):
	%
	NewName = text_utils:substitute( _SourceChar=$., _TargetChar=$:,
									 EmitterName ),

	wooper:return_static( NewName ).




% Section for helper functions (not methods).


% Waits for the feedback of the result manager, after this probe declared itself
% to it (after a call to its declareProbe/4 request).
%
-spec wait_result_declaration_outcome( probe_name(), wooper:state() ) ->
												wooper:state().
wait_result_declaration_outcome( ProbeName, State ) ->

	%trace_utils:info_fmt( "Waiting for the result declaration outcome for "
	%						"probe '~ts'.", [ ProbeName ] ),

	% Finally waits the answer from the result manager to a prior declareProbe/2
	% call or similar:
	%
	% (in terms of result selection, what is true now may not be true anymore
	% later, if the state of the result manager changes)
	%
	receive

		{ wooper_result, output_not_requested } ->

			?info_fmt( "The probe '~ts' would not produce an expected result.",
						[ ProbeName ] ),

			setAttribute( State, enabled_producer, false );


		{ wooper_result, output_requested } ->

			% Default is enabled_producer set to true:
			?info_fmt( "The probe '~ts' will produce an expected result.",
					   [ ProbeName ] ),

			State

	end.



% Returns a probe_settings record with specified informations (expressed as
% plain strings) and default values for the other fields.
%
-spec get_probe_settings( title(), label(), label(),
						  basic_utils:two_digit_version() ) -> probe_settings().
get_probe_settings( Title, MaybeXLabel, YLabel, GnuplotVersion ) ->

	{ Xtick, KeyOption } = get_basic_options( GnuplotVersion ),

	ActualXLabel = case MaybeXLabel of

		undefined ->
			"Simulation time";

		_ ->
			MaybeXLabel

	end,

	#probe_settings{
	   title=text_utils:string_to_binary( Title ),
	   x_tick=Xtick,
	   key_options=KeyOption,
	   %image_format=text_utils:string_to_binary( "svg" );
	   y_tick=text_utils:string_to_binary( "auto" ),
	   x_label=text_utils:string_to_binary( ActualXLabel ),
	   y_label=text_utils:string_to_binary( YLabel ),
	   x_range=undefined,
	   y_range=undefined,
	   plot_style=text_utils:string_to_binary( "linespoints" ),
	   fill_style=text_utils:string_to_binary( "empty" ) }.



% Returns some basic options, depending on the current gnuplot version.
%
% (helper, for code sharing)
%
-spec get_basic_options( basic_utils:two_digit_version() ) ->
								{ binary(), binary() }.
get_basic_options( GnuplotVersion ) ->

	% If using a very old version of gnuplot (ex: < 4.2), these key options
	% use default values:

	case basic_utils:compare_versions( GnuplotVersion,
									   get_gnuplot_reference_version() ) of

		second_bigger ->
			% Here we only have access to an older gnuplot:
			{ _Xtick2 = text_utils:string_to_binary( "auto" ),
			  _KeyOption2 = text_utils:string_to_binary( "" ) };

		_ ->
			% Here we have a recent enough gnuplot:
			{

			 % By default we prefer not having rotated ticks:

			 %_Xtick1=text_utils:string_to_binary( "rotate by - 45 auto" ),
			 %
			 % 'out' has been added to avoid that tick marks get hidden by any
			 % filled area in the plot (ex: boxes).
			 %
			 % We used to specify also 'out', yet stopped as it was wreaking
			 % havoc the layout and/or cropping done by gnuplot (leading to
			 % overlapping legend and truncated timestamps).
			 %
			 _Xtick1 = text_utils:string_to_binary(
						%"axis out mirror font \"sans,8\" auto" ),
						 "out mirror font \"sans,8\" auto" ),

			 % Extra size for box, otherwise may collide with inner text:
			 _KeyOption1 = text_utils:string_to_binary(
				"bmargin center horizontal width 0.5 height 0.5" ) }

			%image_format = text_utils:string_to_binary( "svg" );

	end.



% Adds back the index in the Names list, as read from the CurveEntries list,
% without changing the order of the Names list.
%
% Transforms plain strings in binaries as well.
%
% Ex: add_probe_index_back( [ "b", "c", "a" ], CurveEntries ) with
% CurveEntries=[ {3,<<"a">>}, {2,<<"b">>}, {1,<<"c">>} ] should return:
% [ {2,<<"b">>}, {1,<<"c">>}, {3,<<"a">>} ], i.e. the items of Names, in their
% original order in Names, with their index added back.
%
% (helper function)
%
-spec add_probe_index_back( [ string_curve_name() ], curve_entries() ) ->
									curve_entries().
add_probe_index_back( Names, CurveEntries ) ->
	add_probe_index_back( Names, CurveEntries, _Acc=[] ).


add_probe_index_back( _Names=[], _CurveEntries, Acc ) ->
	lists:reverse( Acc );

add_probe_index_back( [ Name | T ], CurveEntries, Acc ) ->

	BinName = text_utils:string_to_binary( Name ),

	% We do not check for duplicated names and removed ones resulting in a
	% correct length of the name list:
	%
	case lists:keyfind( BinName, 2, CurveEntries ) of

		false ->
			throw( { unknown_curve, Name } );

		{ Index, _Name, PlotSuffix } ->
			add_probe_index_back( T, CurveEntries,
				  [ { Index, BinName, PlotSuffix } | Acc ] )

	end.



% Interprets the creation-time probe options:
-spec interpret_options( probe_options() ) ->
			{ boolean(), boolean(), boolean(), maybe( directory_path() ) }.
interpret_options( _ProbeOptions=#probe_options{
			create_command_file_initially=CreateCommandFileInitially,
			deferred_data_writes=DeferredDataWrites,
			register_as_tracked_producer=IsTrackedProducer,
			probe_directory=ProbeDirectory } ) ->

	check_is_boolean( CreateCommandFileInitially,
					  create_command_file_initially ),

	check_is_boolean( DeferredDataWrites, deferred_data_writes ),

	check_is_boolean( IsTrackedProducer, register_as_tracked_producer ),

	{ ProbeDir, MaybeBinProbeDir }  = case ProbeDirectory of

		undefined ->
			% On a side note, results by default in having basic probes write
			% their (transient - as before transfer to the user node) files
			% (.dat, .p, .png) in the '/tmp/sim-diasca-CASE*/deployed-elements'
			% directory rather than its 'outputs' sibling:
			%
			{ file_utils:get_current_directory(), undefined };

		Dir ->
			{ Dir, text_utils:string_to_binary( Dir ) }

	end,

	{ CreateCommandFileInitially, DeferredDataWrites, IsTrackedProducer,
	  ProbeDir, MaybeBinProbeDir }.



% Updates specified probe settings with any extra settings specified.
-spec apply_extra_settings( maybe( settings_table() ), probe_settings(),
							wooper:state() ) ->
				  { probe_settings(), maybe( [ extra_curve_settings() ] ) }.
apply_extra_settings( _MaybeExtraSettingsTable=undefined, ProbeSettings,
					  _State ) ->
	{ ProbeSettings, _ExtraCurveSettings=undefined };

apply_extra_settings( ExtraSettingsTable, ProbeSettings, State ) ->

	% For plot style:
	{ GlobalPlotStyle, StyleShrunkTable } =
		table:extract_entry_with_defaults( _Key=global_plot_style,
						   _DefaultValue=linespoints, ExtraSettingsTable ),

	StyleProbeSettings = case GlobalPlotStyle of

		boxes ->
			NewExtraDefs = [
				text_utils:string_to_binary( "set style fill solid 0.5" )
							 | ProbeSettings#probe_settings.extra_defines ],
			ProbeSettings#probe_settings{
			  plot_style=text_utils:atom_to_binary( boxes ),
			  extra_defines=NewExtraDefs };

		OtherPlotStyle ->
			ProbeSettings#probe_settings{
			  plot_style=text_utils:atom_to_binary( OtherPlotStyle ) }

	end,


	% For canvas size:
	DefaultCanvasSize = { ?default_canvas_width, ?default_canvas_height },

	{ CanvasSize, CanvasSizeShrunkTable } =
		table:extract_entry_with_defaults( canvas_size,
						DefaultCanvasSize, StyleShrunkTable ),

	CanvasSizeProbeSettings = case CanvasSize of

		{ CWidth, CHeight } when is_integer( CWidth )
								 andalso is_integer( CHeight ) ->
			StyleProbeSettings#probe_settings{ canvas_width=CWidth,
											   canvas_height=CHeight };

		OtherCSize ->
			throw( { invalid_canvas_size, OtherCSize } )

	end,


	% For curve colors:
	{ MaybeCurveColors, CurveColorShrunkTable } =
		table:extract_entry_with_defaults( curve_colors,
			_DefaultCurveColors=undefined, CanvasSizeShrunkTable ),

	% May be extended beyond colors in the future:
	MaybeExtraCurveSettings = MaybeCurveColors,


	% For tick (note the singular) options:
	DefaultTickOptions = { undefined, undefined },

	{ TickOptions, TickOptShrunkTable } =
		table:extract_entry_with_defaults( tick_options,
						DefaultTickOptions, CurveColorShrunkTable ),

	TickOptProbeSettings =
		update_tick_options( TickOptions, CanvasSizeProbeSettings ),

	% For ticks (note the plural) options:

	DefaultTicksOptions = { undefined, undefined },

	{ TicksOptions, TicksOptShrunkTable } =
		table:extract_entry_with_defaults( ticks_options,
						DefaultTicksOptions, TickOptShrunkTable ),

	TicksOptProbeSettings =
		update_ticks_options( TicksOptions, TickOptProbeSettings ),


	% For timestamp time format:
	{ TimeFormatOpt, TimeFmtShrunkTable } =
		table:extract_entry_with_defaults( timestamp_time_format,
				_DefaultTimeFmt=undefined, TicksOptShrunkTable ),

	% Precise checking done later:
	TimeFmtProbeSettings = case is_atom( TimeFormatOpt ) of

		true ->
			TicksOptProbeSettings#probe_settings{
			  x_ticks_timestamp_time_format=TimeFormatOpt };

		false ->
			throw( { invalid_timestamp_time_format, TimeFormatOpt } )

	end,


	% Add any extra setting you want to support here.

	% Finally:
	FinalSettingsTable = TimeFmtShrunkTable,
	FinalProbeSettings = TimeFmtProbeSettings,

	% Checking that all settings were taken into account:
	case table:is_empty( FinalSettingsTable ) of

		true ->
			{ FinalProbeSettings, MaybeExtraCurveSettings };

		false ->
			?error_fmt( "Unexpected extra settings were specified: ~ts",
						[ table:to_string( FinalSettingsTable ) ] ),
			throw( { unexpected_probe_extra_settings,
					 table:keys( FinalSettingsTable ) } )

	end.


% Updates the probe settings based on specified tick options.
-spec update_tick_options( { tick_option(), tick_option() },
						   probe_settings() ) -> probe_settings().
update_tick_options( _TickOptions={ XtickOpt, YtickOpt },
		ProbeSettings=#probe_settings{ x_tick=Xtick, y_tick=Ytick } ) ->

	NewXtick = text_utils:bin_format( "~ts ~ts",
					[ Xtick, get_tick_option( XtickOpt ) ] ),

	NewYtick = text_utils:bin_format( "~ts ~ts",
					[ Ytick, get_tick_option( YtickOpt ) ] ),

	ProbeSettings#probe_settings{ x_tick=NewXtick, y_tick=NewYtick };

update_tick_options( TickOptions, _ProbeSettings ) ->
	throw( { invalid_tick_options, TickOptions } ).



% Returns the string setting corresponding to specified tick option.
-spec get_tick_option( maybe( tick_option() ) ) -> ustring().
get_tick_option( _MaybeTickOption=undefined ) ->
	"";

get_tick_option( _TickOption=rotate_cw ) ->
	?rotate_cw_tick_label_option;

get_tick_option( _TickOption=rotate_ccw ) ->
	?rotate_ccw_tick_label_option;

get_tick_option( OtherTickOption ) ->
	throw( { invalid_tick_option, OtherTickOption } ).



% Updates the probe settings based on specified ticks options.
-spec update_ticks_options( { ticks_option(), ticks_option() },
							probe_settings() ) -> probe_settings().
update_ticks_options( _TicksOptions={ MaybeXticksOpt, MaybeYticksOpt },
					  ProbeSettings ) ->

	NewXticks = case MaybeXticksOpt of

		undefined ->
			ProbeSettings#probe_settings.x_ticks;

		XticksStr ->
			text_utils:string_to_binary( XticksStr )

	end,

	NewYticks = case MaybeYticksOpt of

		undefined ->
			ProbeSettings#probe_settings.y_ticks;

		YticksStr ->
			teyt_utils:string_to_binary( YticksStr )

	end,

	ProbeSettings#probe_settings{ x_ticks=NewXticks, y_ticks=NewYticks }.



% Updates the curve entries with any specified extra settings.
-spec update_curve_entries( [ curve_entry() ],
			maybe( [ extra_curve_settings() ] ) ) -> [ curve_entry() ].
update_curve_entries( CurveEntries, _MaybeExtraCurveSettings=undefined ) ->
	CurveEntries;

update_curve_entries( CurveEntries, ExtraCurveSettings ) ->
	CurveCount = length( CurveEntries ),
	case length( ExtraCurveSettings ) of

		CurveCount ->
			ok;

		ExtraSetCount ->
			throw( { invalid_extra_curve_setting_count, ExtraCurveSettings,
					 CurveEntries, { ExtraSetCount, CurveCount } } )
	end,

	[ update_curve_entry( CurveE, MaybeExtraSet )
	  || { CurveE, MaybeExtraSet } <- lists:zip( CurveEntries,
												 ExtraCurveSettings ) ].


% (helper)
update_curve_entry( CurveE, _MaybeExtraSet=undefined ) ->
	CurveE;

update_curve_entry( _CurveE={ CIdnex, CName, CPlotSuffix }, ExtraSetStr )
  when is_list( ExtraSetStr ) ->
	NewCPlotSuffix = text_utils:bin_format( "~ts lt rgb \"#~ts\"",
											[ CPlotSuffix, ExtraSetStr ] ),
	{ CIdnex, CName, NewCPlotSuffix };

update_curve_entry( _CurveE, ExtraSet ) ->
	throw( { invalid_extra_curve_setting, ExtraSet } ).



% Checks that the specified variable is a boolean.
check_is_boolean( Var, _VarName ) when is_boolean( Var ) ->
	ok;

check_is_boolean( Var, VarName ) ->
	throw( { boolean_value_expected_for, VarName, Var } ).



% Generates the appropriate gnuplot command file.
%
% Returns an updated state.
%
% (helper)
%
-spec generate_command_file( wooper:state() ) -> wooper:state().
generate_command_file( State ) ->

	Name = text_utils:binary_to_string( ?getAttr(name) ),

	ProbeDir = ?getAttr(probe_dir),

	CommandFileName = get_command_filename( Name, ProbeDir ),

	case ?getAttr(command_file_up_to_date)
			 andalso file_utils:is_existing_file( CommandFileName ) of

		true ->
			State;

		false ->

			Settings = ?getAttr(settings),

			CurveEntries = ?getAttr(curve_entries),

			ZoneEntries = ?getAttr(zone_entries),

			IsTimestamped = case ?getAttr(maybe_tick_duration) of

				undefined ->
					false;

				_ ->
					true

			end,

			% Returned path ignored:
			generate_command_file( Name, Settings, CurveEntries, ZoneEntries,
								   IsTimestamped, ProbeDir ),

			setAttribute( State, command_file_up_to_date, true )

	end.





% Generates unconditionally the appropriate gnuplot command file.
%
% Returns the name, as a plain string, of the command file.
%
% Helper function defined to be shared with the data-logger.
%
-spec generate_command_file( ustring(), probe_settings(), curve_entries(),
	   zone_entries(), boolean(), directory_path() ) -> file_name().
generate_command_file( Name, Settings, CurveEntries, ZoneEntries,
					   IsTimestamped, ProbeDir ) ->

	%trace_utils:debug_fmt( "generate_command_file for probe '~ts'.",
	%   [ Name ] ),

	LabelDefs = get_label_definitions( Settings#probe_settings.labels ),

	ExtraDefs = text_utils:join( _Sep="\n",
		[ text_utils:binary_to_string( D )
		  || D <- Settings#probe_settings.extra_defines ] ),

	DataFilename = get_data_filename( Name ),

	XrangeOpt = get_x_range_option( Settings ),

	YrangeOpt = get_y_range_option( Settings ),

	% Corresponding to the '*ticks' counterparts, not the base '*tick' ones:
	XticksOpt = get_xticks_option( Settings ),
	YticksOpt = get_yticks_option( Settings ),

	check_probe_directory( ProbeDir ),

	PNGFilename = get_report_filename( Name ),

	CommandFilename = get_command_filename( Name, ProbeDir ),

	% Possible race condition if two processes try to create it at once:
	file_utils:remove_file_if_existing( CommandFilename ),

	% No 'delayed_write' I/O option useful here:
	File = file_utils:open( CommandFilename, ?base_open_flags ),

	%trace_utils:debug_fmt( "Generating command file '~ts'.",
	%                       [ CommandFilename ] ),

	% Changes shall be applied if using timestamps rather than raw ticks:
	{ PreambleStr, CurveOffset } =
		get_timestamp_settings( Settings, IsTimestamped ),


	PlotCommand = get_plot_command( Settings, CurveEntries, CurveOffset,
									ZoneEntries, DataFilename ),

	% Use 'undefined' in sample if not having available data for an element.
	% Set terminal png *transparent* could be used as well.
	%
	file_utils:write_ustring( File,
		"~ts~n"
		"set autoscale~n"
		"unset log~n"
		"set grid~n"
		"set style data ~ts~n"
		"set style fill ~ts~n"
		"set key box ~ts~n"
		"set pointsize ~B~n"
		"set xtic ~ts~n"
		"set ytic ~ts~n"

		% Note that {x,y}tics options shall be specified *after* {x,y}tic ones
		% in order to be taken into account:
		%
		"~ts~n"
		"~ts~n"

		"~ts~n"
		"~ts~n"

		"set title \"~ts\" noenhanced~n"

		% Newline added, otherwise the label of the X axis may collide with the
		% upper part of the box of the key:
		%
		"set xlabel \"~ts\\n\"~n"
		%"set xlabel \"~ts\" offset 0,2~n"

		"set ylabel \"~ts\"~n"
		"set datafile missing 'undefined'~n"
		"set terminal ~ts size ~B, ~B~n"
		"~ts~n"
		"~ts~n"
		"set output \"~ts\"~n"
		"~ts",
		[ PreambleStr,
		  Settings#probe_settings.plot_style,
		  Settings#probe_settings.fill_style,
		  Settings#probe_settings.key_options,
		  Settings#probe_settings.point_size,
		  Settings#probe_settings.x_tick,
		  Settings#probe_settings.y_tick,
		  XticksOpt,
		  YticksOpt,
		  XrangeOpt,
		  YrangeOpt,
		  Settings#probe_settings.title,
		  Settings#probe_settings.x_label,
		  Settings#probe_settings.y_label,
		  Settings#probe_settings.image_format,
		  Settings#probe_settings.canvas_width,
		  Settings#probe_settings.canvas_height,
		  LabelDefs,
		  ExtraDefs,
		  PNGFilename,
		  PlotCommand ] ),

	file_utils:close( File ),

	CommandFilename.



% Returns the full path to the command file corresponding to specified settings.
%
% (helper)
%
get_command_filename( Name, ProbeDir ) ->
	file_utils:join( ProbeDir, get_command_filename( Name ) ).



% Generates the appropriate file containing probe data.
generate_data_file( State ) ->

	%trace_utils:debug_fmt( "Generating data file '~ts'.",
	%                       [ ?getAttr(data_filename) ] ),

	% Sanity check:
	undefined = ?getAttr(data_file),

	DataTable = ?getAttr(data_table),

	case DataTable of

		[] ->
			throw( { no_available_data_sample,
					 class_TraceEmitter:get_plain_name( State ) } );

		_NonEmpty ->
			ok

	end,

	CurveCount = ?getAttr(curve_count),

	%trace_utils:debug_fmt( "Data table: ~p", [ DataTable ] ),

	RowFormatStr = case ?getAttr(row_format_string) of

		undefined ->
			forge_format_string_for( CurveCount );

		RowFStr ->
			RowFStr

	end,

	FormattedData = format_rows( DataTable, CurveCount, RowFormatStr ),

	DataFilename = text_utils:binary_to_string( ?getAttr(data_filename) ),

	file_utils:remove_file_if_existing( DataFilename ),

	% delayed_write would not be terribly useful here, if not
	% counter-productive:
	%
	File = file_utils:open( DataFilename, ?base_open_flags ),

	write_header( File, ?getAttr(curve_entries), ?getAttr(zone_entries),
				  ?getAttr(settings), ?getAttr(name), ?getAttr(meta_data) ),

	file_utils:write_ustring( File, FormattedData ),

	file_utils:close( File ).



% Writes the probe header to the data file.
-spec write_header( file(), curve_entries(), zone_entries(),
					probe_settings(), probe_name(),
					class_ResultManager:meta_data() ) -> void().
write_header( File, CurveEntries, ZoneEntries, Settings, Name, Metadata ) ->

	{ { Year, Month, Day }, { Hour, Minute, Second } } =
		time_utils:get_timestamp(),

	% Curves might have been reordered, of course we want the order of the names
	% to match the order in the data samples, so we reorder according to the
	% curve index (first element of the curve entry):
	%
	ReorderedCurveEntries = lists:keysort( _Index=1, CurveEntries ),

	%trace_utils:debug_fmt( "Listed curve entries: ~p~nReordered: ~p.",
	%						[ CurveEntries, ReorderedCurveEntries ] ),

	CurveDescriptions = format_curve_info( ReorderedCurveEntries, _Acc=[] ),

	%trace_utils:debug_fmt( "Curve descriptions: ~p.", [ CurveDescriptions ] ),

	ZoneDescriptions = format_zone_info( ZoneEntries ),

	Title = Settings#probe_settings.title,

	MetadataAllStrings = [ text_utils:binary_to_string( BinText )
						   || { _Key, BinText } <- Metadata ],

	MetadataString = text_utils:strings_to_string( MetadataAllStrings,
												   _Sep="# - " ),

	file_utils:write_ustring( File,
		"# This time series data file has been written on ~B/~B/~B, at "
		"~B:~2..0B:~2..0B, on~n"
		"# host ~ts (node: ~ts).~n~n"
		"# Probe name: '~ts'.~n"
		"# Probe title: '~ts'.~n~n"
		"# Associated meta-data: ~ts~n~n"
		"# First column corresponds to the abscissa, "
		"expressed in tick offsets.~n"
		"# Next columns correspond to following curve names "
		"(in that order):~n~ts~n"
		"# ~ts",
		[ Day, Month, Year, Hour, Minute, Second,
		  net_utils:localhost(), net_utils:localnode(), Name, Title,
		  MetadataString, CurveDescriptions, ZoneDescriptions ] ).


% (helper)
format_curve_info( _CurveEntries=[], Acc ) ->
	lists:reverse( Acc );

format_curve_info( [ _CurveEntry={ Num, BinName, _BinPlotSuffix } | T ],
				   Acc ) ->
	Entry = text_utils:format( "# - curve #~B: '~ts'~n", [ Num, BinName ] ),
	format_curve_info( T, [ Entry | Acc ] ).


% (helper)
format_zone_info( _ZoneInfoList=[] ) ->
	text_utils:format( "No zone defined.~n~n", [] );

format_zone_info( ZoneInfoList ) ->
	text_utils:format( "Following zones were defined:~n"
				  ++ format_zone_info( ZoneInfoList, _Acc=[] )
				  ++ text_utils:format( "~n~n", [] ), [] ).



% (helper)
format_zone_info( _ZoneInfoList=[], Acc ) ->
	lists:reverse( Acc );

format_zone_info( [ { BinName, {FirstBound, SecondBound} } | T ], Acc ) ->
	Entry = text_utils:format( "# - zone '~ts', extending from ~p to ~p~n",
							   [ BinName, FirstBound, SecondBound ] ),
	format_zone_info( T, [ Entry | Acc ] ).



% Triggers unconditionally an update of the command file, if requested, and
% returns an updated state.
%
% (helper)
%
trigger_command_file_update( _UpdateRequested=false, State ) ->
	State;

trigger_command_file_update( _UpdateRequested=true, State ) ->

	% Forces the update:
	generate_command_file(
		setAttribute( State, command_file_up_to_date, false ) ).



% Returns the gnuplot command filename.
-spec get_command_filename( probe_name() ) -> file_name().
get_command_filename( Name ) ->
	file_utils:convert_to_filename( Name ++ ".p" ).



% Returns the gnuplot data filename.
-spec get_data_filename( probe_name() ) -> file_name().
get_data_filename( Name ) ->
	file_utils:convert_to_filename( Name ++ ".dat" ).



% Returns the report filename.
-spec get_report_filename( probe_name() ) -> file_name().
get_report_filename( Name ) ->
	file_utils:convert_to_filename( Name ++ ".png" ).
	%file_utils:convert_to_filename( Name ++ ".svg" ).



% Returns a format string suitable for the writing of corresponding samples.
-spec forge_format_string_for( curve_count() ) ->
									text_utils:format_string().
forge_format_string_for( CurveCount ) ->

	% Timestamp (binary) string (corresponding to raw tick or actual textual
	% timestamp, if available), then as many values as needed (some of which
	% being possibly 'undefined', hence '~p'):
	%
	"~ts " ++ lists:flatten( lists:duplicate( CurveCount, "~w " ) )
		++ " ~n".



% Formats specified rows according to specified format.
format_rows( DataTable, CurveCount, RowFormatString ) ->

	%trace_utils:debug_fmt(
	%  "Row format string: '~ts'; curve count: ~B;~ndata table: ~p.",
	%  [ RowFormatString, CurveCount, DataTable ] ),

	format_rows( DataTable, CurveCount, RowFormatString, _Acc=[] ).



% (helper)
format_rows( _DataTable=[], _CurveCount, _RowFormatString, Acc ) ->
	lists:flatten( Acc );

format_rows( _DataTable=[ { TimestampBinStr, Sample } | T ], CurveCount,
			 RowFormatString, Acc ) ->


	RowStr = format_row( TimestampBinStr, Sample, CurveCount, RowFormatString ),

	%trace_utils:debug_fmt( "RowStr: ~ts", [ RowStr ] ),

	format_rows( T, CurveCount, RowFormatString, [ RowStr | Acc ] ).



% Returns a formatted version of specified sample row.
%
% Defined also for reuse (ex: by the datalogger).
%
-spec format_row( ustring(), sample_data(), curve_count(),
				  text_utils:format_string() ) -> ustring().
format_row( TimestampString, Sample, CurveCount, RowFormatString ) ->

	SampleValues = tuple_to_list( Sample ),

	% As samples may contain an increasing number of values over time:
	case size( Sample ) of

		% Simple, standard case:
		CurveCount ->
			text_utils:format( RowFormatString,
							   [ TimestampString | SampleValues ] );

		% Lacking (undefined) values shall be added:
		VCount when VCount < CurveCount ->
			UndefCount = CurveCount - VCount,
			text_utils:format( RowFormatString,
				[ TimestampString | SampleValues ]
					++ lists:duplicate( UndefCount, undefined ) );

		LargerCount ->
			throw( { too_many_sample_values, {got,LargerCount},
					 {expected,CurveCount}, {sample,Sample},
					 {timestamp,TimestampString} } )

	end.



% Used by third-party modules.
-spec write_row( file(), timestamp_bin_string(), sample_data() ) ->
							void().
write_row( File, TimestampBinString, DataTuple ) ->
	RowFormatString = forge_format_string_for( size( DataTuple ) ),
	write_row( File, RowFormatString, TimestampBinString, DataTuple ).


% Used for direct data writing.
-spec write_row( file(), text_utils:format_string(),
				 timestamp_bin_string(), sample_data() ) -> void().
write_row( File, RowFormatString, TimestampBinString, DataTuple ) ->
	file_utils:write_ustring( File, RowFormatString,
		[ TimestampBinString | tuple_to_list( DataTuple ) ] ).



% Creates or updates an entry for the specified curve.
-spec setFilledCurvesOptions( wooper:state(), ustring(), curve_index() ) ->
									oneway_return().
setFilledCurvesOptions( State, CurveName, ColumnSpecifier ) ->
	NewState = setFilledCurvesOptions( State, CurveName, ColumnSpecifier,
									   _GenerateFile=false ),
	wooper:return_state( NewState ).




% Creates or updates an entry for the specified curve.
-spec setFilledCurvesOptions( wooper:state(), ustring(), curve_index(),
							  boolean() ) -> oneway_return().
setFilledCurvesOptions( State, CurveName, ColumnSpecifier, GenerateFile ) ->

	SpecifiedFilledCurve = { CurveName,
							 _PlotStyle="filledcurves",
							 _ColumnSpecifier=ColumnSpecifier },

	FilledCurveList = ?getAttr(filled_curve_list),

	% Verifying whether the curve exists already in the filled curve list; if
	% yes, its setting option will be replaced by the new one:
	%
	UpdatedList = case lists:keyfind( CurveName, 1, FilledCurveList ) of

			false ->
				% Just appends:
				[ SpecifiedFilledCurve | FilledCurveList ];

			Tuple ->
				% Erases and replaces:
				NewList = lists:delete( Tuple, FilledCurveList ),
				[ SpecifiedFilledCurve | NewList ]

		end,

	UpdatedState = setAttribute( State, filled_curve_list, UpdatedList ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	wooper:return_state( CommandState ).



% Returns the Gnuplot command appropriate to render all registered labels.
-spec get_label_definitions( [ probe_label() ] ) -> command_element().
get_label_definitions( Labels ) ->
	text_utils:join( _Separator="\n",
					 get_label_definitions( Labels, _Acc=[] ) ).


% (helper)
get_label_definitions( _Labels=[], Acc ) ->
	% Nothing to reverse, labels will end up being rendered in the order they
	% were specified:
	Acc;

get_label_definitions( [ #probe_label{ location={ X, Y }, text=BinText,
	  color=Color, position=Position, orientation=Orientation } | T ], Acc ) ->

	% For a list of supported colors, see:
	% www.uni-hamburg.de/Wiss/FB/15/Sustainability/schneider/gnuplot/colors.htm
	%
	ActualColor = gui_color:get_color_for_gnuplot( Color ),

	LabelString = text_utils:format(
		"set label \"~ts\" at ~p,~p ~ts ~ts textcolor rgbcolor \"~ts\"",
		[ text_utils:binary_to_string( BinText ), X, Y, Position,
		  get_formatted_orientation( Orientation ), ActualColor] ),

	get_label_definitions( T, [ LabelString | Acc ] ).



% Returns the settings for fine control of the major (labeled) ticks on the
% abscissa axis, as read from the specified settings.
%
-spec get_xticks_option( probe_settings() ) -> command_element().
get_xticks_option( #probe_settings{ x_ticks=undefined } ) ->
	"# No x_ticks set.";

get_xticks_option( #probe_settings{ x_ticks=XticksBinSpec } )
  when is_binary( XticksBinSpec ) ->
	text_utils:format( "set xtics ~ts~n", [ XticksBinSpec ] );

get_xticks_option( #probe_settings{ x_ticks=Other } ) ->
	throw( { invalid_x_ticks_option, Other } ).



% Returns the settings for fine control of the major (labeled) ticks on the
% ordinate axis, as read from the specified settings.
%
-spec get_yticks_option( probe_settings() ) -> command_element().
get_yticks_option( #probe_settings{ y_ticks=undefined } ) ->
	"# No y_ticks set.";

get_yticks_option( #probe_settings{ y_ticks=YticksBinSpec } )
  when is_binary( YticksBinSpec ) ->
	text_utils:format( "set ytics ~ts~n", [ YticksBinSpec ] );

get_yticks_option( #probe_settings{ y_ticks=Other } ) ->
	throw( { invalid_y_ticks_option, Other } ).



% Returns the abscissa range options as read from the specified settings.
-spec get_x_range_option( probe_settings() ) -> command_element().
get_x_range_option( Settings ) ->

	case Settings#probe_settings.x_range of

		undefined ->
			"# No x_range set.";

		{ MinX, MaxX } when is_number( MinX ) andalso is_number( MaxX ) ->
			text_utils:format( "set xrange [~w:~w]", [ MinX, MaxX ] )

	end.



% Returns the ordinate range options as read from the specified settings.
-spec get_y_range_option( probe_settings() ) -> command_element().
get_y_range_option( Settings ) ->

	case Settings#probe_settings.y_range of

		undefined ->
			"# No y_range set.";

		{ MinY, MayY } when is_number( MinY ) andalso is_number( MayY ) ->
			text_utils:format( "set yrange [~w:~w]", [ MinY, MayY ] )

	end.



% Returns the option to control the labels of the major ticks on the x axis, as
% read from the specified settings.
%
-spec get_x_ticks_option( probe_settings() ) -> command_element().
get_x_ticks_option( Settings ) ->

	case Settings#probe_settings.x_ticks of

		undefined ->
			"# No label set for the major ticks on the x axis.";

		XTicksInfo ->
			text_utils:format( "set xtics ~ts", [ XTicksInfo ] )

	end.



% Returns the option to control the labels of the major ticks on the y axis, as
% read from the specified settings.
%
-spec get_y_ticks_option( probe_settings() ) -> command_element().
get_y_ticks_option( Settings ) ->

	case Settings#probe_settings.y_ticks of

		undefined ->
			"# No label set for the major ticks on the y axis.";

		YTicksInfo ->
			text_utils:format( "set ytics ~ts", [ YTicksInfo ] )

	end.



% Returns the appropriate settings, depending on whether the abscissa axis
% gathers timestamps or not.
%
-spec get_timestamp_settings( probe_settings(), boolean() ) ->
									{ command_element(), curve_offset() }.
get_timestamp_settings(
		#probe_settings{ x_ticks_timestamp_time_format=TimeFmt },
		_IsTimestamped=true ) ->

	% Also a check:
	UserTimeFormat = case TimeFmt of

		undefined ->
			_DefaultTimeFormat=double_line;

		single_line ->
			single_line;

		double_line ->
			double_line;

		OtherTimeFormat ->
			throw( { invalid_timestamp_time_format, OtherTimeFormat } )

	end,

	TimeFormatStr = case UserTimeFormat of

		single_line ->
			"\"%d %b %Y %H:%M:%S\"";

		double_line ->
			"\"%d %b %Y\\n\\n%H:%M:%S\""

	end,

	PreambleStr = text_utils:format(
					"set xdata time~n"

					% As read from the data (our standard format):
					"set timefmt \"%Y/%m/%d %H:%M:%S\"~n"

					 % As shall be displayed:
					"set format x ~ts~n", [ TimeFormatStr ] ),

	{ PreambleStr, _CurveOffset=1 };


get_timestamp_settings( _Probe_settings, _IsTimestamped=false ) ->
	{ _PreambleStr="", _CurveOffset=0 }.



% Returns the Gnuplot command appropriate to render that probe output.
%
% Defines one plot curve per declared curve, with current plot settings, and
% defines as well any specified zone.
%
-spec get_plot_command( probe_settings(), curve_entries(), curve_offset(),
						zone_entries(), file_name() ) -> command_element().
get_plot_command( _Settings, _CurveEntries=[], _CurveOffset, _ZoneEntries=[],
				  _DataFilename ) ->
		throw( no_curve_nor_zone_defined );

get_plot_command( Settings, CurveEntries, CurveOffset, ZoneEntries,
				  DataFilename ) ->

	% Typical expected output:
	%
	% plot 'silver.dat' using 1:2 with lines, 'silver.dat' using 1:3 with lines,
	% 'silver.dat' using 1:2:3 with filledcurves

	% For us it is: "plot " ++ tl( join( _Prefix=", 'silver.dat' using 1:",
	%  [ "2 with lines", "3 with lines", "2:3 with filledcurves" ]
	%
	% (knowing that tl is used to remove the prefix head (','), i.e. the first
	% extra comma)
	%
	% So:

	Prefix = text_utils:format( ", \"~ts\" using 1:", [ DataFilename ] ),

	% We prefer not rendering curves that are used to delimit zones:
	FilteredCurveEntries =
		remove_zone_specific_curves( CurveEntries, ZoneEntries ),

	CurvePlots = get_plot_commands_for_curves( FilteredCurveEntries,
											   CurveOffset, Settings ),

	ZonePlots = get_plot_commands_for_zones( ZoneEntries, CurveOffset,
											 Settings ),

	% Note that we specify the zones first, otherwise the curves would be hidden
	% below:
	%
	JoinedCommand = text_utils:join( Prefix, ZonePlots ++ CurvePlots ),

	% Two tl to remove prefix head (i.e. ", "):
	text_utils:format( "plot ~ts~ts~n", [ tl( tl( Prefix ) ), JoinedCommand ] ).



% Returns a list of curve entries in which there are no more curves that are
% used to define a zone among the ones specified.
%
-spec remove_zone_specific_curves( curve_entries(), zone_entries() ) ->
											curve_entries().
remove_zone_specific_curves( CurveEntries, ZoneEntries ) ->

	CurvesToRemove = select_curves( ZoneEntries, _Acc=[] ),

	Selector = fun( { CurveIndex, _CurveName, _CurvePlotSuffix } ) ->
					   not lists:member( CurveIndex, CurvesToRemove ) end,

	lists:filter( Selector, CurveEntries ).



% Selects all curve indexes that are mentioned (possibly with duplicates).
select_curves( _ZoneEntries=[], Acc ) ->
	Acc;

select_curves( _ZoneEntries=[ { _ZoneName, { abscissa_top, C } } | T ], Acc ) ->
	select_curves( T, [ C | Acc ] );

select_curves( _ZoneEntries=[ { _ZoneName, { abscissa_bottom, C } } | T ],
			   Acc ) ->
	select_curves( T, [ C | Acc ] );

select_curves( _ZoneEntries=[ { _ZoneName, { C1, C2 } } | T ], Acc ) ->
	select_curves( T, [ C1, C2 | Acc ] ).



-spec get_plot_commands_for_curves( curve_entries(), curve_offset(),
									probe_settings() ) -> [ command_element() ].
get_plot_commands_for_curves( CurveEntries, CurveOffset, Settings ) ->

	% After some potential reordering, curve entries might be:
	% [ {3,<<"c">>}, {1,<<"a">>}, {2,<<"b">>} ]
	%
	% We expect: [ "4 title \"c\"", "2 title \"a\"", "3 title \"b\"" ]
	%
	% (note that each curve index is incremented, as the first column is the
	% tick)
	%
	% We simply write them in-order, with an appropriate title:
	[ get_curve_command( C, CurveOffset, Settings ) || C <- CurveEntries ].



% Returns a command element suitable to render specified curve.
%
% (helper)
%
-spec get_curve_command( curve_entry(), curve_offset(), probe_settings() ) ->
								command_element().
get_curve_command( { CurveIndex, BinCurveName, BinPlotSuffix }, CurveOffset,
				   _Settings ) ->

	Title = text_utils:binary_to_string( BinCurveName ),

	% +1 to account for the abscissa (time) first field.
	text_utils:format( "~B title \"~ts\" ~ts",
		[ CurveIndex + CurveOffset + 1, Title, BinPlotSuffix ] ).




% Returns command elements suitable to render specified zones.
%
% (helper)
%
-spec get_plot_commands_for_zones( zone_entries(), curve_offset(),
								   probe_settings() ) -> [ command_element() ].
get_plot_commands_for_zones( ZoneEntries, CurveOffset, Settings ) ->

	% Zone entries are a list of:
	% {BinZoneName, {ExtendedCurveName1, ExtendedCurveName2}}
	%
	% We expect returned values to be either "3:5 with filledcurves" (for a zone
	% between curves 2 and 4) or "3 with filledcurves x1" (for a zone between
	% curve 2 and the abscissa axis):
	%
	[ get_zone_command( Z, CurveOffset, Settings ) || Z <- ZoneEntries ].



% Returns a command element suitable to render specified zone.
%
% (helper)
%
-spec get_zone_command( zone_definition(), curve_offset(), probe_settings() ) ->
								command_element().
get_zone_command( _ZoneEntry={ BinZoneName,
							   { FirstExtendedCurve, SecondExtendedCurve } },
				  CurveOffset, _Settings ) ->

	FirstPart = case FirstExtendedCurve of

		'abscissa_top' ->
			% The other is necessarily an index (+1, as the first column is the
			% tick):
			%
			ActualCurveIndex = SecondExtendedCurve + CurveOffset + 1,
			text_utils:format( "~B with filledcurves x2",
							   [ ActualCurveIndex ] );

		'abscissa_bottom' ->
			% The other is necessarily an index (+1, as the first column is the
			% tick):
			%
			ActualCurveIndex = SecondExtendedCurve + CurveOffset + 1,
			text_utils:format( "~B with filledcurves x1",
							   [ ActualCurveIndex ] );

		_BinCurveName ->
			ActualFirstCurveIndex = FirstExtendedCurve + 1,
			ActualSecondCurveIndex = SecondExtendedCurve + 1,
			text_utils:format( "~B:~B with filledcurves",
				[ ActualFirstCurveIndex, ActualSecondCurveIndex ] )

	end,

	FirstPart ++ text_utils:format( " title \"~ts\"",
						[ text_utils:binary_to_string( BinZoneName ) ] ).



% Returns a Gnuplot-compatible rotation specification.
%
% (helper)
%
-spec get_formatted_orientation( label_orientation() ) -> command_element().
get_formatted_orientation( upright ) ->
	"norotate";

get_formatted_orientation( Angle ) when is_number( Angle ) ->
	text_utils:format( "rotate by ~p", [ Angle ] ).



% Actual (synchronous) generation of the report.
%
% Returns an updated state.
%
% (helper function)
%
generate_report( Name, State ) ->

	false = ?getAttr(result_collected),

	% Generating an updated report should not be very common:
	case ?getAttr(result_produced) of

		true ->
			?warning_fmt( "Report '~ts' has already been generated "
						  "at least once.", [ Name ] );

		false ->
			ok

	end,

	SetState = setAttribute( State, result_produced, true ),

	TargetFilename = get_report_filename( Name ),

	case file_utils:is_existing_file( TargetFilename ) of

		true ->
			?warning_fmt( "The file '~ts' was already existing, "
						  "it has been removed.", [ TargetFilename ] ),
			file_utils:remove_file( TargetFilename );

		false ->
			ok

	end,

	%file_utils:remove_file_if_existing( TargetFilename ),

	?notice_fmt( "Generation of probe report '~ts' requested.",
				 [ TargetFilename ] ),

	% To allow specialized probes to override the command generation:
	CommandState = executeOneway( SetState, generateCommandFile ),

	ensure_data_file_available( CommandState ),

	ProbeDir = ?getAttr(probe_dir),

	% Generates a PNG:
	% (gnuplot might issue non-serious warnings)
	%
	CommandFilename = get_command_filename( Name ),

	%trace_utils:debug_fmt( "Generating plot based on ~ts.",
	%                      [ GeneratingFile ] ),

	% We must change the current directory (in the command, as we do not want to
	% interfere at the level of the whole VM) otherwise the PNG will be created
	% in the directory of the simulation case; specifying in the command file an
	% absolute path for the PNG is not an option either, as we are to move the
	% files to the result directory afterwards.
	%
	Command = executable_utils:get_gnuplot_path() ++  " '" ++ CommandFilename
		++ "'",

	OutputMessage = case system_utils:run_command( Command,
								_Environment=[], _WorkingDir=ProbeDir ) of

		{ _ReturnCode=0, _CmdOutput=[] } ->
			[];


		{ _ReturnCode=0, CmdOutput } ->

			?warning_fmt( "Report generation succeeded for '~ts', but "
				"it output following information: ~ts", [ Name, CmdOutput ] ),

			CmdOutput;


		{ ReturnCode, CmdOutput } ->

			?error_fmt( "Report generation failed for '~ts' (code: ~B); "
				"following information reported: ~ts",
				[ Name, ReturnCode, CmdOutput ] ),

			throw( { report_generation_failed_for, Name, CmdOutput,
					 ReturnCode } )

	end,

	ResultPath = file_utils:join( ProbeDir, TargetFilename ),

	case file_utils:is_existing_file( ResultPath ) of

		true ->
			ok;

		false->
			case OutputMessage of

				[] ->

					?error_fmt( "Report generation failed for '~ts' "
						"(result file '~ts' not available, no "
						"message output)", [ Name, ResultPath ] ),

					throw( { report_generation_failed_for, Name } );

				_ ->
					?error_fmt( "Report generation failed for '~ts' "
						"(result file '~ts' not available, message "
						"output: '~ts')",
						[ Name, ResultPath, OutputMessage ] ),

					throw( { report_generation_failed_for, Name,
							 OutputMessage } )
			end

	end,

	CommandState.




% Ensures that the command file for this probe is available.
%
% Returns an updated state.
%
% (helper)
%
ensure_command_file_available( State ) ->

	Name = text_utils:binary_to_string( ?getAttr(name) ),

	ProbeDir = ?getAttr(probe_dir),

	CommandFileName = get_command_filename( Name, ProbeDir ),

	case file_utils:is_existing_file( CommandFileName ) of

		true ->
			State;

		false ->
			% Returned state is thrown away:
			executeOneway( State, generateCommandFile )

	end.




% Ensures that the data file for this probe is available.
%
% (const helper function, not returning anything useful)
%
ensure_data_file_available( State ) ->

	case ?getAttr(deferred_data_writes) of

		true ->
			generate_data_file( State );

		false ->
			% Here we need to have the data file available, regardless of
			% buffering:
			%
			ok = file:sync( ?getAttr(data_file) )

	end.



% Returns the Gnuplot reference version for us.
-spec get_gnuplot_reference_version() ->
					static_return( basic_utils:two_digit_version() ).
get_gnuplot_reference_version() ->
	wooper:return_static( ?gnuplot_reference_version ).



% Serialisation section.
%
% Hooks are defined so that the WOOPER-provided serialise/3 request is
% customised for probes.


% Triggered just before serialisation.
%
% We are to fix file handles here. The PIDs (none is internal to a probe) will
% be converted later by the entry transformer.
%
-spec pre_serialise_hook( wooper:state() ) -> wooper:state().
pre_serialise_hook( State ) ->

	NewDataFileValue = case ?getAttr(data_file) of

		undefined ->
			undefined;

		_File ->
			resilience_recreate_data_file

	end,

	setAttributes( State, [ { data_filename, undefined },
							{ data_file, NewDataFileValue },
							{ probe_dir, undefined },
							{ gnuplot_version, undefined } ] ).



% Triggered just after serialisation, based on the selected entries.
%
% The value returned by this hook will be converted "as is" into a binary, that
% will be written.
%
% Instead of returning a mere {Classname, Entries} tuple for binarisation, a
% probe returns a more complete {Classname, Entries, BinCommand, BinData} tuple.
%
-spec post_serialise_hook( classname(),
		wooper_serialisation:term_serialisation(), wooper:state() ) -> term().
post_serialise_hook( Classname, Entries, State ) ->

	% Content of the command file (if any):

	Name = text_utils:binary_to_string( ?getAttr(name) ),
	ProbeDir = ?getAttr(probe_dir),

	CommandFileName = get_command_filename( Name, ProbeDir ),

	BinCommand = case file_utils:is_existing_file( CommandFileName ) of

		true ->
			file_utils:read_whole( CommandFileName );

		false ->
			undefined

	end,

	% Content of the data file (if any):

	DataFilename = file_utils:join( ProbeDir, get_data_filename( Name ) ),

	BinData = case file_utils:is_existing_file( DataFilename ) of

		true ->
			file_utils:read_whole( DataFilename );

		false ->
			undefined

	end,

	{ Classname, Entries, BinCommand, BinData }.





% Triggered just before deserialisation.
%
% Here we mostly perform the reverse operations done in post_serialise_hook/3.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
									wooper_serialisation:term_serialisation().
pre_deserialise_hook( _SerialisationTerm={ _Classname, _Entries, _BinCommand,
										   _BinData }, _UserData ) ->
	%Entries.
	throw( fixme_not_implemented_yet ).


% Triggered just after deserialisation.
-spec post_deserialise_hook( wooper:state() ) -> wooper:state().
post_deserialise_hook( State ) ->
	State.



% Deserialises a probe from specified binaries.
%
% Never returns: the probe takes ownership of the process.
%
-spec deserialise( binary(), binary(), binary(), pid() ) -> no_return().
deserialise( BinContent, BinCommand, _BinData, ReaderPid ) ->

	% Let's start with the files:
	{ _CommandFilename, _CommandBinContent } = binary_to_term( BinCommand ),

	{ Classname, RawEntries } = binary_to_term( BinContent ),

	% Too early to transform serialisation markers, as we need up-to-date
	% instance trackers beforehand:
	%
	% (we just update the local tracker with information about that actor)
	%
	% Will never return:
	%
	Classname:wooper_deserialise( RawEntries,
			_EntryTransformer=undefined, _UserData=undefined,
			_ListenerPid=ReaderPid ),

	throw( fixme_not_implemented_yet ).




% Helper section.


% Returns the default per-curve plot suffix, as a binary.
-spec get_default_plot_suffix() -> static_return( curve_plot_suffix() ).
get_default_plot_suffix() ->

	% "noenhanced" to avoid that a name like 'foo_bar' gets displayed as foo
	% with bar put as subscript.
	%
	wooper:return_static( <<"noenhanced">> ).



% Transforms a list of names into a list of {Number, Name, CurvePlotSuffix}
% curve entries, where Number is the index of the name in the list (starting at
% 1), Name is a binary name, and CurvePlotSuffix is the curve-specific default
% plot suffix.
%
% Respects the order of specified names.
%
% Ex: transform_curve_names(["a", "b", "c"]) should result in: [
% {1,<<"a">>,DefaultBinPlotSuffix}, {2,<<"b">>,DefaultBinPlotSuffix},
% {3,<<"c">>,DefaultBinPlotSuffix} ].
%
-spec transform_curve_names( [ declared_curve_name() ] ) -> curve_entries().
transform_curve_names( NameList ) ->
	transform_curve_names( NameList, get_default_plot_suffix(),
						   _Acc=[], _Count=1 ).


% (helper)
transform_curve_names( _NameList=[], _BinPlotSuffix, Acc, _Count ) ->
	lists:reverse( Acc );

transform_curve_names( _NameList=[ CurveName | T ], BinPlotSuffix, Acc,
					   Count ) ->

	CurveEntry = { Count, text_utils:string_to_binary( CurveName ),
				   BinPlotSuffix },

	transform_curve_names( T, BinPlotSuffix, [ CurveEntry| Acc ], Count+1 ).



% Transforms a list of zone declarations into actual zone entries, while
% checking them against the curve names.
%
-spec transform_declared_zones( [ declared_zone() ], curve_entries() ) ->
										[ zone_definition() ].
transform_declared_zones( DeclaredZones, CurveEntries ) ->
	transform_declared_zones( DeclaredZones, CurveEntries, _Acc=[] ).


transform_declared_zones( _DeclaredZones=[], _CurveEntries, Acc ) ->
	% We preserve order here as well, otherwise zones will listed in the keys in
	% reverse order:
	%
	lists:reverse( Acc );

transform_declared_zones( [ Z={ ZoneName,
								{ FirstCurveName, SecondCurveName } } | T ],
						  CurveEntries, Acc ) ->

	First = get_curve_index_for( FirstCurveName, CurveEntries ),
	Second = get_curve_index_for( SecondCurveName, CurveEntries ),

	% We want to ensure:
	%
	%  1. that at least one actual curve is referenced (not two 'abscissa_*'
	%  atoms)
	%
	%  2. that if there is one 'abscissa_*' atom, it ends up in first position
	%  of the returned pair
	%
	NewBounds = case First of

		_ when First == 'abscissa_top' orelse First == 'abscissa_bottom' ->

			case Second of

				_ when Second == 'abscissa_top'
					   orelse Second == 'abscissa_bottom' ->
					throw( { curveless_zone, Z } );

				_ ->
					{ First, Second }

			end;

		_ ->
			% So that we are sure that any abscissa_* atom would end up in first
			% position:
			%
			{ Second, First }

	end,

	ZoneBinName = text_utils:string_to_binary( ZoneName ),

	transform_declared_zones( T, CurveEntries,
							  [ { ZoneBinName, NewBounds } | Acc ] ).




% Returns an appropriate curve index to define internally a zone.
-spec get_curve_index_for( declared_extended_curve_name(), curve_entries() ) ->
									extended_curve_name().
get_curve_index_for( CurveName='abscissa_top', _CurveEntries ) ->
	CurveName;

get_curve_index_for( CurveName='abscissa_bottom', _CurveEntries ) ->
	CurveName;

get_curve_index_for( CurveName, CurveEntries ) ->

	BinCurveName = text_utils:string_to_binary( CurveName ),

	case lists:keyfind( _Key=BinCurveName, _Index=2, CurveEntries ) of

		false ->
			throw( { zone_specified_unknown_curve, CurveName, CurveEntries } );

		{ CurveIndex, _BinCurveName, _BinPlotSuffix } ->
			CurveIndex

	end.



% Checks that the (specified) probe directory is indeed existing.
check_probe_directory( ProbeDir ) ->

	case file_utils:is_existing_directory( ProbeDir ) of

		true ->
			ok;

		false ->
			throw( { non_existing_probe_directory, ProbeDir } )

	end.
