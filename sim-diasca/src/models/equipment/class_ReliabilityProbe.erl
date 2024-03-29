% Copyright (C) 2008-2024 EDF R&D
%
% This file is part of Sim-Diasca.
%
% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: 2008.


% @doc A simple probe to denote <b>reliability metrics</b>.
-module(class_ReliabilityProbe).


-define( class_description,
		 "Reliability Probe class, regarding failures and reparations of an "
		 "equipment. See class_Probe.erl" ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_Probe ] ).



% For the plot_settings record:
-include_lib("myriad/include/plot_utils.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Probe.Reliability" ).


% Allows to use macros for trace sending:
-include_lib("traces/include/class_TraceEmitter.hrl").

% Green when ok:
-define( nominal_color, "#3ab001" ).

% Red when failed:
-define( failed_color, "#ec0505" ).


% Shorthands:

-type plot_settings() :: plot_utils:plot_settings().


% @doc Constructs a reliability probe.
%
% Parameters are:
%
% - Name is the name of this probe, and will be used for the generated data and
% command files
%
% - Title will be the graph title
%
construct( State, Name, Title ) ->

	% First the direct mother classes:
	ProbeState = class_Probe:construct( State, ?trace_categorize(Name),
		_Curves=[ "Equipment State" ],
		_Zones=[],
		Title,
		_XLabel="Duration",
		_YLabel="Failure state",
		_MetaData=[] ),

	% Updates the inherited settings:
	ProbeSettings =
		update_plot_settings( getAttribute( ProbeState, settings ) ),

	% Then the class-specific actions:
	%
	% Overrides probe default settings, sets the probe output to reliability
	% mode, to track the changes in the state of an equipment:
	%
	% (curve_count set to 1 whereas two curve names given: above/below)
	%
	StartState = setAttribute( ProbeState, settings, ProbeSettings ),

	?send_info( ProbeState, "New reliability probe created." ),

	StartState.




% Methods section.



% @doc Generates the appropriate gnuplot command file.
%
% Note: mostly defined to override its inherited version and branch to the
% helper just below.
%
-spec generateCommandFile( wooper:state() ) -> const_oneway_return().
generateCommandFile( State ) ->

	Settings = ?getAttr(settings),

	Name = class_TraceEmitter:get_plain_name( State ),

	LabelDefs =
		plot_utils:get_label_definitions( Settings#plot_settings.labels ),

	PlotCommand = get_plot_command( Name, State ),

	ProbeDir = ?getAttr(probe_dir),

	class_Probe:check_probe_directory( ProbeDir ),

	PNGFilename = class_Probe:get_report_filename( Name ),

	CommandFilename = file_utils:join( ProbeDir,
		class_Probe:get_command_filename( Name ) ),

	XrangeOpt = plot_utils:get_x_range_option( Settings ),

	YrangeOpt = plot_utils:get_y_range_option( Settings ),

	% If a probe is created directly from the simulation case, its files will be
	% created in the current directory (instead of in a dedicated directory) and
	% thus may step on each other from a run to the next:
	%
	file_utils:is_existing_file( CommandFilename ) andalso
		begin
			?notice_fmt( "Command filename ('~ts') found already existing, "
						 "removing it first.", [ CommandFilename ] ),
			file_utils:remove_file( CommandFilename )
		end,

	% No 'delayed_write' I/O option useful here:
	File = file_utils:open( CommandFilename,
		[ raw, write, exclusive, file_utils:get_default_encoding_option() ] ),

	%trace_utils:debug_fmt( "Generating command file '~ts'.",
	%                       [ CommandFilename ] ),

	XTicksOpt = plot_utils:get_x_ticks_option( Settings ),

	YTicksOpt = plot_utils:get_y_ticks_option( Settings ),

	% Use 'undefined' in sample if not having available data for an element.
	% Set terminal png *transparent* could be used as well.
	%
	file_utils:write_ustring( File,
							  "set autoscale~n"
							  "unset log~n"
							  "set grid~n"
							  "set style data ~ts~n"
							  "set style fill ~ts~n"
							  "set key box ~ts~n"
							  "set pointsize ~B~n"
							  "set xtic ~ts~n"
							  "set ytic ~ts~n"
							  "~ts~n"
							  "~ts~n"
							  "~ts~n"
							  "~ts~n"
							  "set title \"~ts\"~n"
							  "set xlabel \"~ts\" offset 75~n"
							  "set ylabel \"~ts\"~n"
							  "set datafile missing 'undefined'~n"
							  "set terminal ~ts size ~B, ~B~n"
							  "~ts~n"
							  "set output \"~ts\"~n"
							  "~ts",
							  [ Settings#plot_settings.plot_style,
								Settings#plot_settings.fill_style,
								Settings#plot_settings.key_options,
								Settings#plot_settings.point_size,
								Settings#plot_settings.x_tick,
								Settings#plot_settings.y_tick,
								XrangeOpt,
								YrangeOpt,
								XTicksOpt,
								YTicksOpt,
								Settings#plot_settings.title,
								Settings#plot_settings.x_label,
								Settings#plot_settings.y_label,
								Settings#plot_settings.image_format,
								Settings#plot_settings.canvas_width,
								Settings#plot_settings.canvas_height,
								LabelDefs,
								PNGFilename,
								PlotCommand ] ),

	file_utils:close( File ),

	wooper:const_return().




% Helper section.


% @doc Returns (as a plain string) an appropriate gnuplot command for this
% probe.
%
% (helper)
%
get_plot_command( Name, State ) ->

	% Not wanting a full path here:
	DataFilename = class_Probe:get_data_filename( Name ),

	Settings = ?getAttr(settings),

	PlotStyle = Settings#plot_settings.plot_style,

	FirstTitle = "Equipment is operational",

	SecondTitle = "Equipment is dysfunctional",

	text_utils:format( "plot \"~ts\" using 1:2:(0.0) title \"~ts\" "
		"with ~ts above lt rgb \"~ts\", \"~ts\" using 1:2:(0.0) "
		"title \"~ts\" with ~ts below lt rgb \"~ts\"",
		[ DataFilename, FirstTitle, PlotStyle, ?nominal_color,
		  DataFilename, SecondTitle, PlotStyle, ?failed_color ] ).



% @doc Returns a plot_settings record with updated informations (expressed as
% plain strings) and default values for the other fields.
%
-spec update_plot_settings( plot_settings() ) -> plot_settings().
update_plot_settings( Settings ) ->

	Settings#plot_settings{

		key_options="outside center bottom horizontal width 1 height 1",

		%x_tick=text_utils:string_to_binary( "auto" ),
		%y_tick=text_utils:string_to_binary( "auto" ),

		y_range={ -2, 2 },

		x_ticks="",
		y_ticks=
			"(\"Dysfunctional\" -1.0, \"\" 0.0, \"Operational\" 1.0) in rotate",

		% x_label left as is.

		y_label= <<"Equipment Condition">>,
		plot_style= <<"filledcurves">> }.
