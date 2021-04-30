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


% The name of a probe (ex: to be checked against a result specification):
-type probe_name() :: class_ResultProducer:producer_name().
-type bin_probe_name() :: class_ResultProducer:bin_producer_name().


% The tick, for a probe, corresponds to an (absolute) tick (not a tick offset).
%
% Note that if no tick duration is specified to a given probe (thus using
% internally ticks rather than higher-level timestamps), then setting a tick
% offset for it (see class_Probe:setTickOffset/2) allows to subtract that offset
% to all recorded ticks, and thus to display tick offsets rather than absolute
% ticks.
%
-type probe_tick() :: class_TimeManager:tick().



% Corresponds to the 2D coordinates of the label on the plot:
-type label_location() :: linear_2D:point().


% Actual text of the label:
-type label_text() :: text_utils:bin_string().


% Color of the text (default: "blue"):
-type label_color() :: gui_color:color().


% Describes the position of the text based on to the specified location for the
% label:
%
-type label_position() :: 'left' | 'center' | 'right'.


% Describes whether the text of the label should be rendered with an angle from
% the abscissa axis:
%
-type label_orientation() :: 'upright' | unit_utils:int_degrees().


% Defaults:

-define( default_canvas_width, 1600 ).
-define( default_canvas_height, 800 ).



% Fully defines a label on a probe rendering:
-record( probe_label, {

		% 2D coordinates of the label on the plot:
		location :: label_location(),

		% Actual text of the label:
		text :: label_text(),

		% Color of the text:
		color :: label_color(),

		% Position of the text based on to the location for the label:
		position :: label_position(),

		% The label may be rendered with an angle from the abscissa axis:
		orientation :: label_orientation()

} ).

-type probe_label() :: #probe_label{}.



% Records the (rendering) settings of a probe.
%
% Used by plain probes and by the datalogger.
%
-record( probe_settings, {

	% Title of any probe report (as a binary):
	title :: text_utils:bin_string(),

	% Key (legend) options (as a binary):
	key_options :: text_utils:bin_string(),


	% Label for the abscissa axis (as a binary):
	x_label :: text_utils:bin_string(),

	% Label for the ordinate axis (as a binary):
	y_label :: text_utils:bin_string(),


	% Settings for tick layout along the abscissa axis (as a binary):
	x_tick :: text_utils:bin_string(),

	% Settings for tick layout along the ordinate axis (as a binary):
	y_tick :: text_utils:bin_string(),


	% Abscissa range (pair of {MinX,MaxX} integers, or 'undefined'):
	x_range :: maybe( { gui:coordinate(), gui:coordinate() } ),

	% Ordinate range (pair of {MinY,MaxY} integers, or 'undefined'):
	y_range :: maybe( { gui:coordinate(), gui:coordinate() } ),


	% Fine control of the major (labeled) ticks on the abscissa axis (as a
	% binary):
	%
	x_ticks :: maybe( text_utils:bin_string() ),


	% The display time format to use if the x axis is a timestamped one:
	x_ticks_timestamp_time_format ::
					  maybe( class_Probe:timestamp_time_format() ),


	% Fine control of the major (labeled) ticks on the ordinate axis (as a
	% binary):
	%
	y_ticks :: maybe( text_utils:bin_string() ),


	% Defines how graphs should be rendered (as a binary):
	%
	% (our default is linespoints, in order to add - compared to mere lines - a
	% symbol on top of each data point)
	%
	plot_style :: text_utils:bin_string(),


	% Defines the size of each point; 'set pointsize 2' means the point size is
	% twice the default size.
	%
	point_size = 1 :: non_neg_integer(),


	% Defines how areas like histograms should be filled (as a binary):
	fill_style :: text_utils:bin_string(),


	% Defines the width of the canvas, i.e. the actual width, in pixels, of the
	% corresponding plot:
	%
	canvas_width = ?default_canvas_width :: gui:length(),

	% Defines the height of the canvas, i.e. the actual height, in pixels, of
	% the corresponding plot:
	%
	canvas_height = ?default_canvas_height :: gui:length(),


	% The default image format for probe rendering (as a binary):
	image_format = <<"png">> :: text_utils:bin_string(),

	% Lists the arbitrary labels that may be defined over the probe rendering:
	labels = [] :: [ probe_label() ],

	% Lists extra defines that shall be added verbatim to the command file (near
	% its top):
	%
	extra_defines = [] :: [ text_utils:bin_string() ] } ).


-type probe_settings() :: #probe_settings{}.



% Describes management (not rendering) options that apply to (basic) probes.
-record( probe_options, {


		   % If true, the gnuplot command file will be written at probe
		   % start-up, thus preventing the taking into account of any subsequent
		   % change in the rendering parameters, but remaining available even if
		   % the simulation was to be interrupted.
		   %
		   create_command_file_initially = false :: boolean(),


		   % If true, received sample data will be stored in memory instead of
		   % being directly written to disk (default: false, as the memory
		   % footprint might become then very significant).
		   %
		   deferred_data_writes = false :: boolean(),


		   % If true, this probe will register itself to the result manager, and
		   % be driven by it.
		   %
		   register_as_tracked_producer = true :: boolean(),


		   % Specifies the directory in which the files related to this probe
		   % (ex: *.p, *.data, *.png) should be written.
		   %
		   probe_directory = undefined :: maybe( file_utils:directory_name() )

}).

-type probe_options() :: #probe_options{}.


% The external name for a curve:
-type string_curve_name() :: text_utils:ustring().

% The internal name for a curve:
-type curve_name() :: text_utils:bin_string().


% A tuple of data (numbers) to be sent as sample to a probe-like result
% producer:
%
-type sample_data() :: tuple().
