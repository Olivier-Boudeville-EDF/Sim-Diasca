% Copyright (C) 2023-2024 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
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
% Creation date: Saturday, October 7, 2023.



% Define section.


% To have rotated tick labels:

% Rotate clockwise (hence end of text below beginning):
-define( rotate_cw_tick_label_option, "rotate by - 45" ).

% Rotate counter-clockwise (hence end of text above beginning):
% (note the right alignment here, otherwise the labels run from the base of the
% graph upwards)
%
-define( rotate_ccw_tick_label_option, "rotate by 45 right" ).



% Not all plot features are available in older gnuplot versions:
-define( gnuplot_reference_version, { 5, 4 } ).


% To centralise basic open (write) flags:
%
% (exclusive used to avoid that two plots bearing the same name by mistake may
% end up writing to the same files simultaneously)
%
-define( base_open_flags, [ write, exclusive ] ).



% Defaults:

-define( default_canvas_width, 1600 ).
-define( default_canvas_height, 800 ).


% File extensions:

% For (gnuplot) command files:
-define( command_extension, "p" ).


% For data files (typically text files organised by columns separated by
% whitespaces):
%
-define( data_extension, "dat" ).


% The settings governing a given plot.
%
% The underlying conventions are the gnuplot ones.
%
-record( plot_settings, {

	% The (mandatory) name of this plot, whence for example the corresponding
	% file names will be derived.
	%
	name :: plot_utils:bin_plot_name(),


	% The title, if any, to display on this plot.
	title :: maybe( ui:bin_title() ),

	% Key (legend) options:
	key_options :: maybe( plot_utils:key_options() ),


	% Label for the abscissa axis (as a binary):
	x_label :: maybe( plot_utils:label_text() ),

	% Label for the ordinate axis (as a binary):
	y_label :: maybe( plot_utils:label_text() ),


	% Settings for tick layout along the abscissa axis:
	x_tick :: maybe( plot_utils:tick_option() ),

	% Settings for tick layout along the ordinate axis:
	y_tick :: maybe( plot_utils:tick_option() ),


	% Abscissa range (pair of {MaybeMinX,MaybeMaxX} integers, or 'undefined'),
	% knowing that such a range can be open, if either of the bounds is not
	% specified (e.g. resulting in a "[5:]" range):
	%
	x_range :: maybe(
		{ maybe( gui:coordinate() ), maybe( gui:coordinate() ) } ),

	% Ordinate range (pair of {MaybeMinY,MaybeMaxY} integers, or 'undefined'),
	% knowing that such a range can be open, if either of the bounds is not
	% specified (e.g. resulting in a "[5:]" range):
	%
	y_range :: maybe(
		{ maybe( gui:coordinate() ), maybe( gui:coordinate() ) } ),


	% Fine control of the major (labeled) ticks on the abscissa axis.
	x_ticks :: maybe( plot_utils:ticks_option() ),


	% Tells whether the abscissa axis gathers timestamps.
	is_timestamped = false :: boolean(),

	% The display time format to use if the x axis is a timestamped one:
	x_ticks_timestamp_time_format ::
		maybe( plot_utils:timestamp_time_format() ),


	% Fine control of the major (labeled) ticks on the ordinate axis.
	y_ticks :: maybe( plot_utils:ticks_option() ),


	% Defines how graphs should be rendered:
	%
	% (our default is linespoints, in order to add - compared to mere lines - a
	% graphical symbol on top of each data point)
	%
	plot_style = 'linespoints' :: plot_utils:plot_style(),


	% Defines the size of each point; 'set pointsize 2' means the point size is
	% twice the default size.
	%
	point_size = 1 :: non_neg_integer(),


	% Defines how areas like histograms should be filled:
	fill_style = 'empty' :: plot_utils:fill_style(),


	% Defines the width of the canvas, i.e. the actual width, in pixels, of the
	% corresponding plot:
	%
	canvas_width = ?default_canvas_width :: gui:width(),

	% Defines the height of the canvas, i.e. the actual height, in pixels, of
	% the corresponding plot:
	%
	canvas_height = ?default_canvas_height :: gui:height(),


	% The image format for plot rendering (for the generated plot files):
	%
	% (default could be 'svg' some day; the rendering is quite close, yet the
	% file sizes are at least twice as large)
	%
	image_format = 'png' :: gui_image:image_format(),


	% Lists the arbitrary labels that may be defined over the plot rendering:
	labels = [] :: [ plot_utils:plot_label() ],

	% Lists extra defines that shall be added verbatim to the command file (near
	% its top):
	%
	extra_defines = [] :: [ text_utils:bin_string() ],


	% Extra information about data, used to enrich (including for in-file
	% comments) data to be plotted, for example to specify the origin of data,
	% measurement time, source, author, accuracy, version, any specific number
	% of points to be plotted, etc.
	%
	meta_data = [] :: plot_utils:plot_meta_data(),

	% The directory (if any; otherwise the current working directory will be
	% used) in which the plot is to be generated.
	%
	plot_directory :: maybe( file_utils:bin_directory_name() ),

	% The filename (if any; otherwise it will be generated) to the plot that is
	% to be generated.
	%
	plot_filename :: maybe( file_utils:bin_file_name() ),


	% Internals:

	% An ordered list of {CurveIndex, BinCurveName, BinPlotSuffix} triplets,
	% with CurveIndex keeping track of the order according to which the curves
	% were declared and fed (so that, prior to generating a report, curves can
	% be reordered while being still associated to their values), and with curve
	% names being binaries; the order in this list dictates the actual rendering
	% order of curves that will be performed.
	%
	curve_entries = [] :: [ plot_utils:curve_entry() ],


	% A list of definitions of zones, between two curves in a 2D plot:
	zone_entries = [] :: [ plot_utils:zone_entry() ],


	% A precomputed format string (if any) used to write new samples.
	%
	% How a data row shall be formatted when writing a data file, should a
	% specific format be wanted:
	%
	row_format_string :: maybe( text_utils:format_bin_string() ) } ).




% Fully defines a label on a plot:
-record( plot_label, {

	% Actual text of the label:
	text :: plot_utils:label_text(),

	% 2D coordinates of the label on the plot:
	location :: plot_utils:label_location(),

	% Specific color (if any) for the text:
	color = 'black' :: maybe( plot_utils:label_color() ),

	% Justification (if any) of the text based onto the location for the label:
	justification :: maybe( plot_utils:label_justification() ),

	% The label may be rendered with an angle from the abscissa axis:
	%
	% (by default no rotation applied)
	%
	orientation = 'upright' :: maybe( plot_utils:label_orientation() ),

	% Tells whether a point shall be rendered at the label location:
	point :: maybe( plot_utils:point_style_spec() ) } ).
