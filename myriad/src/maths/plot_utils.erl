% Copyright (C) 2023-2026 Olivier Boudeville
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

-module(plot_utils).

-moduledoc """
Gathering of facilities **to plot graphs**.

Plots display samples (a.k.a. points).

Two interfaces are provided:

- the low-level one, based on the `plot_settings` record, and which is, at least
  currently, implemented thanks to [gnuplot](http://gnuplot.info/), and
  relatively tightly linked with it

- the user-oriented, higher-level one, which translates internally to the
  previous one, but hides most technical details/dependencies (so that it could
  rely on other lower-level interfaces if needed)
""".


% For the plot_settings record:
-include("plot_utils.hrl").



% Section for lower-level plot settings.


-doc """
The lower-level settings governing a given plot.

The underlying conventions are the gnuplot ones.
""".
-type plot_settings() :: #plot_settings{}.



-doc """
The user-specified plot name, whence for example the corresponding file names
will be derived.
""".
-type user_plot_name() :: any_string().



-doc "Internal plot name.".
-type bin_plot_name() :: bin_string().



-doc """
The path to a file corresponding to a rendering of a plot, as an image.

Typically a PNG or a SVG file.
""".
-type user_plot_path() :: any_file_path().



-doc """
Plot style.

Describes, for which sample point, what should be rendered.

(default being `symbols_and_lines`).
""".
% Refert to plot_style_to_backend/1 for conversions to backend conventions.
-type plot_style() ::

   % Per-sample symbols are rendered, and they are linked with lines (default).
   'symbols_and_lines'

   % Only per-sample symbols are rendered.
 | 'symbols_only'

   % Only lines between samples are rendered (no symbol).
 | 'lines_only'

 | 'boxes'

 | 'histograms'

 | 'filled_curves'

 | 'filled_steps'.


-doc """
Back-end level plot style (default being `linespoints`).

See [http://gnuplot.info/docs_5.5/Plotting_Styles.html].
""".
-type backend_plot_style() :: 'linespoints' % (default)
                            | 'lines'
                            | 'points'
                            | 'boxes'
                            | 'histograms'
                            | 'filledcurves'
                            | 'fillsteps'
                            | atom(). % As many others exist.


-doc """
Fill style (default being `undefined`, thus empty, i.e. no filling done).

Any alpha coordinate is taken into account.
""".
% Fill patterns could be added:
-type fill_style() :: option( any_color() ).


-doc """
Back-end level fill style (default being `empty`).

See [http://gnuplot.info/docs_6.0/loc16040.html].
""".
-type backend_fill_style() :: ustring(). % Default: "empty".


-doc """
A user-specified string containing key options.

For example `"box"`.
""".
-type user_key_options() :: any_string().



-doc "The name of a gnuplot command file (see the `command_extension` define).".
-type bin_command_file_name() :: bin_file_name().



-doc "A path to a gnuplot command file (see the `command_extension` define).".
-type command_file_path() :: file_path().



-doc "A path to a gnuplot command file (see the `command_extension` define).".
-type bin_command_file_path() :: bin_file_path().



-doc """
Extra information about data, used to enrich (as comments) data to be plotted,
for example to specify origin of data, measurement time, source, author,
accuracy, version, etc.

Reserved keys:

- `first_column_description :: ustring()`: to describe the semantics of the plot
parameter; for example: "First column corresponds to the abscissa, expressed in
tick offsets."

- `plot_point_count :: count()`: the number of points to plot
""".
-type plot_meta_data() :: list_table( atom(), any_string() ).



-doc "A function to be plotted.".
-type plot_function() :: float_to_float_fun().


-doc "A bound of a plot.".
-type plot_bound() :: math_utils:any_bounds().


-doc """
A possibly open plotting interval, based on any upper and lower bounds.
""".
-type plot_bounds() ::
    { MaybeMinBound :: option( bound() ), MaybeMaxBound :: option( bound() ) }.


-doc """
A data to plot, based on an (unordered) list of points to be plotted.
""".
-type plot_data() :: [ plot_point() ].



-doc """
A data point to plot, made of a sample parameter (like the one of a function,
for example a time parameter) and the associated sampled values to be plotted
(like the values of various functions for the previous parameter).

May also be designated as a data row.

For example `{"Monday", {1, undefined, 5.1}}`, or `{5.5, "hello"}`.
""".
-type plot_point() :: { sample_parameter(), plot_sample() }.



-doc """
A parameter corresponding to a plot sample, to be used as an abscissa
(e.g. regarding time) / a key associated to that sample.

So this may be a timestamp.

For example `"Monday"`, or `14557`.
""".
-type sample_parameter() :: any_string() | number().



-doc "A string corresponding to a sample parameter.".
-type sample_parameter_bin_string() :: bin_string().



-doc """
A sample to plot, i.e. a value generally associated to a sample parameter.

For example `{1, undefined, "red", 5.1}`, or `"hello"`.
""".
-type plot_sample() :: type_utils:tuploid( option( sample_value() ) ).



-doc """
A value corresponding to a plot sample.

For example `"active"`, or `1.12`.
""".
-type sample_value() :: any_string() | number().



-doc "The name of a data file (see the `data_extension` define).".
-type bin_data_file_name() :: bin_file_name().



-doc "A path to a data file (see the `data_extension` define).".
-type data_file_path() :: file_path().



-doc "A path to a data file (see the `data_extension` define).".
-type bin_data_file_path() :: bin_file_path().



-doc "A data row, typically to be written in a data file.".
-type row_data_string() :: format_string().



-doc "Describes how a data row is to be formatted.".
-type row_format_string() :: format_string().



-doc """
A path to a plot (image) file (see the `image_format` field in the plot
settings).
""".
-type bin_plot_path() :: bin_file_path().



-doc """
A filename of a plot (image) file (see the `image_format` field in the plot
settings).
""".
-type bin_plot_filename() :: bin_file_name().



-doc "The possible outcomes of an attempt of plot generation.".
-type plot_generation_outcome() ::
    { 'success', bin_plot_path() }
  | { 'warning', bin_plot_path(), warning_message() }
  | { 'error', error_message() }.



-doc """
Any warning (still a success case, plot was generated) message issued when
trying to generate a plot.
""".
-type warning_message() :: ustring().



-doc """
Any error (failure case, no plot was generated) message issued when trying to
generate a plot.
""".
-type error_message() :: ustring().



% Section for user-oriented, higher-level plot settings.
%
% Implementation note: meta_data field voluntarily omitted, as to be resorbed.


-define( default_plot_name, "myriad-plot" ).

-define( default_sample_count, 400 ).


-doc "A table holding any number of user-oriented, higher-level plot settings.".
-type plot_settings_table() ::
    list_table:list_table( plot_setting_key(), plot_setting_value() ).


-doc "A key (as an atom) in a `plot_settings_table/0`.".
-type plot_setting_key() ::

    % The name of this plot, whence for example the corresponding file names
    % will be derived.
    %
    % Associated to a value of type any_string(), whose default being the
    % default_plot_name define.
    %
    'name'

    % The title, if any, to display on this plot.
    %
    % Associated to a value of type option(any_string()), whose default is
    % 'undefined'.
    %
  | 'title'

    % Key (i.e. legend) options.
    %
    % Associated to a value of type option(user_key_options()), whose default is
    % 'undefined'.
    %
  | 'key_options'

    % The label for the abscissa axis.
    %
    % Associated to a value of type option(any_string()), whose default is
    % 'undefined'.
    %
  | 'x_label'

    % The label for the ordinate axis.
    %
    % Associated to a value of type option(any_string()), whose default is
    % 'undefined'.
    %
  | 'y_label'


    % Settings for the layout of the major ticks along the abscissa axis.
    %
    % Associated to a value of type plot_parametrisation(), whose default is
    % 'undefined'.
    %
  | 'x_tick_options'


    % Settings for the layout of the major ticks along the ordinate axis.
    %
    % Associated to a value of type plot_parametrisation(), whose default is
    % 'undefined'.
    %
  | 'y_tick_options'


    % Abscissa range (pair of {MaybeMinX,MaybeMaxX} abscissas), knowing that
    % such a range can be open, if either of the bounds is not specified
    % (e.g. resulting in a "[5:]" range).
    %
    % Note that, if plotting a function (as opposed to sample data), both bounds
    % of this range must be specified.
    %
    % Associated to a value of type option({option(number()),option(number())}),
    % whose default is 'undefined' (meaning that, if applied to samples, this
    % range will be auto-determined on a best-effort basis).
    %
  | 'x_range'


    % Ordinate range (pair of {MaybeMinY,MaybeMaxY}), knowing that such a range
    % can be open, if either of the bounds is not specified (e.g. resulting in a
    % "[5:]" range).
    %
    % Associated to a value of type option({option(number()),option(number())}),
    % whose default is 'undefined' (meaning that this range will be
    % auto-determined on a best-effort basis).
    %
  | 'y_range'


    % Tells whether the abscissa axis gathers timestamps.
    %
    % Associated to a value of type boolean(), whose default is false.
    %
  | 'is_timestamped'


    % The display time format to use if the abscissa axis is a timestamped one.
    %
    % Associated to a value of type option(timestamp_time_format()).
    %
  | 'x_ticks_timestamp_time_format'


    % How the samples to plot shall be rendered.
    %
    % Associated to a value of type plot_style(), whose default is
    % 'symbols_and_lines'.
    %
  | 'plot_style'

    % The scaling factor for any symbol rendered.
    %
    % Associated to a value of type number(), whose default is 1.
    %
  | 'symbol_scale'

    % Defines how areas like histograms should be filled.
    %
    % Associated to a value of type fill_style(), whose default is 'undefined'
    % (thus no filling).
    %
  | 'fill_style'

    % The name of any (single) curve that is to be plotted.
    %
    % Associated to a value of type option(any_string()), whose default is
    % 'undefined' (meaning that this curve will not have a specific text in the
    % overall key).
    %
  | 'curve_name'

    % The name of the curves that are to be plotted.
    %
    % Associated to a value of type [option(any_string()], whose length must be
    % equal to the number of curves that are to be plotted (for a curve,
    % 'undefined' meaning that it will not have a specific text in the overall
    % key).
    %
  | 'curve_names'

    % Defines the arbitrary labels that may be defined over the plot rendering.
    %
    % Associated to a value of type [plot_utils:plot_label(), whose default is
    % [].
    %
  | 'labels'

    % Defines the number of abscissa pixels between two samplings of any plotted
    % function.
    %
    % Associated to a value of type gui:width(), whose default is
    % 1 (thus one sample for each pixel).
    %
  | 'sampling_offset'

    % Lists extra defines that shall be added verbatim, uncommented to the
    % command file (near its top).
    %
    % Associated to a value of type [text_utils:any_string()], whose default is
    % [].
    %
  | 'extra_defines'

    % Defines the width of the canvas, i.e. the actual width, in pixels, of the
    % corresponding plot.
    %
    % Associated to a value of type gui:width(), whose default is
    % ?default_canvas_width.
    %
  | 'canvas_width'

    % Defines the height of the canvas, i.e. the actual height, in pixels, of
    % the corresponding plot.
    %
    % Associated to a value of type gui:height(), whose default is
    % ?default_canvas_height.
    %
  | 'canvas_height'

    % Defines the image format for plot rendering (for the generated plot
    % files).
    %
    % Associated to a value of type gui_image:image_format(), whose default is
    % 'png'.
    %
  | 'image_format'


    % Sets the directory (if any; otherwise the current working directory will
    % be used) in which the plot is to be generated.
    %
    % Associated to a value of type option(file_utils:any_directory_name()),
    % whose default is 'undefined'.
    %
  | 'plot_directory'

    % Sets the filename (if any; otherwise it will be generated) of the plot
    % that is to be generated.
    %
    % Associated to a value of type option(file_utils:any_file_name()), whose
    % default is 'undefined'.
    %
  | 'plot_filename'

    % Requests the plot to be displayed once generated.
    %
    % Associated to a value of type boolean(), whose default is 'false'.
    %
  | 'display'.



-doc """
The name of a possibly-parametrised finer configuration element defined in the
context of a given plot setting.
""".
-type plot_parameter_name() :: atom().


-doc """
Any value set for a parametrised finer configuration element defined in the
context of a given plot setting.
""".
-type plot_parameter_value() :: term().


-doc """
A parameter, that is a finer configuration element defined in the context of a
given plot setting.
""".
-type plot_parameter() :: plot_parameter_name()
                        | { plot_parameter_name(), plot_parameter_value() }.


-doc "The parametrisation of a plot setting.".
-type plot_parametrisation() :: maybe_list( plot_parameter() ).



-doc """
The value of a key in a `plot_settings_table/0`.
""".
-type plot_setting_value() :: plot_parametrisation().


-doc "An entry of a `plot_settings_table/0`.".
-type plot_settings_entry() :: { plot_setting_key(), plot_setting_value() }.


-export_type([ plot_settings/0,
               user_plot_name/0, bin_plot_name/0, user_plot_path/0,
               plot_style/0, fill_style/0,

               bin_command_file_name/0,
               command_file_path/0, bin_command_file_path/0,

               plot_meta_data/0, plot_function/0,
               plot_bound/0, plot_bounds/0,
               plot_data/0, plot_point/0,
               sample_parameter/0, sample_parameter_bin_string/0,
               plot_sample/0, sample_value/0,

               bin_data_file_name/0,
               data_file_path/0, bin_data_file_path/0,

               row_data_string/0, row_format_string/0,

               bin_plot_path/0, bin_plot_filename/0,

               plot_generation_outcome/0, warning_message/0, error_message/0,

               % Specific to the the higher-level interface:

               plot_parameter_name/0, plot_parameter_value/0, plot_parameter/0,
               plot_parametrisation/0,

               plot_setting_key/0, plot_setting_value/0, plot_settings_entry/0,
               plot_settings_table/0 ]).



-doc "The name of a user-specified curve.".
-type declared_curve_name() :: any_string().



-doc """
Identifiers of pseudo-curves (Ordinate = constant), corresponding respectively
to the highest ordinate value and lowest one.
""".
-type special_curve_id() :: 'abscissa_top' | 'abscissa_bottom'.



-doc "The identifier of a user-specified extended curve.".
-type declared_curve_id() :: declared_curve_name() | special_curve_id().



-doc "The name of a user-specified zone.".
-type declared_zone_name() :: any_string().



-doc """
The definition of a user-specified zone, which is the specific area between the
two specified curves.
""".
-type declared_zone() ::
        { declared_zone_name(), { declared_curve_id(), declared_curve_id() } }.



-doc "A number of curves.".
-type curve_count() :: count().



-doc "A factor by which the default point size is multiplied.".
-type point_size_factor() :: pos_integer().


-export_type([ declared_curve_name/0, special_curve_id/0, declared_curve_id/0,
               declared_zone_name/0, declared_zone/0,
               curve_count/0,
               point_size_factor/0 ]).



-doc "Fully defines a label on a plot.".
-type plot_label() :: #plot_label{}.



-doc "Corresponds to the 2D integer coordinates of the label on the plot.".
-type label_location() :: gui:point().



-doc """
Actual text of a label.

Note that texts can be "enhanced" (e.g. `"for some {/:Bold important} text"`),
and may include newlines (e.g. `"radius\\nat..."`).
""".
-type label_text() :: bin_string().



-doc """
Color of the text (default: `"blue"`).
""".
-type label_color() :: color().



-doc """
Describes the justification of the text based on the specified location for the
label.
""".
-type label_justification() :: 'left' | 'center' | 'right'.



-doc """
Describes whether the text of the label should be rendered with an angle, from
the abscissa axis.
""".
-type label_orientation() :: 'upright' | int_degrees().



-doc """
A user specification of a type of symbol to be rendered.

If true, a default symbol is selected; if false no symbol will be rendered.
""".
-type symbol_spec() ::  boolean()
                      | 'dot'   % Thus '.'
                      | 'plus'  % '+'
                      | 'times' % 'x'
                      | 'star'  % '*'
                      | 'square_empty'
                      | 'square_filled'
                      | 'circle'
                      | 'disc'
                      | 'up_triangle_empty'
                      | 'up_triangle_filled'
                      | 'down_triangle_empty'
                      | 'down_triangle_filled'
                      | 'diamond_empty'
                      | 'diamond_filled'
                      | 'down_arrow_empty'
                      | 'down_arrow_filled'.




-doc """
The internal specification of a type of point to be rendered.

For example 1, as in `<<"pointtype 1">>`.

To see all available point types, executes `gnuplot`, and type `test` on this
shell (with the qt terminal).
""".
-type point_style() :: count().



-doc "A user specification of a label.".
-type label_spec() :: { label_text(), label_location() }
        | { label_text(), label_location(), point_style() }.


-export_type([ plot_label/0,
               label_location/0, label_text/0, label_color/0,
               label_justification/0, label_orientation/0,
               symbol_spec/0, point_style/0,
               label_spec/0 ]).



-doc "A version of gnuplot.".
-type gnuplot_version() :: basic_utils:two_digit_version().



-export_type([ gnuplot_version/0 ]).


-doc "The color of a given curve.".
-type extra_curve_settings() :: rgb_hexastring().



-doc "The color of a given zone.".
-type extra_zone_settings() :: rgb_hexastring().



-doc """
Options related to the plot key.

See [http://gnuplot.info/docs_5.5/loc12343.html].
""".
-type key_options() :: bin_string().



-doc "Option applying to label ticks.".
-type tick_option() :: 'rotate'.



-doc """
Option applying to ticks (e.g. axis, border, start, font, textcolor, etc.) for a
fine control of the major (labelled) tics on an axis (e.g. see `Xtics`, in
[http://www.gnuplot.info/docs_4.2/node295.html]).
""".
-type ticks_option() :: bin_string().



-doc "The display time format to use for timestamped axes.".
-type timestamp_time_format() ::

    % Time then date, on a single line:
    'single_line'

    % Time, newline, date, hence on two lines:
  | 'double_line'.


-export_type([ extra_curve_settings/0, extra_zone_settings/0,
               key_options/0, tick_option/0, ticks_option/0,
               timestamp_time_format/0 ]).



-doc """
A (complete) elementary (gnuplot) command, that is a self-standing line
(possibly a comment) of a command file thereof.
""".
-type elementary_command() :: ustring().



-doc "An element of a gnuplot command.".
-type command_element() :: ustring().



-doc """
String describing a timestamp (typically either a tick or a textual timestamp).
""".
-type timestamp_string() :: ustring().



-doc """
String describing a timestamp (typically either a tick or a textual timestamp).
""".
-type timestamp_bin_string() :: bin_string().


-export_type([ elementary_command/0, command_element/0,
               timestamp_string/0, timestamp_bin_string/0 ]).



% Higher-level user API:
-export([ plot/2, label/2, label/3, label/6 ]).


% Backend-level API:
-export([ plot/3, plot_multi/3, plot/4, plot_multi/4,

          get_plot_settings/1,
          get_default_plot_settings/0, get_default_plot_settings/1,

          get_timestamp_settings/1, get_timestamp_settings/2,
          set_plot_name/2,
          set_title/2, set_x_label/2, set_y_label/2,
          set_point_count/2, set_key_options/2,

          add_label/3, add_label/4, add_label/7, add_labels/2,

          remove_labels/1,

          declare_curves/2, declare_zones/2,
          get_plot_command/5,
          plot_samples/2, plot_samples/3,

          % Useful for example if a test wants to remove any prior generation:
          get_command_filename/1, get_data_filename/1 ]).



% Exported gnuplot helpers, mostly for internal use:
-export([ transform_curve_names/1, transform_declared_zones/2,
          get_default_curve_plot_suffix/0, get_default_zone_plot_suffix/0,
          get_formatted_orientation/1,
          get_label_definitions/1, get_label_definitions/2,
          get_gnuplot_reference_version/0, get_basic_options/1,
          get_xticks_option/1, get_yticks_option/1,
          get_x_range_option/1, get_y_range_option/1,
          get_x_ticks_option/1, get_y_ticks_option/1,
          add_plot_index_back/2, get_escaped_text/1,
          generate_command_file/1, generate_data_file/2, write_row/3,
          forge_format_string_for/1 ]).



% Section for internal types:


-doc "The internal plot name.".
-type plot_name() :: bin_string().



-doc """
The (internal) path to a file corresponding to a rendering of a plot, as an
image.

Typically a PNG or a SVG file.
""".
-type plot_path() :: bin_file_path().



-doc "The internal name for a curve.".
-type curve_name() :: bin_string().



-doc """
Curves are numbered internally, and correspond to the position of data in
specified samples.
""".
-type curve_index() :: curve_count().



-doc "Extended to allow for zone definitions.".
-type extended_curve_id() :: curve_index() | special_curve_id().



-doc "Information specific to the rendering of a curve.".
-type curve_entry() :: { curve_index(), curve_name(), curve_plot_suffix() }.



-doc """
Applies notably if using (unquoted) timestamps that account for more than one
field in data. Prefer relying on quoted timestamps.
""".
-type curve_offset() :: count().



-doc """
A (binary string) suffix (e.g. `<<"noenhanced with filledcurves">>`, `<<"with
boxes">>` or `<<"with boxes lt rgb '#f0f0f0'">>`) to be added to the plot
command of the corresponding curve.
""".
-type curve_plot_suffix() :: bin_string().



-doc "Tne name of a zone.".
-type zone_name() :: bin_string().



-doc """
A (binary string) suffix (e.g. `<<"fillcolor red">>`) to be added to the plot
command of the corresponding zone.
""".
-type zone_plot_suffix() :: bin_string().



-doc "Information specific to the rendering of a zone.".
-type zone_entry() ::
        { zone_name(), { extended_curve_id(), extended_curve_id() },
          zone_plot_suffix() }.


% Backend-specific (gnuplot) types:
-export_type([ backend_plot_style/0, backend_fill_style/0 ]).


% For internal sharing:
-export_type([ plot_name/0, plot_path/0,
               curve_name/0, curve_index/0, extended_curve_id/0,
               curve_entry/0, curve_offset/0, curve_plot_suffix/0,
               zone_name/0, zone_plot_suffix/0, zone_entry/0 ]).





% Implementation notes:
%
% Although there are open-source, cross-platform alternatives to gnuplot
% (e.g. LabPlot, SciDaVis, KAlgebra, KSEG and clip), none seemed to reach its
% richness/level of maturity. So we stick with good old gnuplot, hopefully for
% the best.

% Element needed:
%
% - gnuplot version 5.4 or higher is preferred (and 4.2 or above is required)
%
% - an image viewer, for example eog (eye of gnome) of gwenview; see the
% executable_utils module (e.g. display_image_file/2 or browse_images_in/1)
% module for viewers


% To debug the generated command/data files, one can run directly gnuplot. This
% is as simple as 'gnuplot My_test_plot.plot'.

% See http://www.gnuplot.info/docs/gnuplot.html for graph generation.


% Regarding zones:
%
% Zones are different from "stacked histograms" (see 'rowstacked') insofar as a
% zone corresponds just to the filling of the area between two curves, whereas a
% stacked histogram should stack (add) values read from columns; for example,
% if, for a given abscissa, V1 can be read for column C1 and V2 for column C2,
% then a zone would spread in [C1,C2] whereas a stacked histogram would
% represent a first "zone" between the abscissa axis and C1, and a second zone
% between C1 and C1+C2 (*not* C2).
%
% The simplest way (other options, such as using 'rowstacked' gnuplot histograms
% or filledcurves, are considerably less convenient/more problematic) to display
% with gnuplot such "stacked histograms" is to use our zone feature, and thus to
% preprocess entries so that they stack additively; e.g. instead of having raw
% samples like {Timestamp, C1, C2, C3}, the probe should be fed with {Timestamp,
% C1, C1+C2, C1+C2+C3} samples, and curves shall be rendered from the topmost to
% the bottom one (i.e. C1+C2+C3, C1+C2 and C1 here), so that the C1+C2 is drawn
% over C1+C2+C3, and so on; refer to send_stacked_data/3 to have it done for
% you.
%
% We however used filledcurves, an approach supposed to be more robust, yet it
% does not (and probably cannot) render the desired histograms: with
% filledcurves two data points are linked by a line segment (which of course
% gets filled), leading to unwanted filled triangles (picture a curve equal to
% zero until being equal to 1 at timestamp T: the area delimited by the previous
% timestamp and T will be an upright triangle raising from 0 to 1, whereas we
% would want a step from 0 to 1 at T. So we now use normal curves drawn as
% "boxes" (actually 'fillsteps').
%
% When rendering them, generally a fill style is needed (generally solid),
% specific zone colors are specified, ordinates shall start at zero (no
% autoscaling: y_range = {0,undefined}) and no zone based on abscissa_top is
% requested.


% Regarding terminals:
%
% We rely generally on the 'png' one. Of interest is also the 'qt' one, which
% can be tested interactively by execting directly 'gnuplot' and issuing on that
% shell 'test'.


% Type shorthands:

-type count() :: basic_utils:count().
-type maybe_list( T ) :: basic_utils:maybe_list( T ).

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().
-type format_string() :: text_utils:format_string().

-type list_table( K, V ) :: list_table:list_table( K, V ).


% Implies any_string():
-type title() :: ui:title().
-type label() :: ui:label().


-type file_path() :: file_utils:file_path().
-type any_file_path() :: file_utils:any_file_path().
-type bin_file_path() :: file_utils:bin_file_path().
-type bin_file_name() :: file_utils:bin_file_name().
-type file() :: file_utils:file().

-type int_degrees() :: unit_utils:int_degrees().

-type bound() :: math_utils:bound().
-type float_to_float_fun() :: math_utils:float_to_float_fun().

-type width() :: gui:width().
-type height() :: gui:height().

-type color() :: gui_color:color().
-type rgb_hexastring() :: gui_color:rgb_hexastring().
-type any_color() :: gui_color:any_color().

-type image_format() :: gui_image:image_format().



-doc "Returns the specification of a plot of the specified name.".
-spec get_plot_settings( user_plot_name() ) -> plot_settings().
get_plot_settings( UsrPlotName ) ->
    #plot_settings{ name=text_utils:ensure_binary( UsrPlotName ) }.



-doc """
Returns reasonable defaults in terms of plot settings, for a single, anonymous
curve.
""".
-spec get_default_plot_settings() -> plot_settings().
get_default_plot_settings() ->
    get_default_plot_settings( "Myriad plot" ).



-doc """
Returns reasonable defaults in terms of plot settings, for the specified single
curve.
""".
-spec get_default_plot_settings( user_plot_name() ) -> plot_settings().
get_default_plot_settings( PlotName ) ->
    BasePlotSettings = get_plot_settings( PlotName ),
    declare_curves( _CurveNames=[ "Function value" ], BasePlotSettings ).



-doc "Sets the name of the target plot.".
-spec set_plot_name( user_plot_name(), plot_settings() ) -> plot_settings().
set_plot_name( UsrPlotName, PlotSettings ) ->
    PlotSettings#plot_settings{ name=text_utils:ensure_binary( UsrPlotName ) }.



-doc """
Returns the specification of a plot corresponding to the specified one once the
specified title (if any) has been set.
""".
-spec set_title( option( title() ), plot_settings() ) -> plot_settings().
set_title( MaybeTitle, PlotSettings ) ->
    PlotSettings#plot_settings{
        title=text_utils:ensure_maybe_binary( MaybeTitle ) }.



-doc """
Returns the specification of a plot corresponding to the specified one once the
specified label (if any) for abscissas has been set.
""".
-spec set_x_label( option( label() ), plot_settings() ) -> plot_settings().
set_x_label( MaybeLabel, PlotSettings ) ->
    PlotSettings#plot_settings{
        x_label=text_utils:ensure_maybe_binary( MaybeLabel ) }.



-doc """
Returns the specification of a plot corresponding to the specified one once the
specified label (if any) for ordinate has been set.
""".
-spec set_y_label( option( label() ), plot_settings() ) -> plot_settings().
set_y_label( MaybeLabel, PlotSettings ) ->
    PlotSettings#plot_settings{
        y_label=text_utils:ensure_maybe_binary( MaybeLabel ) }.


-doc """
Returns the specification of a plot corresponding to the specified one once the
number of points to plot have been set.
""".
-spec set_point_count( count(), plot_settings() ) -> plot_settings().
set_point_count( PointCount, PlotSettings ) ->

    NewMetadata = list_table:add_entry( _K=plot_point_count, _V=PointCount,
        PlotSettings#plot_settings.meta_data ),

    PlotSettings#plot_settings{ meta_data=NewMetadata }.



-doc """
Returns the specification of a plot corresponding to the specified one once the
specified key options (if any) have been set.
""".
-spec set_key_options( option( user_key_options() ), plot_settings() ) ->
                                            plot_settings().
set_key_options( MaybeKeyOpts, PlotSettings ) ->
    PlotSettings#plot_settings{
        key_options=text_utils:ensure_maybe_binary( MaybeKeyOpts ) }.



-doc """
Returns a in-plot label, with the specified text at the specified location.
""".
-spec label( label_text(), label_location() ) -> plot_label().
label( Text, Location ) ->
    #plot_label{ text=text_utils:ensure_binary( Text ),
                 location=Location }.


-doc """
Returns a in-plot label, with the specified text at the specified location and
the specified style.
""".
-spec label( label_text(), label_location(), symbol_spec() ) -> plot_label().
label( Text, Location, SymbolSpec ) ->
   #plot_label{ text=text_utils:ensure_binary( Text ),
                location=Location,
                point_type=from_symbol_spec( SymbolSpec ) }.


-doc """
Adds a in-plot label, with the specified text at the specified location,
possibly with a point being marked there.

(most complete label definition)
""".
-spec label( label_text(), label_location(), label_color(),
             label_justification(), label_orientation(), symbol_spec() ) ->
                                                    plot_label().
label( Text, Location, Color, Justification, Orientation, SymbolSpec ) ->
   #plot_label{ text=text_utils:ensure_binary( Text ),
                location=Location,
                color=Color,
                justification=Justification,
                orientation=Orientation,
                point_type=from_symbol_spec( SymbolSpec ) }.




-doc "Adds a in-plot label, with the specified text at the specified location.".
-spec add_label( label_text(), label_location(), plot_settings() ) ->
                                            plot_settings().
add_label( Text, Location, PlotSettings=#plot_settings{ labels=Labels } ) ->
    Label = label( Text, Location ),
    PlotSettings#plot_settings{ labels=[ Label | Labels ] }.



-doc """
Adds a in-plot label, with the specified text at the specified location,
possibly with a point being marked there.
""".
-spec add_label( label_text(), label_location(), symbol_spec(),
                 plot_settings() ) -> plot_settings().
add_label( Text, Location, UsrPtStyleSpec,
           PlotSettings=#plot_settings{ labels=Labels } ) ->
    Label = label( Text, Location, UsrPtStyleSpec ),
    PlotSettings#plot_settings{ labels=[ Label | Labels ] }.



-doc """
Adds a in-plot label, with the specified text at the specified location,
possibly with a point being marked there.

(most complete label definition)
""".
-spec add_label( label_text(), label_location(), label_color(),
                 label_justification(), label_orientation(), symbol_spec(),
                 plot_settings() ) -> plot_settings().
add_label( Text, Location, Color, Justification, Orientation, UsrPtStyleSpec,
           PlotSettings=#plot_settings{ labels=Labels } ) ->

    Label = label( Text, Location, Color, Justification, Orientation,
                   UsrPtStyleSpec ),

    PlotSettings#plot_settings{ labels=[ Label | Labels ] }.



% (helper)
-spec from_symbol_spec( symbol_spec() ) -> point_style().
from_symbol_spec( _DoMarkPoint=false ) ->
    undefined;

from_symbol_spec( _DoMarkPoint=true ) ->
     from_symbol_spec( times ); % Default

from_symbol_spec( MarkPointStyle ) ->
    text_utils:bin_format( "pointtype ~B",
                           [ get_point_style( MarkPointStyle ) ] ).



-doc "Adds the specified in-plot labels.".
-spec add_labels( [ label_spec() ], plot_settings() ) -> plot_settings().
add_labels( _LabelSpecs=[], PlotSettings ) ->
    PlotSettings;

add_labels( _LabelSpecs=[ { Text, Location } | T ], PlotSettings ) ->
    NewPlotSettings = add_label( Text, Location, PlotSettings ),
    add_labels( T, NewPlotSettings );

add_labels( _LabelSpecs=[ { Text, Location, UserPtStSpec } | T ],
            PlotSettings ) ->
    NewPlotSettings = add_label( Text, Location, UserPtStSpec, PlotSettings ),
    add_labels( T, NewPlotSettings );

add_labels( _LabelSpecs=[ { Text, Location, Color, Justification, Orientation,
                            UserPtStSpec } | T ],
            PlotSettings ) ->
    NewPlotSettings = add_label( Text, Location, Color, Justification,
                                 Orientation, UserPtStSpec, PlotSettings ),
    add_labels( T, NewPlotSettings ).



-doc "Removes all in-plot labels.".
-spec remove_labels( plot_settings() ) -> plot_settings().
remove_labels( PlotSettings ) ->
    PlotSettings#plot_settings{ labels=[] }.



-doc "Returns the ordered list of all supported actual symbol specs.".
-spec get_symbol_specs() -> [ symbol_spec() ].
get_symbol_specs() ->
    % Listed in the order of symbol_spec/0, which corresponds to the gnuplot
    % one, the indexes here being incremented compared to the point
    % styles. Booleans not here, as mere "references":
    %
    [ dot, plus, times, star, square_empty, square_filled, circle, disc,
      up_triangle_empty, up_triangle_filled,
      down_triangle_empty, down_triangle_filled, diamond_empty, diamond_filled,
      down_arrow_empty, down_arrow_filled ].



-doc "Returns the backend point style.".
-spec get_point_style( symbol_spec() ) -> point_style().
get_point_style( SymbolSpec ) ->
     case list_utils:get_maybe_index_of( _Elem=SymbolSpec,
                                         _List=get_symbol_specs() ) of

         undefined ->
             throw( { invalid_symbol_spec, SymbolSpec } );

         I ->
             % As per gnuplot conventions (e.g. 'dot' being 0):
             I-1

     end.



-doc """
Declares the specified curves.

Supersedes any previous curve entries.
""".
-spec declare_curves( [ declared_curve_name() ], plot_settings() ) ->
                                            plot_settings().
declare_curves( CurveNames, PlotSettings ) ->

    CurveEntries = transform_curve_names( CurveNames ),

    PlotSettings#plot_settings{ curve_entries=CurveEntries }.



-doc """
Transforms a list of names into a list of `{Number, Name, CurvePlotSuffix}`
curve entries, where Number is the index of the name in the list (starting at
1), Name is a binary name, and CurvePlotSuffix is the curve-specific default
plot suffix.

Respects the order of specified names.

For example `transform_curve_names(["a", "b", "c"])` should result in:
```
[{1,<<"a">>,DefaultBinPlotSuffix}, {2,<<"b">>,DefaultBinPlotSuffix},
  {3,<<"c">>,DefaultBinPlotSuffix}]
```
""".
-spec transform_curve_names( [ declared_curve_name() ] ) -> [ curve_entry() ].
transform_curve_names( CurveNames ) ->
    transform_curve_names( CurveNames, get_default_curve_plot_suffix(), _Acc=[],
                           _Count=1 ).


% (helper)
transform_curve_names( _CurveNames=[], _BinPlotSuffix, Acc, _Count ) ->
    lists:reverse( Acc );

transform_curve_names( _CurveNames=[ CurveName | T ], BinPlotSuffix, Acc,
                       Count ) ->

    CurveEntry = { Count, text_utils:ensure_binary( CurveName ),
                   BinPlotSuffix },

    transform_curve_names( T, BinPlotSuffix, [ CurveEntry| Acc ], Count+1 ).



-doc """
Declares the specified zones.

Supersedes any previous zone entries.
""".
-spec declare_zones( [ declared_zone() ], plot_settings() ) -> plot_settings().
declare_zones( DeclaredZones, PlotSettings=#plot_settings{
                                curve_entries=CurveEntries } ) ->

    ZoneEntries = transform_declared_zones( DeclaredZones, CurveEntries ),

    PlotSettings#plot_settings{ zone_entries=ZoneEntries }.



-doc """
Transforms a list of zone declarations into actual zone entries, while checking
them against the curve names.

(defined for re-use)
""".
-spec transform_declared_zones( [ declared_zone() ], [ curve_entry() ] ) ->
                                        [ zone_entry() ].
transform_declared_zones( DeclaredZones, CurveEntries ) ->
    transform_declared_zones( DeclaredZones, CurveEntries, _Acc=[] ).



% Transforms a list of zone declarations into actual zone entries, while
% checking them against the curve names.
%
% (helper)
transform_declared_zones( _DeclaredZones=[], _CurveEntries, Acc ) ->
    % We preserve order here as well, otherwise zones will be listed in the plot
    % keys in reverse order:
    %
    lists:reverse( Acc );

transform_declared_zones( [ Z={ ZoneName,
                                { FirstCurveName, SecondCurveName } } | T ],
                          CurveEntries, Acc ) ->

    First = get_curve_index_for( FirstCurveName, CurveEntries ),
    Second = get_curve_index_for( SecondCurveName, CurveEntries ),

    % We want to ensure that:
    %
    % 1. at least one actual curve is referenced (not two 'abscissa_*' atoms)
    %
    % 2. if there is one 'abscissa_*' atom, it ends up in first position of the
    % returned pair
    %
    % 3. we preserve the input curve order (useful for plot styles requiring a
    % single value column, like fillsteps, rather than two, like linecurves:
    % they can always select the second curve of the pair):
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
            %{ Second, First }

            % Now preserving input order of normal curves:
            case Second == 'abscissa_top'
                    orelse Second == 'abscissa_bottom' of

                true ->
                    { Second, First };

                false ->
                    { First, Second }

            end

    end,

    ZoneBinName = text_utils:ensure_binary( ZoneName ),

    transform_declared_zones( T, CurveEntries,
        [ { ZoneBinName, NewBounds, get_default_zone_plot_suffix() } | Acc ] );


transform_declared_zones( [ Other | _T ], _CurveEntries, _Acc ) ->
    throw( { invalid_zone_declaration, Other } ).



-doc "Returns an appropriate curve index to define internally a zone.".
-spec get_curve_index_for( declared_curve_id(), [ curve_entry() ] ) ->
                                    extended_curve_id().
get_curve_index_for( CurveId='abscissa_top', _CurveEntries ) ->
    CurveId;

get_curve_index_for( CurveId='abscissa_bottom', _CurveEntries ) ->
    CurveId;

get_curve_index_for( CurveName, CurveEntries ) ->

    BinCurveName = text_utils:ensure_binary( CurveName ),

    case lists:keyfind( _Key=BinCurveName, _Index=2, CurveEntries ) of

        false ->
            throw( { zone_specified_unknown_curve, CurveName, CurveEntries } );

        { CurveIndex, _BinCurveName, _BinPlotSuffix } ->
            CurveIndex

    end.



-doc """
Plots the specified samples, based on the specified plot specification, and
returns the path to the corresponding generated plot file.

Any previous plot file will be overwritten.

The plot will not be specifically displayed.
""".
-spec plot_samples( plot_data(), plot_settings() ) ->
                                            plot_generation_outcome().
plot_samples( PlotData, PlotSettings ) ->
    plot_samples( PlotData, PlotSettings, _DoDisplay=false ).



-doc """
Plots the specified samples, based on the specified plot specification, displays
the result if requested, and returns the path to the corresponding generated
plot file.

Any previous plot file will be overwritten.
""".
-spec plot_samples( plot_data(), plot_settings(), boolean() ) ->
                                            plot_generation_outcome().
plot_samples( PlotData, PlotSettings=#plot_settings{
        name=BinPlotName,
        image_format=ImgFormat,
        plot_directory=MaybePlotDir }, DoDisplay ) ->

    BinPlotDir = basic_utils:set_option( MaybePlotDir,
        file_utils:get_bin_current_directory() ),

    % Same logic as in the command file:
    BinImgFilename = get_plot_filename( PlotSettings ),

    BinPlotPath = file_utils:bin_join( BinPlotDir, BinImgFilename ),

    cond_utils:if_defined( myriad_debug_plot,
        trace_utils:debug_fmt( "Sample plot to be generated in '~ts'.",
                               [ BinPlotPath ] ) ),

    BinCmdFilename = generate_command_file( PlotSettings ),

    _BinDataFilename = generate_data_file( PlotData, PlotSettings ),

    GnuplotExecPath = executable_utils:get_gnuplot_path(),

    BinPlotPath = file_utils:bin_join( BinPlotDir, BinImgFilename ),

    % Not wanting to be confused by a prior file:
    file_utils:remove_file_if_existing( BinPlotPath ),

    % We must set the current directory to the one intended for the plot, yet
    % just for that execution (we do not want to interfere at the level of the
    % whole VM), otherwise the plot image will be created in the current
    % directory; specifying in the command file an absolute path for the image
    % is not an option either, as we want to be able to move around all
    % plot-related files
    %
    { ReturnCode, CmdOutput } = system_utils:run_executable( GnuplotExecPath,
        _Args=[ BinCmdFilename ], _Env=[], _MaybeWorkingDir=MaybePlotDir ),

    { Outcome, Displayable } =
            case file_utils:is_existing_file( BinPlotPath ) of

        % Must have succeeded:
        true ->
            case ReturnCode of

                0 ->
                    case CmdOutput of

                        "" ->
                            { { success, BinPlotPath }, _Displ=true };

                        _ ->
                            WarningMsg = text_utils:format(
                                "Plot generation succeeded for '~ts', but "
                                "following information was output: ~ts",
                                [ BinPlotName, CmdOutput ] ),

                            { { warning, BinPlotPath, WarningMsg },
                              _Displ=true }

                    end;

                % Abnormal, surprising case, we suppose that this plot file is
                % bogus:
                %
                ErrorRetCode ->
                    BaseErrorMsg = case CmdOutput of

                        "" ->
                            " (no generation information output)";

                        _ ->
                            text_utils:format(
                                ", following information was output: '~ts'",
                                [ CmdOutput ] )

                    end,

                    ErrorMsg = text_utils:format( "Plot generation failed "
                        "for '~ts' with error code #~B~ts; "
                        "a plot file was however written",
                        [ BinPlotName, ErrorRetCode, BaseErrorMsg ] ),

                    { { error, ErrorMsg }, _Displ=false }

            end;


        % Must have failed:
        false ->
            ErrorMsg = case ReturnCode of

                % Abnormal, surprising case, as nevertheless failed:
                0 ->
                    case CmdOutput of

                        "" ->
                            text_utils:format( "Plot generation failed "
                                "for '~ts', despite no error or "
                                "output being reported)", [ BinPlotName ] );

                        _ ->
                            text_utils:format( "Plot generation "
                                "failed for '~ts', despite no error being "
                                "reported; output was: '~ts'",
                                [ BinPlotName, CmdOutput ] )

                    end;

                % Abnormal, surprising case, we suppose that this plot file is
                % bogus:
                %
                ErrorRetCode ->
                    BaseErrorMsg = case CmdOutput of

                        "" ->
                            " (no generation information output)";

                        _ ->
                            text_utils:format(
                                ", following information was output: '~ts'",
                                [ CmdOutput ] )
                    end,

                    text_utils:format( "Plot generation failed "
                        "for '~ts'; error code was #~B~ts",
                        [ BinPlotName, ErrorRetCode, BaseErrorMsg ] )

            end,

            { { error, ErrorMsg }, _Displ=false }

    end,

    % Non-blocking:
    DoDisplay andalso Displayable andalso
        executable_utils:display_image_file( BinPlotPath, ImgFormat ),

    Outcome.



-doc """
Plots the specified function within the specified bounds, based on the specified
(lower-level) plot settings (if any, otherwise default ones will be used) and
returns the path to the corresponding generated plot file.

Any previous plot file will be overwritten.
""".
-spec plot( plot_function(), plot_bounds(), option( plot_settings() ) ) ->
                                            plot_generation_outcome().
plot( FunToPlot, Bounds, MaybePlotSettings ) ->
    plot( FunToPlot, Bounds, MaybePlotSettings, _DoDisplay=false ).


-doc """
Plots the specified functions within the specified bounds, based on the
specified (lower-level) plot settings (if any, otherwise default ones will be
used) and returns the path to the corresponding generated plot file.

Any previous plot file will be overwritten.
""".
-spec plot_multi( [ plot_function() ], plot_bounds(),
                  option( plot_settings() ) ) -> plot_generation_outcome().
plot_multi( FunsToPlot, Bounds, MaybePlotSettings ) ->
    plot_multi( FunsToPlot, Bounds, MaybePlotSettings, _DoDisplay=false ).



-doc """
Plots the specified function within the specified bounds, based on the specified
(lower-level) plot settings (if any, otherwise default ones will be used),
displays it if requested, and returns the path to the corresponding generated
plot file.

Any previous plot file will be overwritten.
""".
-spec plot( plot_function(), plot_bounds(), option( plot_settings() ),
            boolean() ) -> plot_generation_outcome().
plot( FunToPlot, Bounds, _MaybePlotSettings=undefined, DoDisplay ) ->
    plot( FunToPlot, Bounds, get_default_plot_settings(), DoDisplay );

plot( FunToPlot, Bounds, PlotSettings=#plot_settings{
        meta_data=MetadataTable }, DoDisplay ) ->

    SampleCount = list_table:get_value_with_default( _K=plot_point_count,
        ?default_sample_count, MetadataTable ),

    % Bounds are canonicalised; returns a list of plot points:
    PlotData = math_utils:sample_as_pairs_for( FunToPlot, Bounds, SampleCount ),

    plot_samples( PlotData, PlotSettings, DoDisplay ).



-doc """
Plots the specified functions within the specified bounds, based on the
specified (lower-level) plot settings (if any, otherwise default ones will be
used), displays them if requested, and returns the path to the corresponding
generated plot file.

Any previous plot file will be overwritten.
""".
-spec plot_multi( [ plot_function() ], plot_bounds(), option( plot_settings() ),
                  boolean() ) -> plot_generation_outcome().
plot_multi( FunsToPlot, Bounds, _MaybePlotSettings=undefined, DoDisplay ) ->
    plot_multi( FunsToPlot, Bounds, get_default_plot_settings(), DoDisplay );

plot_multi( FunsToPlot, Bounds, PlotSettings=#plot_settings{
        meta_data=MetadataTable }, DoDisplay ) ->

    SampleCount = list_table:get_value_with_default( _K=plot_point_count,
        ?default_sample_count, MetadataTable ),

    % Bounds are canonicalised; returns a list of plot points:
    PlotData = math_utils:sample_multi_as_pairs_for( FunsToPlot, Bounds,
                                                     SampleCount ),

    plot_samples( PlotData, PlotSettings, DoDisplay ).




-doc """
Plots the specified function(s) within the specified bounds (see `x_range`,
which must be specified), based on the specified higher-level plot settings (if
any, otherwise default ones will be used), displays such function(s) if
requested, and returns the path to the corresponding generated plot file.

Any previous plot file will be overwritten.
""".
-spec plot( maybe_list( plot_function() ), plot_settings_table() ) ->
                                            plot_generation_outcome().
plot( FunsToPlot, PlotSettingsTable ) when is_list( FunsToPlot ) ->

    { _V=MaybeCurveNames, CurveNamesShrunkPST } =
        list_table:extract_entry_with_default( _K=curve_names,
            _DefaultV=undefined, PlotSettingsTable ),

    CurveNames = case MaybeCurveNames of

        undefined ->
            list_utils:duplicate( _Elem="Unknown curve",
                                  _Count=length( FunsToPlot ) );

       CNs ->
            basic_utils:assert_equal( length( FunsToPlot ), length( CNs ) ),
            CNs

    end,

    % No symbol wanted, as in the case of a function plot, we would have by
    % design one symbol per pixel, blurring the curve (creating a "tube"):
    %
    { Bounds, SetPST, DoDisplay } = get_plot_settings_from_table(
        CurveNamesShrunkPST, _DefaultPlotStyle=lines_only ),

    DeclaredPST = declare_curves( CurveNames, SetPST ),

    plot_multi( FunsToPlot, Bounds, DeclaredPST, DoDisplay );


plot( FunToPlot, PlotSettingsTable ) ->

    { _V=CurveName, CurveNameShrunkPST } =
        list_table:extract_entry_with_default( _K=curve_name,
            _DefaultV="Sampled function", PlotSettingsTable ),


    % No symbol wanted, as in the case of a function plot, we would have by
    % design one symbol per pixel, blurring the curve (creating a "tube"):
    %
    { Bounds, SetPST, DoDisplay } = get_plot_settings_from_table(
        CurveNameShrunkPST, _DefaultPlotStyle=lines_only ),

    DeclaredPST = declare_curves( _CurveNames=[ CurveName ], SetPST ),

    plot( FunToPlot, Bounds, DeclaredPST, DoDisplay ).



-doc """
Returns information extracted from the specified table of plot settings,
including a lower-level `plot_settings` record.

This returned record is not complete, notably because no curve is declared for
it.

Ensures that all entries are extracted from the specified table (so for example
any curve-related entry shall have been extracted beforehand).

(factoring function)
""".
-spec get_plot_settings_from_table( plot_settings_table(),
                                    option( plot_style() ) ) ->
        { plot_bounds(), plot_settings(), DoDisplay :: boolean() }.
get_plot_settings_from_table( PlotSettingsTable, MaybeDefaultPlotStyle ) ->

    % Let's construct a (lower-level) plot_settings record from the table (named
    % PST here), in the order we use to define keys in plot_setting_key/0 (a
    % foldl would have been probably messier).
    %
    % The settings should be best checked when plotting, at the level of the
    % resulting plot_settings record.

    { _V=Name, NameShrunkPST } = list_table:extract_entry_with_default(
        _K=name, _DefaultV=?default_plot_name, PlotSettingsTable ),

    { MaybeTitle, TitlePST } = list_table:extract_entry_with_default(
        title, undefined, NameShrunkPST ),

    { MaybeKeyOpts, KeyOptsPST } = list_table:extract_entry_with_default(
        key_options, undefined, TitlePST ),


    { MaybeXLabel, XLabelPST } = list_table:extract_entry_with_default(
        x_label, undefined, KeyOptsPST ),

    { MaybeYLabel, YLabelPST } = list_table:extract_entry_with_default(
        y_label, undefined, XLabelPST ),


    { MaybeXTickOpts, XTickOptsPST } = list_table:extract_entry_with_default(
        x_tick_options, undefined, YLabelPST ),

    { MaybeYTickOpts, YTickOptsPST } = list_table:extract_entry_with_default(
        y_tick_options, undefined, XTickOptsPST ),


    { MaybeXRange, XRangePST } = list_table:extract_entry_with_default(
        x_range, undefined, YTickOptsPST ),

    % The plot backend (e.g. gnuplot) could do without abscissa bounds, but we
    % cannot, as we sample the function by ourselves:
    %
    XRange = case MaybeXRange of

        undefined ->
            throw( { no_plot_bounds_specified, undefined_x_range } );

        _ ->
            math_utils:check_any_bounds( MaybeXRange )

    end,

    { MaybeYRange, YRangePST } = list_table:extract_entry_with_default(
        y_range, undefined, XRangePST ),


    { IsTimestamped, IsTimestampPST } = list_table:extract_entry_with_default(
        is_timestamped, false, YRangePST ),

    { MaybeTmstpTimeFormat, TmstpTimeFormatPST } =
        list_table:extract_entry_with_default( x_ticks_timestamp_time_format,
                                               undefined, IsTimestampPST ),

    DefaultPlotStyle = basic_utils:set_option( MaybeDefaultPlotStyle,
                                               _Otherwise=symbols_and_lines ),

    { PlotStyle, PlotStylePST } = list_table:extract_entry_with_default(
        plot_style, DefaultPlotStyle, TmstpTimeFormatPST ),

    { SymbolScale, SymbolScalePST } = list_table:extract_entry_with_default(
        symbol_scale, 1, PlotStylePST ),

    { MaybeFillStyle, FillStylePST } = list_table:extract_entry_with_default(
        fill_style, undefined, SymbolScalePST ),


    { CanvasWidth, CanvasWidthPST } = list_table:extract_entry_with_default(
        canvas_width, ?default_canvas_width, FillStylePST ),

    ( is_integer( CanvasWidth ) andalso CanvasWidth > 0 ) orelse
        throw( { invalid_canvas_width, CanvasWidth } ),

    { CanvasHeight, CanvasHeightPST } = list_table:extract_entry_with_default(
        canvas_height, ?default_canvas_height, CanvasWidthPST ),


    { ImgFormat, ImgFormatPST } = list_table:extract_entry_with_default(
        image_format, png, CanvasHeightPST ),

    { Labels, LabelsPST } = list_table:extract_entry_with_default(
        labels, [], ImgFormatPST ),


    { SamplingOffset, SamplingPST } = list_table:extract_entry_with_default(
        sampling_offset, 1, LabelsPST ),

    ( is_integer( SamplingOffset ) andalso SamplingOffset > 0 ) orelse
        throw( { invalid_sampling_offset, SamplingOffset } ),

    PointCount = CanvasWidth div SamplingOffset,


    { ExtraDefines, ExtraDefinesPST } = list_table:extract_entry_with_default(
        extra_defines, [], SamplingPST ),

    { MaybePlotDir, PlotDirPST } = list_table:extract_entry_with_default(
        plot_directory, undefined, ExtraDefinesPST ),

    { MaybePlotFilename, PlotFilenamePST } =
        list_table:extract_entry_with_default(
            plot_filename, undefined, PlotDirPST ),

    { MaybePlotFilename, PlotFilenamePST } =
        list_table:extract_entry_with_default( plot_filename, undefined,
                                               PlotDirPST ),

    { DoDisplay, DisplayPST } = list_table:extract_entry_with_default(
        display, false, PlotFilenamePST ),

    FinalPST = DisplayPST ,

    case list_table:enumerate( FinalPST ) of

        [] ->
            ok;

        ExtraElements ->
            trace_utils:error_fmt( "Unexpected elements found in the plot "
                                   "settings table:~n ~p", [ ExtraElements ] ),
            throw( { invalid_plot_settings, ExtraElements } )

    end,

    PlotMetadata = list_table:new( [ { plot_point_count, PointCount } ] ),

    PlotSettings = #plot_settings{
        name=text_utils:ensure_binary( Name ),
        title=text_utils:ensure_maybe_binary( MaybeTitle ),
        key_options=MaybeKeyOpts,
        x_label=text_utils:ensure_maybe_binary( MaybeXLabel ),
        y_label=text_utils:ensure_maybe_binary( MaybeYLabel ),
        x_tick=MaybeXTickOpts,
        y_tick=MaybeYTickOpts,
        x_range=XRange,
        y_range=MaybeYRange,
        is_timestamped=type_utils:check_boolean( IsTimestamped ),
        x_ticks_timestamp_time_format=MaybeTmstpTimeFormat,
        plot_style=plot_style_to_backend( PlotStyle ),
        point_size=SymbolScale,
        fill_style=fill_style_to_backend( MaybeFillStyle ),
        canvas_width=CanvasWidth,
        canvas_height=CanvasHeight,
        image_format=ImgFormat,
        labels=Labels,
        extra_defines=ExtraDefines,
        meta_data=PlotMetadata,
        plot_directory=text_utils:ensure_maybe_binary( MaybePlotDir ),
        plot_filename=text_utils:ensure_maybe_binary( MaybePlotFilename ) },

    { _Bounds=XRange, PlotSettings, DoDisplay }.


% Section for gnuplot helpers.


-doc "Returns the default per-curve plot suffix, as a binary.".
-spec get_default_curve_plot_suffix() -> curve_plot_suffix().
get_default_curve_plot_suffix() ->
    % "noenhanced" to avoid that a name like 'foo_bar' gets displayed as foo
    % with bar put as subscript.
    %
    <<"noenhanced">>.



-doc "Returns the default per-zone plot suffix, as a binary.".
-spec get_default_zone_plot_suffix() -> curve_plot_suffix().
get_default_zone_plot_suffix() ->
    <<"">>.



-doc "Returns a gnuplot-compatible rotation specification.".
-spec get_formatted_orientation( label_orientation() ) -> command_element().
get_formatted_orientation( upright ) ->
    "norotate";

get_formatted_orientation( Angle ) when is_number( Angle ) ->
    text_utils:format( "rotate by ~p", [ Angle ] ).



-doc "Returns the gnuplot command appropriate to render all registered labels.".
-spec get_label_definitions( [ plot_label() ] ) -> command_element().
get_label_definitions( Labels ) ->
    text_utils:join( _Separator="\n",
                     get_label_definitions( Labels, _Acc=[] ) ).


% (helper)
get_label_definitions( _Labels=[], Acc ) ->
    % Nothing to reverse, labels will end up being rendered in the order they
    % were specified:
    %
    Acc;

get_label_definitions( [ #plot_label{ location={ X, Y }, text=BinText,
        color=Color, justification=MaybeJustif, orientation=Orientation,
        point_type=MaybePointType } | T ], Acc ) ->

    % For a list of supported colors, see:
    % www.uni-hamburg.de/Wiss/FB/15/Sustainability/schneider/gnuplot/colors.htm
    %
    ActualColor = gui_color:get_color_for_gnuplot( Color ),

    JustifStr = case MaybeJustif of

        undefined ->
            "";

        % Atom, but works just as well:
        Justif ->
            Justif

    end,

    PointStr = case MaybePointType of

        undefined ->
            "";

        PointType ->
            text_utils:format( "point ~ts", [ PointType ] )

    end,

    LabelStr = text_utils:format(
        "set label \"~ts\" at ~p,~p ~ts ~ts ~ts textcolor rgbcolor \"~ts\"",
        [ text_utils:binary_to_string( BinText ), X, Y, PointStr, JustifStr,
          get_formatted_orientation( Orientation ), ActualColor ] ),

    get_label_definitions( T, [ LabelStr | Acc ] );

get_label_definitions( [ InvalidLabel | _T ], _Acc ) ->
    throw( { invalid_label_record, InvalidLabel } ).



-doc "Returns Myriad's gnuplot reference version.".
-spec get_gnuplot_reference_version() -> gnuplot_version().
get_gnuplot_reference_version() ->
    ?gnuplot_reference_version.



-doc """
Returns some basic rendering options, depending on the current gnuplot version.

(helper, for code sharing)
""".
-spec get_basic_options( option( gnuplot_version() ) ) ->
            { bin_string(), bin_string() }.
get_basic_options( _MaybeGnuplotVersion=undefined ) ->

    % For an increased safety:
    %trace_utils:warning( "Determining gnuplot options whereas its version "
    %                     "is not known." ),

    get_basic_options_for_older_gnuplot();


get_basic_options( GnuplotVersion ) ->

    % If using a very old version of gnuplot (e.g. prior to 4.2), these key
    % options use default values:

    case basic_utils:compare_versions( GnuplotVersion,
                                       get_gnuplot_reference_version() ) of

        second_bigger ->
            get_basic_options_for_older_gnuplot();

        _ ->
            % Here we have a recent enough gnuplot:
            {

             % By default we prefer not having rotated ticks:

             %_Xtick = <<"rotate by - 45 auto">>,
             %
             % 'out' had been added to avoid that tick marks get hidden by any
             % filled area in the plot (e.g. boxes), yet it was wreaking havoc
             % on the layout and/or cropping done by gnuplot (leading to
             % overlapping legend and truncated timestamps).
             %
             %_Xtick = <<"axis out mirror font \"sans,8\" auto">>,
             _Xtick= <<"out mirror font \"sans,8\" auto">>,

             % Extra size for box, otherwise may collide with inner text:
             _KeyOption=
                <<"bmargin center horizontal width 0.5 height 0.5">> }

            %image_format = <<"svg">>;

    end.


% (helper)
get_basic_options_for_older_gnuplot() ->
    % As here we only have access to an older, limited gnuplot:
    { _Xtick= <<"auto">>, _KeyOption= <<"">> }.



-doc """
Returns the settings for fine control of the major (labeled) ticks on the
abscissa axis, as read from the specified settings.
""".
-spec get_xticks_option( plot_settings() ) -> command_element().
get_xticks_option( #plot_settings{ x_ticks=undefined } ) ->
    "# No x_ticks set.";

get_xticks_option( #plot_settings{ x_ticks=XticksBinSpec } )
                                            when is_binary( XticksBinSpec ) ->
    text_utils:format( "set xtics ~ts~n", [ XticksBinSpec ] );

get_xticks_option( #plot_settings{ x_ticks=Other } ) ->
    throw( { invalid_x_ticks_option, Other } ).



-doc """
Returns the settings for fine control of the major (labeled) ticks on the
ordinate axis, as read from the specified settings.
""".
-spec get_yticks_option( plot_settings() ) -> command_element().
get_yticks_option( #plot_settings{ y_ticks=undefined } ) ->
    "# No y_ticks set.";

get_yticks_option( #plot_settings{ y_ticks=YticksBinSpec } )
                                            when is_binary( YticksBinSpec ) ->
    text_utils:format( "set ytics ~ts~n", [ YticksBinSpec ] );

get_yticks_option( #plot_settings{ y_ticks=Other } ) ->
    throw( { invalid_y_ticks_option, Other } ).



-doc """
Returns the abscissa range options, as read from the specified plot settings.
""".
-spec get_x_range_option( plot_settings() ) -> command_element().
get_x_range_option( #plot_settings{ x_range=undefined} ) ->
    "# No xrange set.";

get_x_range_option( #plot_settings{
            x_range={ _MaybeMinX=undefined, _MaybeMaxX=undefined }  } ) ->
    "# No xrange set.";

get_x_range_option( #plot_settings{
            x_range={ _MaybeMinX=undefined, MaxX } } ) when is_number( MaxX )->
    text_utils:format( "set xrange [:~w]", [ MaxX ] );

get_x_range_option( #plot_settings{
            x_range={ MinX, _MaybeMaxX=undefined } } ) when is_number( MinX )->
    text_utils:format( "set xrange [~w:]", [ MinX ] );

get_x_range_option( #plot_settings{ x_range={ MinX, MaxX } } )
                        when is_number( MinX ) andalso is_number( MaxX ) ->
    text_utils:format( "set xrange [~w:~w]", [ MinX, MaxX ] ).



-doc """
Returns the ordinate range options, as read from the specified plot settings.
""".
-spec get_y_range_option( plot_settings() ) -> command_element().
get_y_range_option( #plot_settings{ y_range=undefined} ) ->
    "# No yrange set.";

get_y_range_option( #plot_settings{
            y_range={ _MaybeMinY=undefined, _MaybeMaxY=undefined }  } ) ->
    "# No yrange set.";

get_y_range_option( #plot_settings{
            y_range={ _MaybeMinY=undefined, MaxY } } ) when is_number( MaxY )->
    text_utils:format( "set yrange [:~w]", [ MaxY ] );

get_y_range_option( #plot_settings{
            y_range={ MinY, _MaybeMaxY=undefined } } ) when is_number( MinY )->
    text_utils:format( "set yrange [~w:]", [ MinY ] );

get_y_range_option( #plot_settings{ y_range={ MinY, MaxY } } )
                        when is_number( MinY ) andalso is_number( MaxY ) ->
    text_utils:format( "set yrange [~w:~w]", [ MinY, MaxY ] ).



-doc "Returns the plot directory that should be used.".
get_plot_directory( #plot_settings{ plot_directory=undefined } ) ->
    file_utils:get_bin_current_directory();

get_plot_directory( #plot_settings{ plot_directory=BinPlotDir } ) ->
    file_utils:is_existing_directory( BinPlotDir ) orelse
        throw( { non_existing_plot_directory, BinPlotDir } ),
    BinPlotDir.



-doc """
Returns the option to control the labels of the major ticks on the x axis, as
read from the specified settings.
""".
-spec get_x_ticks_option( plot_settings() ) -> command_element().
get_x_ticks_option( #plot_settings{ x_ticks=undefined } ) ->
    "# No label set for the major ticks on the x axis.";

get_x_ticks_option( #plot_settings{ x_ticks=XTicksInfo } ) ->
    text_utils:format( "set xtics ~ts", [ XTicksInfo ] ).



-doc """
Returns the option to control the labels of the major ticks on the y axis, as
read from the specified settings.
""".
-spec get_y_ticks_option( plot_settings() ) -> command_element().
get_y_ticks_option( #plot_settings{ x_ticks=undefined } ) ->
    "# No label set for the major ticks on the y axis.";

get_y_ticks_option( #plot_settings{ x_ticks=YTicksInfo } ) ->
    text_utils:format( "set ytics ~ts", [ YTicksInfo ] ).



-doc """
Returns the gnuplot command filename corresponding to the specified plot name.
""".
-spec get_command_filename( plot_name() ) -> bin_file_name().
get_command_filename( Name ) ->
    file_utils:convert_to_filename_with_extension( Name, ?command_extension ).



-doc """
Returns the gnuplot data filename corresponding to the specified plot name.
""".
-spec get_data_filename( plot_name() ) -> bin_file_name().
get_data_filename( Name ) ->
    file_utils:convert_to_filename_with_extension( Name, ?data_extension ).



-doc """
Returns the plot (image) filename implied by the specified plot settings.
""".
-spec get_plot_filename( plot_settings() ) -> bin_file_name().
get_plot_filename( #plot_settings{
        name=BinPlotName,
        image_format=ImgFormat,
        plot_filename=undefined } ) ->
    % Returns a binary:
    file_utils:convert_to_filename_with_extension( BinPlotName,
        gui_image:image_format_to_extension( ImgFormat ) );

get_plot_filename( #plot_settings{ plot_filename=PlotBinFilename } ) ->
    PlotBinFilename.



-doc """
Returns the appropriate settings, depending on whether the abscissa axis gathers
timestamps or not.

Note: now, thanks to quoting, curve offset is always zero as a timestamp
occupies only one column (exactly).
""".
-spec get_timestamp_settings( plot_settings(), boolean() ) ->
                                    { command_element(), curve_offset() }.
get_timestamp_settings( #plot_settings{
        % Overidden here: is_timestamped=xxx,
        x_ticks_timestamp_time_format=MaybeTimeFmt }, _IsTimestamped=true ) ->

    % Also a check:
    UserTimeFormat = case MaybeTimeFmt of

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
        % (we have to add single quotes, so that gnuplot sees:
        %   set timefmt '"%Y/%m/%d %H:%M:%S"'
        % so that we can specify our timestamps as:
        %   "2000/1/1 00:00:00"
        % Indeed, should they be written as:
        %   2000/1/1 00:00:00
        % gnuplot would see two columns
        % (another option is to set another separator than space)
        %
        "set timefmt '\"%Y/%m/%d %H:%M:%S\"'~n"

         % As shall be rendered across axes:
        "set format x ~ts~n", [ TimeFormatStr ] ),

    % No more curve offset, as now in all cases, thanks to quoting, time
    % translates to exactly to the first column (not the first two columns, as
    % when an unquoted timestamp would be read as a time and a date, not as a
    % single value):
    %
    %{ PreambleStr, _CurveOffset=1 };
    { PreambleStr, _CurveOffset=0 };


get_timestamp_settings( _PlotSettings, _IsTimestamped=false ) ->
    { _PreambleStr="", _CurveOffset=0 }.



-doc """
Returns the appropriate settings, depending on whether the abscissa axis gathers
timestamps or not.
""".
-spec get_timestamp_settings( plot_settings() ) ->
                                    { command_element(), curve_offset() }.
get_timestamp_settings( PlotSettings=#plot_settings{
                                is_timestamped=IsTimestamped } ) ->
    get_timestamp_settings( PlotSettings, IsTimestamped ).



-doc """
Returns the gnuplot command appropriate to render that plot.

Defines one plot curve per declared curve, with current plot settings, and
defines as well any specified zone.
""".
-spec get_plot_command( plot_settings(), [ curve_entry() ], curve_offset(),
            [ zone_entry() ], bin_file_name() ) -> command_element().
get_plot_command( _Settings, _CurveEntries=[], _CurveOffset, _ZoneEntries=[],
                  _DataFilename ) ->
    throw( no_curve_or_zone_defined );

get_plot_command( Settings, CurveEntries, CurveOffset, ZoneEntries,
                  DataFilename ) ->

    % Typical expected output:
    %
    % plot 'silver.dat' using 1:2 with lines, 'silver.dat' using 1:3 with lines,
    % 'silver.dat' using 1:2:3 with filledcurves

    % For us it is: "plot " ++ tl(join(_Prefix=", 'silver.dat' using 1:",
    %  ["2 with lines", "3 with lines", "2:3 with filledcurves"]))
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



-doc """
Returns a list of curve entries in which there are no more curves that are used
to define a zone among the ones specified.
""".
-spec remove_zone_specific_curves( [ curve_entry() ], [ zone_entry() ] ) ->
                                            [ curve_entry() ].
remove_zone_specific_curves( CurveEntries, ZoneEntries ) ->

    CurvesToRemove = select_curves( ZoneEntries, _Acc=[] ),

    Selector = fun( { CurveIndex, _CurveName, _CurvePlotSuffix } ) ->
                not lists:member( CurveIndex, CurvesToRemove )
               end,

    lists:filter( Selector, CurveEntries ).



-doc "Returns the plot commands corresponding to the specified curves.".
-spec get_plot_commands_for_curves( [ curve_entry() ], curve_offset(),
                                    plot_settings() ) -> [ command_element() ].
get_plot_commands_for_curves( CurveEntries, CurveOffset, Settings ) ->

    % Curve entries are a list of:
    %  {CurveIndex, BinCurveName, BinPlotSuffix}
    %
    % After some potential reordering, curve entries might be:
    % [{3, <<"c">>, _}, {1,<<"a">>, _}, {2,<<"b">>, _}]
    %
    % We expect: ["4 title \"c\"", "2 title \"a\"", "3 title \"b\""]
    %
    % (note that each curve index is incremented, as the first column is the
    % tick)
    %
    % We simply write them in-order, with an appropriate title:
    [ get_curve_command( C, CurveOffset, Settings ) || C <- CurveEntries ].



-doc "Returns a command element suitable to render the specified curve.".
-spec get_curve_command( curve_entry(), curve_offset(), plot_settings() ) ->
                                command_element().
get_curve_command( { CurveIndex, BinCurveName, BinPlotSuffix }, CurveOffset,
                   _Settings ) ->

    Title = text_utils:binary_to_string( BinCurveName ),

    % +1 to account for the abscissa (time) first field.
    text_utils:format( "~B title \"~ts\" ~ts",
        [ CurveIndex + CurveOffset + 1, Title, BinPlotSuffix ] ).



-doc "Returns command elements suitable to render the specified zones.".
-spec get_plot_commands_for_zones( [ zone_entry() ], curve_offset(),
                                   plot_settings() ) -> [ command_element() ].
get_plot_commands_for_zones( ZoneEntries, CurveOffset,
        Settings=#plot_settings{ plot_style=BinPlotStyle } ) ->

    % Zone entries are a list of:
    % {BinZoneName, {ExtendedCurveName1, ExtendedCurveName2}}
    %
    % We expect returned values to be either "3:5 with filledcurves" (for a zone
    % between curves 2 and 4) or "3 with filledcurves x1" (for a zone between
    % curve 2 and the abscissa axis).
    %
    % Apparently, in terms of order, for a proper rendering, filledcurves shall
    % be rendered from top to bottom, whereas at least for fillsteps the
    % opposite order shall be used (bottom to top); so:
    %
    OrderedZoneEntries = case BinPlotStyle of

        <<"filledcurves">> ->
            ZoneEntries;

        % For example for fillsteps (side-effects: reverses key order):
        _ ->
            lists:reverse( ZoneEntries )

    end,

    [ get_zone_command( Z, CurveOffset, Settings ) || Z <- OrderedZoneEntries ].



-doc "Returns a command element suitable to render the specified zone.".
-spec get_zone_command( zone_entry(), curve_offset(), plot_settings() ) ->
                                command_element().
get_zone_command(
        _ZoneEntry={ BinZoneName, { FirstExtendedCurve, SecondExtendedCurve },
                     ZonePlotSuffix },
        CurveOffset,
        _Settings=#plot_settings{ plot_style=BinPlotStyle } ) ->

    %trace_utils:debug_fmt( "Zone command for entry ~p.", [ ZoneEntry ] ),

    FirstPart = case FirstExtendedCurve of

        'abscissa_top' ->
            % The other curve is necessarily an index (+1, as the first column
            % is the tick/timestamp):
            %
            ActualCurveIndex = SecondExtendedCurve + CurveOffset + 1,
            case BinPlotStyle of

                <<"filledcurves">> ->
                    text_utils:format( "~B with ~ts ~ts below x2",
                        [ ActualCurveIndex, BinPlotStyle, ZonePlotSuffix ] );


                _ ->
                    text_utils:format( "~B with ~ts ~ts",
                        [ ActualCurveIndex, BinPlotStyle, ZonePlotSuffix ] )

            end;


        'abscissa_bottom' ->

            % The other curve is necessarily an index (+1, as the first column
            % is the tick/timestamp):
            %
            ActualCurveIndex = SecondExtendedCurve + CurveOffset + 1,

            case BinPlotStyle of

                <<"filledcurves">> ->
                    text_utils:format( "~B with ~ts ~ts above x1",
                        [ ActualCurveIndex, BinPlotStyle, ZonePlotSuffix ] );

                _ ->
                    text_utils:format( "~B with ~ts ~ts",
                        [ ActualCurveIndex, BinPlotStyle, ZonePlotSuffix ] )

            end;

        _BinCurveName ->
            ActualFirstCurveIndex = FirstExtendedCurve + 1,
            ActualSecondCurveIndex = SecondExtendedCurve + 1,
            case BinPlotStyle of

                <<"filledcurves">> ->
                    text_utils:format( "~B:~B with ~ts ~ts",
                        [ ActualFirstCurveIndex, ActualSecondCurveIndex,
                          BinPlotStyle, ZonePlotSuffix ] );

                _ ->
                    text_utils:format( "~B with ~ts ~ts",
                        [ ActualSecondCurveIndex, BinPlotStyle,
                          ZonePlotSuffix ] )

            end

    end,

    FirstPart ++ text_utils:format( " title \"~ts\"",
        [ text_utils:binary_to_string( BinZoneName ) ] ).



-doc """
Selects all curve indexes that are mentioned in zones (possibly with
duplicates).
""".
select_curves( _ZoneEntries=[], Acc ) ->
    Acc;

select_curves(
        _ZoneEntries=[ { _ZoneName, { abscissa_top, C }, _ZPlotSuffix } | T ],
        Acc ) ->
    select_curves( T, [ C | Acc ] );

select_curves( _ZoneEntries=[
                { _ZoneName, { abscissa_bottom, C }, _ZPlotSuffix } | T ],
               Acc ) ->
    select_curves( T, [ C | Acc ] );

select_curves( _ZoneEntries=[ { _ZoneName, { C1, C2 }, _ZPlotSuffix } | T ],
               Acc ) ->
    select_curves( T, [ C1, C2 | Acc ] ).



-doc """
Adds back the index in the list of curve names, as read from the list of curve
entries, without changing the order of the curve names.

Transforms any plain string in a binary one as well.

For example `add_plot_index_back(["b", "c", "a"], CurveEntries)` with
`CurveEntries=[ {3,<<"a">>}, {2,<<"b">>}, {1,<<"c">>}]` should return:
`[{2,<<"b">>}, {1,<<"c">>}, {3,<<"a">>}]`, i.e. the items of curve names, in
their original order there, with their index added back.
""".
-spec add_plot_index_back( [ declared_curve_name() ], [ curve_entry() ] ) ->
                                                [ curve_entry() ].
add_plot_index_back( CurveNames, CurveEntries ) ->
    add_plot_index_back( CurveNames, CurveEntries, _Acc=[] ).


% (helper)
add_plot_index_back( _CurveNames=[], _CurveEntries, Acc ) ->
    lists:reverse( Acc );

add_plot_index_back( _CurveNames=[ CName | T ], CurveEntries, Acc ) ->

    BinCName = text_utils:ensure_binary( CName ),

    % We do not check for duplicated names and removed ones resulting in a
    % correct length of the name list:
    %
    case lists:keyfind( BinCName, 2, CurveEntries ) of

        false ->
            throw( { unknown_curve, CName } );

        { Index, _CName, PlotSuffix } ->
            add_plot_index_back( T, CurveEntries,
                                 [ { Index, BinCName, PlotSuffix } | Acc ] )

    end.



-doc """
Returns the specified text (possibly a label, a title, etc.) as a plain string
that is escaped so that it is properly rendered by gnuplot as it is (rather than
being interpreted).

For example otherwise, in a rendering, `some_thing` would put the `t` as
subscript.

Note that this applies to potentially-enhanced gnuplot text, i.e. that the
'noenhanced' option shall not be used for them.
""".
-spec get_escaped_text( any_string() ) -> ustring().
get_escaped_text( Text ) ->

    % So that <<"a_b">> becomes "a\\_b", not just "a\_b":

    %text_utils:escape_with( text_utils:ensure_string( Text ),
    %                        _CharsToEscape=[ $_ ], _EscapingChar=$\ ).

    % Yes, 4 anti-slashes needed, as they must visibly be escaped as well:
    text_utils:flatten( string:replace( Text, _SearchPattern="_",
                                        _Replacement="\\\\_", _Where=all ) ).



-doc """
Generates unconditionally an appropriate gnuplot command file corresponding to
the specified plot settings, and returns its (absolute) path.

Returning a filename is more convenient than returning an absolute path, so that
interlinked files can be specified as local files and thus directly moved as a
whole.
""".
-spec generate_command_file( plot_settings() ) -> bin_command_file_name().
generate_command_file( PlotSettings=#plot_settings{
        name=BinPlotName,
        key_options=MaybeKeyOptsBinStr,
        x_label=MaybeXLabelBinStr,
        y_label=MaybeYLabelBinStr,
        x_tick=MaybeXTickBinStr,
        y_tick=MaybeYTickBinStr,
        x_ticks=MaybeXTicksBinStr,
        y_ticks=MaybeYTicksBinStr,
        plot_style=PlotStyle,
        point_size=PointSize,
        fill_style=FillStyle,
        canvas_width=CanvasWidth,
        canvas_height=CanvasHeight,
        image_format=ImgFormat,
        labels=Labels,
        extra_defines=ExtraDefines,
        curve_entries=CurveEntries,
        zone_entries=ZoneEntries } ) ->

    cond_utils:if_defined( myriad_debug_plot,
        trace_utils:debug_fmt( "Generating a command file for plot '~ts'; "
            "curve entries:~n  ~p; zone entries:~n  ~p.",
            [ BinPlotName, CurveEntries, ZoneEntries ] ) ),

    LabelDefs = get_label_definitions( Labels ),

    ExtraDefs = text_utils:join( _Sep="\n",
        [ text_utils:binary_to_string( D ) || D <- ExtraDefines ] ),

    DataFilename = get_data_filename( BinPlotName ),

    XrangeOpt = get_x_range_option( PlotSettings ),

    YrangeOpt = get_y_range_option( PlotSettings ),

    BinPlotDir = get_plot_directory( PlotSettings ),

    BinImgFilename = get_plot_filename( PlotSettings ),

    % We prefer defining relative paths, so that a command file and its
    % dependencies are movable afterwards:
    %
    %ImgFilePath = file_utils:bin_join( BinPlotDir, BinImgFilename ),

    CommandFilename = get_command_filename( BinPlotName ),

    % This one is an entry point, and shall preferably be absolute:
    CommandFilePath = file_utils:bin_join( BinPlotDir, CommandFilename ),

    % Possible race condition if two processes try to create it at once:
    file_utils:remove_file_if_existing( CommandFilePath ),

    % No 'delayed_write' I/O option useful here:
    File = file_utils:open( CommandFilePath, ?base_open_flags ),

    cond_utils:if_defined( myriad_debug_plot,
        trace_utils:debug_fmt( "Generating command file '~ts'.",
                               [ CommandFilePath ] ) ),

    % Changes shall be applied if using timestamps rather than raw ticks:
    { PreambleStr, CurveOffset } = get_timestamp_settings( PlotSettings ),

    PlotCommand = get_plot_command( PlotSettings, CurveEntries, CurveOffset,
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
        "~ts~n" % key_options
        "set pointsize ~B~n"
        "~ts~n" % xtic
        "~ts~n" % ytic

        % Note that {x,y}tics options shall be specified *after* {x,y}tic ones
        % in order to be taken into account:
        %
        "~ts~n" % x_ticks
        "~ts~n" % y_ticks

        "~ts~n" % XrangeOpt
        "~ts~n" % YrangeOpt

        "set title \"~ts\" noenhanced~n"

        "~ts~n" % XLabel
        "~ts~n" % YLabel

        "set datafile missing 'undefined'~n"
        "~ts~n" % get_terminal_info/3
        "~ts~n" % LabelDefs
        "~ts~n" % ExtraDefs
        "set output \"~ts\"~n" % BinImgFilename
        "~ts", % PlotCommand
        [ PreambleStr,
          PlotStyle,
          FillStyle,
          get_key_options( MaybeKeyOptsBinStr ),
          PointSize,
          get_x_tick( MaybeXTickBinStr ),
          get_y_tick( MaybeYTickBinStr ),

          % Corresponding to the '*ticks' counterparts, not the base '*tick'
          % ones:
          %
          get_x_ticks( MaybeXTicksBinStr ),
          get_y_ticks( MaybeYTicksBinStr ),

          XrangeOpt,
          YrangeOpt,
          get_title( PlotSettings ),

          get_x_label( MaybeXLabelBinStr ),
          get_y_label( MaybeYLabelBinStr ),

          get_terminal_info( ImgFormat, CanvasWidth, CanvasHeight ),
          LabelDefs,
          ExtraDefs,
          BinImgFilename,
          PlotCommand ] ),

    file_utils:close( File ),

    % More flexible than CommandFilePath:
    CommandFilename.



-doc "Returns the title from the specified plot settings.".
-spec get_title( plot_settings() ) -> title().
get_title( #plot_settings{ name=BinPlotName,
                           title=undefined } ) ->
    text_utils:format( "Plot '~ts'", [ BinPlotName ] );

get_title( #plot_settings{ title=BinTitle } ) ->
    BinTitle.



-doc "Returns the abscissa label from the specified plot settings.".
-spec get_x_label( option( label_text() ) ) -> elementary_command().
get_x_label( _MaybeXLabelBinStr=undefined ) ->
    "# (no x label)";

get_x_label( XLabelBinStr ) ->
    % Newline added, otherwise the label of the X axis may collide with the
    % upper part of the box of the key:
    %
    %                  "set xlabel \"~ts\" offset 0,2~n"
    text_utils:format( "set xlabel \"~ts\\n\"", [ XLabelBinStr ] ).



-doc "Returns the ordinate label from the specified plot settings.".
-spec get_y_label( option( label_text() ) ) -> elementary_command().
get_y_label( _MaybeYLabelBinStr=undefined ) ->
    "# (no y label)";

get_y_label( YLabelBinStr ) ->
    text_utils:format( "set ylabel \"~ts\"~n", [ YLabelBinStr ] ).



-doc "Returns the terminal information from the specified plot settings.".
-spec get_terminal_info( image_format(), width(), height() ) ->
                                            elementary_command().
get_terminal_info( ImgFormat, CanvasWidth, CanvasHeight ) ->
    % At least currently, ImgFormat can be taken verbatim:
    text_utils:format( "set terminal ~ts size ~B, ~B",
                       [ ImgFormat, CanvasWidth, CanvasHeight ] ).



-doc "Returns the key-related options from the specified plot settings.".
-spec get_key_options( option( key_options() ) ) -> elementary_command().
get_key_options( _MaybeKeyOptsBinStr=undefined ) ->
    "# (no key option)";

get_key_options( KeyOptsBinStr ) ->
    text_utils:format( "set key ~ts", [ KeyOptsBinStr ] ).



-doc "Returns the abscissa tick option from the specified plot settings.".
get_x_tick( _MaxbeXTickBinStr=undefined ) ->
    "# (no xtick option)";

get_x_tick( XTickBinStr ) ->
    text_utils:format( "set xtic ~ts", [ XTickBinStr ] ).



-doc "Returns the ordinate tick option from the specified plot settings.".
get_y_tick( _MaybeYTickBinStr=undefined ) ->
    "# (no ytick option)";

get_y_tick( YTickBinStr ) ->
    text_utils:format( "set ytic ~ts", [ YTickBinStr ] ).



-doc "Returns the abscissa ticks option from the specified plot settings.".
get_x_ticks( _MaxbeXTicksBinStr=undefined ) ->
    "# (no xticks option)";

get_x_ticks( XTicksBinStr ) ->
    text_utils:format( "set xtic ~ts", [ XTicksBinStr ] ).



-doc "Returns the ordinate ticks option from the specified plot settings.".
get_y_ticks( _MaybeYTicksBinStr=undefined ) ->
    "# (no yticks option)";

get_y_ticks( YTicksBinStr ) ->
    text_utils:format( "set ytic ~ts", [ YTicksBinStr ] ).



-doc """
Generates unconditionally an appropriate data file (typically for gnuplot)
corresponding to the specified (non-empty) data and plot settings, and returns
its filename.

Returning a filename is more convenient than returning an absolute path, so that
interlinked files can be specified as local files and thus directly moved as a
whole.
""".
-spec generate_data_file( plot_data(), plot_settings() ) ->
                                            bin_data_file_name().
generate_data_file( _PlotData=[], #plot_settings{ name=BinPlotName } ) ->
    throw( { empty_data_to_plot, BinPlotName } );

generate_data_file( PlotData, PlotSettings=#plot_settings{
        name=BinPlotName,
        %labels=Labels,
        %extra_defines=ExtraDefines,
        plot_filename=MaybePlotBinFilename,
        curve_entries=CurveEntries,
        zone_entries=ZoneEntries,
        row_format_string=MaybeRowFmtStr } ) ->

    cond_utils:if_defined( myriad_debug_plot,
        trace_utils:debug_fmt( "Generating a data file for plot '~ts', whose "
            "first data point is:~n ~p.", [ BinPlotName, hd( PlotData ) ] ) ),

    DataFilename = basic_utils:set_option( MaybePlotBinFilename,
                                           get_data_filename( BinPlotName ) ),

    BinPlotDir = get_plot_directory( PlotSettings ),

    DataFilePath = file_utils:bin_join( BinPlotDir, DataFilename ),

    CurveCount = length( CurveEntries ),

    RowFormatStr = basic_utils:set_option( MaybeRowFmtStr,
        forge_format_string_for( CurveCount ) ),

    FormattedData = format_rows( PlotData, CurveCount, RowFormatStr ),

    file_utils:remove_file_if_existing( DataFilename ),

    % delayed_write would not be terribly useful here, if not
    % counter-productive:
    %
    File = file_utils:open( DataFilePath, ?base_open_flags ),

    write_header( File, CurveEntries, ZoneEntries, BinPlotName, PlotSettings ),

    file_utils:write_ustring( File, FormattedData ),

    file_utils:close( File ).



-doc """
Returns a format string suitable for the writing of the corresponding samples.
""".
-spec forge_format_string_for( curve_count() ) -> format_string().
forge_format_string_for( CurveCount ) ->

    % A (binary) string corresponding to a plot parameter (e.g. a raw tick-like
    % information or actual textual timestamp, if available), then as many
    % values as needed (some of which being possibly 'undefined', hence '~w'):
    %
    "~ts " ++ lists:flatten( lists:duplicate( CurveCount, "~w " ) ) ++ " ~n".



-doc "Formats the specified data rows according to the specified format.".
-spec format_rows( plot_data(), curve_count(), row_format_string() ) ->
                                            ustring().
format_rows( PlotData, CurveCount, RowFormatStr ) ->

    %trace_utils:debug_fmt(
    %  "Row format string: '~ts'; curve count: ~B;~ndata: ~p.",
    %  [ RowFormatStr, CurveCount, PlotData ] ),

    format_rows( _PlotPoints=PlotData, CurveCount, RowFormatStr, _Acc=[] ).



% (helper)
format_rows( _PlotPoints=[], _CurveCount, _RowFormatStr, Acc ) ->
    lists:flatten( Acc );


% If the plot parameter is already a string (e.g. as a timestamp):
format_rows( _PlotPoints=[ _DataRow={ PlotParamStr, PlotSample } | T ],
             CurveCount, RowFormatStr, Acc ) when is_list( PlotParamStr ) ->

    RowStr = format_row( PlotParamStr, PlotSample, CurveCount, RowFormatStr ),

    %trace_utils:debug_fmt( "RowStr: ~ts", [ RowStr ] ),

    format_rows( T, CurveCount, RowFormatStr, [ RowStr | Acc ] );


% Here PlotParam is not a string yet:
format_rows( _PlotPoints=[ _DataRow={ PlotParam, PlotSample } | T ], CurveCount,
             RowFormatStr, Acc ) ->

    % Hope for the best:
    PlotParamStr = text_utils:format( "~w", [ PlotParam ] ),

    RowStr = format_row( PlotParamStr, PlotSample, CurveCount, RowFormatStr ),

    %trace_utils:debug_fmt( "RowStr: ~ts", [ RowStr ] ),

    format_rows( T, CurveCount, RowFormatStr, [ RowStr | Acc ] ).



-doc """
Returns a formatted version of the specified plot sample / data row.

Defined also for reuse.
""".
-spec format_row( ustring(), plot_sample(), curve_count(),
                  row_format_string() ) -> row_data_string().
% Real tuploid:
format_row( PlotParamStr, PlotSample, CurveCount, RowFormatStr )
                                            when is_tuple( PlotSample ) ->

    SampleValues = tuple_to_list( PlotSample ),

    format_row_helper( PlotParamStr, SampleValues, CurveCount, RowFormatStr );


% Basic tuploid:
format_row( PlotParamStr, PlotSample, CurveCount, RowFormatStr ) ->
    format_row_helper( PlotParamStr, _SampleValues=[ PlotSample ], CurveCount,
                       RowFormatStr ).


% (helper)
format_row_helper( PlotParamStr, SampleValues, CurveCount, RowFormatStr ) ->

    % As samples may contain an increasing number of values over time, we have
    % to complement non-defined values with 'undefined' ones:
    %
    case length( SampleValues ) of

        % Simple, standard, fully-specified case:
        CurveCount ->
            text_utils:format( RowFormatStr, [ PlotParamStr | SampleValues ] );

        % The lacking (undefined) values shall be added:
        VCount when VCount < CurveCount ->
            UndefCount = CurveCount - VCount,
            text_utils:format( RowFormatStr,
                [ PlotParamStr | SampleValues ]
                    ++ lists:duplicate( UndefCount, undefined ) );

        LargerCount ->
            throw( { too_many_sample_values, {got,LargerCount},
                {expected,CurveCount}, {sample_values,SampleValues},
                {plot_parameter,PlotParamStr} } )

    end.



-doc "Used (only) by third-party modules.".
-spec write_row( file(), sample_parameter_bin_string(),
                 plot_sample() ) -> void().
write_row( File, SampleParamBinStr, PlotSample ) when is_tuple( PlotSample ) ->
    RowFormatStr = forge_format_string_for( size( PlotSample ) ),
    write_row( File, RowFormatStr, SampleParamBinStr,
               _PlotTerms=tuple_to_list( PlotSample ) );

% Thus a basic tuploid:
write_row( File, SampleParamBinStr, PlotSample ) ->
    RowFormatStr = forge_format_string_for( _Size=1 ),
    write_row( File, RowFormatStr, SampleParamBinStr,
               _PlotTerms=[ PlotSample ] ).



-doc "Used for direct data writing.".
-spec write_row( file(), format_string(), timestamp_bin_string(),
                 [ term() ] ) -> void().
write_row( File, RowFormatStr, SampleParamBinStr, PlotTerms ) ->
    file_utils:write_ustring( File, RowFormatStr,
                              [ SampleParamBinStr | PlotTerms ] ).



-doc "Writes the plot header in the specified data file.".
-spec write_header( file(), [ curve_entry() ], [ zone_entry() ],
                    bin_plot_name(), plot_settings() ) -> void().
write_header( File, CurveEntries, ZoneEntries, BinPlotName,
              #plot_settings{ title=MaybeBinTitle,
                              meta_data=Metadata } ) ->

    { { Year, Month, Day }, { Hour, Minute, Second } } =
        time_utils:get_timestamp(),

    % Curves might have been reordered; of course we want the order of the names
    % to match the order in the data samples, so we reorder them according to
    % the curve index (first element of the curve entry):
    %
    ReorderedCurveEntries = lists:keysort( _Index=1, CurveEntries ),

    %trace_utils:debug_fmt( "Listed curve entries: ~p~nReordered: ~p.",
    %                       [ CurveEntries, ReorderedCurveEntries ] ),

    CurveDescriptions = format_curve_info( ReorderedCurveEntries, _Acc=[] ),

    %trace_utils:debug_fmt( "Curve descriptions: ~p.", [ CurveDescriptions ] ),

    ZoneDescriptions = format_zone_info( ZoneEntries ),

    TitleStr = case MaybeBinTitle of

        undefined ->
            "(no plot title defined)";

        BinTitle ->
            text_utils:format( "Plot title: '~ts'.", [ BinTitle ] )

    end,

    FirstColDescDef =
        "(no information given regarding the meaning of the first column)",

    { FirstColDesc, ShrunkMetadata } = list_table:extract_entry_with_default(
        _K=first_column_description, FirstColDescDef, Metadata ),

    ShrunkMetadataStrs = [ text_utils:format( "~ts: ~p", [ Key, Value ] )
                                || { Key, Value } <- ShrunkMetadata ],

    MetadataStr = text_utils:strings_to_string( ShrunkMetadataStrs,
                                                _Sep="# - " ),

    file_utils:write_ustring( File,
        "# This plot data file has been written on ~B/~B/~B, at "
        "~B:~2..0B:~2..0B, on~n"
        "# host ~ts (node: ~ts).~n~n"
        "# Plot name: '~ts'.~n"
        "# ~ts~n~n"
        "# Associated meta-data: ~ts~n~n"
        "# ~ts~n"
        "# Next columns correspond to following curve names "
        "(in that order):~n~ts~n"
        "# ~ts",
        [ Day, Month, Year, Hour, Minute, Second,
          net_utils:localhost(), net_utils:localnode(), BinPlotName,
          TitleStr, MetadataStr,
          FirstColDesc, CurveDescriptions, ZoneDescriptions ] ).


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

format_zone_info(
        [ { BinName, { FirstBound, SecondBound }, _ZPlotSuffix } | T ], Acc ) ->

    Entry = text_utils:format( "# - zone '~ts', extending from ~p to ~p~n",
                               [ BinName, FirstBound, SecondBound ] ),

    format_zone_info( T, [ Entry | Acc ] ).


% Section for backend-related (gnuplot) conversions.

-doc "Converts our specified plot style into a backend one.".
-spec plot_style_to_backend( plot_style() ) -> backend_plot_style().
plot_style_to_backend( _PlotStyle=symbols_and_lines ) ->
    linespoints;

plot_style_to_backend( _PlotStyle=symbols_only ) ->
    points;

plot_style_to_backend( _PlotStyle=lines_only ) ->
    lines;

plot_style_to_backend( _PlotStyle=boxes ) ->
    boxes;

plot_style_to_backend( _PlotStyle=histograms ) ->
    histograms;

plot_style_to_backend( _PlotStyle=filled_curves ) ->
    filledcurves;

plot_style_to_backend( _PlotStyle=filled_steps ) ->
    fillsteps;

plot_style_to_backend( InvPlotStyle ) ->
    throw( { invalid_plot_style, InvPlotStyle } ).



-doc "Converts our specified fill style into a backend one.".
-spec fill_style_to_backend( fill_style() ) -> backend_fill_style().
fill_style_to_backend( _FillStyle=undefined ) ->
    empty;

% Any kind of color (i.e. any_color()):
fill_style_to_backend( _FillStyle=Color ) ->
    text_utils:format( "fillcolor rgbcolor \"~ts\"",
                       [ gui_color:get_color_for_gnuplot( Color ) ] ).
