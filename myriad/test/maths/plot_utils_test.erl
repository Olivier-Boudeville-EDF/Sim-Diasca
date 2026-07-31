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

-module(plot_utils_test).

-moduledoc """
Unit tests for the **plot-related basic toolbox** facilities.

See the `plot_utils` tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").


% Silencing:
-export([ test_plot_with_lower_level_interface/0,
          test_plot_with_user_interface/0, test_function_plot/0 ]).


% Type shorthand:
-type plot_generation_outcome() :: plot_utils:plot_generation_outcome().



-spec examine_outcome( plot_generation_outcome() ) -> void().
examine_outcome( { success, BinPlotPath } ) ->
    test_facilities:display( "Plot generated in '~ts'.", [ BinPlotPath ] );

examine_outcome( { warning, BinPlotPath, WarningMsg } ) ->
    test_facilities:display( "Plot generated in '~ts', yet led to the "
        "following warning: ~ts.", [ BinPlotPath, WarningMsg ] );

examine_outcome( { error, ErrorMsg } ) ->
    trace_utils:error( ErrorMsg ).



test_plot_with_lower_level_interface() ->

    InitPlotSettings = plot_utils:get_plot_settings(
        "Sun-like Time factors plot" ),

    TitlePlotSettings = plot_utils:set_title( "Evaluation of the time factor "
        "due to a larger mass, based on its distance to the observer.",
        InitPlotSettings ),

    XLabelPlotSettings = plot_utils:set_x_label(
        "Mass-observer distance d, in kilometers", TitlePlotSettings ),

    YLabelPlotSettings = plot_utils:set_y_label( "Time factor Tf, as a ratio "
        "between the proper time\\nof an observer and the coordinate time",
        XLabelPlotSettings ),

    KeyPlotSettings = plot_utils:set_key_options( "box right bottom height 1",
                                                  YLabelPlotSettings ),

    CurvePlotSettings = plot_utils:declare_curves(
        [ "For a Sun-like mass", "For a 10-solar mass" ], KeyPlotSettings ),


    MSun = physics_utils:m_sun(),

    % 10 solar masses:
    M10 = 10 * MSun,

    MSag = physics_utils:m_sagittarius_a_star(),

    % The orders of magnitude between the Sun and Sagittarius A* differ vastly
    % (by a factor of more than 4 million), so they should not be displayed on
    % the same graph.

    % Minimum distance, in kilometers:
    [ SunSchzRadius, TenSchzRadius, SagSchzRadius ] =
        [ physics_utils:get_schwarzschild_radius( M ) / 1000
            || M <- [ MSun, M10, MSag ] ],


    % Closure; D in kilometers here:
    TimeFactorFun = fun( D ) ->
        % As we cannot evaluate a time factor below its Schwarzschild radius:
        case D < TenSchzRadius of

            true ->
                { physics_utils:get_time_factor( 1000 * D, MSun ),
                  undefined };

            false ->
                { physics_utils:get_time_factor( 1000 * D, MSun ),
                  physics_utils:get_time_factor( 1000 * D, M10 ) }

        end

    end,

    SunLabelLoc = { SunSchzRadius, 0 },

    SunLabelText = text_utils:format( "Sun Schwarzschild radius at ~ts",
        [ unit_utils:meters_to_string( 1000 * SunSchzRadius ) ] ),


    TenLabelLoc = { TenSchzRadius, 0 },

    TenLabelText = text_utils:format(
        "10-solar mass Schwarzschild radius at ~ts",
        [ unit_utils:meters_to_string( 1000 * TenSchzRadius ) ] ),


    Color = black,
    Justification = left,
    Orientation = 30,
    UsrPtStyleSpec = true,

    LabelPlotSettings = plot_utils:add_labels( [
        { SunLabelText, SunLabelLoc, Color, Justification, Orientation,
          UsrPtStyleSpec },
        { TenLabelText, TenLabelLoc, Color, Justification, Orientation,
          UsrPtStyleSpec } ], CurvePlotSettings ),

    FinalPlotSettings = LabelPlotSettings,

    % Kilometers:
    Start = SunSchzRadius,
    Stop = 15 * TenSchzRadius,

    % Meters:
    StartM = 1000 * Start,
    StopM = 1000 * Stop,

    SampleCount = 150,

    test_facilities:display(
        "Sampling ~B points from a distance of ~ts (~f m) to ~ts (~f m).",
        [ SampleCount, unit_utils:meters_to_string( StartM ), StartM,
          unit_utils:meters_to_string( StopM ), StopM ] ),

    Pairs = math_utils:sample_as_pairs_for( TimeFactorFun, Start, Stop,
                                            SampleCount ),

    % We add the very first point corresponding to the Schwarzschild radius, as
    % the slope is so large that we cannot have it even by increasing a lot the
    % sample density:
    %
    AllPairs = [ { TenSchzRadius, { undefined,
        physics_utils:get_time_factor( 1000 * TenSchzRadius, M10 ) } }
                                            | Pairs ],

    % [{kilometers(), {percent(), percent()}]:
    %trace_utils:debug_fmt( "Sample pairs: ~w.", [ AllPairs ] ),

    DoDisplay = not executable_utils:is_batch(),

    examine_outcome( plot_utils:plot_samples( AllPairs,
                                              FinalPlotSettings, DoDisplay ) ),

    % Now a curve just for Sagittarius A*:

    SagFirstPlotSettings = plot_utils:remove_labels( FinalPlotSettings ),

    SagPlotSettings = plot_utils:set_plot_name(
        "Sagittarius A* Time factor plot", SagFirstPlotSettings ),


    SagLabelLoc = { SagSchzRadius, 0 },

    SagLabelText = text_utils:format(
        "Sagittarius A* Schwarzschild radius at ~ts",
        [ unit_utils:meters_to_string( 1000 * SagSchzRadius ) ] ),

    SagFinalPlotSettings = plot_utils:add_label( SagLabelText, SagLabelLoc,
        Color, Justification, Orientation, UsrPtStyleSpec, SagPlotSettings ),

    SagStart = SagSchzRadius,
    SagStop = 20 * SagSchzRadius,

    SagCurvePlotSettings = plot_utils:declare_curves(
        [ "For a Sagittarius A*-like mass" ], SagFinalPlotSettings ),

    % In kilometers:
    SagTimeFactorFun = fun( D ) ->
        physics_utils:get_time_factor( 1000 * D, MSag )
                       end,

    % Same as before, adding the exact first point wanted:
    SagPairs = math_utils:sample_as_pairs_for( SagTimeFactorFun,
                                               SagStart, SagStop, SampleCount ),

    examine_outcome( plot_utils:plot_samples( SagPairs, SagCurvePlotSettings,
                                              DoDisplay ) ).


test_function_plot() ->

    P = 0.3456224429838643,

    Lambda1 = 2.507520854513985,
    K1 = 429.196179207228,

    Lambda2 = 1.0401480099991025,
    K2 = 0.10712068420515797,

    FunToPlot = fun( S ) ->
        random_utils:weibull_mixture_pdf( S, P, Lambda1, K1, Lambda2, K2 )
                end,

    % badarith:
    %FunToPlot( 25 ),

    Bounds = { 0.0, 13.0 },
    %Bounds = { 13.03159, 13.0316 },

    PlotName = text_utils:format( "Weibull on ~ts",
                                  [ math_utils:bounds_to_string( Bounds ) ] ),

    Title = text_utils:format( "Weibull-mixture with P=~w, "
        "Lambda1=~w, K1=~w, Lambda2=~w, K2=~w.",
        [ P, Lambda1, K1, Lambda2, K2 ] ),

    %PlotSettings = undefined,
    PlotSettings = plot_utils:set_title( Title,
        plot_utils:get_default_plot_settings( PlotName ) ),

    DoDisplay = not executable_utils:is_batch(),

     examine_outcome(
         plot_utils:plot( FunToPlot, Bounds, PlotSettings, DoDisplay ) ).



test_plot_with_user_interface() ->

    MyLabel = plot_utils:label( _Text="My label", _Pos={ 1.5, 0.5 },
                                _SymbolSpec=up_triangle_empty ),

    % Testing most known settings (abusing list_table with a direct list):
    examine_outcome( plot_utils:plot( _FunToPlot=fun math:sin/1, [
        { name, "My test single plot" },
        { title, "My title" },
        { key_options, "box" },
        { x_label, "My abscissa label" },
        { y_label, "My ordinate label" },
        { x_tick_options, "axis in"},
        { x_range, { 0.0, 2*math:pi() } },
        { y_range, { -1.1, undefined } },
        { is_timestamped, false }, % Useless
        % Not set: x_ticks_timestamp_time_format, plot_style, symbol_scale,
        % fill_style.
        { labels, [ MyLabel ] },
        { curve_name, "My sinus curve name" },
        { display, not executable_utils:is_batch() } ] ) ),

    examine_outcome( plot_utils:plot(
        _FunsToPlot=[ fun math:cos/1, fun math:sqrt/1 ], [
            { name, "My test multi-plot" },
            { title, "My title" },
            { key_options, "box" },
            { x_label, "My abscissa label" },
            { y_label, "My ordinate label" },
            { x_tick_options, "axis in"},
            { x_range, { 0.0, 2*math:pi() } },
            { y_range, { -1.5, undefined } },
            % Not set: is_timestamped, x_ticks_timestamp_time_format,
            % plot_style, symbol_scale, fill_style.
            { labels, [ MyLabel ] },
            { curve_names,
                [ "My cosinus curve name", "My square root curve name" ] },
            { display, not executable_utils:is_batch() } ] ) ).





-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    case executable_utils:get_maybe_gnuplot_path() of

        undefined ->
            test_facilities:display(
                "No gnuplot tool found, no plot generated." );

        GnuplotPath ->

            test_facilities:display( "Gnuplot available (as '~ts'), "
                "proceeding with tests.", [ GnuplotPath ] ),

            %test_plot_with_lower_level_interface(),
            %test_function_plot(),
            test_plot_with_user_interface()

    end,

    test_facilities:stop().
