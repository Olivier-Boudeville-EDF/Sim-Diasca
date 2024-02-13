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


% @doc Unit tests for the <b>plot-related basic toolbox</b> facilities.
%
% See the plot_utils tested module.
%
-module(plot_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Silencing:
-export([ test_basic_plot/0, test_function_plot/0 ]).



test_basic_plot() ->

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

	{ success, BinPlotPath } = plot_utils:plot_samples( AllPairs,
		FinalPlotSettings, DoDisplay ),

	test_facilities:display( "Plot file '~ts' generated.", [ BinPlotPath ] ),



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

	{ success, SagBinPlotPath } = plot_utils:plot_samples( SagPairs,
		SagCurvePlotSettings, DoDisplay ),

	test_facilities:display( "Plot file '~ts' generated.", [ SagBinPlotPath ] ).



test_function_plot() ->

	%FunToPlot = fun math:sin/1,

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

	plot_utils:plot( FunToPlot, Bounds, PlotSettings, DoDisplay ).



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

			%test_basic_plot(),
			test_function_plot()

	end,

	test_facilities:stop().
