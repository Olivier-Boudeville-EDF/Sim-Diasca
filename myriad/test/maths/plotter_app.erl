% Copyright (C) 2025-2026 Olivier Boudeville
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
% Creation date: Thursday, September 4, 2025.

-module(plotter_app).

-moduledoc """
A minimalistic program to plot the function of your choice.

To be run, possibly once modified, as: `make plotter_exec`.
""".


-export([ exec/0 ]).


plot_function() ->

    %FunToPlot = fun( P ) -> 1 - math:exp( -P/2 ) end,

    %FunToPlot = fun( P ) -> 1.0 / (1.0 + math:exp( 1/2 - P ) ) end,

    FunToPlot = fun( P ) -> 1.0 / (1.0 + math:exp( - P ) ) end,

    %Bounds = { 0.0, 5.0 },
    %Bounds = { 0.0, 1.0 },
    Bounds = { -100.0, 100.0 },

    PlotName = text_utils:format( "Plotting of function on ~ts bounds.",
        [ math_utils:any_bounds_to_string( Bounds ) ] ),

    PlotSettings = plot_utils:set_point_count( 500,
        plot_utils:set_title( _Title=PlotName,
            plot_utils:get_default_plot_settings( PlotName ) ) ),

    DoDisplay = not executable_utils:is_batch(),

    plot_utils:plot( FunToPlot, Bounds, PlotSettings, DoDisplay ).



-spec exec() -> no_return().
exec() ->

    test_facilities:start( ?MODULE ),

    case executable_utils:get_maybe_gnuplot_path() of

        undefined ->
            test_facilities:display(
                "No gnuplot tool found, no plot generated." );

        GnuplotPath ->

            test_facilities:display( "Gnuplot available (as '~ts'), "
                "proceeding with plotting.", [ GnuplotPath ] ),

            plot_function()

    end,

    test_facilities:stop().
