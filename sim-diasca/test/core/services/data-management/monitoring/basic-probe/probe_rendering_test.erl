% Copyright (C) 2008-2023 EDF R&D
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


% @doc Unit tests for the <b>rendering of (basic, generic) probes</b>.
%
% This is not an integration test like class_Probe_test, we just focus on the
% probe own behaviour, mostly in terms of rendering.
%
% See the class_Probe.erl module.
%
-module(probe_rendering_test).


% To avoid that Dialyzer resolves UseTickOffsets statically:
-export([ run/1 ]).


% Silencing:
-export([ test_basic_rendering/1, test_zone_rendering/1 ]).

% For facilities common to all cases:
-include_lib("traces/include/traces_for_tests.hrl").


% For result_manager_name:
-include("class_ResultManager.hrl").




% @doc Runs the tests.
-spec run() -> no_return().
run() ->
	run( _UseTickOffsets=true ).



test_basic_rendering( UseTickOffsets ) ->

	?test_info( "Creating a basic probe, for basic rendering." ),

	ExtraSettingsTable = table:new( [
		{ curve_colors, [ "ff0000", "00ff00", "0000ff" ] } ] ),

	ProbeDirectory = file_utils:get_current_directory(),

	% Must not be synchronous (otherwise deadlock):
	MyProbe = class_Probe:create_facility_probe( _Name="Basic Test probe",
		_Curves=[ "First curve", "Second curve", "Last curve" ],
		_Zones=[], _Title="This is a basic test of the generic probe class",
		_MaybeXLabel="Simulation tick (20 ms)", _YLabel="Number of events",
		ProbeDirectory, _MetaData=[], ExtraSettingsTable ),

	?test_info( "Sending data to the probe." ),

	% Corresponds to the first second of Y2K (with default settings):
	InitialTick = 3155695200000,

	case UseTickOffsets of

		true ->
			MyProbe ! { setTickOffset, InitialTick };

		false ->
			% Longer, exact, rotated and a probably less useful abscissa labels:
			% Useful otherwise they may overlap:
			MyProbe ! setRotatedTickLabels

	end,

	MyProbe ! { setData, [ InitialTick+1, {1,3,7} ] },

	% This works as well, even if the probe was not created, as determined by
	% the result manager:
	%
	class_Probe:send_data( MyProbe, InitialTick+2, {2,2,3} ),

	% Here we happen to have no relevant value for the second curve:
	MyProbe ! { setData, [ InitialTick+3, {3,undefined,0} ] },

	% We can jump over time-steps:
	MyProbe ! { setData, [ InitialTick+5, {4,2,-1} ] },

	% Surprise, a new curve is added dynamically:
	MyProbe ! { addCurve, ["Dynamically-added curve"] },
	MyProbe ! { setData, [ InitialTick+6, {4,3,1,2} ] },
	MyProbe ! { setData, [ InitialTick+7, {5,2,3,4} ] },
	MyProbe ! { setData, [ InitialTick+8, {4,3,7,0} ] },

	% We can jump over time-steps:
	MyProbe ! { setData, [ InitialTick+10, {5,4,8,1} ] },
	MyProbe ! { setData, [ InitialTick+11, {3,4,2,5} ] },

	% Changing the default settings:
	% (commented out, as could fail with ancient gnuplot versions)
	%MyProbe ! { setKeyOptions, [ "outside right" ] },

	MyProbe ! { setCanvasSize, [800,300] },

	% Uncomment to modify ranges:
	%yProbe ! { setAbscissaRange, [ 2, 13 ] },
	%yProbe ! { setOrdinateRange, [ -5, 5 ] },

	% Let's retrieve the curve names in order to re-order their rendering:
	MyProbe ! { getCurveRenderOrder, [], self() },
	CurveNames = test_receive(),

	?test_notice_fmt( "Original curve names: ~ts.",
					  [ text_utils:strings_to_string( CurveNames ) ] ),

	% Let's suppose we want to swap the third and fourth curves:
	[ N1, N2, N3, N4 ] = CurveNames,

	NewCurveNames = [ N1, N2, N4, N3 ],


	?test_notice_fmt( "Curve names after reordering: ~ts.",
					  [ text_utils:strings_to_string( NewCurveNames ) ] ),

	MyProbe ! { setCurveRenderOrder, [ NewCurveNames ] },

	% Now the dynamic curve is the third, and the so-called "last" is indeed the
	% last.

	?test_info( "Requesting the generation of probe report." ),

	% Manages batch mode and al:
	class_Probe:generate_report_for( MyProbe ),

	class_Probe:delete_facility_probe( MyProbe ).



test_zone_rendering( UseTickOffsets ) ->

	?test_info( "Creating a basic probe, for zone rendering." ),

	ProbeDirectory = file_utils:get_current_directory(),

	ExtraSettingsTable = table:new( [
		% Implies various relevant defaults to be applied:
		{ global_plot_style, fillsteps },
		{ zone_colors, [ _Red="ff0000", _Green="00ff00", _Blue="0000ff",
						 _Black="000000" ] },
		{ extra_defines, [
			"set key title \"This is the {/:Italic title} of this test key\""
						 ] } ] ),

	% Must not be synchronous (otherwise deadlock):
	MyProbe = class_Probe:create_facility_probe( _Name="Zone Test probe",
		_Curves=[ "First curve", "Second curve", "Third curve",
				  "Fourth curve" ],
		% So A will be filled in red, B in green, etc.:
		_Zones=[ { "A", { abscissa_bottom, "First curve"  } },
				 { "B with some {/:Bold important} text",
				   { "First curve", "Second curve" } },
				 { "C", { "Second curve", "Third curve"  } },
				 { "D", { "Third curve", "Fourth curve" } } ],
				 % Not relevant here, there is no roof:
				 %{ "E", { "Fourth curve", abscissa_top  } } ],
		_Title="This is a zone test of the generic probe class",
		_MaybeXLabel="Simulation tick (20 ms)", _YLabel="Number of events",
		ProbeDirectory, _MetaData=[], ExtraSettingsTable ),

	?test_info( "Sending data to the probe." ),

	% Corresponds to the first second of Y2K (with default settings):
	InitialTick = 3155695200000,

	case UseTickOffsets of

		true ->
			MyProbe ! { setTickOffset, InitialTick };

		false ->
			% Longer, exact, rotated and a probably less useful abscissa labels:
			% Useful otherwise they may overlap:
			MyProbe ! setRotatedTickLabels

	end,

	% We expect zones making stripes, with a bump of the third zone (C) within
	% the fourth one (D), while the last zone (E) has an oscillating bottom
	% fronteer.

	% When data is already spaced according to zones (hence already with
	% accumulated offsets):
	%
	class_Probe:send_data( MyProbe, InitialTick+1, {1,2,3,6} ),
	class_Probe:send_data( MyProbe, InitialTick+2, {1,2,3,7} ),
	class_Probe:send_data( MyProbe, InitialTick+3, {1,2,4,6} ),
	class_Probe:send_data( MyProbe, InitialTick+4, {1,2,4,7} ),
	class_Probe:send_data( MyProbe, InitialTick+5, {1,2,3,6} ),

	% When only unitary, per-column data is available (columns are not already
	% stacked to account for zones):
	%
	class_Probe:send_data_to_accumulate( MyProbe, InitialTick+11, {1,1,1,3} ),
	class_Probe:send_data_to_accumulate( MyProbe, InitialTick+12, {1,1,1,4} ),
	class_Probe:send_data_to_accumulate( MyProbe, InitialTick+13, {1,1,2,2} ),
	class_Probe:send_data_to_accumulate( MyProbe, InitialTick+14, {1,1,2,3} ),
	class_Probe:send_data_to_accumulate( MyProbe, InitialTick+15, {1,1,1,3} ),

	% Manages batch mode and al:
	class_Probe:generate_report_for( MyProbe ),

	class_Probe:delete_facility_probe( MyProbe ).




% @doc UseTickOffsets tells whether we should display full ticks or tick offsets
% (note: their origin can be freely defined, there are not necessarily
% simulation tick offsets):
%
run( UseTickOffsets ) ->

	?test_start,

	?test_info( "This test allows to test the rendering of basic probes." ),

	% Here we will emulate from this test the result manager:
	?test_info( "Creating first a mock result manager, "
				"as it is needed by the probe." ),

	_MockResultManager = class_ResultManager:create_mockup_environment(),

	test_basic_rendering( UseTickOffsets ),

	test_zone_rendering( UseTickOffsets ),

	?test_stop.
