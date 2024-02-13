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


% Describes management (not rendering) options that apply to (basic) probes:
-record( probe_options, {

	% If true, the gnuplot command file will be written at probe start-up, thus
	% preventing the taking into account of any subsequent change in the
	% rendering parameters, but remaining available even if the simulation was
	% to be interrupted.
	%
	create_command_file_initially = false :: boolean(),


	% If true, received sample data will be stored in memory instead of being
	% directly written to disk (default: false, as the memory footprint might
	% become then very significant).
	%
	deferred_data_writes = false :: boolean(),


	% If true, this probe will register itself to the result manager, and be
	% driven by it.
	%
	register_as_tracked_producer = true :: boolean(),


	% Specifies the directory in which the files related to this probe (ex: *.p,
	% *.data, *.png) should be written.
	%
	probe_directory = undefined :: maybe( file_utils:directory_name() ),


	% Allows to disable from the very start, at construction-time, the support
	% for probe rendering, typically to bypass the lookup and version check of
	% gnuplot (which may exhaust the number of opened file descriptors, should
	% many probes be created)
	%
	rendering_enabled = true :: boolean() } ).
