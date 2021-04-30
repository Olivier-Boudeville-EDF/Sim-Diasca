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
% Creation date: Tuesday, June 18, 2019


% Describes options that apply to web probes.
-record( web_probe_options, {

		   % If true, this probe will register itself to the result manager, and
		   % be driven by it.
		   %
		   register_as_tracked_producer = true :: boolean(),

		   % Specifies the directory in which the files related to this probe
		   % (ex: *.p, *.data, *.png) should be written.
		   %
		   probe_directory = undefined :: maybe( file_utils:directory_name() )

}).

-type web_probe_options() :: #web_probe_options{}.
