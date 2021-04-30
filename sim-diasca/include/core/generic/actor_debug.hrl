% Copyright (C) 2018-2021 EDF R&D

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


% Uncomment to enable SimDiasca-level most basic traces for parse transforms:
% -define( enable_sim_diasca_traces, ).


-ifdef(enable_sim_diasca_traces).

 -define( display_trace( S ), trace_utils:info( "[SD] " ++ S ) ).

 -define( display_trace( S, F ),
		  trace_utils:info_fmt( "[SD] " ++ S, F ) ).

-else. % enable_sim_diasca_traces

 % To avoid variables being reported as unused depending on the mode:

 -define( display_trace( S ),
		  basic_utils:ignore_unused( { sim_diasca_trace_disabled, S } ) ).

 -define( display_trace( S, F ),
		  basic_utils:ignore_unused({ sim_diasca_trace_disabled, S, F } ) ).

-endif. % enable_sim_diasca_traces
