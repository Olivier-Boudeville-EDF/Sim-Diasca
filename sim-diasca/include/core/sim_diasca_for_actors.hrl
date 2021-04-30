% Copyright (C) 2012-2021 EDF R&D

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


% This is the main Sim-Diasca header file for simulation models (a.k.a. actors).
%
% It is defined so that actual actors can include a single header, this one, on
% a future-proof manner.


% Defined to be shared:
-include("sim_diasca_actor_types.hrl").


% So that methods (typically static ones) can filter incoming messages at the
% level of the cases/tests:
%
-include("receive_helpers_header.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% A base used for cases as well:
-include("sim_diasca_base.hrl").

-include("receive_helpers_footer.hrl").
