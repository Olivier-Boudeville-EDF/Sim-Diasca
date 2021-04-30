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


% List of common types defined for Soda-Test.



% Duration, in minutes:
%
% Note: a better approach would be to specify a duration in minutes (of virtual
% time), and to convert it at simulation time into a number of ticks (depending
% on the simulation frequency).
%
-type duration() :: unit_utils:minutes().


% Amount of money:
-type amount() :: float().


% Can count:
-type can_count() :: basic_utils:count().

-type deterministic_customer_pid() ::
		class_DeterministicThirstyCustomer:customer_pid().


-type stochastic_customer_pid() ::
		class_StochasticThirstyCustomer:customer_pid().


% Exported to silence unused warning:
-export_type([ duration/0, amount/0, can_count/0,
			   deterministic_customer_pid/0, stochastic_customer_pid/0 ]).
