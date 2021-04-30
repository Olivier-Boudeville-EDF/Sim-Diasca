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


% Shared constructs for most cases.
% Avoids code duplication.
%
% Directly obtained from test_constructs.hrl.



% Defines trace title, if not already specified:
-ifndef(TraceTitle).

  -define(TraceTitle,get_title()).

-endif. % TraceTitle


% So that test primitives are still available from cases:

-define( test_emergency( Message ),
  ?case_emergency( Message )
).

-define( test_emergency_fmt( MessageFormat, FormatValues ),
  ?case_emergency_fmt( MessageFormat, FormatValues )
).



-define( test_alert( Message ),
  ?case_alert( Message )
).

-define( test_alert_fmt( MessageFormat, FormatValues ),
  ?case_alert_fmt( MessageFormat, FormatValues )
).



-define( test_critical( Message ),
  ?case_critical( Message )
).

-define( test_critical_fmt( MessageFormat, FormatValues ),
  ?case_critical_fmt( MessageFormat, FormatValues )
).



-define( test_error( Message ),
  ?case_error( Message )
).

-define( test_error_fmt( MessageFormat, FormatValues ),
  ?case_error_fmt( MessageFormat, FormatValues )
).



-define( test_warning( Message ),
  ?case_warning( Message )
).

-define( test_warning_fmt( MessageFormat, FormatValues ),
  ?case_warning_fmt( MessageFormat, FormatValues )
).



-define( test_notice( Message ),
  ?case_notice( Message )
).


-define( test_notice_fmt( MessageFormat, FormatValues ),
  ?case_notice_fmt( MessageFormat, FormatValues )
).



-define( test_info( Message ),
  ?case_info( Message )
).


-define( test_info_fmt( MessageFormat, FormatValues ),
  ?case_info_fmt( MessageFormat, FormatValues )
).



-define( test_debug( Message ),
  ?case_debug( Message )
).


-define( test_debug_fmt( MessageFormat, FormatValues ),
  ?case_debug_fmt( MessageFormat, FormatValues )
).


% For trace facilities:
-include("traces_for_cases.hrl").
