% Copyright (C) 2019-2021 Olivier Boudeville
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
% Creation date: Sunday, May 24, 2020.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Currently, by default we are in development mode, and we want as much
% information as possible, even at the expense of some performances, whereas in
% production mode the reciprocal is true.



% Uncomment '#EXECUTION_TARGET := production' in GNUmakevars.inc to test:


% Never both performs exports and definitions in a given include, otherwise
% sooner or later the 'attribute export after function definitions' compilation
% error will not be avoidable:
%
%-export([ get_execution_target/0 ]).



% Returns the execution target this module (hence, probably, that layer as a
% whole) was compiled with, i.e. either the atom 'development' or 'production'.

% Dispatched in actual clauses, otherwise Dialyzer will detect an
% underspecification:
%
% -spec get_execution_target() -> execution_target().

-ifdef(exec_target_is_production).

-spec get_execution_target() -> 'production'.
get_execution_target() ->
	production.

-else. % exec_target_is_production

-spec get_execution_target() -> 'development'.
get_execution_target() ->
	development.

-endif. % exec_target_is_production
