% Copyright (C) 2024-2026 Olivier Boudeville
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
% Creation date: Wednesday, October 9, 2024.


% Possibly useful:
-ifndef(myriad_utils_includes_hrl_guard).
-define(myriad_utils_includes_hrl_guard,).


% In alphabetical order:

-include("app_facilities.hrl").
% Only meant to be included by basic_utils.erl: -include("basic_utils.hrl").
-include("fsm_utils.hrl").
-include("net_utils.hrl").
-include("spawn_utils.hrl").
-include("system_utils.hrl").
% Only to be included by tests: -include("test_facilities.hrl").
-include("time_utils.hrl").
-include("web_utils.hrl").


-endif. % myriad_utils_includes_hrl_guard
