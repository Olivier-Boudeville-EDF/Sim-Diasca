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


% This is the overall Myriad include; it includes in turn the ones of all topics
% covered by Myriad.


% Possibly useful:
-ifndef(myriad_hrl_guard).
-define(myriad_hrl_guard,).


% In alphabetical order:

-include("data-management/myriad_data_management_includes.hrl").
-include("maths/myriad_maths_includes.hrl").
-include("meta/myriad_meta_includes.hrl").
% Only of interest for scripts: -include("scripts/myriad_script_includes.hrl").
-include("user-interface/myriad_gui_includes.hrl").
-include("utils/myriad_utils_includes.hrl").


-endif. % myriad_hrl_guard
