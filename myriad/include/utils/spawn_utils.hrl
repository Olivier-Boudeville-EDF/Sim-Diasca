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
% Creation date: Saturday, July 6, 2019.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% Currently, by default we are in development mode, and we want as much
% information as possible, even at the expense of some performances, whereas in
% production mode the reciprocal is true.


% Tested in basic_utils_test.erl.


% Uncomment '#EXECUTION_TARGET := production' in GNUmakevars.inc to test:
-ifdef(exec_target_is_production).

  -define( myriad_spawn_info, "basic spawn" ).
  -define( myriad_spawn, spawn ).
  -define( myriad_spawn_link, spawn_link ).

-else.

  % In development mode then:

  % Currently disabled as leading to way too verbose error reports (ex: detailed
  % state of all neighbours):
  %
  %-define( myriad_spawn_info, "proc_lib spawn" ).
  %-define( myriad_spawn, proc_lib:spawn ).
  %-define( myriad_spawn_link, proc_lib:spawn_link ).

  % So currently basic as well:
  -define( myriad_spawn_info, "basic spawn" ).
  -define( myriad_spawn, spawn ).
  -define( myriad_spawn_link, spawn_link ).

-endif. % exec_target_is_production
