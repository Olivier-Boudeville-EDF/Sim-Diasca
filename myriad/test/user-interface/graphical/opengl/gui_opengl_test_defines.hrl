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
% Creation date: Sunday, March 31, 2024.


% This header files allows to share defines for navigation within OpenGL tests.


% Key bindings (Z-being-altitude conventions, i.e. Z-UP); note that the
% user-triggered movements are by default the ones of the model (the object of
% interest), not the ones of the view (the camera), and that they are defined by
% default in absolute terms, relatively to the global coordinate system (as
% opposed to, for example, based on the camera or the local coordinate system of
% the object).
%
% First supposing that a keypad is available:

%-define( has_keypad, true ).
-define( has_keypad, false ).



-if( ?has_keypad =:= true ).


% X (abscissa in the Z-up coordinate system) is controlled by left-right keypad
% numbers/arrows:

% Object moving along the +X axis (to the right on the screen, with the default
% camera) when hitting the key labelled "6" on keypad:
%
-define( square_increase_x_scan_code, ?MYR_SCANCODE_KP_6 ).

% Object moving along the -X axis (to the left on the screen, with the default
% camera) when hitting the key labelled "4" on keypad:
%
-define( square_decrease_x_scan_code, ?MYR_SCANCODE_KP_4 ).


% Y (depth in the Z-up coordinate system)

% Object moving along the +Y axis (to the top of the screen, with the default
% camera) when hitting the key labelled "8" on keypad:
%
-define( square_increase_y_scan_code, ?MYR_SCANCODE_KP_8 ).

% Object moving along the -Y axis (to the bottom of the screen, with the default
% camera) when hitting the key labelled "2" on keypad:
%
-define( square_decrease_y_scan_code, ?MYR_SCANCODE_KP_2 ).


% Z (ordinate / altitude in the Z-up coordinate system)

% Object moving along the +Z axis (from front to behind, with the default
% camera) when hitting the key labelled "9" on keypad:
%
-define( square_increase_z_scan_code, ?MYR_SCANCODE_KP_9 ).

% Object moving along the -Z axis (from behind to front, with the default
% camera) when hitting the key labelled "9" on keypad:
%
-define( square_decrease_z_scan_code, ?MYR_SCANCODE_KP_3 ).



-else. % Not using keypad here:



% X (abscissa in the Z-up coordinate system) is controlled by left-right keypad
% numbers/arrows:

% Object seen moving to the right with the default camera:
-define( square_increase_x_scan_code, ?MYR_SCANCODE_RIGHT ).

% To the left:
-define( square_decrease_x_scan_code, ?MYR_SCANCODE_LEFT ).


% Y (ordinate)

% Up:
-define( square_increase_y_scan_code, ?MYR_SCANCODE_UP ).

% Down:
-define( square_decrease_y_scan_code, ?MYR_SCANCODE_DOWN ).


% Z (depth/altitude)

% Moving nearer/upward:
-define( square_increase_z_scan_code, ?MYR_SCANCODE_PAGEUP ).

% Moving farther/downward:
-define( square_decrease_z_scan_code, ?MYR_SCANCODE_PAGEDOWN ).



-endif. % has_keypad


% Changes the projection mode:
-define( projection_mode_scan_code, ?MYR_SCANCODE_P ).

% Displays the help:
-define( help_scan_code, ?MYR_SCANCODE_H ).

% Ends the test:
-define( quit_scan_code, ?MYR_SCANCODE_ESCAPE ).




% For camera control (movement of its origin in world space), we use the WASD
% keys (translating, with French keyboards, to ZQSD); so using scancodes to
% designate key locations, rather than the characters labelled on them.

% Key layout:
% qWeR
% ASDF

% As comments: onscreen camera movement
-define( camera_decrease_x_scan_code, ?MYR_SCANCODE_A). % go left (-X)
-define( camera_increase_x_scan_code, ?MYR_SCANCODE_D). % go right (+X)

-define( camera_decrease_y_scan_code, ?MYR_SCANCODE_S). % go down (-Y)
-define( camera_increase_y_scan_code, ?MYR_SCANCODE_W). % go up (+Y)

-define( camera_decrease_z_scan_code, ?MYR_SCANCODE_R). % go forward (-Z)
-define( camera_increase_z_scan_code, ?MYR_SCANCODE_F). % go backward (+Z)


% An increment on a given dimension:
-define ( delta_coord, 0.1 ).

% An increment on a given angle, in degrees:
-define ( delta_angle, 2.0 ).

% A factor of a given scaling:
-define ( delta_scale, 0.1 ).
