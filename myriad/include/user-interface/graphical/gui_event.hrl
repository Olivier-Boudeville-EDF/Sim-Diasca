% Copyright (C) 2023-2023 Olivier Boudeville
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
% Creation date: Sunday, June 18, 2023.



% A full, GUI-related applicative state to be kept around, notably so that it
% can be used by the event drivers, therefore in applicative mode (as opposed to
% in direct mode).
%
% One of its interests is to move away from a purely application-specific state
% datastructure: defining a common structure allows to share more behaviours
% that may be applied to it, while still allowing for the needed
% application-specific GUI information.
%
-record( app_gui_state, {

	% Similar to a user event registry, defined to abstract out the various ways
	% for the user to generate application-level events (e.g. based on remapped
	% keys, mouse actions, etc.).
	%
	% Event drivers are to take care of the various types of incoming user
	% events; for maximum flexibility, the built-in default event driver may be
	% overridden by the application.
	%
	% Also aggregates tables translating user events into higher-level
	% application events.
	%

	% Allows to determine how a (lower-level) user event (such as {onResized,
	% [...]}) shall be processed (possibly resulting in an application event),
	% by calling, based on the type of that event, any corresponding registered
	% event driver.
	%
	event_driver_table :: gui_event:event_driver_table(),

	% So that all sorts of basic, atom-based user-level events (like
	% 'window_closed' - as opposed to events related to mice or keyboards) can
	% be converted into application-level events:
	%
	basic_event_table :: gui_event:basic_event_table(),

	% So that a button being clicked may result into an application event:
	button_table :: gui_event:button_table(),

	% So that a key-as-scancode being pressed can result into an application
	% event:
	%
	scancode_table :: gui_event:scancode_table(),

	% So that a key-as-keycode being pressed can result into an application
	% event:
	%
	keycode_table :: gui_event:keycode_table(),



	% Any OpenGL state to be kept around.
	%
	% (exposed separately to be accessible from all drivers in a standard way)
	%
	% Useful as event drivers (at least default ones) may act differently
	% depending on whether OpenGL is used (e.g. when repainting is needed).
	%
	opengl_base_state :: gui_event:opengl_base_state(),


	% Any arbitrary application-specific GUI information (typically a record) to
	% be kept around, notably so that it can be used by the application-specific
	% event drivers.
	%
	% Contains generally references to the widgets instantiated by the
	% application (e.g. the main frame, buttons, etc.), and possibly OpenGL
	% elements.
	%
	app_specific_info :: maybe( gui_event:app_specific_info() ) } ).
