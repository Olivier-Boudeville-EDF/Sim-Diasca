% Copyright (C) 2022-2025 Olivier Boudeville
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
% Creation date: Saturday, February 26, 2022.

-module(gui_window_manager).

-moduledoc """
Services in terms of **window management**, to offer a window manager specific
to a (multi-window) application.

More generally offers global, application-wide features.
""".



-doc """
The name of a window, typically designating its role (e.g. 'top_frame').
""".
-type window_name() :: atom().
 

-export_type([ window_name/0 ]).


-export([ is_maximised/0, maximize/0, set_title/1,
		  reset_opengl_video_mode/2, quit/0 ]).

-compile( { inline, [ get_env/1 ] } ).

% For gui_env_reg_name:
-include("gui_base.hrl").


% Shorthands:

-type width() :: gui:width().
-type height() :: gui:height().
-type title():: gui:title().



-doc """
Tells whether the application - that is its top-level window - is maximised.
""".
-spec is_maximised() -> boolean().
is_maximised() ->
	gui:is_maximised( get_env( top_level_window ) ).



-doc "Maximises or restores the application - that is its top-level window.".
-spec maximize() -> void().
maximize() ->
	gui:maximize( get_env( top_level_window ) ).



-doc """
Sets the title of the application - that is the one of its top-level window.
""".
-spec set_title( title() ) -> void().
set_title( Title ) ->
	gui:set_title( get_env( top_level_window ), Title ).



-doc """
Resets the video mode for OpenGL.

Apparently needed by Mac OS.
""".
-spec reset_opengl_video_mode( width(), height() ) -> void().
reset_opengl_video_mode( _Width, _Height ) ->
	[ GLCanvas, GLContext ] = get_env( [ gl_canvas, gl_context ] ),
	gui:set_focus( GLCanvas ),
	gui_opengl:set_context( GLCanvas, GLContext).



-doc "Quits the application.".
-spec quit() -> void().
quit() ->
	TopWindow = get_env( top_level_window  ),
	gui:destruct_window( TopWindow ),
	gui:stop().



-doc """
Returns the value associated to the specified key in the MyriadGUI environment.

(local helper)
""".
-spec get_env( environment:key() ) -> term().
get_env( Key ) ->
	environment:get( Key, _Designator=?gui_env_reg_name ).
