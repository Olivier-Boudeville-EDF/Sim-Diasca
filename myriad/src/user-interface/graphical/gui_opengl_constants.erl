% Copyright (C) 2023-2023 Olivier Boudeville
%
% This file is part of the Ceylan-Oceanic library.
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
% Creation date: Monday, March 27, 2023.


% @doc Module defining most of the MyriadGUI OpenGL constants.
%
% Called by gui_opengl:generate_support_modules/0.
%
-module(gui_opengl_constants).


-export([ get_debug_source_topic_spec/0, get_debug_type_topic_spec/0,
		  get_debug_severity_topic_spec/0 ] ).


-export([ list_topic_spec_functions/0 ]).


% For the GL defines:
-include_lib("wx/include/gl.hrl").


% Shorthands:

-type debug_source() :: gui_opengl:debug_source().
-type gl_debug_source() :: gui_opengl:gl_debug_source().

-type debug_type() :: gui_opengl:debug_type().
-type gl_debug_type() :: gui_opengl:gl_debug_type().

-type debug_severity() :: gui_opengl:debug_severity().
-type gl_debug_severity() :: gui_opengl:gl_debug_severity().



% @doc Lists all the functions of this module that define a topic specification.
-spec list_topic_spec_functions() -> [ basic_utils:function_name() ].
list_topic_spec_functions() ->

	% Directly adapted from the first export define:
	[ get_debug_source_topic_spec, get_debug_type_topic_spec,
	  get_debug_severity_topic_spec ].



% For the wx defines:
-include("gui_internal_defines.hrl").


% Implementation notes:
%
% These constants correspond to many of the ones that were initially defined as
% one-way functions in gui_opengl, before realising that the other direction was
% useful as well (e.g. for debug context information like sources, types,
% severities).
%
% All topics could be maybe-ones to resist to unknown elements, yet for a
% well-known specification like OpenGL we prefer crashing.
%
% At least generally the first elements are MyriadGUI ones, and the second ones
% are OpenGL ones.
%
% For a topic T, we generate here gui_generated::get_{first,second}_for_T/1 (if
% both directions are enabled).


% Shorthands:

-type topic_spec( F, S ) :: const_bijective_topics:topic_spec( F, S ).



% @doc Returns the two-way conversion specification for the 'debug_source'
% topic, regarding the OpenGL debug context.
%
% First elements are debug_source(), second ones are gl_debug_source().
%
-spec get_debug_source_topic_spec() ->
		topic_spec( debug_source(), gl_debug_source() ).
get_debug_source_topic_spec() ->

	% We use our recommended order (first set for internal, second one for
	% third-party).

	Entries = [
		{ api,             ?GL_DEBUG_SOURCE_API },
		{ window_system,   ?GL_DEBUG_SOURCE_WINDOW_SYSTEM },
		{ shader_compiler, ?GL_DEBUG_SOURCE_SHADER_COMPILER },
		{ third_party,     ?GL_DEBUG_SOURCE_THIRD_PARTY },
		{ application,     ?GL_DEBUG_SOURCE_APPLICATION },
		{ other,           ?GL_DEBUG_SOURCE_OTHER },
		{ all,             ?GL_DONT_CARE } ],

	% Thus strict look-up:
	{ debug_source, Entries }.



% @doc Returns the two-way conversion specification for the 'debug_type'
% topic, regarding the OpenGL debug context.
%
% First elements are debug_type(), second ones are gl_debug_type().
%
-spec get_debug_type_topic_spec() ->
		topic_spec( debug_type(), gl_debug_type() ).
get_debug_type_topic_spec() ->

	% We use our recommended order (first set for internal, second one for
	% third-party).

	Entries = [
		{ type_error,           ?GL_DEBUG_TYPE_ERROR },
		{ deprecated_behaviour, ?GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR },
		{ undefined_behaviour,  ?GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR },
		{ portability,          ?GL_DEBUG_TYPE_PORTABILITY },
		{ performance,          ?GL_DEBUG_TYPE_PERFORMANCE },
		{ marker,               ?GL_DEBUG_TYPE_MARKER },
		{ push_group,           ?GL_DEBUG_TYPE_PUSH_GROUP },
		{ pop_group,            ?GL_DEBUG_TYPE_POP_GROUP },
		{ other,                ?GL_DEBUG_TYPE_OTHER },
		{ all,                  ?GL_DONT_CARE } ],

	% Thus strict look-up:
	{ debug_type, Entries }.



% @doc Returns the two-way conversion specification for the 'debug_severity'
% topic, regarding the OpenGL debug context.
%
% First elements are debug_severity(), second ones are gl_debug_severity().
%
-spec get_debug_severity_topic_spec() ->
		topic_spec( debug_severity(), gl_debug_severity() ).
get_debug_severity_topic_spec() ->

	% We use our recommended order (first set for internal, second one for
	% third-party).

	Entries = [
		{ low,    ?GL_DEBUG_SEVERITY_LOW },
		{ medium, ?GL_DEBUG_SEVERITY_MEDIUM },
		{ high,   ?GL_DEBUG_SEVERITY_HIGH },
		{ all,    ?GL_DONT_CARE } ],

	% Thus strict look-up:
	{ debug_severity, Entries }.
