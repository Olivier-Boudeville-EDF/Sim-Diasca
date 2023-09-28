% Copyright (C) 2017-2023 Olivier Boudeville
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
% Creation date: Monday, September 11, 2017.


% @doc Gathering of various facilities for <b>OpenGL rendering</b>, notably done
% through wxWidgets.
%
% See gui_opengl_test.erl for the corresponding test.
%
% See gui.erl for more general rendering topics.
%
% Refer to [https://myriad.esperide.org/#for-3d-applications] for further
% information.
%
-module(gui_opengl).


% For example for WX_GL_CORE_PROFILE:
-include_lib("wx/include/wx.hrl").


% "wx/include/gl.hrl" and "wx/include/glu.hrl" included in "gui_opengl.hrl".


% Usage notes:
%
% While error checking (see check_error/0) and the use of a debug context (see
% the 'debug_context' attribute) are invaluable to troubleshoot an application
% using OpenGL, they use resources and are bound to slow down that
% application. As a consequence, release builds will probably disable their use.

% For error management, the 'debug output' extension (which became part of core
% OpenGL since version 4.3; can be queried as the ARB_debug_output (that we
% found as 'GL_ARB_debug_output') or 'AMD_debug_output' extension; apparently
% not available in Mac OS X).
%
% Refer to https://learnopengl.com/In-Practice/Debugging for more information.



% Implementation notes:
%
% Of course at least some sorts of OpenGL software and/or hardware support must
% be available on the local host.
%
% See [https://myriad.esperide.org/#for-3d-applications]


% Of interest with the Erlang standard distribution, one may refer to the
% following modules:
%
% - gl: [https://www.erlang.org/doc/man/gl.html]
% - glu: [https://www.erlang.org/doc/man/glu.html]
%
% One may also refer to the wx:demo/0 function (see
% lib/wx/examples/demo/ex_gl.erl) and to lib/wx/test/wx_opengl_SUITE.erl.
%
% For some reason, they use wx defines (e.g. ?WX_GL_DOUBLEBUFFER) rather than
% pure OpenGL ones (e.g. ?GL_DOUBLEBUFFER). Of course we prefer the latter ones.
%
% Note that almost all OpenGL operations require that an OpenGL context already
% exists, otherwise an no_gl_context error report is expected to be triggered
% (e.g. {'_egl_error_',5086,no_gl_context}).

% Note: with OpenGL, angles are in degrees.

% For most OpenGL operations to succeed, first the wx/gl NIF must be loaded
% (hence after gui:start/0), and a GL context must be available (thus this
% cannot happen before the main frame is shown).

% Relying here only on the wx API version 3.0 (not supporting older ones such as
% 2.8; see wx_opengl_SUITE.erl for an example of supporting both).

% Note that wx operations are mostly message-based (each widget is a process)
% and thus delayed (when the receiver processes a corresponding message),
% whereas OpenGL ones are NIF-based (hence instantaneous). As such some
% operations require to be synchronised (e.g. the creation of a wxGLCanvas must
% be over before OpenGL calls apply to it).

% Much inspiration was taken from the excellent Wings3D modeller (see
% http://www.wings3d.com/).

% What is the interest of wrapping a stable API like OpenGL? This allows us to
% offer primitives that are a bit integrated/higher-level and more typed, to
% elect names (for functions, types, variables, etc.) that we find clearer, to
% provide a more Erlangish API (e.g. by replacing ?GL* defines with atoms), to
% add documentation and conditionally-enabled error checking, to emit traces
% (logs) wherever appropriate and possibly in some future to better encapsulate
% other libraries providing similar features.

% With OpenGL, parameters are set and rarely read. As such, little interest in
% defining constant bijective tables was found.



% For the numerous GL defines notably:
-include("gui_opengl.hrl").

% For the mesh record:
-include("mesh.hrl").


-type gl_base_type() :: ?GL_BYTE | ?GL_UNSIGNED_BYTE | ?GL_UNSIGNED_SHORT
					  | ?GL_SHORT | ?GL_UNSIGNED_INT | ?GL_INT
					  | ?GL_FLOAT | ?GL_DOUBLE.
% The GL base numerical types.


-type enum() :: non_neg_integer().
% A value belonging to an OpenGL enumeration.

-type glxinfo_report() :: [ ustring() ].
% A report issued by the glxinfo executable.


-type vendor_name() :: bin_string().
% The name of the OpenGL vendor of a driver, that is the company responsible for
% the corresponding OpenGL implementation.
%
% For example: `<<"FOOBAR Corporation">>'.


-type vendor() :: 'nvidia'
				| 'amd' % Ex-ATI
				| 'intel'
				| 'unknown'.
% Our identifier for the OpenGL vendor of a driver, that is the company
% responsible for the corresponding OpenGL implementation.
%
% Useful to trigger vendor-specific fixes.


-type renderer_name() :: bin_string().
% The name of the OpenGL renderer of a driver (typically specific to a
% particular configuration of a hardware platform).
%
% For example: `<<"FOOBAR Frobinator GTX 1060 6GB/PCIe/SSE2">>'.


-type platform_identifier() :: { vendor_name(), renderer_name() }.
% Uniquely identifies an (OpenGL driver) platform; does not change from release
% to release, and should be used by platform-recognition algorithms.


-type gl_version() :: basic_utils:three_digit_version().
% An OpenGL version (major/minor/path), typically as queried from a driver.


-type gl_profile() :: 'core'           % Deprecated functions are disabled
					| 'compatibility'. % Deprecated functions are allowed
% An OpenGL profile, typically as queried from a driver. Profiles are defined
% relatively to a particular version of OpenGL.
%
% Apparently the 'compatibility' profile is the default one.


-type gl_extension() :: atom().
% Designates an OpenGL extension.
%
% For example: 'GL_ARB_shader_atomic_counters'.


-type info_table_id() :: ets:tid().
% The identifier of an ETS-based OpenGL information table, registering the
% following static information for an easier/more efficient (frequent) lookup:
%  - {gl_version, gl_version()}
%  - {gl_profile, gl_profile()}
%  - all extensions detected as supported by the current card (an atom entry
%  each)



-opaque gl_canvas() :: wxGLCanvas:wxGLCanvas().
% An OpenGL-based, back-buffered canvas (not to be mixed with a basic
% gui_canvas:canvas/0 one), to which an OpenGL context shall be set in order to
% execute OpenGL commands.
%
% Any OpenGL canvas is not resized when its containers are resized.


% See https://docs.wxwidgets.org/stable/glcanvas_8h.html#wxGL_FLAGS for more
% backend details.
%
% Note though that not all attributes are listed there, a more complete list is
% in https://github.com/wxWidgets/wxWidgets/blob/master/include/wx/glcanvas.h;
% for example the WX_GL_DEBUG define does exist and is indeed managed.
%
-type device_context_attribute() ::

	% Use true color (the default if no attributes at all are specified); do not
	% use a palette. Then each element contains all four components, each of
	% which is clamped to the range [0,1]:
	%
	'rgba' | 'bgra'

	% To enable double-buffering if present:
  | 'double_buffer'

	% Use red buffer with at least this number of bits:
  | { 'min_red_size', bit_size() }

	% Use green buffer with at least this number of bits:
  | { 'min_green_size', bit_size() }

	% Use blue buffer with at least this number of bits:
  | { 'min_blue_size', bit_size() }

	% The number of bits for Z-buffer (typically 0, 16 or 32):
  | { 'depth_buffer_size', bit_size() }

	% Request the use of an OpenGL core profile (as opposed to a mere
	% compatibility one); note that is implies requesting at least OpenGL
	% version 3.0; at least in some settings, this attribute seems to be ignored
	% (compatibility profile being returned).
	%
  | 'use_core_profile'

	% Use an OpenGL debug context:
  | 'debug_context'.


-type gl_canvas_option() :: { 'gl_attributes', [ device_context_attribute() ] }
						  | gui_wx_backend:other_wx_device_context_attribute().
% Options of an OpenGL canvas.


-opaque gl_context() :: wxGLContext:wxGLContext().
% An OpenGL context represents the state of an OpenGL state machine and the
% connection between OpenGL and the running system.
%
% Such a context always uses physical pixels, even on the platforms where the
% window() base widget uses logical pixels. So dimensions like client sizes may
% have to be multiplied by the content scale factor before being used with
% functions like glViewport().


-type factor() :: math_utils:factor().
% A floating-point factor, typically in [0.0,1.0].


-type length_factor() :: math_utils:factor().
% A floating-point factor, typically in [0.0,1.0], typically to designate
% widths, heights, etc. when they have been normalised.
%
% Typically, for textures, dimensions are powers of two, and a length factor is
% the ratio between the real, original texture dimension and the actual one of
% its buffer (a power of two).
%
% This is the coordinate type of UV coordinates.


-type gl_boolean() :: ?GL_TRUE | ?GL_FALSE.


-type matrix_stack() :: ?GL_MODELVIEW
					  | ?GL_PROJECTION
					  | ?GL_TEXTURE
					  | ?GL_COLOR.
% The various matrix stacks available, a.k.a. the current matrix mode.


-type gl_buffer() :: buffer().
% Any type of OpenGL buffer.

-type gl_buffer_id() :: non_neg_integer().
% The identifier of an (OpenGL) buffer, i.e. a "buffer object name".


% Probably better than:
%    'punctual'    % set once and used at most a few times
%  | 'read_only'   % set once and used many times
%  | 'read_write'. % modified repeatedl and used many times
%
-type buffer_usage_hint() ::
		{ buffer_access_usage(), buffer_access_pattern() }.
% Hint given to the GL implementation regarding how a buffer will be
% accessed.
%
% This enables the GL implementation to possibly make more intelligent decisions
% that may significantly impact buffer object performance.


-type buffer_access_usage() ::
	'draw'  % The buffer will be modified by the application, and used as
			% the source for GL drawing and image specification commands.

  | 'read'  % The buffer will be modified by reading data from OpenGL, and
			% used to return that data when queried by the application.

  | 'copy'. % The buffer is modified by reading data from OpenGL, and used
			% to return that data when queried by the application.


-type buffer_access_pattern() ::
	'stream'   % The buffer will be modified once, and used at most a few times.
  | 'static'   % The buffer will be modified once, and used many times.
  | 'dynamic'. % The buffer will be modified repeatedly, and used many times.


-opaque gl_error() :: enum().
% An error code reported by OpenGL.

-opaque glu_error() :: enum().
% An error code reported by GLU.


-opaque any_error() :: gl_error() | glu_error().
% An error code reported by the graphic subsystem, OpenGL or GLU.


-opaque glu_id() :: non_neg_integer().
% An identifier (actually a pointer) returned by GLU (e.g. when creating a
% quadrics). A null value usually means that there was not enough memory to
% allocate the object.


-type polygon_facing_mode() ::
	?GL_FRONT           % for front-facing polygons
  | ?GL_BACK            % for back-facing polygons
  | ?GL_FRONT_AND_BACK. % for front- and back-facing polygons
% A selection of polygons, based on how they face the viewpoint (winding).
%
% For example:
% gl:enable( ?GL_CULL_FACE ), % Enables the culling of faces
% gl:cullFace( ?GL_BACK),     % Culls the back faces
% gl:frontFace( ?GL_CW ),     % Front faces are here the ones whose vertices
%                             % are listed clock-wise


-type rasterization_mode() ::
	?GL_POINT
  | ?GL_LINE
  | ?GL_FILL. % The default, for both front- and back-facing polygons



-type actual_debug_source() :: 'api'
							 | 'window_system'
							 | 'shader_compiler'
							 | 'third_party'
							 | 'application'
							 | 'other'.


-type debug_source() :: actual_debug_source()
					  | 'all'.


% The source of debug messages to enable or disable.
-type gl_debug_source() :: ?GL_DEBUG_SOURCE_API
						 | ?GL_DEBUG_SOURCE_WINDOW_SYSTEM
						 | ?GL_DEBUG_SOURCE_SHADER_COMPILER
						 | ?GL_DEBUG_SOURCE_THIRD_PARTY
						 | ?GL_DEBUG_SOURCE_APPLICATION
						 | ?GL_DEBUG_SOURCE_OTHER
						 | ?GL_DONT_CARE. % all of them
% The (low-level) source of debug messages to enable or disable.



-type actual_debug_type() :: 'type_error'
						   | 'deprecated_behaviour'
						   | 'undefined_behaviour'
						   | 'portability'
						   | 'performance'
						   | 'marker'
						   | 'push_group'
						   | 'pop_group'
						   | 'other'.


-type debug_type() :: actual_debug_type()
					| 'all'.


% The type of debug messages to enable or disable.

-type gl_debug_type() :: ?GL_DEBUG_TYPE_ERROR
					   | ?GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR
					   | ?GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR
					   | ?GL_DEBUG_TYPE_PORTABILITY
					   | ?GL_DEBUG_TYPE_PERFORMANCE
					   | ?GL_DEBUG_TYPE_MARKER
					   | ?GL_DEBUG_TYPE_PUSH_GROUP
					   | ?GL_DEBUG_TYPE_POP_GROUP
					   | ?GL_DEBUG_TYPE_OTHER
					   | ?GL_DONT_CARE. % all of them
% The (low-level) type of debug messages to enable or disable.


-type actual_debug_severity() :: 'low'
							   | 'medium'
							   | 'high'.
% The severity of debug messages to enable or disable.


-type debug_severity() :: actual_debug_severity()
						| 'all'.
% The severity of debug messages to enable or disable.

-type gl_debug_severity() :: ?GL_DEBUG_SEVERITY_LOW
						   | ?GL_DEBUG_SEVERITY_MEDIUM
						   | ?GL_DEBUG_SEVERITY_HIGH
						   | ?GL_DONT_CARE. % all of them
% The (low-level) severity of debug messages to enable or disable.



-type debug_selector() :: 'enable' | 'disable'.
% To filter debug messages.


-type debug_message_id() :: integer().
% Any application-level identifier assigned to a debug message.



% Linear types handled by the 'gl' module.

% These are then transformed into C data types that the actual OpenGL library
% can handle.
%
% With 'gl', no distinction is made between vectors and points: tuples (most
% often of float()) are used.
%
% With Myriad, vectors are lists and points are tuples.
%
% In practice no transformation is needed for vertices (for Myriad, these are
% points, thus already the tuples that gl expects).
%
% Yet normals are vectors, and thus should be lists in Myriad. Using
% tuple_vector/* is then permitted to spare useless conversions between lists
% and tuples.


-type gl_vector2() :: { f(), f() }. % A.k.a. point2:point2().
% A 2D (float) vector, according to the conventions of the gl module.

-type gl_vector3() :: { f(), f(), f() }. % A.k.a. point3:point3().
 % A 3D (float) vector, according to the conventions of the gl module.

-type gl_vector4() :: { f(), f(), f(), f() }. % A.k.a. point4:point4().
 % A (float) 4D vector, according to the conventions of the gl module.


% For matrices, gl uses tuples of floats in their OpenGL standard "column major"
% order.
%
% This corresponds to Myriad canonical matrices once their first element (e.g
% 'matrix2'), which corresponds to the record tag, has been chopped.


-type gl_matrix2() :: { f(), f(), f(), f() }.
% A 2x2 (float) matrix (hence with 4 elements), according to the conventions of
% the gl module.


-type gl_matrix3() :: { f(), f(), f(), f(), f(), f(), f(), f(), f() }.
% A 3x3 (float) matrix (hence with 9 elements), according to the conventions of
% the gl module.


-type gl_matrix4() :: { f(), f(), f(), f(), f(), f(), f(), f(), f(),
						f(), f(), f(), f(), f(), f(), f() }.
% A 4x4 (float) matrix (hence with 16 elements), according to the conventions of
% the gl module.




-type debug_context_message() :: { debug_message_id(), Message :: bin_string(),
	actual_debug_severity(), actual_debug_source(), actual_debug_type() }.
% A message in the debug context, with its metadata.


-export_type([ gl_base_type/0, enum/0, glxinfo_report/0,
			   vendor_name/0, renderer_name/0, platform_identifier/0,
			   gl_version/0, gl_profile/0, gl_extension/0, info_table_id/0,
			   gl_canvas/0, gl_canvas_option/0,
			   device_context_attribute/0, gl_context/0,

			   factor/0, length_factor/0,
			   gl_boolean/0,
			   matrix_stack/0, gl_buffer/0, gl_buffer_id/0,

			   gl_error/0, glu_error/0, any_error/0,
			   glu_id/0,

			   polygon_facing_mode/0, rasterization_mode/0 ]).


% For gl linear types:
-export_type([ gl_vector2/0, gl_vector3/0, gl_vector4/0,
			   gl_matrix2/0, gl_matrix3/0, gl_matrix4/0 ]).


% For the debug context:
-export_type([ actual_debug_source/0, debug_source/0, gl_debug_source/0,
			   actual_debug_type/0, debug_type/0, gl_debug_type/0,
			   actual_debug_severity/0, debug_severity/0, gl_debug_severity/0,
			   debug_selector/0, debug_message_id/0, debug_context_message/0 ]).


% General OpenGL information:
-export([ get_vendor_name/0, get_vendor/0,
		  get_renderer_name/0, get_platform_identifier/0,
		  get_version_string/0, get_version/0, get_version/1,
		  get_supported_profile/0, get_supported_profile/1,
		  get_supported_extensions/0, get_support_description/0,

		  init_info_table/0, secure_info_table/0,
		  is_version_compatible_with/1, is_version_compatible_with/2,
		  is_extension_supported/1, is_extension_supported/2,
		  are_extensions_supported/1, are_extensions_supported/2,
		  get_unsupported_extensions/1, get_unsupported_extensions/2,

		  check_requirements/1, check_requirements/2, check_requirements/3,

		  is_hardware_accelerated/0, is_hardware_accelerated/1,
		  get_glxinfo_strings/0,

		  get_component_size/1 ]).


% Error-related operations:
-export([ check_error/0, check_error/1,
		  check_gl_error/0, check_gl_error/1,
		  check_gl_debug_context_error/0, check_gl_debug_context_error/1,
		  interpret_error/1 ]).


% Support of the OpenGL debug context:
-export([ is_debug_context_supported/0, is_debug_context_enabled/0,

		  enable_all_debug_context_reporting/0,
		  disable_all_debug_context_reporting/0,

		  enable_debug_context_reporting/3, disable_debug_context_reporting/3,

		  set_debug_context_reporting/4, insert_debug_context_message/5,
		  get_debug_context_messages/0, get_debug_context_messages/3,

		  debug_context_message_to_string/1, debug_context_messages_to_string/1

		]).


% Rendering-related operations:
-export([ get_default_canvas_attributes/0,
		  create_canvas/1, create_canvas/2,
		  create_context/1, set_context_on_shown/2, set_context/2,
		  swap_buffers/1,

		  set_polygon_raster_mode/2,

		  render_mesh/1,

		  enter_2d_mode/1, leave_2d_mode/0,

		  set_matrix/1, get_matrix/1,

		  boolean_to_gl/1, buffer_usage_hint_to_gl/1 ]).


% API for module generation:
-export([ generate_support_modules/0 ]).


% Shorthands:

-type f() :: float().

-type two_digit_version() :: basic_utils:two_digit_version().
-type three_digit_version() :: basic_utils:three_digit_version().

-type two_or_three_digit_version() :: two_digit_version()
									| three_digit_version().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type bit_size() :: system_utils:bit_size().
-type byte_size() :: system_utils:byte_size().


-type any_vertex3() :: point3:any_vertex3().

-type unit_normal3() :: vector3:unit_normal3().

-type matrix4() :: matrix4:matrix4().


-type mesh() :: mesh:mesh().
-type indexed_face() :: mesh:indexed_face().
-type face_type() :: mesh:face_type().

-type window() :: gui:window().
-type buffer() :: gui:buffer().

-type render_rgb_color() :: gui_color:render_rgb_color().


% @doc Returns the name of the OpenGL vendor of the current driver, that is the
% company responsible for this OpenGL implementation.
%
% For example: `<<"FOOBAR Corporation">>'.
%
% Only available if a current OpenGL context is set.
%
-spec get_vendor_name() -> vendor_name().
get_vendor_name() ->
	Res= gl:getString( ?GL_VENDOR ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),

	text_utils:string_to_binary( Res ).



% @doc Returns the identifier of the OpenGL vendor of the current driver, that
% is the company responsible for this OpenGL implementation.
%
% Useful to trigger vendor-specific fixes.
%
% Only available if a current OpenGL context is set.
%
-spec get_vendor() -> vendor().
get_vendor() ->
	VendorName = get_vendor_name(),
	CanonicalName = text_utils:to_lowercase( VendorName ),
	Elems = text_utils:split_at_whitespaces( CanonicalName ),

	% By decreasing discrimination power:
	case lists:member( "nvidia", Elems ) of

		true ->
			nvidia;

		false ->
			case lists:member( "intel" , Elems ) of

				true ->
					intel;

				false ->
					case lists:member( "amd", Elems ) of

						true ->
							amd;

						false ->
							trace_utils:warning_fmt( "Unable to determine the "
								"OpenGL vendor corresponding to '~ts'.",
								[ VendorName ] ),
							unknown

					end

			end

	end.



% @doc Returns the name of the OpenGL renderer of the current driver (typically
% specific to a particular configuration of a hardware platform).
%
% For example: `<<"FOOBAR Frobinator GTX 1060 6GB/PCIe/SSE2">>'.
%
% Only available if a current OpenGL context is set.
%
-spec get_renderer_name() -> renderer_name().
get_renderer_name() ->
	Res = gl:getString( ?GL_RENDERER ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),

	text_utils:string_to_binary( Res ).



% @doc Returns the platform driver identifier.
%
% For example: `{<<"FOOBAR Corporation">>, <<"FOOBAR Frobinator GTX 1060
% 6GB/PCIe/SSE2">>}'.
%
% Only available if a current OpenGL context is set.
%
-spec get_platform_identifier() -> platform_identifier().
 get_platform_identifier() ->
	{ get_vendor_name(), get_renderer_name() }.



% @doc Returns the full version of the currently used OpenGL implementation, as
% a string (if any) returned by the driver.
%
% Example: `"4.6.0 FOOBAR 495.44"', or `"4.6 (Compatibility Profile) Mesa
% 23.0.2"'.
%
% Only available if a current OpenGL context is set.
%
-spec get_version_string() -> maybe( ustring() ).
get_version_string() ->
	MaybeRes = try gl:getString( ?GL_VERSION ) of

		Str ->
			Str

	catch E ->
		trace_utils:warning_fmt( "Unable to obtain the OpenGL version "
								 "string: ~p.", [ E ] ),
		undefined

	end,

	cond_utils:if_defined( myriad_check_opengl, check_error() ),

	MaybeRes.



% @doc Returns the major / minor / release version number of the currently used
% OpenGL implementation.
%
% Example: {4,6,1}.
%
% Only supported on GL contexts with version 3.0 and above (hence not that
% convenient).
%
% Only available if a current OpenGL context is set.
%
-spec get_version() -> gl_version().
get_version() ->

	% Instead of single-element lists, extra (garbage?) elements are returned by
	% gl:getIntegerv/1:
	%
	% (disabled, as the third version number, 'release', is not available that
	% way)
	%
	%Major = hd( gl:getIntegerv( ?GL_MAJOR_VERSION ) ),
	%Minor = hd( gl:getIntegerv( ?GL_MINOR_VERSION ) ),

	case get_version_string() of

		undefined ->
			AssumedVersion = { 1, 1, 0 },
			trace_utils:warning_fmt( "Unable to determine the OpenGL version, "
				"assuming ~ts",
				[ text_utils:version_to_string( AssumedVersion ) ] ),
			AssumedVersion;

		VersionStr ->

			%trace_utils:debug_fmt( "OpenGL version string: '~ts'.",
			%                       [ VersionStr ] ),

			% Parsing "4.6.0 FOOBAR 495.44" for example:
			{ MajStr, MinStr, Release } = case text_utils:split( VersionStr,
					_Delimiters=[ $., $ ] ) of

				% Assuming a release version 0 if not having better information:
				[ MajorStr, MinorStr ] ->
					trace_utils:warning( "No release version for OpenGL "
										 "returned, assuming 0."),
					{ MajorStr, MinorStr, _ReleaseI=0 };

				% Typically Mesa:
				[ MajorStr, MinorStr, "(Compatibility" | _ ] ->
					{ MajorStr, MinorStr, 0 };

				[ MajorStr, MinorStr, ReleaseStr | _ ] ->
					ReleaseI = case text_utils:try_string_to_integer(
							ReleaseStr ) of

						undefined ->
							trace_utils:warning_fmt( "No release version for "
								"OpenGL can be parsed from '~ts', assuming 0.",
								[ ReleaseStr ] ),
							0;

						ReleaseInt ->
							ReleaseInt

						end,

				{ MajorStr, MinorStr, ReleaseI }

				end,

			Major = text_utils:string_to_integer( MajStr ),
			Minor = text_utils:string_to_integer( MinStr ),

			{ Major, Minor, Release }

	end.



% @doc Returns the major / minor / release version number of the currently used
% OpenGL implementation, based on a cached value in ETS.
%
% Example: {4,6,1}.
%
% Only supported on GL contexts with version 3.0 and above (hence not that
% convenient).
%
% Only available if a current OpenGL context is set.
%
-spec get_version( info_table_id() ) -> gl_version().
get_version( Tid ) ->
	[ { _VKey, CurrentGLVersion } ] = ets:lookup( Tid, gl_version ),
	CurrentGLVersion.


% @doc Returns whether the specified OpenGL profile is supported.
%
% Only supported on GL contexts with version 3.0 and above.
%
% Only available if a current OpenGL context is set.
%
-spec get_supported_profile() -> gl_profile().
get_supported_profile() ->
	get_supported_profile( _Tid=secure_info_table() ).


% @doc Returns whether the specified OpenGL profile is supported.
%
% Only supported on GL contexts with version 3.0 and above.
%
% Only available if a current OpenGL context is set.
%
-spec get_supported_profile( info_table_id()) -> gl_profile().
get_supported_profile( Tid ) ->

	% Apparently one cannot know a priori whether a given profile is supported:
	% an attempt of context creation shall be made, and if it succeeds then that
	% profile was available, and is used.

	% An option is to detect the GL_ARB_compatibility extension, which is found
	% iff being in a compatibility profile, as it is exclusive with the core
	% one. Indeed both profiles cannot be supported by the same context (since
	% the core profile mandates functional restrictions not present in the
	% compatibility profile); so:

	ProfileFromExt = case is_extension_supported( 'GL_ARB_compatibility',
												  Tid ) of

		true ->
			compatibility;

		false ->
			core

	end,

	% Another option is to rely on glGetIntegerv(GL_CONTEXT_PROFILE_MASK). It
	% will return a bitmask that contains either GL_CONTEXT_CORE_PROFILE_BIT or
	% GL_CONTEXT_COMPATIBILITY_PROFILE_BIT - but apparently these
	% characteristics require at least OpenGL 3.2.

	% Only a single (the first) element is of interest (others are garbage
	% bytes):
	%
	[ ProfMask | _ ] = gl:getIntegerv( ?GL_CONTEXT_PROFILE_MASK ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),

	IsCoreFromMask = ProfMask band ?GL_CONTEXT_CORE_PROFILE_BIT =/= 0,

	IsCompatibilityFromMask =
		ProfMask band ?GL_CONTEXT_COMPATIBILITY_PROFILE_BIT =/= 0,

	case { ProfileFromExt, IsCoreFromMask, IsCompatibilityFromMask } of

		{ core, true, false } ->
			core;

		{ compatibility, false, true } ->
			compatibility;

		Other ->
			throw( { inconsistent_opengl_profile_found, Other } )

	end.



% No need to store profiles in ETS.




% @doc Returns a list of the supported extensions.
%
% For example 390 extensions like 'GL_AMD_multi_draw_indirect',
% 'GL_AMD_seamless_cubemap_per_texture', etc. may be reported by modern
% cards/drivers.
%
-spec get_supported_extensions() -> [ gl_extension() ].
get_supported_extensions() ->
	ExtStr = gl:getString( ?GL_EXTENSIONS ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),

	%trace_utils:debug_fmt( "Extension string: ~ts", [ ExtStr ] ),

	% Wanting to avoid a final space that would result in the '' atom:
	% (a trim would be at least as relevant)
	FilteredExtStr = case list_utils:extract_last_element( ExtStr ) of

		{ $ , PrefixStr } ->
			PrefixStr;

		_ ->
			ExtStr

	end,

	ExtStrs = text_utils:split( FilteredExtStr, _Delimiters=[ $ ] ),
	text_utils:strings_to_atoms( ExtStrs ).


% @doc Returns a synthetic string describing the host-local OpenGL support, also
% known as the current GL connection.
%
-spec get_support_description() -> ustring().
get_support_description() ->

	% So that it is requested only once:
	Exts = get_supported_extensions(),

	% get_supported_profile/0 will use it:
	TId = init_info_table( Exts ),

	VendStr = text_utils:format( "driver vendor: ~ts", [ get_vendor_name() ] ),

	RendStr = text_utils:format( "driver renderer: ~ts",
								 [ get_renderer_name() ] ),

	% Checks that a proper version could be obtained indeed:
	ImplStr = text_utils:format( "implementation version: described as '~ts', "
		"i.e. ~ts", [ get_version_string(),
					  text_utils:version_to_string( get_version( TId ) ) ] ),

	ProfStr = text_utils:format( "supported profile: ~ts",
		[ get_supported_profile() ] ),

	DebugStr = "debug context: " ++ case is_debug_context_supported() of

		true ->
			"supported " ++ case is_debug_context_enabled() of

				true ->
					"and enabled";

				false ->
					"but not enabled"

							end;

		false ->
			"not supported"

	end,

	ShadStr = text_utils:format( "shading language version: ~ts",
		[ gui_shader:get_shading_language_version() ] ),

	% Way too long (e.g. 390 extensions returned):
	%ExtStr = text_utils:format( "~B OpenGL extensions: ~ts", [ length( Exts ),
	%   text_utils:atoms_to_listed_string( Exts ) ] ),

	ExtStr = text_utils:format( "~B OpenGL extensions supported",
								[ length( Exts ) ] ),

	text_utils:strings_to_string(
		[ VendStr, RendStr, ImplStr, ProfStr, DebugStr, ShadStr, ExtStr ] ).



% @doc Initialises the OpenGL information ETS table storing (caching) related
% static versions, profiles and extensions.
%
-spec init_info_table() -> info_table_id().
init_info_table() ->
	init_info_table( _SupportedExts=get_supported_extensions() ).


% @doc Initialises the OpenGL information ETS table storing (caching) related
% static versions, profiles and extensions.
%
-spec init_info_table( [ gl_extension() ] ) -> info_table_id().
init_info_table( SupportedExts ) ->

	TableId = ets:new( ?gl_info_ets_name,
					   [ named_table, public, ordered_set ] ),

	ExtsAsMonoTuples = [ { E } || E <- SupportedExts ],

	Elems = [ { gl_version, get_version() } | ExtsAsMonoTuples ],

	ets:insert( TableId, Elems ),

	% Two passes needed as table used here:
	GlProf = get_supported_profile( TableId ),

	ets:insert( TableId, { gl_profile, GlProf } ),

	% Actually is just ?gl_info_ets_name:
	TableId.



% @doc Returns the OpenGL information ETS table, possibly after having created
% it if necessary.
%
-spec secure_info_table() -> info_table_id().
secure_info_table() ->

	case ets:whereis( ?gl_info_ets_name ) of

		undefined ->
			init_info_table();

		Tid ->
			Tid

	end.



% @doc Tells whether the current OpenGL version is compatible with the specified
% one, that is whether the current one matches, or is more recent than, the
% specified one.
%
-spec is_version_compatible_with( two_or_three_digit_version() ) -> boolean().
is_version_compatible_with( TargetVersion ) ->
	is_version_compatible_with( TargetVersion, secure_info_table() ).


% @doc Tells whether the current OpenGL version is compatible with the specified
% one, that is whether the current one matches, or is more recent than, the
% specified one.
%
-spec is_version_compatible_with( two_or_three_digit_version(),
								  info_table_id() ) -> boolean().
is_version_compatible_with( { Major, Minor }, Tid ) ->
	is_version_compatible_with( { Major, Minor, _Release=0 }, Tid );

is_version_compatible_with( TargetThreeDigitVersion, Tid ) ->
	[ { _VKey, CurrentGLVersion } ] = ets:lookup( Tid, gl_version ),
	case basic_utils:compare_versions( CurrentGLVersion,
									   TargetThreeDigitVersion ) of

		second_bigger ->
			false;

		_EqualOrFirstBigger ->
			true

	end.



% @doc Tells whether the specified OpenGL extension is supported.
-spec is_extension_supported( gl_extension() ) -> boolean().
is_extension_supported( Extension ) ->
	is_extension_supported( Extension, _Tid=secure_info_table() ).


% @doc Tells whether the specified OpenGL extension is supported.
-spec is_extension_supported( gl_extension(), info_table_id() ) -> boolean().
is_extension_supported( Extension, Tid ) when is_atom( Extension ) ->
	ets:member( Tid, Extension ).


% @doc Tells whether the specified OpenGL extensions are (all) supported.
-spec are_extensions_supported( [ gl_extension() ] ) -> boolean().
are_extensions_supported( Extensions ) ->
	are_extensions_supported( Extensions, secure_info_table() ).


% @doc Tells whether the specified OpenGL extensions are (all) supported.
-spec are_extensions_supported( [ gl_extension() ], info_table_id() ) ->
											boolean().
are_extensions_supported( _Exts=[], _Tid ) ->
	true;

are_extensions_supported( _Exts=[ Ext | T ], Tid ) ->
	case is_extension_supported( Ext, Tid ) of

		true ->
			are_extensions_supported( T, Tid );

		false ->
			false

	end.



% @doc Returns a list of the specified OpenGL extensions that are not supported.
-spec get_unsupported_extensions( [ gl_extension() ] ) -> [ gl_extension() ].
get_unsupported_extensions( Extensions ) ->
	get_unsupported_extensions( Extensions, secure_info_table() ).


% @doc Returns a list of the specified OpenGL extensions that are not supported.
-spec get_unsupported_extensions( [ gl_extension() ], info_table_id() ) ->
												[ gl_extension() ].
get_unsupported_extensions( Extensions, Tid ) ->
	[ E || E <- Extensions, not is_extension_supported( E, Tid ) ].



% Subsection for the management of debug contexts.


% @doc Tells whether the OpenGL debug context is supported on this host.
%
% Always true with OpenGL version 4.3 or higher.
%
-spec is_debug_context_supported() -> boolean().
is_debug_context_supported() ->
	is_extension_supported( 'GL_ARB_debug_output' ) orelse
		is_extension_supported( 'AMD_debug_output' ).


% @doc Tells whether the OpenGL debug context is enabled on this host.
-spec is_debug_context_enabled() -> boolean().
is_debug_context_enabled() ->
	Flags = hd( gl:getIntegerv( ?GL_CONTEXT_FLAGS ) ),
	Flags band ?GL_CONTEXT_FLAG_DEBUG_BIT =/= 0.



% @doc Enables all reporting regarding the OpenGL debug context.
-spec enable_all_debug_context_reporting() -> void().
enable_all_debug_context_reporting() ->
	enable_debug_context_reporting( _DebugSrc=all, _DebugType=all,
									_DebugSeverity=all ).


% @doc Disables all reporting regarding the OpenGL debug context.
-spec disable_all_debug_context_reporting() -> void().
disable_all_debug_context_reporting() ->
	disable_debug_context_reporting( _DebugSrc=all, _DebugType=all,
									 _DebugSeverity=all ).



% @doc Specifies how the reporting of debug messages in a debug context shall be
% done, by enabling the specified message source, type and severity.
%
% Although debug messages may be enabled in a non-debug context, the quantity
% and detail of such messages may be substantially inferior to those in a debug
% context. In particular, a valid implementation of the debug message queue in a
% non-debug context may produce no messages at all.
%
-spec enable_debug_context_reporting( debug_source(), debug_type(),
									  debug_severity() ) -> void().
enable_debug_context_reporting( TargetSource, MessageType, MessageSeverity ) ->

	gl:enable( ?GL_DEBUG_OUTPUT ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),

	gl:enable( ?GL_DEBUG_OUTPUT_SYNCHRONOUS ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ),

	set_debug_context_reporting( TargetSource, MessageType, MessageSeverity,
								 _DebugSelector=enable ).


% @doc Specifies how the reporting of debug messages in a debug context shall be
% done, by disabling the specified message source, type and severity.
%
% Although debug messages may be enabled in a non-debug context, the quantity
% and detail of such messages may be substantially inferior to those in a debug
% context. In particular, a valid implementation of the debug message queue in a
% non-debug context may produce no messages at all.
%
-spec disable_debug_context_reporting( debug_source(), debug_type(),
								   debug_severity() ) -> void().
disable_debug_context_reporting( TargetSource, MessageType, MessageSeverity ) ->
	set_debug_context_reporting( TargetSource, MessageType, MessageSeverity,
								 _DebugSelector=disable ).


% @doc Specifies how the reporting of debug messages shall be done in a debug
% context, by adding a filter based on the specified message source, type and
% severity.
%
% Although debug messages may be enabled in a non-debug context, the quantity
% and detail of such messages may be substantially inferior to those in a debug
% context. In particular, a valid implementation of the debug message queue in a
% non-debug context may produce no messages at all.
%
-spec set_debug_context_reporting( debug_source(), debug_type(),
			debug_severity(), debug_selector() ) -> void().
set_debug_context_reporting( TargetSource, MessageType, MessageSeverity,
							 DebugSelector ) ->

	GLTargetSource =
		gui_opengl_generated:get_second_for_debug_source( TargetSource ),

	GLMessageType =
		gui_opengl_generated:get_second_for_debug_type( MessageType ),

	GLMessageSeverity =
		gui_opengl_generated:get_second_for_debug_severity( MessageSeverity ),

	IsEnabled = case DebugSelector of

		enable ->
			?GL_TRUE;

		disable ->
			?GL_FALSE

	end,

	% Supposedly meaning all message identifiers (like NULL):
	CtrlIds = [],

	gl:debugMessageControl( GLTargetSource, GLMessageType, GLMessageSeverity,
		CtrlIds, IsEnabled ),

	cond_utils:if_defined( myriad_check_opengl, check_error() ).



% @doc Inserts the specified message in the OpenGL debug context.
%
% User-specified messages shall be, in terms of source, either 'application' or
% 'third_party'.
%
-spec insert_debug_context_message( debug_message_id(), ustring(),
	actual_debug_severity(), actual_debug_source(), actual_debug_type() ) ->
			void().
insert_debug_context_message( MsgId, Msg, MsgSeverity, MsgSource, MsgType ) ->

	GLMsgSeverity =
		gui_opengl_generated:get_second_for_debug_severity( MsgSeverity ),

	GLMsgSource = gui_opengl_generated:get_second_for_debug_source( MsgSource ),

	GLMsgType = gui_opengl_generated:get_second_for_debug_type( MsgType ),

	gl:debugMessageInsert( GLMsgSource, GLMsgType, MsgId, GLMsgSeverity, Msg ),

	% Only pure GL checked, not its debug context as of course it has a message
	% now:
	%
	cond_utils:if_defined( myriad_check_opengl, check_gl_error() ).



% @doc Fetches and removes from the OpenGL debug context all messages found, of
% any source, type and severity.
%
-spec get_debug_context_messages() -> [ debug_context_message() ].
get_debug_context_messages() ->
	get_debug_context_messages( _MsgSource=all, _MsgType=all,
								_MsgSeverity=all ).



% @doc Fetches and removes from the OpenGL debug context all messages found of
% the specified source, type and severity.
%
% To sort the returned messages by application identifiers, just use
% `lists:sort(_Index=1, DbcContextMsgs)'.
%
-spec get_debug_context_messages( debug_source(), debug_type(),
		debug_severity() ) -> [ debug_context_message() ].
get_debug_context_messages( MsgSource, MsgType, MsgSeverity ) ->

	GLMsgSource =
		gui_opengl_generated:get_second_for_debug_source( MsgSource ),

	GLMsgType = gui_opengl_generated:get_second_for_debug_type( MsgType ),

	GLMsgSeverity =
		gui_opengl_generated:get_second_for_debug_severity( MsgSeverity ),


	BufferByteCount = 5000,

	% Actually not created by the caller:
	%Buffer = bin_utils:create_binary( ByteCount ),

	MaxMsgCount = 10,

	fetch_debug_context_messages( BufferByteCount, MaxMsgCount,
		GLMsgSource, GLMsgType, GLMsgSeverity, _Acc=[] ).


% (helper)
fetch_debug_context_messages( BufferByteCount, MaxMsgCount, GLMsgSource,
							  GLMsgType, GLMsgSeverity, Acc ) ->

	{ FetchCount, GLSources, GLTypes, Ids, Severities, MessageLogs } =
		gl:getDebugMessageLog( MaxMsgCount, BufferByteCount ),

	% Not done, otherwise infinite recursion:
	%cond_utils:if_defined( myriad_check_opengl, check_error() ),
	cond_utils:if_defined( myriad_check_opengl, check_gl_error() ),

	% Testing the 'gl' implementation actually:
	cond_utils:if_defined( myriad_check_opengl,
		list_utils:check_equal( [ FetchCount, length( GLSources ),
			length( GLTypes ), length( Ids ), length( Severities ),
			length( MessageLogs ) ] ),
		basic_utils:ignore_unused( FetchCount ) ),

	case debug_context_messages( GLSources, GLTypes, Ids, Severities,
								 MessageLogs, _AccMsg=[] ) of

		[] ->
			Acc;

		CtxtMsgs ->
			fetch_debug_context_messages( BufferByteCount, MaxMsgCount,
				GLMsgSource, GLMsgType, GLMsgSeverity, CtxtMsgs ++ Acc )

	end.



% (helper)
debug_context_messages( _GLSources=[], _GLTypes=[], _Ids=[], _Severities=[],
						_MessageLogs=[], AccMsg ) ->
	% Preferring chronological insertion order:
	lists:reverse( AccMsg );

debug_context_messages( _GLSources=[ HSrc | TSrc ], _GLTypes=[ HTyp | TTyp ],
		_Ids=[ HId | TId ], _Severities=[ HSev | TSev ],
		_MessageLogs=[ Msg | T ], AccMsg ) ->

	BinMsg = text_utils:string_to_binary( Msg ),

	% First is MyriadGUI version, second is GL one:
	Src = gui_opengl_generated:get_first_for_debug_source( HSrc ),
	Type = gui_opengl_generated:get_first_for_debug_type( HTyp ),
	Sev = gui_opengl_generated:get_first_for_debug_severity( HSev ),

	FullMsg = { HId, BinMsg, Sev, Src, Type },

	debug_context_messages( TSrc, TTyp, TId, TSev, T, [ FullMsg | AccMsg ] ).



% @doc Returns a textual description of the specified debug context message.
-spec debug_context_message_to_string( debug_context_message() ) -> ustring().
debug_context_message_to_string(
		{ MsgId, Msg, MsgSeverity, MsgSource, MsgType } ) ->
	text_utils:format( "[~ts][~ts][~ts][~B] ~ts",
					   [ MsgSource, MsgType, MsgSeverity, MsgId, Msg ] ).


% @doc Returns a textual description of the specified debug context messages.
-spec debug_context_messages_to_string( [ debug_context_message() ] ) ->
												ustring().
debug_context_messages_to_string( Msgs ) ->
	text_utils:strings_to_string(
		[ debug_context_message_to_string( M ) || M <- Msgs ] ).



% @doc Checks the OpenGL requirements of this program against the local support,
% regarding the minimum OpenGL version; displays an error message and throws an
% exception if this requirement is not met.
%
-spec check_requirements( two_or_three_digit_version() ) -> void().
check_requirements( MinOpenGLVersion ) ->
	check_requirements( MinOpenGLVersion, _RequiredProfile=core ).



% @doc Checks the OpenGL requirements of this program against the local support,
% regarding the minimum OpenGL version and the specified profile; displays an
% error message and throws an exception if a requirement is not met.
%
-spec check_requirements( two_or_three_digit_version(), gl_profile() ) ->
															void().
check_requirements( MinOpenGLVersion, RequiredProfile ) ->
	check_requirements( MinOpenGLVersion, RequiredProfile,
						_RequiredExtensions=[] ).



% @doc Checks the OpenGL requirements of this program against the local support,
% regarding the minimum OpenGL version, the specified profile and extensions;
% displays an error message and throws an exception if a requirement is not met.
%
-spec check_requirements( two_or_three_digit_version(), gl_profile(),
						  [ gl_extension() ] ) -> void().
check_requirements( MinOpenGLVersion, RequiredProfile, RequiredExtensions ) ->

	Tid = secure_info_table(),

	is_version_compatible_with( MinOpenGLVersion, Tid ) orelse
		begin

			LocalVersion = get_version( Tid ),

			trace_utils:error_fmt( "The local OpenGL version, ~ts, is not "
				"compatible with the necessary one, ~ts. Drivers may have "
				"to be updated.",
				[ text_utils:version_to_string( LocalVersion ),
				  text_utils:version_to_string( MinOpenGLVersion ) ] ),

			throw( { incompatible_opengl_version, LocalVersion,
					 MinOpenGLVersion } )

		end,


	get_supported_profile() =:= RequiredProfile orelse
		begin

			trace_utils:error_fmt( "The local OpenGL driver does not support "
				"the '~ts' profile. Drivers may have to be updated.",
				[ RequiredProfile ] ),

			throw( { unsupported_opengl_profile, RequiredProfile } )

		end,


	case get_unsupported_extensions( RequiredExtensions, Tid ) of

		[] ->
			ok;

		[ LackingExt ] ->
			trace_utils:error_fmt( "An OpenGL extension, '~ts', is lacking. "
				"Drivers may have to be updated.", [ LackingExt ] ),
			throw( { unsupported_opengl_extensions, [ LackingExt ] } );

		LackingExts ->
			trace_utils:error_fmt( "~B OpenGL extensions are not found "
				"available: ~ts~nDrivers may have to be updated.",
				[ length( LackingExts ),
				  text_utils:atoms_to_string( LackingExts ) ] ),
			throw( { unsupported_opengl_extensions, LackingExts } )

	end.



% @doc Returns the size of a component the specified GL type, once it is
% serialised (typically in a buffer - and regardless of the Erlang datatypes).
%
-spec get_component_size( gl_base_type() ) -> byte_size().
get_component_size( _GLType=?GL_BYTE ) ->
	1;

get_component_size( _GLType=?GL_UNSIGNED_BYTE ) ->
	1;

get_component_size( _GLType=?GL_UNSIGNED_SHORT ) ->
	2;

get_component_size( _GLType=?GL_SHORT ) ->
	2;

get_component_size( _GLType=?GL_UNSIGNED_INT ) ->
	4;

get_component_size( _GLType=?GL_INT ) ->
	4;

get_component_size( _GLType=?GL_FLOAT ) ->
	4;

get_component_size( _GLType=?GL_DOUBLE ) ->
	8.



% @doc Tells whether OpenGL hardware acceleration is available on this host,
% based on the glxinfo executable.
%
-spec is_hardware_accelerated() -> boolean().
is_hardware_accelerated() ->

	case get_glxinfo_strings() of

		undefined ->
			trace_utils:warning( "No glxinfo status obtained, supposing that "
				"no OpenGL hardware acceleration is available." ),
			false;

		GlxinfoStrs ->
			is_hardware_accelerated( GlxinfoStrs )

	end.



% @doc Tells whether OpenGL hardware acceleration is available on this host,
% based on the specified glxinfo report.
%
-spec is_hardware_accelerated( glxinfo_report() ) -> boolean().
is_hardware_accelerated( GlxinfoStrs ) ->

	% For example "direct rendering: Yes"
	case list_utils:get_element_at( GlxinfoStrs, _Index=3 ) of

		"direct rendering: " ++ Next ->
			case Next of

				"Yes" ->
					true;

				"No" ->
					false

			end;

		OtherAnswer ->
			trace_utils:warning_fmt( "Unexpected status ('~ts') for "
				"direct rendering, supposing that no OpenGL hardware "
				"acceleration is available.", [ OtherAnswer ] ),
			false

	end.



% @doc Returns the list of strings (if any) returned by glxinfo when requesting
% basic information.
%
% Of course the 'glxinfo' executable must be available on the PATH (install it
% on Arch Linux with 'pacman -S mesa-utils').
%
-spec get_glxinfo_strings() -> maybe( glxinfo_report() ).
get_glxinfo_strings() ->

	% Best diagnosis we know:
	Tool = "glxinfo",

	case executable_utils:lookup_executable( Tool ) of

		false ->
			trace_utils:warning_fmt( "No '~ts' tool found, "
									 "no OpenGL status reported.", [ Tool ] ),
			undefined;

		ExecPath ->
			% -B: brief output, print only the basics.
			case system_utils:run_executable( ExecPath, [ "-B" ] ) of

				{ _ReturnCode=0, ReturnedStr } ->
					text_utils:remove_empty_lines(
						text_utils:split( ReturnedStr, "\n" ) );

				{ ErrorCode, ReturnedStr } ->
					trace_utils:error_fmt( "The ~ts query returned an error "
						"(code: ~B, message: '~ts'), no status reported.",
						[ Tool, ErrorCode, ReturnedStr ] ),
					undefined

			end

	end.



% @doc Returns a list of the default attributes for the creation of OpenGL
% canvases.
%
% To be used with create_canvas/*.
%
-spec get_default_canvas_attributes() -> [ device_context_attribute() ].
get_default_canvas_attributes() ->
	[ rgba, double_buffer, { min_red_size, 8 }, { min_green_size, 8 },
	  { min_blue_size, 8 }, { depth_buffer_size, 24 } ].



% @doc Creates and returns an OpenGL canvas with the specified parent widget and
% default settings: RGBA and double-buffering.
%
% Note: not to be mixed up with gui:create_canvas/1, which creates a basic
% (non-OpenGL) canvas.
%
-spec create_canvas( window() ) -> gl_canvas().
create_canvas( Parent ) ->
	% Might be added: the {style, full_repaint_on_resize} option.
	create_canvas( Parent,
		_Opts=[ { gl_attributes, get_default_canvas_attributes() } ] ).


% @doc Creates and returns an OpenGL canvas with the specified parent widget and
% options.
%
% If the device context attributes are not set (i.e. no gl_attributes entry),
% following defaults apply: RGBA and double-buffering.
%
% Note: not to be mixed up with gui:create_canvas/1, which creates a basic
% (non-OpenGL) canvas.
%
% Note also that using the use_core_profile attribute will result in also
% requesting OpenGL at least version 3.0.
%
-spec create_canvas( window(), [ gl_canvas_option() ] ) -> gl_canvas().
create_canvas( Parent, Opts ) ->

	cond_utils:if_defined( myriad_debug_opengl,
		trace_utils:debug_fmt( "Creating a GL canvas from user options:~n ~p.",
							   [ Opts ] ) ),

	% Not using list_table:extract_entry_with_default/3, as Opts may contain
	% single atoms:
	%
	{ Attrs, OtherOpts } = list_utils:extract_pair_with_default(
		_K=gl_attributes, _Def=[ rgba, double_buffer ], Opts ),

	%trace_utils:debug_fmt( "Creating a GL canvas from options:~n ~p,~n "
	%   "hence with Attrs = ~p~n and OtherOpts = ~p.",
	%   [ Opts, Attrs, OtherOpts ] ),

	WxAttrs = gui_wx_backend:to_wx_device_context_attributes( Attrs ),

	OtherWxOpts = gui_window:to_wx_window_options( OtherOpts ),

	WxOpts = [ { attribList, WxAttrs } | OtherWxOpts ],

	%trace_utils:debug_fmt( "WxOpts = ~p", [ WxOpts ] ),

	% Using newer wxGL API (of arity 2, not 3); no error case to handle:
	Res = wxGLCanvas:new( Parent, WxOpts ),

	% Commented-out, as not relevant (an OpenGL context may not already exist at
	% this point):
	%
	%cond_utils:if_defined( myriad_check_opengl, check_error() ),

	Res.



% @doc Returns the OpenGL context obtained from the specified OpenGL canvas; it
% is created but not bound yet (not set as current, hence not usable yet, no
% OpenGL command can be issued yet).
%
-spec create_context( gl_canvas() ) -> gl_context().
create_context( Canvas ) ->

	% No specific, relevant option applies, like to create first an OpenGL 2.1
	% context, possibly enable a GLEW-like service, and use an extension for the
	% creation of an OpenGL 3.x context like with
	% WGL_CONTEXT_{MAJOR,MINOR}_VERSION_ARB,
	% WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB and WGL_ARB_create_context:
	%
	% (nevertheless we still have OpenGL 4.6.0 when querying it, see
	% get_version/1)
	%
	Res = wxGLContext:new( Canvas ),

	% Commented-out, as the OpenGL context is not set as current yet:
	%cond_utils:if_defined( myriad_check_opengl, check_error() ),

	Res.



% @doc Sets the specified (OpenGL) context to the specified (OpenGL) canvas once
% its window is shown, so that it applies to the next operations (OpenGL calls)
% made on it.
%
% To be only called when the parent window is shown on screen; see
% gui_opengl_test.erl for an example thereof.
%
-spec set_context_on_shown( gl_canvas(), gl_context() ) -> void().
set_context_on_shown( Canvas, Context ) ->

	% According to Wings3D (wings_gl.erl), wxGLCanvas:setCurrent/2 may fail (on
	% GTK) as the 'show' event may be received before the window is actually
	% displayed; so:
	%
	timer:sleep( 250 ),

	set_context( Canvas, Context ).



% @doc Sets the specified (OpenGL) context to the specified (OpenGL) canvas, so
% that it applies to the next operations (OpenGL calls) made on it.
%
-spec set_context( gl_canvas(), gl_context() ) -> void().
set_context( Canvas, Context ) ->

	% Using wx API 3.0 (not supporting older ones such as 2.8):
	wxGLCanvas:setCurrent( Canvas, Context ) orelse
		throw( failed_to_set_opengl_context ),

	cond_utils:if_defined( myriad_check_opengl, check_error() ).



% @doc Swaps the double-buffer of the corresponding OpenGL canvas (making the
% back-buffer the front-buffer and vice versa), so that the output of the
% previous OpenGL commands is displayed on this window.
%
% The corresponding window must already be shown.
%
% Includes a gl:flush/0.
%
-spec swap_buffers( gl_canvas() ) -> void().
swap_buffers( Canvas ) ->

	% wxGLCanvas:swapBuffers/1 may or may not include any kind of gl:flush/0; so
	% it is preferable to trigger one by ourselves.

	% Ensures that the drawing commands are actually directly triggered
	% (i.e. started, not necessarily completed; use gl:finish/0 to make this
	% operation synchronous, i.e. to wait for its end) rather than stored in a
	% buffer awaiting additional OpenGL commands:
	%
	gl:flush(),
	% More expensive, as blocks: gl:finish(),

	wxGLCanvas:swapBuffers( Canvas ) orelse throw( failed_to_swap_buffers ),

	cond_utils:if_defined( myriad_check_opengl, check_error() ).



% @doc Set the polygon rasterization mode.
-spec set_polygon_raster_mode( polygon_facing_mode(),
							   rasterization_mode() ) -> void().
set_polygon_raster_mode( FacingMode, RasterMode ) ->
	gl:polygonMode( FacingMode, RasterMode ),
	cond_utils:if_defined( myriad_check_opengl, check_error() ).



% @doc Renders the specified mesh in a supposedly appropriate OpenGL context.
%
% See gui_opengl_test.erl for an usage example.
%
-spec render_mesh( mesh() ) -> void().
render_mesh( #mesh{ vertices=Vertices,
					face_type=FaceType,
					faces=IndexedFaces,
					normal_type=per_face,
					normals=Normals,
					rendering_info={ color, per_vertex, Colors } } ) ->

	% We could batch the commands sent to the GUI backend (e.g. with wx:batch/1
	% or wx:foreach/2).

	cond_utils:if_defined( myriad_check_opengl, check_error() ),

	render_faces( FaceType, IndexedFaces, Vertices, Normals, Colors ).





% @doc Enters in 2D mode for the specified window (typically an OpenGL canvas):
% applies relevant general state changes, and specific to modelview (which is
% reset) and to projection (a projection matrix relevant for 2D operations is
% applied).
%
% Refer to https://myriad.esperide.org/#2d-referential for more details.
%
-spec enter_2d_mode( window() ) -> void().
enter_2d_mode( Window ) ->

	% Directly deriving from lib/wx/examples/demo/ex_gl.erl:

	{ Width, Height } = wxWindow:getClientSize( Window ),

	% General state changes; depending on the current OpenGL state, other
	% elements may have to be updated:

	gl:pushAttrib( ?GL_ENABLE_BIT ),
	%cond_utils:if_defined( myriad_check_opengl, check_error() ),

	gl:disable( ?GL_DEPTH_TEST ),
	gl:disable( ?GL_CULL_FACE ),
	gl:enable( ?GL_TEXTURE_2D ),

	%cond_utils:if_defined( myriad_check_opengl, check_error() ),

	% This allows the alpha blending of 2D textures with the scene:
	gl:enable( ?GL_BLEND ),
	gl:blendFunc( ?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA ),

	% Updating first the projection matrix for 2D:

	gl:matrixMode( ?GL_PROJECTION ),
	gl:pushMatrix(),
	gl:loadIdentity(),

	%cond_utils:if_defined( myriad_check_opengl, check_error() ),

	% In all MyriadGUI referentials mentioned, abscissas are to increase when
	% going from left to right.
	%
	% As for ordinates, with the Myriad 2D referential (refer to the 'Geometric
	% Conventions' in Myriad's technical manual), like for the backend
	% coordinates (e.g. SDL, wxWidgets), they are to increase when going from
	% top to bottom.
	%
	% It is the opposite by default with OpenGL (increasing from bottom to top;
	% the elements would therefore be upside-down in the OpenGL world), so in
	% the next orthogonal projection bottom and top coordinates used to be
	% mirrored; then OpenGL complied with the previous convention.
	%
	% Doing so may be more relevant than flipping the textures/images
	% themselves, as the projection also applies to mouse coordinates.
	%
	% Yet now we prefer directly flipping vertically (upside-down) the textures
	% at creation.

	% Multiplies the projection matrix with this orthographic one, assuming that
	% the eye is located at (0, 0, 0); implements the MyriadGUI 2D conventions,
	% with pixel-level coordinates (another option could have been to rely on
	% normalised, definition-independent coordinates, ranging in [0.0, 1.0]):
	%
	% (corresponds to glu:ortho2D/4)
	%
	%gl:ortho( _Left=0.0, _Right=float( Width ), _Bottom=float( Height ),
	%          _Top=0.0, _Near=-1.0, _Far=1.0 ),
	gl:ortho( _Left=0.0, _Right=float( Width ), _Bottom=0.0,
			  _Top=float( Height ), _Near=-1.0, _Far=1.0 ),

	% Then reseting the modelview matrix:
	gl:matrixMode( ?GL_MODELVIEW ),
	gl:pushMatrix(),
	gl:loadIdentity(),

	cond_utils:if_defined( myriad_check_opengl, check_error() ).



% @doc Leaves the 2D mode, resets the modelview and projection matricesn and the
% GL attributes.
%
-spec leave_2d_mode() -> void().
leave_2d_mode() ->
	gl:matrixMode( ?GL_MODELVIEW ),
	gl:popMatrix(),

	gl:matrixMode( ?GL_PROJECTION ),
	gl:popMatrix(),

	gl:popAttrib(),

	cond_utils:if_defined( myriad_check_opengl, check_error() ).




% @doc Replaces the current OpenGL matrix (top of the currently selected stack,
% see matrix_stack/0) with the specified matrix4 instance.
%
-spec set_matrix( matrix4() ) -> void().
set_matrix( M=identity_4  )  ->
	set_matrix( matrix4:to_canonical( M ) );

% Works both for canonical_matrix4 and compact_matrix4:
set_matrix( M ) ->

	% Removes the record tag, resulting in a tuple of 16 or 12 coordinates
	% respectively:
	%
	ShrunkTuple = erlang:delete_element( _Index=1, M ),

	% 'f' float suffix, not 'd' by definition of matrix4.
	%
	% Transpose, as gl:loadMatrixf expects column-major order, not row-major
	% one:
	%
	gl:loadTransposeMatrixf( ShrunkTuple ).



% @doc Returns the current OpenGL matrix at the top of the specified stack, as a
% matrix4 instance.
%
-spec get_matrix( matrix_stack() ) -> matrix4().
get_matrix( _Stack ) ->
	% Not relevant: gl:getDoublev( Stack )
	% and gl:get/1 not available through NIF.
	% gl:get( Stack ).
	throw( not_implemented ).



% Conversion helpers, from MyriadGUI to OpenGL.


% @doc Converts a MyriadGUI hint about buffer usage to OpenGL conventions.
-spec boolean_to_gl( boolean() ) -> gl_boolean().
boolean_to_gl( true ) ->
	?GL_TRUE;

boolean_to_gl( false ) ->
	?GL_FALSE.



% @doc Converts a MyriadGUI hint about buffer usage to OpenGL conventions.
%
% See https://registry.khronos.org/OpenGL-Refpages/gl4/html/glBufferData.xhtml.
-spec buffer_usage_hint_to_gl( buffer_usage_hint()  ) -> enum().
buffer_usage_hint_to_gl( _UsageHint={ _Usage=draw, _Access=stream } ) ->
	% In gl.hrl:
	?GL_STREAM_DRAW;

buffer_usage_hint_to_gl( _UsageHint={ _Usage=read, _Access=stream } ) ->
	?GL_STREAM_READ;

buffer_usage_hint_to_gl( _UsageHint={ _Usage=copy, _Access=stream } ) ->
	?GL_STREAM_COPY;


buffer_usage_hint_to_gl( _UsageHint={ _Usage=draw, _Access=static } ) ->
	% In gl.hrl:
	?GL_STATIC_DRAW;

buffer_usage_hint_to_gl( _UsageHint={ _Usage=read, _Access=static } ) ->
	?GL_STATIC_READ;

buffer_usage_hint_to_gl( _UsageHint={ _Usage=copy, _Access=static } ) ->
	?GL_STATIC_COPY;


buffer_usage_hint_to_gl( _UsageHint={ _Usage=draw, _Access=dynamic } ) ->
	% In gl.hrl:
	?GL_DYNAMIC_DRAW;

buffer_usage_hint_to_gl( _UsageHint={ _Usage=read, _Access=dynamic } ) ->
	?GL_DYNAMIC_READ;

buffer_usage_hint_to_gl( _UsageHint={ _Usage=copy, _Access=dynamic } ) ->
	?GL_DYNAMIC_COPY.




% @doc Renders the specified indexed faces.
-spec render_faces( face_type(), [ indexed_face() ], [ any_vertex3() ],
					[ unit_normal3() ], [ render_rgb_color() ] ) -> void().
render_faces( _FaceType=triangle, IndexedFaces, Vertices, Normals,
			  Colors ) ->
	gl:'begin'( ?GL_TRIANGLES ),
	render_triangles( IndexedFaces, _FaceCount=1, Vertices, Normals, Colors ),
	gl:'end'(),
	cond_utils:if_defined( myriad_check_opengl, check_error() );

render_faces( _FaceType=quad, IndexedFaces, Vertices, Normals, Colors ) ->
	gl:'begin'( ?GL_QUADS ),
	render_quads( IndexedFaces, _FaceCount=1, Vertices, Normals, Colors ),
	gl:'end'(),
	cond_utils:if_defined( myriad_check_opengl, check_error() ).



% (helper)
render_triangles( _IndexedFaces=[], _FaceCount, _Vertices, _Normals,
				  _Colors ) ->
	ok;

render_triangles( _IndexedFaces=[ [ V1Idx, V2Idx, V3Idx ] | T ], FaceCount,
				  Vertices, Normals, Colors ) ->

	gl:normal3fv( list_to_tuple( lists:nth( FaceCount, Normals ) ) ),

	gl:color3fv( lists:nth( V1Idx, Colors ) ),
	gl:texCoord2f( 0.0, 0.0 ),
	gl:vertex3fv( lists:nth( V1Idx, Vertices ) ),

	gl:color3fv( lists:nth( V2Idx, Colors ) ),
	gl:texCoord2f( 1.0, 0.0 ),
	gl:vertex3fv( lists:nth( V2Idx, Vertices ) ),

	gl:color3fv( lists:nth( V3Idx, Colors ) ),
	gl:texCoord2f( 1.0, 1.0 ),
	gl:vertex3fv( lists:nth( V3Idx, Vertices ) ),

	render_triangles( T, FaceCount+1, Vertices, Normals, Colors ).



% (helper)
render_quads( _IndexedFaces=[], _FaceCount, _Vertices, _Normals, _Colors ) ->
	ok;

render_quads( _IndexedFaces=[ [ V1Idx, V2Idx, V3Idx, V4Idx ] | T ], FaceCount,
			  Vertices, Normals, Colors ) ->

	gl:normal3fv( list_to_tuple( lists:nth( FaceCount, Normals ) ) ),

	gl:color3fv( lists:nth( V1Idx, Colors ) ),
	gl:texCoord2f( 0.0, 0.0 ),
	gl:vertex3fv( lists:nth( V1Idx, Vertices ) ),

	gl:color3fv( lists:nth( V2Idx, Colors ) ),
	gl:texCoord2f( 1.0, 0.0 ),
	gl:vertex3fv( lists:nth( V2Idx, Vertices ) ),

	gl:color3fv( lists:nth( V3Idx, Colors ) ),
	gl:texCoord2f( 1.0, 1.0 ),
	gl:vertex3fv( lists:nth( V3Idx, Vertices ) ),

	gl:color3fv( lists:nth( V4Idx, Colors ) ),
	gl:texCoord2f( 0.0, 1.0 ),
	gl:vertex3fv( lists:nth( V4Idx, Vertices ) ),

	render_quads( T, FaceCount+1, Vertices, Normals, Colors ).





% Error management section.


% Useless: each call to gl:getError/0 resets it to ?GL_NO_ERROR.
%% reset_error() ->
%%   gl:getError().


% @doc Checks whether an OpenGL-related error occurred previously (since last
% check, otherwise since OpenGL initialisation); if yes, displays information
% regarding it, and throws an exception.
%
% Note that an OpenGL context must already exist and be set as current (see
% set_context*/2), otherwise a no_gl_context error will be triggered.
%
-spec check_error() -> void().
check_error() ->
	check_error( _DoThrowOnError=true ).


% @doc Checks whether an OpenGL-related error occurred previously (since last
% check, otherwise since OpenGL initialisation); if yes, displays information
% regarding it, and throws an exception if requested: DoThrowOnError tells
% whether the detection of an OpenGL shall throw an exception or only output an
% error message in the console.
%
% Note that an OpenGL context must already exist and be set as current (see
% set_context*/2), otherwise a no_gl_context error will be triggered.
%
-spec check_error( boolean() ) -> void().
check_error( DoThrowOnError ) ->
	check_gl_error( DoThrowOnError ),
	check_gl_debug_context_error( DoThrowOnError ).


% (helper)
-spec check_gl_error() -> void().
check_gl_error() ->
	check_gl_error( _DoThrowOnError=true ).


-spec check_gl_error( boolean() ) -> void().
check_gl_error( DoThrowOnError ) ->

	%DoTrace = true,
	DoTrace = false,

	DoTrace andalso
		begin

			{ TMod, TFunc, TArity, [ { file, TSrcFile },
									 { line, TLine } ] } =
				hd( code_utils:get_stacktrace( _SkipLastElemCount=3 ) ),

			trace_utils:debug_fmt( "Check in ~ts:~ts/~B (file ~ts, "
				"line ~B)", [ TMod, TFunc, TArity, TSrcFile, TLine ] )

		end,

	% Reset the error status when returning:
	case gl:getError() of

		?GL_NO_ERROR ->
			ok;

		GlError ->
			Diagnosis = interpret_error( GlError ),
			% Stacktrace expected, as bound to be useful (even if the error
			% might have happened some time before, any time between the
			% previous check and this one):
			%
			SkipLastElemCount = 2,

			case DoThrowOnError of

				true ->
					{ Mod, Func, Arity, [ { file, SrcFile },
										  { line, Line } ] } =
						hd( code_utils:get_stacktrace( SkipLastElemCount ) ),

					trace_utils:error_fmt( "OpenGL error reported (~B): ~ts; "
						"this error was detected in ~ts:~ts/~B (file ~ts, "
						"line ~B); aborting.",
						[ GlError, Diagnosis, Mod, Func, Arity, SrcFile,
						  Line ] ),

					throw( { opengl_error, GlError, Diagnosis } );

				false ->
					trace_utils:error_fmt( "OpenGL error detected (~B): ~ts; "
						"stacktrace:~n  ~p.", [ GlError, Diagnosis,
							code_utils:get_stacktrace( SkipLastElemCount ) ] ),

					% Recursing until having ?GL_NO_ERROR, knowing that, when
					% OpenGL is run in distributed mode (like frequently found
					% on X11 systems), calling gl:getError/0 only resets one of
					% the error code flags (instead of all of them):
					%
					check_gl_error( DoThrowOnError )

			end

	end.


% (helper)
-spec check_gl_debug_context_error() -> void().
check_gl_debug_context_error() ->
	check_gl_debug_context_error( _DoThrowOnError=true ).


% (helper)
-spec check_gl_debug_context_error( boolean() ) -> void().
check_gl_debug_context_error( DoThrowOnError ) ->

	% There might still be messages even if the debug context is not
	% specifically enabled:
	%
	case get_debug_context_messages() of

		[] ->
			ok;

		Msgs ->
			% Similar as for gl:getError/0:
			SkipLastElemCount = 2,

			case DoThrowOnError of

				true ->
					{ Mod, Func, Arity, [ { file, SrcFile },
										  { line, Line } ] } =
						hd( code_utils:get_stacktrace( SkipLastElemCount ) ),

					trace_utils:error_fmt( "~B OpenGL error(s) reported "
						"through its debug context: ~ts~n"
						"Detected in ~ts:~ts/~B (file ~ts, "
						"line ~B); aborting.",
						[ length( Msgs ),
						  debug_context_messages_to_string( Msgs ),
						  Mod, Func, Arity, SrcFile, Line ] ),

					throw( { opengl_error_from_debug_context, Msgs } );

				false ->
					trace_utils:error_fmt( "~B OpenGL error(s) detected "
						"through its debug context: ~ts~nStacktrace:~n  ~p.",
						[ length( Msgs ),
						  debug_context_messages_to_string( Msgs ),
						  code_utils:get_stacktrace( SkipLastElemCount ) ] )

			end

	end.



% @doc Returns a (textual) diagnosis regarding the specified OpenGL-related
% (including GLU) error.
%
-spec interpret_error( any_error() ) -> ustring().
% Reference being
% https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetError.xhtml:
interpret_error( ?GL_INVALID_ENUM ) ->
	"invalid value specified for an enumerated argument (GL_INVALID_ENUM)";

interpret_error( ?GL_INVALID_VALUE ) ->
	"out-of-range numeric argument (GL_INVALID_VALUE)";

interpret_error( ?GL_INVALID_OPERATION ) ->
	"a specified operation is not allowed in the current state "
	"(GL_INVALID_OPERATION)";

interpret_error( ?GL_INVALID_FRAMEBUFFER_OPERATION ) ->
	"the framebuffer object is not complete (GL_INVALID_FRAMEBUFFER_OPERATION)";

interpret_error( ?GL_OUT_OF_MEMORY ) ->
	"there is not enough memory left to execute the command (GL_OUT_OF_MEMORY)";

interpret_error( ?GL_STACK_UNDERFLOW ) ->
	"an attempt has been made to perform an operation that would cause "
	"an internal stack to underflow (GL_STACK_UNDERFLOW)";

interpret_error( ?GL_STACK_OVERFLOW ) ->
	"an attempt has been made to perform an operation that would cause "
	"an internal stack to overflow (GL_STACK_OVERFLOW)";

interpret_error( ?GL_NO_ERROR ) ->
	"no OpenGL error reported (GL_NO_ERROR)";

interpret_error( OtherCode ) ->
	text_utils:format( "OpenGL-related error of code ~B, interpreted as '~ts'.",
		[ OtherCode, glu:errorString( OtherCode ) ] ).



% Section for the build-time generation of support modules.


% @doc To be called by the 'gui_opengl_generated.beam' automatic make target in
% order to generate, here, a (single) module to share the MyriadGUI OpenGL
% constants.
%
-spec generate_support_modules() -> no_return().
generate_support_modules() ->

	TargetModName = gui_opengl_generated,

	%trace_bridge:info_fmt( "Generating module '~ts'...", [ TargetModName ] ),

	TopicSpecs = [ gui_opengl_constants:F()
					|| F <- gui_opengl_constants:list_topic_spec_functions() ],

	_ModFilename =
		const_bijective_topics:generate_in_file( TargetModName, TopicSpecs ),

	%trace_bridge:info_fmt( "File '~ts' generated.", [ ModFilename ] ),

	erlang:halt().
