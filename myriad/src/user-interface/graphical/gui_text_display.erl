% Copyright (C) 2010-2025 Olivier Boudeville
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
% Creation date: Monday, February 15, 2010.

-module(gui_text_display).

-moduledoc """
Gathering of various facilities for **text display**.

See also the corresponding gui_dialog:message_dialog() dialog.
""".


-doc """
A widget displaying a text; a text display behaves like a panel dedicated to the
rendering of a text.

The current font applies to this display.
""".
-opaque text_display() :: wxStaticText:wxStaticText().



-doc "An option for the creation of a text display.".
-type text_display_option() ::
	{ 'position', point() }
  | { 'size', size() }
  | { 'style', [ text_display_style() ] }.



-doc """
A style element of a text display.

See also <http://docs.wxwidgets.org/stable/classwx_static_text.html>.
""".
-type text_display_style() ::
	'align_left'   % Align the text to the left.
  | 'align_right'  % Align the text to the right.
  | 'center'       % Center the text (horizontally).
  | 'fixed_size'   % No auto-resize.
  | 'ellipsize_begin'     % Any shrinking done from the start of the text.
  | 'ellipsize_middle'    % Any shrinking done at the middle of the text.
  | 'ellipsize_end'.      % Any shrinking done at the end of the text.


-export_type([ text_display/0,
			   text_display_option/0, text_display_style/0 ]).


-export([ create/2, create_presized/3, create/3, create_presized/4,
		  create_with_id/3, create/4,
		  destruct/1 ]).



% For related defines:
-include("gui_base.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").



% Type shorthands:

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type label() :: gui:label().
-type parent() :: gui:parent().
-type point() :: gui:point().
-type size() :: gui:size().

-type id() :: gui_id:id().

-type font() :: gui_font:font().
-type precise_text_extent() :: gui_font:precise_text_extent().

-type wx_opt_pair() :: gui_wx_backend:wx_opt_pair().



-doc "Creates a text display, based on the specified label.".
-spec create( label(), parent() ) -> text_display().
create( Label, Parent ) ->
	create( Label, gui_id:get_any_id(), _Opts=[], Parent ).



-doc """
Creates a text display, based on the specified label and on a precomputed size,
determined thanks to the specified font, which is associated to it. Returns the
created display, together with its precise text extent.

This function may be useful as, in some cases, even the rendering of a
single-line label in a panel may be wrong, being cropped or extended for some
unknown reason (presumably a wxWidgets bug).
""".
-spec create_presized( label(), font(), parent() ) ->
			{ text_display(), precise_text_extent() }.
create_presized( Label, Font, Parent ) ->
	% See explanation in create_presized/4.
	PExtent = { W, H, _Descent, _ExtLeading } =
		gui_font:get_precise_text_extent( Label, Font ),

	LabelSize = { W, H },

	Display = create( Label, gui_id:get_any_id(),
					  _Opts=[ { size, LabelSize } ], Parent ),

	gui_widget:set_font( Display, Font ),
	{ Display, PExtent }.



-doc """
Creates a text display, based on the specified label and option(s).
""".
-spec create( label(), maybe_list( text_display_option() ), parent() ) ->
											text_display().
create( Label, Options, Parent ) ->
	create( Label, gui_id:get_any_id(), Options, Parent ).



-doc """
Creates a text display, based on the specified label, (non-size) option(s) and
its precise text extent, determined thanks to the specified font, which is
associated to it.

Note that the display height may be higher than the one of the actual text, due
to the margin taken for letters possibly going below the baseline (like 'g').

This function may be useful as, in some cases, even the rendering of a
single-line label in a panel may be wrong, being cropped or extended for some
unknown reason (presumably a wxWidgets bug).
""".
-spec create_presized( label(), maybe_list( text_display_option() ),
					   font(), parent() ) ->
				{ text_display(), precise_text_extent() }.
create_presized( Label, Options, Font, Parent ) ->

	PExtent = { W, H, _Descent, _ExtLeading } =
		gui_font:get_precise_text_extent( Label, Font ),

	LabelSize = { W, H },

	% One may test it with (at least in our setting, size is wrong - seems to
	% take into account the former font):
	%
	%FullOpts = list_utils:ensure_list( Options ),

	FullOpts = [ { size, LabelSize } | list_utils:ensure_list( Options ) ],

	%trace_utils:debug_fmt( "FullOpts = ~p for text static display of '~ts'.",
	%                       [ FullOpts, Label ] ),

	Display = create( Label, gui_id:get_any_id(), FullOpts, Parent ),

	gui_widget:set_font( Display, Font ),

	% The issue is that if no explicit size is set, they will still not match
	% even after fit/layout:
	%
	%trace_utils:debug_fmt( "Display size = ~p, text extent = ~p.",
	%   [ gui_widget:get_size( Display ),
	%     gui_font:get_text_extent( Label, Font ) ] ),

	{ Display, PExtent }.



-doc """
Creates a text display, based on the specified label and identifier.
""".
-spec create_with_id( label(), id(), parent() ) -> text_display().
create_with_id( Label, Id, Parent ) ->
	create( Label, Id, _Options=[], Parent ).



-doc """
Creates a text display, based on the specified label, identifier and option(s).
""".
-spec create( label(), id(), maybe_list( text_display_option() ),
			  parent() ) -> text_display().
create( Label, Id, Options, Parent ) ->
	WxOpts = to_wx_static_display_opts( Options ),

	%trace_utils:debug_fmt( "Options of text display '~ts': ~p.",
	%                       [ Label, WxOpts ] ),

	wxStaticText:new( Parent, Id, Label, WxOpts ).



-doc "Destructs the specified text display.".
-spec destruct( text_display() ) -> void().
destruct( TextDisplay ) ->
	wxStaticText:destroy( TextDisplay ).




% Helper section.


-doc """
Converts the specified text display option(s) into the appropriate backend
specific options.

(helper)
""".
-spec to_wx_static_display_opts( maybe_list( text_display_option() ) ) ->
											[ wx_opt_pair() ].
to_wx_static_display_opts( Options ) when is_list( Options )->
	[ to_wx_static_text_opt( O ) || O <- Options ];

% Probably a pair:
to_wx_static_display_opts( Opt ) ->
	to_wx_static_display_opts( [ Opt ] ).


% (helper)
-spec to_wx_static_text_opt( text_display_option() ) -> wx_opt_pair().
to_wx_static_text_opt( _Opt={ position, Pos } ) ->
	{ pos, Pos };

to_wx_static_text_opt( Opt={ size, _S } ) ->
	Opt;

to_wx_static_text_opt( _Opts={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		gui_generated:get_second_for_text_display_style( S ) bor Acc end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle }.
