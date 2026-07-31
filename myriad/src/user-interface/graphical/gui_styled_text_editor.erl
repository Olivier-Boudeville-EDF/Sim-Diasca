% Copyright (C) 2026-2026 Olivier Boudeville
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
% Creation date: Wednesday, March 25, 2026.

-module(gui_styled_text_editor).

-moduledoc """
Gathering of various facilities for **GUI-based more advanced, styled text
editing**, for example applying to source code.

A styled text editor is abbreviated as STE here.

Various styles can be defined (relatively to a given STE) and applied to
portions of the text that it holds.

It relies on a direct, bijective mapping of "raw" scintilla interface, whose
documentation can be found in the [Scintilla
website](http://www.scintilla.org/).

See also:
- the corresponding `gui_styled_text_editor_test` test module
- the `gui_text_editor` module for more basic text editing, often used as a
  reference here; note that all `gui_*text_editor` modules are designed to
  respect the same core message exchange patterns
""".



-doc """
A widget to edit a styled text (single line or multi-line).

One can subscribe to the following events that can be emitted by a styled text
editor:
- `onTextUpdated`, if its text has been modified
- `onEnterPressed`, if the Enter key is pressed whereas the text editor enabled
  its process_enter_key option
- `onTextOverflow`, if the length limit of the text in the editor is reached
""".
-opaque styled_text_editor() ::
    % Atom tagging convenient for event processing (see gui_event):
    { 'myriad_styled_text_editor', wxStyledTextCtrl:wxStyledTextCtrl(),
      style_table() }.


-doc "Shorthand for styled text editor.".
-type ste() :: styled_text_editor().


-doc "An option for the creation of a styled text editor.".
-type styled_text_editor_option() ::
    { 'position', point() }
  | { 'size', size() }
  | { 'style', [ text_editor_style() ] }.



-doc """
A style element specific to a text editor.

See also <https://docs.wxwidgets.org/stable/classwx_text_ctrl.html>.
""".
-type text_editor_style() :: gui_text_editor:text_editor_style().


-doc """
A type of event possibly emitted by a (styled) text editor, to which one can
subscribe.
""".
-type text_editor_event_type() :: gui_text_editor:text_editor_event_type().


-doc """
The name of a user-defined style.

A style of a styled text editor is defined by the user, corresponds to a set of
style options (like bold, underline, etc.), is designated by its name, as an
atom, and is relative to a styled text editor.

For example one's program may define the `my_warning_style` style of a given STE
as being bold and orange.
""".
-type style_name() :: atom().


-doc "An option that can be applied to define a given style.".
-type style_option() :: 'bold'
                      | 'underline'
                      | { 'foreground_color', any_color() }
                      | { 'background_color', any_color() }.


-doc """
An handle to a specified backend-level style.

No need to create them specifically, assigning them properties is sufficient.
""".
-type backend_style() :: wx_style().


% We consider that style 0 is the default one and shall not be modified.
-type wx_style() :: 0..255.


-doc "A table associating MyriadGUI styles and backend ones.".
-type style_table() ::
    bijective_table:bijective_table( style_name(), wx_style() ).



-export_type([ styled_text_editor/0, ste/0, styled_text_editor_option/0,
               text_editor_style/0, text_editor_event_type/0,
               style_name/0, style_option/0 ]).


-doc "The settings of a text range.".
-opaque range_settings() :: gui_text_editor:wxTextAttr().


-export_type([ range_settings/0 ]).



% Operations related to (styled) text editors:
-export([ create/1, create/2, destruct/1,

          % For styles:
          declare_style/2, declare_style/3,
          set_styling_at/3, set_underline/3,
          set_foreground_color/3, set_background_color/3

%          set_default_font/2, set_default_background_color/2,
%          set_text/2, add_text/2, clear/1,
%          show_position/2, show_text_end/1, get_last_position/1,
%          set_cursor_position/2, set_cursor_position_to_end/1,
%          get_cursor_position/1, offset_cursor_position/2,
%          set_from/2
]).


-compile( [ { nowarn_unused_function, [ {to_style_name,2} ] } ] ).

% For related defines:
-include("gui_base.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").



% Implementation section:
%
% Based on wxStyledTextCtrl (to rely on the more basic wxTextCtrl, use our
% gui_text_editor module).
%
% Note that we can override the management of keys by subscribing for example to
% onKeyPressed; see gui_shell for an example thereof.
%
% Based on the wxWidgets implementation of the Scintilla source code editing
% component.
%
% Contrary to the wx implementation, as we have atoms and thus a conversion
% table, styles have to be declared explicitly (otherwise any operation would
% have to be able to auto-declare a style, and thus would have to return a style
% table.



% Type shorthands:

-type maybe_list( T ) :: list_utils:maybe_list( T ).

%-type text() :: ui:text().


%-type text_edit() :: text_edit:text_edit().

% Starts at 1 (backend positions start at zero):
-type char_pos() :: text_edit:char_pos().

-type parent() :: gui:parent().
-type point() :: gui:point().
-type size() :: gui:size().


%-type font() :: gui_font:font().

% RGB(A):
-type any_color() :: gui_color:any_color().

%-type char_pos() :: text_edit:char_pos().
%-type offset_char_pos() :: text_edit:offset_char_pos().

-type wx_opt_pair() :: gui_wx_backend:wx_opt_pair().




-doc "Creates and shows a styled text editor.".
-spec create( parent() ) -> styled_text_editor().
create( Parent ) ->
    STECtrl = wxStyledTextCtrl:new( Parent, gui_id:get_any_id() ),
    { STECtrl, _StyleTable=bijective_table:new() }.



-doc """
Creates and shows a styled text editor, based on the specified option(s).
""".
-spec create( maybe_list( styled_text_editor_option() ), parent() ) ->
                                        styled_text_editor().
create( Option={ _K, _V }, Parent ) ->
    create( _Opts=[ Option ], Parent );

create( Options, Parent ) ->

    { MaybeText, OtherOpts } = list_table:extract_entry_with_default( _K=text,
        _DefaultValue=undefined, Options ),

    WxOpts = to_wx_editor_opts( OtherOpts ),

    % Not specifically adding {id, gui_id:get_any_id()}:
    STECtrl = wxStyledTextCtrl:new( Parent, WxOpts ),

    case MaybeText of

        undefined ->
            ok;

        Text ->
            wxStyledTextCtrl:addText( STECtrl, Text )

    end,

    { STECtrl, _StyleTable=bijective_table:new() }.



-doc "Destructs the specified styled text editor.".
-spec destruct( styled_text_editor() ) -> void().
destruct( _STE={ STECtrl, _StyleTable } ) ->
    wxStyledTextCtrl:destroy( STECtrl ).




% Style section.


-doc "Declares the specified style, with no specific settings".
-spec declare_style( ste(), style_name() ) -> ste().
declare_style( _STE={ STECtrl, StyleTable }, StyleName ) ->

    % Simpler than also storing a count:
    NewStyleTable = bijective_table:add_entry( _First=StyleName,
        _Second=get_next_backend_style( StyleTable ), StyleTable ),

    { STECtrl, NewStyleTable }.



-doc "Declares the specified style with the specific settings".
-spec declare_style( ste(), style_name(), maybe_list( style_option() ) ) ->
                                                    ste().
declare_style( STE, StyleName, StyleOpts ) when is_list( StyleOpts ) ->
    % Must be declared for the next apply operations:
    NewSTE = declare_style( STE, StyleName ),
    [ apply_style_option( NewSTE, StyleName, SO ) || SO <- StyleOpts ],
    NewSTE;

declare_style( STE, StyleName, SingleStyleOpt ) ->
    declare_style( STE, StyleName, _StyleOpts=[ SingleStyleOpt ] ).




-doc "Sets the current styling start to the specified position.".
-spec set_styling_at( ste(), style_name(), char_pos() ) -> void().
set_styling_at( _STE={ STECtrl, StyleTable }, StyleName, StartPos ) ->
    wxStyledTextCtrl:startStyling( STECtrl, StartPos-1,
        to_backend_style( StyleName, StyleTable ) ).


-doc "Sets the specified style of the specified STE to be bold or not.".
-spec set_bold( ste(), style_name(), boolean() ) -> void().
set_bold( _STE={ STECtrl, StyleTable }, StyleName, IsBold ) ->
    wxStyledTextCtrl:styleSetBold( STECtrl,
        to_backend_style( StyleName, StyleTable ), IsBold ).


-doc "Sets the specified style of the specified STE to be underlined or not.".
-spec set_underline( ste(), style_name(), boolean() ) -> void().
set_underline( _STE={ STECtrl, StyleTable }, StyleName, IsUnderlined ) ->
    wxStyledTextCtrl:styleSetUnderline( STECtrl,
        to_backend_style( StyleName, StyleTable ), IsUnderlined ).


-doc "Sets the foreground color of the specified style of the specified STE.".
-spec set_foreground_color( ste(), style_name(), any_color() ) -> void().
set_foreground_color( _STE={ STECtrl, StyleTable }, StyleName, AnyColor ) ->

    wxStyledTextCtrl:styleSetForeground( STECtrl,
        to_backend_style( StyleName, StyleTable ),
        _AnyColorByDecimal=gui_color:get_any_color( AnyColor ) ).


-doc "Sets the background color of the specified style of the specified STE.".
-spec set_background_color( ste(), style_name(), any_color() ) -> void().
set_background_color( _STE={ STECtrl, StyleTable }, StyleName, AnyColor ) ->

    wxStyledTextCtrl:styleSetBackground( STECtrl,
        to_backend_style( StyleName, StyleTable ),
        _AnyColorByDecimal=gui_color:get_any_color( AnyColor ) ).




% Helper section for styled text editors.


-doc "Returns the new backend style that may be declared.".
-spec get_next_backend_style( style_table() ) -> backend_style().
get_next_backend_style( StyleTable ) ->
    bijective_table:size( StyleTable ) + 1.


-doc "Applies the specified style option to the specified style.".
-spec apply_style_option( ste(), style_name(), style_option() ) -> void().
apply_style_option( STE, StyleName, _StyleOpt=bold ) ->
    set_bold( STE, StyleName, _IsBold=true );


apply_style_option( STE, StyleName, _StyleOpt=underline ) ->
    set_underline( STE, StyleName, _IsUnderlined=true );

apply_style_option( STE, StyleName,
                    _StyleOpt={ foreground_color, AnyColor } ) ->
    set_foreground_color( STE, StyleName, AnyColor );


apply_style_option( STE, StyleName,
                    _StyleOpt={ background_color, AnyColor } ) ->
    set_background_color( STE, StyleName, AnyColor );


apply_style_option( _STE, _StyleName, OtherStyleOpt ) ->
    throw( { unsupported_style_option, OtherStyleOpt } ).



-doc """
Converts the specified text editor option(s) into the appropriate backend
specific options.

(helper)
""".
-spec to_wx_editor_opts( maybe_list( styled_text_editor_option() ) ) ->
                                            [ wx_opt_pair() ].
to_wx_editor_opts( Options ) when is_list( Options )->
    [ to_wx_text_editor_opt( O ) || O <- Options ];

% Probably a pair:
to_wx_editor_opts( Opt ) ->
    to_wx_editor_opts( [ Opt ] ).


% (helper)
-spec to_wx_text_editor_opt( styled_text_editor_option() ) -> wx_opt_pair().
to_wx_text_editor_opt( _Opt={ position, Pos } ) ->
    { pos, Pos };

to_wx_text_editor_opt( Opt={ size, _S } ) ->
    Opt;

% Corresponds to the general MyriadGUI/wx widget styles, such as 'left_justify':
to_wx_text_editor_opt( _Opts={ style, Styles } ) ->
    WxStyle = lists:foldl( fun( S, Acc ) ->
        gui_generated:get_second_for_text_editor_style( S ) bor Acc end,
        _InitialAcc=0,
        _List=Styles ),

    { style, WxStyle }.



-doc """
Translates the specified MyriadGUI style name into the corresponding backend
style, registering it for the first time if needed.
""".
-spec to_backend_style( style_name(), style_table() ) ->
                                            { backend_style(), style_table() }.
to_backend_style( StyleName, StyleTable ) ->
    case bijective_table:get_maybe_second_for( _K=StyleName, StyleTable ) of

        undefined ->
            throw( { unknown_style_name, StyleName } );

        WxStyle ->
            WxStyle

    end.



-doc """
Translates the specified backend style into the corresponding MyriadGUI style
name.
""".
-spec to_style_name( backend_style(), style_table() ) -> style_name().
to_style_name( WxStyle, StyleTable ) ->
    case bijective_table:get_maybe_first_for( _K=WxStyle, StyleTable ) of

        undefined ->
            throw( { unknown_backend_style_name, WxStyle } );

        StyleName ->
            StyleName

    end.
