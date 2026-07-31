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
% Creation date: Wednesday, July 17, 2024.

-module(gui_splitter).

-moduledoc """
Gathering of various facilities for **splitter windows**, which are windows able
to be split into two panes.

See also the gui_window module and the corresponding test,
gui_splitter_test.erl.
""".



-doc """
A window able to be split into two panes; it may thus manage up to two
subwindows.

An horizontal splitter separates a splitter window in a top pane ("first" pane)
and a bottom one ("second" pane), whereas a vertical splitter separates a
splitter window in a left pane ("first" pane) and a right one ("second" pane).

One may click (first mouse button) on the splitter and, while maintaining that
button down, drag (if this movement is allowed) the splitter to another
position.

If the allow_unsplit style has been selected - otherwise if the minimum pane
length is zero (the default) - double-clicking on the splitter separation will
unsplit the splitter, i.e. make the bottom (for vertical splitters) or right
(for horizontal ones) pane disappear (any hidden pane will still exist).
""".
-opaque splitter_window() :: wxSplitterWindow:wxSplitterWindow().



-doc "Splitter-specific options.".
-type splitter_option() :: { 'position', point() }
                         | { 'size', size() }
                         | { 'style', [ splitter_style() ] }.



-doc """
A style element of a splitter.

See also <https://docs.wxwidgets.org/3.1/classwx_splitter_window.html>.
""".
-type splitter_style() ::
    'three_dim_effect' % Draws a 3D effect for splitter and border.
  | 'thin_splitter' % Draws a thin splitter.
  | 'three_dim_splitter' % Draws a 3D splitter (part of the default style).
  | 'three_dim_border' % Draws a 3D border.
  | 'standard_border' % Draws a standard border.
  | 'no_border' % Draws no border (the default).
  | 'no_xp_theme' % On Windows, pre-XP look.
  | 'allow_unsplit' % Always allows to unsplit, even if the minimum pane length
                    % is zero.
  | 'live_update'. % Resizes the child windows immediately (strongly recommended
                   % for a smoother control / to avoid flicker, unless at least
                   % a pane is long to redraw).


-doc """
Tells, as a ratio, how much the first pane of a splitter window is to grow while
resizing that window; for example:
 - 0.0: only the bottom/right pane window is automatically resized
 - 0.5: both pane windows grow by equal size
 - 1.0: only left/top pane window is resized

This ratio should be a value between 0.0 and 1.0; its default value is 0.0 (the
length of the first pane is therefore fixed).
""".
% (corresponds to wx's sash gravity).
-type scaling_ratio() :: number().


-doc """
The position, in pixels, of a splitter within its window, between the two panes.

A null position will set the splitter to the middle of the window.

A negative value will "wrap around" the splitter's position. For example, -10
will place the splitter at 10 pixels from right of an (vertical) splitter
window.
""".
-type splitter_position() :: length().




-export_type([ splitter_window/0, scaling_ratio/0, splitter_position/0,
               splitter_option/0 ]).



-export([ create/1, create/2, create/5, create/6, destruct/1,
          get_scaling_ratio/1, set_scaling_ratio/2,
          get_splitter_position/1, set_splitter_position/2,
          get_minimum_pane_length/1, set_minimum_pane_length/2,
          get_orientation/1, set_orientation/2,
          is_split/1,
          set_unique_pane/2, set_panes/4,
          get_first_pane/1, get_second_pane/1, get_panes/1,
          replace_pane/3, unsplit/1, update_size/1 ]).



% For ?gui_any_id:
-include("gui_internal_defines.hrl").


% Silencing:
-export([ to_wx_splitter_options/1 ]).


% Type shorthands:

-type bit_mask() :: type_utils:bit_mask().

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type os_type() :: system_utils:os_type().

-type length() :: gui:length().
-type point() :: gui:point().
-type size() :: gui:size().

-type position() :: gui:position().
-type orientation() :: gui:orientation().
-type parent() :: gui:parent().

-type window() :: gui_window:window().

-type id() :: gui_id:id().


% Local type:

-type wx_splitter_option() :: term().


% Implementation notes:
%
% MyriadGUI replaces the term 'sash' with 'splitter'.
%
% In wxWidgets/wx, in addition to the wxSplitterWindow used here, there is also
% a wxSashWindow whose purpose seems quite the same. Maybe that the
% wxSplitterWindow API is a bit more convenient.



-doc """
Creates a splitter, in the specified parent window.

No live update will be enabled, resulting on the dragging of the splitter
appearing not smooth.

The up to two subwindows and splitting can be declared afterwards.
""".
-spec create( parent() ) -> splitter_window().
create( Parent ) ->
    wxSplitterWindow:new( Parent ).



-doc """
Creates a splitter, in the specified parent window, with the specified options.

This is the most flexible way of creating a splitter; however the versions of
higher arities of this function are generally preferred, as the splitter styles
are very platform-specific and offer actually little choices.
""".
-spec create( maybe_list( splitter_option() ), parent() ) -> splitter_window().
create( Options, Parent ) ->
    wxSplitterWindow:new( Parent, to_wx_splitter_options( Options ) ).



-doc """
Creates a splitter of the specified position, size and styles, in the specified
parent window, with the specified identifier.

The up to two subwindows and splitting can be declared afterwards.
""".
-spec create( position(), size(), [ splitter_style() ], id(), parent() ) ->
                                            splitter_window().
create( Position, Size, Styles, Id, Parent ) ->
    create( Position, Size, Styles,
            _OSType=system_utils:get_operating_system_type(),
            Id, Parent ).



-doc """
Creates a splitter of the specified position, size and styles, in the specified
parent window, according to the specified OS, with the specified identifier.

The up to two subwindows and splitting can be declared afterwards.

Most complete creation function.
""".
-spec create( position(), size(), [ splitter_style() ], os_type(), id(),
              parent() ) -> splitter_window().
create( Position, Size, Styles, OSType, Id, Parent ) ->

    ExtraStyles = case OSType of

        { _OSFamily=unix, _OSName=darwin } ->
            [ live_update, three_dim_splitter ];

        { win32, _ } ->
            [ live_update, standard_border ];

        _ ->
            [ live_update, three_dim_effect ]

    end,

    AllStyles = ExtraStyles ++ Styles,

    ActualId = gui_id:declare_any_id( Id ),

    SplitterWin = wxSplitterWindow:new( Parent, [
        { id, ActualId },
        { pos, Position },
        { size, Size },
        { style, splitter_styles_to_bitmask( AllStyles ) } ] ),

    % No wxSplitterWindow:split{Horizontally,Vertically}(Splitter, Win1, Win2)
    % call here, as we do not have Win1 or Win2 yet, and cannot have them at
    % that point, as their parent must be the just-created, still-to-be-returned
    % splitter window.

    SplitterWin.



-doc "Destructs the specified splitter window.".
-spec destruct( splitter_window() ) -> void().
destruct( SplitterWin ) ->
    wxSplitterWindow:destroy( SplitterWin ).




-doc """
Returns the scaling ratio that shall apply between the two panes of the
specified splitter window.
""".
-spec get_scaling_ratio( splitter_window() ) -> scaling_ratio().
get_scaling_ratio( SplitterWin ) ->
    wxSplitterWindow:getSashGravity( SplitterWin ).


-doc """
Sets the scaling ratio that shall apply between the two panes of the specified
splitter window.
""".
-spec set_scaling_ratio( splitter_window(), scaling_ratio() ) -> void().
set_scaling_ratio( SplitterWin, ScalingRatio ) ->
    wxSplitterWindow:setSashGravity( SplitterWin, _SashGravity=ScalingRatio ).




-doc """
Returns the current position of the splitter.
""".
-spec get_splitter_position( splitter_window() ) -> splitter_position().
get_splitter_position( SplitterWin ) ->
    wxSplitterWindow:getSashPosition( SplitterWin ).


-doc """
Sets the current position of the splitter.

Triggers a redraw.

Does not currently check for an out-of-range value.
""".
-spec set_splitter_position( splitter_window(), splitter_position() ) -> void().
set_splitter_position( SplitterWin, SplitterPos ) ->
    wxSplitterWindow:setSashPosition( SplitterWin, _SashPosition=SplitterPos ).




-doc """
Returns the minimum pane length of the specified splitter window.
""".
-spec get_minimum_pane_length( splitter_window() ) -> length().
get_minimum_pane_length( SplitterWin ) ->
    wxSplitterWindow:getMinimumPaneSize( SplitterWin ).


-doc """
Sets the minimum pane length of the specified splitter window.

The default minimum pane length is zero, which means that either pane can be
reduced to zero by dragging the splitter, leading to hiding this pane.

To prevent this behaviour (and veto out-of-range splitter dragging), a minimum
size (e.g. 20 pixels) shall be set, provided the allow_unsplit option was not
used at creation.
""".
-spec set_minimum_pane_length( splitter_window(), length() ) -> void().
set_minimum_pane_length( SplitterWin, MinPaneLength ) ->
    wxSplitterWindow:setMinimumPaneSize( SplitterWin, MinPaneLength ).




-doc "Returns the current orientation of the specified splitter.".
-spec get_orientation( splitter_window() ) -> orientation().
get_orientation( SplitterWin ) ->
    wxSplitterWindow:getSplitMode( SplitterWin ).


-doc """
Sets the current orientation of the specified splitter.

Only sets the internal variable; does not update the display.
""".
-spec set_orientation( splitter_window(), orientation() ) -> void().
set_orientation( SplitterWin, Orientation ) ->
    wxSplitterWindow:setSplitMode( SplitterWin, Orientation ).



-doc """
Returns whether the splitter is split, that is if its two panes are visible.
""".
-spec is_split( splitter_window() ) -> boolean().
is_split( SplitterWin ) ->
    wxSplitterWindow:isSplit( SplitterWin ).



-doc """
Sets the specified splitter in a single pane configuration, using for that the
specified window.

The child window (main, single pane) is shown if it is currently hidden.
""".
-spec set_unique_pane( splitter_window(), window() ) -> void().
set_unique_pane( SplitterWin, SinglePaneWin ) ->
    wxSplitterWindow:initialize( SplitterWin, SinglePaneWin ).



-doc """
Sets the two panes of the specified splitter window, splitting it according to
the specified orientation.

Returns whether it has been split (true), or if it was already split (false).

The application should have checked whether the splitter is not already split
(see is_split/1).
""".
-spec set_panes( splitter_window(), window(), window(), orientation() ) ->
                                            boolean().
% Initializes the top and bottom panes of the splitter window:
set_panes( SplitterWin, FirstWindowPane, SecondWindowPane,
           _Orientation=horizontal ) ->
    wxSplitterWindow:splitHorizontally( SplitterWin, FirstWindowPane,
                                        SecondWindowPane );

% Initializes the left and right panes of the splitter window:
set_panes( SplitterWin, FirstWindowPane, SecondWindowPane,
           _Orientation=vertical ) ->
    wxSplitterWindow:splitVertically( SplitterWin, FirstWindowPane,
                                      SecondWindowPane ).


-doc "Returns the first pane (if any) of the specified splitter window.".
-spec get_first_pane( splitter_window() ) -> option( window() ).
get_first_pane( SplitterWin ) ->
    wxSplitterWindow:getWindow1( SplitterWin ).


-doc "Returns the second pane (if any) of the specified splitter window.".
-spec get_second_pane( splitter_window() ) -> option( window() ).
get_second_pane( SplitterWin ) ->
    wxSplitterWindow:getWindow2( SplitterWin ).


-doc "Returns the two panes (if any) of the specified splitter window.".
-spec get_panes( splitter_window() ) ->
                                { option( window() ), option( window() ) }.
get_panes( SplitterWin ) ->
    { wxSplitterWindow:getWindow1( SplitterWin ),
      wxSplitterWindow:getWindow2( SplitterWin ) }.



-doc """
Replaces the specified child pane by the specified window.

It is in general better to call this function instead of calling unsplit/2 and
then resplitting the window back, because it will provoke much less flicker (if
any). It is valid to call this function whether the splitter has two panes or
only one.

CurrentWindowPane must specify one of the panes managed by the splitter.

If the parameters are incorrect or the window could not be replaced, false is
returned. Otherwise the function will return true, but please notice that it
will not delete the replaced window and you may wish to do it yourself.
""".
-spec replace_pane( splitter_window(), window(), window() ) -> boolean().
replace_pane( SplitterWin, CurrentWindowPane, NewWindowPane ) ->
    wxSplitterWindow:replaceWindow( SplitterWin, _WinOld=CurrentWindowPane,
                                    _WinNew=NewWindowPane ).



-doc """
Unsplits the specified splitter window, so that only the first pane is visible;
returns whether it could be unsplit, i.e. if it was initially split indeed.
""".
-spec unsplit( splitter_window() ) -> boolean().
unsplit( SplitterWin ) ->
    wxSplitterWindow:unsplit( SplitterWin ).



-doc """
Causes any pending sizing of the splitter and child panes to take place
immediately.

Such resizing normally takes place in idle time, in order to wait for layout to
be completed. However, this can cause unacceptable flicker as the panes are
resized after the window has been shown. To work around this, you can perform
window layout (for example by sending a size event to the parent window), and
then call this function, before showing the top-level window.
""".
-spec update_size( splitter_window() ) -> void().
update_size( SplitterWin ) ->
    wxSplitterWindow:updateSize( SplitterWin ).



-doc """
Converts the specified MyriadGUI splitter option(s) into the appropriate
wx-specific options.

(exported helper)
""".
-spec to_wx_splitter_options( maybe_list( splitter_option() ) ) ->
                                            [ wx_splitter_option() ].
to_wx_splitter_options( Options ) when is_list( Options ) ->
    to_wx_splitter_options( Options, _Acc=[] );

to_wx_splitter_options( Option ) ->
    to_wx_splitter_options( [ Option ] ).



% (helper)
to_wx_splitter_options( _Options=[], Acc ) ->
    Acc;

to_wx_splitter_options( _Options=[ { style, Style } | T ], Acc ) ->
    to_wx_splitter_options( T,
        [ { style, splitter_styles_to_bitmask( Style ) } | Acc ] );

% Unchanged:
to_wx_splitter_options( _Options=[ H | T ], Acc ) ->
    to_wx_splitter_options( T, [ H | Acc ] ).



-doc """
Converts the specified MyriadGUI splitter style elements into the appropriate
wx-specific bit mask.

(helper)
""".
-spec splitter_styles_to_bitmask( [ splitter_style() ] ) -> bit_mask().
splitter_styles_to_bitmask( StyleOpts ) when is_list( StyleOpts ) ->
    lists:foldl( fun( S, Acc ) ->
                    gui_generated:get_second_for_splitter_style( S ) bor Acc
                 end,
                 _InitialAcc=0,
                 _List=StyleOpts ).

% Styles are better not maybe_lists (only options are):
%splitter_styles_to_bitmask( StyleOpt ) ->
%   gui_generated:get_second_for_splitter_style( StyleOpt ).
