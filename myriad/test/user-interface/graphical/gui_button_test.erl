% Copyright (C) 2023-2026 Olivier Boudeville
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
% Creation date: Friday, August 18, 2023.

-module(gui_button_test).

-moduledoc """
Unit tests for the management of **buttons**, possibly with icons in them.
""".


% For run/0 export and al:
-include("test_facilities.hrl").


-doc """
Here the main loop just has to remember the frame whose closing is awaited for
(and a few buttons).
""".
-type my_test_state() :: { frame(),
    { toggle_button(), [ bitmap_button() ], [ button() ] } }.



% Type shorthands:

-type frame() :: gui_frame:frame().
-type button() :: gui_button:button().
-type toggle_button() :: gui_button:toggle_button().
-type bitmap_button() :: gui_button:bitmap_button().



-doc "Executes the actual test.".
-spec run_gui_test() -> void().
run_gui_test() ->

    test_facilities:display( "~nStarting the button test." ),

    gui:start(),

    Frame = gui_frame:create( "This is the overall frame for button testing",
                              _Size={ 1280, 1024 } ),


    % Should a button of interest (e.g. see
    % https://docs.wxwidgets.org/stable/page_stockitems.html) be lacking, feel
    % free to add it to MyriadGUI (refer to the gui and the gui_constants
    % modules).
    %
    test_facilities:display(
        "A button may be clicked for further information." ),


    Panel = gui_panel:create( Frame ),

    Position = auto,
    ButtonSize = auto,
    ButtonStyle = [],
    ButtonParent = Panel,

    ColumnCount = 8,

    % Will auto-adjust the number of rows:
    GridSizer = gui_sizer:create_grid( _RowCount=0, ColumnCount,
                                       _HorizGap=2, _VertGap=1 ),

    gui_widget:set_sizer( Panel, GridSizer ),

    % Fetching all standard button identifiers:
    { button_id, Entries, _ElemLookup } =
        gui_constants:get_button_id_topic_spec(),

    % Button identifier, with the MyriadGUI specific ones added::
    AllButtonIds = pair:firsts( Entries )
        ++ gui_button:get_myriadgui_identifiers(),

    trace_utils:notice_fmt( "Identifiers of the ~B MyriadGUI buttons: ~ts.",
        [ length( AllButtonIds ),
          text_utils:atoms_to_listed_string( AllButtonIds ) ] ),


    % First, two non-basic buttons:

    ToggleButton = gui_button:create_toggle( "I am a\ntoggle button",
        _TogId=toggle_button, ButtonParent ),


    CustomImagePath = "../../../doc/myriad-very-small.png",

    CustomBmp = gui_bitmap:create_from( CustomImagePath ),

    CustomBitmapButton = gui_button:create_bitmap( CustomBmp,
        _BmpId=bitmap_button, ButtonParent ),



    AllBitmapButtons= [ CustomBitmapButton ],

    % Showing that we cannot set custom labels if selecting a standard/stock
    % identifier (so we cannot have both a non-default label and the
    % corresponding icon unfortunately)
    %
    LostIconButton = gui_button:create( "This is a label\n(thus no icon)",
        _ButtId=zoom_factor_fit_button, ButtonParent ),


    % To force the use of the stock labels:
    NoLabel = "",

    BasicButtons = [ LostIconButton |
        [ gui_button:create( NoLabel, Position, ButtonSize, ButtonStyle,
                             BId, ButtonParent ) || BId <- AllButtonIds ] ],

    AllPlainButtons = AllBitmapButtons ++ BasicButtons,

    AllButtons = [ ToggleButton | AllPlainButtons ],

    ButtonFlags = [ { proportion, 0 }, { border_width, 4 }, all_borders ],

    gui_sizer:add_elements( GridSizer, AllButtons, ButtonFlags ),

    % No specific need to call gui:layout/1 or gui:{refresh,update}/1.

    gui:subscribe_to_events( [ { onWindowClosed, Frame },
                               { onButtonToggled, ToggleButton }
        | [ { onButtonClicked, B } || B <- AllPlainButtons ] ] ),

    gui_frame:show( Frame ),

    test_main_loop( _InitialState={ Frame,
        { ToggleButton, AllBitmapButtons, BasicButtons } } ).



-doc """
A very simple main loop, whose actual state is simply the GUI object
corresponding to the frame that shall be closed to stop the test
(i.e. CloseFrame), and the buttons.
""".
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( State={ Frame, _Buttons } ) ->

    trace_utils:info( "Test main loop running..." ),

    receive

        { onButtonClicked, [ Button, ButtonId=exit_button, EventContext ] } ->
            trace_utils:debug_fmt(
                "Exit Button ~ts (~ts) clicked (~ts).",
                [ gui:object_to_string( Button ),
                  gui_id:id_to_string( ButtonId ),
                  gui_event:context_to_string( EventContext ) ] ),
            stop( State );

        { onButtonClicked, [ Button, ButtonId, EventContext ] } ->
            trace_utils:debug_fmt( "Plain button ~ts (~ts) clicked (~ts).",
                [ gui:object_to_string( Button ),
                  gui_id:id_to_string( ButtonId ),
                  gui_event:context_to_string( EventContext ) ] ),
            test_main_loop( State );

        { onButtonToggled, [ Button, ButtonId, EventContext ] } ->
            trace_utils:debug_fmt( "Toggle Button ~ts (~ts) toggled (~ts).",
                [ gui:object_to_string( Button ),
                  gui_id:id_to_string( ButtonId ),
                  gui_event:context_to_string( EventContext ) ] ),
            test_main_loop( State );

        { onWindowClosed, [ Frame, _FrameId, _EventContext ] } ->
            trace_utils:info( "Main frame has been closed." ),
            stop( State );

        Other ->
            trace_utils:warning_fmt( "Test main loop ignored following "
                                     "message: ~p.", [ Other ] ),
            test_main_loop( State )

    end.


stop( _State={ Frame, { ToggleButton, AllBitmapButtons, BasicButtons } } ) ->
    trace_utils:info( "Test success, destructing buttons." ),

    gui_button:destruct_toggle( ToggleButton ),

    [ gui_button:destruct_bitmap( BMB ) || BMB <- AllBitmapButtons ],

    [ gui_button:destruct( B ) || B <- BasicButtons ],

    gui_frame:destruct( Frame ),

    trace_utils:info( "Stopping." ),
    gui:stop().



-doc "Runs the test.".
-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    case executable_utils:is_batch() of

        true ->
            test_facilities:display(
                "(not running the MyriadGUI test, being in batch mode)" );

        false ->
            run_gui_test()

    end,

    test_facilities:stop().
