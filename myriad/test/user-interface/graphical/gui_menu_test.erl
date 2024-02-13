% Copyright (C) 2022-2024 Olivier Boudeville
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
% Creation date: Friday, April 22, 2022.


% @doc Unit tests for the management of <b>menus</b>, in menu bars and popup
% menus.
%
-module(gui_menu_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Shorthands:

-type frame() :: gui_frame:frame().
-type menu() :: gui_menu:menu().
-type menu_bar() :: gui_menu:menu_bar().


-type my_test_state() :: { frame(), menu() }.
% Here the main loop just has to remember the popup menu to activate in case of
% right click on the main frame, and this frame whose closing is awaited for.



-spec create_named_ids_dropdown( menu_bar() ) -> menu().
create_named_ids_dropdown( MenuBar ) ->

	% Named drop-down:
	NamedMenu = gui_menu:create(),

	[ gui_menu:add_item( NamedMenu, N,
		_Label=text_utils:format( "I am named &'~ts'", [ N ] ) )
								|| N <- gui_menu:get_standard_item_names() ],

	gui_menu:add_menu( MenuBar, NamedMenu, "&Named menu" ),

	NamedMenu.



-spec create_numerical_ids_dropdown( menu_bar()  ) -> menu().
create_numerical_ids_dropdown( MenuBar ) ->

	% Numerical drop-down:
	NumMenu = gui_menu:create(),

	[ gui_menu:add_item( NumMenu, N,
					_Label=text_utils:format( "I am id #~B", [ N ] ) )
								|| N <- lists:seq( 4999, 5206 ) ],

	gui_menu:add_menu( MenuBar, NumMenu, "&Numerical menu" ),

	NumMenu.



-spec create_popup_menu() -> { menu(), menu(), menu() }.
create_popup_menu() ->

	PopupMenu = gui_menu:create(),

	% No name specified, so they will rely on an arbitrary integer identifier:
	_A = gui_menu:add_item( PopupMenu, _Label="Item A" ),
	_B = gui_menu:add_item( PopupMenu, _Id=undefined, "Item B" ),

	gui_menu:add_separator( PopupMenu ),

	FirstSubMenu = gui_menu:create(),

	% Defining here named identifiers rathen than auto-set numerical ones:
	_C = gui_menu:append_submenu( PopupMenu, item_c, "Item C", FirstSubMenu ),

	SecondSubMenu = gui_menu:create(),

	_D = gui_menu:append_submenu( PopupMenu, item_d, "Item D", SecondSubMenu,
								  "I am D's help" ),


	_E = gui_menu:add_checkable_item( FirstSubMenu, _EId=item_e, "Item E" ),
	_F = gui_menu:add_checkable_item( FirstSubMenu, _FId=item_f, "Item F",
									  "I am F's help" ),
	_G = gui_menu:add_checkable_item( FirstSubMenu, _GId=item_g, "Item G" ),

	% E let as it is.
	gui_menu:set_checkable_item( FirstSubMenu, item_f,
									  _SetAsChecked=true ),

	gui_menu:set_checkable_item( FirstSubMenu, item_g, false ),

	_H = gui_menu:add_radio_item( PopupMenu, undefined, "Item H" ),

	_I = gui_menu:add_radio_item( PopupMenu, undefined, "Item I",
								  "I am I's help" ),

	_J = gui_menu:add_separator( PopupMenu ),

	gui_menu:set_item_status( PopupMenu, item_c, enabled ),
	gui_menu:set_item_status( PopupMenu, item_d, disabled ),

	{ PopupMenu, FirstSubMenu, SecondSubMenu }.



% @doc Executes the actual test.
-spec run_gui_test() -> void().
run_gui_test() ->

	test_facilities:display( "~nStarting the menu test; use the menu bar "
		"and/or right-click on the frame to obtain a popup menu." ),

	gui:start(),

	Frame = gui_frame:create( "This is the overall frame for menu testing" ),


	MenuBar = gui_menu:create_bar(),

	test_facilities:display( "If you spot a menu item of interest in "
		"the 'Numerical menu' dropdown that is not in 'Named menu', "
		"feel free to add it to MyriadGUI "
		"(refer to the gui and the gui_constants modules)." ),

	NameMenu = create_named_ids_dropdown( MenuBar ),

	NumMenu = create_numerical_ids_dropdown( MenuBar ),

	gui_frame:set_menu_bar( Frame, MenuBar ),

	{ PopupMenu, FirstSubMenu, SecondSubMenu } = create_popup_menu(),

	AllMenus = [ PopupMenu, FirstSubMenu, SecondSubMenu, NameMenu, NumMenu ],

	gui:subscribe_to_events( [
		{ [ onMouseRightButtonReleased, onWindowClosed ], Frame },
		{ onItemSelected, AllMenus } ] ),

	gui_frame:show( Frame ),

	test_main_loop( _InitialState={ Frame, PopupMenu } ).




% @doc A very simple main loop, whose actual state is simply the GUI object
% corresponding to the frame that shall be closed to stop the test
% (i.e. CloseFrame).
%
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( State={ Frame, PopupMenu } ) ->

	trace_utils:info( "Test main loop running..." ),

	receive

		{ onMouseRightButtonReleased, [ Frame, _FrameId, _Context ] } ->
			%trace_utils:debug_fmt( "onMouseRightButtonReleased for frame ~w.",
			%                       [ Frame ] ),
			gui_menu:activate_as_popup( PopupMenu, Frame ),
			test_main_loop( State );

		{ onItemSelected, [ _Menu, _ItemId=exit_menu_item, _Context ] } ->
			trace_utils:info( "Exit menu item selected, stopping test." );

		{ onItemSelected, [ _Menu, ItemId, _Context ] } ->
			%trace_utils:debug_fmt( "Received for menu ~w: ~w",
			%                       [ Menu, Context ] ),
			trace_utils:info_fmt( "Menu item '~w' selected.", [ ItemId ] ),
			test_main_loop( State );

		{ onWindowClosed, [ Frame, _FrameId, _Context ] } ->
			trace_utils:info( "Main frame has been closed; test success." ),
			gui_frame:destruct( Frame ),
			gui:stop();

		Other ->
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message: ~p.", [ Other ] ),
			test_main_loop( State )

	end.



% @doc Runs the test.
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
