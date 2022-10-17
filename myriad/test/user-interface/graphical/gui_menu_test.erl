% Copyright (C) 2022-2022 Olivier Boudeville
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

-type frame() :: gui:frame().
-type menu() :: gui:menu().
-type menu_bar() :: gui:menu_bar().


-type my_test_state() :: { frame(), menu() }.
% Here the main loop just has to remember the popup menu to activate in case of
% right click on the main frame, and this frame whose closing is awaited for.


-spec create_named_ids_dropdown( menu_bar() ) -> menu().
create_named_ids_dropdown( MenuBar ) ->

	% Named drop-down:
	NamedMenu = gui:create_menu(),

	[ gui:add_item( NamedMenu, N,
					_Label=text_utils:format( "I am named &'~ts'", [ N ] ) )
								|| N <- gui:get_standard_item_names() ],

	gui:add_menu( MenuBar, NamedMenu, "&Named menu" ),

	NamedMenu.



-spec create_numerical_ids_dropdown( menu_bar()  ) -> menu().
create_numerical_ids_dropdown( MenuBar ) ->

	% Numerical drop-down:
	NumMenu = gui:create_menu(),

	[ gui:add_item( NumMenu, N,
					_Label=text_utils:format( "I am id #~B", [ N ] ) )
								|| N <- lists:seq( 4999, 5206 ) ],

	gui:add_menu( MenuBar, NumMenu, "&Numerical menu" ),

	NumMenu.



-spec create_popup_menu() -> { menu(), menu(), menu() }.
create_popup_menu() ->

	PopupMenu = gui:create_menu(),

	% No name specified, so they will rely on an arbitrary integer identifier:
	_A = gui:add_item( PopupMenu, _Label="Item A" ),
	_B = gui:add_item( PopupMenu, _Id=undefined, "Item B" ),

	gui:add_separator( PopupMenu ),

	FirstSubMenu = gui:create_menu(),

	% Defining here named identifiers rathen than auto-set numerical ones:
	_C = gui:append_submenu( PopupMenu, item_c, "Item C", FirstSubMenu ),

	SecondSubMenu = gui:create_menu(),

	_D = gui:append_submenu( PopupMenu, item_d, "Item D", SecondSubMenu,
							 "I am D's help" ),


	_E = gui:add_checkable_item( FirstSubMenu, _EId=item_e, "Item E" ),
	_F = gui:add_checkable_item( FirstSubMenu, _FId=item_f, "Item F",
								 "I am F's help" ),
	_G = gui:add_checkable_item( FirstSubMenu, _GId=item_g, "Item G" ),

	% E let as it is.
	gui:set_checkable_menu_item( FirstSubMenu, item_f, _SetAsChecked=true ),
	gui:set_checkable_menu_item( FirstSubMenu, item_g, false ),

	_H = gui:add_radio_item( PopupMenu, undefined, "Item H" ),
	_I = gui:add_radio_item( PopupMenu, undefined, "Item I", "I am I's help" ),

	_J = gui:add_separator( PopupMenu ),

	gui:set_menu_item_status( PopupMenu, item_c, enabled ),
	gui:set_menu_item_status( PopupMenu, item_d, disabled ),

	{ PopupMenu, FirstSubMenu, SecondSubMenu }.



% @doc Executes the actual test.
-spec run_gui_test() -> void().
run_gui_test() ->

	test_facilities:display( "~nStarting the menu test; use the menu bar "
		"and/or right-click on the frame to obtain a popup menu." ),

	gui:start(),

	Frame = gui:create_frame( "This is the overall frame for menu testing" ),


	MenuBar = gui:create_menu_bar(),

	test_facilities:display( "If you spot a menu item of interest in "
		"the 'Numerical menu' dropdown that is not in 'Named menu', "
		"feel free to add it to MyriadGUI." ),

	NameMenu = create_named_ids_dropdown( MenuBar ),

	NumMenu = create_numerical_ids_dropdown( MenuBar ),

	gui:set_menu_bar( Frame, MenuBar ),

	{ PopupMenu, FirstSubMenu, SecondSubMenu } = create_popup_menu(),

	AllMenus = [ PopupMenu, FirstSubMenu, SecondSubMenu, NameMenu, NumMenu ],

	gui:subscribe_to_events( [
		{ [ onMouseRightButtonReleased, onWindowClosed ], Frame },
		{ onItemSelected, AllMenus } ] ),

	gui:show( Frame ),

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
			%						[ Frame ] ),
			gui:activate_popup_menu( Frame, PopupMenu ),
			test_main_loop( State );

		{ onItemSelected, [ _Menu, _ItemId=exit_menu_item, _Context ] } ->
			trace_utils:info( "Exit menu item selected, stopping test." );

		{ onItemSelected, [ _Menu, ItemId, _Context ] } ->
			%trace_utils:debug_fmt( "Received for menu ~w: ~w",
			%                       [  Menu, Context ] ),
			trace_utils:info_fmt( "Menu item '~w' selected.", [ ItemId ] ),
			test_main_loop( State );

		{ onWindowClosed, [ Frame, _FrameId, _Context ] } ->
			trace_utils:info( "Main frame has been closed; test success." ),
			gui:destruct_window( Frame ),
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
