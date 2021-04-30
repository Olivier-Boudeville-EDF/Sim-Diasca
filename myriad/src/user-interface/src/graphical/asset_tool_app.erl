% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redisruntribute it and/or modify it under the terms of the
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



% Tool to manage rendering assets.
%
-module(asset_tool_app).


% For exec/0 export and al:
-include("app_facilities.hrl").




% To remove:
-export([ gui_main_loop/1, get_canvas_width/0, get_canvas_height/0,
		  render_main_view/1 ]).


% State of the program, passed between event handlers.
%
-record( app_state, {

		  main_frame = undefined :: gui:frame(),
		  load_image_button = undefined :: gui:button(),
		  quit_button = undefined :: gui:button(),
		  info_sizer = undefined :: gui:sizer(),
		  left_panel = undefined :: gui:panel(),
		  canvas = undefined :: gui:canvas()

}).

-type app_state() :: #app_state{}.

% FIXME:
-export_type([ app_state/0 ]).
-export([ load_image/1 ]).


-spec get_main_window_width() -> linear:coordinate().
get_main_window_width() ->
	800.


-spec get_main_window_height() -> linear:coordinate().
get_main_window_height() ->
	600.



-spec get_canvas_width() -> linear:coordinate().
get_canvas_width() ->
	640.


-spec get_canvas_height() -> linear:coordinate().
get_canvas_height() ->
	480.




-spec init_app_gui() -> no_return().
init_app_gui() ->

	InitialGUIState = gui:start(),

	FrameSize = { get_main_window_width(), get_main_window_height() },

	MainFrame = gui:create_frame( _Title="Asset tool", FrameSize ),

	gui:connect( MainFrame, close_window ),

	%gui:set_background_color( MainFrame, red ),
	%gui:set_background_color( LeftPanel, blue ),
	%gui:set_background_color( RightPanel, green ),

	StatusBar = gui:create_status_bar( MainFrame ),

	gui:push_status_text( "Waiting for assets to manage.", StatusBar ),

	LeftPanel = gui:create_panel( MainFrame ),

	RightPanel = gui:create_panel( MainFrame ),

	MainSizer = gui:create_sizer( horizontal ),

	% Constant width:
	gui:add_to_sizer( MainSizer, LeftPanel,
					  [ { proportion, 0 }, { flag,[ expand_fully ] } ] ),

	% Grows with the window:
	gui:add_to_sizer( MainSizer, RightPanel,
					  [ { proportion, 2 }, { flag, [ expand_fully ] } ] ),

	LeftSizer = gui:create_sizer( vertical ),

	ControlBoxSizer = gui:create_sizer_with_labelled_box( vertical, LeftPanel,
														  "Controls" ),

	InfoSizer = gui:create_sizer_with_labelled_box( vertical, LeftPanel,
													"Information" ),

	update_information_sizer( InfoSizer, LeftPanel, [ "(no image loaded)" ] ),

	gui:add_to_sizer( LeftSizer, ControlBoxSizer ),

	gui:add_to_sizer( LeftSizer, InfoSizer ),

	% Adding the buttons to the control panel:

	% Common settings:

	LoadImageButton = gui:create_button( "Load image", LeftPanel ),

	QuitButton = gui:create_button( "Quit", LeftPanel ),

	gui:set_tooltip( LeftPanel, "Controls for assets" ),

	% Not working apparently:
	gui:set_tooltip( LoadImageButton, "Load image" ),

	ButtonOpt = [ { flag, [ expand_fully ] } ],

	gui:add_to_sizer( ControlBoxSizer, [ LoadImageButton, QuitButton ], 
					  ButtonOpt ),


	gui:set_sizer( LeftPanel, LeftSizer ),

	AssetBoxSizer = gui:create_sizer_with_labelled_box( vertical, RightPanel,
														"Asset View" ),

	Canvas = gui_canvas:create( RightPanel ),

	gui_canvas:set_background_color( Canvas, pink ),

	gui:add_to_sizer( AssetBoxSizer, Canvas,
					  [ { proportion, 1 }, { flag, [ expand_fully ] } ] ),

	gui:set_tooltip( Canvas, "Asset view." ),

	gui:set_sizer( RightPanel, AssetBoxSizer ),

	gui:set_sizer( MainFrame, MainSizer ),

	% Sets the GUI to visible:
	gui:show( MainFrame ),

	InitialGUIState = #app_state{ main_frame=MainFrame,
								  load_image_button=LoadImageButton,
								  quit_button=QuitButton,
								  info_sizer=InfoSizer,
								  left_panel=LeftPanel,
								  canvas=Canvas },

	SubscribedEvents = [ { onWindowClosed, MainFrame } ],

	ReadyGUIState = gui:handle_events( InitialGUIState, SubscribedEvents ),

	app_main_loop( MainFrame, ReadyGUIState ).


app_main_loop( _, _ ) ->
	fixme.


-spec render_main_view( gui_canvas:canvas() ) -> void().
render_main_view( Canvas ) ->

	app_facilities:display( "Rendering main view." ),

	gui_canvas:set_background_color( Canvas, white ),

	gui_canvas:clear( Canvas ),

	gui_canvas:blit( Canvas ).



gui_main_loop( _GUIState=#app_state{ main_frame=_MainFrame,
									 load_image_button=_LoadImageButton,
									 quit_button=_QuitButton,
									 info_sizer=_InfoSizer,
									 left_panel=_LeftPanel,
									 canvas=_Canvas } ) ->

	app_facilities:display( "~nEntering main loop.", [] ).

	%% Update = receive

	%%	#wx{ obj=MainFrame, event={wxClose,close_window} } ->
	%%		app_facilities:display( "Quitting GUI app." ),
	%%		quit;

	%%	#wx{ obj=LoadImageButton,
	%%		 event=#wxCommand{ type=command_button_clicked } } ->
	%%		app_facilities:display( "Load image clicked." ),
	%%		Texts = load_image( Canvas ),
	%%		gui_canvas:blit( Canvas ),
	%%		update_information_sizer( InfoSizer, LeftPanel, Texts ),
	%%		State;

	%%	#wx{ obj=QuitButton,
	%%		 event=#wxCommand{ type=command_button_clicked } } ->
	%%		app_facilities:display( "Quit button clicked." ),
	%%		quit;

	%%	#wx{ obj=Any, event=#wxCommand{ type=command_button_clicked } } ->
	%%		app_facilities:display( "Following button clicked: ~w.", [ Any ] ),
	%%		quit;


	%%	% Received for example when another window overlapped:
	%%	#wx{ event=#wxPaint{} } ->
	%%		render_main_view( Canvas ),
	%%		State ;


	%%	#wx{ event=#wxSize{ size=NewSize } } ->

	%%		app_facilities:display( "Resizing to ~w.", [ NewSize ] ),

	%%		NewCanvas = gui_canvas:resize( Canvas, NewSize ),

	%%		render_main_view( NewCanvas ),

	%%		State#app_state{ canvas=NewCanvas };


	%%	Any ->
	%%		app_facilities:display( "GUI got event '~w' (ignored).",
	%%								[ Any ] ),
	%%		State

	%% end,

	%% case Update of

	%%	quit ->
	%%		% Simply stop recursing:
	%%		ok;

	%%	NewState ->
	%%		gui_main_loop( NewState )

	%% end.



% Loads image in specified canvas, and returns a list of information texts about
% it.
%
load_image( Canvas ) ->
	gui_canvas:load_image( Canvas, { 40, 20 }, "../../doc/example.bmp" ),
	[ "eeee", "aaaa", "rrrrr" ].



% Updates information sizer with specified texts.
%
update_information_sizer( InfoSizer, Panel, Texts ) ->

	%gui:clear_sizer( InfoSizer ),

	TextOpts = [ { flag, [ expand_fully ] }, { border, 10 } ],

   [ gui:add_to_sizer( InfoSizer, gui_text:create_static( Panel, T ),
						TextOpts ) || T <- Texts ].


% Executes that application.
%
-spec exec() -> no_return().
exec() ->

	app_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			app_facilities:display( "(not running the asset application, "
									"being in batch mode)" );

		false ->
			init_app_gui()

	end,

	app_facilities:stop().
