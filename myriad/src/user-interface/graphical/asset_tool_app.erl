% Copyright (C) 2010-2023 Olivier Boudeville
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
% Creation date: 2010.


% @doc The very beginning of a tool to manage <b>assets</b>, especially for
% rendering.
%
% @hidden Note: extremely far from being ready.
%
-module(asset_tool_app).


-export([ exec/0 ]).


% State of the program, passed between event handlers.
-record( app_state, {

	main_frame :: frame(),
	load_image_button :: button(),
	quit_button :: button(),
	info_sizer :: sizer(),
	left_panel :: panel(),
	canvas :: canvas() } ).

-type app_state() :: #app_state{}.
% The application state.



% Shorthands:

-type width() :: gui:width().
-type height() :: gui:height().

-type canvas() :: gui_canvas:canvas().

-type frame() :: gui_frame:frame().

-type sizer() :: gui_sizer:sizer().

-type button() :: gui_button:button().

-type panel() :: gui_panel:panel().


% Temporary:
-export_type([ app_state/0 ]).

-export([ gui_main_loop/1, get_canvas_width/0, get_canvas_height/0,
		  render_main_view/1, load_image/1 ]).


% @doc Returns the width of the main frame.
-spec get_main_frame_width() -> width().
get_main_frame_width() ->
	800.

% @doc Returns the height of the main frame.
-spec get_main_frame_height() -> height().
get_main_frame_height() ->
	600.


% @doc Returns the width of the displayed canvas.
-spec get_canvas_width() -> width().
get_canvas_width() ->
	640.

% @doc Returns the height of the displayed canvas.
-spec get_canvas_height() -> height().
get_canvas_height() ->
	480.



% (helper)
-spec init_app_gui() -> no_return().
init_app_gui() ->

	gui:start(),

	FrameSize = { get_main_frame_width(), get_main_frame_height() },

	MainFrame = gui_frame:create( _Title="Asset tool", FrameSize ),

	gui:subscribe_to_events( { onWindowClosed, MainFrame } ),

	%gui_widget:set_background_color( MainFrame, red ),
	%gui_widget:set_background_color( LeftPanel, blue ),
	%gui_widget:set_background_color( RightPanel, green ),

	StatusBar = gui_statusbar:create( MainFrame ),

	gui_statusbar:push_text( StatusBar, "Waiting for assets to manage." ),

	LeftPanel = gui_panel:create( MainFrame ),

	RightPanel = gui_panel:create( MainFrame ),

	MainSizer = gui_sizer:create( _Orientation=horizontal ),

	% Constant width:
	gui_sizer:add_element( MainSizer, LeftPanel,
						   [ { proportion, 0 }, expand_fully ] ),

	% Grows with the window:
	gui_sizer:add_element( MainSizer, RightPanel,
						   [ { proportion, 2 }, expand_fully ] ),

	LeftSizer = gui_sizer:create( vertical ),

	ControlBoxSizer = gui_sizer:create_with_labelled_box( vertical, "Controls",
														  LeftPanel ),

	InfoSizer = gui_sizer:create_with_labelled_box( vertical, "Information",
													LeftPanel ),

	update_information_sizer( InfoSizer, LeftPanel, [ "(no image loaded)" ] ),

	gui_sizer:add_elements( LeftSizer, [ ControlBoxSizer, InfoSizer ] ),


	% Adding the buttons to the control panel:

	% Common settings:

	LoadImageButton = gui_button:create( "Load image", LeftPanel ),

	QuitButton = gui_button:create( "Quit", LeftPanel ),

	gui_widget:set_tooltip( LeftPanel, "Controls for assets" ),

	% Not working apparently:
	gui_widget:set_tooltip( LoadImageButton, "Load image" ),

	gui_sizer:add_elements( ControlBoxSizer, [ LoadImageButton, QuitButton ],
						   expand_fully ),


	gui_widget:set_sizer( LeftPanel, LeftSizer ),

	AssetBoxSizer = gui_sizer:create_with_labelled_box( vertical, "Asset View",
														RightPanel ),

	Canvas = gui_canvas:create( RightPanel ),

	gui_canvas:set_background_color( Canvas, pink ),

	gui_sizer:add_element( AssetBoxSizer, Canvas,
						   [ { proportion, 1 }, expand_fully ] ),

	gui_widget:set_tooltip( Canvas, "Asset view." ),

	gui_widget:set_sizer( RightPanel, AssetBoxSizer ),

	gui_widget:set_sizer( MainFrame, MainSizer ),

	% Sets the GUI to visible:
	gui_frame:show( MainFrame ),

	InitialAppState = #app_state{ main_frame=MainFrame,
								  load_image_button=LoadImageButton,
								  quit_button=QuitButton,
								  info_sizer=InfoSizer,
								  left_panel=LeftPanel,
								  canvas=Canvas },

	EventsOfInterest = [ { onWindowClosed, MainFrame } ],

	% To be done before rendering the GUI (with gui:show/1), as it may result in
	% events to be emitted (e.g. onRepaintNeeded) that would not be received, if
	% not already subscribed to:
	%
	gui:subscribe_to_events( EventsOfInterest ),

	app_main_loop( InitialAppState  ).


app_main_loop( _AppState ) ->
	fixme.


% @doc Renders the main view of the tool.
-spec render_main_view( canvas() ) -> void().
render_main_view( Canvas ) ->

	app_facilities:display( "Rendering main view." ),

	gui_canvas:set_background_color( Canvas, white ),

	gui_canvas:clear( Canvas ),

	gui_canvas:blit( Canvas ).


% (helper)
%
% @private
%
gui_main_loop( _GUIState=#app_state{ main_frame=_MainFrame,
									 load_image_button=_LoadImageButton,
									 quit_button=_QuitButton,
									 info_sizer=_InfoSizer,
									 left_panel=_LeftPanel,
									 canvas=_Canvas } ) ->

	app_facilities:display( "~nEntering main loop.", [] ).

	%% Update = receive

	%	#wx{ obj=MainFrame, event={wxClose,close_window} } ->
	%		app_facilities:display( "Quitting GUI app." ),
	%		quit;

	%	#wx{ obj=LoadImageButton,
	%		 event=#wxCommand{ type=command_button_clicked } } ->
	%		app_facilities:display( "Load image clicked." ),
	%		Texts = load_image( Canvas ),
	%		gui_canvas:blit( Canvas ),
	%		update_information_sizer( InfoSizer, LeftPanel, Texts ),
	%		State;

	%	#wx{ obj=QuitButton,
	%		 event=#wxCommand{ type=command_button_clicked } } ->
	%		app_facilities:display( "Quit button clicked." ),
	%		quit;

	%	#wx{ obj=Any, event=#wxCommand{ type=command_button_clicked } } ->
	%		app_facilities:display( "Following button clicked: ~w.", [ Any ] ),
	%		quit;


	%	% Received for example when another window overlapped:
	%	#wx{ event=#wxPaint{} } ->
	%		render_main_view( Canvas ),
	%		State ;


	%	#wx{ event=#wxSize{ size=NewSize } } ->

	%		app_facilities:display( "Resizing to ~w.", [ NewSize ] ),

	%		NewCanvas = gui_canvas:resize( Canvas, NewSize ),

	%		render_main_view( NewCanvas ),

	%		State#app_state{ canvas=NewCanvas };


	%	Any ->
	%		app_facilities:display( "GUI got event '~w' (ignored).",
	%								[ Any ] ),
	%		State

	% end,

	% case Update of

	%	quit ->
	%		% Simply stop recursing:
	%		ok;

	%	NewState ->
	%		gui_main_loop( NewState )

	% end.



% @doc Loads image in specified canvas, and returns a list of information texts
% about it.
%
load_image( Canvas ) ->
	gui_canvas:load_image( Canvas, { 40, 20 }, "../../doc/example.bmp" ),
	[ "eeee", "aaaa", "rrrrr" ].



% @doc Updates information sizer with specified texts.
update_information_sizer( InfoSizer, Panel, Texts ) ->

	%gui_sizer:clear( InfoSizer ),

	TextOpts = [ { border, 10 }, expand_fully ],

   [ gui_sizer:add_element( InfoSizer,
		gui_text:create_static_display( T, Panel ), TextOpts ) || T <- Texts ].



% @doc Executes that application.
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
