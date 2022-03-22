% Copyright (C) 2010-2022 Olivier Boudeville
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


% @doc Gathering of various facilities for <b>text rendering</b>.
-module(gui_text).



% Operations for static labels-related.
-export([ create_static/2, create_static/3, create_static/4 ]).



-type static_text() :: wxStaticText:wxStaticText().

-type style_option() :: 'align_left' | 'center' | 'align_right'
					  | 'no_autoresize'.


-type text_option() ::   { 'pos', point() }
					   | { 'size', size() }
					   | { 'style', [ style_option() ] }.


-type text_options() :: [ text_option() ].


-export_type([ static_text/0, style_option/0, text_option/0, text_options/0 ]).



% For related defines:
-include("gui.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").


% Shorthands:

-type bit_mask() :: basic_utils:bit_mask().

-type ustring() :: text_utils:ustring().

-type window() :: gui:window().
-type id() :: gui:id().

-type point() :: gui:point().
-type size() :: gui:size().



% @doc Creates a static text, based on specified identifier and plain string.
-spec create_static( window(), ustring() ) -> static_text().
create_static( Parent, Label ) ->
	create_static( _DefaultId=-1, Parent, Label ).



% @doc Creates a static text, based on specified identifier and plain string.
-spec create_static( id(), window(), ustring() ) -> static_text().
create_static( Id, Parent, Label ) ->
	create_static( Id, Parent, Label, _Options=[] ).



% @doc Creates a static text, based on specified identifier, plain string and
% options.
%
-spec create_static( id(), window(), ustring(), text_options() ) ->
												static_text().
create_static( Id, Parent, Label, Options ) ->

	ActualOpts = get_text_options( Options ),

	wxStaticText:new( Parent, Id, Label, ActualOpts ).



% @doc Converts specified text options into the appropriate back-end specific
% options.
%
% (helper)
%
get_text_options( _Options=[] ) ->
	[];

get_text_options( Options ) ->
	get_text_options( Options, _Acc=[] ).


get_text_options( _Opts= [ { style, S } | T ], Acc ) ->
	get_text_options( T, [ { style, style_option_to_bitmask( S ) } | Acc ] );


get_text_options( [ H | T ], Acc ) ->
	get_text_options( T, [ H | Acc ] ).



% @doc Converts specified text style into the appropriate back-end specific bit
% mask.
%
% (helper)
%
-spec style_option_to_bitmask( style_option() | [ style_option() ] ) ->
														bit_mask().
style_option_to_bitmask( StyleList ) when is_list( StyleList ) ->

	lists:foldl( fun( S, Acc ) -> style_option_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

style_option_to_bitmask( _Style=align_left ) ->
	?wxALIGN_LEFT;

style_option_to_bitmask( _Style=center ) ->
	?wxALIGN_CENTER;

style_option_to_bitmask( _Style=align_right ) ->
	?wxALIGN_RIGHT;

style_option_to_bitmask( _Style=no_autoresize ) ->
	?wxST_NO_AUTORESIZE.
