% Copyright (C) 2023-2024 Olivier Boudeville
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
% Creation date: Thursday, December 21, 2023.


% @doc Gathering of various facilities for <b>scrollable widgets</b>, like
% panels that can be scrolled horizontally and/or vertically.
%
% Note that using the with_vertical_scrollbar / with_horizontal_scrollbar window
% styles is generally not a viable solution (refer to their specification for
% more details).
%
-module(gui_scrollable).



% Implementation notes:
%
% To be able to scrolling in some orientation, a non-zero increment shall have
% set for it.


-opaque scrollable() :: wxScrolledWindow:wxScrolledWindow().
% A scrollable is a widget that can be scrolled horizontally and/or vertically.


-type step_count() :: integer().
% A number of scroll steps.

-type pixels_per_scroll_step() :: integer().
% A number of pixels per scroll step (a.k.a. a scroll increment).


-export_type([ scrollable/0, step_count/0, pixels_per_scroll_step/0 ]).


-export([ create/1, create/2, destruct/1 ]).


% Default number of pixels per step:
-define( default_scroll_increment, 5 ).



% Shorthands:

-type parent() :: gui:parent().



% @doc Creates a scrollable, with the specified parent and the default number of
% pixels per scroll step.
%
-spec create( maybe( parent() ) ) -> scrollable().
create( MaybeParent ) ->
	create( _Inc=?default_scroll_increment, MaybeParent ).



% @doc Creates a scrollable, with the specified scroll increment (both for
% horizontal and vertical orientations) and the specified parent.
%
-spec create( pixels_per_scroll_step()
			| { pixels_per_scroll_step(), pixels_per_scroll_step() },
			  maybe( parent() ) ) -> scrollable().
create( { HPixPerStep, VPixPerStep }, MaybeParent ) ->
	Scrollable = wxScrolledWindow:new( MaybeParent ),

	% Otherwise no scrollbar will be enable:
	wxScrolledWindow:setScrollRate( Scrollable, HPixPerStep, VPixPerStep ),

	Scrollable;

create( PixPerStep, MaybeParent ) ->
	create( { PixPerStep, PixPerStep }, MaybeParent ).



% @doc Destructs the specified scrollable.
-spec destruct( scrollable() ) -> void().
destruct( Scrollable  ) ->
	wxScrolledWindow:destroy( Scrollable ).
