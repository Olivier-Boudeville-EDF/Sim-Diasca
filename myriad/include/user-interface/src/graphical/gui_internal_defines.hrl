% Copyright (C) 2003-2021 Olivier Boudeville
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
% Creation date: Saturday, September 16, 2017.


% Internal gui defines, hence to be included solely by the gui subsystem, not by
% user code (as depends on wx).


% For wx records and all:
-include_lib("wx/include/wx.hrl").


% Some defines:

-define( any_id, ?wxID_ANY ).

-define( no_parent, wx:null() ).


% The special color that means "transparent" (i.e. no filling):
-define( transparent_color, ?wxTRANSPARENT_BRUSH ).



% A reference onto a GUI object, for the widgets that MyriadGUI added to the
% backend at hand.
%
% Results in terms such as: { myriad_object_ref, canvas, CanvasPid }.
%
-record( myriad_object_ref, {

		% The type of GUI object referred to (ex: 'canvas'):
		object_type :: gui:myriad_object_type(),

		% The identifier of this referenced instance:
		myriad_instance_pid :: gui:myriad_instance_pid()

}).

-type myriad_object_ref() :: #myriad_object_ref{}.
