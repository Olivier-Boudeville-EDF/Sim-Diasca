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
% Creation date: Tuesday, January 29, 2013



% Header to export MyriadGUI-related defines, both for user code and for
% internal one.
%
% See gui.erl for the corresponding implementation.


% Context sent to corresponding subscribers together with an event.
%
% This context can be ignored in most cases.
%
-record( gui_event_context, {


		   % The identifier of the event source (generally not useful, as the
		   % gui_object shall be enough):
		   %
		   id :: gui:id(),


		   % Usually of no use, as such user data is a means of preserving a
		   % state, whereas the user event loop is better to do so:
		   %
		   user_data = [] :: gui:user_data(),


		   % The full, lower-level event (if any) resulting in our event:
		   %
		   % (useful for example when deciding to propagate it upward in the
		   % widget hierarchy)
		   %
		   backend_event = undefined :: maybe( gui:backend_event() )


}).

-type gui_event_context() :: #gui_event_context{}.
