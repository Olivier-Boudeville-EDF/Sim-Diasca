% Copyright (C) 2021-2023 Olivier Boudeville
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
% Creation date: Sunday, August 8, 2021.


% @doc Gathering of various convenient <b>email-related</b> facilities.
-module(email_utils).



% Implementation notes.
%



% Type declarations.


-type email_address() :: ustring().
% Electronic address for email (ex: "john@hello.org").


-type bin_email_address() :: bin_string().
% Binary email address (ex: `<<"john@hello.org">>').


% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().


-export_type([ email_address/0, bin_email_address/0 ]).
