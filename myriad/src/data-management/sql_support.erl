% Copyright (C) 2016-2021 Olivier Boudeville
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
% Creation date: Wednesday, June 8, 2016



% Gathering of various facilities regarding the management of SQL databases from
% Erlang.
%
% The current implementation is mostly geared towards the use of SQLite3 (an
% optional dependency), through the erlang-sqlite3 binding (see
% https://github.com/alexeyr/erlang-sqlite3).
%
% See sql_support_test.erl for the corresponding test.
%
-module(sql_support).

% For the various types defined:
-include("sqlite3.hrl").


% Implementation notes:
%
% To manage SQL databases, we rely on SQLite3 and the erlang-sqlite3 binding,
% expected to be already built and installed, in ~/Software/erlang-sqlite3.
%
-export([ start/0, stop/0 ]).




% General support section, regarding the SQL service itself.


% Starts (checks and inits) the SQL (in practice: SQLite3) service support.
%
-spec start() -> void().
start() ->

	% We have to secure the erlang-sqlite3 binding, nevertheless nothing special
	% is needed (ex: finding ebin/sqlite3.beam results in finding automatically
	% priv/sqlite3_drv.so).
	%
	%io:format( "Starting SQL support, using SQLite3.~n").
	ok.


% Stops the SQL support.
%
-spec stop() -> void().
stop() ->
	%io:format( "Stopping SQL support.").
	ok.
