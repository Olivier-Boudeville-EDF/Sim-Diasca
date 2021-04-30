% Copyright (C) 2010-2021 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)



% This is an Erlang script preferably to be called by the edit-time-series.sh
% wrapper script, for convenience.
%
-module(edit_time_series_app).


-include_lib("myriad/include/app_facilities.hrl").


% No export needed for exec/0 here.

-spec exec() -> no_return().
exec() ->

	app_facilities:start( ?MODULE ),

	io:format( "Parameters received: ~p~n.", [ init:get_plain_arguments() ] ),

	app_facilities:stop().
