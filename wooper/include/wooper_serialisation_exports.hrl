% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER library.
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


% Modular WOOPER header gathering all serialisation-related exports.



% The conventional atom to mark internal, local processes that must escape the
% serialisation/deserialisation processes:
%
-define( process_restoration_marker, 'wooper_restore_local_process' ).


% The conventional atom to mark internal, local open files (akin to file
% descriptor)s) that must escape the serialisation/deserialisation processes:
%
-define( file_restoration_marker, 'wooper_restore_local_file' ).


% The conventional atom to mark internal, local terms that must escape the
% serialisation process (ex: they may be recreated afterwards):
%
-define( term_restoration_marker, 'wooper_restore_local_term' ).



% Serialisation hooks and all:
%
-export([

		 % Not exported anymore, as no method shall be:
		 %serialise/3,

		 pre_serialise_hook/1, post_serialise_hook/3,
		 pre_deserialise_hook/2, post_deserialise_hook/1

		 ]).
