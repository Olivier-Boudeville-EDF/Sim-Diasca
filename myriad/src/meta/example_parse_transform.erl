% Copyright (C) 2014-2026 Olivier Boudeville
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
% Creation date: Friday, December 19, 2014.

-module(example_parse_transform).

-moduledoc """
Example of parse transform.

See meta_utils.erl and meta_utils_test.erl.
""".


-export([ parse_transform/2 ]).


% Shorthands:

-type ast() :: ast_base:ast().

-type parse_transform_options() :: meta_utils:parse_transform_options().



-doc """
The parse transform itself, transforming the specified Abstract Format code into
another one.
""".
-spec parse_transform( ast(), parse_transform_options() ) -> ast().
parse_transform( AST, _Options ) ->

    % Less verbose:
    io:format( "          (applying example parse transform '~p')~n",
               [ ?MODULE ] ),

    %io:format( "Applying example parse transform '~p' on:~n~p",
    %          [ ?MODULE, AST ] ),

    AST.
