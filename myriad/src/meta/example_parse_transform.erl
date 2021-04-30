% Copyright (C) 2014-2021 Olivier Boudeville
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



% Example of parse transform.
%
% See meta_utils.erl and meta_utils_test.erl.
%
-module(example_parse_transform).



% Implementation notes:


-export([ parse_transform/2 ]).


% The parse transform itself, transforming the specified Abstract Format code
% into another one.
%
parse_transform( AST, _Options ) ->

	% Less verbose:
	io:format( "		  (applying example parse transform '~p')~n",
			   [ ?MODULE ] ),

	%io:format( "Applying example parse transform '~p' on:~n~p",
	%		   [ ?MODULE, AST ] ),

	AST.
