% Copyright (C) 2014-2021 Olivier Boudeville
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
% Creation date: Wednesday, December 24, 2014


% Centralizes, on behalf of the WOOPER parse transform, some utilities to
% transform ASTs.
%
-module(wooper_parse_utils).


-export([ get_state_type/0, get_state_var/0 ]).


-type form_element() :: ast_base:form_element().



% Returns a form element corresponding to wooper:state().
%
% (shorthand)
%
-spec get_state_type() -> form_element().
get_state_type() ->
	Line=0,
	{ remote_type, Line, [ {atom,Line,wooper}, {atom,Line,state}, [] ] }.



% Returns a form element corresponding to the State variable.
%
% (shorthand)
%
-spec get_state_var() -> form_element().
get_state_var() ->
	Line=0,
	{ var, Line, 'State' }.
