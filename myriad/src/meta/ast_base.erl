% Copyright (C) 2018-2021 Olivier Boudeville
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
% Creation date: Sunday, February 4, 2018.



% Module in charge of providing base, transverse constructs to manage ASTs,
% notably in order to transform them (meta-programming).
%
-module(ast_base).



% Section about general locations in AST, sources, etc.


% So that forms can be independently managed while being able to be reordered
% later, according to their original position in a source AST.
%
-type form_location() :: id_utils:sortable_id().


% In-file reference, typically like:
% {"../data-management/simple_parse_transform_target.erl",1}.
%
-type file_reference() :: basic_utils:maybe( file_utils:file_path() ).



% Line location (i.e. line number) of a form in a source file:
-type line() :: erl_anno:line().


% Line-related location in a source file (either line() or {line(), column()}):
%
-type file_loc() :: erl_anno:location().


% Context of a form:
-type form_context() :: basic_utils:maybe( line() | file_loc() ).


% In-source context (typically to report errors):
% (ex: {"foo.erl",112}).
%
-type source_context() :: { file_utils:filename(),
							basic_utils:maybe( line() ) }.

-export_type([ form_location/0, file_reference/0, line/0, file_loc/0,
			   form_context/0, source_context/0 ]).




% Section about AST elements.


% Most general form of an element of an AST.
%
-type ast_element() :: tuple().


% Abstract form, part of an AST (ex: {attribute,40,file,{"foo.erl",40}}):
%
-type form() :: erl_parse:abstract_form() | erl_parse:form_info().


% An element (a part) of a form (ex: a clause of a function definition):
%
-type form_element() :: any().


% Abstract Syntax Tree, standard representation of parse trees for Erlang
% programs as Erlang terms. This representation is known as the abstract format.
%
% Defined as erl_parse_tree().
%
% See also:
%
% - for the type: http://erlang.org/doc/man/erl_parse.html#type-erl_parse_tree
%
% - for the overall logic and structure:
% http://erlang.org/doc/apps/erts/absform.html
%
-type ast() :: [ form() ].


% In-AST description of a value of type atom:
%
-type ast_atom() :: { 'atom', line(), atom() }.


-export_type([ ast_element/0, form/0, form_element/0, ast/0, ast_atom/0 ]).
