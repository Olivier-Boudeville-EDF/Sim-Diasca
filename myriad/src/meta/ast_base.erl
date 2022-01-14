% Copyright (C) 2018-2022 Olivier Boudeville
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



% @doc Module in charge of providing <b>base, transverse constructs to manage
% ASTs</b>, notably in order to transform them (meta-programming).
%
-module(ast_base).



% Section about general locations in AST, sources, etc.


-type form_location() :: id_utils:sortable_id().
% So that forms can be independently managed while being able to be reordered
% later, according to their original position in a source AST.


-type file_reference() :: basic_utils:maybe( file_utils:file_path() ).
% In-file reference, typically like:
% `{"../data-management/simple_parse_transform_target.erl",1}'.


-type line() :: erl_anno:line().
% Line location (that is: line number, starting at 0) of a form in a source
% file.


-type column() :: erl_anno:column().
% Column location (that is: column number, starting at 1) of a form in a source
% file.


-type file_loc() :: erl_anno:location().
% Line-related location in a source file (either `line()' or `{line(),
% column()}').


-type form_context() :: basic_utils:maybe( file_loc() ).
% Context of a form.


-type source_context() ::
		{ file_utils:filename(), basic_utils:maybe( file_loc() ) }.
% In-source context (typically to report errors); ex: `{"foo.erl",{112,4}}'.


-export_type([ form_location/0, file_reference/0, line/0, column/0, file_loc/0,
			   form_context/0, source_context/0 ]).




% Section about AST elements.


-type ast_element() :: tuple().
% Most general form of an element of an AST.


-type form() :: erl_parse:abstract_form() | erl_parse:form_info().
% Abstract form, part of an AST (ex: `{attribute,40,file,{"foo.erl",40}}').


-type form_element() :: any().
% An element (a part) of a form (ex: a clause of a function definition).


-type ast() :: [ form() ].
% <em>Abstract Syntax Tree</em>, standard representation of parse trees for
% Erlang programs as Erlang terms. This representation is known as the abstract
% format.
%
% Defined as `erl_parse_tree()'.
%
% See also:
%
% - for the type: [http://erlang.org/doc/man/erl_parse.html#type-erl_parse_tree]
%
% - for the overall logic and structure:
% [http://erlang.org/doc/apps/erts/absform.html]


-type ast_atom() :: { 'atom', file_loc(), atom() }.
% In-AST description of a value of type atom.


-export_type([ ast_element/0, form/0, form_element/0, ast/0, ast_atom/0 ]).
