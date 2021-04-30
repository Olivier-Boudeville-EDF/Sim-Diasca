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
% Creation date: Saturday, February 3, 2018.




% Describes the transformations to be applied onto an AST when scanning it.
%
% Typically centralises the automatic replacements of all known kinds to be
% performed.
%
-record( ast_transforms, {


	% Transformations (if any) defined for module-local types:
	local_types = undefined :: basic_utils:maybe(
								  ast_transform:local_type_transform_table() ),


	% Transformations (if any) defined for remote types:
	remote_types = undefined :: basic_utils:maybe(
								  ast_transform:remote_type_transform_table() ),


	% Transformations (if any) defined for module-local calls:
	local_calls = undefined :: basic_utils:maybe(
								  ast_transform:local_call_transform_table() ),


	% Transformations (if any) defined for remote calls:
	remote_calls = undefined :: basic_utils:maybe(
								  ast_transform:remote_call_transform_table() ),


	% Allows to record the name of the module being transformed (useful for
	% example for error messages, to report the source filename):
	%
	transformed_module_name = undefined ::
	  basic_utils:maybe( basic_utils:module_name() ),


	% Allows to record the name and arity of the function (if any) being
	% transformed (useful for example for error messages):
	%
	transformed_function_identifier = undefined ::
	  basic_utils:maybe( meta_utils:function_id() ),


	% Records the transformations (if any) to be applied on AST elements as a
	% whole (generally exceeding the mere transformations of calls into calls,
	% like rewriting bodies) whenever a trigger is found.
	%
	% Triggers currently supported: 'call', 'body'.
	%
	transform_table = undefined ::
	  basic_utils:maybe( ast_transform:ast_transform_table() ),


	% Any user-defined transformation state that is to be kept and updated
	% in the course of a transformation.
	%
	transformation_state = undefined :: ast_transform:transformation_state(),


	% The function to call in order to format, for the sake of logging in a
	% user-friendly manner, the output of a transformation.
	%
	% Introduced so that a transformation caller can set a formatter managing
	% the transformation state that was defined (useful if some parts of it are
	% not of interest and/or very bulky to format).
	%
	transform_formatter = fun ast_transform:default_formatter/2
								   :: ast_transform:transform_formatter()

} ).
