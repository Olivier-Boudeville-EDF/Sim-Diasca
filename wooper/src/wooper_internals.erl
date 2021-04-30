% Copyright (C) 2018-2021 Olivier Boudeville
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
% Creation date: Friday, April 13, 2018.


% Gathering of internal helpers.
-module(wooper_internals).


-export([ raise_error/1, raise_error/2,
		  raise_usage_error/1, raise_usage_error/2, raise_usage_error/3,
		  raise_usage_error/4,
		  notify_warning/1, notify_warning/2 ]).


% For the ast_transforms record:
-include_lib("myriad/include/ast_transform.hrl").


% Shorthands:

-type ustring() :: text_utils:ustring().
-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type line() :: ast_base:line().

-type ast_transforms() :: ast_transform:ast_transforms().

-type classname() :: wooper:classname().



% To better report errors:
-define( origin_layer, "WOOPER" ).


% We try to distinguish two kinds of errors:
%
% - rather WOOPER-internal ones, maybe not involving a mistake from the user,
% rather uncommon and where a stacktrace might help (raise_error/N); results in
% an exception being thrown
%
% - usage-related ones, where a precise message with hints may help and where
% the WOOPER-based stacktrace is a useless technical detail
% (raise_usage_error/N); results in the compilation aborting



% Raises a (compile-time, rather ad hoc) technical, internal error when applying
% this parse transform, to stop the build on failure and report the actual
% error.
%
-spec raise_error( term() ) -> no_return().
raise_error( ErrorTerm ) ->
	raise_error( ErrorTerm, _Context=undefined ).



% Raises a (compile-time, rather ad hoc) technical, internal error, with
% specified source context, when applying this parse transform, to stop the
% build on failure and report the actual error.
%
-spec raise_error( term(), ast_base:source_context() ) -> no_return().
raise_error( ErrorTerm, Context ) ->
	ast_utils:raise_error( ErrorTerm, Context, ?origin_layer ).



% Raises a (compile-time, rather ad hoc) user-related error (when no specific
% source context is available), when applying this parse transform, to stop the
% build on failure and report adequately the actual error to the user.
%
-spec raise_usage_error( ustring() ) -> no_return().
raise_usage_error( ErrorString ) ->
	raise_usage_error( ErrorString, _ErrorFormatValues=[] ).


% Raises a (compile-time, rather ad hoc) user-related error (when no specific
% source context is available), when applying this parse transform, to stop the
% build on failure and report adequately the actual error to the user.
%
-spec raise_usage_error( format_string(), format_values() ) -> no_return().
raise_usage_error( ErrorFormatString, ErrorFormatValues ) ->

	io:format( "error: " ++ ErrorFormatString ++ "~n", ErrorFormatValues ),

	% Almost the only way to stop the processing of the AST:
	halt( 6 ).


% Raises a (compile-time, rather ad hoc) user-related error, with specified
% source context, when applying this parse transform, to stop the build on
% failure and report adequately the actual error to the user.
%
-spec raise_usage_error( ustring(), ast_transforms(), line() ) -> no_return();
					   ( ustring(), classname(), line() ) -> no_return();
					   ( format_string(), format_values(), classname() ) ->
								no_return().
raise_usage_error( ErrorString,
				   #ast_transforms{ transformed_module_name=Classname },
				   Line ) ->

	ExpectedSrcFile = wooper:get_class_filename( Classname ),

	% Finally not used, as we do not need here to specify the layer or to print
	% a stacktrace:
	%
	%ast_utils:raise_error( ErrorString, _Context={ ExpectedModFile, Line },
	%					   ?origin_layer ).
	io:format( "~ts:~B: ~ts~n", [ ExpectedSrcFile, Line, ErrorString ] ),

	% Almost the only way to stop the processing of the AST:
	halt( 6 );


raise_usage_error( ErrorString, Classname, Line ) when is_atom( Classname ) ->

	ExpectedSrcFile = wooper:get_class_filename( Classname ),

	io:format( "~ts:~B: ~ts~n", [ ExpectedSrcFile, Line, ErrorString ] ),

	% Almost the only way to stop the processing of the AST:
	halt( 6 );


raise_usage_error( ErrorFormatString, ErrorFormatValues, Classname ) ->

	ExpectedSrcFile = wooper:get_class_filename( Classname ),

	% Cannot target better:
	Line = 0,

	io:format( "~ts:~B: " ++ ErrorFormatString ++ "~n",
			   [ ExpectedSrcFile, Line | ErrorFormatValues ] ),

	% Almost the only way to stop the processing of the AST:
	halt( 6 ).



% Raises a (compile-time, rather ad hoc) user-related error, with specified
% source context, when applying this parse transform, to stop the build on
% failure and report the actual error.
%
-spec raise_usage_error( format_string(), format_values(),
			ast_transforms() | classname(), line() ) -> no_return().
raise_usage_error( ErrorFormatString, ErrorValues, TransformsOrClass, Line ) ->
	ErrorString = text_utils:format( ErrorFormatString, ErrorValues ),
	raise_usage_error( ErrorString, TransformsOrClass, Line ).



% Notifies a (compile-time, rather ad hoc) warning, with no specific context,
% when applying this parse transform.
%
% Does not stop the build.
%
-spec notify_warning( [ term() ] ) -> void().
notify_warning( Elements ) ->
	notify_warning( Elements, _Context=undefined ).


% Notifies a (compile-time, rather ad hoc) warning, with specified context, when
% applying this parse transform.
%
% Does not stop the build.
%
-spec notify_warning( [ term() ], ast_base:form_context() ) -> void().
notify_warning( Elements, Context ) ->
	ast_utils:notify_warning( Elements, Context ).
