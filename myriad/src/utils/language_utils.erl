% Copyright (C) 2017-2024 Olivier Boudeville
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
% Creation date: Saturday, July 12, 2017.


% @doc Gathering of various convenient facilities regarding the <b>support of
% various programming or communication languages</b>.
%
% See language_utils_test.erl for the corresponding test.
%
-module(language_utils).


-type language() :: 'erlang' | 'python' | 'java'.
% Type to designate all known programming languages.


-type human_language() :: 'english' | 'french' | 'spanish' | 'german'
						| 'italian' | 'russian' | 'chinese' | 'japanese'.
% Type to designate all supported human languages.


-type runtime_container_pid() :: pid().
% Type to designate an (Erlang) process driving a runtime container (e.g. a
% Python interpreter or a Java virtual machine) of a given language runtime
% (typically on a given node).


-type python_interpreter_container_pid() :: python_utils:interpreter_pid().
% The PID of a Python interpreter runtime container.


-type java_vm_container_pid() :: runtime_container_pid().
% The PID of a Java virtual machine runtime container (e.g. it can be a binding
% agent, otherwise directly a controller mbox).


-export_type([ language/0, human_language/0,
			   runtime_container_pid/0, python_interpreter_container_pid/0,
			   java_vm_container_pid/0 ]).



-export([ get_supported_foreign_languages/0, get_supported_languages/0,
		  language_to_string/1, language_to_string/2,
		  get_additional_beam_directories_for/1 ]).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type possibly_resolvable_path() :: file_utils:possibly_resolvable_path().



% @doc Returns a list of the supported foreign (non-native, meaning non-Erlang)
% programming languages.
%
-spec get_supported_foreign_languages() -> [ language() ].
get_supported_foreign_languages() ->
	[ python, java ].



% @doc Returns a list of all supported programming languages (including Erlang).
-spec get_supported_languages() -> [ language() ].
get_supported_languages() ->
	[ erlang | get_supported_foreign_languages() ].



% @doc Returns a textual description of the specified language.
-spec language_to_string( language() ) -> ustring().
language_to_string( Language ) ->
	language_to_string( Language, _IndentationLevel=0 ).


% @doc Returns an indented textual description of the specified language.
-spec language_to_string( language(), text_utils:indentation_level() ) ->
											ustring().
language_to_string( erlang, _IndentationLevel ) ->
	"Erlang";

language_to_string( python, _IndentationLevel ) ->
	"Python";

language_to_string( java, _IndentationLevel ) ->
	"Java";

language_to_string( Language, _IndentationLevel ) when is_atom( Language ) ->
	throw( { unknown_language, Language } );

language_to_string( { Language, CodePath }, IndentationLevel )
						when is_atom( Language ) andalso is_list( CodePath ) ->
	text_utils:format( "~ts, with following code path: ~ts",
		[ language_to_string( Language ),
		  text_utils:strings_to_string( CodePath, IndentationLevel+1 ) ] );

language_to_string( LanguageInvalidArg, _IndentationLevel ) ->
	throw( { invalid_language_specification, LanguageInvalidArg } ).



% @doc Returns the BEAM locations of all the dependencies related to the
% specified language bindings.
%
-spec get_additional_beam_directories_for( [ language() ] ) ->
												[ possibly_resolvable_path() ].
get_additional_beam_directories_for( Languages ) ->
	get_additional_beam_directories_for( Languages, _Acc=[] ).


% (helper)
get_additional_beam_directories_for( _Languages=[], Acc ) ->
	lists:reverse( Acc );


get_additional_beam_directories_for( _Languages=[ erlang | T ], Acc ) ->
	% Erlang natively supported "as is":
	get_additional_beam_directories_for( T, Acc );


get_additional_beam_directories_for( _Languages=[ python | T ], Acc ) ->

	% Finds the BEAM directories of the Python-specific dependencies :
	NewBeamDirs = python_utils:get_beam_directories_for_binding(),

	% Adds them to list of dependencies and goes for the next language:
	get_additional_beam_directories_for( T, NewBeamDirs ++ Acc );


get_additional_beam_directories_for( _Languages=[ java | T ], Acc ) ->

	% Finds the BEAM directories of the Java-specific dependencies :
	NewBeamDirs = java_utils:get_beam_directories_for_binding(),

	% Adds them to list of dependencies and goes for the next language:
	get_additional_beam_directories_for( T, NewBeamDirs ++ Acc ).
