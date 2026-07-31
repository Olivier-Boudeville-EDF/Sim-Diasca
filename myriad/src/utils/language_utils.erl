% Copyright (C) 2017-2026 Olivier Boudeville
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

-module(language_utils).

-moduledoc """
Gathering of various convenient facilities regarding the **support of various
communication (typically for user interfaces) or programming-related
languages**.

See the `language_utils_test` module for the corresponding test.
""".



% For human languages:


-doc """
Type to designate some human languages of interest.

See also [the ISO 639-3 Codes for the representation of names of
languages](https://en.wikipedia.org/wiki/ISO_639-3).

Note that a locale is a broader notion.
""".
-type human_language() :: 'english' | 'french' | 'spanish' | 'german'
                        | 'italian' | 'russian' | 'chinese' | 'japanese'.




% For programming languages:


-doc "Type to designate all programming languages of interest.".
-type language() :: 'erlang' | 'python' | 'c' | 'c++' | 'java' | 'rust'.


-doc """
Type to designate an (Erlang) process driving a runtime container (e.g. a Python
interpreter or a Java virtual machine) of a given language runtime (typically on
a given node).
""".
-type runtime_container_pid() :: pid().


-doc "The PID of a Python interpreter runtime container.".
-type python_interpreter_container_pid() :: python_utils:interpreter_pid().


-doc """
The PID of a Java virtual machine runtime container (e.g. it can be a binding
agent, otherwise directly a controller mbox).
""".
-type java_vm_container_pid() :: runtime_container_pid().


-export_type([ human_language/0, language/0,
               runtime_container_pid/0, python_interpreter_container_pid/0,
               java_vm_container_pid/0 ]).


% For human languages:
-export([ get_user_language/0, set_user_language/1,
          get_file_path/1, get_file_path/2 ]).



% For programming languages:
-export([ get_supported_foreign_languages/0, get_supported_languages/0,
          get_additional_beam_directories_for/1,
          language_to_string/1, language_to_string/2 ]).



% Type shorthands:

-type ustring() :: text_utils:ustring().

-type any_file_path() :: file_utils:any_file_path().
-type file_path() :: file_utils:file_path().
-type abs_file_path() :: file_utils:abs_file_path().
-type possibly_resolvable_path() :: file_utils:possibly_resolvable_path().



% Section for (human) communication languages.

% To ease the management of an application, user-level language, one may rely on
% a persistent_term (therefore application-global), rather than on a
% (process-level) process dictionary or even on a function-scoped variable for
% language settings, which may cumbersome to pass around.
%
% To define easily that the English version of a file shall be the default one
% should no language-specific one be defined, just create a symbolic link; for
% example from conf/my-file.txt to conf/my-file-english.txt.


% Key to be used for a persistent term:
-define( myriad_user_language_key, myriad_user_language ).



-doc """
Returns the current setting in terms of application, user-level language,
typically so that any interface can adapt.
""".
-spec get_user_language() -> human_language().
 get_user_language() ->
    persistent_term:get( _K=?myriad_user_language_key, _Default=english ).


-doc """
Sets globally the current setting in terms of application, user-level language.

Defines which human language shall be used by default for user interaction.

Various interface-related functions (in Myriad or in the layers above) should
read this setting and act accordingly.

Changing such a setting in the course of execution may be expensive if many
processes exist.
""".
-spec set_user_language( human_language() ) -> persistent_term:info().
set_user_language( NewLanguage ) when is_atom( NewLanguage )->

    %io:format( "Setting application language to ~ts (was: ~ts).~n",
    %           [ NewLanguage, get_user_language() ] ),

    persistent_term:put( _K=?myriad_user_language_key, _V=NewLanguage ).



-doc """
Returns the most suitable version, based on the current user language, of the
specified file path.

For example, if given `conf/my-file.txt`, may return `conf/my-file-french.txt`
if `french` is the current human language and if this file exists, otherwise
will return as a fallback `conf/my-file.txt` if it exists, otherwise will throw
an exception.
""".
-spec get_file_path( any_file_path() ) -> file_path().
get_file_path( AnyFilePath ) ->
    get_file_path( AnyFilePath, get_user_language() ).



-doc """
Returns the most suitable existing version, based on the specified user
language, of the specified file path.

For example, if `FilePath=conf/my-file.txt` and `Language=french`, may return
`/.../conf/my-file-french.txt` if this file exists, otherwise will return as a
fallback `/.../conf/my-file.txt` if it exists, otherwise will throw an
exception.
""".
-spec get_file_path( any_file_path(), human_language() ) -> abs_file_path().
get_file_path( BinFilePath, Language ) when is_binary( BinFilePath )->
    get_file_path( text_utils:binary_to_string( BinFilePath ), Language );

get_file_path( FilePath, Language ) ->

    { PfxPath, Ext } = file_utils:split_extension( FilePath ),

    AbsLangFilePath = file_utils:ensure_path_is_absolute(
        text_utils:format( "~ts-~ts.~ts",
                           [ PfxPath, Language, Ext ] ) ),

    case file_utils:is_existing_file_or_link( AbsLangFilePath ) of

        true ->
            AbsLangFilePath;

        false ->
            AbsFilePath = file_utils:ensure_path_is_absolute( FilePath ),
            case file_utils:is_existing_file_or_link( AbsFilePath ) of

                true ->
                    trace_bridge:warning_fmt( "No '~ts' version found "
                        "for '~ts', falling back to ~ts.",
                        [ Language, AbsLangFilePath, AbsFilePath ] ),

                    AbsFilePath;

                false ->
                    throw( { no_version_for, FilePath, Language,
                           { AbsLangFilePath, AbsFilePath } } )

            end

    end.





% Section for programming languages.


-doc """
Returns a list of the supported foreign (non-native, meaning non-Erlang)
programming languages.
""".
-spec get_supported_foreign_languages() -> [ language() ].
get_supported_foreign_languages() ->
    [ python, java ].



-doc """
Returns a list of all supported programming languages (including Erlang).
""".
-spec get_supported_languages() -> [ language() ].
get_supported_languages() ->
    [ erlang | get_supported_foreign_languages() ].



-doc "Returns a textual description of the specified language.".
-spec language_to_string( language() ) -> ustring().
language_to_string( Language ) ->
    language_to_string( Language, _IndentationLevel=0 ).



-doc """
Returns an indented textual description of the specified language.
""".
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



-doc """
Returns the BEAM locations of all the dependencies related to the specified
language bindings.
""".
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
