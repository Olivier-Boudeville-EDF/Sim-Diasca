% Copyright (C) 2015-2024 Olivier Boudeville
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
% Creation date: Tuesday, May 12, 2015.


% @doc This module allows to generate <b>read-only associative tables whose
% key/value pairs can be read from any number (potentially extremely large) of
% readers very efficiently</b> (possibly the most efficient way in Erlang).
%
% These key/value pairs can be decided at runtime, from any source; keys must be
% atoms while values can be of any permanent (non-transient) type (and two
% values in a table do not have to be of the same type). Using a transient type
% is bound to result in a badarg.
%
% These tables may be kept in-memory only (hence with the corresponding modules
% being generated and used at runtime) and/or be generated and stored in an
% actual BEAM file, for a later direct (re)loading thereof.
%
% No ETS table, replication (e.g. per-user table copy) or message sending is
% involved: thanks to meta-programming, a module is generated on-the-fly,
% exporting as many functions as there are different keys in the entries of
% interest; calling a function corresponding to a key returns the associated
% value.
%
% More precisely, a module name (e.g. 'foobar') and a list of `{atom(), any()}'
% entries are provided to the `const_table:generate*/*' functions; for each
% key/value pair in the specified entries (e.g. `{'baz', 42.0}'), a 0-arity
% function is generated and exported in that module, as if we had:```
%
% -module(foobar).
%
% [...]
%
% -export([baz/0]).
%
% -spec baz() -> term().
% baz() ->
%    42.0.'''
%
% Then third-party code can call for example `foobar:baz()' and have `42.0'
% returned. This is presumably the most efficient way of sharing constants in
% Erlang.
%
% Keys must be atoms (as they will correspond to function names), and the
% resulting table is immutable (const), even if, thanks to hot code upgrade, one
% may imagine updating the table at will, having any number of successive
% versions of it.
%
% However generating a table of the same name more than once should be done with
% care, as if a given table is generated three times (hence updated twice), the
% initial table would become 'current', then 'old', and then be removed. Any
% process that would linger in it would then be terminated (see
% [http://www.erlang.org/doc/reference_manual/code_loading.html]). However, due
% to the nature of these tables (just one-shot fully-qualified calls, no
% recursion or message-waiting construct), this is not expected to happen.
%
% Refer to:
% - const_table_test.erl for an usage example and testing thereof
% - map_hashtable.erl for a runtime, mutable, term-based table
% - const_bijective_table.erl for a constant, two-way (bijective) table
%
-module(const_table).


% User API:
-export([ generate_in_memory/2, generate_in_file/2, generate_in_file/3 ]).


-type key() :: permanent_term().
% Designates the keys of the table.
% A module-based storage cannot hold transient terms.

-type value() :: permanent_term().
% Designates the values of the table.
% A module-based storage cannot hold transient terms.


-type entry() :: { key(), value() }.
% An entry to be fed to a const-table.

-type entries() :: [ entry() ].
% Entries to be fed to a const-table.

-export_type([ key/0, value/0, entry/0, entries/0 ]).


% Implementation notes:
%
% We of course expect direct function calls to be both quicker and more compact
% in memory than using any table in the generated module, for all numbers of
% entries.
%
% TO-DO: add a corresponding type specification (see ast_function).


% Shorthands:

-type module_name() :: basic_utils:module_name().

-type file_name() :: file_utils:file_name().
-type any_directory_path() :: file_utils:any_directory_path().

-type permanent_term() :: type_utils:permanent_term().


% @doc Generates in memory (only) and loads a module sharing the specified
% entries by exporting as many functions named according to the keys, and
% returning the value corresponding to the selected key.
%
% Note that no actual module file is generated (e.g. no 'foobar.beam'), the
% operation remains fully in-memory.
%
-spec generate_in_memory( module_name(), entries() ) -> void().
generate_in_memory( ModuleName, Entries ) ->

	cond_utils:if_defined( myriad_debug_code_generation,
		trace_utils:debug_fmt( "Generating pseudo-module '~ts' from following "
			"entries:~n~ts",
			[ ModuleName, list_table:to_string( Entries ) ] ) ),

	% Just a name here, not designating any actual file:
	ModulePseudoFilename = get_generated_beam_filename_for( ModuleName ),

	Forms = generate_forms( ModuleName, Entries ),
	%trace_utils:debug_fmt( "Generated forms:~p", [ Forms ] ),

	% Not wanting an actual file:
	CompileOpts = [ binary | meta_utils:get_compile_base_opts() ],

	BinaryObjectCode = case compile:forms( Forms, CompileOpts ) of

		% Matches the module name:
		{ ok, ModuleName, Binary } ->
			Binary;

		Error ->
			throw( { module_generation_failed, ModuleName, Error } )

	end,

	code:load_binary( ModuleName, ModulePseudoFilename, BinaryObjectCode ).

	% Contains for example '{foobar,"const_table_generated_foobar.beam"}':
	%
	%trace_utils:debug_fmt( "Loaded modules:~n~p", [ code:all_loaded() ] ),

	% We loaded this new module also as otherwise any previous various version
	% of it would still be used instead.



% @doc Generates in-file (a BEAM file created in the current directory) and
% loads a module sharing the specified entries by exporting as many functions
% named according to the keys, and returning the value corresponding to the
% selected key.
%
% Returns the generated filename (not path), for any further reference.
%
-spec generate_in_file( module_name(), entries() ) -> file_name().
generate_in_file( ModuleName, Entries ) ->
	generate_in_file( ModuleName, Entries,
					  file_utils:get_current_directory() ).


% @doc Generates in-file (a BEAM file created in the specified directory) a
% module sharing the specified entries by exporting as many functions named
% according to the keys, and returning the value corresponding to the selected
% key.
%
% For a clearer setting, generated modules may be named as such
% (e.g. 'foobar_generated').
%
% The resulting module is not loaded by this function.
%
% Returns the generated filename (not path), for any further reference.
%
-spec generate_in_file( module_name(), entries(), any_directory_path() ) ->
														file_name().
generate_in_file( ModuleName, Entries, TargetDir ) ->

	file_utils:is_existing_directory_or_link( TargetDir ) orelse
		throw( { non_existing_output_directory, TargetDir } ),

	ModuleFilename = get_generated_beam_filename_for( ModuleName ),

	cond_utils:if_defined( myriad_debug_code_generation,
		trace_utils:debug_fmt( "Generating module '~ts' in file '~ts', in the "
			"'~ts' directory, for ~ts.", [ ModuleName, ModuleFilename,
				TargetDir, list_table:to_string( Entries ) ] ) ),

	Forms = generate_forms( ModuleName, Entries ),
	%trace_utils:debug_fmt( "Generated forms:~p", [ Forms ] ),

	CompileOpts =
		[ { outdir, TargetDir } | meta_utils:get_compile_base_opts() ],

	BinaryObjectCode = case compile:forms( Forms, CompileOpts ) of

		% Matches the module name; apparently 'binary' is implicit and thus no
		% file is written:
		%
		{ ok, ModuleName, Binary } ->
			Binary;

		Error ->
			throw( { module_generation_failed, ModuleName, Error } )

	end,

	% So we do it by ourselves:
	TargetFilePath = file_utils:join( TargetDir, ModuleFilename ),
	file_utils:write_whole( TargetFilePath, BinaryObjectCode ),

	cond_utils:if_defined( myriad_check_code_generation,
		file_utils:is_existing_file( TargetFilePath ) orelse
			throw( { no_module_file_generated, TargetFilePath } ) ),

	ModuleFilename.



% Helper section.



% @doc Returns a filename corresponding to the specified BEAM module to be
% generated.
%
-spec get_generated_beam_filename_for( module_name() ) -> file_name().
get_generated_beam_filename_for( ModName ) ->

	% Clearer, but longer, and anyway the runtime will expect ModName, not
	% another atom:
	%
	%"const_table_generated_" ++ code_utils:get_beam_filename( ModName ).
	code_utils:get_beam_filename( ModName ).




% Generates the forms corresponding to the specified entries and module.
generate_forms( ModuleName, Entries ) ->

	Line = 0,

	% We prefer defining the entries in their specified order; preferably ends
	% with end of file:
	%
	% (refer to https://www.erlang.org/doc/apps/erts/absform.html)
	%
	FunForms = generate_fun_forms( lists:reverse( Entries ), Line,
								   _Acc=[ { eof, Line } ] ),

	[ { attribute, Line, module, ModuleName } | FunForms ].



% Generates the forms corresponding to 'foo() -> 42.0':
generate_fun_forms( _Entries=[], _Line, AccForms ) ->
	AccForms;


generate_fun_forms( _Entries=[ { K, V } | T ], Line, AccForms )
												when is_atom( K ) ->

	% We have here to generate a function K/0 returning a constant V (e.g.
	% V=42.0):

	% For example returns '{float,0,42.0}' (as a term):
	ASTForV = ast_utils:term_to_form( V ),

	FunForm = { function, Line, K, _Arity=0,
				[ { clause, Line, _PatternSeq=[], _GuardSeq=[],
					_Body=[ ASTForV ] } ] },

	generate_fun_forms( T,  Line, [ FunForm | AccForms ] );


generate_fun_forms( _Entries=[ { K, _V } | _T ], _Line, _AccForms ) ->
	throw( { non_atom_key, K } ).
