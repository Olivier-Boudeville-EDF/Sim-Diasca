% Copyright (C) 2015-2021 Olivier Boudeville
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

% Creation date: Tuesday, May 12, 2015

% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]




% This module allows to generate a read-only associative table whose key/value
% pairs can be read from any number (potentially extremely large) of readers
% very efficiently (possibly the most efficient way in Erlang).
%
% No ETS table, replication (ex: per-user table copy) or message sending is
% involved: thanks to meta-programming, a module is generated on-the-fly,
% exporting as many functions as there are different keys in the table of
% interest; calling a function corresponding to a key returns the associated
% value.
%
% More precisely, a module name (ex: 'foobar') and a table:table( atom(), any()
% ) are provided to const_table:generate/2; for each key/value pair in the
% specified table (ex: { 'foo', 42.0 }), a 0-arity function is generated and
% exported in that module, as if we had:
%
% -module(foobar).
%
% [...]
%
% -export( [ foo/0 ] ).
%
% -spec foo() -> term().
% foo() ->
%    42.0.
%
% Then third-party code can call for example 'foobar:foo()' and have 42.0
% returned. This is presumably the most efficient way of sharing constants in
% Erlang.
%
% Keys must be atoms, and the table of interest shall be immutable (const), even
% if, thanks to hot code upgrade, one may imagine updating the table at
% will, having any number of successive versions of it.

% However generating a table of the same name more than once should be done with
% care, as if a given table is generated three times, the initial table would
% become 'current', then 'old', and then be removed. Any process that would
% linger in it would then be terminated (see
% http://www.erlang.org/doc/reference_manual/code_loading.html). However, due to
% the nature of these tables (just one-shot fully-qualified calls, no recursion
% or message-waiting construct), this is not expected to happen.
%
-module(const_table).


-export([ generate/2 ]).


% Generates and loads a pseudo-module sharing the entries of specified table by
% exporting as many functions named according to the keys and returning the
% value corresponding to the selected key.
%
% Note that no actual module file is generated (ex: no 'foobar.beam'), the
% operation remains fully in-memory.
%
-spec generate( basic_utils:module_name(), table:table() ) -> void().
generate( ModuleName, Table ) ->

	%io:format( "Generating pseudo-module '~ts' from following table:~n~ts~n",
	%		   [ ModuleName, table:to_string( Table ) ] ),

	% Just a name, not any actual file:
	PseudoModuleFilename = "const_table_generated_"
		++ text_utils:atom_to_string( ModuleName ) ++ ".beam",

	Forms = generate_forms( ModuleName, table:enumerate( Table ) ),
	%io:format( "Generated forms:~p~n", [ Forms ] ),

	% We want all look-up key-based function to be exported:
	%
	CompileOpts = [ binary, verbose,report_errors,report_warnings,
					warnings_as_errors, export_all ],

	BinaryObjectCode = case compile:forms( Forms, CompileOpts ) of

			% Matches the module name:
			{ ok, ModuleName, BinaryOrCode } ->
				BinaryOrCode;

			Error ->
				throw( { generation_failed, ModuleName, Error } )

	end,

	code:load_binary( ModuleName, PseudoModuleFilename, BinaryObjectCode ).

	% Contains for example '{foobar,"const_table_generated_foobar.beam"}':
	%
	%io:format( "Loaded modules:~n~p~n", [ code:all_loaded() ] ),

	% We loaded this new module also as otherwise any previous various version
	% of it would still be used instead.



% Generates the forms corresponding to specified entries and module.
generate_forms( ModuleName, Entries ) ->

	Line = 0,

	% Preferably ends with end of file:
	FunForms = generate_fun_forms( Entries, Line, [ { eof, Line } ] ),

	[ { attribute, Line, module, ModuleName } | FunForms ].



% Generates the forms corresponding to 'foo() -> 42.0':
generate_fun_forms( _Entries=[], _Line, AccForms ) ->
	AccForms;


generate_fun_forms( _Entries=[ { K, V } | T ], Line, AccForms )
  when is_atom( K ) ->

	% We have here to generate a function K/0 returning a constant V (ex:
	% V=42.0):

	% Ex: returns '{float,0,42.0}' (as a term):
	%
	ASTForV = ast_utils:term_to_form( V ),

	FunForm = { function, Line, K, 0, [ { clause, Line, [], [],
								  [ ASTForV ] } ] },

	generate_fun_forms( T,  Line, [ FunForm | AccForms ] );


generate_fun_forms( _Entries=[ { K, _V } | _T ], _Line, _AccForms ) ->
	throw( { non_atom_key, K } ).
