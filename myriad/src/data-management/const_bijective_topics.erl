% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Wednesday, September 14, 2022.


% @doc This module allows to generate <b>modules sharing read-only, per-topic
% two-way (bijective) associative tables whose stored pairs can be read from any
% number (potentially extremely large) of readers very efficiently</b> (possibly
% the most efficient way in Erlang), for each topic.
%
% Such modules gathering topic-based conversion tables are especially useful
% when implementing conversions between external constants (e.g. the ones of a
% graphical interface, or of a protocol) and ones defined by the current program
% (possibly as convenient atoms), moreover with a low overhead. Consider relying
% on the const_bijective_table module is a single (possibly large) table is to
% be shared.
%
% Any number of topics (e.g. 'color', 'reference', 'city') can be declared,
% whose pairs can be decided at runtime, from any source. Their elements can be
% of any permanent (non-transient) type (not atoms only; the first elements can
% be heterogeneous, they do not have to be of the same type, and the same of
% course applies for the second elements). Using a transient type is bound to
% result in a badarg. As two-way conversions are to be performed, for a given
% set of values (first ones or second ones), there must be no duplicated
% elements (otherwise no bijectivity can exist).
%
% These tables may be kept in-memory only (hence with the corresponding modules
% being generated and used at runtime) and/or be generated and stored in an
% actual BEAM file, for a single generation and later direct (re)loading(s)
% thereof.
%
% No ETS table, replication (ex: per-user table copy) or message sending is
% involved: thanks to metaprogramming, a module is generated on-the-fly,
% exporting two functions designed to access either of the elements of the
% entries of interest.
%
% More precisely, a module name (ex: 'foobar'), a list of topics (atoms)
% together with their respective list of `{any(), any()}' entries must be
% provided to the `const_bijective_topics:generate*/*' functions; for any given
% topic (e.g. 'packet_type'), any element E of any pair in the resulting table
% (e.g. 'discover_packet') in `{'discover_packet', <<42,11>>}') can then be
% accessed thanks to foobar:get_{first|second}_for_TOPIC/1 (e.g. discover_packet
% = foobar:get_first_for_packet_type(<<42,11>>)).
%
% For that, for each topic, two corresponding 1-arity functions are generated
% and exported in that module, as if we were using a bijective_table()
% underneath.
%
% No restriction applies to the elements stored (notably they may be all of
% different types), except, at the leve of each topicn the absence of duplicates
% between its first values and between its second ones.
%
% This is presumably the most efficient way of sharing constants in Erlang.
%
% However generating a module of the same name more than once should be done
% with care, as if a given module is generated three times (hence updated/loaded
% twice), the initial module would become 'current', then 'old', and then be
% removed. Any process that would linger in it would then be terminated (see
% [http://www.erlang.org/doc/reference_manual/code_loading.html]). However, due
% to the nature of these modules (just one-shot fully-qualified calls, no
% recursion or message-waiting construct), this is not expected to happen.
%
% Refer to:
% - const_bijective_topics_test.erl for an usage example and testing thereof
% - const_bijective_table.erl for a "single-topic" constant bijective table
% - bijective_table.erl for a runtime, mutable, term-based bijective table
% - const_table.erl, for a constant, "simple" (oneway) associative table
%
-module(const_bijective_topics).


% For example, we may want to support, thanks to a 'foobar' generated module,
% three topic-based bijective tables, namely:
%
% - 'identifier', whose first elements are of type my_id(), and second ones
% buz_id()
%
% - 'packet_type', whose first elements are of type my_type(), and second ones
% buz_type()
%
% - 'color', whose first elements are of type color_by_name(), and second ones
% color_by_rgb()
%
% Then the 'foobar' generated module will include three pairs (one per topic) of
% corresponding functions:
%
% - for topic 'identifier':
%     -spec get_first_for_identifier( buz_id() ) -> my_id().
%     -spec get_second_for_identifier( my_id() ) -> buz_id().
%
% - for topic 'packet_type':
%     -spec get_first_for_packet_type( buz_type() ) -> my_type().
%     -spec get_second_for_packet_type( my_type() ) -> buz_type().
%
% - for topic 'color':
%     -spec get_first_for_color( color_by_rgb() ) -> color_by_name().
%     -spec get_second_for_color( color_by_name() ) -> color_by_rgb().


-export([ generate_in_memory/2, generate_in_file/2, generate_in_file/3 ]).


% For re-used in other modules (e.g. const_bijective_table):
-export([ generate_header_form/2, generate_footer_form/1, generate_forms/4,
		  generate_strict_calling_clauses/4,
		  generate_first_clauses/3, generate_second_clauses/3 ]).


-type topic_name() :: atom().
% A topic name, designating a specific table shared by a generated module.
% Example of such topic names: colour, bar_identifier, font_style.
%
% These atom should be acceptable suffixes to a function name (e.g. not
% including spaces, dashes, etc.).



-type first_type() :: permanent_term().
% Designates the first elements of the table pairs.
% A module-based storage cannot hold transient terms.

-type second_type() :: permanent_term().
% Designates the second elements of the table pairs.
% A module-based storage cannot hold transient terms.


-type entry() :: { first_type(), second_type() }.
% An entry to be fed to a const-bijective table.

-type entries() :: [ entry() ].
% Entries that can be fed to a const-bijective table.


-type element_lookup() :: 'strict' % Throws an exception if element not found
						| 'maybe'. % Returns 'undefined' if element not found
% Tells how elements shall be looked up.
%
% Note that selecting the 'maybe' element look-up is not recommended if either
% of the first and second sets contains the 'undefined' atom, as it leads to
% ambiguity.



-type topic_spec() :: { topic_name(), entries() }
					| { topic_name(), entries(), element_lookup() }.
% The specified of a topic: name, entries and type of element look-up.
%
% The 'strict' element look-up is the default one.


-export_type([ topic_name/0, first_type/0, second_type/0, entry/0, entries/0,
			   element_lookup/0, topic_spec/0 ]).



% Implementation notes:
%
% We suppose pattern-matched function calls (going through potentially many
% 'foobar:get_first_for(Sn) -> Fn;' clauses) to be both quicker and more compact
% / less duplicated in memory than using any table in the generated module, for
% all numbers of entries, but as long as not tested this remains an assumption.

% For each topic T it can be defined how elements shall be looked up:
%
%  - strict: only get_{first,second}_for_T/1 are defined, throwing directly an
%  exception if an element is not found
%
%  - maybe: get_maybe_{first,second}_for_T/1 are defined, returning 'undefined'
%  if an element is not found, and get_{first,second}_for_T/1 are defined from
%  them, throwing an exception if an element is not found
%
% (therefore the 'maybe' element look-up implies the 'strict' one)


% Shorthands:

-type module_name() :: basic_utils:module_name().
-type error_type() :: basic_utils:error_type().

-type file_name() :: file_utils:file_name().
-type any_directory_path() :: file_utils:any_directory_path().

-type permanent_term() :: type_utils:permanent_term().

-type form() :: ast_base:form().

-type file_loc() :: ast_base:file_loc().



% @doc Generates in memory (only) and loads a module sharing bijectively the
% specified entries for the specified topic by exporting suitably-generated
% get_first_for_TOPIC/1 and get_second_for_TOPIC/1 functions in order to access
% either element of the recorded pairs.
%
% Note that no actual module file is generated (ex: no 'foobar.beam'), the
% operation remains fully in-memory.
%
-spec generate_in_memory( module_name(), [ topic_spec() ] ) -> void().
generate_in_memory( ModuleName, TopicSpecs ) ->

	cond_utils:if_defined( myriad_debug_code_generation,
		% list_table cannot be used, as "keys" (first) are not necessarily
		% atoms:
		%
		trace_utils:debug_fmt( "Generating pseudo-module '~ts' from following "
			"topic specs:~n ~p", [ ModuleName, TopicSpecs ] ) ),

	% Just a name here, not designating any actual file:
	ModulePseudoFilename = get_generated_beam_filename_for( ModuleName ),

	Forms = generate_topic_forms( ModuleName, TopicSpecs ),

	%trace_utils:debug_fmt( "Generated forms:~n ~p", [ Forms ] ),

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

	% Contains for example '{foobar,
	% "const_bijective_table_generated_foobar.beam"}':
	%
	%trace_utils:debug_fmt( "Loaded modules:~n~p", [ code:all_loaded() ] ),

	% We loaded this new module also, as otherwise any previous various version
	% of it would still be used instead.



% @doc Generates in-file (a BEAM file created in the current directory) a module
% sharing the specified entries by exporting suitably-generated get_first_for/1
% and get_second_for/1 functions in order to access either element of the
% recorded pairs.
%
% For a clearer setting, generated modules may be named as such
% (e.g. 'foobar_generated').
%
% The resulting module is not loaded by this function.
%
% Returns the generated filename (not path), for any further reference.
%
-spec generate_in_file( module_name(), entries() ) -> file_name().
generate_in_file( ModuleName, Entries ) ->
	generate_in_file( ModuleName, Entries,
					  file_utils:get_current_directory() ).


% @doc Generates in-file (a BEAM file created in the specified directory) a
% module sharing the specified entries by exporting suitably-generated
% get_first_for/1 and get_second_for/1 functions in order to access either
% element of the recorded pairs.
%
% For a clearer setting, generated modules may be named as such
% (e.g. 'foobar_generated').
%
% The resulting module is not loaded by this function.
%
% Returns the generated filename (not path), for any further reference.
%
-spec generate_in_file( module_name(), [ topic_spec() ],
						any_directory_path() ) -> file_name().
generate_in_file( ModuleName, TopicSpecs, TargetDir ) ->

	file_utils:is_existing_directory_or_link( TargetDir ) orelse
		throw( { non_existing_output_directory, TargetDir } ),

	ModuleFilename = get_generated_beam_filename_for( ModuleName ),

	cond_utils:if_defined( myriad_debug_code_generation,
		% list_table cannot be used, as "keys" (first) are not necessarily
		% atoms:
		%
		trace_utils:debug_fmt( "Generating module '~ts' in file '~ts', in the "
			"'~ts' directory, for following topics and entries:~n ~p.",
			[ ModuleName, ModuleFilename, TargetDir, TopicSpecs ] ) ),

	Forms = generate_topic_forms( ModuleName, TopicSpecs ),

	%trace_utils:debug_fmt( "Generated forms:~n ~p", [ Forms ] ),

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
	%"const_bijective_table_generated_"
	%    ++ code_utils:get_beam_filename( ModName ).

	code_utils:get_beam_filename( ModName ).



% @doc Returns the header forms corresponding to the specified module, declared
% at the specified file location.
%
-spec generate_header_form( module_name(), file_loc() ) -> form().
generate_header_form( ModuleName, FileLoc ) ->
	{ attribute, FileLoc, module, ModuleName }.


% @doc Returns suitable footer forms.
-spec generate_footer_form( file_loc() ) -> form().
generate_footer_form( FileLoc ) ->
	{ eof, FileLoc }.



% @doc Generates the forms corresponding to the specified topic.
-spec generate_topic_forms( module_name(), [ topic_spec() ] ) -> [ form() ].
generate_topic_forms( ModuleName, TopicSpecs ) ->

	FileLoc = ast_utils:get_generated_code_location(),

	Topics = type_utils:check_atoms(
				[ element( _Index=1, TS ) || TS <- TopicSpecs ] ),

	case list_utils:get_duplicates( Topics ) of

		[] ->
			ok;

		Dups ->
			throw( { duplicated_bijective_topics, Dups } )

	end,

	CanonicalTopicSpecs = [ case TS of

								{ T, E } ->
									{ T, E, _DefaultLookup=strict };

								Triplet={ _T, _E, _LU } ->
									Triplet

							end || TS <- TopicSpecs ],

	% Now we add an initial '_' before each topic (e.g. 'color' becomes
	% '_color') so that const-bijective table can request generate_forms/4 to
	% generate for example 'get_first_for', not 'get_first_for_':
	%
	[ generate_header_form( ModuleName, FileLoc ) | list_utils:flatten_once(
		[ generate_forms( TP, ET, LU, FileLoc )
							|| { TP, ET, LU } <- CanonicalTopicSpecs ] ) ]
		++ [ generate_footer_form( FileLoc ) ].



% @doc Generates the forms corresponding to the specified first/second function
% names, entries and module, depending on the specified look-up.
%
-spec generate_forms( topic_name(), entries(), element_lookup(), file_loc() ) ->
								[ form() ].
generate_forms( TopicName, Entries, _ElementLookup=strict, FileLoc ) ->

	% We prefer defining get_first_for_TOPIC/1 then get_second_for_TOPIC/1, and
	% respecting the order of the specified entries; preferably ends with end of
	% file:
	%
	% (refer to https://www.erlang.org/doc/apps/erts/absform.html)

	FirstFunName = text_utils:atom_format( "get_first_for_~ts", [ TopicName ] ),

	SecondFunName = text_utils:atom_format( "get_second_for_~ts",
											[ TopicName ] ),

	RevEntries = lists:reverse( Entries ),

	FirstFunForm = generate_strict_fun_form_for_first( RevEntries, FirstFunName,
													   TopicName, FileLoc ),

	SecondFunForm = generate_strict_fun_form_for_second( RevEntries,
						SecondFunName, TopicName, FileLoc ),

	[ FirstFunForm, SecondFunForm ];


generate_forms( TopicName, Entries, _ElementLookup=maybe, FileLoc ) ->

	RevEntries = lists:reverse( Entries ),

	FirstFunForms =
		generate_maybe_fun_forms_for_first( RevEntries, TopicName, FileLoc ),

	%trace_utils:debug_fmt( "Generated first maybe-form for topic '~ts':~n ~p",
	%                       [ TopicName, FirstFunForms ] ),

	SecondFunForms =
		generate_maybe_fun_forms_for_second( RevEntries, TopicName, FileLoc ),

	FirstFunForms ++ SecondFunForms.



% Generates the strict form corresponding to foobar:FirstFunName/1.
generate_strict_fun_form_for_first( Entries, FirstFunName, TopicName,
									FileLoc ) ->

	% We have here to generate first the 'foobar:get_first_for_TOPIC(Sn) -> Fn;'
	% clauses:
	%
	Clauses = generate_first_clauses( Entries, FileLoc,
		_Acc=[ catch_all_clause( second_not_found, TopicName, _Lookup=strict,
								 FileLoc ) ] ),

	{ function, FileLoc, FirstFunName, _Arity=1, Clauses }.


% (helper)
generate_first_clauses( _Entries=[], _FileLoc, Acc ) ->
	% Already reversed:
	Acc;

generate_first_clauses( _Entries=[ _E={ F, S } | T ], FileLoc, Acc ) ->

	ASTForF = ast_utils:term_to_form( F ),
	ASTForS = ast_utils:term_to_form( S ),

	NewAcc = [ { clause, FileLoc, _PatternSeq=[ ASTForS ], _GuardSeq=[],
				 _Body=[ ASTForF ] } | Acc ],

	generate_first_clauses( T, FileLoc, NewAcc ).


% Generates the strict form corresponding to foobar:SecondFunName/1.
generate_strict_fun_form_for_second( Entries, SecondFunName, TopicName,
									 FileLoc ) ->

	% We have here to generate second the
	% 'foobar:get_second_for_TOPIC(Sn) -> Fn;' clauses:
	%
	Clauses = generate_second_clauses( Entries, FileLoc,
		_Acc=[ catch_all_clause( first_not_found, TopicName, _Lookup=strict,
								 FileLoc ) ] ),

	{ function, FileLoc, SecondFunName, _Arity=1, Clauses }.



% (helper)
generate_second_clauses( _Entries=[], _FileLoc, Acc ) ->
	% Already reversed:
	Acc;

generate_second_clauses( _Entries=[ _E={ F, S } | T ], FileLoc, Acc ) ->

	ASTForF = ast_utils:term_to_form( F ),
	ASTForS = ast_utils:term_to_form( S ),

	% Note the F/S swapping compared to generate_first_clauses/3:
	NewAcc = [ { clause, FileLoc, _PatternSeq=[ ASTForF ], _GuardSeq=[],
				 _Body=[ ASTForS ] } | Acc ],

	generate_second_clauses( T, FileLoc, NewAcc ).



% Generates the maybe forms corresponding to foobar:get_maybe_first_for_TOPIC/1.
generate_maybe_fun_forms_for_first( Entries, TopicName, FileLoc ) ->

	% We generate first a full maybe function, then derive its strict
	% counterpart from it (the strict one calling the maybe one):

	Arity = 1,

	MaybeFunName =
		text_utils:atom_format( "get_maybe_first_for_~ts", [ TopicName ] ),

	MaybeClauses = generate_first_clauses( Entries, FileLoc,
		_MAcc=[ catch_all_clause( second_not_found, TopicName, _Lookup='maybe',
								  FileLoc ) ] ),

	MaybeFunForm = { function, FileLoc, MaybeFunName, Arity, MaybeClauses },


	StrictFunName =
		text_utils:atom_format( "get_first_for_~ts", [ TopicName ] ),

	StrictClauses = generate_strict_calling_clauses(
		_ErrorAtom=second_not_found, MaybeFunName, TopicName, FileLoc ),

	%trace_utils:debug_fmt( "Strict clauses:~n ~p", [ StrictClauses ] ),

	StrictFunForm = { function, FileLoc, StrictFunName, Arity, StrictClauses },

	[ MaybeFunForm, StrictFunForm ].



% Generates the maybe forms corresponding to
% foobar:get_maybe_second_for_TOPIC/1.
%
generate_maybe_fun_forms_for_second( Entries, TopicName, FileLoc ) ->

	Arity = 1,

	MaybeFunName =
		text_utils:atom_format( "get_maybe_second_for_~ts", [ TopicName ] ),

	MaybeClauses = generate_second_clauses( Entries, FileLoc,
		_MAcc=[ catch_all_clause( first_not_found, TopicName, _Lookup='maybe',
								  FileLoc ) ] ),

	MaybeFunForm = { function, FileLoc, MaybeFunName, Arity, MaybeClauses },


	StrictFunName =
		text_utils:atom_format( "get_second_for_~ts", [ TopicName ] ),

	StrictClauses = generate_strict_calling_clauses(
		_ErrorAtom=first_not_found, MaybeFunName, TopicName, FileLoc ),

	StrictFunForm = { function, FileLoc, StrictFunName, Arity, StrictClauses },

	[ MaybeFunForm, StrictFunForm ].



% (helper)
generate_strict_calling_clauses( ErrorAtom, MaybeFunName, TopicName,
								 FileLoc ) ->

	% Corresponds to a clause:
	%  FUNC( X ) ->
	%    case MaybeFunName( X ) of
	%
	%        undefined ->
	%            throw( { ErrorAtom, TopicName, X } );
	%
	%        V ->
	%            V
	%
	%    end

	XVar = {var,FileLoc,'X'},

	VVar = {var,FileLoc,'V'},

	ThrowTuple = { tuple, FileLoc, [ {atom,FileLoc,ErrorAtom},
									 {atom,FileLoc,TopicName}, XVar ] },

	% Single clause:
	[ { clause, FileLoc, _PatternSeq=[ XVar ], _GuardSeq=[],
		[ { 'case', FileLoc,
			{ call, FileLoc, {atom,FileLoc,MaybeFunName}, [ XVar ] },
			[ { clause, FileLoc, [{atom,FileLoc,undefined}], [],
				[ { call, FileLoc, _Fun={atom,FileLoc,throw},
				  _Args=[ ThrowTuple ] } ] },
			  { clause, FileLoc, [VVar], [],
				[ VVar ] } ] } ] } ].



% @doc Returns a catch-all clause throwing an (hopefully) informative
% {ErrorAtom, TopicName, Value} exception, like {first_not_found, my_topic,
% MyUnexpectedValue} (rather than a {my_generated_module,
% '-inlined-get_second_for_TOPIC/1-', ... function_clause).
%
% Thus results in { {nocatch, {first_not_found, my_topic, MyUnexpectedValue} },
% [{my_generated_module, get_second_for_my_topic,1,[]}, ...
%
-spec catch_all_clause( error_type(), topic_name(), element_lookup(),
						file_loc() ) -> form().
catch_all_clause( ErrorAtom, TopicName, _Lookup=strict, FileLoc ) ->

	% Corresponds to a clause:
	%  FUNC( NotMatched ) ->
	%    throw( { ErrorAtom, TopicName, NotMatched } )

	NotMatchedVar = { var, FileLoc, 'NotMatched' },

	% Not {remote, FileLoc, _Mod={atom,FileLoc,erlang}, _FunThrow...
	ThrowCall = { call, FileLoc, _Fun={atom,FileLoc,throw},
		_Args=[ { tuple, FileLoc, [ {atom,FileLoc,ErrorAtom},
								{atom,FileLoc,TopicName}, NotMatchedVar ] } ] },

	{ clause, FileLoc, _PatternSeq=[ NotMatchedVar ], _GuardSeq=[],
		_Body=[ ThrowCall ] };


catch_all_clause( _ErrorAtom, _TopicName, _Lookup=maybe, FileLoc ) ->

	% Corresponds to a clause:
	%  FUNC( _NotMatched ) ->
	%    undefined.

	NotMatchedVar = { var, FileLoc, '_' },

	{ clause, FileLoc, _PatternSeq=[ NotMatchedVar ], _GuardSeq=[],
		_Body=[ {atom,FileLoc,'undefined'} ] }.
