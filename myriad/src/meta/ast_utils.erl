% Copyright (C) 2018-2023 Olivier Boudeville
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
% Creation date: Monday, January 1, 2018.


% @doc Gathering of various convenient <b>facilities to manage ASTs</b>
% (Abstract Syntax Trees): direct bridge towards plain Erlang AST.
%
% Convenient to isolate processings from the current Erlang AST syntax, which
% could change over time (a bit like the erl_syntax standard module, albeit with
% a different set of conventions).
%
% See also:
% - the meta_utils module, for meta primitives less directly linked with syntax
% - the ast_scan module, to perform a full, strict traversal of an AST
%
-module(ast_utils).



% For the file_info record:
-include_lib("kernel/include/file.hrl").


% Directly obtained from the epp module:

-type include_path() :: [ directory_name() ].
-type macro() :: atom() | { atom(), term() }.
-type source_encoding() :: 'latin1' | 'utf8'.

-type preprocessor_option() :: { 'includes', include_path() }
							 | { 'macros', [ macro() ] }
							 | { 'default_encoding', source_encoding() }
							 | 'extra'.

-export_type([ include_path/0, macro/0, source_encoding/0,
			   preprocessor_option/0 ]).


% For ast_transforms():
-include("ast_transform.hrl").


% For the default_generation_location defines:
-include("ast_utils.hrl").



% Directly inspired from erl_lint:


-type issue_description() :: term().
% Description of a compilation-related issue (error or warning).


-type issue_info() :: { file_loc(), module(), issue_description() }.
% Full information about a compilation-related issue.
%
% The module is the one emitting that issue (e.g. erl_lint).


-type issue_report() :: { file_name(), [ issue_info() ] }.
% A warning regarding a source file, corresponding to a list of error
% information.


-export_type([ issue_description/0, issue_info/0, issue_report/0 ]).



% Checking:
-export([ check_ast/1,
		  check_file_loc/1, check_file_loc/2,
		  check_module_name/1, check_module_name/2,
		  check_inline_options/1, check_inline_options/2,
		  check_arity/1, check_arity/2 ]).


% Converting:
-export([ erl_to_ast/1, erl_to_ast/2,
		  beam_to_ast/1, term_to_form/1, variable_names_to_ast/2,
		  string_to_form/1, string_to_form/2,
		  string_to_expressions/1, string_to_expressions/2,
		  string_to_value/1 ]).



% Displaying:
-export([ display_debug/1, display_debug/2,
		  display_info/1, display_info/2,
		  display_notice/1, display_notice/2,
		  display_warning/1, display_warning/2,
		  display_error/1, display_error/2,
		  display_critical/1, display_critical/2,
		  display_alert/1, display_alert/2,
		  display_emergency/1, display_emergency/2 ]).


% Signaling:
-export([ notify_warning/2,
		  raise_error/1, raise_error/2, raise_error/3,
		  raise_usage_error/3, raise_usage_error/4,
		  get_error_form/3, format_error/1 ]).


% Other:
-export([ get_generated_code_location/0,
		  format_file_loc/1, file_loc_to_line_string/1, format_file_loc_alt/1,
		  file_loc_to_string/1, file_loc_to_explicative_term/1,
		  write_ast_to_file/2 ]).


% Shorthands:

-type void() :: basic_utils:void().
-type module_name() :: basic_utils:module_name().
-type maybe(T) :: basic_utils:maybe(T).
-type error_reason() :: basic_utils:error_reason().

-type ustring() :: text_utils:ustring().
-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type file_name() :: file_utils:file_name().
-type directory_name() :: file_utils:directory_name().

-type ast() :: ast_base:ast().
-type form() :: ast_base:form().
-type file_loc() :: ast_base:file_loc().
-type form_context() :: ast_base:form_context().
-type source_context() :: ast_base:source_context().
-type ast_transforms() :: ast_transform:ast_transforms().

-type function_id() :: meta_utils:function_id().


% Checking section.


% @doc Checks whether specified AST is legit: lints it.
-spec check_ast( ast() ) -> void().
check_ast( AST ) ->

	%display_debug( "~p", [ AST ] ),

	% Directly outputing the warnings or errors is generally useless; for
	% example, in addition to:
	%
	%  simple_parse_transform_target.erl:68: type void() undefined
	%
	% We would get: [{"simple_parse_transform_target.erl",
	%               [{68,erl_lint,{undefined_type,{void,0}}}]}]

	% Finally interpret_issue_reports/1 directly used to output the issues;
	% however some are legit (e.g. 'type void() undefined'), so we must let them
	% go through:
	%
	case erl_lint:module( AST ) of

		{ ok, _Warnings=[] } ->
			%display_trace( "(no warning or error emitted)~n" ),
			ok;

		{ ok, Warnings } ->
			%display_error( "Warnings, reported as errors: ~p~n",
			%  [ Warnings ] ),
			interpret_issue_reports( Warnings ),
			%exit( warning_reported );
			warning_reported;

		{ error, Errors, _Warnings=[] } ->
			%display_error( "Errors reported: ~p~n", [ Errors ] ),
			interpret_issue_reports( Errors ),
			%exit( error_reported );
			error_reported;

		{ error, Errors, Warnings } ->
			%display_error( "Errors reported: ~p~n", [ Errors ] ),
			interpret_issue_reports( Errors ),

			%display_error(
			%  "Warnings, reported as errors: ~p~n", [ Warnings ] ),
			interpret_issue_reports( Warnings ),
			%exit( error_reported )
			error_reported

	end.



% @doc Interprets the specified list of issue reports.
-spec interpret_issue_reports( [ issue_report() ] ) -> void().
%interpret_issue_reports( _IssueReports=[] ) ->
%	% Should never happen:
%	display_info( "(no remark emitted)" );

% No need to further special-case the number of issue reports, as it is not
% meaningful (one may include an arbitrary long list):

%interpret_issue_reports( _IssueReports=[ OneIssueReport ] ) ->
%   interpret_issue_report( OneIssueReport );

interpret_issue_reports( IssueReports ) ->

	[ interpret_issue_report( R ) || R <- IssueReports ].

	%text_utils:format( "~B remarks: ~ts", [ length( IssueReports ),
	%   text_utils:strings_to_string( ReportStrings ) ] ).



% @doc Interprets the specified issue report.
-spec interpret_issue_report( issue_report() ) -> void().
interpret_issue_report( _IssueReport={ Filename, IssueInfos } ) ->

	% We could normalise it instead, yet file_utils would become a dependency:
	CanonicFilename = filename:basename( Filename ),

	[ interpret_issue_info( CanonicFilename, E ) || E <- IssueInfos ].

	%text_utils:format( "in file '~ts': ~ts", [ CanonicFilename,
	%   text_utils:strings_to_string( IssueStrings ) ] ).



% @doc Interprets the specified error description.
-spec interpret_issue_info( file_name(), issue_info() ) -> void().
interpret_issue_info( Filename,
					  _IssueInfo={ FileLoc, DetectorModule, IssueDesc } ) ->

	% Module is the detecting one, typically erl_lint:
	%text_utils:format( "~ts, module '~p', ~ts", [
	%   file_loc_to_string( FileLoc ), Module,
	%   interpret_issue_description( IssueDesc ) ] ).

	%text_utils:format( "~ts: ~ts", [ file_loc_to_string( FileLoc ),
	%   interpret_issue_description( IssueDesc, DetectorModule ) ] ).

	io:format( "~ts:~ts: ~ts~n", [ Filename, file_loc_to_string( FileLoc ),
		interpret_issue_description( IssueDesc, DetectorModule ) ] ).



% @doc Interprets the specified issue description, detected by the specified
% module.
%
% Note: full control is offered here to enrich this function at will, if wanted.
%
-spec interpret_issue_description( issue_description(), module_name() ) ->
										ustring().
interpret_issue_description( IssueDescription, DectectorModule ) ->
	%For example, the detector module may be erl_lint:
	DectectorModule:format_error( IssueDescription ).



% @doc Checks that the specified source, in-file location is legit.
-spec check_file_loc( term() ) -> file_loc().
check_file_loc( Line ) ->
	check_file_loc( Line, _Context=undefined ).


% @doc Checks that the specified source, in-file location is legit.
-spec check_file_loc( term(), maybe( form_context() ) ) -> file_loc().
check_file_loc( Line, _Context )
						when is_integer( Line ) andalso Line >= 0 ->
	Line;

% Since OTP 24.0:
check_file_loc( FileLoc={ Line, Column }, _Context )
		when is_integer( Line ) andalso Line >= 0
			 andalso is_integer( Column ) andalso Column >=0 ->
	FileLoc;

check_file_loc( Other, Context ) ->
	% Not raise_error/2:
	throw( { invalid_file_location, Other, Context } ).



% @doc Checks that the specified module name is legit, and returns it.
-spec check_module_name( term() ) -> module_name().
check_module_name( Name ) ->
	check_module_name( Name, _Context=undefined ).



% @doc Checks that the specified module name is legit, and returns it.
-spec check_module_name( term(), form_context() ) -> module_name().
check_module_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_module_name( Other, Context ) ->
	raise_error( [ invalid_module_name, Other ], Context ).



% @doc Checks that the specified inline options are legit.
-spec check_inline_options( term() ) -> [ function_id() ].
check_inline_options( FunIds ) ->
	check_inline_options( FunIds, _Context=undefined ).


% @doc Checks that the specified inline options are legit.
-spec check_inline_options( term(), form_context() ) -> [ function_id() ].
check_inline_options( FunIds, Context ) when is_list( FunIds ) ->
	ast_function:check_function_ids( FunIds, Context );

check_inline_options( Other, Context ) ->
	raise_error( [ invalid_inline_options, Other ], Context ).




% @doc Checks that the specified (function or type) arity is legit.
-spec check_arity( term() ) -> arity().
check_arity( Arity ) ->
	check_arity( Arity, _Context=undefined ).


% @doc Checks that the specified (function or type) arity is legit.
-spec check_arity( term(), form_context() ) -> arity().
check_arity( Arity, _Context ) when is_integer( Arity ) andalso Arity >= 0 ->
	Arity;

check_arity( Other, Context ) ->
	raise_error( [ invalid_arity, Other ], Context ).



% Conversion section.


% @doc Reads specified Erlang source file (*.erl) and returns the corresponding
% AST, based on default preprocessor options.
%
% For example useful to debug a parse transform first separately from the
% compile pipe-line, relying here on the usual, convenient error management
% instead of having little informative messages like: 'undefined parse transform
% 'foobar'' as soon as a call to a non-existing module:function/arity is made.
%
-spec erl_to_ast( file_name() ) -> ast().
erl_to_ast( ErlSourceFilename ) ->
	erl_to_ast( ErlSourceFilename, _PreprocessorOptions=[] ).



% @doc Reads specified Erlang source file (*.erl) and returns the corresponding
% AST, based on specified preprocessor (eep) options.
%
% For example useful to debug a parse transform first separately from the
% compile pipe-line, relying here on the usual, convenient error management
% instead of having little informative messages like: 'undefined parse transform
% 'foobar'' as soon as a call to a non-existing module:function/arity is made.
%
-spec erl_to_ast( file_name(), [ preprocessor_option() ] ) -> ast().
erl_to_ast( ErlSourceFilename, PreprocessorOptions ) ->

	case epp:parse_file( ErlSourceFilename, PreprocessorOptions ) of

		{ error, Error } ->
			throw( { parse_file_failed, ErlSourceFilename, Error } );

		{ ok, AST } ->
			AST

	end.



% @doc Reads the specified BEAM file (expected to be compiled with debug
% information) and returns the corresponding AST.
%
% Note that the filename must be a relative or absolute path pointing directly
% to the BEAM file (it is not searched through the code path).
%
-spec beam_to_ast( file_name() ) -> ast().
beam_to_ast( BeamFilename ) ->

	% We do not use functions from other Myriad modules here (e.g. file_utils)
	% as they are not expected to be built yet (they will be built with the
	% myriad parse transform afterwards).
	%
	case file:read_link_info( BeamFilename ) of

		{ ok, FileInfo } ->
			#file_info{ type=regular } = FileInfo,
			ok;

		{ error, eloop } ->
			% Probably a recursive symlink:
			throw( { too_many_symlink_levels, BeamFilename } );

		{ error, enoent } ->
			throw( { non_existing_beam_file, BeamFilename } )

	end,

	% We could basically list all chunks, but we are only interested here in the
	% abstract code:

	% Everything:
	%Chunks = [ abstract_code, attributes, compile_info, exports,
	%           labeled_exports, imports, indexed_imports, locals,
	%           labeled_locals, atoms ],

	% Just the code AST:
	Chunks = [ abstract_code ],

	% Everything but the code AST:
	% OtherChunks = [ attributes, compile_info, exports,
	%                 labeled_exports, imports, indexed_imports, locals,
	%                 labeled_locals, atoms ],

	%Options = [ allow_missing_chunks ],

	Options=[],

	MyriadCryptoKeyFun =
		fun( init ) ->
			ok;

		   ( { debug_info, _Mode, _Module, _Filename } ) ->
				% Refer to GNUmakevars.inc:
				_Key="Ceylan-Myriad";

		   ( clear ) ->
				ok

	end,

	ok = beam_lib:crypto_key_fun( MyriadCryptoKeyFun ),

	case beam_lib:chunks( BeamFilename, Chunks, Options ) of

		{ ok, { _Module, [ { abstract_code, { _RawAbstractV1,
											  AbstractCode } } ] } } ->
			%display_debug( "Module = ~p.", [ Module ] ),
			AbstractCode;

		{ error, beam_lib, Reason } ->
			throw( { beam_reading_failed, Reason } )

	end.




% Section to manage ASTs and forms.


% @doc Converts the specified Erlang term (e.g. the float '42.0') into a
% corresponding form (e.g. '{float, _FileLoc={0,1}, 42.0}').
%
-spec term_to_form( term() ) -> form().
term_to_form( Term ) ->

	case erl_syntax:abstract( Term ) of

		% Either the doc or the type information for erl_syntax:abstract/1 is
		% incorrect:

		%badarg ->
		%   throw( { term_abstraction_failed, Term } );

		SyntaxTree ->

			% Could be used with erl_syntax:is_tree/1:
			% case erl_syntax:revert( SyntaxTree ) of...
			erl_syntax:revert( SyntaxTree )

	end.



% @doc Converts a list of names of variables into the corresponding AST, at
% the specified in-file location.
%
% For example if wanting to specify '[V1, Alpha, A]', we have:
% variable_names_to_ast( ["V1", "Alpha", "A"], _FileLoc=0) = [ {cons,0,
% {var,0,'V1'}, {cons,0,{var,0,'Alpha'}, {cons,0,{var,0,'A'}, {nil,0}}}}]
%
-spec variable_names_to_ast( [ ustring() ], file_loc() ) -> ast().
variable_names_to_ast( VariableNames, FileLoc ) ->

	% Could be done directly recursively by incrementally 'consing' reversed
	% list.

	NameListString = "[ " ++ text_utils:join( ", ",  VariableNames ) ++ " ].",

	string_to_expressions( NameListString, FileLoc ).



% @doc Converts the specified source code of a form (as a string) into its
% corresponding abstract form (using the default in-file location applying to
% generated code).
%
% For example string_to_form("f() -> hello_world.") may return
%   {function, {0,1}, f, 0, [{clause, {0,1}, [], [],
%       [ {atom, {0,1}, hello_world} ] } ] }
%
-spec string_to_form( ustring() ) -> form().
string_to_form( FormString ) ->
	string_to_form( FormString, _FileLoc=?default_generation_location ).



% @doc Converts the specified source code of a form (that is, a string) into its
% corresponding abstract form, at the specified in-file location.
%
% For example string_to_form("f() -> hello_world.", 42) may return
%   {function, 42, f, 0, [{clause, 42, [], [], [{atom,42,hello_world}]}]}
%
-spec string_to_form( ustring(), file_loc() ) -> form().
string_to_form( FormString, FileLoc ) ->

	% First get Erlang tokens from that string:
	Tokens = case erl_scan:string( FormString, FileLoc ) of

		% For example [{atom,1,f}, {'(',1},{')',1}, {'->',1},
		% {atom,1,hello_world}, {dot,1}].
		%
		{ ok, Toks, _EndLocation } ->
			%display_debug( "Tokens: ~p", [ Toks ] ),
			Toks;

		ErrorTok ->
			throw( { form_tokenizing_error, FormString, ErrorTok } )

	end,

	% Tokens to erl_parse trees:

	case erl_parse:parse_form( Tokens ) of

		{ ok, ParseTree } ->
			ParseTree;

		ErrorPar ->
			throw( { form_parsing_error, FormString, ErrorPar } )

	end.



% @doc Converts the specified source code of a list of expressions (that is, a
% string) into its corresponding AST (using the default in-file location
% applying to generated code).
%
% For example string_to_expressions("[{a, 1}, foobar]") may return:
%   [{cons,  {0,1}, {tuple,  {0,1}, [{atom, {0,1},a}, {integer, {0,1},1}]},
%    {cons,  {0,1}, {atom, {0,1},foobar}, {nil, {0,1}}}}]
%
-spec string_to_expressions( ustring() ) -> ast().
string_to_expressions( ExpressionString ) ->
	string_to_expressions( ExpressionString,
						   _FileLoc=?default_generation_location ).



% @doc Converts the specified source code of a term (that is, a string) and a
% location into the corresponding abstract form.
%
% For example string_to_expressions("[{a, 1}, foobar]", _Loc=42) may return
%   [ {cons, 42, {tuple, 42, [ {atom,42,a}, {integer,42,1} ]},
%     {cons, 42, {atom,42,foobar}, {nil,42} }}]
%
% Note: at least with OTP24, apparently FileLoc cannot include a column
-spec string_to_expressions( ustring(), file_loc() ) -> ast().
string_to_expressions( ExpressionString, FileLoc ) ->

	% First get Erlang tokens from that string:
	Tokens = case erl_scan:string( ExpressionString, FileLoc ) of

		% For example [ {'[',42}, {'{',42}, {atom,42,a}, {',',42},
		% {integer,42,1}, {'}',42}, {',',42}, {atom,42,foobar}, {']',42}]
		%
		{ ok, Toks, _EndFileLoc } ->
			%display_debug( "Tokens: ~p (at ~p)", [ Toks, EndFileLoc ] ),
			Toks;

		ErrorTok ->
			throw( { expression_tokenizing_error, ExpressionString, ErrorTok } )

	end,

	% Tokens to erl_parse trees:

	case erl_parse:parse_exprs( Tokens ) of

		{ ok, ParseTree } ->
			ParseTree;

		ErrorPar ->
			throw( { expression_parsing_error, ExpressionString, ErrorPar } )

	end.



% @doc Converts the specified source code of a term (that is, a string) into its
% corresponding value.
%
% For example string_to_value("[{tiger,[lion,leopard]}]") returns the
% [{tiger, [lion,leopard]}] term.
%
-spec string_to_value( ustring() ) -> term().
string_to_value( ExpressionString ) ->

	% We automatically add the necessary final dot:
	[ Expr ] = string_to_expressions( ExpressionString ++ "." ),

	{ value, Result, _NewBindings } = erl_eval:expr( Expr, _Bindings=[] ),

	Result.




% Subsection for trace outputs that are specific to parse-transforms.


% @doc Displays specified text as debug.
-spec display_debug( ustring() ) -> void().
display_debug( String ) ->
	io:format( "[debug] ~ts~n", [ String ] ).


% @doc Displays specified formatted text as debug.
-spec display_debug( format_string(), [ term() ] ) -> void().
display_debug( FormatString, Values ) ->
	display_debug( io_lib:format( FormatString, Values ) ).



% @doc Displays specified text as info.
-spec display_info( ustring() ) -> void().
display_info( String ) ->
	io:format( "[info] ~ts~n", [ String ] ).


% @doc Displays specified formatted text as info.
-spec display_info( format_string(), [ term() ] ) -> void().
display_info( FormatString, Values ) ->
	display_info( io_lib:format( FormatString, Values ) ).



% @doc Displays specified text as notice.
-spec display_notice( ustring() ) -> void().
display_notice( String ) ->
	io:format( "[notice] ~ts~n", [ String ] ).


% @doc Displays specified formatted text as notice.
-spec display_notice( format_string(), [ term() ] ) -> void().
display_notice( FormatString, Values ) ->
	display_notice( io_lib:format( FormatString, Values ) ).



% @doc Displays specified text as warning.
-spec display_warning( ustring() ) -> void().
display_warning( String ) ->
	io:format( "[warning] ~ts~n", [ String ] ).


% @doc Displays specified formatted text as warning.
-spec display_warning( format_string(), [ term() ] ) -> void().
display_warning( FormatString, Values ) ->
	display_warning( io_lib:format( FormatString, Values ) ).



% @doc Displays specified text as error.
-spec display_error( ustring() ) -> void().
display_error( String ) ->
	io:format( "~n[error] ~ts~n", [ String ] ).


% @doc Displays specified formatted text as error.
-spec display_error( format_string(), [ term() ] ) -> void().
display_error( FormatString, Values ) ->
	display_error( io_lib:format( FormatString, Values ) ).



% @doc Displays specified text as critical.
-spec display_critical( ustring() ) -> void().
display_critical( String ) ->
	io:format( "[critical] ~ts~n", [ String ] ).


% @doc Displays specified formatted text as critical.
-spec display_critical( format_string(), [ term() ] ) -> void().
display_critical( FormatString, Values ) ->
	display_critical( io_lib:format( FormatString, Values ) ).



% @doc Displays specified text as alert.
-spec display_alert( ustring() ) -> void().
display_alert( String ) ->
	io:format( "[alert] ~ts~n", [ String ] ).


% @doc Displays specified formatted text as alert.
-spec display_alert( format_string(), [ term() ] ) -> void().
display_alert( FormatString, Values ) ->
	display_alert( io_lib:format( FormatString, Values ) ).



% @doc Displays specified text as emergency.
-spec display_emergency( ustring() ) -> void().
display_emergency( String ) ->
	io:format( "[emergency] ~ts~n", [ String ] ).


% @doc Displays specified formatted text as emergency.
-spec display_emergency( format_string(), [ term() ] ) -> void().
display_emergency( FormatString, Values ) ->
	display_emergency( io_lib:format( FormatString, Values ) ).



% @doc Notifies a warning, with specified context.
-spec notify_warning( [ term() ], form_context() ) -> void().
notify_warning( Elements, Context ) ->

	% The specified elements may then be prefixed by a context string:
	case get_elements_with_context( Elements, Context ) of

		% Supposedly a string (and no context):
		[ SingleElement ] when is_list( SingleElement ) ->
			display_warning( "~ts", [ SingleElement ] );

		[ FirstElem | OtherElems ] when is_list( FirstElem ) ->
			display_warning( "~ts ~p", [ FirstElem, OtherElems ] );

		AllElements ->
			display_warning( "~p", [ AllElements ] )

	end.


% @doc Raises a (compile-time, rather ad hoc) error when applying a parse
% transform, to stop the build on failure and report the actual error, thanks to
% the specified term (often, a list of error elements).
%
% Used to be a simple throw, but then for parse transforms the error message was
% garbled in messages like:
%
% ```
% internal error in lint_module;
% crash reason: function_clause
%
%  in function  erl_lint:'-compiler_options/1-lc$^0/1-0-'/1
%     called as erl_lint:'-compiler_options/1-lc$^0/1-0-'({
% table_type_defined_more_than_once,{line,12},foo_hashtable,bar_hashtable})
% '''
%
% Note:
%
% - this function is used to report errors detected by Myriad itself (not by the
% Erlang toolchain)
%
% - prefer using raise_usage_error/* to report errors in a more standard,
% convenient way
%
-spec raise_error( term() ) -> no_return().
raise_error( ErrorTerm ) ->

	%throw( ErrorTerm )
	%display_error( "~p", [ ErrorTerm ] ),

	% Does not add any information (just non-relevant erl_parse, epp
	% etc state):
	%
	%erlang:exit( { ErrorTerm, erlang:get_stacktrace() } ).

	%erlang:exit( ErrorTerm ).

	% Possibly a list of elements:
	raise_error( ErrorTerm, _Context=undefined ).



% @doc Raises an error, with specified context, thanks to the specified term
% (often, a list of error elements), from the Myriad layer.
%
% For example raise_error([invalid_module_name, Other], _Context=112) shall
% result in throwing {invalid_module_name, Other, {line, 112}}.
%
% Note:
%
% - this function is used to report errors detected by Myriad itself (not by the
% Erlang toolchain)
%
% - prefer using raise_usage_error/* to report errors in a more standard,
% convenient way
%
-spec raise_error( term(), maybe( source_context() ) ) -> no_return().
raise_error( ErrorTerm, Context ) ->
	raise_error( ErrorTerm, Context, _OriginLayer="Myriad" ).



% @doc Raises an error, with specified context, from the specified layer
% (expected to be above Myriad).
%
% For example raise_error([invalid_module_name, Other], _Context=112,
% _OriginLayer="FooLayer") shall result in throwing {invalid_module_name, Other,
% {line, 112}}.
%
% Note:
%
% - this function is used to report errors detected by Myriad itself (not by the
% Erlang toolchain)
%
% - prefer using raise_usage_error/* to report errors in a more standard,
% convenient way
%
-spec raise_error( term(), maybe( source_context() ),
				   basic_utils:layer_name() ) -> no_return();
				 ( ustring(), ast_transforms(), file_loc() ) -> no_return().
raise_error( Message, #ast_transforms{ transformed_module_name=ModName },
			 FileLoc ) ->

	io:format( "~ts.erl:~ts: ~ts~n",
			   [ ModName, format_file_loc( FileLoc ), Message ] ),

	halt( 5 );


raise_error( Message, MaybeSourceContext, OriginLayer ) ->

	%trace_utils:debug_fmt( "Message: ~p, MaybeSourceContext: ~p, Layer: ~p",
	%   [ Message, MaybeSourceContext, OriginLayer ] ),

	Prefix = case MaybeSourceContext of

		undefined ->
			"Error";

		Filename when is_binary( Filename ) ->
			io_lib:format( "Error in ~ts", [ Filename ] );

		{ Line, Column } when is_integer( Line ) andalso
							  is_integer( Column ) ->
			io_lib:format( "Error at line #~B, column ~B", [ Line, Column ] );

		Line when is_integer( Line ) ->
			io_lib:format( "Error at line #~B", [ Line ] );

		{ Filename, FileLoc } ->
			io_lib:format( "~ts:~ts: error",
						   [ Filename, file_loc_to_string( FileLoc ) ] );

		Other ->
			io_lib:format( "Error in unexpected context ~p", [ Other ] )

	end,

	% Used to rely on display_error/1, yet we want to respect the standard error
	% report format, so:
	%
	case text_utils:is_string( Message ) of

		true->
			io:format( "~ts raised while performing ~ts-level transformations: "
				"~ts~n", [ Prefix, OriginLayer, Message ] );

		false ->
			io:format( "~ts raised while performing ~ts-level transformations:"
				"~n  ~p~n", [ Prefix, OriginLayer, Message ] )

	end,

	%DisplayStacktrace = true,
	DisplayStacktrace = false,

	try

		DisplayStacktrace andalso
			% To get a stack trace:
			throw( { ast_transformation_failed_in, OriginLayer } )

	catch

		% Class is 'throw', R is what we just threw:

		% Pre-21.0 code:
		%_C:_R ->
		%
		%   % Removing useless {ast_utils,raise_error,2,...:
		%   ActualStackTrace = tl( erlang:get_stacktrace() ),

		% Post-21.0 code:
		_C:_R:StackTrace ->

			% Used to confirm that, in some cases, the direct, original
			% stacktrace returned by the Erlang VM seems limited to the N=8
			% latest calls (deeper ones being thus lacking, unfortunately):
			%
			%io:format( "Full stack trace:~n~p~n", [ StackTrace ] ),

			% Removing useless {ast_utils,raise_error,2,...:
			ActualStackTrace = tl( StackTrace ),

			StackElements =
				interpret_stack_trace( ActualStackTrace, _Acc=[], _Count=1 ),

			% These are Myriad-internal information, generally of no use to
			% understand the problem regarding the code being compiled:
			%
			display_debug( "Transformation error happened in "
						   "(latest calls first):~n~ts", [ StackElements ] )

	end,

	% Would not interrupt the processing of the AST anyway:
	%throw( list_to_tuple( AllElements ) );
	%{ error, AllElements };
	%exit( AllElements );
	halt( 5 ).


% (helper)
interpret_stack_trace( _StackTrace=[], Acc, _Count ) ->
	lists:reverse( Acc );

interpret_stack_trace( _StackTrace=[ { Module, FunName, Arity,
							_FileLoc=[ { file, Path }, { line, Line } ] } | T ],
					   Acc, Count ) ->

	Text = io_lib:format( " [~B] ~ts:~ts/~B    [~ts, line ~B]~n",
						  [ Count, Module, FunName, Arity, Path, Line ] ),

	interpret_stack_trace( T, [ Text | Acc ], Count+1 );

interpret_stack_trace( _StackTrace=[ { Module, FunName, Arity,
		_FileLoc=[ { file, Path }, { line, Line }, { column, Column } ] } | T ],
					   Acc, Count ) ->

	Text = io_lib:format( " [~B] ~ts:~ts/~B    [~ts, line ~B, column ~B]~n",
		[ Count, Module, FunName, Arity, Path, Line, Column ] ),

	interpret_stack_trace( T, [ Text | Acc ], Count+1 );

interpret_stack_trace( _StackTrace=[ H | T ], Acc, Count ) ->
	Text = io_lib:format( "~p~n", [ H ] ),
	interpret_stack_trace( T, [ Text | Acc ], Count+1 ).




% @doc Raises a (compile-time, relatively standard) user-related error, with
% specified source context, to stop the build on failure and report adequately
% the actual error to the user.
%
-spec raise_usage_error( format_string(), format_values(),
						 file_name() | module_name() ) -> no_return().
raise_usage_error( ErrorFormatString, ErrorValues, FileOrModname ) ->
	raise_usage_error( ErrorFormatString, ErrorValues, FileOrModname,
					   _ActualFileLoc=?default_generation_location ).



% @doc Raises a (compile-time, relatively standard) user-related error, with
% specified source context, to stop the build on failure and report adequately
% the actual error to the user.
%
-spec raise_usage_error( format_string(), format_values(),
			file_name() | module_name(), maybe( file_loc() ) ) -> no_return().
raise_usage_error( ErrorFormatString, ErrorValues, FileOrModname,
				   _FileLoc=undefined ) ->
	raise_usage_error( ErrorFormatString, ErrorValues, FileOrModname,
					   _ActualFileLoc=?default_generation_location );

raise_usage_error( ErrorFormatString, ErrorValues, ModuleName, FileLoc )
								when is_atom( ModuleName ) ->
	Filename = io_lib:format( "~ts.erl", [ ModuleName ] ),
	raise_usage_error( ErrorFormatString, ErrorValues, Filename, FileLoc );

raise_usage_error( ErrorFormatString, ErrorValues, Filename, FileLoc ) ->

	ErrorString = io_lib:format( ErrorFormatString, ErrorValues ),

	io:format( "~ts:~ts: ~ts~n",
			   [ Filename, format_file_loc( FileLoc ), ErrorString ] ),

	% Almost the only way to stop the processing of the AST:
	halt( 5 ).



% @doc Returns an AST form in order to raise a (compile-time, standard) error
% when applying a parse transform, to stop the build on failure and report the
% actual error.
%
% The specified error term will be transformed by the specified module into a
% (textual) error message (see format_error/1), and then will be reported as
% originating from the specified location in the source file of the module being
% compiled.
%
-spec get_error_form( error_reason(), module_name(), file_loc() ) -> form().
get_error_form( ErrorTerm, FormatErrorModule, FileLoc ) ->

	% Actually the most standard way of reporting an error seems to insert a
	% dedicated form in the AST.

	% May ultimately report (thanks to ?MODULE:format_error/1), when compiling a
	% foobar module and if:
	%
	% - Line is 15
	%
	% - 'apply(FormatErrorModule, format_error, [ErrorTerm])' is "my error
	% message":
	%
	% the following error message: "foobar:15: my error message".
	%
	{ error, { FileLoc, FormatErrorModule, ErrorTerm } }.



% @doc This function (whose name is standard, conventional) is to be defined on
% a per-module basis (typically in the module defining the parse transform being
% applied) and allows to convert error terms (that are, here, related to
% parse-transforms) into textual messages that can be output by the build chain.
%
-spec format_error( error_reason() ) -> ustring().
format_error( ErrorTerm ) ->
	% Of course this is just an example:
	text_utils:format( "my ast_utils error reported: ~ts", [ ErrorTerm ] ).



% @doc Returns error/warning elements including the specified context.
%
% (helper)
%
-spec get_elements_with_context( [ term() ], maybe( source_context() ) ) ->
										[ term() ].
get_elements_with_context( Elements, _Context=undefined ) ->
	Elements;

% Tricky matching, as FilePath may be either a plain or binary string, and
% FileLoc, if not undefined, may be either a line or a {Line,Column} pair - but
% Context may be also directly such a pair (thus with no FilePath).
%
get_elements_with_context( Elements, _Context=FileLoc={ L, C } )
		when is_integer( L ) andalso is_integer( C ) ->
	Prefix = io_lib:format( "~ts: ", [ file_loc_to_string( FileLoc ) ] ),
	[ Prefix | Elements ];
	%Elements ++ [ file_loc_to_explicative_term( FileLoc ) ];

% FilePath is a string, plain or binary:
get_elements_with_context( Elements, _Context={ FilePath, FileLoc } ) ->
	% Was: Elements ++ [ { file, text_utils:binary_to_string( FilePath ) },
	%                    { line, Line } ];

	% We mimic the default error formatting so that tools (like IDE) have a
	% chance to automatically point to the right location in the sources:
	%
	Prefix = io_lib:format( "~ts:~ts: ",
							[ FilePath, file_loc_to_string( FileLoc ) ] ),

	[ Prefix | Elements ];

get_elements_with_context( Elements, _Context=Line ) when is_integer( Line ) ->
	Elements ++ [ file_loc_to_explicative_term( Line ) ];

get_elements_with_context( Elements, _Context=FilePath )
								when is_binary( FilePath ) ->
	Prefix = io_lib:format( "~ts:0: ",
							[ text_utils:binary_to_string( FilePath ) ] ),
	[ Prefix | Elements ];

get_elements_with_context( Elements, Context ) ->
	% No list_utils module used from this module, direct append:
	Elements ++ [ Context ].




% @doc Returns the conventional virtual in-file (not AST) location denoting
% generated code.
%
-spec get_generated_code_location() -> file_loc().
get_generated_code_location() ->
	% Preferring currently not returning { _Line=0, _Column=1 }, for pre-OTP24
	% compliance:
	%
	%_Line=0.
	%{ _Line=0, _Column=1 }.
	?default_generation_location.



% @doc Returns a standard textual description of specified in-file location
% (typically to output the usual, canonical reference expected by most tools,
% often to report compilation issues).
%
-spec format_file_loc( file_loc() ) -> ustring().
format_file_loc( { Line, Column } ) ->
	io_lib:format( "~B:~B", [ Line, Column ] );

format_file_loc( Line ) ->
	io_lib:format( "~B", [ Line ] ).



% @doc Returns an alternative textual description of specified in-file location
% (e.g. in order to name variables in AST).
%
-spec format_file_loc_alt( file_loc() ) -> ustring().
format_file_loc_alt( { Line, Column } ) ->
	io_lib:format( "~B_~B", [ Line, Column ] );

format_file_loc_alt( Line ) ->
	io_lib:format( "~B", [ Line ] ).



% @doc Returns a textual, user-friendly description of specified in-file
% location.
%
-spec file_loc_to_string( file_loc() ) -> ustring().
file_loc_to_string( { Line, Column } ) ->
	io_lib:format( "line ~B, column ~B", [ Line, Column ] );

file_loc_to_string( Line ) ->
	io_lib:format( "line ~B", [ Line ] ).


% @doc Returns a textual, user-friendly description of specified in-file
% location, just specifying the line (defined for the cases where a column would
% not be especially useful, like when referencing a clause).
%
-spec file_loc_to_line_string( file_loc() ) -> ustring().
file_loc_to_line_string( { Line, _Column } ) ->
	io_lib:format( "line ~B", [ Line ] );

file_loc_to_line_string( Line ) ->
	io_lib:format( "line ~B", [ Line ] ).


% @doc Returns an explicative term (typically to be part of a thrown exception)
% corresponding to the specified in-file location.
%
-spec file_loc_to_explicative_term( file_loc() ) -> term().
file_loc_to_explicative_term( { Line, Column } ) ->
	{ { line, Line }, { column, Column } };

file_loc_to_explicative_term( Line ) ->
	{ line, Line }.



% @doc Writes the specified AST into the specified (text) file.
%
% Useful for example to determine differences between ASTs.
%
-spec write_ast_to_file( ast(), file_name() ) -> void().
write_ast_to_file( AST, Filename ) ->

	% Note: we cannot actually use file_utils, which is not a prerequisite of
	% the 'Myriad' parse transform:

	% We overwrite any pre-existing file:
	{ ok, File } = file:open( Filename, [ write, raw ] ),

	[ ok = file:write( File, io_lib:format( "~p~n", [ F ] )  ) || F <- AST ],

	ok = file:close( File ).
