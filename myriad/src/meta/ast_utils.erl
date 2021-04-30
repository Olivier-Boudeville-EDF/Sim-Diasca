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
% Creation date: Monday, January 1, 2018.



% Gathering of various convenient facilities to manage ASTs (Abstract Syntax
% Trees): direct bridge towards plain Erlang AST.
%
% Convenient to isolate processings from the current Erlang AST syntax, which
% could change over time (a bit like the erl_syntax standard module, albeit with
% a different set of conventions).
%
% See also:
%
% - the meta_utils module, for meta primitives less directly linked with syntax
%
% - the ast_scan module, to perform a full, strict traversal of an AST
%
-module(ast_utils).



% For the file_info record:
-include_lib("kernel/include/file.hrl").


% Directly obtained from the epp module:

-type include_path() :: [ file_utils:directory_name() ].
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


% Directly inspired from erl_lint:


% Description of a compilation-related issue (error or warning).
-type issue_description() :: term().


% Full information about a compilation-related issue.
%
% The module is the one emitting that issue (ex: erl_lint)
%
-type issue_info() :: { line(), module(), issue_description() }.


% A warning regarding a source file, corresponding to a list of error
% informations.
%
-type issue_report() :: { file_name(), [ issue_info() ] }.



-export_type([ issue_description/0, issue_info/0, issue_report/0 ]).



% Checking:
-export([ check_ast/1,
		  check_line/2,
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
-export([ write_ast_to_file/2 ]).


% Shorthands:

-type void() :: basic_utils:void().
-type module_name() :: basic_utils:module_name().

-type ustring() :: text_utils:ustring().
-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type file_name() :: file_utils:file_name().

-type ast() :: ast_base:ast().
-type form() :: ast_base:form().
-type line() :: ast_base:line().
-type form_context() :: ast_base:form_context().
-type ast_transforms() :: ast_transform:ast_transforms().


% Checking section.


% Checks whether specified AST is legit: lints it.
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
	% however some are legit (ex: 'type void() undefined'), so we must let them
	% go through:
	%
	case erl_lint:module( AST ) of

		{ ok, _Warnings=[] } ->
			%display_trace(
			% "(no warning or error emitted)~n" ),
			ok;

		{ ok, Warnings } ->
			%display_error(
			%  "Warnings, reported as errors: ~p~n",
			%		   [ Warnings ] ),
			interpret_issue_reports( Warnings ),
			%exit( warning_reported );
			warning_reported;

		{ error, Errors, _Warnings=[] } ->
			%display_error( "Errors reported: ~p~n",
			%  [ Errors ] ),
			interpret_issue_reports( Errors ),
			%exit( error_reported );
			error_reported;

		{ error, Errors, Warnings } ->
			%display_error( "Errors reported: ~p~n",
			%  [ Errors ] ),
			interpret_issue_reports( Errors ),

			%display_error(
			%  "Warnings, reported as errors: ~p~n", [ Warnings ] ),
			interpret_issue_reports( Warnings ),
			%exit( error_reported )
			error_reported

	end.



% Interprets specified list of issue reports.
-spec interpret_issue_reports( [ issue_report() ] ) -> void().
interpret_issue_reports( _IssueReports=[] ) ->
	% Should never happen:
	display_info( "(no remark emitted)" );

% No need to further special-case the number of issue reports, as it is not
% meaningful (one may include an arbitrary long list):

%interpret_issue_reports( _IssueReports=[ OneIssueReport ] ) ->
%	interpret_issue_report( OneIssueReport );

interpret_issue_reports( IssueReports ) ->

	[ interpret_issue_report( R ) || R <- IssueReports ].

	%text_utils:format( "~B remarks: ~ts", [ length( IssueReports ),
	%					text_utils:strings_to_string( ReportStrings ) ] ).


% Interprets specific issue report.
-spec interpret_issue_report( issue_report() ) -> void().
interpret_issue_report( _IssueReport={ Filename, IssueInfos } ) ->

	% We could normalise it instead, yet file_utils would become a dependency:
	CanonicFilename = filename:basename( Filename ),

	[ interpret_issue_info( CanonicFilename, E ) || E <- IssueInfos ].

	%text_utils:format( "in file '~ts': ~ts", [ CanonicFilename,
	%		   text_utils:strings_to_string( IssueStrings ) ] ).



% Interprets specific error description.
-spec interpret_issue_info( file_name(), issue_info() ) -> void().
interpret_issue_info( Filename,
					  _IssueInfo={ Line, DetectorModule, IssueDesc } ) ->

	% Module is the detecting one, typically erl_lint:
	%text_utils:format( "line #~B, module '~p', ~ts", [ Line, Module,
	%						interpret_issue_description( IssueDesc ) ] ).

	%text_utils:format( "line #~B: ~ts", [ Line,
	%		interpret_issue_description( IssueDesc, DetectorModule ) ] ).

	io:format( "~ts:~B: ~ts~n", [ Filename, Line,
			interpret_issue_description( IssueDesc, DetectorModule ) ] ).



% Interprets specific issue description, detected by specified module.
%
% Note: full control is offered here to enrich this function at will, if wanted.
%
-spec interpret_issue_description( issue_description(), module_name() ) ->
										ustring().
interpret_issue_description( IssueDescription, DectectorModule ) ->
	%For example, the detector module may be erl_lint:
	DectectorModule:format_error( IssueDescription ).




% Checks that specified line reference is legit.
-spec check_line( term(), form_context() ) -> line().
check_line( Line, _Context ) when is_integer( Line ) andalso Line >= 0 ->
	Line;

check_line( Other, Context ) ->
	% Not raise_error/2:
	throw( { invalid_line, Other, Context } ).



% Checks that specified module name is legit.
-spec check_module_name( term() ) -> module_name().
check_module_name( Name ) ->
	check_module_name( Name, _Context=undefined ).




% Checks that specified module name is legit.
-spec check_module_name( term(), form_context() ) -> module_name().
check_module_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_module_name( Other, Context ) ->
	raise_error( [ invalid_module_name, Other ], Context ).



% Checks that specified inline options are legit.
-spec check_inline_options( term() ) -> [ meta_utils:function_id() ].
check_inline_options( FunIds ) ->
	check_inline_options( FunIds, _Context=undefined ).


% Checks that specified inline options are legit.
-spec check_inline_options( term(), form_context() ) ->
									[ meta_utils:function_id() ].
check_inline_options( FunIds, Context ) when is_list( FunIds ) ->
	ast_function:check_function_ids( FunIds, Context );

check_inline_options( Other, Context ) ->
	raise_error( [ invalid_inline_options, Other ], Context ).




% Checks that specified (function or type) arity is legit.
-spec check_arity( term() ) -> arity().
check_arity( Arity ) ->
	check_arity( Arity, _Context=undefined ).


% Checks that specified (function or type) arity is legit.
-spec check_arity( term(), form_context() ) -> arity().
check_arity( Arity, _Context ) when is_integer( Arity ) andalso Arity >= 0 ->
	Arity;

check_arity( Other, Context ) ->
	raise_error( [ invalid_arity, Other ], Context ).



% Conversion section.


% Reads specified Erlang source file (*.erl) and returns the corresponding AST,
% based on default preprocessor options.
%
% For example useful to debug a parse transform first separately from the
% compile pipe-line, relying here on the usual, convenient error management
% instead of having little informative messages like: 'undefined parse transform
% 'foobar'' as soon as a call to a non-existing module:function/arity is made.
%
-spec erl_to_ast( file_name() ) -> ast().
erl_to_ast( ErlSourceFilename ) ->
	erl_to_ast( ErlSourceFilename, _PreprocessorOptions=[] ).



% Reads specified Erlang source file (*.erl) and returns the corresponding AST,
% based on specified preprocessor (eep) options.
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



% Reads the specified BEAM file (expected to be compiled with debug information)
% and returns the corresponding AST.
%
% Note that the filename must be a relative or absolute path pointing directly
% to the BEAM file (it is not searched through the code path).
%
-spec beam_to_ast( file_name() ) -> ast().
beam_to_ast( BeamFilename ) ->

	% We do not use functions from other Myriad modules here (ex: file_utils) as
	% they are not expected to be built yet (they will be built with the myriad
	% parse transform afterwards).
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
	%			labeled_exports, imports, indexed_imports, locals,
	%			labeled_locals, atoms ],

	% Just the code AST:
	Chunks = [ abstract_code ],

	% Everything but the code AST:
	% OtherChunks = [ attributes, compile_info, exports,
	%				  labeled_exports, imports, indexed_imports, locals,
	%				  labeled_locals, atoms ],

	%Options = [ allow_missing_chunks ],

	Options=[],

	MyriadCryptoKeyFun = fun( init ) ->
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


% Converts the specified Erlang term (ex: the float '42.0') into a corresponding
% form (ex: '{ float, _Line=0, 42.0 }').
%
-spec term_to_form( term() ) -> form().
term_to_form( Term ) ->

	case erl_syntax:abstract( Term ) of

		% Either the doc or the type information for erl_syntax:abstract/1 is
		% incorrect:

		%badarg ->
		%	throw( { term_abstraction_failed, Term } );

		SyntaxTree ->

			% Could be used with erl_syntax:is_tree/1:
			% case erl_syntax:revert( SyntaxTree ) of...
			erl_syntax:revert( SyntaxTree )

	end.



% Converts a list of names of variables into the corresponding AST.
%
% Ex: if wanting to specify '[V1, Alpha, A]', we have: variable_names_to_ast(
% ["V1", "Alpha", "A"], _Line=0) = [ {cons,0, {var,0,'V1'},
% {cons,0,{var,0,'Alpha'}, {cons,0,{var,0,'A'}, {nil,0} } } } ]
%
-spec variable_names_to_ast( [ ustring() ], line() ) -> ast().
variable_names_to_ast( VariableNames, Line ) ->

	% Could be done directly recursively by incrementally 'consing' reversed
	% list.

	NameListString = "[ " ++ text_utils:join( ", ",  VariableNames ) ++ " ].",

	string_to_expressions( NameListString, Line ).



% Converts the specified source code of a form (as a string) into its
% corresponding abstract form (assuming being in line #1).
%
% Ex: string_to_form( "f() -> hello_world." ) returns
%   { function, 1, f, 0, [ { clause, 1, [], [], [ {atom,1,hello_world} ] } ] }
%
-spec string_to_form( ustring() ) -> form().
string_to_form( FormString ) ->
	string_to_form( FormString, _Loc=1 ).



% Converts the specified source code of a form (i.e., a string) into its
% corresponding abstract form.
%
% Ex: string_to_form( "f() -> hello_world.", 42 ) returns
%   { function, 1, f, 0, [ { clause, 42, [], [], [ {atom,1,hello_world} ] } ] }
%
-spec string_to_form( ustring(), ast_base:file_loc() ) -> form().
string_to_form( FormString, Location ) ->

	% First get Erlang tokens from that string:
	Tokens = case erl_scan:string( FormString, Location ) of

		% Ex: [{atom,1,f},{'(',1},{')',1},{'->',1},{atom,1,hello_world},{dot,1}]
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



% Converts the specified source code of a list of expressions (i.e., a string)
% into its corresponding AST (assuming being in line #1).
%
% Ex: string_to_expressions( "[{a, 1}, foobar ]" ) returns
%   [ {cons, 1, {tuple, 1, [ {atom,1,a}, {integer,1,1} ]},
%     {cons, 1, {atom,1,foobar}, {nil,1}} } ]
%
-spec string_to_expressions( ustring() ) -> ast().
string_to_expressions( ExpressionString ) ->
	string_to_expressions( ExpressionString, _Loc=1 ).



% Converts the specified source code of a term (i.e., a string) and a location
% into the corresponding abstract form.
%
% Ex: string_to_expressions( "[{a, 1}, foobar]", _Loc=42 ) returns
%   [ {cons, 42, {tuple, 42, [ {atom,42,a}, {integer,42,1} ]},
%     {cons, 42, {atom,42,foobar}, {nil,42} }} ]
%
-spec string_to_expressions( ustring(), ast_base:file_loc() ) -> ast().
string_to_expressions( ExpressionString, Location ) ->

	% First get Erlang tokens from that string:
	Tokens = case erl_scan:string( ExpressionString, Location ) of

		% Ex: [ {'[',42}, {'{',42}, {atom,42,a}, {',',42}, {integer,42,1},
		% {'}',42}, {',',42}, {atom,42,foobar}, {']',42} ]
		{ ok, Toks, _EndLocation } ->
			%display_debug( "Tokens: ~p", [ Toks ] ),
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



% Converts the specified source code of a term (i.e., a string) into its
% corresponding value.
%
% Ex: string_to_value( "[ {tiger,[lion,leopard]} ]" ) returns the
% [{tiger,[lion,leopard]}] term.
%
-spec string_to_value( ustring() ) -> term().
string_to_value( ExpressionString ) ->

	% We automatically add the necessary final dot:
	[ Expr ] = string_to_expressions( ExpressionString ++ "." ),

	{ value, Result, _NewBindings } = erl_eval:expr( Expr, _Bindings=[] ),

	Result.




% Subsection for trace outputs that are specific to parse-transforms.


% Displays specified text as debug.
-spec display_debug( ustring() ) -> void().
display_debug( String ) ->
	io:format( "[debug] ~ts~n", [ String ] ).


% Displays specified formatted text as debug.
-spec display_debug( format_string(), [ term() ] ) ->
						  void().
display_debug( FormatString, Values ) ->
	display_debug( io_lib:format( FormatString, Values ) ).



% Displays specified text as info.
-spec display_info( ustring() ) -> void().
display_info( String ) ->
	io:format( "[info] ~ts~n", [ String ] ).


% Displays specified formatted text as info.
-spec display_info( format_string(), [ term() ] ) -> void().
display_info( FormatString, Values ) ->
	display_info( io_lib:format( FormatString, Values ) ).



% Displays specified text as notice.
-spec display_notice( ustring() ) -> void().
display_notice( String ) ->
	io:format( "[notice] ~ts~n", [ String ] ).


% Displays specified formatted text as notice.
-spec display_notice( format_string(), [ term() ] ) -> void().
display_notice( FormatString, Values ) ->
	display_notice( io_lib:format( FormatString, Values ) ).



% Displays specified text as warning.
-spec display_warning( ustring() ) -> void().
display_warning( String ) ->
	io:format( "[warning] ~ts~n", [ String ] ).


% Displays specified formatted text as warning.
-spec display_warning( format_string(), [ term() ] ) -> void().
display_warning( FormatString, Values ) ->
	display_warning( io_lib:format( FormatString, Values ) ).



% Displays specified text as error.
-spec display_error( ustring() ) -> void().
display_error( String ) ->
	io:format( "~n[error] ~ts~n", [ String ] ).


% Displays specified formatted text as error.
-spec display_error( format_string(), [ term() ] ) -> void().
display_error( FormatString, Values ) ->
	display_error( io_lib:format( FormatString, Values ) ).



% Displays specified text as critical.
-spec display_critical( ustring() ) -> void().
display_critical( String ) ->
	io:format( "[critical] ~ts~n", [ String ] ).


% Displays specified formatted text as critical.
-spec display_critical( format_string(), [ term() ] ) -> void().
display_critical( FormatString, Values ) ->
	display_critical( io_lib:format( FormatString, Values ) ).



% Displays specified text as alert.
-spec display_alert( ustring() ) -> void().
display_alert( String ) ->
	io:format( "[alert] ~ts~n", [ String ] ).


% Displays specified formatted text as alert.
-spec display_alert( format_string(), [ term() ] ) -> void().
display_alert( FormatString, Values ) ->
	display_alert( io_lib:format( FormatString, Values ) ).



% Displays specified text as emergency.
-spec display_emergency( ustring() ) -> void().
display_emergency( String ) ->
	io:format( "[emergency] ~ts~n", [ String ] ).


% Displays specified formatted text as emergency.
-spec display_emergency( format_string(), [ term() ] ) -> void().
display_emergency( FormatString, Values ) ->
	display_emergency( io_lib:format( FormatString, Values ) ).




% Notifies a warning, with specified context.
-spec notify_warning( [ term() ], form_context() ) -> void().
notify_warning( Elements, Context ) ->

	case get_elements_with_context( Elements, Context ) of

		% Supposedly a string:
		[ SingleElement ] when is_list( SingleElement ) ->
			display_warning( "~ts", [ SingleElement ] );

		AllElements ->
			display_warning( "~p", [ AllElements ] )

	end.



% Raises a (compile-time, rather ad hoc) error when applying a parse transform,
% to stop the build on failure and report the actual error, thanks to the
% specified term (often, a list of error elements).
%
% Used to be a simple throw, but then for parse transforms the error message was
% garbled in messages like:
%
% """
% internal error in lint_module;
% crash reason: function_clause
%
%  in function  erl_lint:'-compiler_options/1-lc$^0/1-0-'/1
%     called as erl_lint:'-compiler_options/1-lc$^0/1-0-'({
% table_type_defined_more_than_once,{line,12},foo_hashtable,bar_hashtable})
%
% Note:
% - this function is used to report errors detected by Myriad itself (not by the
% Erlang toolchain)
% - prefer using raise_usage_error/* to report errors in a more standard,
% convenient way
%
-spec raise_error( term() ) -> no_return().
raise_error( ErrorTerm ) ->

	%throw( ErrorTerm )
	%display_error( "~p", [ ErrorTerm ] ),

	% Does not add any information (just non-relevant erl_parse, epp
	% etc. state):
	%
	%erlang:exit( { ErrorTerm, erlang:get_stacktrace() } ).

	%erlang:exit( ErrorTerm ).

	% Possibly a list of elements:
	raise_error( ErrorTerm, _Context=undefined ).



% Raises an error, with specified context, thanks to the specified term (often,
% a list of error elements), from the Myriad layer.
%
% Ex: raise_error( [ invalid_module_name, Other ], _Context=112 ) shall
% result in throwing { invalid_module_name, Other, { line, 112 } }.
%
% Note:
% - this function is used to report errors detected by Myriad itself (not by the
% Erlang toolchain)
% - prefer using raise_usage_error/* to report errors in a more standard,
% convenient way
%
-spec raise_error( term(), basic_utils:maybe( form_context() ) ) -> no_return().
raise_error( ErrorTerm, Context ) ->
	raise_error( ErrorTerm, Context, _OriginLayer="Myriad" ).



% Raises an error, with specified context, from the specified layer (expected to
% be above Myriad).
%
% Ex: raise_error( [ invalid_module_name, Other ], _Context=112,
% _OriginLayer="FooLayer" ) shall result in throwing { invalid_module_name,
% Other, { line, 112 } }.
%
% Note:
% - this function is used to report errors detected by Myriad itself (not by the
% Erlang toolchain)
% - prefer using raise_usage_error/* to report errors in a more standard,
% convenient way
%
-spec raise_error( term(), basic_utils:maybe( ast_base:source_context() ),
				   basic_utils:layer_name() ) -> no_return();
				 ( ustring(), ast_transforms(), line() ) -> no_return().
raise_error( Message, #ast_transforms{ transformed_module_name=ModName },
			 Line ) ->
	io:format( "~ts.erl:~B: ~ts~n", [ ModName, Line, Message ] ),
	halt( 5 );

raise_error( Message, Context, OriginLayer ) ->

	%trace_utils:debug_fmt( "Message: ~p, Context: ~p, Layer: ~p",
	%						[ Message, Context, OriginLayer ] ),

	Prefix = case Context of

		undefined ->
			"Error";

		{ Filename, Line } ->
			io_lib:format( "~ts:~B: error", [ Filename, Line ] );

		Filename when is_binary( Filename ) ->
			io_lib:format( "Error in ~ts", [ Filename ] );

		Line when is_integer( Line ) ->
			io_lib:format( "Error at line ~B", [ Line ] );

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

	DisplayStacktrace = true,
	%DisplayStacktrace = false,

	try

		case DisplayStacktrace of

			true ->
				% To get a stack trace:
				throw( { ast_transformation_failed_in, OriginLayer } );

			false ->
				ok

		end

	catch

		% Class is 'throw', R is what we just threw:

		% Pre-21.0 code:
		%_C:_R ->
		%
		%	% Removing useless {ast_utils,raise_error,2,...:
		%	ActualStackTrace = tl( erlang:get_stacktrace() ),

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

interpret_stack_trace( _StackTrace=[ H | T ], Acc, Count ) ->
	Text = io_lib:format( "~p~n", [ H ] ),
	interpret_stack_trace( T, [ Text | Acc ], Count+1 ).




% Raises a (compile-time, relatively standard) user-related error, with
% specified source context, to stop the build on failure and report adequately
% the actual error to the user.
%
-spec raise_usage_error( format_string(), format_values(), file_name() ) ->
								no_return().
raise_usage_error( ErrorFormatString, ErrorValues, Filename ) ->
	raise_usage_error( ErrorFormatString, ErrorValues, Filename, _Line=0 ).



% Raises a (compile-time, relatively standard) user-related error, with
% specified source context, to stop the build on failure and report adequately
% the actual error to the user.
%
-spec raise_usage_error( format_string(), format_values(), file_name(),
						 ast_base:line() ) -> no_return().
raise_usage_error( ErrorFormatString, ErrorValues, Filename,
				   _Line=undefined ) ->
	raise_usage_error( ErrorFormatString, ErrorValues, Filename,
					   _ActualLine=0 );

raise_usage_error( ErrorFormatString, ErrorValues, ModuleName, Line )
  when is_atom( ModuleName ) ->
	Filename = io_lib:format( "~ts.erl", [ ModuleName ] ),
	raise_usage_error( ErrorFormatString, ErrorValues, Filename, Line );

raise_usage_error( ErrorFormatString, ErrorValues, Filename, Line ) ->

	ErrorString = io_lib:format( ErrorFormatString, ErrorValues ),

	io:format( "~ts:~B: ~ts~n", [ Filename, Line, ErrorString ] ),

	% Almost the only way to stop the processing of the AST:
	halt( 5 ).



% Returns an AST form in order to raise a (compile-time, standard) error when
% applying a parse transform, to stop the build on failure and report the actual
% error.
%
% The specified error term will be transformed by the specified module into a
% (textual) error message (see format_error/1), and then will be reported as
% originating from the specified line in the source file of the module being
% compiled.
%
-spec get_error_form( basic_utils:error_reason(), module_name(), line() ) ->
							form().
get_error_form( ErrorTerm, FormatErrorModule, Line ) ->

	% Actually the most standard way of reporting an error seems to insert a
	% dedicated form in the AST.

	% May ultimately report (thanks to ?MODULE:format_error/1), when compiling a
	% foobar module and if:
	%
	% - Line is 15
	%
	% - 'apply( FormatErrorModule, format_error, [ ErrorTerm ] )' is "my error
	% message":
	%
	% the following error message: "foobar:15: my error message".
	%
	{ error, { Line, FormatErrorModule, ErrorTerm } }.



% This function (whose name is standard, conventional) is to be defined on a
% per-module basis (typically in the module defining the parse transform being
% applied) and allows to convert error terms (that are, here, related to
% parse-transforms) into textual messages that can be output by the build chain.
%
-spec format_error( basic_utils:error_reason() ) -> ustring().
format_error( ErrorTerm ) ->
	% Of course this is just an example:
	text_utils:format( "my ast_utils error reported: ~ts", [ ErrorTerm ] ).



% Returns error/warning elements including specified context.
%
% (helper)
%
-spec get_elements_with_context( [ term() ], ast_base:form_context() ) ->
										[ term() ].
get_elements_with_context( Elements, _Context=undefined ) ->
	Elements;

get_elements_with_context( Elements, _Context={ FilePath, Line } )
  when is_binary( FilePath ) andalso is_integer( Line ) ->
	%Elements ++ [ { file, text_utils:binary_to_string( FilePath ) },
	%			  { line, Line } ];

	% We mimic the default error formatting so that tools (like IDE) have a
	% chance to automatically point to the right location in the sources:
	%
	Prefix = io_lib:format( "~ts:~B: ",
							[ text_utils:binary_to_string( FilePath ), Line ] ),
	[ Prefix | Elements ];

get_elements_with_context( Elements, _Context=Line ) when is_integer( Line ) ->
	Elements ++ [ { line, Line } ];

get_elements_with_context( Elements, _Context=FilePath )
  when is_binary( FilePath ) ->
	Prefix = io_lib:format( "~ts:0: ",
							[ text_utils:binary_to_string( FilePath ) ] ),
	[ Prefix | Elements ];

get_elements_with_context( Elements, Context ) ->
	% No list_utils module used from this module:
	Elements ++ [ Context ].



% Writes specified AST into specified (text) file.
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
