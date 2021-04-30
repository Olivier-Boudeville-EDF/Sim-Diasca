% Copyright (C) 2014-2021 Olivier Boudeville
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
% Creation date: Wednesday, December 24, 2014.


% Centralises, on behalf of the WOOPER parse transform, the support for method
% management.
%
-module(wooper_method_management).


-export([ manage_methods/1,

		  get_wooper_transform_table/0,

		  % Sharable, top-level transformers:
		  clause_transformer/2, body_transformer/2, call_transformer/4,
		  if_transformer/3, case_transformer/4, simple_receive_transformer/3,
		  receive_with_after_transformer/5, try_transformer/6,
		  catch_transformer/3,

		  get_blank_transformation_state/0, get_blank_transformation_state/1,
		  ensure_exported/2, ensure_exported_at/2, ensure_all_exported_in/2,
		  methods_to_functions/5,
		  raise_no_implementation_error/6,
		  take_spec_into_account/6, get_info_from_clause_spec/3,
		  check_clause_spec/5, check_state_argument/3,
		  function_nature_to_string/1,

		  function_to_request_info/2, request_to_function_info/2,
		  function_to_oneway_info/2, oneway_to_function_info/2,
		  function_to_static_info/2, static_to_function_info/2,

		  format_log/2 ]).


% For the function_info record:
-include_lib("myriad/include/ast_info.hrl").

% For the ast_transforms record:
-include_lib("myriad/include/ast_transform.hrl").

% for the rec_guard macro:
-include_lib("myriad/include/ast_utils.hrl").


% For the class_info record:
-include("wooper_info.hrl").


% The (WOOPER-level) nature of a given Erlang function.
%
% ('throw' designates a function whose all clauses throw an exception, and as
% such may be temporarily not identified in terms of nature)
%
-type function_nature() :: 'constructor' | 'destructor'
			| 'request' | 'oneway' | 'static' | 'function' | 'throw'.


-export_type([ function_nature/0 ]).


% Shorthands:

-type location() :: ast_base:form_location().
-type line() :: ast_base:line().

-type function_info() :: ast_info:function_info().
-type marker_table() :: ast_info:section_marker_table().
-type function_table() :: ast_info:function_table().

-type function_id() :: meta_utils:function_id().
-type clause_def() :: meta_utils:clause_def().

-type ast_transforms() :: ast_transform:ast_transforms().
-type ast_transform_table() :: ast_transform:ast_transform_table().

-type ast_expression() :: ast_expression:ast_expression().

-type ast_body() :: ast_clause:ast_body().
-type ast_case_clause() :: ast_clause:ast_case_clause().
-type ast_clause() :: ast_clause:ast_clause().
-type ast_if_clause() :: ast_clause:ast_if_clause().

-type classname() :: wooper:classname().
-type function_export_set() :: wooper:function_export_set().

-type compose_pair() :: wooper_parse_transform:compose_pair().

-type request_info() :: wooper_info:request_info().
-type oneway_info() ::  wooper_info:oneway_info().
-type static_info() ::  wooper_info:static_info().

-type request_table() :: wooper_info:request_table().
-type oneway_table() :: wooper_info:oneway_table().
-type static_table() :: wooper_info:static_table().
-type method_qualifiers() :: wooper:method_qualifiers().


% Comment to disable logging (too detailed, almost untractable even to display):
%-define( log_traversal, ).

-ifdef(log_traversal).

 -define(debug,trace_utils:debug).
 -define(debug_fmt,trace_utils:debug_fmt).

 -define(trace,trace_utils:info).
 -define(trace_fmt,trace_utils:info_fmt).

-else. % log_traversal

 -define(debug,trace_utils:void).
 -define(debug_fmt,trace_utils:void_fmt).

 -define(trace,trace_utils:void).
 -define(trace_fmt,trace_utils:void_fmt).

-endif. % log_traversal



% Implementation notes:

% To determine whether a function is a method and, if yes, of what kind it is
% (request, oneway, etc.), we have to traverse recursively at least one of its
% clauses until finding at least one final expression in order to examine, check
% and possibly transform any method terminator found. We chose to traverse all
% clauses in order to catch any inconsistency in the user code.
%
% Recursing in nested local calls is not needed here, as by convention the
% method terminators should be local to the method body.
%
% For that we can either follow exactly the AST legit structure that we can
% foresee (typically based on http://erlang.org/doc/apps/erts/absform.html), at
% the risk of rejecting correct code - should our traversal be not perfect, or
% we can "blindly" rewrite calls for example corresponding to
% wooper:return_state_result( S, R ) as { S, R } (and check we have no
% incompatible method terminators).
%
% We preferred initially here the latter solution (lighter, simpler, safer), but
% we had already a full logic (in Myriad's meta) to traverse and transform ASTs
% or part thereof, so we relied on it for the best, now that it has been
% enriched so that it can apply and maintain a transformation state.
%
% A combination of ast_transform:get_remote_call_transform_table/1 with
% ast_expression:transform_expression/2 would have been close to what we needed,
% yet we do not want to replace a *call* by another, we want it to be replaced
% by a tuple creation, i.e. an expression. Moreover we need to be stateful, to
% remember any past method terminator and check their are consistent with the
% newer ones being found.
%
% Finally, a difficulty is that normal functions by nature do not use
% terminators, whereas, when handling a method, for example a clause not using
% terminators among legit clauses shall be caught, despite the lack of an
% explicit trigger. Another difficulty is to handle throws, which are compatible
% with all function natures, and thus do not teach anything - even if they can
% constitue the sole clauses of a given function (any type spec thereof is then
% needed to remove this ambiguity).

% So, here are the WOOPER traversal rules to cover these needs:
%
% - as always with Myriad, it is a depth-first, integral, single-pass traversal;
% however we do not attempt anymore to merge this WOOPER pass with the Myriad
% one, as for example calls into case patterns are irrelevant here
%
% - initially, the function nature is 'undefined'
%
% - we traverse all bodies of all function clauses

% - when traversing a body, only its last expression (if any) is of interest for
% WOOPER (all preceding expressions are thus skipped, WOOPER-wise)
%
% - as soon an element is found (ex: method terminator, throw), the
% corresponding function nature supersedes the 'undefined' status
%
% - all (non-undefined) natures (resulting from method terminators) supersede
% 'throw'
%
% - returning from a transformation with an 'undefined' nature implies the new
% nature is 'function' (there is no method terminator nor throw in any nested
% branch; it is a "by default" property)
%
% - the traversal does not recurse in calls, as by design a WOOPER terminator is
% to be the most global one (i.e. the first one in any function composition) -
% however deeply nested in branching clauses

% Said otherwise, the traversal starts with the function clauses, which are
% managed by our clause_transformer/2, in charge of calling body_transformer/3.
% Should no method terminator nor throw be found, the function nature is still
% at 'undefined', so we assign it to 'function'.



% Extracts the methods found in the specified function table, transforms them,
% and interprets that information to update the specified class information.
%
% Returns an updated pair thereof.
%
-spec manage_methods( compose_pair() ) -> compose_pair().
manage_methods( { CompleteFunctionTable,
				  ClassInfo=#class_info{ class={ Classname, _ClassForm },
										 requests=RequestTable,
										 oneways=OnewayTable,
										 statics=StaticTable,
										 markers=MarkerTable } } ) ->

	AllFunEntries = table:enumerate( CompleteFunctionTable ),

	% We used to filter-out the WOOPER builtin functions (based on
	% wooper_info:get_wooper_builtins/0), as they would not teach WOOPER
	% anything about the class at hand, yet we want them to be correctly
	% identified by WOOPER (they are of different natures); moreover, not all of
	% them are already in a final form (ex: they still use method terminators),
	% so their processing shall not be skipped.

	ExportLoc = ast_info:get_default_export_function_location( MarkerTable ),

	% Determined once for all:
	WOOPERExportSet = wooper:get_exported_functions_set(),

	% FunctionTable starts from scratch as all functions are to be found in
	% AllFunEntries:
	%
	{ NewFunctionTable, NewRequestTable, NewOnewayTable, NewStaticTable } =
		sort_out_functions( AllFunEntries, _FunctionTable=table:new(),
			RequestTable, OnewayTable, StaticTable, Classname, ExportLoc,
			WOOPERExportSet ),

	% Split as {Functions, Methods}:
	{ NewFunctionTable, ClassInfo#class_info{ requests=NewRequestTable,
											  oneways=NewOnewayTable,
											  statics=NewStaticTable } }.



% Transforms and categorises each of the specified functions according to its
% real nature (ex: a given Erlang function may actually be a WOOPER oneway).
%
sort_out_functions( _FunEntries=[], FunctionTable, RequestTable, OnewayTable,
					StaticTable, _Classname, _ExportLoc, _WOOPERExportSet ) ->
	{ FunctionTable, RequestTable, OnewayTable, StaticTable };

% Checks that all sorted functions have an actual implementation:
sort_out_functions( _FunEntries=[ { _FunId={ FName, Arity },
								#function_info{ %line=undefined,
												clauses=[],
												spec=Spec } } | T ],
	FunctionTable, RequestTable, OnewayTable, StaticTable, Classname,
	_ExportLoc, _WOOPERExportSet ) when Spec =/= undefined ->

	AllTables = [ FunctionTable, RequestTable, OnewayTable, StaticTable ],

	SpecLine = case Spec of

		{ _Loc, { attribute, Line, spec, _Tuple } } ->
			Line;

		_ ->
			undefined

	end,

	raise_no_implementation_error( Classname, FName, Arity, SpecLine,
								   _RemainingFunEntries=T, AllTables );


sort_out_functions( _FunEntries=[ { FunId, FunInfo=#function_info{
											clauses=OriginalClauses,
											spec=Spec } } | T ],
					FunctionTable, RequestTable, OnewayTable, StaticTable,
					Classname, ExportLoc, WOOPERExportSet ) ->

	%?debug_fmt( "Examining Erlang function ~ts/~B", pair:to_list( FunId ) ),

	% We used to infer the function nature based on its first clause, and then
	% to make a custom full traversal to transform method terminators.
	%
	% Now we reuse the Myriad ast_transforms instead, and perform everything
	% (guessing/checking/transforming) in one pass:
	%
	{ NewClauses, FunNature, Qualifiers } = manage_method_terminators(
					OriginalClauses, FunId, Classname, WOOPERExportSet ),

	%?debug_fmt( "~p is a ~ts whose qualifiers are ~p.",
	%					   [ FunId, function_nature_to_string( FunNature ),
	%						 Qualifiers ] ),


	NewFunInfo = FunInfo#function_info{ clauses=NewClauses },

	% Last chance to determine the actual nature of a function reported as
	% 'throw': it may have a type spec to remove ambiguity; any spec is used
	% also to check consistency with the guessed elements:
	%
	{ FinalNature, FinalQualifiers } = take_spec_into_account( Spec, FunId,
								  FunNature, Qualifiers, Classname, FunInfo ),

	% Stores the result in the right category and recurses:
	case FinalNature of

		function ->

			NewFunctionTable =
				table:add_new_entry( FunId, NewFunInfo, FunctionTable ),

			sort_out_functions( T, NewFunctionTable, RequestTable, OnewayTable,
				StaticTable, Classname, ExportLoc, WOOPERExportSet );

		request ->

			check_state_argument( NewClauses, FunId, Classname ),

			ExportedFunInfo = ensure_exported_at( NewFunInfo, ExportLoc ),

			RequestInfo =
				function_to_request_info( ExportedFunInfo, FinalQualifiers ),

			NewRequestTable = table:add_new_entry( FunId, RequestInfo,
												   RequestTable ),

			sort_out_functions( T, FunctionTable, NewRequestTable, OnewayTable,
				StaticTable, Classname, ExportLoc, WOOPERExportSet );

		oneway ->

			check_state_argument( NewClauses, FunId, Classname ),

			ExportedFunInfo = ensure_exported_at( NewFunInfo, ExportLoc ),

			OnewayInfo =
				function_to_oneway_info( ExportedFunInfo, FinalQualifiers ),

			NewOnewayTable = table:add_new_entry( FunId, OnewayInfo,
												  OnewayTable ),

			sort_out_functions( T, FunctionTable, RequestTable, NewOnewayTable,
				StaticTable, Classname, ExportLoc, WOOPERExportSet );

		static ->

			ExportedFunInfo = ensure_exported_at( NewFunInfo, ExportLoc ),

			StaticInfo =
				function_to_static_info( ExportedFunInfo, FinalQualifiers ),

			NewStaticTable = table:add_new_entry( FunId, StaticInfo,
												  StaticTable ),

			sort_out_functions( T, FunctionTable, RequestTable, OnewayTable,
				NewStaticTable, Classname, ExportLoc, WOOPERExportSet )

	end;

sort_out_functions( _Functions=[ #function_info{ name=FunName,
												 arity=Arity } | _T ],
					_FunctionTable, _RequestTable, _OnewayTable,
					_StaticTable, Classname, _ExportLoc, _WOOPERExportSet ) ->

	% Error raised directly, could be appended to the class_info.errors:
	wooper_internals:raise_usage_error( "no clause found for ~ts/~B; function "
		"exported yet not defined?", [ FunName, Arity ], Classname ).



% Tries to locate among specified function entries and tables any implementation
% of the specified function with any (different) arity, and raises an
% appropriate exception.
%
% (helper, defined for reuse by upper layers)
%
raise_no_implementation_error( Classname, FName, FArity, MaybeLine,
							   FunctionEntries, Tables ) ->

	BaseMsg =
		"function ~ts/~B has a type specification, yet has never been defined",

	% Scans all known functions (hopefully does not include FArity):
	KnownArities = [ A || { { F, A }, _FI } <- FunctionEntries, F =:= FName ]
	  ++ list_utils:flatten_once(
		[ [ A || { F, A } <- table:keys( T ), F =:= FName ] || T <- Tables ] ),

	%trace_utils:debug_fmt( "KnownArities = ~w", [ KnownArities ] ),

	% As by design (on purpose) the type of the values held by the specified
	% tables is not known (and most probably is heterogeneous), there is no
	% simple way to extract the line number of, for example, a function
	% definition to point the error cursor to it.
	%
	FullMsg = BaseMsg ++ case KnownArities of

		[] ->
			text_utils:format( " (no function '~ts' defined, for any arity)",
								 [ FName ] );

		[ SingleArity ] ->
			% No function definition line easily obtainable:
			text_utils:format( ". Maybe this spec should correspond to "
							   "~ts/~B instead? ", [ FName, SingleArity ] );

		Arities ->
			ArStrs = [ text_utils:integer_to_string( A )
					   || A <- lists:sort( Arities ) ],
			text_utils:format( ". Maybe this spec should correspond to the "
				"function ~ts for one of its implemented arities, "
				"which are ~ts? ",
				[ FName, text_utils:strings_to_listed_string( ArStrs ) ] )

	end,

	ActualLine = case MaybeLine of

		undefined ->
			0;

		_ ->
			MaybeLine

	end,

	wooper_internals:raise_usage_error( FullMsg, [ FName, FArity ], Classname,
										ActualLine ).




% Checks that the method spec (if any) corresponds indeed to the right nature of
% function, i.e. relies on the right method terminators with the right
% qualifiers, and returns a corresponding pair.
%
% (helper)
%
-spec take_spec_into_account( maybe( ast_info:located_form() ), function_id(),
		function_nature(), method_qualifiers(), classname(),
		function_info() ) -> { function_nature(), method_qualifiers() }.
% Function specs are generally optional, yet are useful in all cases, and even
% needed in some ones (ex: what is the actual nature of a function whose all
% clauses throw?)
%
take_spec_into_account( _LocSpec=undefined, FunId, _FunNature=throw,
						_Qualifiers, Classname, FunInfo ) ->

	Line = case FunInfo#function_info.line of

		undefined ->
			0;

		L ->
			L

	end,

	wooper_internals:raise_usage_error(
	  "all clauses of ~ts/~B throw an exception; as a result, this "
	  "function can be of any nature. Please define a type specification for "
	  "that function in order to remove this ambiguity "
	  "(ex: use request_return/1 to mark it as a request).",
	  pair:to_list( FunId ), Classname, Line );


% Special case for a function nature detected as 'throw': any information
% collected from its spec is accepted as is (provided there is only one spec).
%
take_spec_into_account( _LocSpec={ _Loc,
		   { attribute, _, spec, { FunId, [ ClauseSpec ] } } },
						FunId, _FunNature=throw, _Qualifiers, Classname,
						_FunInfo ) ->
	get_info_from_clause_spec( ClauseSpec, FunId, Classname );


% Throw function with a spec comprising multiple clauses is not supported:
take_spec_into_account( _LocSpec={ _Loc,
		   { attribute, Line, spec, { FunId, _ClauseSpecs } } },
						_FunId, _FunNature=throw, _Qualifiers, Classname,
						FunInfo ) ->

	Line = case FunInfo#function_info.line of

		undefined ->
			0;

		L ->
			L

	end,

	wooper_internals:raise_usage_error(
	  "all clauses of the function ~ts/~B throw an exception; as a result, "
	  "this function can be of any nature. Its type specification shall "
	  "however comprise a single clause to remove this ambiguity.",
	  pair:to_list( FunId ), Classname, Line );


% No spec defined, non-throw, so input elements are accepted as such:
take_spec_into_account( _LocSpec=undefined, _FunId, FunNature, Qualifiers,
						_Classname, _FunInfo ) ->
	{ FunNature, Qualifiers };


% Spec available for a non-throw:
take_spec_into_account( _LocSpec={ _Loc,
					   { attribute, _, spec, { FunId, ClauseSpecs } } },
			_FunId, FunNature, Qualifiers, Classname, _FunInfo ) ->
	[ check_clause_spec( C, FunNature, Qualifiers, FunId, Classname )
	  || C <- ClauseSpecs ],
	% If check not failed, approved, so:
	{ FunNature, Qualifiers }.



% Returns a {FunctionNature, Qualifiers} pair deduced from specified clause
% spec.
%
get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, _, request_return, [ _RType ] } ] },
						   _FunId, _Classname ) ->
	{ request, [] };


get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, Line, request_return, RTypes } ] },
						   FunId, Classname ) ->
	wooper_internals:raise_usage_error(
	  "wrong arity of the specified WOOPER return type for the spec of ~ts/~B: "
	  "it should be request_return/1 (not request_return/~B).",
	  pair:to_list( FunId ) ++ [ length( RTypes ) ], Classname, Line );


get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, _, const_request_return,
						  [ _RType ] } ] }, _FunId, _Classname ) ->
	{ request, [ const ] };


get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, Line, const_request_return, RTypes } ] },
						   FunId, Classname ) ->
	wooper_internals:raise_usage_error(
	  "wrong arity of the specified WOOPER return type for the spec of ~ts/~B: "
	  "it should be const_request_return/1 (not const_request_return/~B).",
	  pair:to_list( FunId ) ++ [ length( RTypes ) ], Classname, Line );


get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, _, oneway_return, [] } ] },
						   _FunId, _Classname ) ->
	{ oneway, [] };


get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, Line, oneway_return, RTypes } ] },
						   FunId, Classname ) ->
	wooper_internals:raise_usage_error(
	  "wrong arity of the specified WOOPER return type for the spec of ~ts/~B: "
	  "it should be oneway_return/0 (not oneway_return/~B).",
	  pair:to_list( FunId ) ++ [ length( RTypes ) ], Classname, Line );


get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, _, const_oneway_return, [] } ] },
						   _FunId, _Classname ) ->
	{ oneway, [ const ] };


get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, Line, const_oneway_return, RTypes } ] },
						   FunId, Classname ) ->
	wooper_internals:raise_usage_error(
	  "wrong arity of the specified WOOPER return type for the spec of ~ts/~B: "
	  "it should be const_oneway_return/0 (not const_oneway_return/~B).",
	  pair:to_list( FunId ) ++ [ length( RTypes ) ], Classname, Line );


get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, _, static_return, [ _RType ] } ] },
						   _FunId, _Classname ) ->
	{ static, [] };

get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, _, static_void_return, [] } ] },
						   _FunId, _Classname ) ->
	{ static, [] };

get_info_from_clause_spec( _ClauseSpec={ type, _, 'fun',
	_Seqs=[ _TypeProductForArgs,
			_ResultType={ user_type, Line, static_return, RTypes } ] },
						   FunId, Classname ) ->
	wooper_internals:raise_usage_error(
	  "wrong arity of the specified WOOPER return type for the spec of ~ts/~B: "
	  "it should be static_return/1 (not static_return/~B).",
	  pair:to_list( FunId ) ++ [ length( RTypes ) ], Classname, Line );

% For the rest, we assume it designates plain function:
%
% (wrong spelling, arity, etc. could be detected here as well, like with
% check_clause_spec/5)
%
get_info_from_clause_spec( _ClauseSpec, _FunId, _Classname ) ->
	{ function, [] }.




% Checks the specified method clause spec.
%
% (helper)

%% For requests:

% Spec implies non-const request:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, request_return, [ _RType ] } ] },
	 _FunNature=request, Qualifiers, FunId, Classname ) ->

	case lists:member( const, Qualifiers ) of

		true ->
			wooper_internals:raise_usage_error( "the ~ts/~B request has been "
				"detected as const, however its spec uses request_return/1 "
				"instead of const_request_return/1.", pair:to_list( FunId ),
				Classname, Line );

		false ->
			ok

	end;


% Spec implies const request:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_request_return, [ _RType ] } ] },
	 _FunNature=request, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			ok;

		false ->
			wooper_internals:raise_usage_error( "the ~ts/~B request has been "
				"detected as non-const, however its spec uses "
				"const_request_return/1 instead of request_return/1.",
				pair:to_list( FunId ), Classname, Line )

	end;


% Spec implies (non-const) request whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, request_return, [ _RType ] } ] },
	 NonReqFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B has been detected as a ~ts, "
		"not as a (non-const) request, however its spec uses request_return/1.",
		pair:to_list( FunId )
			++ [ function_nature_to_string( NonReqFunNature ) ],
		Classname, Line );


% Spec implies (const) request whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_request_return, [ _RType ] } ] },
	 NonReqFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B has been detected as a ~ts, "
		"not as a (const) request, however its spec uses "
		"const_request_return/1.",
		pair:to_list( FunId )
			++ [ function_nature_to_string( NonReqFunNature ) ],
		Classname, Line );


% Wrong arity for request_return/1:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, request_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses request_return/~B, "
		"which does not exist; its correct arity is 1.",
		pair:to_list( FunId ) ++ [ length( Types ) ], Classname, Line );


% Wrong arity for const_request_return/1:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_request_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses const_request_return/~B, "
		"which does not exist; its correct arity is 1.",
		pair:to_list( FunId ) ++ [ length( Types ) ], Classname, Line );


% *_result used instead of *_return:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_request_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses the (unknown) "
		"'const_request_result' type: probably that "
		"const_request_return/1 was meant instead.",
		pair:to_list( FunId ), Classname, Line );


check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, request_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses the (unknown) "
		"'request_result' type: probably that "
		"request_return/1 was meant instead.",
		pair:to_list( FunId ), Classname, Line );



%% For oneways:

% Spec implies non-const oneway:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, oneway_return, [] } ] },
	 _FunNature=oneway, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			wooper_internals:raise_usage_error( "the ~ts/~B oneway has been "
				"detected as const, however its spec uses oneway_return/0 "
				"instead of const_oneway_return/0.", pair:to_list( FunId ),
				Classname, Line );

		false ->
			ok

	end;


% Spec implies const oneway:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_oneway_return, [] } ] },
	 _FunNature=oneway, Qualifiers, FunId, Classname ) ->
	case lists:member( const, Qualifiers ) of

		true ->
			ok;

		false ->
			wooper_internals:raise_usage_error( "the ~ts/~B oneway has been "
				"detected as non-const, however its spec uses "
				"const_oneway_return/0 instead of oneway_return/0.",
				pair:to_list( FunId ), Classname, Line )

	end;


% Spec implies (non-const) oneway whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, oneway_return, [] } ] },
	 NonOnwFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B has been detected as a ~ts, "
		"not as a (non-const) oneway, however its spec uses oneway_return/0.",
		pair:to_list( FunId )
			++ [ function_nature_to_string( NonOnwFunNature ) ],
		Classname, Line );


% Spec implies (const) oneway whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_oneway_return, [] } ] },
	 NonOnwFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B has been detected as a ~ts, "
		"not as a (const) oneway, however its spec uses const_oneway_return/0.",
		pair:to_list( FunId )
			++ [ function_nature_to_string( NonOnwFunNature ) ],
		Classname, Line );


% Wrong arity for oneway_return/0:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, oneway_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses oneway_return/~B, "
		"which does not exist; its correct arity is 0.",
		pair:to_list( FunId ) ++ [ length( Types ) ], Classname, Line );


% Wrong arity for const_oneway_return/0:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_oneway_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses const_oneway_return/~B, "
		"which does not exist; its correct arity is 0.",
		pair:to_list( FunId ) ++ [ length( Types ) ], Classname, Line );


% *_result used instead of *_return:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, const_oneway_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses the (unknown) "
		"'const_oneway_result' type: probably that "
		"const_oneway_return/0 was meant instead.",
		pair:to_list( FunId ), Classname, Line );


check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, oneway_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses the (unknown) "
		"'oneway_result' type: probably that "
		"oneway_return/0 was meant instead.",
		pair:to_list( FunId ), Classname, Line );



%% For static methods:


% Spec implies static method:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, static_return, [ _RType ] } ] },
	 _FunNature=static, _Qualifiers, _FunId, _Classname ) ->
	ok;


% Void return; correct arity for static_void_return/0, and it is a static method
% indeed:
%
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, _, static_void_return, _Types=[] } ] },
	 _FunNature=static, _Qualifiers, _FunId, _Classname ) ->
	ok;


% Spec implies static method whereas is not:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, static_return, [ _RType ] } ] },
	 NonStatFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B has been detected as a ~ts, "
		"not as a static method, however its spec uses static_return/1.",
		pair:to_list( FunId )
			++ [ function_nature_to_string( NonStatFunNature ) ],
		Classname, Line );


% Wrong arity for static_return/1:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, static_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses static_return/~B, "
		"which does not exist; its correct arity is 1.",
		[ length( Types ) | pair:to_list( FunId ) ], Classname, Line );


% Correct arity for static_void_return/0 - however it is not a static method
% here:
%
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, static_void_return, _Types=[] } ] },
	 FunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses static_void_return/0, "
		"whereas it is not detected as a static method (detected as ~ts). "
		"Maybe a call to the wooper:return_static_void() method terminator "
		"is lacking?",
		pair:to_list( FunId ) ++ [ FunNature ], Classname, Line );


% Wrong arity for static_void_return:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, static_void_return, Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses static_void_return/~B, "
		"which does not exist; its correct arity is 0.",
		pair:to_list( FunId ) ++ [ length( Types ) ], Classname, Line );


% *_result used instead of *_return:
check_clause_spec( { type, _, 'fun', _Seqs=[ _TypeProductForArgs,
	 _ResultType={ user_type, Line, static_result, _Types } ] },
	 _AnyFunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B uses the (unknown) "
		"'static_result' type: probably that "
		"static_return/1 was meant instead.",
		pair:to_list( FunId ), Classname, Line );



%% For functions, nothing special to check, except that they are not actually
%% faulty static methods:

check_clause_spec( { type, Line, 'fun',
		_Seqs=[ _TypeProductForArgs,
				_ResultType={remote_type,_,[ {atom,_,wooper},
											 {atom,_,static_return}, _ ] } ] },
				   _FunNature=function, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "~ts/~B is detected as a plain "
		"function whereas its spec uses a WOOPER static terminator type. "
		"Maybe it is actually a static method whose clause lacks a proper "
		"wooper:return_static/1 last call?", pair:to_list( FunId ), Classname,
		Line );

% Monomorphic types are licit (typically: wooper:state()):
check_clause_spec( { type, _Line, 'fun',
		_Seqs=[ _TypeProductForArgs,
				_ResultType={remote_type,_,[ {atom,_,wooper}, _, [] ] } ] },
				   _FunNature=function, _Qualifiers, _FunId, _Classname ) ->
	ok;


% Polymorphic types are probably a faulty WOOPER terminator (typically akin to
% wooper:return_static(term())):
%
check_clause_spec( { type, Line, 'fun',
		_Seqs=[ _TypeProductForArgs,
				 _ResultType={remote_type,_,[ {atom,_,wooper}, _, _ ] } ] },
				   _FunNature=function, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the type specification of ~ts/~B "
		"is not expected to rely on a terminator type prefixed with the wooper "
		"module (applying it in the context of a plain function is even "
		"more irrelevant).", pair:to_list( FunId ), Classname, Line );

check_clause_spec( { type, _, 'fun',
		_Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=function, _Qualifiers, _FunId, _Classname ) ->
	ok;


%% For unmatched spec returns:

% A method is not supposed to return explicity a state():
check_clause_spec( { type, Line, 'fun',
		_Seqs=[ _TypeProductForArgs,
				_ResultType={remote_type,_,[ {atom,_,wooper},
											 {atom,_,state},[] ] } ] },
				   _FunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the type specification of ~ts/~B "
		"is not expected to directly specify wooper:state() as a "
		"returned type. Maybe oneway_return() or alike was meant instead?",
		pair:to_list( FunId ), Classname, Line );

% Presumably a result spec wrongly qualified as wooper:S instead of S:
check_clause_spec( { type, Line, 'fun',
		_Seqs=[ _TypeProductForArgs,
				_ResultType={remote_type,_,[ {atom,_,wooper}, _, _ ] } ] },
				   _FunNature, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the type specification of ~ts/~B "
		"is not expected to rely on a terminator prefixed with the wooper "
		"module (hint: just remove 'wooper:').", pair:to_list( FunId ),
		Classname, Line );

% Presumably a rogue request (not using the right return type):
check_clause_spec( { type, Line, 'fun',
					 _Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=request, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the clauses of ~ts/~B indicate "
		"that this is a request, yet in the type specification no known "
		"request terminator is used (hint: request_return/1 or "
		"const_request_return/1 would be expected in this context).",
		pair:to_list( FunId ), Classname, Line );

% Rogue oneway:
check_clause_spec( { type, Line, 'fun',
		_Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=oneway, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the clauses of ~ts/~B indicate "
		"that this is a oneway, yet in the type specification no known "
		"oneway terminator is used (hint: oneway_return/0 or "
		"const_oneway_return/0 would be expected in this context).",
		pair:to_list( FunId ), Classname, Line );


% Rogue static method:
check_clause_spec( { type, Line, 'fun',
		_Seqs=[ _TypeProductForArgs, _ResultType ] },
				   _FunNature=static, _Qualifiers, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the clauses of ~ts/~B indicate "
		"that this is a static method, yet in the type specification no known "
		"static method terminator is used (hint: static_return/1 would be "
		"expected in this context).", pair:to_list( FunId ), Classname,
		Line );


check_clause_spec( { type, _, 'fun', [ _Args,
		_ResultType={ user_type, Line, TypeName, RTypes } ] },
				   FunNature, _Qualifiers, FunId, Classname ) ->
	%code_utils:display_stacktrace(),
	wooper_internals:raise_usage_error( "unexpected return type (~ts/~B) "
		"in the type specification of ~ts/~B (which is detected as a ~ts).",
		[ TypeName, length( RTypes ) | pair:to_list( FunId ) ]
						 ++ [ function_nature_to_string( FunNature ) ],
		Classname, Line );


check_clause_spec( _UnexpectedTypeForm, FunNature, _Qualifiers, FunId,
				   Classname ) ->
	wooper_internals:raise_usage_error( "unexpected return type in the type "
		"specification of ~ts/~B (which is detected as a ~ts).",
		pair:to_list( FunId ) ++ [ function_nature_to_string( FunNature ) ],
		Classname, _Line=0 ).



% Returns a textual description of the specified function nature.
-spec function_nature_to_string( maybe( function_nature() ) ) ->
										text_utils:ustring().
function_nature_to_string( request ) ->
	"request";

function_nature_to_string( oneway ) ->
	"oneway";

function_nature_to_string( static ) ->
	"static method";

function_nature_to_string( function ) ->
	"plain function";

function_nature_to_string( throw ) ->
	"throw-only function";

function_nature_to_string( undefined ) ->
	"undefined type of function";

% An unknown nature is not deemed an error here, so that upper layers do not
% have to define their own, very similar version of check_clause_spec/5 for
% example.
%
function_nature_to_string( Other ) when is_atom( Other ) ->
	text_utils:format( "function whose nature, '~ts', is not known of WOOPER",
					   [ Other ] ).




% Checks that, in the specified clauses of specified function (corresponding to
% a request or a oneway), the first parameter is 'State' indeed.
%
% Note: enforces a very welcome convention, but also complies with the
% expression that the support for example of const_return_result/1
% introduces (ex: { var, LineCall, 'State'} added in the AST, hence the enforced
% variable name).
%
-spec check_state_argument( [ clause_def() ], function_id(),
							classname() ) -> void().
check_state_argument( Clauses, FunId, Classname ) ->
	[ check_clause_for_state( C, FunId, Classname ) || C <- Clauses ].



% (helper)
check_clause_for_state(
  _Clause={ clause, _, _Params=[ {var,_,'State'} | _ ], _Guards, _Body },
  _FunId, _Classname ) ->
	ok;

% Tolerated iff throwing afterwards:
check_clause_for_state(
  _Clause={ clause, _, _Params=[ {var,_,'_State'} | _ ], _Guards, _Body },
  _FunId, _Classname ) ->
	ok;

check_clause_for_state(
  _Clause={ clause, _, _Params=[ {var,Line,NonState} | _ ], _Guards, _Body },
  FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the first parameter of this clause of "
		"method ~ts/~B shall be named 'State', not '~ts'.",
		pair:to_list( FunId ) ++ [ NonState ], Classname, Line );

% Should a non-var form be found, we were considering not halting the
% transformation (as the compiler would raise an error afterwards), however it
% would then report "variable 'State' is unbound", which is less clear than:
%
check_clause_for_state( _Clause, FunId, Classname ) ->
	wooper_internals:raise_usage_error( "the first parameter of this clause of "
		"method ~ts/~B shall be named 'State'.", pair:to_list( FunId ),
		Classname ).



% Infers the nature of the corresponding function and any relevant method
% qualifier(s), ensures that all method terminators correspond, and transforms
% them appropriately (for WOOPER), in one pass.
%
% We consider that no method is to be explicitly exported and that all actual
% clauses of a method must explicitly terminate with a WOOPER method terminator
% (the same for all clauses, except regarding constness), rather than calling an
% helper function that would use such a terminator (otherwise the nature of
% methods could not be auto-detected, as there would be no way to determine
% whether said helper should be considered as a method or not).
%
-spec manage_method_terminators( clause_def(), function_id(),
								 classname(), function_export_set()) ->
		{ clause_def(), function_nature(), method_qualifiers(),
		  function_export_set() }.
manage_method_terminators( _Clauses=[], FunId, Classname, _WOOPERExportSet ) ->

	% No easy way to search for other arities or to determine the line of the
	% corresponding export attribute:
	%
	wooper_internals:raise_usage_error(
	  "the function ~ts/~B is exported yet not defined.", pair:to_list( FunId ),
	  Classname );

manage_method_terminators( Clauses, FunId, Classname, WOOPERExportSet ) ->

	?trace_fmt( "Studying ~ts/~B...", pair:to_list( FunId ) ),

	TransformTable = get_wooper_transform_table(),

	Transforms = #ast_transforms{
	  transformed_module_name=Classname,
	  transform_table=TransformTable,
	  transformed_function_identifier=FunId,
	  transformation_state=get_blank_transformation_state( WOOPERExportSet ) },

	%?debug_fmt( "transforming now ~p.", [ FunId ] ),

	{ NewClauses, NewTransforms } =
		ast_clause:transform_function_clauses( Clauses, Transforms ),

	% Unless found different, a function is a (plain) function:
	{ FunNature, Qualifiers } =
			  case NewTransforms#ast_transforms.transformation_state of

		{ undefined, _, _WOOPERExportSet } ->
			%?debug_fmt( "~ts/~B detected as a plain function.",
			%					   pair:to_list( FunId ) ),

			{ function, _Qualifiers=[] };

		% Ex: { request, [ const ], _ }
		{ OtherNature, SomeQualifiers, _WOOPERExportSet } ->
			%?debug_fmt( "~ts/~B detected as: ~p (qualifiers: ~w)",
			%    pair:to_list( FunId ) ++ [ OtherNature, Qualifiers ] ),
			{ OtherNature, SomeQualifiers }

	end,

	{ NewClauses, FunNature, Qualifiers }.




% Returns a blank transformation state, based on the WOOPER exports.
%
% Defined to be safely reused from various locations.
%
get_blank_transformation_state() ->
	WOOPERExportSet = wooper:get_exported_functions_set(),
	get_blank_transformation_state( WOOPERExportSet ).



% Returns a suitable, blank transformation state, from specified export set.
%
% Defined to be safely reused from various locations.
%
get_blank_transformation_state( ExportSet ) ->
	{ _FunctionNature=undefined, _Qualifiers=[], ExportSet }.



% Returns the WOOPER-specific transform table.
-spec get_wooper_transform_table() -> ast_transform_table().
get_wooper_transform_table() ->

	% We define first the transformation functions in charge of the
	% guessing/checking/transforming of the method terminators.
	%
	% The clause transform-fun allows to handle plain functions, knowing that,
	% when no method terminator is found in a code branch, no specific behaviour
	% can be triggered (used to result in such branches to be basically
	% ignored).
	%
	% The body transform-fun will be used to skip all expressions of a body but
	% the last one, while the call transform-fun will be applied to call
	% expressions found there, as:
	%
	% { NewExpr, NewTransforms } = TransformFun( LineCall, FunctionRef,
	%                                            Params, Transforms )
	%
	% Transforms is an ast_transforms record that contains a
	% transformation_state field, which itself contains a value of type:
	%   { maybe( function_nature() ), method_qualifiers(),  } ).
	%
	% Indeed it starts as 'undefined', then the first terminal call branch of
	% the first clause allows to detect and set the function nature, and
	% afterwards all other branches are checked against it, and transformed.

	% Note that a member method is const iff all its clauses are const; so a
	% method is flagged içnitially as const depending on the constness of its
	% first clause, and loses any initial const status as soon as one clause
	% happens to be non-const (otherwise a non_const flag/qualifier would have
	% to be introduced).

	% So this (WOOPER-level) transformation is quite different (ex: partial vs
	% exhaustive) from the Myriad-level one that will be performed near the end
	% of the overall processing (merging these two would be tricky and would not
	% have much interest).

	% WOOPER traverses quite specifically the AST, so we override the default
	% traversal with transformation triggers (ex: we must trigger our
	% call-transformer only on final elements of bodies, not on test expressions
	% of cases):
	%
	table:new( [

			{ 'clause', fun clause_transformer/2 },

			% Finally the traversal is not driven by the overall Myriad logic:
			% we drive it explicitly here, so the standard body_transformer/2 is
			% expected never to be triggered: instead we explicitly trigger,
			% from the WOOPER transformers, our body_transformer/3 variation; we
			% nevertheless define the standard version to detect if ever it was
			% called, and in this case report a blocking error:
			%
			{ 'body', fun body_transformer/2 },

			% Expression-level triggers:
			{ 'call', fun call_transformer/4 },
			{ 'if', fun if_transformer/3 },
			{ 'case', fun case_transformer/4 },
			{ 'simple_receive', fun simple_receive_transformer/3 },
			{ 'receive_with_after', fun receive_with_after_transformer/5 },
			{ 'try', fun try_transformer/6 },
			{ 'catch', fun catch_transformer/3 } ] ).



% List of WOOPER-specific transformers.


% Drives the AST transformation of a clause.
%
% For any clause, we are to transform here, for WOOPER, only the last expression
% (not the preceding ones) of the body (hence not touching parameters or
% guards).
%
% The goal is to feed the call-transformer only with relevant expressions.
%
-spec clause_transformer( ast_clause(), ast_transforms() ) ->
								{ ast_clause(), ast_transforms() }.
clause_transformer(
  Clause={ clause, Line, Params, Guards, Body },
  Transforms ) ?rec_guard ->
  %Transforms=#ast_transforms{
		%transformed_function_identifier=FunId,
		%transformation_state={ InitialNature, InitialQualifiers,
		%					   WOOPERExportSet } } ) ->

	?debug_fmt( "Transforming for WOOPER clause ~p", [ Clause ] ),

	% No need to reset transformation state, as done by body_transformer/3:

	{ NewBody, BodyTransforms } = body_transformer( Body, Transforms, Line ),

	NewClause = { clause, Line, Params, Guards, NewBody },

	UpdatedTransforms = update_transformation_state( Transforms,
							BodyTransforms, Line ),

	{ NewClause, UpdatedTransforms }.



% The standard body transformer, expected never to be called due to the
% WOOPER-driven traversal of branching constructs.
%
-spec body_transformer( ast_body(), ast_transforms() ) ->
							{ ast_body(), ast_transforms() }.
body_transformer( Body, Transforms=#ast_transforms{
							transformed_function_identifier=FunId } ) ->
	wooper_internals:raise_usage_error( "standard body transform called when "
		"traversing the AST of the ~ts/~B function (abnormal); "
		"body was:~n  ~p;~nCorresponding stacktrace was: ~ts",
		pair:to_list( FunId ) ++ [ Body, code_utils:interpret_stacktrace() ],
		Transforms, _Line=0 ).




% Drives the AST transformation of a body: processes specifically each last
% expression of a body to be transformed (and only them), as it is the place
% where we can guess the nature of a function and possibly, if it is a method,
% at least some of its qualifiers.
%
% Used to be an anonymous function, yet now exported so that it can be re-used
% by upper layers.
%
% Note: resets its transforms by itself.
%
-spec body_transformer( ast_body(), ast_transforms(), line() ) ->
							{ ast_body(), ast_transforms() }.
% As empty bodies may happen (ex: 'receive' without an 'after'):
% (nevertheless should never happen in this WOOPER traversal)
body_transformer( _BodyExprs=[], Transforms, _Line ) ->

	?trace( "Transforming for WOOPER empty body." ),

	%UpdatedTransforms = update_transformation_state( Transforms,
	%												 Transforms, Line ),

	{ _Exprs=[], Transforms };


% Commented-out as the last expression is managed differently (we cannot recurse
% easily), but the spirit remains:
%
%body_transformer( _BodyExprs=[ LastExpr ], Transforms ) ->
%    ast_expression:transform_expressions( LastExpr, Transforms );


% At least an element exists here:
body_transformer( BodyExprs, Transforms, Line ) ->
								% superfluous: when is_list( BodyExprs )

	?trace_fmt( "Transforming for WOOPER body ~p", [ BodyExprs ] ),

	% Warning: we currently skip intermediate expressions as a whole (we do not
	% transform them at all, as currently WOOPER does not have any need for
	% that), but maybe in the future this will have to be changed.
	%
	% We cannot use easily a Y-combinator here, as the signature of this
	% anonymous function is constrained: { [ Expr | SomeFun(T) ] }, Transforms }
	%
	% More efficient than list_utils:extract_last_element/2 and then recreating
	% the list:
	%
	[ LastExpr | RevFirstExprs ] = lists:reverse( BodyExprs ),

	?trace_fmt( "Requesting the transformation of last expression ~p",
				[ LastExpr ] ),

	ResetTransforms = reset_transformation_state( Transforms ),

	% This may or may not trigger in turn our transformers, knowing that the
	% standard body_transformer/2 is expected never to be called (always
	% intercepted):
	%
	% (in practice, either one expression is returned, or in some cases none at
	% all; at least they are supposed to be already in-order - no need to
	% reverse them)
	%
	{ NewLastExprs, NewTransforms } =
		ast_expression:transform_expression( LastExpr, ResetTransforms ),

	NewExprs = case lists:reverse( RevFirstExprs ) ++ NewLastExprs of

		% An empty body may happen (ex: if defining a static method returning
		% void whilr having no prior expression), which would not be legit:
		%
		[] ->

			PlaceholderAtom = wooper_void_return,

			%trace_utils:warning_fmt(
			%  "Empty method body, replaced by the returning "
			%  "of atom '~ts'.", [ PlaceholderAtom ] ),

			% We return this, as something have to be returned:
			[ { atom, Line, PlaceholderAtom } ];

		NonEmptyExprs ->
			NonEmptyExprs

	end,

	UpdatedTransforms = update_transformation_state( Transforms,
													 NewTransforms, Line ),

	?trace_fmt( "Nature after body transformation: ~p",
		[ element( 1,
			   UpdatedTransforms#ast_transforms.transformation_state ) ] ),

	{ NewExprs, UpdatedTransforms }.



% Drives the AST transformation of a call: in charge of detecting the method
% terminators and qualifiers, in a WOOPER context.
%
% Only the top-level call is of interest here (no need to recurse to hunt for
% method terminators).
%
% (anonymous mute variables correspond to line numbers)
%
-spec call_transformer( line(), ast_expression:function_ref_expression(),
			ast_expression:params_expression(), ast_transforms() ) ->
					{ [ ast_expression:ast_expression() ], ast_transforms() }.

% Initial case: a (necessarily local) call to throw shall be intercepted, as any
% clause of a method may throw, and it should not be interpreted as a non-method
% clause; so:
%
call_transformer( LineCall, FunctionRef={atom,_,throw}, Params,
				  Transforms=#ast_transforms{
		  transformation_state={ _Nature, _Qualifiers, WOOPERExportSet } } ) ->

	?trace_fmt( "Transforming for WOOPER throw call, params: ~p.",
				[ Params ] ),

	%?trace( "throw expression intercepted" ),

	% Reconstructs the original throw expression:
	NewExpr = { call, LineCall, FunctionRef, Params },

	% A throw cannot tell whether a given function clause is a method or a plain
	% function (so we do not return 'undefined', which would be translated as
	% plain function):
	%
	NewTransforms = Transforms#ast_transforms{
		  transformation_state={ throw, [], WOOPERExportSet } },

	{ [ NewExpr ], NewTransforms };


% First, requests:

% Better use the most precise return pseudo-function if this clause is const
% (since returning the initial State):
%
call_transformer( _LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
											{atom,_,return_state_result} },
				  _Params=[ _StateExpr={ var, Line, 'State' }, _ResExpr ],
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId } ) ->
	wooper_internals:raise_usage_error( "this const clause of "
		"request ~ts/~B shall use wooper:const_return_result/1 (instead "
		"of wooper:return_state_result/2).", pair:to_list( FunId ), Transforms,
		Line );

% First (correct, non-const) request detection:
call_transformer( LineCall,
		_FunctionRef={ remote, _, {atom,_,wooper},
						{atom,_,return_state_result} },
		Params=[ _StateExpr, _ResExpr ],
		Transforms=#ast_transforms{
			transformed_function_identifier=FunId,
			transformation_state={ undefined, _, WOOPERExportSet } } ) ->

	?debug_fmt( "~ts/~B detected as a non-const request.",
				pair:to_list( FunId ) ),

	% So that wooper:return_state_result( S, R ) becomes simply { S, R }:
	NewExpr = { tuple, LineCall, Params },

	NewTransforms = Transforms#ast_transforms{
					  transformation_state={ request, [], WOOPERExportSet } },

	{ [ NewExpr ], NewTransforms };


% Already detected as a request, checking qualifiers:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,return_state_result} },
				  Params=[ _StateExpr, _ResExpr ],
				  Transforms=#ast_transforms{
						transformed_function_identifier=FunId,
						transformation_state={ request, Qualifiers,
											   WOOPERExportSet } } ) ->

	?debug_fmt( "~ts/~B confirmed as a non-const request.",
				pair:to_list( FunId ) ),

	% 'const' may or may not be still there, and will surely not:
	NewQualifiers = lists:delete( const, Qualifiers ),

	NewExpr = { tuple, LineCall, Params },

	NewTransforms = Transforms#ast_transforms{
					  transformation_state={ request, NewQualifiers,
											 WOOPERExportSet } },

	{ [ NewExpr ], NewTransforms };


% Faulty return_state_result/2 arity:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,return_state_result} },
				  Params,
				  Transforms=#ast_transforms{
								transformed_function_identifier=FunId } )
  when length( Params ) =/= 2 ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for wooper:return_state_result/2, for request ~ts/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,return_state_result} },
				  _Params,
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId,
					transformation_state={ OtherNature, _Qualifiers,
										   _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~ts/~B: wooper:return_state_result/2 implies "
		"request, whereas was detected as a ~ts.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );


% First (correct, a priori const) request detection:
call_transformer( LineCall,
	  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,const_return_result} },
	  Params=[ _ResExpr ],
	  Transforms=#ast_transforms{
			transformed_function_identifier=FunId,
			transformation_state={ undefined, [], WOOPERExportSet } } ) ->

	?debug_fmt( "~ts/~B detected as a const request.",
				pair:to_list( FunId ) ),

	% So that wooper:const_return_result( R ) becomes simply { S, R }:
	NewExpr = { tuple, LineCall, [ { var, LineCall, 'State' } | Params ] },

	NewTransforms = Transforms#ast_transforms{
			  transformation_state={ request, [ const ], WOOPERExportSet } },

	{ [ NewExpr ], NewTransforms };


% Already detected as a request, this clause is const, this will not change
% overall constness status:
%
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return_result} },
				  Params=[ _ResExpr ],
				  Transforms=#ast_transforms{
						transformation_state={ request, _Qualifiers,
											   _WOOPERExportSet } } ) ->

	NewExpr = { tuple, LineCall, [ { var, LineCall, 'State' } | Params ] },
	{ [ NewExpr ], Transforms };


% Faulty const_return_result/1 arity:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return_result} },
				  Params,
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId } )
  when length( Params ) =/= 1 ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for wooper:const_return_result/1, for request ~ts/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return_result} },
				  _Params,
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId,
					transformation_state={ OtherNature, _Qualifiers,
										   _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~ts/~B: wooper:const_return_result/1 implies "
		"request, whereas was detected as a ~ts.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );



% Second, oneways:

% Better use the most precise return pseudo-function if this clause is const
% (since returning the initial State):
%
call_transformer( _LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
											{atom,_,return_state} },
				  _Params=[ _StateExpr={ var, Line, 'State'} ],
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId } ) ->
	wooper_internals:raise_usage_error( "this const clause of oneway "
		"~ts/~B shall use wooper:const_return/0 "
		"(instead of wooper:return_state/1).",
		pair:to_list( FunId ), Transforms, Line );


% First (correct, non-const) oneway detection:
call_transformer( _LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
											{atom,_,return_state} },
				  _Params=[ StateExpr ],
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId,
					transformation_state={ undefined, _Qualifiers,
										   WOOPERExportSet } } ) ->

	?debug_fmt( "~ts/~B detected as a non-const oneway.",
				pair:to_list( FunId ) ),

	% So that wooper:return_state( S ) becomes simply S:
	NewExpr = StateExpr,
	NewTransforms = Transforms#ast_transforms{
					  transformation_state={ oneway, [], WOOPERExportSet } },
	{ [ NewExpr ], NewTransforms };


% Already detected as a oneway, checking qualifiers:
call_transformer( _LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
											{atom,_,return_state} },
				  _Params=[ StateExpr ],
				  Transforms=#ast_transforms{
						transformation_state={ oneway, Qualifiers,
											   WOOPERExportSet } } ) ->

	% 'const' may or may not be still there, and will surely not:
	NewQualifiers = lists:delete( const, Qualifiers ),

	NewExpr = StateExpr,

	NewTransforms = Transforms#ast_transforms{
					  transformation_state={ oneway, NewQualifiers,
											 WOOPERExportSet } },

	{ [ NewExpr ], NewTransforms };


% Faulty return_state/1 arity:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,return_state} },
				  Params,
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId } )
  when length( Params ) =/= 1 ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for wooper:return_state/1, for oneway ~ts/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,return_state} },
				  _Params,
				  Transforms=#ast_transforms{
						transformed_function_identifier=FunId,
						transformation_state={ OtherNature, _Qualifiers,
											   _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~ts/~B: wooper:return_state/1 implies "
		"oneway, whereas was detected as a ~ts.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );


% First (correct, a priori const) oneway detection:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return} },
				  _Params=[],
				  Transforms=#ast_transforms{
						transformed_function_identifier=FunId,
						transformation_state={ undefined, _,
											   WOOPERExportSet } } ) ->

	?debug_fmt( "~ts/~B detected as a const oneway.",
				pair:to_list( FunId ) ),

	% So that wooper:const_return() becomes simply S:
	NewExpr = { var, LineCall, 'State' },
	NewTransforms = Transforms#ast_transforms{
		  transformation_state={ oneway, [ const ], WOOPERExportSet } },
	{ [ NewExpr ], NewTransforms };


% Already detected as a oneway, this clause is const, this will not change
% overall constness status:
%
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return} },
				  _Params=[],
				  Transforms=#ast_transforms{
						transformation_state={ oneway, _Qualifiers,
											   _WOOPERExportSet } } ) ->

	NewExpr = { var, LineCall, 'State' },

	{ [ NewExpr ], Transforms };


% Faulty const_return/0 arity:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return} },
				  Params,
				  Transforms=#ast_transforms{
								transformed_function_identifier=FunId } )
  when Params =/= [] ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for wooper:const_return/0, for oneway ~ts/~B. Note: if it is actually "
		"a request, use wooper:const_return_result/1 instead.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
										   {atom,_,const_return} },
				  _Params,
				  Transforms=#ast_transforms{
					transformed_function_identifier=FunId,
					transformation_state={ OtherNature, _Qualifiers,
										   _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~ts/~B: wooper:const_return/0 implies "
		"oneway, whereas was detected as a ~ts.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );


% Third, static methods:

% First (correct) static method detection of void return:
call_transformer( _LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
											{atom,_,return_static_void} },
		  _Params=[],
		  Transforms=#ast_transforms{
				transformed_function_identifier=FunId,
				transformation_state={ undefined, _, WOOPERExportSet } } ) ->

	?debug_fmt( "~ts/~B detected as a static method (void return).",
				pair:to_list( FunId ) ),

	% So that wooper:return_static( void ) becomes a no-op:
	NewTransforms = Transforms#ast_transforms{
					transformation_state={ static, [], WOOPERExportSet } },
	{ [], NewTransforms };


call_transformer( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,return_static} },
		  _Params=[ ResultExpr ],
		  Transforms=#ast_transforms{
				transformed_function_identifier=FunId,
				transformation_state={ undefined, _, WOOPERExportSet } } ) ->

	?debug_fmt( "~ts/~B detected as a static method.", pair:to_list( FunId ) ),

	% So that wooper:return_static( R ) becomes simply R:
	NewExpr = ResultExpr,
	NewTransforms = Transforms#ast_transforms{
					  transformation_state={ static, [], WOOPERExportSet } },
	{ [ NewExpr ], NewTransforms };


% Already detected as a static:
%
% (mostly the same clause as above, as qualifiers do not matter for static):
%
call_transformer( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,return_static} },
		  _Params=[ ResultExpr ],
		  Transforms=#ast_transforms{
				transformed_function_identifier=FunId,
				transformation_state={ static, _Qualifiers,
									   _WOOPERExportSet } } ) ->

	?debug_fmt( "~ts/~B confirmed as a static method.", pair:to_list( FunId ) ),

	% So that wooper:return_static( R ) becomes simply R:
	NewExpr = ResultExpr,
	NewTransforms = Transforms,
	{ [ NewExpr ], NewTransforms };


% Faulty static arity:
call_transformer( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,return_static} },
		  Params,
		  Transforms=#ast_transforms{ transformed_function_identifier=FunId } )
  when length( Params ) =/= 1 ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for wooper:return_static/1, for static method ~ts/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
call_transformer( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,return_static} },
		  _Params,
		  Transforms=#ast_transforms{
			transformed_function_identifier=FunId,
			transformation_state={ OtherNature, _Qualifiers,
								   _WOOPERExportSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~ts/~B: wooper:return_static/1 implies "
		"static method, whereas was detected as a ~ts.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );




% Fourth, throwing declarations, to mark the return of a given call as a throw:


% First (correct) throwing helper detection:
call_transformer( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,throwing} },
		  _Params=[ ResultExpr ],
		  Transforms=#ast_transforms{
				transformed_function_identifier=FunId,
				transformation_state={ undefined, _, WOOPERExportSet } } ) ->

	?debug_fmt( "~ts/~B detected as a throwing method.",
				pair:to_list( FunId ) ),

	% So that wooper:throwing( R ) becomes simply R (assimilated to a mere
	% throw):
	%
	NewExpr = ResultExpr,
	NewTransforms = Transforms#ast_transforms{
					transformation_state={ throw, [], WOOPERExportSet } },
	{ [ NewExpr ], NewTransforms };


% Already detected as a throw:
%
% (mostly the same clause as above, as qualifiers do not matter for throwing):
%
call_transformer( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,throwing} },
		  _Params=[ ResultExpr ],
		  Transforms=#ast_transforms{
			transformed_function_identifier=FunId,
			transformation_state={ throw, _Qualifiers, _WOOPERExportSet } } ) ->

	?debug_fmt( "~ts/~B confirmed as a throw method.", pair:to_list( FunId ) ),

	% So that wooper:throwing( R ) becomes simply R:
	NewExpr = ResultExpr,
	NewTransforms = Transforms,
	{ [ NewExpr ], NewTransforms };


% Faulty throwing arity:
call_transformer( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,throwing} },
		  Params,
		  Transforms=#ast_transforms{ transformed_function_identifier=FunId } )
  when length( Params ) =/= 1 ->
	wooper_internals:raise_usage_error( "wrong arity (~B) specified "
		"for wooper:throwing/1, for method ~ts/~B.",
		[ length( Params ) | pair:to_list( FunId ) ], Transforms, LineCall );


% Nature mismatch:
call_transformer( LineCall,
		  _FunctionRef={ remote, _, {atom,_,wooper}, {atom,_,throwing} },
		  _Params,
		  Transforms=#ast_transforms{
			transformed_function_identifier=FunId,
			transformation_state={ OtherNature, _Qualifiers,
								   _WOOPERExporSet } } ) ->
	wooper_internals:raise_usage_error( "method terminator mismatch "
		"for method ~ts/~B: wooper:throwing/1 implies "
		"throwing method, whereas was detected as a ~ts.",
		pair:to_list( FunId ) ++ [ OtherNature ], Transforms, LineCall );


% The commented clause below cannot be kept, as a plain function may for example
% terminate with a call to wooper:execute_request/4, with is licit and should
% not be interpreted as an invalid method terminator:
%
% Invalid method terminator:
%call_transformer( LineCall, _FunctionRef={ remote, _, {atom,_,wooper},
%										   {atom,_,UnexpectedTerminator} },
%				  _Params,
%				  Transforms=#ast_transforms{
%								transformed_function_identifier=FunId } ) ->
%	wooper_internals:raise_usage_error( "invalid method terminator specified "
%		"for ~ts/~B: wooper:~ts does not exist (for any arity).",
%		pair:to_list( FunId ) ++ [ UnexpectedTerminator ], Transforms,
%		LineCall );


% So we selectively accept the WOOPER non-terminator functions, and reject the
% others, i.e. the ones that the wooper module does not export:
%
% (the purpose is to intercept any wrong method terminator that would be
% introduced by the user)
%
call_transformer( LineCall, FunctionRef={ remote, _, {atom,_,wooper},
										  {atom,_,FunctionName} },
				  Params,
				  Transforms=#ast_transforms{
						transformed_function_identifier=FunId,
						transformation_state={ Nature, _Qualifiers,
											   WOOPERExportSet } } ) ->

	CallFunId = { FunctionName, length( Params ) },

	% So this call does not correspond to any known/expected method terminator;
	% let's check whether it is a legit WOOPER call (allowed as last expression
	% of a body) or a WOOPER-unknown one (hence most probably a faulty
	% terminator):
	%
	case set_utils:member( CallFunId, WOOPERExportSet ) of

		true ->
			% OK, terminating a body with a call to a function of the wooper
			% module is allowed (provided it is not a method but a plain
			% function), so we let this call pass through:

			?debug_fmt( "~ts/~B detected as a plain function;~n"
				" - function ref is:~n~p~n - transform is:~n~p~n",
				pair:to_list( FunId ) ++ [ FunctionRef, Transforms ] ),

			SameExpr = { call, LineCall, FunctionRef, Params },
			{ [ SameExpr ], Transforms };

		false ->

			%?debug_fmt( "Known functions exported by the wooper "
			%					   "module: ~ts",
			%					   [ set_utils:to_string( WOOPERExportSet ) ] ),

			%?debug_fmt( "Known functions exported by the wooper "
			%					   "module:~n  ~ts",
			%					   [ table:to_string( WOOPERExportSet ) ] ),

			% To convert end of lines:
			ExtraHint = text_utils:format( case Nature of

				request ->
					"~nThe supported terminators for requests are "
					"wooper:return_state_result/2 and "
					"wooper:const_return_result/1.";

				oneway ->
					"~nThe supported terminators for oneways are "
					"wooper:return_state/1 and wooper:const_return/0.";

				static ->
					"~nThe only supported terminator for static methods is "
					"wooper:return_static/1.";

				_ ->
					"~nSupported terminators are (all prefixed with the "
					"'wooper' module):~n"
					"  - for requests: return_state_result/2 and "
					"const_return_result/1~n"
					"  - for oneways: return_state/1 and const_return/0~n"
					"  - for static methods: return_static/1~n"

			end, [] ),

			wooper_internals:raise_usage_error( "invalid method terminator "
			  "specified for ~ts/~B: wooper:~ts/~B is neither a known "
			  "terminator nor a WOOPER-exported function.~ts",
			  pair:to_list( FunId ) ++ pair:to_list( CallFunId )
									++ [ ExtraHint ],
			  Transforms, LineCall )

	end;


% Finally, of course calls unrelated to WOOPER shall go through as well -
% provided this is either 'undefined' (single, direct clause) or 'function':
%
call_transformer( LineCall, FunctionRef, Params,
				  %Transforms ) ->
				  Transforms=#ast_transforms{
						transformed_function_identifier=FunId,
						transformation_state={ Nature, Qualifiers,
											   _WOOPERExportSet } } ) ->
 % No nature restriction, as even from a method we can explore 'normal' calls:
 %when Nature =:= undefined orelse Nature =:= function ->

	?debug_fmt( "Deducing that ~ts/~B is a plain function "
				"(nature: ~p, qualifiers: ~p)",
				pair:to_list( FunId ) ++ [ Nature, Qualifiers ] ),

	SameExpr = { call, LineCall, FunctionRef, Params },

	%?debug_fmt( "Letting call remaining as ~p, while nature is ~p",
	%			[ SameExpr, Nature ] ),

	{ [ SameExpr ], Transforms }.


% To help debugging any non-match:
% call_transformer( _LineCall, FunctionRef, _Params, Transforms ) ->
%
%	trace_utils:error_fmt( "Unexpected transforms for call ~p:~n  ~ts",
%				   [ FunctionRef,
%					 ast_transform:ast_transforms_to_string( Transforms ) ] ),
%
%	throw( { unexpected_transforms, Transforms } ).



% In the transformers below, we drive the traversal as needed, for example not
% to recurse into case test expressions and alike (which must not be considered
% for function nature diagnosis).
%
% So we define here the WOOPER counterpart versions of the
% ast_clause:transform_*_clause functions.



% Drives the AST transformation of a 'if' construct.
%
% (see ast_expression:transform_if/3)
%
-spec if_transformer( line(), [ ast_if_clause() ], ast_transforms() ) ->
						{ [ ast_expression() ], ast_transforms() }.
if_transformer( Line, IfClauses, Transforms ) ?rec_guard ->

	{ NewIfClauses, NewTransforms } =
		lists:mapfoldl( fun if_clause_transformer/2, _Acc0=Transforms,
						_List=IfClauses ),

	NewExpr = { 'if', Line, NewIfClauses },

	{ [ NewExpr ], NewTransforms }.



% Drives the AST transformation of a 'case' construct.
%
% (see ast_expression:transform_case/4)
%
-spec case_transformer( line(), ast_expression(), [ ast_case_clause() ],
			ast_transforms() ) -> { [ ast_expression() ], ast_transforms() }.
case_transformer( Line, TestExpression, CaseClauses, Transforms ) ?rec_guard ->

	% Test expression not traversed here.

	{ NewCaseClauses, CaseTransforms } =
		lists:mapfoldl( fun case_clause_transformer/2, _Acc0=Transforms,
						_List=CaseClauses ),

	NewExpr = { 'case', Line, TestExpression, NewCaseClauses },

	{ [ NewExpr ], CaseTransforms }.



% Drives the AST transformation of a 'simple_receive' construct.
%
% (see ast_expression:transform_simple_receive/3)
%
-spec simple_receive_transformer( line(), [ ast_case_clause() ],
		ast_transforms() ) -> { [ ast_expression() ], ast_transforms() }.
simple_receive_transformer( Line, ReceiveClauses, Transforms ) ?rec_guard ->

	% Receive clauses are actually case clauses:
	{ NewReceiveClauses, NewTransforms } =
		lists:mapfoldl( fun case_clause_transformer/2,
						_Acc0=Transforms, _List=ReceiveClauses ),

	NewExpr = { 'receive', Line, NewReceiveClauses },

	{ [ NewExpr ], NewTransforms }.




% Drives the AST transformation of a 'receive_with_after' construct.
%
% (see ast_expression:transform_receive_with_after/5)
%
-spec receive_with_after_transformer( line(), [ ast_case_clause() ],
		ast_expression(), ast_body(), ast_transforms() ) ->
								{ [ ast_expression() ], ast_transforms() }.
receive_with_after_transformer( Line, ReceiveClauses, AfterTest, AfterBody,
								Transforms ) ?rec_guard ->

	% Receive clauses are actually case clauses:
	{ NewReceiveClauses, ReceiveTransforms } =
		lists:mapfoldl( fun case_clause_transformer/2,
						_Acc0=Transforms, _List=ReceiveClauses ),

	% Note that all "actual" updates in the transformation state are done by the
	% call_transformer, which is to be called (almost) solely by
	% body_transformer/3 - which is expected to catch and check state updates;
	% that function is called in each sub-transformer above and called below as
	% well, so no state change is expected to be missed:

	% Test unchanged.

	{ NewAfterBody, AfterTransforms } =
		body_transformer( AfterBody, ReceiveTransforms, Line ),

	NewExpr = { 'receive', Line, NewReceiveClauses, AfterTest, NewAfterBody },

	{ [ NewExpr ], AfterTransforms }.




% Drives the AST transformation of a 'try' construct.
%
% Actually it is rather tricky, as a try, in terms of return value, can have 3
% different forms (cf. http://erlang.org/doc/reference_manual/expressions.html):
%
% (1) try EXPR catch (CATCH_PATTERN_1 -> BODY_1), (CATCH_PATTERN_2 -> BODY_2)
%
% (2) try EXPR of (PATTERN_1 -> BODY_1), (PATTERN_2 -> BODY_2) catch
% (CATCH_PATTERN_A -> BODY_A), (CATCH_PATTERN_B -> BODY_B),...
%
% (3) like (2), with an additional AFTER_BODY, whose value is lost
%
% So, here, for (1) EXPR is a possible return value, whereas not for (2) and
% (3), and we have to discriminate among these cases.
%
% (see also ast_expression:transform_try/6)
%
-spec try_transformer( line(), ast_body(), [ ast_case_clause() ],
					   [ ast_case_clause() ], ast_body(), ast_transforms() ) ->
								{ [ ast_expression() ], ast_transforms() }.
% Form (1): no try clauses (and no after):
try_transformer( Line, TryBody, TryClauses=[], CatchClauses, AfterBody=[],
				 Transforms ) ?rec_guard ->

	% Only returns through TryBody or CatchClauses:

	{ NewTryBody, TryBodyTranforms } =
		body_transformer( TryBody, Transforms, Line ),

	{ NewCatchClauses, CatchTransforms } =
		lists:mapfoldl( fun catch_clause_transformer/2, _Acc0=TryBodyTranforms,
						_List=CatchClauses ),

	% After body not transformed, as actually never returned:
	%{ NewAfterBody, AfterTransforms } =
	%	body_transformer( AfterBody, CatchTransforms, Line ),

	NewExpr = { 'try', Line, NewTryBody, TryClauses, NewCatchClauses,
				AfterBody },

	{ [ NewExpr ], CatchTransforms };


% Form (2) and (3): we have try clauses (and after body can be ignored):
try_transformer( Line, TryBody, TryClauses, CatchClauses, AfterBody,
				 Transforms ) ?rec_guard ->

	% Only returns through TryClauses (not TryBody) or CatchClauses:

	% These are case clauses:
	{ NewTryClauses, TryTransforms } =
		lists:mapfoldl( fun case_clause_transformer/2, _FirstAcc0=Transforms,
						_FirstList=TryClauses ),

	% These are catch clauses:
	{ NewCatchClauses, CatchTransforms } =
		lists:mapfoldl( fun catch_clause_transformer/2,
						_SecondAcc0=TryTransforms, _SecondList=CatchClauses ),


	% After body not transformed, as actually never returned:
	%{ NewAfterBody, AfterTransforms } =
	%	body_transformer( AfterBody, CatchTransforms, Line ),

	NewExpr = { 'try', Line, TryBody, NewTryClauses, NewCatchClauses,
				AfterBody },

	{ [ NewExpr ], CatchTransforms }.



% Drives the AST transformation of a 'catch' construct.
%
% Not: for catch as an expression, not as a component of try.
%
-spec catch_transformer( line(), ast_expression(), ast_transforms() ) ->
							{ [ ast_expression() ], ast_transforms() }.
catch_transformer( Line, Expression, Transforms ) ?rec_guard ->

	% Only known place where a call_transformer might be called out of the
	% context of body_transformer/3:

	ResetTransforms = reset_transformation_state( Transforms ),

	{ [ NewExpression ], NewTransforms } =
		ast_expression:transform_expression( Expression, ResetTransforms ),

	UpdatedTransforms = update_transformation_state( Transforms,
													 NewTransforms, Line ),

	NewExpr = { 'catch', Line, NewExpression },

	{ [ NewExpr ], UpdatedTransforms }.





% Subsection for WOOPER helper transformers.
%
% They may be used by multiple top-level transformers.



% Transforms an 'if' clause just for the sake of WOOPER.
%
% (corresponds to ast_clause:transform_if_clause/2)
%
if_clause_transformer( _Clause={ 'clause', Line, HeadPatternSequence=[],
								 GuardSequence, BodyExprs },
					   Transforms ) ?rec_guard ->

	% Non-existing head and guards not traversed.

	% No need to reset transformation state, as done by body_transformer/3:

	% We only transform the body here (and this integrates the update
	% logic regarding function detection):
	%
	{ NewBodyExprs, BodyTransforms } =
		body_transformer( BodyExprs, Transforms, Line ),

	NewExpr = { 'clause', Line, HeadPatternSequence, GuardSequence,
				NewBodyExprs },

	Res = { NewExpr, BodyTransforms },

	%ast_utils:display_debug( "... returning if clause and state ~p", [ Res ] ),

	Res.



% Transforms a 'case' clause just for the sake of WOOPER.
%
% (corresponds to ast_clause:transform_case_clause/2)
%
case_clause_transformer( _Clause={ 'clause', Line, CaseHead=[ _Pattern ],
								   GuardSequence, BodyExprs },
						 Transforms ) ?rec_guard ->

	% Pattern and guards not traversed.

	% No need to reset transformation state, as done by body_transformer/3:

	% We only transform the body here (and this integrates the update
	% logic regarding function detection):
	%
	{ NewBodyExprs, BodyTransforms } =
		body_transformer( BodyExprs, Transforms, Line ),

	NewExpr = { 'clause', Line, CaseHead, GuardSequence,
				NewBodyExprs },

	Res = { NewExpr, BodyTransforms },

	%ast_utils:display_debug( "... returning case clause and state ~p",
	%                         [ Res ] ),

	Res.


% Transforms a 'catch' clause just for the sake of WOOPER.
%
% (corresponds to ast_clause:transform_catch_clause/2)
%
catch_clause_transformer(
  _Clause={ 'clause', Line, Throw=[ { throw, _Pattern, _Any } ], GuardSequence,
			BodyExprs },
  Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting catch clause ~p...", [ Clause ] ),

	% We believe that only the body is to be traversed here:

	% No need to reset transformation state, as done by body_transformer/3:

	{ NewBodyExprs, BodyTransforms } =
		body_transformer( BodyExprs, Transforms, Line ),

	NewExpr = { 'clause', Line, Throw, GuardSequence, NewBodyExprs },

	Res = { NewExpr, BodyTransforms },

	%ast_utils:display_debug( "... returning catch clause and state ~p",
	%                         [ Res ] ),

	Res;


catch_clause_transformer(
  _Clause={ 'clause', Line, Head=[ _HeadPattern={ _X, _P, _Any } ],
			GuardSequence, BodyExprs },
  Transforms ) ?rec_guard ->

	%ast_utils:display_debug( "Intercepting catch clause with variable ~p...",
	%						 [ Clause ] ),

	% We believe that only the body is to be traversed here:

	% No need to reset transformation state, as done by body_transformer/3:

	{ NewBodyExprs, BodyTransforms } =
		body_transformer( BodyExprs, Transforms, Line ),

	NewExpr = { 'clause', Line, Head, GuardSequence, NewBodyExprs },

	Res = { NewExpr, BodyTransforms },

	%ast_utils:display_debug( "... returning catch clause with variable "
	%                         "and state ~p", [ Res ] ),

	Res.



% Resets the transformation state, so that new findings can be compared to
% previous knowledge.
%
% Otherwise, for example if having already a 'request' nature found and finding
% afterwards a 'function' expression (i.e. a nature established by default), the
% recorded nature would remain to 'request' and thus the mismatching 'function'
% clause would not be detected.
%
-spec reset_transformation_state( ast_transforms() ) -> ast_transforms().
reset_transformation_state( Transforms=#ast_transforms{
		transformation_state={ _Nature, _Qualifiers, WOOPERExportSet } } ) ->
	Transforms#ast_transforms{
			transformation_state={ undefined, [], WOOPERExportSet } }.


% Returns an updated transformation state, based on an initial one and one
% returned by a transformation.
%
% Note that the base one provided to the transformation in-between shall have
% been reset (see reset_transformation_state/1).
%
-spec update_transformation_state( ast_transforms(), ast_transforms(),
								   line() ) -> ast_transforms().
update_transformation_state(
  _InitialTransforms=#ast_transforms{ transformation_state={ InitialNature,
						InitialQualifiers, _WOOPERExportSet } },
  NewTransforms=#ast_transforms{
				   transformed_function_identifier=FunId,
				   transformation_state={ NewRawNature, NewQualifiers,
										  WOOPERExportSet } },
  Line ) ->

	% As soon as we return from the transformation whereas the nature is still
	% 'undefined', this means that:
	%
	%  1. no method terminator has been found in the full branch (as we explore
	%  the AST of a function depth-first)
	%
	%  2. it was the first terminal branch explored (as we assign systematically
	%  the function nature when returning from a branch)
	%
	% So it was a plain function, we record that nature for further checking of
	% the other terminal branches. As a result this transformer allows to
	% discriminate 'no information yet' (hence function nature can be anything)
	% from 'no method terminator found' (so it must be a plain function).

	NewPair = { NewActualNature, NewActualQualifiers } = case NewRawNature of

		undefined ->
			{ function, [] };

		_ ->
			{ NewRawNature, NewQualifiers }

	end,


	% Regarding function nature, following rules apply:
	%
	% 1. if the initial nature is 'undefined', any new nature (throw or method
	% one) will supersede it
	%
	% 2. if the initial nature is 'throw', the same applies (either superseded
	% by throw or a method one)
	%
	% 3. if the initial nature is a method one, this nature will be:
	%
	%   - confirmed if new nature is the same
	%   - kept as is if new nature is throw (no additional information)
	%   - mismatching if new nature is a different method one

	{ ResultingNature, ResultingQualifiers } = case InitialNature of

		undefined ->
			NewPair;

		throw ->
			NewPair;

		NewActualNature ->
			% Confirmed nature, only case to handle is loss of constness:
			case lists:member( const, InitialQualifiers )
				andalso not lists:member( const, NewActualQualifiers ) of

				% Actually is non-const then:
				true ->
					{ NewActualNature, NewActualQualifiers };

				% As const as used to be:
				false ->
					{ NewActualNature, InitialQualifiers }

			end;

		_MismatchingNature ->
			case NewActualNature of

				throw ->
					% Can happen for example if first clause of function of
					% interest is a 'function' one, next one is a 'throw':
					%
					% (by design InitialNature cannot be 'undefined' or 'throw'
					% here)
					%
					{ InitialNature, InitialQualifiers };

				_ ->
					wooper_internals:raise_usage_error(
					  "the ~ts/~B function was "
					  "detected as a ~ts, yet the clause at line #~B indicates "
					  "it is a ~ts.", pair:to_list( FunId ) ++ [
						  function_nature_to_string( InitialNature ), Line,
						  function_nature_to_string( NewActualNature ) ],
					  NewTransforms, Line )

			end

	end,

	?debug_fmt( "Nature of ~ts/~B: initial=~p, new raw=~p, "
				"new actual=~p, final=~p.", pair:to_list( FunId ) ++
		   [ InitialNature, NewRawNature, NewActualNature, ResultingNature ] ),

	NewTransforms#ast_transforms{ transformation_state={ ResultingNature,
								ResultingQualifiers, WOOPERExportSet } }.





% Ensures that the specified information is exported, auto-exporting it if
% necessary.
%
% See also: ast_info:ensure_function_exported/4.
%
-spec ensure_exported( function_info(), marker_table() ) -> function_info().
ensure_exported( FunInfo=#function_info{ exported=[] }, MarkerTable ) ->

	%?debug_fmt( "- auto-exporting ~ts",
	%					 [ ast_info:function_info_to_string( FunInfo ) ] ),

	% Not exported yet, hence to export:
	ExportLoc = ast_info:get_default_export_function_location( MarkerTable ),
	FunInfo#function_info{ exported=[ ExportLoc ] };

% Already exported, thus nothing to do:
ensure_exported( FunInfo, _MarkerTable ) ->
	FunInfo.



% Ensures that all functions in specified function table are exported,
% auto-exporting them if necessary.
%
-spec ensure_all_exported_in( function_table(), location() ) ->
									function_table().
ensure_all_exported_in( FunctionTable, ExportLoc ) ->
	table:map_on_values( fun( FunInfo ) ->
							ensure_exported_at( FunInfo, ExportLoc )
						 end,
						 FunctionTable ).


% (helper)
ensure_exported_at( FunInfo=#function_info{ exported=[] }, ExportLoc ) ->
	FunInfo#function_info{ exported=[ ExportLoc ] };

ensure_exported_at( FunInfo=#function_info{ name=Name,
											arity=Arity }, _ExportLoc ) ->

	wooper_internals:notify_warning( [ text_utils:format(
		"~ts/~B should not be explicitly exported, since methods "
		"are automatically exported.", [ Name, Arity ] ) ] ),

	FunInfo.




% Section for the conversion of information records.


% Converts specified (Myriad-level) function information into a (WOOPER-level)
% request information.
%
-spec function_to_request_info( function_info(), method_qualifiers() ) ->
										request_info().
function_to_request_info( #function_info{ name=Name,
										  arity=Arity,
										  location=Loc,
										  line=Line,
										  clauses=Clauses,
										  spec=Spec,
										  callback=false,
										  exported=[] },
						  Qualifiers ) ->
	#request_info{ name=Name,
				   arity=Arity,
				   qualifiers=Qualifiers,
				   location=Loc,
				   line=Line,
				   clauses=Clauses,
				   spec=Spec };


function_to_request_info( #function_info{ name=Name,
										  arity=Arity,
										  location=Loc,
										  line=Line,
										  clauses=Clauses,
										  spec=Spec,
										  callback=false
										  %exported
										},
						  Qualifiers ) ->
	#request_info{ name=Name,
				   arity=Arity,
				   qualifiers=Qualifiers,
				   location=Loc,
				   line=Line,
				   clauses=Clauses,
				   spec=Spec };


function_to_request_info( Other, _Qualifiers ) ->
	throw( { unexpected_function_info, Other, request } ).



% Converts specified (WOOPER-level) request information into a (Myriad-level)
% function information.
%
-spec request_to_function_info( request_info(), location() ) -> function_info().
request_to_function_info( #request_info{ name=Name,
										 arity=Arity,
										 % Unused: qualifiers
										 location=Loc,
										 line=Line,
										 clauses=Clauses,
										 spec=Spec },
						  Location ) ->
	#function_info{ name=Name,
					arity=Arity,
					location=Loc,
					line=Line,
					clauses=Clauses,
					spec=Spec,
					callback=false,
					exported=[ Location ] };

request_to_function_info( Other, _Location ) ->
	throw( { unexpected_request_info, Other, request } ).



% Converts specified (Myriad-level) function information into a (WOOPER-level)
% oneway information.
%
-spec function_to_oneway_info( function_info(), method_qualifiers() ) ->
										oneway_info().
function_to_oneway_info( #function_info{ name=Name,
										 arity=Arity,
										 location=Loc,
										 line=Line,
										 clauses=Clauses,
										 spec=Spec,
										 callback=false,
										 exported=[] },
						 Qualifiers ) ->
	#oneway_info{ name=Name,
				  arity=Arity,
				  qualifiers=Qualifiers,
				  location=Loc,
				  line=Line,
				  clauses=Clauses,
				  spec=Spec };


function_to_oneway_info( #function_info{ name=Name,
										 arity=Arity,
										 location=Loc,
										 line=Line,
										 clauses=Clauses,
										 spec=Spec,
										 callback=false
										 %exported
									   },
						 Qualifiers ) ->
	#oneway_info{ name=Name,
				  arity=Arity,
				  qualifiers=Qualifiers,
				  location=Loc,
				  line=Line,
				  clauses=Clauses,
				  spec=Spec };


function_to_oneway_info( Other, _Qualifiers ) ->
	throw( { unexpected_function_info, Other, oneway } ).



% Converts specified (WOOPER-level) oneway information into a (Myriad-level)
% function information.
%
-spec oneway_to_function_info( oneway_info(), location() ) -> function_info().
oneway_to_function_info( #oneway_info{ name=Name,
									   arity=Arity,
									   % Unused: qualifiers
									   location=Loc,
									   line=Line,
									   clauses=Clauses,
									   spec=Spec },
						 Location ) ->
	#function_info{ name=Name,
					arity=Arity,
					location=Loc,
					line=Line,
					clauses=Clauses,
					spec=Spec,
					callback=false,
					exported=[ Location ] };

oneway_to_function_info( Other, _Location ) ->
	throw( { unexpected_oneway_info, Other, oneway } ).



% Converts specified (Myriad-level) function information into a (WOOPER-level)
% static information.
%
-spec function_to_static_info( function_info(), method_qualifiers() ) ->
									 static_info().
function_to_static_info( #function_info{ name=Name,
										 arity=Arity,
										 location=Loc,
										 line=Line,
										 clauses=Clauses,
										 spec=Spec,
										 callback=false,
										 exported=[] },
						 Qualifiers ) ->
	#static_info{ name=Name,
				  arity=Arity,
				  qualifiers=Qualifiers,
				  location=Loc,
				  line=Line,
				  clauses=Clauses,
				  spec=Spec };


function_to_static_info( #function_info{ name=Name,
										 arity=Arity,
										 location=Loc,
										 line=Line,
										 clauses=Clauses,
										 spec=Spec,
										 callback=false
										 %exported
									   },
						 Qualifiers ) ->
	#static_info{ name=Name,
				  arity=Arity,
				  qualifiers=Qualifiers,
				  location=Loc,
				  line=Line,
				  clauses=Clauses,
				  spec=Spec };


function_to_static_info( Other, _Qualifiers ) ->
	throw( { unexpected_function_info, Other, static } ).



% Converts specified (WOOPER-level) static information into a (Myriad-level)
% function information.
%
-spec static_to_function_info( static_info(), location() ) -> function_info().
static_to_function_info( #static_info{ name=Name,
									   arity=Arity,
									   % Unused: qualifiers
									   location=Loc,
									   line=Line,
									   clauses=Clauses,
									   spec=Spec },
						 Location ) ->
	#function_info{ name=Name,
					arity=Arity,
					location=Loc,
					line=Line,
					clauses=Clauses,
					spec=Spec,
					callback=false,
					exported=[ Location ] };

static_to_function_info( Other, _Location ) ->
	throw( { unexpected_static_info, Other, static } ).



% Transforms the methods in the specified tables into functions, and adds them
% in specified function table.
%
-spec methods_to_functions( request_table(), oneway_table(), static_table(),
						function_table(), marker_table() ) -> function_table().
methods_to_functions( RequestTable, OnewayTable, StaticTable,
					  InitFunctionTable, MarkerTable ) ->

	% Methods shall not be exported, but their corresponding functions surely
	% should, otherwise they could be reported as unused:
	%
	ExportLoc = ast_info:get_default_export_function_location( MarkerTable ),

	RequestPairs = table:enumerate( RequestTable ),

	RequestAsFunPairs = [
		 { ReqId, request_to_function_info( ReqInfo, ExportLoc ) }
						 || { ReqId, ReqInfo } <- RequestPairs ],

	WithRequestsFunTable = table:add_new_entries( RequestAsFunPairs,
												  InitFunctionTable ),


	OnewayPairs = table:enumerate( OnewayTable ),

	OnewayAsFunPairs = [
		 { OnwId, oneway_to_function_info( OnwInfo, ExportLoc ) }
						|| { OnwId, OnwInfo } <- OnewayPairs ],

	WithOnewaysFunTable = table:add_new_entries( OnewayAsFunPairs,
												 WithRequestsFunTable ),


	StaticPairs = table:enumerate( StaticTable ),

	StaticAsFunPairs = [
		 { StId, static_to_function_info( StInfo, ExportLoc ) }
						|| { StId, StInfo } <- StaticPairs ],

	table:add_new_entries( StaticAsFunPairs, WithOnewaysFunTable ).



% To help debugging:
format_log( FormatString, [ { Exprs, #ast_transforms{
		transformation_state={Nature,_Qualifiers,_WOOPERExportSet} } } ] ) ->
	Message = text_utils:format( "whose nature is '~ts'; "
				"expressions are:~n  ~p", [ Nature, Exprs ] ),
	text_utils:format( FormatString, [ Message ] );


format_log( FormatString, Other ) ->
	text_utils:format( FormatString, [ Other ] ).
