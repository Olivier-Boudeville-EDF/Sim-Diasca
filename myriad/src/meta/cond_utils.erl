% Copyright (C) 2018-2021 Olivier Boudeville
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
% Creation date: Tuesday, December 25, 2018.


% Management of conditional compilation - like generalized, limitation-less
% macros.
%
-module(cond_utils).



% Implementation notes:
%
% A large part of this module consists of stubs, i.e. of placeholder definitions
% of the pseudo-functions (such as cond_utils:if_debug/1) meant to be replaced
% at compile time by user-specified conditional code.
%
% Note that if the if_* conditions (ex: if_debug/1) are not fulfilled, the
% specified conditional code is dismissed as a whole, it is not even replaced
% for example by an 'ok' atom (this may matter if this conditional is the only
% expression in a case clause for example, in which case a compilation failure
% like "internal error in core; crash reason: function_clause in function
% v3_core:cexprs/3 called as v3_core:cexprs[...]" will be reported.
%
% Note also that switching conditional flags will select/deselect in-code
% expressions and may lead to variables being declared as unused by the
% compiler; then no better solution than:
% - to mute them (yet this requires to change the code when toggling such flags
% - not desirable)
% - to use nowarn_unused_vars in at least some modules
% - or to introduce once for all (at the expense of a small runtime penalty
% cost) an alternate branch to this conditional silencing these unused warnings,
% like in:
% [...]
% % Should A, B or C be reported as unused if some_token was not set:
% cond_utils:if_defined( some_token,
%                        f( A, B C ),
%                        basic_utils:ignore_unused([A, B, C])),
% [...]
%
% Some function may also become unused in turn, in which case the best solution
% is to rely on:
%
% -compile( { nowarn_unused_function, {my_func,3} } ).
%    OR
% -compile( { nowarn_unused_function, [ {my_func,3}, {my_other_func,0} ] } ).



% About tokens:
%
% There are to be specified as command-line build options, typically thanks to
% the ERLANG_COMPILER_TOKEN_OPT make variable, defined in GNUmakevars.inc.
%
% For example, if defining that variable as:
% ERLANG_COMPILER_TOKEN_OPT := -Dmy_test_token -Dmy_other_test_token=51
%
% then a parse-transform (ex: the Myriad one) would be automatically called as:
%     parse_transform( InputAST, Options ) -> ...
%
% and this option list would contain, among other elements:
%         {d,my_test_token},
%         {d,my_other_test_token,51}
%
% (the atom 'd' must mean 'define' in this compilation context)
%
% Then, in addition to the standard macros such as:
%
% -ifdef(my_test_token).
%  [...]
% -endif
%
% -if(?my_other_test_token >= 22).
%  [...]
% -endif
%
% the parse transform is also able to take into account these information.
%
% Tokens cannot be specified directly in the sources, like shown below, since
% such a definition would not appear per se in the AST (it would result only in
% macro actual substitutions), and thus the corresponding tokens would not be
% known:
%
%-define( my_test_token, 200 ).
%-define( my_other_test_token, some_text ).
%
% As a result, tokens are solely to be defined through command-line options.

% More precisely: we used to believe that a token could be defined either
% through a -D command-line option or through an in-source compile attribute.
%
% If the command-line is suitable for that, this is not the case of a compile
% attribute such as: '-define( my_test_token, 200 ).'.
%
% Indeed, the latter solution only results in any '?my_test_token' to be
% replaced with its associated value, whereas we would have liked to discover a
% priori that the token 'my_test_token' exists and is associated to 200 (in
% order to feed our token table).
%
% As we cannot do that with such a compile attribute (those corresponding to
% '-define(...)' do not end up at all in the AST), one has to stick to the -D
% command-line option (ex: -Dmy_test_token=200).



-export([ get_token_table_from/1,
		  if_debug/1, if_defined/2, if_defined/3,
		  if_set_to/3, if_set_to/4,
		  switch_set_to/2, switch_set_to/3,
		  assert/1, assert/2, assert/3 ]).


% For the table macro:
-include("meta_utils.hrl").



% A token (defined through the command-line), whose definition enables the
% conditional execution of associated code.
%
% Ex: a 'debug_gui' token would enable, if defined, associated code, like in:
% cond_utils:if_defined(debug_gui, [f(), A=B, g(C)])
%
-type token() :: atom().


% An (immediate) value associated to a token can actually of various types
% (ex: atom, integer; as a form of course), as translated by the compiler:
%
-type value() :: term().


% An expression that is conditionally enabled:
-type expression() :: any().

% The conditional code injected is either a single expression or a list thereof:
-type expressions() :: expression() | [ expression() ].

% A table used to associate expression(s) to token values:
-type token_expr_table() :: [ { token(), expressions() } ].


% Table to establish easily whether a token has been defined and, if yes, a
% value (if any; otherwise it is set to 'undefined') that has been associated to
% it.
%
-type token_table() :: ?table:?table( token(), basic_utils:maybe( term() ) ).

-export_type([ token/0, expression/0, expressions/0, token_expr_table/0,
			   token_table/0 ]).


% Shorthand:
-type void() :: basic_utils:void().



% Returns the tokens declared among the compile options.
-spec get_token_table_from( ast_info:compile_option_table() ) ->
									token_table().
get_token_table_from( OptionTable ) ->

	EmptyTable = ?table:new(),

	% The 'd' compile option must correspond to the compilation defines:
	case ?table:lookup_entry( _K='d', OptionTable ) of

		% Ex: L=[my_test_token, {my_other_test_token,51}]
		{ value, L } ->
			% Returns a filled table:
			register_tokens( L, EmptyTable );

		key_not_found ->
			% Empty table then, no token available:
			EmptyTable

	end.


% (helper)
register_tokens( _L=[], TokenTable ) ->
	TokenTable;

register_tokens( _L=[ { Token, Value } | T ], TokenTable )
  when is_atom( Token ) ->
	% Crashes if a token is defined more than once (must be abnormal):
	NewTokenTable = ?table:add_new_entry( Token, Value, TokenTable ),
	register_tokens( T, NewTokenTable );

register_tokens( _L=[ Token | T ], TokenTable ) when is_atom( Token ) ->
	% A token without a value is associated to 'undefined':
	NewTokenTable = ?table:add_new_entry( Token, _V=undefined, TokenTable ),
	register_tokens( T, NewTokenTable ).



% Example of transformation:
%
% cond_utils:if_defined( my_token, [ A = 1,
%									 io:format( "Conditional code executed!" ),
%									 B = A + 1 ] ),
%
% would be by default literally translated into:
%
% (line numbers replaced by anonymous mute variables)
%
%  {call,_,
%	  {remote,_,{atom,_,cond_utils},{atom,_,if_defined}},
%	  [{var,_,'Token'},
%	   {cons,_,
%		   {match,_,{var,_,'A'},{integer,_,1}},
%		   {cons,_,
%			   {call,_,
%				   {remote,_,{atom,_,io},{atom,_,format}},
%				   [{string,_,"Conditional code executed!"}]},
%			   {cons,_,
%				   {match,_,
%					   {var,_,'B'},
%					   {op,_,'+',{var,_,'A'},{integer,_,1}}},
%				   {nil,_}}}}]},
%
%
% whereas we want it to become either (should my_token be defined):
%
%  {match,_,{var,_,'A'},{integer,_,1}},
%               {call,_,
%                     {remote,_,{atom,_,io},{atom,_,format}},
%                     [{string,_,"Conditional code executed!~n"}]},
%               {match,_,
%                      {var,_,'B'},
%                      {op,_,'+',{var,_,'A'},{integer,_,1}}},
%
%    (i.e. we "uncons" said expression list)
%
% or, should my_token not be defined: exactly nothing.




% Conditional execution of specified expression or list thereof, enabled iff the
% debug mode has been set (i.e. iff the 'myriad_debug_mode' token has been
% defined through the command-line).
%
-spec if_debug( expressions() ) -> void().
if_debug( ExpressionsIfDebug ) ->
	if_defined( _Token=myriad_debug_mode, ExpressionsIfDebug ).



% Conditional execution, enabled iff the specified token has been specified
% (i.e. iff its token has been defined through the command-line), in which case
% the specified expression(s) are injected (otherwise they are simply dismissed
% as a whole).
%
% Note: the first parameter, Token, must be an immediate value, an atom (not
% even a variable whose value happens to be an atom).
%
% So 'cond_utils:if_defined( hello, [...] )' will be accepted, while even
% 'A=hello, cond_utils:if_defined( A, [...] )' will be rejected.
%
% As for the second parameter, it shall be *directly* either a single expression
% or a list thereof; for example 'cond_utils:if_defined( myriad_debug_mode,
% _Exprs=[...])' would be rejected.
%
% Finally, should the relevant token not be defined, the corresponding
% expressions are dismissed as a whole, which may lead variables only mentioned
% in said expressions to be reported as unused.
%
% For example: 'A=1, cond_utils:if_defined(non_defined_token, [ A=1,...])' will
% report that variable 'A' is unused.
%
-spec if_defined( token(), expressions() ) -> void().
if_defined( Token, _ExpressionsIfDefined ) ->

	% Never expected to be called, as replaced by the Myriad parse transform
	% either by the actual expressions, or by nothing at all:
	%
	% (note that if the transformation fails, due to strict, non-lazy
	% evaluation, the expressions will be evaluated in all cases)
	%
	%throw( { untransformed_conditional, {if_defined,2}, Token, Expressions } ).

	% Should be sufficient thanks to the stacktrace:
	throw( { untransformed_conditional, {if_defined,2}, Token } ).



% Conditional execution of one of the two specified expressions or lists
% thereof, depending on whether the specified token has been defined through the
% command-line.
%
% If the token has been defined, the first list of expressions is injected,
% otherwise the second is.
%
% See if_defined/2 for use and caveats.
%
-spec if_defined( token(), expressions(), expressions() ) -> void().
if_defined( Token, _ExpressionsIfDefined, _ExpressionsIfNotDefined ) ->

	% Never expected to be called, as replaced by the Myriad parse transform
	% by either of the actual expressions:
	%
	throw( { untransformed_conditional, {if_defined,3}, Token } ).



% Conditional execution of the specified expression or list thereof, depending
% on whether the specified token has been defined through the command-line *and*
% has been set to the specified (immediate) value.
%
% The specified list of expressions is injected iff the token has been defined
% and set to the specified value.
%
% See if_defined/2 for use and caveats.
%
-spec if_set_to( token(), value(), expressions() ) -> void().
if_set_to( Token, _Value, _ExpressionsIfSetTo ) ->

	% Never expected to be called, as replaced by the Myriad parse transform
	% either by the actual expressions, or by nothing at all:
	%
	throw( { untransformed_conditional, {if_set_to,3}, Token } ).



% Conditional execution of one of the two specified expressions or lists
% thereof, depending on whether the specified token has been defined through the
% command-line *and* has been set to the specified (immediate) value.
%
% If the token has been defined and set to the specified value, the first list
% of expressions is injected, otherwise (different value or not defined) the
% second is.
%
% See if_defined/2 for use and caveats.
%
-spec if_set_to( token(), value(), expressions(), expressions() ) -> void().
if_set_to( Token, _Value, _ExpressionsIfMatching, _ExpressionsOtherwise ) ->

	% Never expected to be called, as replaced by the Myriad parse transform by
	% either of the actual expressions:
	%
	throw( { untransformed_conditional, {if_set_to,4}, Token } ).



% Conditional execution of one of the specified expressions or lists thereof
% listed in the token-expression table, depending on whether the specified token
% has been defined through the command-line *and* has been set to one of the
% specified (immediate) values.
%
% If the token has been defined and set to one the values specified in the
% table, the expression(s) associated to this value are injected.
%
% Otherwise (token not set, or set to a value not listed), a compilation-time
% error is raised.
%
% See if_defined/2 for use and caveats.
%
-spec switch_set_to( token(), token_expr_table() ) -> void().
switch_set_to( Token, _TokenExprTable ) ->

	% Never expected to be called, as replaced by the Myriad parse transform by
	% either of the actual expressions:
	%
	throw( { untransformed_conditional, {switch_set_to,2}, Token } ).



% Conditional execution of one of the specified expressions or lists thereof
% listed in the token-expression table, depending on whether the specified token
% has been defined through the command-line *and* has been set to one of the
% specified (immediate) values.
%
% If the token has been defined and set to one the values specified in the
% table, the expression(s) associated to this value are injected.
%
% Otherwise (token not set, or set to a value not listed), the specified default
% token value (expected to be referenced in the table) applies.
%
% See if_defined/2 for use and caveats.
%
-spec switch_set_to( token(), token_expr_table(), value() ) -> void().
switch_set_to( Token, _TokenExprTable, _DefaultTokenValue ) ->

	% Never expected to be called, as replaced by the Myriad parse transform by
	% either of the actual expressions:
	%
	throw( { untransformed_conditional, {switch_set_to,3}, Token } ).



% If in debug mode, asserts that the specified expression is true,
% i.e. evaluates it at runtime and matches it with the atom 'true'.
%
% In debug mode (i.e when the 'myriad_debug_mode' token has been defined), and
% only in that mode, the check will be done (at runtime), and possibly will fail
% by throwing a { assertion_failed, Other } exception, where Other is the actual
% (non-true) value breaking that assertion (of course the usual stacktrace with
% line numbers will be available).
%
-spec assert( expression() ) -> void().
assert( _Expression ) ->
	%assert( _Token=myriad_debug_mode, Expression ).

	% Never expected to be called, as replaced by the Myriad parse transform by
	% either of the actual expressions:
	%
	throw( { untransformed_conditional, {assert,1} } ).



% If the specified token has been defined through the command-line, asserts that
% the specified expression is true, i.e. evaluates it at runtime and matches it
% with the atom 'true'.
%
% See assert/1 for use and caveats.
%
-spec assert( token(), expression() ) -> void().
assert( Token, _Expression ) ->

	% Never expected to be called, as replaced by the Myriad parse transform by
	% either of the actual expressions:
	%
	throw( { untransformed_conditional, {assert,2}, Token } ).



% If the specified token has been defined through the command-line and set to
% the specified value, asserts that the specified expression is true,
% i.e. evaluates it at runtime and matches it with the atom 'true'.
%
% See assert/1 for use and caveats.
%
-spec assert( token(), value(), expression() ) -> void().
assert( Token, _Value, _Expression ) ->

	% Never expected to be called, as replaced by the Myriad parse transform by
	% either of the actual expressions:
	%
	throw( { untransformed_conditional, {assert,3}, Token } ).
