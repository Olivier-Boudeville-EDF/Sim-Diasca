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
% Creation date: Friday, December 19, 2014.



% Management of datatypes.
%
% See type_utils_test.erl for the corresponding test.
%
% See also meta_utils for all topics regarding metaprogrammng, parse-transforms,
% etc.
%
-module(type_utils).


% Design notes about types.


% Types may be defined according to three forms, from the most human-focused to
% the most computer-native one:
%
% F1. type-as-a-string, i.e. a textual specification possibly entered from a
% user interface; for example, a type "my_type" may be specified as:
% "foo|bar|[integer]"
%
% F2. type-as-a-contextual-term, i.e. an Erlang term that defines a type, yet
% may still be contextual (i.e. it may depend on other non-builtin types); the
% same example may then be defined as: { union, [ foo, bar, {list,[integer]} ]
% }, where foo and bar are expected to be defined in the context
%
% F3. explicit-type, i.e. a fully explicit, self-standing term defining a type
% (therefore relying only on built-in types and type constructs); for example,
% supposing that the type foo is an alias for float, and that the type bar is
% specified as "'hello'|'goodbye'", the same example translates to the following
% explicit type: {union, [float, {union,[ {atom,hello}, {atom,goodbye}]},
% {list,[integer]}]}

% Going from:
%  - form F1 to form F2 is named (here) type parsing
%  - form F2 to form F3 is named (here) type resolution




% On type names and signatures.

% A type T (whether built-in or user-defined) is designated directly by its name
% T, as an atom. Ex: written as "count", refered to as: count.

% There are reserved type-related names (atoms), which correspond to:
%  - built-in types: atom, integer, float, boolean, string, any, none
%  - type constructs: list, union, tuple, table


% A type signature is made from the type name and from a list of the type names
% (if any) it depends upon.

% For monomorphic types (i.e. types that are not parametrised by other types),
% their signature is their sole name. Ex: "foo" ("foo()" is also accepted).

% The signature of polymorphic types (i.e. types that are parametrised by other
% types) is made of their name immediately followed by a list of the names of
% the types they depend upon, enclosed in parentheses.
%
% For example, a polymorphic type T that depends on types T1, T2, ..., Tk may
% have for signature "T(T1, T2, ..., Tk)".


% Let D( type_signature() ) -> type() be a pseudo-function returning the
% explicit type definition (as a term) of a type (designated by its signature).




% On built-in types.


% The type 'atom' designates the set of (possibly user-defined) symbols (ex:
% 'true' or 'foo'). In a type definition, such a symbol consists on the atom
% itself, and is always written enclosed in single quotes ("'foo'"), in order to
% distinguish it from the user-defined types (as one may define a type named
% foo). So 'foo' can be considered here both as a type name and a value.

% The type 'integer' designates an integer value. A value of that type is for
% example 4.

% The type 'float' designates a floating-point value. A value of that type is
% for example 3.14.

% The type 'boolean' designates a truth value, either 'true' or 'false'.

% The type 'string' designates a string of characters (a text). A value of that
% type is for example "Yellow submarine".

% The type 'any' designates a value of any type (hence all values may be seen as
% being of the 'any' type). Of course the actual, most precise type shall be
% preferred wherever possible; this type is defined mostly for formal reasons
% (completeness of the language of types)

% The type 'none' designates a value not having a type, which cannot happen
% operationally (defined also on formal grounds, for completeness).

% Finally, for a built-in type T (designated as a whole - as opposed to defining
% immediate values of it, as discussed in next section), D(T) = T. For example,
% D(atom) = atom, or D(my_type) = my_type.



% On immediate values of a given type.

% We need to be able to specify immediate values even at a type level, as we
% might want to define a type as a set of possible values (such as: [2,3,5,7,11]
% or [ 'orange', 'blue', 'red' ]).

% Let T1 be a type defined from an immediate value V of a type that is named T2
% (hence T1 is a type comprising a single value); T1 is specified as "V"
% (knowing that T2 can be inferred from V), and D(T1) = { T2, V }.
%
% So, for example:
%
% - let A be a type corresponding to an immediate value of type atom; D(A) =
% {atom, A}; for example, D(foo) = {atom, 'foo'}
%
% - let I be a type corresponding to an immediate value of type integer; D(I) =
% {integer, I}; for example, D(4) = {integer, 4}
%
% - let F be a type corresponding to an immediate value of type float; D(F) =
% {float, F}; for example, D(3.14) = {float, 3.14}
%
% - let S be a type corresponding to an immediate value of type string; D(S) =
% {string, S}; for example, D("Yellow submarine") = {string, "Yellow submarine"}




% On type constructs.
%
% The supported type constructs are:
%  - list
%  - union
%  - tuple
%  - table
%
% Note: they can also be seen as built-in polymorphic types.


% On lists:
%
% Let L be a type corresponding to an (homogeneous, ordered) list (variable-size
% container) whose all elements are of type T.
%
% L is written "[T]" and defined as D([T]) = {list, D(T)}.
%
% For example, if my_integer_list_type is defined as "[integer]", then
% D(my_integer_list_type) = D([integer]) = {list, integer}
%
% A value of that type may be [] or [4, 9, 147, 5, 9].


% On unions:
%
% Let U be a type corresponding to the union of a set of types T1, T2, Tk; a
% value of type U is thus of at least one of the types of that union.
%
% U is written as "T1|T2|...|Tk" and defined as D(U) = { union,
% [D(T1),D(T2),...,D(Tk)] }.
%
% For example, if my_type is defined as "foo|'kazoo'|[integer]", then D(my_type)
% = {union, [foo, {atom,'kazoo'}, {list,integer}]}.
%
% Values of that types may be 'kazoo', [3,3] of any value of type foo (whatever
% it may be).
%
% One can note that the foo type can also be replaced by its actual definition
% in order to fully resolve my_type (i.e. to go from form F2 to form F3)
%
% We can see here that the boolean type is nothing but the 'true'|'false' union
% and is not in an irreducible form (yet it is still considered as being fully
% explicit).


% On tuples:
%
% Let T be a type corresponding to a fixed-size, ordered container whose
% elements are respectively of type T1, T2, Tk.
%
% D(T) = {tuple, [D(T1), D(T2), ..., D(Tk)]}.
%
% For example, if my_tuple_type is defined as "{integer,boolean|float,[atom]}"
% then D(my_tuple_type)= {list, [integer, {union, [boolean, float]},
% {list,atom}]}.
%
% Values of that type may be {1, true, []} or {42,8.9,[joe,dalton]}.


% On (associative) tables:
%
% Let T be an associative table whose keys are of type Tk and values are of type
% Tv.
%
% D(T) = {table, [D(Tk),D(Tv)]}.
%
% For example, if my_table_type is defined as "table(integer, string)" then
% D(my_table_type)= {table, [integer, string]}.
%
% Values of that type are opaque (their translation as terms should remain
% unbeknownst to the user, as if they were black boxes); such terms are to be
% solely created and handled as a whole by the 'table' pseudo-module.
%
% For example, MyEmptyTable = table:table(), MyTable =
% table:add_new_entry(42,"This is the answer"), MyOtherTable = table:new([{1,
% "One"}, {2, "Two"}, {5, "Five"}]).
%
% Note: tables are not yet supported.



% To contrast, here are a few Erlang examples, obtained thanks to
% meta_utils:string_to_form/1 (see http://erlang.org/doc/apps/erts/absform.html
% for more details); a forward slash ("/") separates these Erlang forms from the
% type constructs defined here.
%
% For instance meta_utils:string_to_form("-type my_type() :: 'a'|'b'."). yields:
% {attribute,1,type,{my_type,{type,1,union,[{atom,1,a},{atom,1,b}]},[]}); this
% may be read as the my_type type being defined as
% {type,1,union,[{atom,1,a},{atom,1,b}]}.
%
% We have thus following respective translations of monomorphic types:
% (format of the bullets below: "ERLANG_TYPE_SPEC" / "OUR_SPEC" -> ERLANG_FORM /
% OUR_TERM)
%
% - single-value types:
%   - "4" / "4" -> {integer,1,4} / {integer,4}
%   - "foo" or "'foo'" / "'foo'" -> {atom,1,foo} / {atom,foo}
%
% - alias types:
%    - "float()" / "float" -> {type,1,float,[]} / float

%    - "my_other_type() / "my_other_type" or "my_other_type()" ->
%    {user_type,1,my_other_type,[]} / my_other_type
%
% - union types: "'a'|'b'" / "'a'|'b'" -> {type,1,union,[{atom,1,a},{atom,1,b}]}
% / {union,[{atom,a},{atom,b}]}
%
% - list types : "list(integer())" or "[integer()]" / "[integer]" ->
% {type,1,list,[{type,1,integer,[]}]} / { list, integer }

% - random examples:
%
% - "{integer(),float()}" / "{integer,float}" ->
%        {type,1,tuple, [{type,1,integer,[]},{type,1,float,[]}]} /
%        {tuple,[integer,float]}



% Describes the name of a type (without the names of the types it depends on,
% for polymorphic ones).
%
% Ex: 'my_count'
%
-type type_name() :: atom().


% Number of types a (possibly polymorphic) type depends on (possibly zero for
% plain types).
%
-type type_arity() :: count().


% Analoguous to function_id/0:
-type type_id() :: { type_name(), type_arity() }.




% The "most precise" description of a primitive, simple type (ex: 'boolean' and
% 'atom') coexist, 'number' are not used, etc.
%
% A note about Erlang floats: they are actually IEEE 754 double-precision
% floating-point numbers, a format that occupies 8 bytes (64 bits) per float in
% memory.
%
% More precisely, as one can see in erts/emulator/beam/erl_term.h, a float_def
% is an union able to contain a ieee754_8 datatype, aliased to the 'double' C
% datatype.
%
% Polymorphic types (ex: lists) are described with no mention of the types they
% may depend on (ex: 'list' can be specified, not 'list(float())' or anything
% like that).
%
-type primitive_type_description() :: 'atom'
									| 'binary'
									| 'boolean'
									| 'float'
									| 'function'
									| 'integer'
									| 'list'
									| 'pid'
									| 'port'
									| 'record'
									| 'reference'
									| 'tuple'.


% The description of any given type is based on primitive_type_description/0)
% and can be done in two complementary forms: the textual one, and the internal
% one, which are relatively different.



% Textual type description: type-as-a-string, inspired from the syntax used for
% type specifications (http://erlang.org/doc/reference_manual/typespec.html),
% yet different. Notably, monomorphic types do not end with empty parentheses
% (ex: "integer", not "integer()") and atoms are always surrounded by simple
% quotes (ex: "'an_atom'|'another_one'").
%
% For example: "[{float, boolean}]".
%
-type type_description() :: ustring().



% Description of a nesting depth reached when parsing a type description.
%
% It is in pratice a {P,B} pair, where P is the parenthesis depth (i.e. the
% number of the parentheses that have been opened and not closed yet) and B is
% the bracket depth (i.e. the same principle, for "[]" instead of for "()"):
%
-type nesting_depth() :: { count(), count() }.


% Internal, "formal", actual programmatic description of a type according to our
% conventions: type-as-a-term (either contextual or explicit, F2 or F3), relying
% on a translated version of the textual type (which is for example:
% "[{float,boolean}]").
%
% This "internal type language of the Myriad layer" is largely inspired from the
% forms that can be found in actual ASTs.
%
% Requirements for this term-based description were:
%
% - be able to represent at least any actual (that can be readily instantiated,
% hence non-polymorphic) type (like "-type a() :: ...", not "-type a(T) ::
% ..."); should, in the future, polymorphic types have to be *defined* (not
% merely used), then (non-empty) parentheses could be introduced
%
% - be able to nevertheless *use* polymorphic types, as they are certainly
% useful (ex: associative tables, lists, etc.); a problem is that, in terms (as
% opposed to in the textual counterpart), parentheses cannot be used to express
% these polymorphic types (not only they denote function calls, but also are
% not legit components of a term); therefore the convention chosen here is to
% specify types as pairs, the first element being the name of the type, the
% second one being the (ordered) list of the types it depends on; then the
% textual type "a( T1, T2 )" is translated to the {a,[T1,T2]} type term; most
% types being "monomorphic", they are represented as {my_simple_type,[]} (which
% cannot be abbreviated by only the 'my_simple_type' atom, as it would lead to
% ambiguous forms)
%
% So, as an example, the type-as-a-term corresponding to "[{float,boolean}]"
% is: {list, [{tuple, [{float,[]}, {boolean,[]} ]}]}
%
% Note that an alternate type language (sticking more closely to its textual
% counterpart) could have been a more direct [{float,boolean}] term (hence
% getting rid of the parentheses and the pair with an empty list in second
% position); reason for not doing so: then no possible support of the
% polymorphic types that happen to be often needed.
%
% The origin of this term-as-a-type notation is clearly the standard (Erlang)
% type specifications; for example 'meta_utils:string_to_form( "-type a() ::
% [{float(),boolean()}]." ).' returns following AST form:
%
%  '{attribute,1,type, {a,{type,1,list, [{type,1,tuple,[{type,1,float,[]},
%  {type,1,boolean,[]}]}]}'
%
% As a result the counterpart to the aforementioned "[{float(), boolean()}]"
% type string is translated in ASTs as:
%
% {type, 1, list, [{type, 1, tuple, [{type, 1, float, []}, {type, 1, boolean,
% []}]}]}
%
% Then one can remove:
%
% - the 'type' (and 'user_type') atoms (not making then a specific distinction
% between the origin of a type); a list of built-in types - names and arities -
% is maintained, other types being then user ones)
%
% - the line numbers (the '1's here), not useful in that context, hence stripped
%
% Then we obtain our aforementioned term-as-a-type:
%    {list, [{tuple, [{float,[]}, {boolean,[]}]}]}
%
% We can therefore describe this way arbitrary types as valid terms.
%
% Next steps:
%
% - define and document the full type language (elementary datatypes - like
% boolean, integer, float, symbols - and constructs - like list, tuple, union,
% atom)
%
% - support it, notably define functions to tell whether a given term is an
% instance of a specified type
%
% Experiment with meta_utils:string_to_form/1 and have fun!
%
% Ex: "-type a() :: [foobar()]." yields: '{attribute,1,type, {a,{type,1,
%    list,[{user_type,1,foobar,[]}]},[]}}'.
%
% See also: http://erlang.org/doc/apps/erts/absform.html
%
% Finally, a direct string representation can be converted into a type(); maybe
% writing a parser may not mandatory, as "{float(), atom()}" may be a string
% expression evaluated with functions that we can bind to obtain a closer term,
% such as: float() -> {float, []}.
%
% Of course, on a related note, if TextualType = "{ list, [
% {tuple,[float,boolean]} ] }", then meta_utils:string_to_value( TextualType )
% will return the expected: {list, [{tuple, [{float, []}, {boolean, []}]}]}
%
% Note that such a type may not be fully explicit, as it may contain unresolved
% references to other types; for example: {list, [{count, []}]} does not specify
% what the count() type is.
%
-type type() :: term().



% An explicit type is a type that has been fully resolved in terms of built-in
% constructs; it is thus autonomous, self-standing.
%
-type explicit_type() :: type().



% Tuploids. See also augment_tuploid/2.

% We name tuploid a pseudo-tuple, i.e. a value that is either an actual tuple or
% a single, standalone term, designated as a "basic tuploid".
%
% That is, a tuploid is a tuple of any size, except that the tuploid of size 1
% is MyTerm, not {MYterm}.
%
-type tuploid() :: tuploid( term() ).


% Probably that such a tuple would contain at least an element of type T:
-type tuploid( T ) :: tuple() | T.


-export_type([ type_name/0, type_arity/0, type_id/0,
			   primitive_type_description/0,
			   type_description/0, nesting_depth/0, type/0, explicit_type/0,
			   tuploid/0, tuploid/1 ]).


% Note: currently, only a very basic, ad hoc type support ("hand-made look-up
% tables") is provided.
%
% Later we would like to really parse any type description (ex: "[ { float, [
% boolean ] } ]") and be able to manage it as type() (including the checking of
% terms against types).



% Type-related functions:
-export([ description_to_type/1, type_to_description/1, type_to_string/1,
		  get_type_of/1, interpret_type_of/1, interpret_type_of/2,
		  get_immediate_types/0, get_ast_simple_builtin_types/0,
		  get_elementary_types/0, get_simple_builtin_types/0,
		  is_type/1, is_of_type/2,
		  is_of_described_type/2, is_homogeneous/1, is_homogeneous/2,
		  are_types_identical/2 ]).



% Conversion:
-export([ ensure_integer/1, ensure_float/1, ensure_number/1, ensure_boolean/1,
		  ensure_string/1, ensure_binary/1 ]).


% Checking:
-export([ check_atom/1, check_boolean/1, check_pid/1, check_list/1,
		  check_binary/1, check_binaries/1, check_tuple/1 ]).


% Specials for datatypes:
-export([ augment_tuploid/2 ]).


% Work in progress:
-export([ tokenise_per_union/1 ]).


% Shorthands:

-type count() :: basic_utils:count().
-type level() :: basic_utils:level().

-type ustring() :: text_utils:ustring().



% Returns the actual type corresponding to specified type description: parses
% the specified string to determine the type described therein.
%
% Note: returns a correct type, but currently rarely the expected, most precise
% one.
%
-spec description_to_type( type_description() ) -> type().
description_to_type( TypeDescription ) ->

	CanonicalDesc = text_utils:remove_whitespaces( TypeDescription ),

	%io:format( "CanonicalDesc = '~ts'~n", [ CanonicalDesc ] ),

	scan_type( CanonicalDesc ).



% To perform its parsing, we must split the full description recursively.
%
% The worst (and thus first) top-level construct to detect is the union. We
% consider that we are always in an union (possibly including only one term, in
% which case it can be simplified out.
%
% We do that by scanning for terms from left-to-right, keeping track of the
% nesting.
%
%-spec scan_type( type_description() ) -> type().
%scan_type( TypeDescription ) ->
	%case tokenise_per_union( TypeDescription ) of

	%	[ T ] ->
	%		T;

	%	UnionisedTypes ->
	%		{ union, [ scan_type( T ) || T <- UnionisedTypes ] }

	%end.

% Last: all other types.
scan_type( _TypeDescription ) ->
	% Most imprecise (yet correct) type (commented-out as may hide issues):
	any.

	% Either not yet implemented or plain wrong:
	%throw( { type_interpretation_failed, TypeDescription } ).


% Splits the specified type description according to union delimiters
-spec tokenise_per_union( type_description() ) -> [ type_description() ].
tokenise_per_union( TypeDescription ) ->

	% We track the nesting depth and only fetch the top-level union members;
	InitialNestingDepth = { _P=0, _B=0 },
	parse_nesting( TypeDescription, InitialNestingDepth ).



% Parses the specified type description in order to split it according in nested
% sub-expressions that may be recursively parsed.
%
-spec parse_nesting( type_description(), nesting_depth() ) ->
							[ type_description() ].
parse_nesting( _TypeDescription, _NestingDepth ) ->

	% A goal is to detect atoms delimited with single quotes (which are
	% immediate atom values) from the unquoted ones (which designate types)
	%
	throw( not_implemented_yet ).





% Returns the type description (in canonical form, notably without whitespaces)
% corresponding to specified type.
%
% Note: currently does not return a really relevant type description; basically
% meant to be the function reciprocal to scan_type/1.
%
-spec type_to_description( type() ) -> type_description().
% First, simple types, in alphabetical order:
type_to_description( _Type=atom ) ->
	"atom";

type_to_description( _Type=integer ) ->
	"integer";

type_to_description( _Type=float ) ->
	"float";

type_to_description( _Type=boolean ) ->
	"boolean";

type_to_description( _Type=string ) ->
	"string";

type_to_description( _Type=any ) ->
	"any";

type_to_description( _Type=none ) ->
	"none";


% Then polymorphic constructs:


% No "list()"-like (with no specific type) supported.

type_to_description( _Type={ list, T } ) ->
	"[" ++ type_to_description( T ) ++ "]";

type_to_description( _Type={ union, TypeList } ) when is_list( TypeList ) ->
	text_utils:join( _Separator="|",
					 [ type_to_description( T ) || T <- TypeList ] );

type_to_description( _Type={ tuple, TypeList } ) when is_list( TypeList ) ->
	TypeString = text_utils:join( _Separator=",",
					  [ type_to_description( T ) || T <- TypeList ] ),
	"{" ++ TypeString ++ "}";

type_to_description( _Type={ table, [ Tk, Tv ] } ) ->
	"table(" ++ type_to_description( Tk ) ++ "," ++ type_to_description( Tv )
		++ ")";


type_to_description( Type ) ->

	% Could be misleading (ex: any() not matching any()):
	%"any".

	%text_utils:format( "~p", [ Type ] ).

	throw( { type_description_failed, Type } ).



% Returns a textual representation of the specified type.
-spec type_to_string( type() ) -> ustring().
type_to_string( Type ) ->
	type_to_description( Type ).



% Returns an atom describing, as precisely as possible, the overall type of the
% specified primitive term.
%
% Note: limited to primitive types, not compounded ones (like [float()]).
%
% is_number/1, is_record/1, etc. not usable here.
%
% Note: often we do not want to retrieve the actual type of a term but need
% instead to determine whether the term can be considered as an instance of a
% specific type (this is not strictly the same need, as a given term in general
% may be seen of being of multiple types).
%
-spec get_type_of( term() ) -> primitive_type_description().
get_type_of( Term ) when is_boolean( Term ) ->
	'boolean';

get_type_of( Term ) when is_atom( Term ) ->
	'atom';

get_type_of( Term ) when is_binary( Term ) ->
	'binary';

get_type_of( Term ) when is_float( Term ) ->
	'float';

get_type_of( Term ) when is_function( Term ) ->
	'function';

get_type_of( Term ) when is_integer( Term ) ->
	'integer';

get_type_of( Term ) when is_pid( Term ) ->
	'pid';

get_type_of( Term ) when is_list( Term ) ->
	case text_utils:is_string( Term ) of

		true ->
			'string';

		false ->
			case text_utils:are_strings( Term ) of

				true ->
					'[string]';

				false ->
					'list'

			end

	end;

get_type_of( Term ) when is_map( Term ) ->
	'map';

get_type_of( Term ) when is_port( Term ) ->
	'port';

%get_type_of( Term ) when is_record( Term ) ->
%	'record';

get_type_of( Term ) when is_tuple( Term ) ->
	'tuple';

get_type_of( Term ) when is_reference( Term ) ->
	'reference';

get_type_of( Term ) ->
	throw( { unknown_type_for, Term } ).




% Returns a string describing, in a user-friendly manner, the type of the
% specified term (up to one level of nesting detailed).
%
-spec interpret_type_of( term() ) -> ustring().
interpret_type_of( Term ) ->
	interpret_type_helper( Term, _CurrentNestingLevel=0,
						   _MaxNestingLevel=1 ).


% Returns a string describing, in a user-friendly manner, the type of the
% specified term, up to the specified nesting level (either a positive integer
% or the 'infinite' atom, to go as deep as possible in the term structure).
%
-spec interpret_type_of( term(), level() | 'infinite' ) -> ustring().
interpret_type_of( Term, MaxNestingLevel ) when MaxNestingLevel >= 0 ->
	interpret_type_helper( Term, _CurrentNestingLevel=0, MaxNestingLevel ).



% Returns a string describing, in a user-friendly manner, the type of the
% specified term, describing any nested subterms up to the specified level.
%
-spec interpret_type_helper( term(), level(), level() ) -> ustring().
interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel )
  when is_boolean( Term ) ->
	text_utils:format( "boolean of value '~ts'", [ Term ] );

interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel )
  when is_atom( Term ) ->
	text_utils:format( "atom of value '~ts'", [ Term ] );

interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel )
  when is_binary( Term ) ->
	% Text might be incorrectly encoded ('~ts' would be needed):
	text_utils:format( "binary of value '~p'", [ Term ] );

interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel )
  when is_float( Term ) ->
	text_utils:format( "float of value '~f'", [ Term ] );

interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel )
  when is_function( Term ) ->
	text_utils:format( "function of value '~w'", [ Term ] );

interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel )
  when is_integer( Term ) ->
	text_utils:format( "integer of value '~B'", [ Term ] );

interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel )
  when is_pid( Term ) ->
	text_utils:format( "PID of value '~w'", [ Term ] );


interpret_type_helper( Term, _CurrentNestingLevel=MaxNestingLevel,
					   MaxNestingLevel ) when is_map( Term ) ->
	text_utils:format( "map of ~B elements", [ maps:size( Term ) ] );

interpret_type_helper( Term, CurrentNestingLevel, MaxNestingLevel )
  when is_map( Term ) ->

	Elems = [ text_utils:format( "key ~ts associated to value ~ts",
				   [ interpret_type_helper( K, CurrentNestingLevel + 1,
											MaxNestingLevel ),
					 interpret_type_helper( V, CurrentNestingLevel + 1,
											MaxNestingLevel ) ] )
			  || { K, V } <- maps:to_list( Term ) ],

	text_utils:format( "map of ~B elements: ~ts", [ maps:size( Term ),
			text_utils:strings_to_string( Elems, CurrentNestingLevel ) ] );


interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel )
  when is_port( Term ) ->
	text_utils:format( "port of value '~p'", [ Term ] );

interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel )
  when is_reference( Term ) ->
	text_utils:format( "reference of value '~p'", [ Term ] );

interpret_type_helper( _Term=[], _CurrentNestingLevel, _MaxNestingLevel ) ->
	"empty list/string";

interpret_type_helper( Term, CurrentNestingLevel, MaxNestingLevel )
  when is_list( Term ) ->

	case text_utils:is_string( Term ) of

		true ->
			text_utils:format( "plain string '~ts'", [ Term ] );

		false ->
			case CurrentNestingLevel of

				MaxNestingLevel ->
					text_utils:format( "list of ~B elements",
									   [ length( Term ) ] );

				_ ->
					Elems = [ interpret_type_helper( E,
								CurrentNestingLevel + 1, MaxNestingLevel )
							  || E <- Term ],

					text_utils:format( "list of ~B elements: ~ts",
						[ length( Term ),
						  text_utils:strings_to_enumerated_string( Elems,
												   CurrentNestingLevel ) ] )

			end

	end;


interpret_type_helper( _Term={ _A, _B }, _CurrentNestingLevel=MaxNestingLevel,
					   MaxNestingLevel ) ->
	"pair";

interpret_type_helper(_Term={ _A, _B, _C },
		  _CurrentNestingLevel=MaxNestingLevel, MaxNestingLevel ) ->
	"triplet";

interpret_type_helper( Term, _CurrentNestingLevel=MaxNestingLevel,
					   MaxNestingLevel ) when is_tuple( Term ) ->
	text_utils:format( "tuple of ~B elements", [ size( Term ) ] );

interpret_type_helper( Term, CurrentNestingLevel, MaxNestingLevel )
  when is_tuple( Term ) ->

	Elems = [ interpret_type_helper( E, CurrentNestingLevel + 1,
									 MaxNestingLevel )
			  || E <- tuple_to_list( Term ) ],

	BaseTupleDesc = interpret_type_helper( Term, MaxNestingLevel,
										   MaxNestingLevel ),

	text_utils:format( "~ts made of: ~ts", [ BaseTupleDesc,
		text_utils:strings_to_enumerated_string( Elems,
												 CurrentNestingLevel ) ] );


interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel )
  when is_port( Term ) ->
	text_utils:format( "port of value '~p'", [ Term ] );

interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel )
  when is_reference( Term ) ->
	text_utils:format( "reference of value '~p'", [ Term ] );

interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel ) ->
	text_utils:format( "unknown type for '~p'", [ Term ] ).




% Returns a list of the possible types for immediate values.
-spec get_immediate_types() -> [ type_name() ].
get_immediate_types() ->
	% Not sure this list is very accurate or relevant:
	[ 'atom', 'float', 'integer', 'binary', 'boolean' ].



% Returns a list of the possible types for immediate values (typically found in
% an AST like, like 'undefined' in: {atom,42,undefined}).
%
% From http://erlang.org/doc/apps/erts/absform.html:
%
% "There are five kinds of atomic literals, which are represented in the same
% way in patterns, expressions, and guards:
%
% - If L is an atom literal, then Rep(L) = {atom,LINE,L}.
%
% - If L is a character literal, then Rep(L) = {char,LINE,L}.
%
% - If L is a float literal, then Rep(L) = {float,LINE,L}.
%
% - If L is an integer literal, then Rep(L) = {integer,LINE,L}.
%
% - If L is a string literal consisting of the characters C_1, ..., C_k, then
% Rep(L) = {string,LINE,[C_1, ..., C_k]}."
%
% Actually additional types can be found in ASTs.
%
-spec get_ast_simple_builtin_types() -> [ type_name() ].
get_ast_simple_builtin_types() ->

	% See http://erlang.org/doc/reference_manual/typespec.html for a complete
	% list:
	%
	[ 'term', 'binary', 'bitstring', 'boolean', 'byte', 'char', 'nil', 'number',
	  'list', 'maybe_improper_list', 'nonempty_list', 'string',
	  'nonempty_string', 'iodata', 'iolist', 'function', 'module', 'mfa',
	  'arity', 'identifier', 'node', 'timeout', 'no_return',
	  'any', 'integer', 'float', 'atom', 'pos_integer', 'neg_integer',
	  'non_neg_integer', 'pid', 'reference', 'port' ].



% Returns a list of the elementary, "atomic" types.
-spec get_elementary_types() -> [ type_name() ].
get_elementary_types() ->
	get_immediate_types() ++
		[ 'function', 'list', 'pid', 'port', 'record', 'reference', 'tuple',
		  'any' ].


% Returns a list of the built-in, non-polymorphic types that can be typically
% found in AST forms.
%
-spec get_simple_builtin_types() -> [ type_name() ].
get_simple_builtin_types() ->
	get_immediate_types() ++ [ 'pid', 'port', 'reference', 'any', 'no_return' ].





% Tells whether specified term designates a type (i.e. a type() instance).
%
% (only the elementary types are currently recognised)
%
-spec is_type( term() ) -> boolean().
%is_type( { Tag, SubTypes } ) when is_list( SubTypes ) ->
%	lists:member( Tag, get_elementary_types() );
%
%is_type( _T ) ->
%	false.

% To be implemented:
is_type( _T ) ->
	true.



% Tells whether specified term is of specified type (predicate).
%
% Note: currently only a very partial checking is made, based on top-level
% primitive types; later the type will be recursed into, in order to check
% whether the term complies with this expected structure.
%
-spec is_of_type( term(), type() ) -> boolean().
is_of_type( _Term, _Type='any' ) ->
	true;

is_of_type( Term, _Type='string' ) when is_list( Term ) ->
	text_utils:is_string( Term );

is_of_type( Term, Type ) ->

	case get_type_of( Term ) of

		Type ->
			true;

		_ ->
			false

	end.



% Tells whether the specified term is of specified textually-described type.
%
% Note: currently no checking is made and the test always succeeds.
%
-spec is_of_described_type( term(), type_description() ) -> boolean().
is_of_described_type( _Term, _TypeDescription ) ->

	%throw( { not_implemented_yet, {is_of_described_type,2} } ).

	% ActualType = description_to_type( TypeDescription ),
	% is_of_type( ActualType ).

	true.



% Tells whether specified non-empty container (list or tuple) is homogeneous in
% terms of type, i.e. whether all its elements are of the same type.
%
% If true, returns the common type.
% If false, returns two of the different types found in the container.
%
-spec is_homogeneous( list() | tuple() ) ->
		{ 'true', primitive_type_description() } | { 'false',
			{ primitive_type_description(), primitive_type_description() } }.
is_homogeneous( _List=[] ) ->
	% We want to return types:
	throw( empty_container );

is_homogeneous( _List=[ H | T ] ) ->

	Type = get_type_of( H ),

	is_homogeneous_full_helper( T, Type );

is_homogeneous( Tuple ) when is_tuple( Tuple ) ->

	ElemList = tuple_to_list( Tuple ),

	is_homogeneous( ElemList ).



% Tells whether specified non-empty container (list or tuple) is homogeneous in
% terms of type, i.e. whether all its elements are of the same, specified,
% primitive type.
%
-spec is_homogeneous( list() | tuple(), primitive_type_description() ) ->
							boolean().
is_homogeneous( _List=[], _Type ) ->
	% Considered homogeneous:
	true;

is_homogeneous( List, Type ) when is_list( List ) ->
	is_homogeneous_helper( List, Type );

is_homogeneous( Tuple, Type ) when is_tuple( Tuple ) ->

	ElemList = tuple_to_list( Tuple ),

	is_homogeneous_helper( ElemList, Type ).


% Helper:
is_homogeneous_full_helper( _Elems=[], Type ) ->
	{ true, Type };

is_homogeneous_full_helper( _Elems=[ H | T ], Type ) ->

	case get_type_of( H ) of

		Type ->
			is_homogeneous_full_helper( T, Type );

		OtherType ->
			{ false, { Type, OtherType } }

	end.


% Other helper:
is_homogeneous_helper( Elems, Type ) ->
	{ Bool, _TypeInfo } = is_homogeneous_full_helper( Elems, Type ),
	Bool.



% Tells whether the two specified types are the same (i.e. designate the same
% actual type, are aliases).
%
-spec are_types_identical( type(), type() ) -> boolean().
are_types_identical( Type, Type ) ->
	true;

are_types_identical( _FirstType, _SecondType ) ->
	false.





% ensure_* section.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. We may even decide
% in the future to output warning traces whenever the specified element happens
% not to be of the target type.



% Ensures that specified term is an integer, and returns it.
%
% If it is a float, will return a truncated (integer) version of it.
%
-spec ensure_integer( number() ) -> integer().
ensure_integer( N ) when is_integer( N ) ->
	N;

ensure_integer( N ) when is_float( N ) ->
	trunc( N );

ensure_integer( N ) ->
	throw( { cannot_be_cast_to_integer, N } ).



% Ensures that specified term is a float, and returns it.
%
% If it is an integer, will return a floating-point version of it.
%
-spec ensure_float( number() ) -> float().
ensure_float( N ) when is_float( N ) ->
	N;

ensure_float( N ) when is_integer( N ) ->
	float( N );

ensure_float( N ) ->
	throw( { cannot_be_cast_to_float, N } ).



% Ensures that specified term is a number, and returns it.
-spec ensure_number( number() ) -> number().
ensure_number( N ) when is_number( N ) ->
	N;

ensure_number( N ) ->
	throw( { not_a_number, N } ).



% Ensures that specified term is a boolean, and returns it.
-spec ensure_boolean( term() ) -> boolean().
ensure_boolean( B ) when is_boolean( B ) ->
	B;

ensure_boolean( B ) ->
	throw( { not_a_boolean, B } ).



% Ensures that specified term is a string, and returns it.
-spec ensure_string( term() ) -> ustring().
ensure_string( S ) ->
	text_utils:ensure_string( S ).



% Ensures that specified term is a binary string, and returns it.
-spec ensure_binary( term() ) -> ustring().
ensure_binary( S ) ->
	text_utils:ensure_binary( S ).



% Checks that specified term is an atom indeed, and returns it.
-spec check_atom( term() ) -> atom().
check_atom( Atom ) when is_atom( Atom ) ->
	Atom;

check_atom( Other ) ->
	throw( { not_atom, Other } ).



% Checks that specified term is a boolean indeed, and returns it.
-spec check_boolean( term() ) -> atom().
check_boolean( true ) ->
	true;

check_boolean( false ) ->
	false;

check_boolean( Other ) ->
	throw( { not_boolean, Other } ).



% Checks that specified term is a PID indeed, and returns it.
-spec check_pid( term() ) -> pid().
check_pid( Pid ) when is_pid( Pid ) ->
	Pid;

check_pid( Other ) ->
	throw( { not_pid, Other } ).



% Checks that specified term is a list indeed, and returns it.
-spec check_list( term() ) -> list().
check_list( List ) when is_list( List ) ->
	List;

check_list( Other ) ->
	throw( { not_list, Other } ).



% Checks that specified term is a binary indeed, and returns it.
-spec check_binary( term() ) -> binary().
check_binary( Binary ) when is_binary( Binary ) ->
	Binary;

check_binary( Other ) ->
	throw( { not_binary, Other } ).


% Checks that specified term is a list of binaries indeed, and returns it.
-spec check_binaries( term() ) -> [ binary() ].
check_binaries( Binaries ) ->
	[ check_binary( B ) || B <- Binaries ].




% Checks that specified term is a tuple indeed, and returns it.
-spec check_tuple( term() ) -> tuple().
check_tuple( Tuple ) when is_tuple( Tuple ) ->
	Tuple;

check_tuple( Other ) ->
	throw( { not_tuple, Other } ).




% Augments the specified tuploid with specified term.
%
% Ex: augment_tuploid(a, 2.0) = {a, 2.0}
%     augment_tuploid({foo, 42}, 2.0) = {foo, 42, 2.0}
%
% Useful typically to augment returned error tuploids (either a single error
% term such as 'invalid_name', or a tuple like '{invalid_name,"Arnold"}' with
% caller-local information, to obtain in all cases a tuploid (a tuple here) with
% that extra information.
%
-spec augment_tuploid( tuploid(), term() ) -> tuploid().
augment_tuploid( Tuploid, ExtraTerm ) when is_tuple( Tuploid ) ->
	List = list_utils:append_at_end( ExtraTerm, tuple_to_list( Tuploid ) ),
	list_to_tuple( List );

augment_tuploid( BasicTuploid, ExtraTerm ) ->
	{ BasicTuploid, ExtraTerm }.
