% Copyright (C) 2014-2025 Olivier Boudeville
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

-module(type_utils).

-moduledoc """
Module helping to manage **datatypes** (and also values), notably in ASTs, but
also for our (admittedly very limited) own type language (which does not
specifically depends on Erlang).

See `type_utils_test.erl` for the corresponding test.

See also `meta_utils` for all topics regarding metaprogramming,
parse-transforms, etc.
""".



% Design notes about types.


% Types may be defined according to three forms, from the most human-focused to
% the most computer-native one:
%
% F1. type-as-a-string, i.e. a textual specification of a type, possibly entered
% from a user interface; for example, a type (possibly named "my_type") may be
% specified as: "union(foo(), bar(), [integer()])"; this corresponds to the
% text_type() type
%
% F2. type-as-a-contextual-term, i.e. an Erlang term that defines a type, yet
% may still be contextual (i.e. it may depend on other non-builtin types); the
% same example may then be defined as the {union, [foo, bar, {list,integer}]}
% contextual type (hence a term), where the foo/0 and bar/0 types are expected
% to be defined in the context; this corresponds to the contextual_type() type;
% note that here foo is a shorthand for {foo,[]}
%
% F3. explicit-type, i.e. a fully explicit, self-standing, Erlang-level term
% defining a type (therefore relying only on built-in types and type constructs,
% the initial type being fully resolved into them); for example, supposing that
% the type foo/0 is an alias for float, and that the type bar/0 is specified as
% "'hello'|'goodbye'", the same example translates to the following explicit
% type: {union, [float, {union,[{atom,hello}, {atom,goodbye}]},
% {list,integer}]}; this corresponds to the resolved_type() type

% Going from:
%
% - form F1 (text_type/0) to form F2 (contextual_type/0) is named (here) type
% parsing
%
% - form F2 (contextual_type/0) to form F3 (resolved_type/0) is named (here)
% type resolution

% Instead of "union(T1, T2, T3)" we would have preferred "T1|T2|T3", but reusing
% parts of the parser (possibly erl_parse.yrl) does not seem obvious.


% On type names and signatures.

% A type T (whether built-in or not - possibly user-defined) is designated
% directly by its name T, as an atom, with its arity (supposing it depends on
% the T1, T2, ..., Tk types); its F2 form (representation) is Rep(T) = {type,
% TAsAtom, [Rep(T1), Rep(T2), ..., Rep(Tk)}.
%
% For example a type written as "count()", refered to as: 'count()', would be in
% a F2 form designated as {type,count,[]} - and not just as
% 'count'.

% There are reserved type-related names (atoms), which correspond to:
%  - built-in types: atom, integer, float, boolean, string, any, none
%  - type constructs: maybe, list, table, tuple, union


% A type signature is made from the type name and from a list of the type names
% (if any) it depends upon.

% For monomorphic types (i.e. types that are not parametrised by other types),
% their signature is their sole name. For example "foo" ("foo()" is also
% accepted).

% The signature of polymorphic types (i.e. types that are parametrised by other
% types) is made of their name immediately followed by a list of the names of
% the types they depend upon, enclosed in parentheses.
%
% For example, a polymorphic type T that depends on types T1, T2, ..., Tk may
% have for signature "T(T1, T2, ..., Tk)".


% Let Rep(type_signature()) -> resolved_type() be a pseudo-function returning
% the explicit type definition (as a term) of a type (designated by its
% signature).




% On built-in types.


% The type 'atom' designates the set of (possibly user-defined) symbols (e.g.
% 'true' or 'foo'). In a type definition, such a symbol consists in the atom
% itself, and is always written enclosed in single quotes ("'foo'"), in order to
% distinguish it from the user-defined types (as one may define a type named
% foo). So 'foo' can be considered here both as a type name and a value.

% The type 'integer' designates an integer value. A value of that type is for
% example 4.

% The type 'float' designates a floating-point value. A value of that type is
% for example 3.14.

% The type 'boolean' designates a truth value, either 'true' or 'false'.

% The type 'string' designates a string of characters (more precisely a UTF8
% text). A value of that type is for example "Yellow submarine".

% The type 'any' designates a value of any type (hence all values may be seen as
% being of the 'any' type). Of course the actual, most precise type shall be
% preferred wherever possible; this type is defined mostly for formal reasons
% (completeness of the language of types).

% The type 'none' designates a value not having a type; this cannot happen
% operationally (all expressions have a value, and all values have a type), but
% means that the actual value is not of interest, i.e. is to be ignored (defined
% also on formal grounds, for completeness).

% Finally, for a built-in type T (designated as a whole - as opposed to defining
% immediate values of it, as discussed in next section), Rep(TAsString) = T. For
% example, Rep("atom") = atom, or Rep("my_type") = my_type.



% On immediate values of a given type.

% We need to be able to specify immediate values even at a type level, as we
% might want to define a type as a set of possible values (such as: [2,3,5,7,11]
% or ['orange', 'blue', 'red']).

% Let T1 be a type defined from an immediate value V of a type that is named T2
% (hence T1 is a type comprising a single value); T1 is specified as "V"
% (knowing that T2 can be inferred from V), and Rep(T1) = {T2, V}.
%
% So, for example:
%
% - let A be a type corresponding to an immediate value of type atom;
% Rep(AAsString) = {atom, A}; for example, Rep("foo") = {atom, 'foo'}
%
% - let I be a type corresponding to an immediate value of type integer;
% Rep(IAsString) = {integer, I}; for example, Rep("4") = {integer, 4}
%
% - let F be a type corresponding to an immediate value of type float;
% Rep(FAsString) = {float, F}; for example, Rep("3.14") = {float, 3.14}
%
% - let S be a type corresponding to an immediate value of type string; Rep(S) =
% {string, S}; for example, Rep("Yellow submarine") = {string, "Yellow
% submarine"}




% On type constructs.
%
% For a type T, let's denote its representation as a term as Rep(T).
%
% The supported type constructs are:
%
%  - maybe/1, i.e. optional values, to express option(T), i.e. T | 'undefined';
%  represented as a term as {maybe, Rep(T)}
%
%  - list/1, i.e. homogeneous lists whose elements are of type T, represented as
%  a term as {list, Rep(T)}; thus to be used for example as '{list, float}' to
%  express [float()]; to designate heterogenous lists, i.e. list() (or [any()]),
%  {list, any} is used.
%
%  - table/2, i.e. associative key/value tables, e.g. '{table,atom,string}' for
%  table(atom(), ustring())
%
%  - tuple/*, i.e. fixed-size tuples of possibly heterogeneous types, e.g
%  '{tuple, [boolean, T, float]}' (a value of that type being thus {true,
%  ValueOfTypeT, 3.14}
%
%  - union/*, i.e. sets of possible types, e.g '{union, [boolean, T, float]}'
%
% Note: they can also be seen as built-in polymorphic types.
%
% Could be added later: integer ranges, records, functions (fun expressions).


% On lists:
%
% Let L be a type corresponding to an (homogeneous, ordered) list (variable-size
% container) whose all elements are of type T.
%
% L is written "[T]" and defined as Rep([T]) = {list, Rep(T)}.
%
% For example, if my_integer_list_type is defined as "[integer]", then
% Rep(my_integer_list_type) = Rep([integer]) = {list, integer}
%
% A value of that type may be [] or [4, 9, 147, 5, 9].


% On (associative) tables (corresponding to maps in Erlang):
%
% Let T be an associative table whose keys are of type Tk and values are of type
% Tv.
%
% Rep(T) = {table, [Rep(Tk),Rep(Tv)]}.
%
% For example, if the my_table_type type is defined as "table(integer, string)"
% then Rep(my_table_type)= {table, [integer, string]}.
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


% On tuples:
%
% Let T be a type corresponding to a fixed-size, ordered container whose
% elements are respectively of type T1, T2, Tk.
%
% Rep(T) = {tuple, [Rep(T1), Rep(T2), ..., Rep(Tk)]}.
%
% For example, if the my_tuple_type type is defined as "{integer,
% union(boolean,float), [atom]}" then Rep(my_tuple_type)= {list, [integer,
% {union, [boolean, float]}, {list,atom}]}.
%
% Values of that type may be {1, true, []} or {42,8.9,[joe,dalton]}.


% On unions:
%
% Let U be a type corresponding to the union of a set of types T1, T2, Tk; a
% value of type U is thus of at least one of the types of that union.
%
% U is written as "union(T1, T2, ..., Tk)" (or, in the future, "T1|T2|...|Tk")
% and defined as Rep(U) = {union, [Rep(T1),Rep(T2),...,Rep(Tk)]}.
%
% For example, if the my_type type is defined as "union(foo(), kazoo,
% [integer()]", then Rep(my_type) = {union, [foo, {atom,'kazoo'},
% {list,integer}]}.
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


% To contrast, here are a few Erlang examples, obtained thanks to
% meta_utils:string_to_form/1 (see http://erlang.org/doc/apps/erts/absform.html
% for more details); a forward slash ("/") separates these Erlang forms from the
% type constructs defined here.
%
% For instance `meta_utils:string_to_form("-type() :: 'a'|'b'.").` yields:
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
%
%    - "my_other_type() / "my_other_type" or "my_other_type()" ->
%    {user_type,1,my_other_type,[]} / my_other_type
%
% - union types: "'a'|'b'" / "'a'|'b'" -> {type,1,union,[{atom,1,a},{atom,1,b}]}
% / {union,[{atom,a},{atom,b}]}
%
% - list types : "list(integer())" or "[integer()]" / "[integer]" ->
% {type,1,list,[{type,1,integer,[]}]} / { list, integer }
%
% - random examples:
%
% - "{integer(),float()}" / "{integer,float}" ->
%        {type,1,tuple, [{type,1,integer,[]},{type,1,float,[]}]} /
%        {tuple,[integer,float]}



% Design notes:

% - a type may be identified solely based on its name and arity
%
% - non-builtin types and atoms must be encoded differently, e.g., respectively,
% [foo()] / {list, {foo,0}} versus [foo] / {list,{atom,foo}}


-doc """
Describes the name of a type (without for polymorphic ones, the names of the
types it depends on,).

For example `my_count`.
""".
-type type_name() :: atom().



-doc """
A number of types a (possibly polymorphic) type depends on (possibly zero for
plain types).
""".
-type type_arity() :: count().



-doc "Analoguous to `function_id/0`.".
-type type_id() :: { type_name(), type_arity() }.



-doc """
A type-as-a-string, i.e. a textual specification of a type, possibly entered
from a user interface; corresponds to the F1 form.

Designed not to be Erlang-specific, for example to be used to specify the
content of any networked messages, yet inspired from the syntax used for the
(Erlang) type specifications
([http://erlang.org/doc/reference_manual/typespec.html]).

For example: `"[{float(), boolean()}]"` or `"foo()|bar|[integer()]"`, where
`foo` is a (monomorphic) type (expected to be defined in the context) and `bar`
is an atom.
""".
-type text_type() :: ustring().



-doc """
A type-as-a-contextual-term, i.e. an Erlang term that defines a type, yet may
still be contextual (i.e. it may depend on other non-builtin types).

This corresponds to the F2 form, and is relatively in the same spirit as the
[Erlang Abstract Format](https://www.erlang.org/doc/apps/erts/absform.html),
albeit this current form only deals with types (not programs, values, etc.).

The same example of the `"foo()|bar|[integer]"` text_type() may then be defined
here as the `{union, [{foo,[]}, {atom,bar}, {list, {integer,[]}}]}` contextual
type (hence a term), where the `foo/0` type is expected to be defined in the
context.
""".
-type contextual_type() :: term().



-doc """
An explicit, fully-resolved type, i.e. a self-standing, Erlang-level term
defining a type (therefore relying only on built-in types and type constructs,
all initial types having been fully resolved into them); corresponds to the F3
form.

For example, supposing that the type `foo()` is an alias for `float()`, and that
the type `buzz()` is specified as `"hello|goodbye"`, a
`"foo()|buzz()|[integer]"` text_type() translates to the following resolved
type: `{union, [float, {union, [{atom,hello}, {atom,goodbye}]},
{list,integer}]}`.
""".
-type resolved_type() :: term().




-doc """
The "most precise" description of an in-memory primitive, in which:
- simple types (e.g. `boolean` and `atom`) coexist (despite overlapping)
- `number` and `bitstring` are not used (as they derive respectively from
`float()|integer()` and `binary()`)

A note about Erlang floats: they are actually IEEE 754 double-precision
floating-point numbers, a format that occupies 8 bytes (64 bits) per float in
memory.

More precisely, as one can see in `erts/emulator/beam/erl_term.h`, a float_def
is an union able to contain a ieee754_8 datatype, aliased to the `double` C
datatype.

Polymorphic types (e.g. lists) are described with no mention of the types they
may depend on (e.g. `list` can be specified, not `list(float())` or anything
like that).

The description of any given type is based on `primitive_type_description/0`,
and can be done in two complementary forms: the textual one, and the internal
one, which are relatively different.
""".
-type primitive_type_description() ::
    'atom'
  | 'binary'
  | 'boolean'
  | 'float'
  | 'integer'
  | 'pid'
  | 'port'
  | 'reference'.


-doc """
For primitive types, the specification is the same of the description.
""".
-type primitive_type_spec() :: primitive_type_description().



-doc """
Describes a compounding type, i.e. a type possibly involving/aggregating other
types.

They can be nested (i.e. they can operate on primitive and/or compounding
types).
""".
-type compounding_type_description() ::
    % Some day maybe: 'function', 'record'
      'list'
    | 'table' % (preferred to 'map')
    | 'tuple'
    | 'union'.



-doc """
Specification of a compounding type.
""".
-type compounding_type_spec() ::
    { 'list', type() }
  | { 'table', type(), type() }
  | { 'tuple', [ type() ] }
  | { 'union', [ type() ] }.





-doc """
The "most precise" description of a value.

Choices overlap intentionally (e.g. `integer` and `pos_integer`), to express
finer types; as a result, the main purpose of this type is to tell whether a
given value matches one of these types (see `is_value_matching/2`).

See also `get_ast_simple_builtin_types/0`.
""".
-type value_description() ::
    'atom'
  | 'binary'
  | 'bitstring'
  | 'boolean'
  | 'float'
  | 'function'
  | 'atom_or_function'

  | 'byte'
  | 'char'

  | 'string' % Plain one
  | 'nonempty_string'

  | 'string_like' % Plain, binary strings, atoms, lists of them, etc.: iolist/0
                  % or unicode:chardata/0

  | 'integer'
  | 'pos_integer'
  | 'neg_integer'
  | 'non_neg_integer'

  | 'number'

  | 'list'
  | 'table' % Could have been 'map'
  | 'pid'
  | 'port'
  | 'record'
  | 'reference'
  | 'tuple'
  | 'term'.



-doc """
Textual type description: type-as-a-string, inspired from the syntax used for
type specifications ([http://erlang.org/doc/reference_manual/typespec.html]),
yet different. Notably, monomorphic types do not end with empty parentheses
(e.g. `"integer"`, not `"integer()"`) and atoms are always surrounded by simple
quotes (e.g. `"'an_atom'|'another_one'"`).

For example: `"[{float, boolean}]"`.
""".
-type type_description() :: ustring().



-doc """
Description of a nesting depth reached when parsing a type description.

It is in pratice a `{P,B}` pair, where `P` is the parenthesis depth (that is the
number of the parentheses that have been opened and not closed yet) and `B` is
the bracket depth (i.e. the same principle, for `"[]"` instead of for `"()"`).
""".
-type nesting_depth() :: { count(), count() }.



-doc """
Internal, "formal", actual programmatic description of a type according to our
conventions: type-as-a-term (either contextual or explicit, i.e. F2 or F3),
relying on a translated version of the textual type (which is for example:
`"[{float,boolean}]"`).

This "internal type language of the Myriad layer" is largely inspired from the
forms that can be found in actual ASTs.

Requirements for this term-based description were:

- be able to represent at least any actual (that can be readily instantiated,
hence non-polymorphic) type (like `-type a() :: ...`, not `-type a(T) :: ...`);
should, in the future, polymorphic types have to be *defined* (not merely used),
then (non-empty) parentheses could be introduced

- be able to nevertheless *use* polymorphic types, as they are certainly useful
(e.g. maps/associative tables, lists, etc.); a problem is that, in terms (as
opposed to in the textual counterpart), parentheses cannot be used to express
these polymorphic types (not only they denote function calls, but also are not
legit components of a term); therefore the convention chosen here is to specify
types as pairs, the first element being the name of the type, the second one
being the (ordered) list of the types it depends on; then the textual type
`"a(T1, T2)"` is translated to the `{a,[T1,T2]}` type term; most types being
"monomorphic", they are represented as `{my_simple_type,[]}` (which cannot be
abbreviated by only the `my_simple_type` atom, as it would lead to ambiguous
forms)

So, as an example, the type-as-a-term corresponding to `"[{float,boolean}]"` is:
`{list, [{tuple, [{float,[]}, {boolean,[]} ]}]}`;

Note that an alternate type language (sticking more closely to its textual
counterpart) could have been a more direct `[{float,boolean}]` term (hence
getting rid of the parentheses and the pair with an empty list in second
position); reason for not doing so: then no possible support of the polymorphic
types that happen to be often needed.

The origin of this term-as-a-type notation is clearly the standard (Erlang) type
specifications; for example `meta_utils:string_to_form("-type a() ::
[{float(),boolean()}].").` returns following AST form:
```
{attribute,1,type, {a,{type,1,list, [{type,1,tuple,[{type,1,float,[]},
 {type,1,boolean,[]}]}]}
```
As a result, the counterpart to the aforementioned `"[{float(), boolean()}]"`
type string is translated in ASTs as:
```
{type, 1, list, [{type, 1, tuple, [{type, 1, float, []}, {type, 1, boolean,
[]}]}]}
```

Then one can remove:

- the `type` (and `user_type`) atoms (not making then a specific distinction
between the origin of a type); a list of built-in types - names and arities -
is maintained, other types being then user ones

- the line numbers (the `1`s here), not useful in that context, hence stripped

Then we obtain our aforementioned term-as-a-type:
`{list, [{tuple, [{float,[]}, {boolean,[]}]}]}`.

We can therefore describe this way arbitrary types as valid terms.

Next steps:

- define and document the full type language (elementary datatypes - like
boolean, integer, float, atoms - and constructs - like list, table, tuple,
union)

- support it, notably define functions to tell whether a given term is an
instance of a specified type

Experiment with `ast_utils:string_to_form/1` and have fun!

For example `"-type a() :: [foobar()]."` yields:
```
{attribute, 1, type, {a,{type,1, list,
  [{user_type, 1, foobar, []}]}, []}}
```
See also [http://erlang.org/doc/apps/erts/absform.html].

Finally, a direct string representation can be converted into a `type()`; maybe
writing a parser may not mandatory, as `"{float(), atom()}"` may be a string
expression evaluated with functions that we can bind to obtain a closer term,
such as: `float() -> {float, []}`.

Of course, on a related note, if `TextualType = "{list, [{tuple, [float,
boolean]}]}"`, then `ast_utils:string_to_value(TextualType)` will return the
expected: `{list, [{tuple, [{float, []}, {boolean, []}]}]}`.

Note that such a type may not be fully explicit, as it may contain unresolved
references to other types; for example: `{list, [{count, []}]}` does not specify
what the `count()` type is.

The fully explicit types (F3) can be obtained by composing the primitive types
(see `primitive_type_description/0`) with the compounding, polymorphic
construction types (see `compounding_type_description/0`).

It is ultimately a recursive type.
""".
%-type type() :: term().
-type type() ::
    tuploid( primitive_type_spec() | compounding_type_spec() ).



-doc """
An explicit type is a type that has been fully resolved in terms of built-in
constructs; it is thus autonomous, self-standing.
""".
-type explicit_type() :: type().




-doc """
To tell that a returned value is not of interest to the caller.

Could have been: `-type void() :: 'myriad_void'` for example.

Nevertheless, should, for any reason, a value of the `void/0` type have to be
specified, the `void` atom shall be preferred, knowing that any value can be
returned and complies with this type.
""".
-type void() :: any() | 'void'.
% Opaque types currently not always well managed by the Erlang standard
% toolchain:
%
% -opaque void() :: any() | 'void'.




-doc """
Designates lower-level types, with a prefix and a size, in bits.

Following prefixes are defined:
- u for unsigned
- s for signed

Datatypes are `int` (for integer) and `float` (for standard IEEE signed,
floating-point values).
""".
-type low_level_type() :: 'uint8'  | 'sint8'
						| 'uint16' | 'sint16'
						| 'uint32' | 'sint32'
						| 'uint64' | 'sint64'
						| 'float32'
						| 'float64'.


-doc """
Denotes an optional value, that is a value that may be set to one in T, or that
may not be set at all.

Note that the type T should not include the `undefined` atom, otherwise one
cannot discriminate between a value that happens to be set to `undefined` versus
a value not defined at all.

Quite often, variables (e.g. record fields) are set to `undefined` before being
set later.
""".
-type option( T ) :: T | 'undefined'.


-doc """
Denotes a value that may be set to one of type T (with no restriction on T -
unlike `option/1` where T should not include the `undefined` value), or that may
not be set at all.

A bit safer and more expensive than `option/1`.

Obviously a nod to Haskell.
""".
-type safe_option( T ) :: { 'just', T } | 'nothing'.


-doc "To account for wildcard entries.".
-type wildcardable( T ) :: T | 'any'.



% Definition of actual datatypes (useful for typing variables):

-doc "Describes a mask of bits.".
-type bit_mask() :: integer().


% Unsigned integers:


-doc "Non-negative 8-bit integer, ranging from 0 to 255 (both included).".
-type uint8() :: 0..255.


-doc "Non-negative 16-bit integer, ranging from 0 to 65,535 (both included).".
-type uint16() :: 0..65535.


-doc """
Non-negative 32-bit integer, ranging from 0 to 4,294,967,295 (both included).
""".
-type uint32() :: 0..4294967295.


-doc """
Non-negative 64-bit integer, ranging from 0 to 18,446,744,073,709,551,615 (both
included).
""".
-type uint64() :: 0..18446744073709551615.



% Signed integers:


-doc "Signed 8-bit integer, ranging from -128 to 127 (both included).".
-type sint8() :: -128..127.


-doc "Signed 16-bit integer, ranging from -32,768 to 32,767 (both included).".
-type sint16() :: -32768..32767.


-doc """
Signed 32-bit integer, ranging from -2,147,483,648 to 2,147,483,647 (both
included).
""".
-type sint32() :: -2147483648..2147483647.


-doc """
Signed 64-bit integer, ranging from -9,223,372,036,854,775,808 to
9,223,372,036,854,775,807 (both included).
""".
-type sint64() :: -9223372036854775808..9223372036854775807.



% Maybe-signed, ambiguous integers:

-doc "8-bit integer, potentially ranging from -128 to 255 (both included).".
-type int8() :: uint8() | sint8().


-doc """
16-bit integer, potentially ranging from -32,768 to 65,535 (both included).
""".
-type int16() :: uint16() | sint16().


-doc """
32-bit integer, potentially ranging from -2,147,483,648 to 4,294,967,295 (both
included).
""".
-type int32() :: uint32() | sint32().


-doc """
64-bit integer, potentially ranging from -9,223,372,036,854,775,808 to
18,446,744,073,709,551,615 (both included).
""".
-type int64() :: uint64() | sint64().


-doc """
A single-precision, IEEE 754 32-bit base-2 floating-point value.

Exact for all integers with up to 7 decimal digits, and for any `2^N` for an
integer number N in `[-149,127]`.

See [https://en.wikipedia.org/wiki/Single-precision_floating-point_format] for
more information.

Note that in Erlang this datatype does not exist as such, only floars are
`float64()` ones.
""".
-type float32() :: float().


-doc """
A binary64/double-precision, IEEE 754 64-bit base-2 floating-point value.

Exact notably for integers from -2^253 to 2^253 (-9,007,199,254,740,992 to
9,007,199,254,740,992).

See <https://en.wikipedia.org/wiki/Double-precision_floating-point_format> for
more information.
""".
-type float64() :: float().


-doc "A float that is strictly positive.".
-type positive_float() :: float().


-doc "A float that is positive or null.".
-type non_negative_float() :: float().



-doc """
Designates a record instance, to discriminate from a mere tuple.
""".
-type record() :: tuple().


-doc """
The first (atom) element of the tuple corresponding to a record instance.

For example `#my_point{x=1, y=2}` is actually `{my_point, 1, 2}` and thus
`my_point` is the corresponding record tag.
""".
-type record_tag() :: atom().



% Tuploids. See also augment_tuploid/2.


-doc """
We name tuploid a pseudo-tuple, that is a value that is either an actual tuple
or a single, standalone term, designated as a "basic tuploid".

That is, a tuploid is a tuple of any size, except that the tuploid of size 1 is
`MyTerm`, not the rather useless `{MyTerm}`. Note that if `MyTerm` can itself be
a tuple, this is bound to lead to ambiguities.
""".
-type tuploid() :: tuploid( term() ).


-doc "Probably that such a tuple would contain at least an element of type T.".
-type tuploid( T ) :: tuple( T ) | T.


-doc "Local alias.".
-type pair() :: pair:pair().


-doc "Any triplet.".
-type triplet() :: { any(), any(), any() }.


-doc """
Designates an uniform tuple (of unspecified size), that is a tuple whose
elements are all of the specified type T.
""".
-type tuple( _T ) :: tuple().


-doc """
Designates an uniform tuple of the specified size, that is a tuple whose S
elements are all of the type T.
""".
-type tuple( _T, _S ) :: tuple().


-doc "A tuple containing counters.".
-type counters() :: tuple( count() ).


% Not needed or useful, as map/0 is a built-in type:
%-type map() :: map( any(), any() ).


-doc """
As `(maps:)map/2 `does not even exist apparently, at least not since 18.0.
""".
-type map( _K, _V ) :: map().


-doc "Any kind of reference onto a process.".
-type pid_ref() :: naming_utils:local_designator().



-doc """
Designates values that are permanent, meaning that are context-free, not
runtime-specific and can be reproduced (e.g. serialised); for example PIDs are
transient terms, not permanent ones.

As for compound datatypes (lists, tuples and thus records, maps), they are also
permanent iff all the terms that they aggregate are themselves permanent terms.

Permanent terms are the opposite of transient ones.
""".
-type permanent_term() :: integer() | float() | atom() | boolean() | binary()
	| list( permanent_term() ) | tuple( permanent_term() )
	% (maps:)map/2 does not exist apparently:
	%| map( permanent_term(), permanent_term() )
	| map().



-doc """
Designates values that are transient, that is that are runtime-specific and
cannot be reproduced a priori.

So for example PIDs are transient terms, whereas integers are permanent terms.

As for compound datatypes (lists, tuples and thus records, maps), they are also
transient iff at least one of the terms that they aggregate is itself a
transient term.

Transient terms are the opposite of permanent ones.
""".
-type transient_term() :: pid() | port() | reference() | fun().



-export_type([ type_name/0, type_arity/0, type_id/0,
               text_type/0, contextual_type/0, resolved_type/0,
			   primitive_type_description/0, primitive_type_spec/0,
               compounding_type_description/0, compounding_type_spec/0,
			   type_description/0, nesting_depth/0, type/0, explicit_type/0,
			   void/0, low_level_type/0,

			   option/1, safe_option/1, wildcardable/1,

			   bit_mask/0,

			   uint8/0, uint16/0, uint32/0, uint64/0,
			   sint8/0, sint16/0, sint32/0, sint64/0,
			   int8/0, int16/0, int32/0, int64/0,
			   float32/0, float64/0,

			   positive_float/0, non_negative_float/0,

			   record/0,
			   tuploid/0, tuploid/1,
			   pair/0, triplet/0, tuple/1, tuple/2,
			   map/2,
			   pid_ref/0,
			   permanent_term/0, transient_term/0 ]).


% Note: currently, only a very basic, ad hoc type support ("hand-made look-up
% tables") is provided.
%
% Later we would like to really parse any type description (e.g.
% "[{float,[boolean]}]") and be able to manage it as type() (including the
% checking of terms against types).



% Type-related functions:
-export([ description_to_type/1, type_to_description/1, type_to_string/1,
		  get_type_of/1, interpret_type_of/1, interpret_type_of/2,
		  get_immediate_types/0, get_ast_simple_builtin_types/0,
		  get_elementary_types/0, get_simple_builtin_types/0,
		  is_type/1, is_of_type/2,
		  is_of_described_type/2, is_homogeneous/1, is_homogeneous/2,
		  are_types_identical/2,
		  is_value_matching/2,
		  get_low_level_type_size/1,
		  is_transient/1, is_byte/1,
		  is_non_neg_integer/1, is_pos_integer/1, is_neg_integer/1,
          coerce_stringified_to_type/2 ]).



% We distinguish here check_* functions, that check that their argument is of a
% given type (and return that argument, unchanged) and ensure_* functions, which
% perform a check and, if necessary, a (legit) cast, hence may return a value
% whose type is different from the one of their argument.


% Term-returning pure checkings: if the specified term could be successfully
% checked, returns it (as opposed to a predicate returning a boolean value), so
% that it can be chained, i.e. passed as an argument to a function.
%
% We prefer the "'positive' vs 'strictly positive'" naming, deemed clearer than
% the "'non_neg' vs 'positive'" one, this 'positive' excluding zero.
%
% So, at least here, 'positive' includes zero (shall be understood as 'positive
% or null'), in constrast to 'strictly positive'.
%
% See also, in list_utils, are_*/1, and in basic_utils: check_undefined/1,
% check_all_undefined/1, are_all_defined/1, check_defined/1,
% check_not_undefined/1, check_all_defined/1.
%
% For comparisons (equality checks), refer to {basic,cond}_utils:check_equal/2
% for example, depending on whether such a check should be conditional.
%
-export([ check_atom/1, check_atoms/1,
		  check_boolean/1, check_booleans/1,

		  check_pid/1, check_maybe_pid/1, check_pids/1,

		  check_number/1, check_maybe_number/1,
		  check_positive_number/1,
		  check_strictly_positive_number/1,

		  check_numbers/1, check_maybe_numbers/1,

		  check_byte/1,

		  check_integer/1, check_maybe_integer/1,
		  check_positive_integer/1, check_strictly_positive_integer/1,
		  check_maybe_positive_integer/1,

		  check_integers/1, check_maybe_integers/1,

		  check_float/1, check_maybe_float/1,
		  check_floats/1, check_maybe_floats/1,

		  check_positive_float/1,
		  check_maybe_positive_float/1,
		  check_positive_floats/1,

		  check_strictly_positive_float/1,
		  check_maybe_strictly_positive_float/1,

		  check_list/1,
		  check_binary/1, check_binaries/1,
		  check_map/1, check_tuple/1 ]).


% Checks with potential conversions:
-export([ ensure_integer/1,
		  ensure_rounded_integer/1,
		  ensure_floored_integer/1, ensure_ceiled_integer/1,

		  ensure_float/1, ensure_positive_float/1,
		  ensure_string/1, ensure_binary/1 ]).


% Sharing:
-export([ share/1, share/2, share/3 ]).


% Boolean predicates, that is checking that returns not the checked value but
% whether it could be successfully be checked.
%
% Most of such boolean predicates can be directly implemented thanks to guard
% expressions (e.g. 'is_pid(T)' or 'X >= 1'), and thus are not specifically
% defined here.
%
% These predicate are more convenient to trigger application-specific feedback
% that the next term-returning checkings of the next section.
%
-export([ are_numbers/1, are_maybe_numbers/1,
		  are_integers/1, are_maybe_integers/1,
		  are_floats/1, are_maybe_floats/1,
		  are_positive_floats/1,
		  are_binaries/1 ]).



% Counter section:
-export([ initialise_counters/1, initialise_counters/2,
		  increment_counter/2, add_to_counter/3 ]).


% Specials for datatypes:
-export([ get_record_tag/1,
          get_last_tuple_element/1, set_last_tuple_element/2,
          augment_tuploid/2,
		  array_to_string/1 ]).


% Work in progress:
-export([ tokenise_per_union/1 ]).


% Type shorthands:

-type count() :: basic_utils:count().
-type level() :: basic_utils:level().
-type positive_index() :: basic_utils:positive_index().

-type array() :: array:array().

-type ustring() :: text_utils:ustring().
-type any_string() :: text_utils:any_string().


-type byte_size() :: system_utils:byte_size().



-doc """
Returns the actual type corresponding to the specified type description: parses
the specified string to determine the type described therein.

Note: returns a correct type, but currently rarely the expected, most precise
one.
""".
-spec description_to_type( type_description() ) -> type().
description_to_type( TypeDescription ) ->
	CanonicalDesc = text_utils:remove_whitespaces( TypeDescription ),
	%io:format( "CanonicalDesc = '~ts'~n", [ CanonicalDesc ] ),
	scan_type( CanonicalDesc ).



-doc """
Scans the specified type description.

To perform its parsing, we must split the full description recursively.

The worst (and thus first) top-level construct to detect is the union. We
consider that we are always in an union (possibly including only one term, in
which case it can be simplified out.

We do that by scanning for terms from left-to-right, keeping track of the
nesting.
""".
-spec scan_type( type_description() ) -> type().
%scan_type( TypeDescription ) ->
	%case tokenise_per_union( TypeDescription ) of

	%   [ T ] ->
	%       T;

	%   UnionisedTypes ->
	%       { union, [ scan_type( T ) || T <- UnionisedTypes ] }

	%end;
% Last: all other types.
scan_type( _TypeDescription ) ->
	% Most imprecise (yet correct) type (commented-out as may hide issues):
	any.

	% Either not yet implemented or plain wrong:
	%throw( { type_interpretation_failed, TypeDescription } ).



-doc """
Splits the specified type description according to union delimiters.
""".
-spec tokenise_per_union( type_description() ) -> [ type_description() ].
tokenise_per_union( TypeDescription ) ->

	% We track the nesting depth and only fetch the top-level union members;
	InitialNestingDepth = { _P=0, _B=0 },
	parse_nesting( TypeDescription, InitialNestingDepth ).


-doc """
Parses the specified type description in order to split it according in nested
sub-expressions that may be recursively parsed.
""".
-spec parse_nesting( type_description(), nesting_depth() ) ->
							[ type_description() ].
parse_nesting( _TypeDescription, _NestingDepth ) ->

	% A goal is to detect atoms delimited with single quotes (which are
	% immediate atom values) from the unquoted ones (which designate types)
	%
	throw( not_implemented_yet ).



-doc """
Returns the type description (in canonical form, notably without whitespaces)
corresponding to specified type.

Note: currently does not return a really relevant type description; basically
meant to be the function reciprocal to `scan_type/1`.
""".
-spec type_to_description( type() ) -> type_description().
% First, simple types, in alphabetical order:
type_to_description( _Type=any ) ->
	"any";

type_to_description( _Type=atom ) ->
	"atom";

type_to_description( _Type=binary ) ->
	"binary";

type_to_description( _Type=boolean ) ->
	"boolean";

type_to_description( _Type=float ) ->
	"float";

type_to_description( _Type=integer ) ->
	"integer";

type_to_description( _Type=none ) ->
	"none";

type_to_description( _Type=string ) ->
	"string";




% Then polymorphic constructs:

% No "list()"-like (with no specific type) supported.

type_to_description( _Type={ list, T } ) ->
	"[" ++ type_to_description( T ) ++ "]";

type_to_description( _Type={ map, [ Tk, Tv ] } ) ->
	"map(" ++ type_to_description( Tk ) ++ "," ++ type_to_description( Tv )
		++ ")";

type_to_description( _Type={ tuple, TypeList } ) when is_list( TypeList ) ->
	TypeString = text_utils:join( _Separator=",",
		[ type_to_description( T ) || T <- TypeList ] ),
	"{" ++ TypeString ++ "}";

type_to_description( _Type={ union, TypeList } ) when is_list( TypeList ) ->
	text_utils:join( _Separator="|",
					 [ type_to_description( T ) || T <- TypeList ] );

type_to_description( Type ) ->

	% Could be misleading (e.g. any() not matching any()):
	%"any".

	%text_utils:format( "~p", [ Type ] ).

	throw( { type_description_failed, Type } ).



-doc "Returns a textual representation of the specified type.".
-spec type_to_string( type() ) -> ustring().
type_to_string( Type ) ->
	type_to_description( Type ).



-doc """
Returns an atom describing, as precisely as possible, the overall type of the
specified primitive term.

Note: limited to primitive types, not compounded ones (like `[float()]`).

`is_number/1`, `is_record/1`, etc. not usable here.

Note: often we do not want to retrieve the actual type of a term but need
instead to determine whether the term can be considered as an instance of a
specific type (this is not strictly the same need, as a given term in general
may be seen of being of multiple types); for that see `is_value_matching/2`.

The lowest-level/most precise typing can be obtained with the (undocumented)
`erts_internal:term_type/1` function.
""".
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
	'table';

get_type_of( Term ) when is_port( Term ) ->
	'port';

%get_type_of( Term ) when is_record( Term ) ->
%   'record';

get_type_of( Term ) when is_tuple( Term ) ->
	'tuple';

get_type_of( Term ) when is_reference( Term ) ->
	'reference';

get_type_of( Term ) ->
	throw( { unknown_type_for, Term } ).



-doc """
Returns a string describing, in a user-friendly manner, the type of the
specified term (up to one level of nesting detailed).
""".
-spec interpret_type_of( term() ) -> ustring().
interpret_type_of( Term ) ->
	interpret_type_helper( Term, _CurrentNestingLevel=0, _MaxNestingLevel=1 ).



-doc """
Returns a string describing, in a user-friendly manner, the type of the
specified term, up to the specified nesting level (either a positive integer or
the 'infinite' atom, to go as deep as possible in the term structure).
""".
-spec interpret_type_of( term(), level() | 'infinite' ) -> ustring().
interpret_type_of( Term, MaxNestingLevel ) when MaxNestingLevel >= 0 ->
	interpret_type_helper( Term, _CurrentNestingLevel=0, MaxNestingLevel ).



-doc """
Returns a string describing, in a user-friendly manner, the type of the
specified term, describing any nested subterms up to the specified level.
""".
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
								CurrentNestingLevel+1, MaxNestingLevel )
									|| E <- Term ],

					text_utils:format( "list of ~B elements: ~ts",
						[ length( Term ),
						  text_utils:strings_to_enumerated_string( Elems,
							CurrentNestingLevel ) ] )

			end

	end;

interpret_type_helper( Term, _CurrentNestingLevel=MaxNestingLevel,
					   MaxNestingLevel ) when is_map( Term ) ->
	text_utils:format( "table of ~B entries", [ maps:size( Term ) ] );

interpret_type_helper( Term, CurrentNestingLevel, MaxNestingLevel )
				when is_map( Term ) ->

	Elems = [ text_utils:format( "key ~ts associated to value ~ts",
				[ interpret_type_helper( K, CurrentNestingLevel+1,
										 MaxNestingLevel ),
				  interpret_type_helper( V, CurrentNestingLevel+1,
										 MaxNestingLevel ) ] )
						|| { K, V } <- maps:to_list( Term ) ],

	text_utils:format( "table of ~B entries: ~ts", [ maps:size( Term ),
		text_utils:strings_to_string( Elems, CurrentNestingLevel ) ] );


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

	Elems = [ interpret_type_helper( E, CurrentNestingLevel+1,
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



-doc "Returns a list of the possible types for immediate values.".
-spec get_immediate_types() -> [ type_name() ].
get_immediate_types() ->
	% Not sure this list is very accurate or relevant:
	[ 'atom', 'float', 'integer', 'binary', 'boolean' ].



-doc """
Returns a list of the possible types for immediate values (typically found in an
AST like, like `undefined` in: `{atom,42,undefined}`).

From [http://erlang.org/doc/apps/erts/absform.html]:

```
There are five kinds of atomic literals, which are represented in the same
way in patterns, expressions, and guards:

- If L is an atom literal, then Rep(L) = {atom,LINE,L}.

- If L is a character literal, then Rep(L) = {char,LINE,L}.

- If L is a float literal, then Rep(L) = {float,LINE,L}.

- If L is an integer literal, then Rep(L) = {integer,LINE,L}.

- If L is a string literal consisting of the characters C_1, ..., C_k, then
Rep(L) = {string,LINE,[C_1, ..., C_k]}."
```

Actually additional types can be found in ASTs.
""".
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



-doc """
Returns a list of the elementary, "atomic" types.
""".
-spec get_elementary_types() -> [ type_name() ].
get_elementary_types() ->
	get_immediate_types() ++
		[ 'function', 'list', 'map', 'pid', 'port', 'record', 'reference',
		  'tuple', 'any' ].



-doc """
Returns a list of the built-in, non-polymorphic types that can be typically
found in AST forms.
""".
-spec get_simple_builtin_types() -> [ type_name() ].
get_simple_builtin_types() ->
	get_immediate_types() ++ [ 'pid', 'port', 'reference', 'any', 'no_return' ].



-doc """
Tells whether the specified term designates a type (i.e. a `type()` instance).
""".
-spec is_type( term() ) -> boolean().
% Certainly to be improved:
is_type( TypeList ) when is_list( TypeList ) ->
    lists:all( _Pred=fun is_type/1, TypeList );

is_type( TypeTuple ) when is_tuple( TypeTuple ) ->
    lists:all( _Pred=fun is_type/1, tuple_to_list( TypeTuple ) );

is_type( ElemType ) ->
    lists:member( ElemType, get_ast_simple_builtin_types() ).



-doc "Tells whether the specified term is of the specified type (predicate).".
-spec is_of_type( term(), type() ) -> boolean().
% First, our own simple types:
is_of_type( _Term, _Type='any' ) ->
	true;

is_of_type( Term, _Type='string' ) when is_list( Term ) ->
	text_utils:is_string( Term );

is_of_type( ListTerm, _Type={ list, ElemType } ) when is_list( ListTerm ) ->
	lists:all( _Pred=fun( E ) -> is_of_type( E, ElemType ) end,
               ListTerm );

is_of_type( TableTerm, _Type={ table, KeyType, ValueType } ) ->
    Entries = table:enumerate( TableTerm ),
    lists:all( _Pred=fun( { K, V } ) ->
        is_of_type( K, KeyType ) andalso is_of_type( V, ValueType )
                     end, Entries );

is_of_type( TupleTerm, _Type={ tuple, _Types } ) when is_tuple( TupleTerm ) ->
    _Elems = tuple_to_list( TupleTerm ),
    throw( fixme );

is_of_type( Term, Type ) ->
	get_type_of( Term ) =:= Type.



-doc """
Tells whether the specified term is of the specified textually-described type.

Note: currently no checking is made and the test always succeeds.
""".
-spec is_of_described_type( term(), type_description() ) -> boolean().
is_of_described_type( _Term, _TypeDescription ) ->

	%throw( { not_implemented_yet, {is_of_described_type,2} } ).

	% ActualType = description_to_type( TypeDescription ),
	% is_of_type( ActualType ).

	true.



-doc """
Tells whether the specified non-empty monomorphic container (list or tuple) is
homogeneous in terms of type, that is whether all its elements are of the same
type.

If true, returns the common type.
If false, returns two of the different types found in the container.
""".
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



-doc """
Tells whether the specified non-empty monomorphic container (list or tuple) is
homogeneous in terms of type, that is whether all its elements are of the same,
specified, primitive type.
""".
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



-doc """
Tells whether the two specified types are the same (i.e. designate the same
actual type / are aliases).
""".
-spec are_types_identical( type(), type() ) -> boolean().
are_types_identical( Type, Type ) ->
	true;

are_types_identical( _FirstType, _SecondType ) ->
	false.


-doc """
Tells whether the specified value matches the specified value description.
""".
-spec is_value_matching( value_description(), term() ) -> boolean().
is_value_matching( _VDesc=atom, Value ) ->
	is_atom( Value );

is_value_matching( _VDesc=binary, Value ) ->
	is_binary( Value );

is_value_matching( _VDesc=bitstring, Value ) ->
	is_bitstring( Value );

is_value_matching( _VDesc=boolean, Value ) ->
	is_boolean( Value );

is_value_matching( _VDesc=float, Value ) ->
	is_float( Value );

is_value_matching( _VDesc=function, Value ) ->
	is_function( Value );

is_value_matching( _VDesc=atom_or_function, Value ) ->
	is_atom( Value) orelse is_function( Value );

is_value_matching( _VDesc=byte, Value ) ->
	is_byte( Value );

is_value_matching( _VDesc=char, Value ) ->
	text_utils:is_char( Value );

is_value_matching( _VDesc=string, Value ) ->
	text_utils:is_string( Value );

is_value_matching( _VDesc=nonempty_string, Value ) ->
	text_utils:is_non_empty_string( Value );

is_value_matching( _VDesc=string_like, Value ) ->
	text_utils:is_string_like( Value );

is_value_matching( _VDesc=integer, Value ) ->
	is_integer( Value );

is_value_matching( _VDesc=pos_integer, Value ) ->
	is_pos_integer( Value );

is_value_matching( _VDesc=neg_integer, Value ) ->
	is_neg_integer( Value );

is_value_matching( _VDesc=non_neg_integer, Value ) ->
	is_non_neg_integer( Value );

is_value_matching( _VDesc=number, Value ) ->
	is_number( Value );

is_value_matching( _VDesc=list, Value ) ->
	is_list( Value );

is_value_matching( _VDesc=map, Value ) ->
	is_map( Value );

is_value_matching( _VDesc=pid, Value ) ->
	is_pid( Value );

is_value_matching( _VDesc=port, Value ) ->
	is_port( Value ) ;

is_value_matching( _VDesc=record, Value ) ->
	% The BIF is is_record/2. Records having at least a tag:
	is_tuple( Value ) andalso is_atom( element( _FirstIndex=1, Value ) );

is_value_matching( _VDesc=reference, Value ) ->
	is_reference( Value );

is_value_matching( _VDesc=tuple, Value ) ->
	is_tuple( Value );

is_value_matching( _VDesc=term, _Value ) ->
	true.



-doc """
Returns the number of bytes used by each value of the specified low-level type.
""".
-spec get_low_level_type_size( low_level_type() ) -> byte_size().
get_low_level_type_size( uint8 ) -> 1;
get_low_level_type_size( sint8 ) -> 1;

get_low_level_type_size( uint16 ) -> 2;
get_low_level_type_size( sint16 ) -> 2;

get_low_level_type_size( uint32 ) -> 4;
get_low_level_type_size( sint32 ) -> 4;

get_low_level_type_size( uint64 ) -> 8;
get_low_level_type_size( sint64 ) -> 8;

get_low_level_type_size( float32 ) -> 4;
get_low_level_type_size( float64 ) -> 8.



-doc "Tells whether the specified term is, just by itself, a transient one.".
-spec is_transient( term() ) -> boolean().
% Maybe is_function/1 could be relevant here:
is_transient( T ) when is_pid( T ); is_port( T ); is_reference( T ) ->
	true;

is_transient( _T ) ->
	false.



-doc "Tells whether the specified term is a byte.".
-spec is_byte( term() ) -> boolean().
is_byte( I ) when is_integer( I ), I >= 0, I =< 255 ->
	true;

is_byte( _Other ) ->
	false.


-doc "Tells whether the specified term is a positive (possibly zero) integer.".
-spec is_non_neg_integer( term() ) -> boolean().
is_non_neg_integer( I ) when is_integer( I ), I >= 0 ->
	true;

is_non_neg_integer( _Other ) ->
	false.


-doc "Tells whether the specified term is a strictly positive integer.".
-spec is_pos_integer( term() ) -> boolean().
is_pos_integer( I ) when is_integer( I ), I > 0 ->
	true;

is_pos_integer( _Other ) ->
	false.


-doc "Tells whether the specified term is a strictly negative integer.".
-spec is_neg_integer( term() ) -> boolean().
is_neg_integer( I ) when is_integer( I ), I < 0 ->
	true;

is_neg_integer( _Other ) ->
	false.



-doc """
Converts the term specified as a string to the actual value that corresponds to
the specified type.

For example: `coerce_stringified_to_type("[4,3]", {list,integer}) = [4,3]`.
""".
-spec coerce_stringified_to_type( any_string(), type() ) -> term().
% To have a plain string in all cases:
coerce_stringified_to_type( BinStr, Type ) when is_binary( BinStr ) ->
    coerce_stringified_to_type( text_utils:binary_to_string( BinStr ),
                                Type );

% Now a plain string;
coerce_stringified_to_type( ValueStr, Type ) ->
    Value = ast_utils:string_to_value( ValueStr ),

    case is_of_type( Value, Type ) of

        true ->
            Value;

        false ->
            throw( { value_not_matching_type, ValueStr, Value, Type } )

    end.



%% first, simple, built-in types:
%% coerce_stringified_to_type( Str, _Type=atom ) when is_list( Str ) ->
%%     text_utils:string_to_atom( Str );

%% coerce_stringified_to_type( Str, _Type=boolean ) when is_list( Str ) ->
%%     Atom = text_utils:string_to_atom( Str ),
%%     case is_boolean( Atom ) of

%%         true ->
%%             Atom;

%%         false ->
%%             throw( { not_boolean, Str } )

%%     end;

% Currently compounding constructs like list, map, tuple, union are not
% supported.





% ensure_* section.
%
% Note: using such functions may be a bad practice, as it may lead to losing the
% awareness of the types of the variables that are handled. We may even decide
% in the future to output warning traces whenever the specified element happens
% not to be of the target type.



-doc """
Ensures that the specified term is an integer, and returns it.

If it is a float, will return a truncated (integer) version of it.
""".
-spec ensure_integer( number() ) -> integer().
ensure_integer( N ) when is_integer( N ) ->
	N;

ensure_integer( N ) when is_float( N ) ->
	trunc( N );

ensure_integer( N ) ->
	throw( { cannot_coerce_to_integer, N } ).



-doc """
Ensures that the specified term is an integer, and returns it.

If it is a float, will return a rounded (integer) version of it.
""".
-spec ensure_rounded_integer( number() ) -> integer().
ensure_rounded_integer( N ) when is_integer( N ) ->
	N;

ensure_rounded_integer( N ) when is_float( N ) ->
	round( N );

ensure_rounded_integer( N ) ->
	throw( { cannot_coerce_to_integer, N } ).



-doc """
Ensures that the specified term is an integer, and returns it.

If it is a float, will return a floored (rounded-down integer) version of it.
""".
-spec ensure_floored_integer( number() ) -> integer().
ensure_floored_integer( N ) when is_integer( N ) ->
	N;

ensure_floored_integer( N ) when is_float( N ) ->
	math_utils:floor( N );

ensure_floored_integer( N ) ->
	throw( { cannot_coerce_to_integer, N } ).



-doc """
Ensures that the specified term is an integer, and returns it.

If it is a float, will return a ceiled (rounded-up integer) version of it.
""".
-spec ensure_ceiled_integer( number() ) -> integer().
ensure_ceiled_integer( N ) when is_integer( N ) ->
	N;

ensure_ceiled_integer( N ) when is_float( N ) ->
	math_utils:ceiling( N );

ensure_ceiled_integer( N ) ->
	throw( { cannot_coerce_to_integer, N } ).



-doc """
Ensures that the specified term is a float, and returns it.

If it is an integer, will return a floating-point version of it.

Yet `float/1` mostly suffices (as it can can operate on floats).
""".
-spec ensure_float( number() ) -> float().
ensure_float( N ) when is_float( N ) ->
	N;

ensure_float( N ) when is_integer( N ) ->
	float( N );

ensure_float( N ) ->
	throw( { cannot_coerce_to_float, N } ).



-doc """
Ensures that the specified term is a positive (possibly null) float, and returns
it.

If it is an integer, will return a floating-point version of it.
""".
-spec ensure_positive_float( number() ) -> float().
ensure_positive_float( F ) when is_float( F ), F >= 0.0 ->
	F;

ensure_positive_float( I ) when is_integer( I ), I >= 0 ->
	float( I );

ensure_positive_float( Other ) ->
	throw( { cannot_coerce_to_positive_float, Other } ).



-doc "Ensures that the specified term is a string, and returns it.".
-spec ensure_string( term() ) -> ustring().
ensure_string( S ) ->
	text_utils:ensure_string( S ).



-doc "Ensures that the specified term is a binary string, and returns it.".
-spec ensure_binary( term() ) -> ustring().
ensure_binary( S ) ->
	text_utils:ensure_binary( S ).



% Sub-term sharing section.


-doc """
Improves the compactness of the specified term ("compresses it"): returns it
once the sharing of its sub-terms has been favored (equal term, but possibly
smaller in memory).

Presumably useful on platforms where the size of a pointer is lower than the one
of the subterms (e.g. an Erlang float, i.e. a double), when some subterms may be
equal (by value), yet are duplicated (not defined once and pointed to multiple
times).

Such a deduplication may happen typically on homogeneous tuples (e.g. vectors),
when received as messages for example, or when read from any external source
(e.g. from file).

Directly inspired from `wings_utils:share/*`.
""".
-spec share( term() ) -> term().
% Pair:
share( {X,X} ) ->
	{X,X};

% Triplet:
share( {X,X,X} ) ->
	{X,X,X};

share( {X,X,Z} ) ->
	{X,X,Z};

share( {X,Y,Y} ) ->
	{X,Y,Y};

share( {X,Y,X} ) ->
	{X,Y,X};

% Quadruplet:
share( {X,X,X,X} ) ->
	{X,X,X,X};

% (3 equal)
share( {X,X,X,A} ) ->
	{X,X,X,A};

share( {X,X,Z,X} ) ->
	{X,X,Z,X};

share( {X,Y,X,X} ) ->
	{X,Y,X,X};

share( {X,Y,Y,Y} ) ->
	{X,Y,Y,Y};


% (2x2 equal)
share( {X,X,Y,Y} ) ->
	{X,X,Y,Y};

share( {X,Y,X,Y} ) ->
	{X,Y,X,Y};

share( {X,Y,Y,X} ) ->
	{X,Y,Y,X};

% (3 equal)
share( {X,X,Z,A} ) ->
	{X,X,Z,A};

share( {X,Y,X,A} ) ->
	{X,Y,X,A};

share( {X,Y,Z,X} ) ->
	{X,Y,Z,X};

share( {X,Y,Y,A} ) ->
	{X,Y,Y,A};

share( {X,Y,Z,Y} ) ->
	{X,Y,Z,Y};

share( {X,Y,Z,Z} ) ->
	{X,Y,Z,Z};

% Untouched:
share( Other ) ->
	Other.



-doc """
Returns the two specified terms as a pair favoring the in-memory sharing of its
elements, if they are equal.

Presumably useful on platforms where the size of a pointer is lower than the one
of the subterms (e.g. an Erlang float, i.e. a double), when some subterms may be
equal (by value), yet are duplicated (not defined once and pointed to multiple
times).

Directly inspired from `wings_utils:share/*`.
""".
-spec share( term(), term() ) -> pair().
share( X, X ) ->
	{X,X};

share( X, Y ) ->
	{X,Y}.



-doc """
Returns the three specified terms as a triplet favoring the in-memory sharing of
its elements, if they are equal.

Presumably useful on platforms where the size of a pointer is lower than the one
of the subterms (e.g. an Erlang float, i.e. a double), when some subterms may be
equal (by value), yet are duplicated (not defined once and pointed to multiple
times).

Directly inspired from `wings_utils:share/*`.
""".
-spec share( term(), term(), term() ) -> triplet().
share( X, X, X ) ->
	{X,X,X};

share( X, X ,Z ) ->
	{X,X,Z};

share( X, Y, Y ) ->
	{X,Y,Y};

share( X, Y, X ) ->
	{X,Y,X};

share( X, Y, Z ) ->
	{ X, Y, Z }.



% Boolean predicates:
%
% (empty lists are considered as legit)


-doc "Returns whether the specified term is a list of numbers.".
-spec are_numbers( term() ) -> boolean().
are_numbers( [] )  ->
	true;

are_numbers( [ N | T ] ) when is_number( N ) ->
	are_numbers( T );

are_numbers( _Other ) ->
	false.



-doc "Returns whether the specified term is a list of maybe-numbers.".
-spec are_maybe_numbers( term() ) -> boolean().
are_maybe_numbers( [] )  ->
	true;

are_maybe_numbers( [ MN | T ] ) when is_number( MN ); MN =:= undefined ->
	are_maybe_numbers( T );

are_maybe_numbers( _Other ) ->
	false.



-doc "Returns whether the specified term is a list of integers.".
-spec are_integers( term() ) -> boolean().
are_integers( [] )  ->
	true;

are_integers( [ I | T ] ) when is_integer( I ) ->
	are_integers( T );

are_integers( _Other ) ->
	false.



-doc "Returns whether the specified term is a list of maybe-integers.".
-spec are_maybe_integers( term() ) -> boolean().
are_maybe_integers( [] )  ->
	true;

are_maybe_integers( [ MI | T ] ) when is_integer( MI ); MI =:= undefined ->
	are_maybe_integers( T );

are_maybe_integers( _Other ) ->
	false.




-doc "Returns whether the specified term is a list of floats.".
-spec are_floats( term() ) -> boolean().
are_floats( [] )  ->
	true;

are_floats( [ F | T ] ) when is_float( F ) ->
	are_floats( T );

are_floats( _Other ) ->
	false.



-doc "Returns whether the specified term is a list of maybe-floats.".
-spec are_maybe_floats( term() ) -> boolean().
are_maybe_floats( [] )  ->
	true;

are_maybe_floats( [ MF | T ] ) when is_float( MF ); MF =:= undefined ->
	are_maybe_floats( T );

are_maybe_floats( _Other ) ->
	false.



-doc "Returns whether the specified term is a list of positive floats.".
-spec are_positive_floats( term() ) -> boolean().
are_positive_floats( [] )  ->
	true;

are_positive_floats( [ PF | T ] ) when is_float( PF ), PF >= 0.0 ->
	are_positive_floats( T );

are_positive_floats( _Other ) ->
	false.



-doc "Returns whether the specified term is a list of binaries.".
-spec are_binaries( term() ) -> boolean().
are_binaries( [] )  ->
	true;

are_binaries( [ B | T ] ) when is_binary( B ) ->
	are_binaries( T );

are_binaries( _Other ) ->
	false.



% Term-returning checkings:


-doc "Checks that the specified term is an atom indeed, and returns it.".
-spec check_atom( term() ) -> atom().
check_atom( Atom ) when is_atom( Atom ) ->
	Atom;

check_atom( Other ) ->
	throw( { not_atom, Other } ).



-doc """
Checks that the specified term is a list of atoms indeed, and returns it.
""".
-spec check_atoms( term() ) -> [ atom() ].
check_atoms( Atoms ) ->
	[ check_atom( T ) || T <- Atoms ],
	Atoms.



-doc "Checks that the specified term is a boolean indeed, and returns it.".
-spec check_boolean( term() ) -> atom().
check_boolean( true ) ->
	true;

check_boolean( false ) ->
	false;

check_boolean( Other ) ->
	throw( { not_boolean, Other } ).



-doc """
Checks that the specified term is a list of booleans indeed, and returns it.
""".
-spec check_booleans( term() ) -> [ boolean() ].
check_booleans( Booleans ) ->
	[ check_boolean( T ) || T <- Booleans ],
	Booleans.



-doc "Checks that the specified term is a PID indeed, and returns it.".
-spec check_pid( term() ) -> pid().
check_pid( Pid ) when is_pid( Pid ) ->
	Pid;

check_pid( Other ) ->
	throw( { not_pid, Other } ).



-doc """
Checks that the specified term is a PID indeed or the `undefined` atom, and
returns it.
""".
-spec check_maybe_pid( term() ) -> option( pid() ).
check_maybe_pid( Pid ) when is_pid( Pid ) ->
	Pid;

check_maybe_pid( undefined ) ->
	undefined;

check_maybe_pid( Other ) ->
	throw( { not_maybe_pid, Other } ).



-doc """
Checks that the specified term is a list of PIDs indeed, and returns it.
""".
-spec check_pids( term() ) -> [ pid() ].
check_pids( Pids )  ->
	% Possibly a bit quicker that way:
	[ check_pid( P ) || P <- Pids ],
	Pids.



-doc "Checks that the specified term is a number indeed, and returns it.".
-spec check_number( term() ) -> number().
check_number( Num ) when is_number( Num ) ->
	Num;

check_number( Other ) ->
	throw( { not_number, Other } ).



-doc """
Checks that the specified term is a maybe-number indeed, and returns it.
""".
-spec check_maybe_number( term() ) -> option( number() ).
check_maybe_number( undefined ) ->
	undefined;

check_maybe_number( Num ) when is_number( Num ) ->
	Num;

check_maybe_number( Other ) ->
	throw( { not_maybe_number, Other } ).



-doc """
Checks that the specified term is a positive or null number indeed, and returns
it.
""".
-spec check_positive_number( term() ) -> number().
check_positive_number( Num ) when is_number( Num ), Num >= 0 ->
	Num;

check_positive_number( Other ) ->
	throw( { not_positive_number, Other } ).



-doc """
Checks that the specified term is a strictly positive number indeed, and returns
it.
""".
-spec check_strictly_positive_number( term() ) -> number().
check_strictly_positive_number( Num ) when is_number( Num ), Num > 0 ->
	Num;

check_strictly_positive_number( Other ) ->
	throw( { not_strictly_positive_number, Other } ).



-doc """
Checks that the specified term is a list of numbers indeed, and returns it.
""".
-spec check_numbers( term() ) -> [ number() ].
check_numbers( Numbers ) ->
	% Possibly a bit quicker that way:
	[ check_number( N ) || N <- Numbers ],
	Numbers.



-doc """
Checks that the specified term is a list of maybe-numbers indeed, and returns
it.
""".
-spec check_maybe_numbers( term() ) -> [ option( number() ) ].
check_maybe_numbers( MaybeNumbers ) ->
	% Possibly a bit quicker that way:
	[ check_maybe_number( MN ) || MN <- MaybeNumbers ],
	MaybeNumbers.



-doc "Checks that the specified term is a byte indeed, and returns it.".
-spec check_byte( term() ) -> integer().
check_byte( Int ) when is_integer( Int ), Int >= 0, Int =< 255 ->
	Int;

check_byte( Other ) ->
	throw( { not_byte, Other } ).



-doc "Checks that the specified term is an integer indeed, and returns it.".
-spec check_integer( term() ) -> integer().
check_integer( Int ) when is_integer( Int ) ->
	Int;

check_integer( Other ) ->
	throw( { not_integer, Other } ).



-doc """
Checks that the specified term is a maybe-integer indeed, and returns it.
""".
-spec check_maybe_integer( term() ) -> option( integer() ).
check_maybe_integer( undefined ) ->
	undefined;

check_maybe_integer( Int ) when is_integer( Int ) ->
	Int;

check_maybe_integer( Other ) ->
	throw( { not_maybe_integer, Other } ).



-doc """
Checks that the specified term is a positive or null integer, and returns it.
""".
-spec check_positive_integer( term() ) -> pos_integer().
check_positive_integer( Int ) when is_integer( Int ), ( Int >= 0 ) ->
	Int;

check_positive_integer( Other ) ->
	throw( { not_positive_integer, Other } ).



-doc """
Checks that the specified term is a strictly positive integer, and returns it.
""".
-spec check_strictly_positive_integer( term() ) -> pos_integer().
check_strictly_positive_integer( Int ) when is_integer( Int ), Int > 0 ->
	Int;

check_strictly_positive_integer( Other ) ->
	throw( { not_strictly_positive_integer, Other } ).



-doc """
Checks that the specified term is a positive or null integer or the `undefined`
atom, and returns it.
""".
-spec check_maybe_positive_integer( term() ) -> option( pos_integer() ).
check_maybe_positive_integer( Int ) when is_integer( Int ), Int >= 0 ->
	Int;

check_maybe_positive_integer( undefined ) ->
	undefined;

check_maybe_positive_integer( Other ) ->
	throw( { not_maybe_positive_integer, Other } ).



-doc """
Checks that the specified term is a list of integers indeed, and returns it.
""".
-spec check_integers( term() ) -> [ integer() ].
check_integers( Integers ) ->
	% Possibly a bit quicker that way:
	[ check_integer( I ) || I <- Integers ],
	Integers.



-doc """
Checks that the specified term is a list of maybe-integers indeed, and returns
it.
""".
-spec check_maybe_integers( term() ) -> [ option( integer() ) ].
check_maybe_integers( MaybeIntegers ) ->
	% Possibly a bit quicker that way:
	[ check_maybe_integer( MI ) || MI <- MaybeIntegers ],
	MaybeIntegers.



-doc "Checks that the specified term is a float indeed, and returns it.".
-spec check_float( term() ) -> float().
check_float( Float ) when is_float( Float ) ->
	Float;

check_float( Other ) ->
	throw( { not_float, Other } ).



-doc """
Checks that the specified term is a maybe-float indeed, and returns it.
""".
-spec check_maybe_float( term() ) -> option( float() ).
check_maybe_float( undefined ) ->
	undefined;

check_maybe_float( F ) when is_float( F ) ->
	F;

check_maybe_float( Other ) ->
	throw( { not_maybe_float, Other } ).



-doc """
Checks that the specified term is a list of floats indeed, and returns it.
""".
-spec check_floats( term() ) -> [ float() ].
check_floats( Floats ) ->
	% Possibly a bit quicker that way:
	[ check_float( F ) || F <- Floats ],
	Floats.



-doc """
Checks that the specified term is a list of maybe-floats indeed, and returns it.
""".
-spec check_maybe_floats( term() ) -> [ option( float() ) ].
check_maybe_floats( MaybeFloats ) ->
	% Possibly a bit quicker that way:
	[ check_maybe_float( MF ) || MF <- MaybeFloats ],
	MaybeFloats.



-doc """
Checks that the specified term is a positive (or null) float indeed, and returns
it.
""".
-spec check_positive_float( term() ) -> float().
check_positive_float( Float )
			when is_float( Float ), Float >= 0.0 ->
	Float;

check_positive_float( Other ) ->
	throw( { not_positive_float, Other } ).



-doc """
Checks that the specified term is a maybe-(positive float) indeed, and returns
it.
""".
-spec check_maybe_positive_float( term() ) -> option( float() ).
check_maybe_positive_float( undefined ) ->
	undefined;

check_maybe_positive_float( Float ) when is_float( Float ), Float >= 0.0 ->
	Float;

check_maybe_positive_float( Other ) ->
	throw( { not_maybe_positive_float, Other } ).



-doc """
Checks that the specified term is a list of positive (or null) floats indeed,
and returns it.
""".
-spec check_positive_floats( term() ) -> [ float() ].
check_positive_floats( Floats ) ->
	% Possibly a bit quicker that way:
	[ ( is_float( F ) andalso F >= 0.0 ) orelse
		throw( { not_positive_float, F } ) || F <- Floats ],
	Floats.



-doc """
Checks that the specified term is a strictly positive float indeed, and returns
it.
""".
-spec check_strictly_positive_float( term() ) -> float().
check_strictly_positive_float( Float )
                                when is_float( Float ) andalso Float > 0.0 ->
	Float;

check_strictly_positive_float( Other ) ->
	throw( { not_strictly_positive_float, Other } ).



-doc """
Checks that the specified term is a maybe-(strictly positive float) indeed, and
returns it.
""".
-spec check_maybe_strictly_positive_float( term() ) -> option( float() ).
check_maybe_strictly_positive_float( undefined ) ->
	undefined;

check_maybe_strictly_positive_float( Float )
                                when is_float( Float ), Float > 0.0 ->
	Float;

check_maybe_strictly_positive_float( Other ) ->
	throw( { not_maybe_strictly_positive_float, Other } ).



-doc "Checks that the specified term is a list indeed, and returns it.".
-spec check_list( term() ) -> list().
check_list( List ) when is_list( List ) ->
	List;

check_list( Other ) ->
	throw( { not_list, Other } ).



-doc "Checks that the specified term is a binary indeed, and returns it.".
-spec check_binary( term() ) -> binary().
check_binary( Binary ) when is_binary( Binary ) ->
	Binary;

check_binary( Other ) ->
	throw( { not_binary, Other } ).



-doc """
Checks that the specified term is a list of binaries indeed, and returns it.
""".
-spec check_binaries( term() ) -> [ binary() ].
check_binaries( Binaries ) ->
	% Possibly a bit quicker that way:
	[ check_binary( B ) || B <- Binaries ],
	Binaries.



-doc "Checks that the specified term is a map indeed, and returns it.".
-spec check_map( term() ) -> map().
check_map( Map ) when is_map( Map ) ->
	Map;

check_map( Other ) ->
	throw( { not_map, Other } ).



-doc "Checks that the specified term is a tuple indeed, and returns it.".
-spec check_tuple( term() ) -> tuple().
check_tuple( Tuple ) when is_tuple( Tuple ) ->
	Tuple;

check_tuple( Other ) ->
	throw( { not_tuple, Other } ).



-doc "Returns the tag of the specified record instance.".
-spec get_record_tag( record() ) -> record_tag().
get_record_tag( RecordTuple ) ->
	element( _Index=1, RecordTuple ).




% Counters subsection.
%
% Maybe a counter_utils module will exist some day.
%
% See also the 'counters' standard module (see
% https://www.erlang.org/doc/man/counters.html) for shared (cross-process)
% atomic counters.


-doc "Initialises the specified number of counters to zero.".
-spec initialise_counters( count() ) -> counters().
initialise_counters( Count ) ->
	initialise_counters( Count, _InitValue=0 ).



-doc """
Initialises the specified number of counters to the specified (initial) value.
""".
-spec initialise_counters( count(), integer() ) -> counters().
initialise_counters( Count, InitValue ) ->
	list_to_tuple( list_utils:duplicate( InitValue, Count ) ).



-doc "Increments the specified counter.".
-spec increment_counter( positive_index(), counters() ) -> counters().
increment_counter( CounterIndex, Counters ) ->
	add_to_counter( _ToAdd=1, CounterIndex, Counters ).



-doc "Adds the specified value to the specified counter.".
-spec add_to_counter( number(), positive_index(), counters() ) -> counters().
add_to_counter( ToAdd, CounterIndex, Counters ) ->
	NewElem = element( CounterIndex, Counters ) + ToAdd,
	setelement( CounterIndex, Counters, NewElem ).



-doc "Returns the last element of the specified tuple.".
-spec get_last_tuple_element( tuple() ) -> term().
get_last_tuple_element( Tuple ) ->
	element( _PosIndex=size( Tuple ), Tuple ).


-doc "Sets the last element of the specified tuple.".
-spec set_last_tuple_element( tuple(), term() ) -> tuple().
set_last_tuple_element( Tuple, NewElement ) ->
	setelement( _PosIndex=size( Tuple ), Tuple, NewElement ).


-doc """
Augments the specified tuploid with the specified term, placed as new last
element.

For example:
```
augment_tuploid(a, 2.0) = {a, 2.0}
augment_tuploid({foo, 42}, 2.0) = {foo, 42, 2.0}
```

Useful typically to augment returned error tuploids (either a single error term
such as `invalid_name`, or a tuple like `{invalid_name,"Arnold"}`) with
caller-local information, to obtain in all cases a tuploid (a tuple here) with
this extra information.
""".
-spec augment_tuploid( tuploid(), term() ) -> tuploid().
augment_tuploid( Tuploid, ExtraTerm ) when is_tuple( Tuploid ) ->
	List = list_utils:append_at_end( ExtraTerm, tuple_to_list( Tuploid ) ),
	list_to_tuple( List );

augment_tuploid( BasicTuploid, ExtraTerm ) ->
	{ BasicTuploid, ExtraTerm }.



-doc "Returns a textual description of the specified array.".
-spec array_to_string( array() ) -> ustring().
array_to_string( Array ) ->
	case array:to_list( Array ) of

		[] ->
			"empty array";

		L ->
			text_utils:format( "array of ~B elements: ~p", [ length( L ), L ] )

	end.
