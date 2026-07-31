% Copyright (C) 2014-2026 Olivier Boudeville
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
Module helping to manage **datatypes** (and also values), notably in plain
programs and ASTs, but also for our own type language (which does not
specifically depends on Erlang); it is named the Myriad type system and
currently supports any parametrised (a type possibly depending on other types),
non-recursive type.

We define here a type as a (potentially infinite) set of values.

See `type_utils_test.erl` for the corresponding test.

See also the `meta_utils` module for all topics regarding metaprogramming,
parse-transforms, etc.
""".


% Design notes about Myriad types.


% Myriad types may be defined according to three forms, from the most
% human-focused to the most computer-native one:
%
% F1. type-as-a-string, i.e. a textual specification of a type, possibly entered
% from a user interface; for example, a type (possibly named "my_type") may be
% specified as: "union(foo(), bar(), [integer()])"; this is mostly in line with
% the Erlang conventions regarding (user-level) type specifications, and
% corresponds here to the text_type() type
%
% F2. type-as-a-contextual-term, i.e. an Erlang term that defines a type, yet
% may still be contextual (i.e. it may depend on other non-builtin types); the
% same example may then be defined as the {union, [{foo,[]}, {bar,[]},
% {list,{integer,[]}}]} contextual type (hence a term), where the foo/0 and
% bar/0 types are expected to be defined in the context; this corresponds to the
% contextual_type() type; see also parse_type/1 to generate them from a F1 form
%
% F3. explicit-type, i.e. a fully explicit, self-standing, Erlang-level term
% defining a type (therefore relying only types and type constructs that are
% built-in, the initial type being fully resolved into them); for example,
% supposing that the type foo/0 is an alias for float, and that the type bar/0
% is specified as 'union(hello, goodbye)', the same example translates to the
% following explicit type: {union, [{float,[]}, {union,[{atom,hello},
% {atom,goodbye}]}, {list,{integer,[]}}]}; this corresponds to the
% explicit_type() type; see resolve_type/2 for that

% Going from:
%
% - form F1 (text_type/0) to form F2 (contextual_type/0) is named (here) type
% parsing, and is implemented by parse_type/1
%
% - form F2 (contextual_type/0) to form F3 (explicit_type/0) is named (here)
% type resolution, and is implemented by resolve_type/2

% Instead of "union(T1, T2, T3)" we would have preferred "T1|T2|T3", but reusing
% parts of the native parser (possibly erl_parse.yrl) for that does not seem
% obvious.


% Links with the Erlang type system and its AST.
%
% The internal, "formal", actual programmatic description of a type according to
% our conventions is a type-as-a-term (either contextual or explicit, i.e. F2 or
% F3), relying on a translated version of the textual type (which is for
% example: "[{float(),boolean()}]").

% This "internal type language of the Myriad layer" is largely inspired from the
% forms that can be found in actual (Erlang) ASTs.
%
% The requirements for this term-based description were to be able to represent
% at least any actual type, including parametrised ones, as they are certainly
% useful (e.g. maps/associative tables, lists, etc.).
%
% A problem is that, in terms (as opposed to their textual counterpart),
% parentheses cannot be used to express parametrised types (not only they denote
% function calls, but also are not legit components of a term); therefore the
% convention chosen here is to specify types as pairs, the first element being
% the name of the type, the second one being the (ordered) list of the types it
% depends on; then the textual type "a(T1, T2)" is translated to the {a,[T1,T2]}
% type term; most types being plain, they are represented as
% '{my_simple_type,[]}', which could be further shortened in the
% 'my_simple_type' (as an atom - as atoms are not homoiconic here, in the sense
% that, in terms of types, an atom 'foobar' is not represented directly as
% 'foobar', but as '{atom,foobar}' - although they could be; atoms in general
% are represented as the '{atom,[]}' explicit type).
%
% So, as an example, the type-as-a-term corresponding to
% '"[{float(),boolean()}]"' is: '{list, {tuple, [{float,[]}, {boolean,[]}]}}';

% Note that an alternate type language (sticking more closely to its textual
% counterpart) could have been a more direct '[{float,boolean}]' term (hence
% getting rid of the parentheses and the pairs whose second element would be an
% empty list); reason for not doing so: then no possible support of the
% parametrised types that happen to be often needed (e.g. 'table(atom,int)' is
% not accepted by the (native) parsing logic, reporting a "bad term").

% The origin of this term-as-a-type notation is clearly the standard (Erlang)
% type specifications; for example 'meta_utils:string_to_form("-type a() ::
% [{float(),boolean()}].").' returns the following AST form:
% {attribute,1,type, {a,{type,1,list, [{type,1,tuple,[{type,1,float,[]},
% {type,1,boolean,[]}]}]}
%
% As a result, the counterpart to the aforementioned '"[{float(), boolean()}]"'
% type string is translated in ASTs as:
% {type, 1, list, [{type, 1, tuple, [{type, 1, float, []}, {type, 1, boolean,
% []}]}]}
%
% Then one can remove:
%
% - the 'type' (and 'user_type') atoms (not making then a specific distinction
% between the origin of a type); a list of built-in types - names and arities -
% is maintained, other types being then user ones; 'typevar' is introduced, for
% "type variables", for type definitions (like foo(T) being, say, {count(),T}
%
% - the line numbers (the '1's here), not useful in that context, hence stripped
%
% - as our (built-in) list is parametrised by a single type (the one of its
% elements), this type does not need to be enclosed in a list
%
% Therefore we finally obtain our aforementioned term-as-a-type:
% '{list, {tuple, [{float,[]}, {boolean,[]}]}}'.
%
% So we can ore describe this way arbitrary types as valid terms.




% On type names and signatures.

% A type T (whether built-in or not - possibly user-defined) is designated
% directly by its name T, as an atom, with its arity (supposing it depends on
% the T1, T2, ..., Tk types); its F2 form (representation) is Rep(T) = {TAsAtom,
% [Rep(T1), Rep(T2), ..., Rep(Tk)}.
%
% For example a type written as "count()", refered to as: 'count()', would be in
% a F2 form designated as {count,[]} - and not just as 'count'.

% There are reserved type-related names (atoms), which correspond to:
%  - built-in types: atom, boolean, integer, float, string, binary, any, none
%  - type constructs: option, list, table, tuple, union
%  - other: typevar


% A type signature is made from the type name and from a list of the type names
% (if any) it depends upon.
%
% A plain type does not depend on any other type, whereas the other (thus the
% non-plain) types are called parametrised types; notably a type parametrised by
% a single type is named here monomorphic, whereas those relying on at least two
% types are named polymorphic. A type depending on k types is named k-morphic.
%
% For "plain" types (i.e. types that are not parametrised by other types), their
% signature is their sole name. For example "foo" ("foo()" is also accepted).
%
% The signature of parametrised types (i.e. types that are parametrised by other
% types) is made of their name immediately followed by the names of the types
% they depend upon, enclosed in parentheses.
%
% For example, a type T that depends on types T1, T2, ..., Tk may have for
% signature "T(T1, T2, ..., Tk)".


% Let Rep(type_signature()) -> explicit_type() be a pseudo-function returning
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
% immediate values of it, as discussed in next section), Rep(TAsString) = T.
% For example, Rep("atom") = atom, or Rep("my_type") = my_type.



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
%  - option/1, i.e. maybe-values, to express option(T), i.e. T | 'undefined';
%  represented as a term as {option, Rep(T)} (note that here as well a 'maybe'
%  atom would not be accepted, resulting in a syntax error when parsing)
%
%  - list/1, i.e. homogeneous lists whose elements are of type T, represented as
%  a term as {list, Rep(T)}; thus to be used for example as '{list, float}' to
%  express [float()]; to designate heterogenous lists, i.e. list() (or [any()]),
%  {list, any} is used; note that an empty list will match any list type
%
%
%  - table/2, i.e. associative key/value tables, e.g. '{table,atom,string}' for
%  table(atom(), ustring()); note that an empty table will match any table type
%
%  - tuple/*, i.e. fixed-size tuples of possibly heterogeneous types, e.g
%  '{tuple, [boolean, T, float]}' (a value of that type being thus {true,
%  ValueOfTypeT, 3.14}
%
%  - union/*, i.e. sets of possible types, e.g '{union, [boolean, T, float]}'
%
% Note: they can also be seen as built-in parametrised types.
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
% For example, if the my_tuple_type type is defined as "{integer(),
% union(boolean(),float()), [atom()]}" then Rep(my_tuple_type)= {list,
% [{integer,[]}, {union, [{boolean,[]}, {float,[]}]}, {list,{atom,[]}}]}.
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
% [integer()]", then Rep(my_type) = {union, [{foo,[]}, {atom,'kazoo'},
% {list,{integer,[]}}]}.
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
% We have thus following respective translations of plain types:
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
% as [foo()] / {list, {foo,[]}} versus [foo] / {list,{atom,foo}}
%
% - at least currently:

%   - no "origin" (like a module) of a type applies, they share the same
%   namespace (e.g. only one 'count' type exists, even if it is actually
%   basic_utils:count(), and if other modules could define their own 'count'
%   type)
%
%   - the {TypeName,Specialiser} structure (see primitive_type_spec/0) could be
%   enriched into a {TypeName,Specialiser,Constraints} one; an example of
%   constraint could be, for integers, to respect open range (e.g. to express
%   the same as pos_integer/0)


% Implementation notes:
%
% This module is a pioneer one, as a result it is not processed by the Myriad
% parse transform and thus cannot use 'cond_utils:if_defined(
% myriad_debug_types, ...)' or table/2 directly.



-doc """
Describes the name of a type (without, for a parametrised one, the names of the
types it depends on).

For example `my_count`.

The reserved type names are:
  - built-in types: `atom`, `integer`, `float`, `string`, `binary`,
    `any`, `none`
  - type constructs: `option`, `list`, `table`, `tuple`, `union`
  - derived types: `boolean`, `count`, possibly the `low_level_type/0` ones
  - other: `typevar`
""".
-type type_name() :: atom().



-doc """
The number of types a (possibly parametrised) type depends on (possibly zero for
plain types).
""".
-type type_arity() :: count().



-doc """
Identifier of a Myriad type.

Analoguous to `function_id/0`.
""".
-type type_id() :: { type_name(), type_arity() }.



-doc """
A type-as-a-string, i.e. a textual specification of a type, possibly entered
from a user interface; corresponds to the F1 form.

Designed not to be Erlang-specific, for example to be used to specify the
content of any networked messages, yet inspired from the syntax used for the
(Erlang) type specifications
([http://erlang.org/doc/reference_manual/typespec.html]).

For example: `"[{float(), boolean(), union(foo()|bar|[integer()])}]"`, where
`foo` is a (plain) type (expected to be defined in the context) and `bar` is an
atom.

Such a string representation can be converted into a `contextual_type()` term,
see the `parse_type/1` function for that.
""".
-type text_type() :: ustring().


-doc """
The most user-friendly textual form of a type.

For example the contextual type `"[{float(), boolean(),
union(foo()|bar|[integer()])}]"` would be represented by "[{float(), boolean(),
foo()|bar|[integer()]}]".

Less flexible that a `text_type/0`, which can be parsed into a contextual type.
""".
-type user_text_type() :: ustring().


-doc """
A type-as-a-contextual-term, i.e. an Erlang term that defines "formally" a
Myriad type, yet may still be contextual (i.e. it may depend on other
non-builtin, possibly parametrised types).

This corresponds to the F2 form, and is relatively in the same spirit as the
[Erlang Abstract Format](https://www.erlang.org/doc/apps/erts/absform.html),
albeit this current form only deals with types (not programs, values, etc.).

So such a type may not be fully explicit, as it may contain unresolved
references to other types; for example: `{list, [{count, []}]}` does not specify
what the `count/0` type is. Same for `{typevar,'T'}`.

The fully explicit types (F3) can be obtained by composing the primitive types
(see `primitive_type_name/0`) with the compounding, parametrised
construction types (see `compounding_type_name/0`).

The same example of the `"union(foo(),bar,[integer()])"` `text_type()` may then
be defined here as the `{union, [{foo,[]}, {atom,bar}, {list, {integer,[]}}]}`
contextual type (hence a term), where the `foo/0` type is expected to be defined
in the context.

Such a contextual type may be translated (see `parse_type/0`) from a textual
type (which is for example: `"[{float(),boolean()}]"`).

It is ultimately a recursive type, and may contain type variables (like in
`thing(SomeType)`) where `SomeType` designates a free type, i.e. can be any
contextual type).

If not specified otherwise, corresponds to the most general, usual notion of
"type" (typically in user code).
""".
-type contextual_type() ::
    tuploid( primitive_type_spec() | compounding_type_spec() ).


-doc """
An explicit, fully-resolved Myriad type, i.e. a self-standing, Erlang-level term
defining a type (therefore relying only on built-in types, type constructs and
possibly type variables), all initial types having been fully resolved into
them; corresponds to the F3 form.

For example, supposing that the type `foo()` is an alias for `float()`, and that
the type `buzz()` is specified as `"hello|goodbye"`, a
`"foo()|buzz()|[integer()]"` `text_type()` translates to the following explicit
type: `{union, [float, {union, [{atom,hello}, {atom,goodbye}]},
{list,integer}]}`.

An explicit type, being a type that has been fully resolved in terms of built-in
constructs, is thus autonomous, self-standing, and is a special case of
contextual type.
""".
-type explicit_type() :: contextual_type().


-doc "An AST form describing a Myriad (contextual) type.".
-type type_form() :: form().




-doc """
The "most precise" descriptive name of an in-memory primitive type, in which:
- simple types do not coexist (e.g. no `boolean` and `atom` overlapping)
- `string` is not used (just a list of characters)
- `number` and `bitstring` are not used (as they derive respectively from
`float()|integer()` and `binary()`)

A note about Erlang floats: they are actually IEEE 754 double-precision
floating-point numbers, a format that occupies 8 bytes (64 bits) per float in
memory.

More precisely, as one can see in `erts/emulator/beam/erl_term.h`, a float_def
is an union able to contain a ieee754_8 datatype, aliased to the `double` C
datatype.

As this type is about names, parametrised types (e.g. lists) are described with
no mention of the types they may depend on (e.g. `list` can be specified, not
`list(float())` or anything like that).

The description of any given type is based on `primitive_type_name/0`,
and can be done in two complementary forms: the textual one, and the internal
one, which are relatively different.
""".
-type primitive_type_name() ::
    'atom' % Note that, in F3 form, {atom,[]} designates the atom/0 type,
           % whereas {atom,foo} designates the 'foo' atom.
  | 'binary'
  | 'float'
  | 'integer'
  | 'pid'
  | 'port'
  | 'reference'
  | 'void'. % This "bottom" type may be useful to describe function returns.



-doc """
A primitive type specialiser is:

- either an empty list, used to designate the type itself (like `[]` in
  `{integer,[]}`)

- or a value of that type to designate a corresponding literal (like in
`{integer,4}`, `{float,1.0}`, `{atom,foo}`, etc.)
""".
-type primitive_type_specialiser() :: [] % The type itself.

                                      % Literals:
                                    | atom()
                                    | binary()
                                    | float()
                                    | integer()
                                    | pid()
                                    | port()
                                    | reference().
                                    % Nothing for 'void' of course.


-doc "Specifies a primitive type (including literals).".
-type primitive_type_spec() ::
    { primitive_type_name(), primitive_type_specialiser() }.



-doc """
Designates the Erlang-level types that may be encountered as such in an actual
term.

Complementary to `abstract_type_name/0`.

Convenient for example to target concrete types to be replaced.
""".
-type concrete_type_name() :: primitive_type_name()
                            | 'map'
                            | 'tuple'
                            | 'list'.


-doc """
Designates the Erlang-level types (enriched a bit by Myriad) that exist but may
not be encountered as such in an actual term.

Complementary to `concrete_type_name/0`.
""".
-type abstract_type_name() :: 'any'
                            | 'none'
                            | 'option'
                            | 'table' % Introduced by Myriad
                            | 'union'.


-doc """
Describes a compounding type, i.e. a type possibly involving/aggregating other
types.

They can be nested (i.e. they can operate on primitive and/or compounding
types).
""".
-type compounding_type_name() ::
    % Some day maybe: 'function', 'record'
    monomorphic_container_type_name()
    | 'table' % 2-morphic (and preferred to 'map', its implementation)
    | 'tuple' % n-morphic
    | 'union'. % n-morphic


-doc "Describes a type of monomorphic container.".
-type monomorphic_container_type_name() ::
    'option'
  | 'list'.


-doc "Specification of a compounding type.".
-type compounding_type_spec() ::
    monomorphic_container_type_spec()
  | { 'table', { contextual_type(), contextual_type() } }
  | { 'tuple', [ contextual_type() ] }
  | { 'union', [ contextual_type() ] }.



-doc "Specification of a monomorphic container type.".
-type monomorphic_container_type_spec() ::
    { 'option', contextual_type() }
  | { 'list', contextual_type() }.



-doc "Designates a value (of a given type), to clarify typing.".
-type value() :: term().


-doc """
The "most precise" (atom) description of a value.

Choices overlap intentionally (e.g. `integer` and `pos_integer`), to express
finer types; as a result, the main purpose of this type is to tell whether a
given value matches specifically one of these types (see `is_value_matching/2`).

See also `get_ast_builtin_types/0`.
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

  | 'string' % Plain ones (lists of characters)
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
The name of a type variable.

Used with `typevar`, to keep track of parametrised types in their definition.

For example `T` when discussing the `foo(T)` parametrised type.
""".
-type type_variable_name() :: atom().



-doc """
A type definition specifies a Myriad type in terms of (generally) other types,
in a similar way as when using the '-type' attribute in Erlang source code.

For example, defining the `count/0` type to be an alias of `integer/0` (done in
the sources with `-type count() :: integer()`) could result in the `{[],
{integer, []}}` type definition.

Types may be parametrised (by other types); as a result, their contextual types
includes any number (including zero) of typevars.
""".
-type type_definition() :: { [ type_variable_name() ], contextual_type() }.


-doc """
Describes, for a given type variable name, the contextual type that it is to
refer to.

For example the `MyType` type variable shall be resolved in the `{list,
integer()}` contextual type.
""".
-type typevar_binding() :: { type_variable_name(), contextual_type() }.



% For the table macro:
-include("meta_utils.hrl").


-doc """
A table of type definitions.

Each type identifier (i.e. their combination of name and arity) shall be unique
in such a table.

Allows to convert a (generally contextual) type into (possibly explicit) types.

For example, if wanting to define the `foo(V,K)` type as `table({bar,K}, V)`,
then an entry whose key would be `{foo,2}` and whose value would be `{['V',
'K'], {table,{{tuple,[{atom,'bar'},{typevar,'K'}]}, {typevar,'V'}}}}` would be
introduced in such a table.
""".
% Not ?table:?table/2, not table/2, as pioneer module:
-type typedef_table() ::
    map_hashtable:map_hashtable( type_id(), type_definition() ).


-doc """
Description of a nesting depth reached when parsing a type description.

It is in pratice a `{P,B}` pair, where `P` is the parenthesis depth (that is the
number of the parentheses that have been opened and not closed yet) and `B` is
the bracket depth (i.e. the same principle, for `"[]"` instead of for `"()"`).
""".
-type nesting_depth() :: { count(), count() }.


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
- `u` for unsigned
- `s` for signed

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
Designates a tuple-based record instance, to discriminate from a mere tuple.
""".
-type tuple_based_record() :: tuple().


-doc """
The first (atom) element of the tuple corresponding to a record instance.

For example `#my_point{x=1, y=2}` is actually `{my_point, 1, 2}` and thus
`my_point` is the corresponding record tag.
""".
-type record_tag() :: atom().



% Tuploids. See also augment_tuploid/2.


-doc """
A pseudo-tuple, that is a value that is either an actual (homogeneous) tuple or
a single, standalone term, designated as a "basic tuploid".

That is, a tuploid is a tuple of any size, except that the tuploid of size 1 is
`MyTerm`, not the rather useless `{MyTerm}`.

Note that if `MyTerm` can itself be a tuple, this is bound to lead to
ambiguities.
""".
-type tuploid() :: tuploid( term() ).


-doc """
Either a single value of type `T`, or a T-homogeneous tuple, i.e. a tuple whose
all elements are of type `T`.
""".
-type tuploid( T ) :: tuple( T ) | T.


-doc "Local alias.".
-type pair() :: pair:pair().


-doc "Any triplet.".
-type triplet() :: { any(), any(), any() }.


-doc """
Designates an homogeneous tuple (of unspecified size), that is a tuple whose
elements are all of the specified type `T`.
""".
-type tuple( _T ) :: tuple().


-doc """
Designates a fixed-size homogeneous tuple, that is a tuple whose `S` elements
are all of the type `T`.

Here ``S`` should be a type that conflates to a single, strictly positive
integer value.
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


-doc """
Describes an error met when coercing a string to a value of a given type.
""".
-type type_coercion_error() :: { 'value_not_matching_type',
    ValueStr :: ustring(), Value :: value(), explicit_type() }.


-export_type([ type_name/0, type_arity/0, type_id/0,
               text_type/0, user_text_type/0,
               contextual_type/0, explicit_type/0,

               primitive_type_name/0, primitive_type_specialiser/0,
               primitive_type_spec/0,

               concrete_type_name/0, abstract_type_name/0,
               compounding_type_name/0,
               monomorphic_container_type_name/0,
               compounding_type_spec/0,
               monomorphic_container_type_spec/0,
               value/0, value_description/0,
               type_variable_name/0, type_definition/0, typevar_binding/0,
               typedef_table/0,
               nesting_depth/0, void/0, low_level_type/0,

               option/1, safe_option/1, wildcardable/1,

               bit_mask/0,

               uint8/0, uint16/0, uint32/0, uint64/0,
               sint8/0, sint16/0, sint32/0, sint64/0,
               int8/0, int16/0, int32/0, int64/0,
               float32/0, float64/0,

               positive_float/0, non_negative_float/0,

               tuple_based_record/0,
               tuploid/0, tuploid/1,
               pair/0, triplet/0, tuple/1, tuple/2,
               map/2,
               pid_ref/0,
               permanent_term/0, transient_term/0,

               type_coercion_error/0 ]).



% Type-related functions:
-export([ type_to_string/1, get_type_of/1,

          interpret_type_of/1, interpret_type_of/2,
          describe_type_of/1,

          get_immediate_types/0, get_ast_builtin_types/0,
          get_elementary_types/0, get_plain_builtin_types/0,

          get_base_typedef_table/0, register_typedef/3, register_typedefs/2,
          resolve_typedef_table/1,
          typedef_to_string/2, typedef_table_to_string/1,

          is_type/1, is_of_type/2,
          is_homogeneous/1, is_homogeneous/2,
          are_types_identical/2, is_subtype_of/2,
          is_value_matching/2,
          get_low_level_type_size/1,
          is_transient/1, is_byte/1,
          is_non_neg_integer/1, is_pos_integer/1, is_neg_integer/1,
          parse_type/1, interpret_parse_type_error/1, vet_contextual_type/1,
          resolve_type/2, instantiate_type/2,
          vet_explicit_type/1,
          define_type/4,
          coerce_stringified_to_type/2, interpret_type_coercion_error/1 ]).



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
          ensure_string/1, ensure_binary/1,

          integer_to_boolean/1 ]).


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


% For the ast_transforms record:
-include("ast_transform.hrl").



% Type shorthands:

-type count() :: basic_utils:count().
-type level() :: basic_utils:level().
-type positive_index() :: basic_utils:positive_index().
-type tagged_fallible( T ) :: basic_utils:tagged_fallible( T ).
-type tagged_error_info() :: basic_utils:tagged_error_info().

% Being a pioneer module:
-type fallible( TSuccess, TFailure ) ::
    basic_utils:fallible( TSuccess, TFailure ).

-type array() :: array:array().

-type ustring() :: text_utils:ustring().
-type any_string() :: text_utils:any_string().

-type byte_size() :: system_utils:byte_size().

-type form() :: ast_base:form().



-doc """
Returns an atom describing the overall first-level type description, as precise
as reasonably possible, of the specified concrete value (term).

No recursive identification is done to investigate nested values/types; for
example if the specified term is `[1,4]`, `list` will be returned (not
`[integer]` for example).

`is_number/1`, `is_record/1`, etc. not usable here.

Note: often we do not want to retrieve the actual type of a term but need
instead to determine whether the term can be considered as a value of a specific
type (this is not strictly the same need, as a given term in general may be seen
of being of multiple types); for that see `is_value_matching/2`.

The lowest-level/most precise typing can be obtained with the (undocumented)
`erts_internal:term_type/1` function.
""".
-spec get_type_of( term() ) -> value_description().
get_type_of( Term ) when is_boolean( Term ) ->
    'boolean';

get_type_of( Term ) when is_atom( Term ) ->
    'atom';

get_type_of( Term ) when is_integer( Term ) ->
    'integer';

get_type_of( Term ) when is_float( Term ) ->
    'float';

get_type_of( Term ) when is_list( Term ) ->
    case text_utils:is_string( Term ) of

        true ->
            'string';

        false ->
            % case text_utils:are_strings( Term ) of

            %   true ->
            %       '[string]';

            %   false ->
            %       'list'

            % end
            'list'
    end;

get_type_of( Term ) when is_binary( Term ) ->
    'binary';

get_type_of( Term ) when is_function( Term ) ->
    'function';

get_type_of( Term ) when is_pid( Term ) ->
    'pid';

get_type_of( Term ) when is_map( Term ) ->
    'table';

get_type_of( Term ) when is_port( Term ) ->
    'port';

%get_type_of( Term ) when is_record( Term ) ->
%   'record';

get_type_of( Term ) when is_tuple( Term ) ->
    'tuple';

get_type_of( Term ) when is_list( Term ) ->
    'list';

get_type_of( Term ) when is_reference( Term ) ->
    'reference';

get_type_of( Term ) ->
    throw( { no_type_for_value, Term } ).



-doc """

Returns a string describing, in a user-friendly manner, the type of the
specified term (up to one level of nesting detailed), and giving some
information about its value.
""".
-spec interpret_type_of( term() ) -> ustring().
interpret_type_of( Term ) ->
    interpret_type_helper( Term, _CurrentNestingLevel=0, _MaxNestingLevel=1 ).



-doc """
Returns a string describing, in a user-friendly manner, the type of the
specified term, up to the specified nesting level (either a positive integer or
the `infinite` atom, to go as deep as possible in the term structure), and
giving some information about its value.
""".
-spec interpret_type_of( term(), level() | 'infinite' ) -> ustring().
interpret_type_of( Term, MaxNestingLevel ) when MaxNestingLevel >= 0 ->
    interpret_type_helper( Term, _CurrentNestingLevel=0, MaxNestingLevel ).



-doc """
Returns a string describing, in a user-friendly manner, the type of the
specified term, describing any nested subterms up to the specified level, and
giving some information about its value.
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
                    case [ interpret_type_helper( E,
                                CurrentNestingLevel+1, MaxNestingLevel )
                                    || E <- Term ] of

                        [] ->
                            "empty list";

                        [ Elem ] ->
                            text_utils:format( "list of a single element: ~ts",
                                               [ Elem ] );

                        Elems ->
                            text_utils:format( "list of ~B elements: ~ts",
                                [ length( Term ),
                                  text_utils:strings_to_enumerated_string(
                                      Elems, CurrentNestingLevel ) ] )

                    end

            end

    end;

interpret_type_helper( Term, _CurrentNestingLevel=MaxNestingLevel,
                       MaxNestingLevel ) when is_map( Term ) ->
    case maps:size( Term ) of

        0 ->
            "empty table";

        1 ->
            "table with a single entry";

        S ->
            text_utils:format( "table of ~B entries", [ S ] )

    end;

interpret_type_helper( Term, CurrentNestingLevel, MaxNestingLevel )
                                        when is_map( Term ) ->

    case [ text_utils:format( "key ~ts associated to value ~ts",
                [ interpret_type_helper( K, CurrentNestingLevel+1,
                                         MaxNestingLevel ),
                  interpret_type_helper( V, CurrentNestingLevel+1,
                                         MaxNestingLevel ) ] )
                        || { K, V } <- maps:to_list( Term ) ] of

        [] ->
            "empty map";

        [ Entry ] ->
            text_utils:format( "map of a single entry: ~ts", [ Entry ] );

        Entries ->
            text_utils:format( "map of ~B entries: ~ts", [ length( Entries ),
                text_utils:strings_to_enumerated_string( Entries,
                    CurrentNestingLevel ) ] )

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

    Elems = [ interpret_type_helper( E, CurrentNestingLevel+1,
                                     MaxNestingLevel )
                || E <- tuple_to_list( Term ) ],

    BaseTupleDesc = interpret_type_helper( Term, MaxNestingLevel,
                                           MaxNestingLevel ),

    text_utils:format( "~ts made of: ~ts", [ BaseTupleDesc,
        text_utils:strings_to_enumerated_string( Elems,
                                                 CurrentNestingLevel ) ] );

interpret_type_helper( Term, _CurrentNestingLevel, _MaxNestingLevel ) ->
    text_utils:format( "unknown type for '~p'", [ Term ] ).



-doc """
Returns any short string describing, in a Erlang-level (thus mentioning maps,
not tables) user-friendly manner, the type of the specified term (only the top
level of nesting being described), if it could be identified.

Useful to describe error terms, as for example they may ellipsed if too long.
""".
-spec describe_type_of( term() ) -> option( ustring() ).
describe_type_of( Term ) when is_boolean( Term ) ->
    "a boolean";

describe_type_of( Term ) when is_atom( Term ) ->
    "an atom";

describe_type_of( Term ) when is_binary( Term ) ->
    "a binary";

describe_type_of( Term ) when is_float( Term ) ->
    "a float";

describe_type_of( Term ) when is_function( Term ) ->
    "a function";

describe_type_of( Term ) when is_integer( Term ) ->
    "an integer";

describe_type_of( Term ) when is_pid( Term ) ->
    "a PID";

describe_type_of( Term ) when is_port( Term ) ->
    "a port";

describe_type_of( Term ) when is_reference( Term ) ->
    "a reference";

describe_type_of( _Term=[] ) ->
    "an empty list/string";

describe_type_of( _Term=[ I ] ) when is_integer( I ) ->
    "a single-element list/string";

describe_type_of( _Term=[ _ ] )  ->
    "a single-element list";

describe_type_of( L ) when is_list( L ) ->
    % Otherwise length/1 will fail:
    case list_utils:is_proper_list( L ) of

        true ->
            Len = length( L ),
            case text_utils:is_string( L ) of

                true ->
                    text_utils:format( "a plain string of ~B characters",
                                       [ Len ] );

                false ->
                    text_utils:format( "a list of ~B elements", [ Len ] )

            end;

        false ->
            % Sometimes created on purpose:
            "an improper list"

    end;

describe_type_of( Term ) when is_map( Term ) ->
    text_utils:format( "a map of ~B entries", [ maps:size( Term ) ] );

describe_type_of( _Term={ _A, _B } ) ->
    "a pair";

describe_type_of( _Term={ _A, _B, _C } ) ->
    "a triplet";

describe_type_of( Term ) when is_tuple( Term ) ->
    text_utils:format( "a tuple of ~B elements", [ size( Term ) ] );

% Unknown:
describe_type_of( _Term ) ->
    undefined.



-doc "Returns a list of the possible types for immediate values.".
-spec get_immediate_types() -> [ type_name() ].
get_immediate_types() ->
    % Not sure this list is very accurate or relevant:
    [ 'atom', 'float', 'integer', 'binary', 'boolean' ].



-doc """
Returns a list of the possible types for immediate values (typically found in an
AST like, like `undefined` in `{atom,42,undefined}`).

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
-spec get_ast_builtin_types() -> [ type_name() ].
get_ast_builtin_types() ->

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
Returns a list of the base, elementary, "atomic", built-in types.
""".
-spec get_elementary_types() -> [ type_name() ].
get_elementary_types() ->
    get_immediate_types() ++
        [ 'function', 'list', 'map', 'pid', 'port', 'record', 'reference',
          'tuple', 'any' ].



-doc """
Returns a list of the plain, built-in types that can be typically found in AST
forms.
""".
-spec get_plain_builtin_types() -> [ type_name() ].
get_plain_builtin_types() ->
    get_immediate_types() ++ [ 'pid', 'port', 'reference', 'any', 'no_return' ].



-doc "Returns the base, Myriad-level type definition table.".
-spec get_base_typedef_table() -> typedef_table().
get_base_typedef_table() ->
    % We add it the contextual (i.e. non-explicit) types that are commonly used
    % in Myriad (corresponds at least in spirit to the Erlang built-in types,
    % see https://www.erlang.org/doc/system/typespec.html):
    %
    map_hashtable:new( [

        % These are {type_id(), type_definition()} pairs, where
        % type_definition() :: { [ type_variable_name() ], contextual_type() }:

        % Two atom literals:
        { {boolean,0}, { [], { union, [ {atom,true}, {atom,false} ] } } },

        % Just an alias (the positive constraint cannot be expressed like with
        % non_neg_integer/0):
        %
        { {count,0}, { [], {integer,[]} } },

        { {number,0}, { [], { union, [ {integer,[]}, {float,[]} ] } } },

        { {char,0}, { [], {integer,[]} } },

        % Already explicit types (see type_utils_test to obtain any static
        % resolution needed):
        %
        %{ {string,0}, { [], { list, {char,[]} } } }
        { {string,0}, { [], { list, {integer,[]} } } }

        % Add here all other types of interest.
    ] ).



-doc """
Adds the definition of the specified contextual type to the specified typedef
table.
""".
-spec register_typedef( type_name(), type_definition(), typedef_table() ) ->
                                                    typedef_table().
register_typedef( TypeName, _TypeDef={ TypeVarNames, CtxtType },
          TypedefTable ) when is_atom( TypeName ) and is_list( TypeVarNames ) ->
    vet_contextual_type( CtxtType ),
    TypeId = { TypeName, length( TypeVarNames ) },
    case map_hashtable:lookup_entry( TypeId, TypedefTable ) of

        key_not_found ->
            map_hashtable:add_entry( _K=TypeId, _V=CtxtType, TypedefTable );

        { value, PrevCtxtType } ->
            throw( { type_already_present, TypeId, PrevCtxtType, CtxtType } )

    end.



-doc """
Adds the definition of the specified contextual types to the specified typedef
table.
""".
-spec register_typedefs( [ { type_name(), type_definition() } ],
                                typedef_table() ) -> typedef_table().
register_typedefs( TypedefPairs, TypedefTable ) ->
    lists:foldl( fun( _TP={ TN, TDef }, AccTDTable ) ->
                   register_typedef( TN, TDef, AccTDTable )
                 end,
                 _Acc0=TypedefTable,
                 _List=TypedefPairs ).



-doc """
Resolves the specified typedef table.

Currently only a very basic implementation; does not manage yet
mutually-dependent types.
""".
-spec resolve_typedef_table( typedef_table() ) -> typedef_table().
resolve_typedef_table( TypedefTable ) ->
    resolve_typedef_table( map_hashtable:enumerate( TypedefTable ),
                           TypedefTable, _Changed=false ).


% (helper)
resolve_typedef_table( _TypePairs=[], TypedefTable, _Changed=false ) ->
    TypedefTable;

resolve_typedef_table( _TypePairs=[], TypedefTable, _Changed=true ) ->
    % Rescans as long at least one more type could be resolved:
    % (obvious risk of infinite loop)

    % No cond_utils:if_defined/* in this module:
    %trace_utils:debug_fmt( "Rescanning ~ts",
    %                       [ map_hashtable:to_string( TypedefTable ) ] ),

    resolve_typedef_table( TypedefTable );

resolve_typedef_table(
        _TypePairs=[ { TypeName, _TypeDef={ TVarNames, CtxtType } } | T ],
        TypedefTable, Changed ) ->

    case resolve_type( CtxtType, TypedefTable ) of

        % Same:
        CtxtType ->
            resolve_typedef_table( T, TypedefTable, Changed );

        ResolvedNewCtxtType ->

            NewTypedefTable = map_hashtable:add_entry( _K=TypeName,
                _V={ TVarNames, ResolvedNewCtxtType }, TypedefTable ),

            resolve_typedef_table( T, NewTypedefTable, _Changed=true )

    end.



-doc "Returns a textual description of the specified type definition table.".
-spec typedef_table_to_string( typedef_table() ) -> ustring().
typedef_table_to_string( TypedefTable ) ->

    case map_hashtable:enumerate( TypedefTable ) of

        [] ->
            "empty type definition table";

        [ { TypeId, TypeDef } ] ->
            text_utils:format( "table defining a single type: ~ts",
                [ typedef_to_string( TypeId, TypeDef ) ] );

        TypedefEntries ->
            text_utils:format( "table defining ~B types: ~ts",
                [ length( TypedefEntries ), text_utils:strings_to_string(
                    [ typedef_to_string( TId, TDef )
                        || { TId, TDef } <- TypedefEntries ] ) ] )

    end.


-doc "Returns a textual description of the specified type definition.".
-spec typedef_to_string( type_id(), type_definition() ) -> ustring().
typedef_to_string( _TypeId={ TypeName, _TypeArity },
                   _TypeDef={ TypeVarNames, CtxtType } ) ->
    TypeVarStrs = [ text_utils:atom_to_string( N ) || N <- TypeVarNames ],
    VarNameStr = text_utils:join( _Sep=", ", TypeVarStrs ),
    text_utils:format( "type ~ts(~ts) :: ~ts",
        [ TypeName, VarNameStr, type_to_string( CtxtType ) ] ).




-doc """
Tells whether the specified term designates a type (i.e. a `contextual_type()`
instance).
""".
-spec is_type( term() ) -> boolean().
% First, our own simple types, as always in definition order:
% The atom/0 type::
is_type( { atom, [] } ) ->
    true;

% Atome literal (i.e. a specific atom):
is_type( { atom, A } ) when is_atom( A ) ->
    true;

is_type( { atom, _ } ) ->
    false;


% boolean/0:
is_type( { boolean, [] } ) ->
    true;

% Boolean literals:
is_type( { boolean, true } ) ->
    true;

is_type( { boolean, false } ) ->
    true;

%is_type( { boolean, _ } ) ->
%    false;


is_type( { integer, [] } ) ->
    true;

is_type( { integer, I } ) when is_integer( I ) ->
    true;

%is_type( { integer, _ ) ->
%   false;


is_type( { float, [] } ) ->
    true;

is_type( { float, F } ) when is_float( F ) ->
    true;

%is_type( { float, _ } ) ->
%   false;


is_type( { string, [] } ) ->
    true;

is_type( { string, S } ) when is_list( S ) ->
    text_utils:is_string( S );

%is_type( { string, _ } ) ->
%   false;


is_type( { any, [] } ) ->
    true;

%is_type( { any, _ } ) ->
%   false;


is_type( { none, [] } ) ->
    true;

%is_type( { none, _ } ) ->
%   false;

is_type( { void, [] } ) ->
    true;

%is_type( { void, _ } ) ->
%   false;

is_type( { option, ElemType } ) ->
    is_type( ElemType );

is_type( { list, ElemType } ) ->
    is_type( ElemType );

is_type( { table, { KeyType, ValueType } } ) ->
    is_type( KeyType ) andalso is_type( ValueType );

is_type( { TypeTag, Types } ) when (TypeTag =:= tuple orelse TypeTag =:= union)
                                            andalso is_list( Types ) ->
    lists:all( _Pred=fun is_type/1, Types );

is_type( { typevar, VarName } ) when is_atom( VarName ) ->
    true;

% Add any lacking type here.

is_type( _T ) ->
    false.



-doc """
Tells whether the specified term (value) is of the specified explicit type
(predicate).
""".
-spec is_of_type( term(), explicit_type() ) -> boolean().
% First, our own simple types, as always in definition order:
% The atom/0 type::
is_of_type( _Term=A, _Type={ atom, [] } ) when is_atom( A ) ->
    true;

% A specific atom:
is_of_type( _Term=A, _Type={ atom, A } ) when is_atom( A ) ->
    true;

is_of_type( _Term, _Type={ atom, _ } ) ->
    false;


is_of_type( Term, _Type={ boolean, [] } ) when is_boolean( Term ) ->
    true;

is_of_type( _Term, _Type={ boolean, [] } ) ->
    false;


is_of_type( Term, _Type={ integer, [] } ) when is_integer( Term ) ->
    true;

is_of_type( _Term, _Type={ integer, [] } ) ->
    false;


is_of_type( Term, _Type={ float, [] } ) when is_float( Term ) ->
    true;

is_of_type( _Term, _Type={ float, [] } ) ->
    false;


is_of_type( Term, _Type={ string, [] } ) when is_list( Term ) ->
    text_utils:is_string( Term );

is_of_type( _Term, _Type={ string, [] } ) ->
    false;

is_of_type( _Term, _Type={ any, [] } ) ->
    true;

is_of_type( _Term, _Type={ none, [] } ) ->
    false;


is_of_type( _Term=undefined, _Type={ option, _ElemType } ) ->
    true;

is_of_type( Term, _Type={ option, ElemType } ) ->
    is_of_type( Term, ElemType );


is_of_type( ListTerm, _Type={ list, ElemType } ) when is_list( ListTerm ) ->
    lists:all( _Pred=fun( E ) -> is_of_type( E, ElemType ) end,
               ListTerm );

is_of_type( _Term, _Type={ list, _ElemType } ) ->
    false;


is_of_type( TableTerm, _Type={ table, { KeyType, ValueType } } ) ->
    Entries = map_hashtable:enumerate( TableTerm ),
    lists:all( _Pred=fun( { K, V } ) ->
        is_of_type( K, KeyType ) andalso is_of_type( V, ValueType )
                     end, Entries );


is_of_type( TupleTerm, _Type={ tuple, Types } ) when is_tuple( TupleTerm )
        andalso size( TupleTerm ) =:= length( Types ) ->
    Elems = tuple_to_list( TupleTerm ),
    Pairs = lists:zip( Elems, Types ),
    lists:all( _Pred=fun( { E, T } ) -> is_of_type( E, T ) end, Pairs );

is_of_type( _Term, _Type={ tuple, _Types } ) ->
    false;


is_of_type( Term, _Type={ union, Types } ) ->
    lists:any( _Pred=fun( T ) -> is_of_type( Term, T ) end, Types );


is_of_type( Term, Type ) ->
    %get_type_of( Term ) =:= Type.
    throw( { cannot_type_check, { term, Term }, { against_type, Type } } ).



-doc """
Tells whether the specified non-empty monomorphic container (list or tuple;
option, table, union do not apply here) is homogeneous in terms of type, that is
whether all its elements are of the same type.

If true, returns the common type.
If false, returns two of the different types found in the container.
""".
-spec is_homogeneous( monomorphic_container_type_name() ) ->
        { 'true', primitive_type_name() } | { 'false',
            { primitive_type_name(), primitive_type_name() } }.
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
Tells whether the specified non-empty monomorphic container is homogeneous in
terms of type, that is whether all its elements are of the same, specified,
primitive type.
""".
-spec is_homogeneous( monomorphic_container_type_name(),
                      primitive_type_name() ) -> boolean().
is_homogeneous( _List=[], _Type ) ->
    % Considered homogeneous:
    true;

is_homogeneous( { option, Type }, Type ) ->
    true;

is_homogeneous( { option, _DifferentType }, _Type ) ->
    false;

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
Tells whether the two specified explicit types are the same (i.e. designate the
same actual type / are aliases).

Even if restraining to explicit types is safer, this function remains
unreliable, as for example the order of types in an union should not matter.

Quite often a more relevant test can be done with `is_subtype_of/2`.
""".
-spec are_types_identical( explicit_type(), explicit_type() ) -> boolean().
are_types_identical( ExplType, ExplType ) ->
    true;

are_types_identical( _FirstExplType, _SecondExplType ) ->
    false.



-doc """
Tells whether the first type can be considered as a subtype of the second one,
i.e. whether the set of values corresponding to the first (explicit) type is
(non-strictly) included in the one of the second (explicit) type.

Said otherwise, all values covered by the first type must be valid values for
the second type.

As a result, any type is considered as a subtype of itself.

Note: still quite incomplete (mostly conservative/pessimistic).
""".
-spec is_subtype_of( explicit_type(), explicit_type() ) -> boolean().
% For equal types:
is_subtype_of( _FirstType=T, _SecondType=T ) ->
    true;

% For second type as any:
is_subtype_of( _FirstType, _SecondType={ any, [] } ) ->
    true;

% For a literal as first type:
is_subtype_of( _FirstType={ TName, _Literal }, _SecondType={ TName, [] } )
        when TName =:= atom orelse TName =:= binary orelse TName =:= float
             orelse TName =:= integer orelse TName =:= pid orelse TName =:= port
             orelse  TName =:= reference orelse TName =:= string->
    true;


% For (second) type as option:
is_subtype_of( _FirstType={ atom, undefined },
               _SecondType={ option, _ElemType } ) ->
    true;

is_subtype_of( _FirstType={ option, FirstElemType },
               _SecondType={ option, SecondElemType } ) ->
    is_subtype_of( FirstElemType, SecondElemType );


% For (second) type as list:
is_subtype_of( _FirstType={ list, FirstElemType },
               _SecondType={ list, SecondElemType } ) ->
    is_subtype_of( FirstElemType, SecondElemType );


% For (second) type as table:
is_subtype_of( _FirstType={ table, { FirstKeyType, FirstValueType } },
               _SecondType={ table, { SecondKeyType, SecondalueType } } ) ->
    is_subtype_of( FirstKeyType, SecondKeyType ) andalso
        is_subtype_of( FirstValueType, SecondalueType );


% For (second) type as tuple:
is_subtype_of( _FirstType={ tuple, FirstTypes },
               _SecondType={ tuple, SecondTypes } ) ->
    case length( FirstTypes ) =:= length( SecondTypes ) of

        true ->
            TPairs = lists:zip( FirstTypes, SecondTypes ),
            lists:all( _Pred=fun( { FT, ST } ) -> is_subtype_of( FT, ST ) end,
                       TPairs );

        false ->
            false

    end;


% For second type as union:
is_subtype_of( FirstType, _SecondType={ union, SecondTypes } ) ->
    lists:any( _Pred=fun( ST ) -> is_subtype_of( FirstType, ST ) end,
               SecondTypes );

% For first type as union:
is_subtype_of( _FirstType={ union, FirstTypes }, SecondType ) ->
    lists:any( _Pred=fun( FT ) -> is_subtype_of( FT, SecondType ) end,
               FirstTypes );


% Includes _SecondType equal to {none,[]}, {void,[]}, etc.:
is_subtype_of( _FirstType, _SecondType ) ->
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
is_transient( T ) when is_pid( T ) orelse is_port( T )
                                            orelse is_reference( T ) ->
    true;

is_transient( _T ) ->
    false.


% No is_boolean/1 needed, this BIF exists.


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
Parses the specified string, supposedly containing a type-as-a-string, i.e. a
textual specification of a type, and on success returns the corresponding
type-as-a-contextual-term, i.e. an Erlang term that defines (contextually) that
type.

As a result, performs "type parsing", i.e. converts a F1 form into a F2 one (see
the design notes above).

For example: `{ok, {list, {count,[]}}} = parse_type("[count()]")`.

Never throws.
""".
-spec parse_type( text_type() ) -> tagged_fallible( contextual_type() ).
parse_type( TypeStr ) ->

    case text_utils:is_string( TypeStr ) of

        true ->
            parse_type_helper( TypeStr );

        false ->
            { error, { not_string, TypeStr } }

    end.


% (helper)
parse_type_helper( TypeStr ) ->

    % (no cond_utils:if_defined/2 usable in a pioneer module)
    DoTrace = false,
    %DoTrace = true,

    DoTrace andalso
        trace_utils:debug_fmt( "Parsing the '~ts' type.", [ TypeStr ] ),

    DotTypeStr = text_utils:format( "~ts.", [ TypeStr ] ),

    % Not using ast_utils:string_to_form/1 for more specific error reporting;
    case erl_scan:string( DotTypeStr ) of

        { ok, Tokens, _EndLocation=1 } ->

            DoTrace andalso
                trace_utils:debug_fmt( "Scanned tokens:~n ~p.", [ Tokens ] ),

            % Neither parse_term/1 nor parse_form/1 but:
            case erl_parse:parse_exprs( Tokens ) of

                { ok, [ ExprForm ] } ->
                    DoTrace andalso trace_utils:debug_fmt( "Form parsed:~n ~p.",
                                                           [ ExprForm ] ),

                    CtxtType = transform_for_contextual_type( ExprForm ),

                    DoTrace andalso trace_utils:debug_fmt(
                        "Form transformed as a contextual type:~n ~p.",
                        [ CtxtType ] ),

                    % Useful as invalid strings (e.g. "[float()]") might be
                    % accepted by the parsing:
                    %
                    { ok, vet_contextual_type( CtxtType ) };

                { error, ErrorPInfo } ->

                    % Trying to obtain a readable term:
                    ErrStr = text_utils:format( "~ts",
                        [ erl_parse:format_error( ErrorPInfo ) ] ),

                    { error, { type_parsing_failed,
                               { ErrStr, TypeStr, Tokens } } }

            end;

        { error, ErrorSInfo, ErrorLocation } ->

            ErrStr = text_utils:format( "~ts",
                [ erl_scan:format_error( ErrorSInfo ) ] ),

            { error, { type_scanning_failed,
                       { ErrStr, TypeStr, ErrorLocation } } }

    end.


-doc """
Returns a textual description of the specified error triggered by
`parse_type/1`.
""".
-spec interpret_parse_type_error( tagged_error_info() ) -> ustring().
interpret_parse_type_error( { _ErrorTag=not_string, _ErrorInfo=TypeStr } ) ->
    text_utils:format( "the specified type to parse is not a textual one; "
        "it is not a string, but '~p'", [ TypeStr ] );

interpret_parse_type_error( { _ErrorTag=type_parsing_failed,
                              _ErrorInfo={ ErrStr, TypeStr, Tokens } } ) ->
    text_utils:format(
        "the parsing of type '~ts' failed: ~ts (tokens being ~p)",
        [ TypeStr, ErrStr, Tokens ] );

interpret_parse_type_error( { _ErrorTag=type_scanning_failed,
        _ErrorInfo={ ErrStr, TypeStr, ErrorLocation } } ) ->
    text_utils:format(
        "the scanning of type '~ts' failed: ~ts (error location being ~p)",
        [ TypeStr, ErrStr, ErrorLocation ] ).



-doc """
Transforms recursively (acting as a pseudo-function named here `Rep`) the
specified type forms in order to return the corresponding contextual type
(i.e. Myriad's type-as-a-term, a.k.a F2 form).

The approach here is to start from the specified AST form (typically obtained
from parsing), to directly transform it as a term (hence not an AST-to-AST
transform), i.e. as the legit corresponding contextual type.

The following transformation cases are to be applied:

A. all native compounding structures into F2 terms; for example, resulting in
  *actual terms* (not AST forms):

  - `Rep("[T]")` (thus, as AST: `{tuple,_,[{atom,_,list},RepAST(T)]}`) shall
    become `{list,RepTerm(T)}`

  - `Rep("{E1,E2}")` shall become `{tuple, [RepTerm(E1), RepTerm(E2)]}`

B. all free types to type variables; for example:
  `Rep("U")` (thus, as AST: `{var,_,'U'}`), shall become `{typevar, 'U'}`

C. all standalone booleans otherwise atoms into their F2 form; for example:
  `Rep("foo")` (thus, as AST, `{atom,_,foo}`) shall become `{atom,foo}`

D. all pseudo local calls to a F2 sub-type reference; for example:
  `Rep("float()")` (thus as AST: `{call,_,{atom,_float},[]}`), shall become
  `{float, []}`

  So `Rep("[bar(integer(),T2)]"` shall become `{list, {bar, [{integer,[]},
  {typevar,'T2'}]}}`
""".
% We had to go for a custom (not relying on ast_transform), single-pass (to
% ensure a transformed, final form is not attempted to be transformed again)
% transformer, as:
% - {atom,1,foo} shall not be transformed the same depending on whether it is a
% type name in a call, as third element of the call tuple (thus not to be
% changed), or a standalone atom as in case C (thus to be converted in a form
% corresponding to the {atom,foo} term)
% - this last form is {tuple,_,[{atom,_,atom},{atom,_,foo}]} that we do not want
%   to be interpreted again
-spec transform_for_contextual_type( type_form() ) -> contextual_type().
% Case A.1 (tuples):
transform_for_contextual_type( { tuple, _FileLoc, TypeForms } ) ->
    { tuple, [ transform_for_contextual_type( TF ) || TF <- TypeForms ] };

% Case A.2 (lists); only single-element lists like '[integer()]' expected here:
transform_for_contextual_type(
        { cons, _FileLoc, HeadForm, _TailForm={nil,_} } ) ->
    { list, transform_for_contextual_type( HeadForm ) };

transform_for_contextual_type( { cons, FileLoc, HeadForm, TailForm } ) ->
    throw( { non_singleton_list_type, FileLoc, TailForm, HeadForm } );

% Case B (type vars):
transform_for_contextual_type( { var, _FileLoc, AtomTypeName } ) ->
    { typevar, AtomTypeName };

% Case C (standalone atoms):
transform_for_contextual_type( { atom, _FileLoc, BoolLiteral=true } ) ->
    { boolean, BoolLiteral };

transform_for_contextual_type( { atom, _FileLoc, BoolLiteral=false } ) ->
    { boolean, BoolLiteral };

transform_for_contextual_type( { atom, _FileLoc, AtomLiteral } ) ->
    { atom, AtomLiteral };

% Case D (calls/sub-type references):

% The rest of our special cases, besides atoms.
% For option:
transform_for_contextual_type(
        { call, _FileLoc, {atom,_SomeFileLoc, _AtomTypeName=option},
          _TypeForms=[ TypeForm ] } ) ->
    % {option,T}, not {option,[T]}:
    { option, transform_for_contextual_type( TypeForm ) };

% For list:
transform_for_contextual_type(
        { call, _FileLoc, {atom,_SomeFileLoc, _AtomTypeName=list},
          _TypeForms=[ TypeForm ] } ) ->
    % {list,T}, not {list,[T]}:
    { list, transform_for_contextual_type( TypeForm ) };

% For table:
transform_for_contextual_type(
        { call, _FileLoc, {atom,_SomeFileLoc, _AtomTypeName=table},
          _TypeForms=[ KeyTypeForm, ValueTypeForm ] } ) ->
    % {table,{K,V}}, not {table,[K,V]}:
    { table, { transform_for_contextual_type( KeyTypeForm ),
               transform_for_contextual_type( ValueTypeForm ) } };

% The base case for types:
transform_for_contextual_type(
        { call, _FileLoc, {atom,_SomeFileLoc, AtomTypeName}, TypeForms } ) ->
    { AtomTypeName,
      [ transform_for_contextual_type( TF ) || TF <- TypeForms ] };

transform_for_contextual_type( Other ) ->
    throw( { unexpected_type_form, Other } ).



-doc """
Returns the specified contextual type once (superficially) checked: tells
whether it may be valid.
""".
% Note: cannot reuse vet_explicit_type/1, as having to recurse as contextual.
-spec vet_contextual_type( contextual_type() ) -> contextual_type().
vet_contextual_type( Type={ atom, AtomName } ) when is_atom( AtomName ) ->
    Type;

vet_contextual_type( _Type={ option, Type } )  ->
    { option, vet_contextual_type( Type ) };

vet_contextual_type( _Type={ list, Type } )  ->
    { list, vet_contextual_type( Type ) };

vet_contextual_type( _Type={ table, { KType, VType } } )  ->
    { table, { vet_contextual_type( KType ), vet_contextual_type( VType ) } };

vet_contextual_type( _Type={ tuple, Types } ) when is_list( Types ) ->
    { tuple, [ vet_contextual_type( T ) || T <- Types ] };

vet_contextual_type( _Type={ union, Types } ) when is_list( Types ) ->
    { union, [ vet_contextual_type( T ) || T <- Types ] };

vet_contextual_type( Type={ typevar, TypeName } ) when is_atom( TypeName ) ->
    Type;

% For user-defined types:
vet_contextual_type( _Type={ TypeName, Types } ) when is_atom( TypeName )
                                         andalso is_list( Types ) ->
    { TypeName, [ vet_contextual_type( T ) || T <- Types ] };

vet_contextual_type( Other ) ->
    throw( { unexpected_contextual_type, Other } ).


-doc """
Converts the specified contextual type into an explicit type, i.e. a fully
explicit, self-standing, Erlang-level term defining a type, a fully-resolved one
(possibly still-parametrise), based on the specified table in order to resolve
implicit (i.e. non-explicit) types.

As a result, performs "type resolution", i.e. converts a F2 form into a F3 one
(see the design notes above).

For example: `{list, {integer,[]}} = resolve_type({list, {count,[]}},
MyTypedefTable)`.
""".
-spec resolve_type( contextual_type(), typedef_table() ) -> explicit_type().
% First accept as-are, with minimum checking, all explicit types.
% Starting with the primitive types:
resolve_type( Type={ PrimTypeName, _Value }, _TypedefTable )
        when PrimTypeName =:= atom orelse PrimTypeName =:= binary
            orelse PrimTypeName =:= float orelse PrimTypeName =:= integer
            orelse PrimTypeName =:= pid orelse PrimTypeName =:= port
            orelse PrimTypeName =:= reference ->
    Type;

resolve_type( _Type={ option, Type }, TypedefTable )  ->
    { option, resolve_type( Type, TypedefTable ) };

resolve_type( _Type={ list, Type }, TypedefTable )  ->
    { list, resolve_type( Type, TypedefTable ) };

resolve_type( _Type={ table, { KType, VType } }, TypedefTable )  ->
    { table, { resolve_type( KType, TypedefTable ),
               resolve_type( VType, TypedefTable ) } };

resolve_type( _Type={ tuple, Types }, TypedefTable ) ->
    { tuple, [ resolve_type( T, TypedefTable ) || T <- Types ] };

resolve_type( _Type={ union, Types }, TypedefTable )  ->
    { union, [ resolve_type( T, TypedefTable ) || T <- Types ] };

resolve_type( Type={ typevar, _TypeName }, _TypedefTable ) ->
    Type;

% For contextual (i.e. non-explicit) types:
resolve_type( _Type={ TypeName, ParamTypes }, TypedefTable ) ->
    % Erase this type by replacing it with its definition:
    TypeId = { TypeName, length( ParamTypes ) },
    case map_hashtable:lookup_entry( _K=TypeId, TypedefTable ) of

        key_not_found ->
            throw( { unknown_type, TypeId } );

        { value, _TypeDef={ VarNames, CtxtType } } ->

            %trace_utils:debug_fmt( "~w resolved as ~w",
            %                       [ TypeId, { VarNames, CtxtType } ] ),

            % If needed:
            ExplType = resolve_type( CtxtType, TypedefTable ),

            % A lists:zip/2 on steroids:
            NameTypedefPairs = [ { VN, resolve_type( T, TypedefTable ) }
                || VN <:- VarNames && T <:- ParamTypes ],

            %trace_utils:debug_fmt( "Applying, for type ~ts/~B, the following "
            %    "type variables: ~ts.", [ TypeName, length( ParamTypes ),
            %        text_utils:strings_to_listed_string(
            %            [ text_utils:format( "~ts=~w", [ VN, VT ] )
            %              || { VN, VT } <- NameTypedefPairs ] ) ] ),

            instantiate_type( ExplType, NameTypedefPairs )

    end;

resolve_type( Other, _TypedefTable ) ->
    throw( { unexpected_contextual_type, Other } ).



-doc """
Assigns the specified type variables to the specified contextual type:
instantiates it by applying the specified bindings, thus replacing the type
variables that it includes with their actual specified type definition.

No type is resolved in this process.
""".
-spec instantiate_type( contextual_type(), [ typevar_binding() ] ) ->
                                        contextual_type().
instantiate_type( CtxtType, _TypevarBindings=[] ) ->
    CtxtType;

instantiate_type( CtxtType,
                  _TypevarBindings=[ { VarName, VarCtxtType } | T ] ) ->
    NewCtxtType = replace_type_var( CtxtType, VarName, VarCtxtType ),
    instantiate_type( NewCtxtType, T ).




-doc """
Replaces, in the first contextual type, all references to the specified typevar
by the contextual type that is specified as third argument.
""".
-spec replace_type_var( contextual_type(), type_variable_name(),
                        contextual_type() ) -> contextual_type().
% Still going for a full, explicit traversal, first with primitive types:
replace_type_var( Type={ PrimTypeName, [] }, _VarName, _VarCtxtType )
        when PrimTypeName =:= atom orelse PrimTypeName =:= binary
            orelse PrimTypeName =:= float orelse PrimTypeName =:= integer
            orelse PrimTypeName =:= pid orelse PrimTypeName =:= port
            orelse PrimTypeName =:= reference ->
    Type;

% Then compounding types:
replace_type_var( Type={ atom, _AtomName }, _VarName, _VarCtxtType )  ->
    Type;

replace_type_var( _DefCtxtType={ option, Type }, VarName, VarCtxtType ) ->
    { option, replace_type_var( Type, VarName, VarCtxtType ) };

replace_type_var( _DefCtxtType={ list, Type }, VarName, VarCtxtType ) ->
    { list, replace_type_var( Type, VarName, VarCtxtType ) };

replace_type_var( _DefCtxtType={ table, { KType, VType } }, VarName,
                  VarCtxtType ) ->
    { table, { replace_type_var( KType, VarName, VarCtxtType ),
               replace_type_var( VType, VarName, VarCtxtType ) } };

replace_type_var( _DefCtxtType={ tuple, Types }, VarName,
                  VarCtxtType ) ->
    { tuple, [ replace_type_var( T, VarName, VarCtxtType ) || T <- Types ] };

replace_type_var( _DefCtxtType={ union, Types }, VarName,
                  VarCtxtType ) ->
    { union, [ replace_type_var( T, VarName, VarCtxtType ) || T <- Types ] };

% The actual target of this function:
replace_type_var( _DefCtxtType={ typevar, VarName }, VarName, VarCtxtType ) ->
    VarCtxtType;

% An unrelated type variable:
replace_type_var( DefCtxtType={ typevar, _VarName }, _OtherVarName,
                  _VarCtxtType ) ->
    DefCtxtType;

replace_type_var( Other, _VarName, _VarCtxtType ) ->
    throw( { unexpected_type_to_replace, Other } ).






-doc "Returns the specified explicit type once checked.".
-spec vet_explicit_type( explicit_type() ) -> explicit_type().
vet_explicit_type( Type={ atom, AtomName } ) when is_atom( AtomName ) ->
    Type;

vet_explicit_type( _Type={ option, Type } )  ->
    { option, vet_explicit_type( Type ) };

vet_explicit_type( _Type={ list, Type } )  ->
    { list, vet_explicit_type( Type ) };

vet_explicit_type( _Type={ table, { KType, VType } } )  ->
    { table, { vet_explicit_type( KType ), vet_explicit_type( VType ) } };

vet_explicit_type( _Type={ tuple, Types } ) when is_list( Types ) ->
    { tuple, [ vet_explicit_type( T ) || T <- Types ] };

vet_explicit_type( _Type={ union, Types } ) when is_list( Types ) ->
    { union, [ vet_explicit_type( T ) || T <- Types ] };

% Not allowed for explicit types:
%vet_explicit_typee( Type={ typevar, TypeName } ) when is_atom( TypeName ) ->
%    Type;

vet_explicit_type( Type={ T, [] } ) when T =:= binary orelse T =:= boolean
        orelse T =:= float orelse T =:= integer orelse T =:= pid
        orelse T =:= port orelse T =:= reference ->
    Type;

vet_explicit_type( Other ) ->
    throw( { unexpected_explicit_type, Other } ).



-doc """
Defines the specified type, with the specified type variables, in the specified
typedef table.

Typevars have to be explicitly listed, in order to account for their declaration
order, so that when they are instantiated with actual types, these types are
assigned to the right type variables.
""".
-spec define_type( type_name(), [ type_variable_name() ], contextual_type(),
                    typedef_table() ) -> typedef_table().
define_type( TypeName, TypeVarNames, CtxtType, TypedefTable )
                  when is_atom( TypeName ) andalso is_list( TypeVarNames ) ->

    check_atoms( TypeVarNames ),
    DeclaredTypeVarNameSet = set_utils:new( TypeVarNames ),

    vet_contextual_type( CtxtType ),

    % For a term-level transformation:
    CollectTermTransformer =
        fun( Tuple={ typevar, AtomTypeName }, _UserData=Set ) ->
            NewSet = set_utils:add( AtomTypeName, Set ),
            { Tuple, NewSet };

           ( OtherTerm, _UserData=Set ) ->
            { OtherTerm, Set }

        end,

    { _CtxtType, CollectedTypeVarNameSet } = meta_utils:transform_term(
        _TargetTerm=CtxtType, _TypeDescription=tuple,
        _TermTransformer=CollectTermTransformer, _UserData=set_utils:new() ),

    %trace_utils:debug_fmt( "Typevars collected from contextual type ~w: ~w.",
    %    [ CtxtType, set_utils:to_list( CollectedTypeVarNameSet ) ] ),

    { NotUsedSet, NotDeclaredSet } = set_utils:differences(
        DeclaredTypeVarNameSet, CollectedTypeVarNameSet ),

    % First clearer:
    set_utils:is_empty( NotDeclaredSet ) orelse
        begin
            NotDeclared = set_utils:to_list( NotDeclaredSet ),
            trace_bridge:error_fmt( "For defined type '~ts', the following "
                "type variables have been found in the definition yet were not "
                "declared: ~ts.", [ TypeName,
                    text_utils:atoms_to_listed_string( NotDeclared ) ] ),
        throw( { undeclared_type_vars, NotDeclared } )

        end,

    set_utils:is_empty( NotUsedSet ) orelse
        begin
            NotUsed = set_utils:to_list( NotUsedSet ),
            trace_bridge:error_fmt( "For defined type '~ts', the following "
                "type variables were declared but have not been found in the "
                "definition of that type: ~ts.", [ TypeName,
                    text_utils:atoms_to_listed_string( NotUsed ) ] ),
        throw( { unused_type_vars, NotUsed } )

        end,

    TypeId = { TypeName, length( TypeVarNames ) },

    TypeDef = { TypeVarNames, CtxtType },

    map_hashtable:add_new_entry( _K=TypeId, _V=TypeDef, TypedefTable ).




-doc """
Converts the term specified as a string to the actual value that corresponds to
the specified explicit type.

For example: `coerce_stringified_to_type("[4,3]", {list,integer}) = [4,3]`.
""".
-spec coerce_stringified_to_type( any_string(), explicit_type() ) ->
                                    fallible( value(), type_coercion_error() ).
% To have a plain string in all cases:
coerce_stringified_to_type( BinValueStr, ExplType )
                                            when is_binary( BinValueStr ) ->
    coerce_stringified_to_type( text_utils:binary_to_string( BinValueStr ),
                                ExplType );

% Now a plain string;
coerce_stringified_to_type( ValueStr, ExplType ) ->

    %trace_utils:debug_fmt(
    %    "Coercing value string '~p' into explicit type '~w'.",
    %    [ ValueStr, ExplType ] ),

    % Security first:
    vet_explicit_type( ExplType ),

    case ast_utils:string_to_value( ValueStr ) of

        Ro={ ok, V } ->
            case is_of_type( V, ExplType ) of

                true ->
                    Ro;

                false ->
                    Reason = { value_not_matching_type, ValueStr, V, ExplType },
                    { error, Reason }

            end;

        Re -> % { error, R } ->
           Re

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


-doc "Interprets the specified type coercion error.".
-spec interpret_type_coercion_error( type_coercion_error() ) -> ustring().
interpret_type_coercion_error(
        _Error={ value_not_matching_type, ValueStr, Value, ExplType } ) ->
    text_utils:format( "the value represented as the '~ts' string translates "
        "to the '~p' actual value (of type ~ts), which does not match "
        "the ~ts type",
        [ ValueStr, Value, get_type_of( Value ), type_to_string( ExplType ) ] );

interpret_type_coercion_error( Error ) ->
    text_utils:format( "type-coercion error: ~p", [ Error ] ).



-doc """
Returns a textual description (in canonical form, notably without whitespaces)
of the specified contextual type.

Expected to return a string that can be parsed back as a contextual type.

Note: at least currently, only explicit types are supported.
""".
-spec type_to_string( contextual_type() ) -> text_type(). % user_text_type().
% First, simple types, as always in definition order:
% The atom/0 type:
type_to_string( _Type={ atom, [] } ) ->
    "atom()";

% A specific atom (an atom literal):
type_to_string( _Type={ atom, AtomName } ) ->
    % Adding single quoted at least for clarity:
    text_utils:format( "'~ts'", [ AtomName ] );


type_to_string( _Type={ binary, [] } ) ->
   "binary()";

type_to_string( _Type={ binary, Bin } ) ->
    text_utils:binary_to_string( Bin );


type_to_string( _Type={ boolean, [] } ) ->
    "boolean()";

type_to_string( _Type={ boolean, _Literal=true } ) ->
    "'true'";

type_to_string( _Type={ boolean, _Literal=false } ) ->
    "'false'";


type_to_string( _Type={ integer, [] } ) ->
    "integer()";

type_to_string( _Type={ integer, I } ) ->
    text_utils:integer_to_string( I );


type_to_string( _Type={ float, [] } ) ->
    "float()";

type_to_string( _Type={ float, F } ) ->
    text_utils:float_to_string( F );


type_to_string( _Type={ pid, [] } ) ->
    "pid()";

type_to_string( _Type={ pid, F } ) ->
    text_utils:pid_to_string( F );


type_to_string( _Type={ port, [] } ) ->
    "port()";

type_to_string( _Type={ port, P } ) ->
    text_utils:port_to_string( P );

type_to_string( _Type={ reference, [] } ) ->
    "reference()";

type_to_string( _Type={ reference, R } ) ->
    text_utils:reference_to_string( R );


type_to_string( _Type={ string, [] } ) ->
    "string()";

type_to_string( _Type={ string, S } ) ->
    S;


type_to_string( _Type={ any, [] } ) ->
    "any()";

type_to_string( _Type={ none, [] } ) ->
    "none()";

type_to_string( _Type={ void, [] } ) ->
    "void()";

% Then parametrised constructs:

type_to_string( _Type={ option, T } ) ->
    text_utils:format( "option(~ts)", [ type_to_string( T ) ] );

type_to_string( _Type={ list, T } ) ->
    text_utils:format( "[~ts]", [ type_to_string( T ) ] );

type_to_string( _Type={ table, { Tk, Tv } } ) ->
    text_utils:format( "table(~ts, ~ts)",
                       [ type_to_string( Tk ), type_to_string( Tv ) ] );

type_to_string( _Type={ tuple, TypeList } ) when is_list( TypeList ) ->

    TypeStr = text_utils:join( _Separator=", ",
        [ type_to_string( T ) || T <- TypeList ] ),

    text_utils:format( "{~ts}", [ TypeStr ] );


type_to_string( _Type={ union, TypeList } ) when is_list( TypeList ) ->
    % Would not be easily parsable:
    %text_utils:join( _Separator="|",
    %                 [ type_to_string( T ) || T <- TypeList ] );

    TypeStr = text_utils:join( _Separator=", ",
        [ type_to_string( T ) || T <- TypeList ] ),

    text_utils:format( "union(~ts)", [ TypeStr ] );


type_to_string( _Type={ typevar, T } ) ->
    text_utils:atom_to_string( T );

type_to_string( _Type={ NonBuiltinType, TypeList } ) when is_list( TypeList ) ->

    TypeStr = text_utils:join( _Separator=", ",
                               [ type_to_string( T ) || T <- TypeList ] ),

    text_utils:format( "~ts(~ts)", [ NonBuiltinType, TypeStr ] );


type_to_string( Type ) ->

    % Could be misleading (e.g. any() not matching any()):
    %"any".

    %text_utils:format( "~p", [ Type ] ).

    throw( { type_to_string_failed, Type } ).




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



-doc "Converts 1 to `true` and 0 to `false`.".
-spec integer_to_boolean( integer() ) -> boolean().
integer_to_boolean( _I=0 ) ->
    false;

integer_to_boolean( _I=1 ) ->
    true.



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
-spec check_boolean( term() ) -> boolean().
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
check_positive_float( Float ) when is_float( Float ), Float >= 0.0 ->
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
