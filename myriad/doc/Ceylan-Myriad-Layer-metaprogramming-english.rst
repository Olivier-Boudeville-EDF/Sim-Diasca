
:raw-latex:`\pagebreak`

.. _Metaprogramming:


Support for Metaprogramming
===========================

Over time, quite a lot of developments grew to form primitives that manage ASTs (*Astract Syntax Trees*), based on Erlang's parse transforms.

These developments are gathered in the ``src/meta`` directory, providing notably:

- ``meta_utils.{e,h}rl``: basic primitives to **transform ASTs**, with a bit of testing (``meta_utils_test``)
- ``type_utils``: a still rather basic toolbox to **manage data types** - whether built-in, compound or parametrised (expressed as strings, as terms, etc.)
- ``ast_*`` modules to handle the various **elements that can be found in an AST** (ex: ``ast_expression``, ``ast_type``, ``ast_pattern``, etc.)


Finally, a few usage examples of these facilities are:

- ``minimal_parse_transform_test``: the simplest parse transform test that we use, typically operating on ``simple_parse_transform_target``
- ``example_parse_transform``: a rather minimal parse transform
- ``myriad_parse_transform``: the parse transform used within ``Myriad``, transforming each and every module of that layer (and of at least some modules of upper layers)

So the purpose of this parse transform is to **convert ASTs that are Myriad-compliant into ASTs that are directly Erlang compliant**.

.. _`table transformations`:

For that, following changes are operated:

- in type specifications, the Myriad-specific ``void/0``, ``maybe/1`` and ``fallible/{1,2}`` types are adequately translated:

 - ``void()`` becomes ``basic_utils:void()``, a type alias of ``any()``, made to denote returned terms that are not expected to be used by the caller (as if that function's only purpose was its side-effects)
 - ``maybe(T)`` becomes the type union ``'undefined'|T``
 - ``fallible(T)`` becomes ultimately the type union ``{'ok',T}|{'error',term()}``, while ``fallible(Tok, Terror)`` becomes ``{'ok',Tok}|{'error',Terror}``

- both in type specifications and actual code, ``table/2``, the Myriad-specific associative table pseudo-type, is translated into an actual `table type`_:
 - by default, ``map_hashtable`` (the generally most efficient one)
 - unless it is overridden on a per-module basis with the ``table_type`` define, like in: ``-table_type(list_table).``

- the ``cond_utils`` services drive conditional code injection: based on the build-time tokens defined, their values can be used to perform compilation-time operations such as **if** (see in this module ``if_debug/1``, ``if_defined/{2,3}``, ``if_set_to/{3,4}``), **switch** (see ``switch_set_to/{2,3}``, possibly with a default clause) or **assert** (``assert/{1,2,3}``); if useful, it should be fairly easy (infrastructure mostly ready) to transform the (currently constant) user-defined build tokens into mutable variables and to add for example compile-time assignments (``cond_utils:create_token(TOKEN, MAYBE_INITIAL_VALUE)``, ``cond_utils:set_token_value(TOKEN, VALUE)`` and ``cond_utils:remove_token(TOKEN, MAYBE_INITIAL_VALUE)``) of these variables and loops (**for**, **while**, etc.) if not going for a Turing-complete language, if ever that made sense for some uses; see the `Support for Code Injection`_ for additional usage details regarding the supported primitives



More generally, Myriad offers the support to traverse *any* AST (the whole Erlang grammar is supported, in its abstract form) and to **transform** it (ex: an expression being removed, transformed or replaced by other expressions), with the ability for the user to define his own type/call replacement mappings, or more general transformation functions to be triggered when specified elements are found in the AST (ex: remote calls with relevant MFA).

The traversal may be done in a stateful manner, i.e. any user-defined transformation will be able to access (read/write) any state of its own in the course of the traversal.

As a result, a single pass through the input AST may be done, in which any kind of transformations may be applied, resulting in another (Erlang-compliant) AST being output and afterwards compiled.
