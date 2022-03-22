:raw-latex:`\pagebreak`

.. _`main conventions`:

---------------------------
``Myriad`` Main Conventions
---------------------------

We list here the conventions of all sorts that the Myriad code (base or contributed one) - and also code in the software stack based on it - shall obey.


Text Conventions
================

The purpose here is to ensure a sufficient **code homogeneity**; for example in all source files are in such a "canonical form", analysing their differences (``diff``) is made simpler.

Any text editor can be used, provided that it saves source files with the UNIX, not DOS, conventions (i.e. lines terminating by the LF character, not by the CRLF characters).

The use of syntax highlighting is encouraged.

Recommended text editors are:

- `Emacs <#emacs-settings>`_
- `Visual Studio Code <https://en.wikipedia.org/wiki/Visual_Studio_Code>`_ (a.k.a. Vscode)
- `ErlIDE <https://erlide.org/>`_ (based on Eclipse)
- Vim, IntelliJ, Gedit, Nedit, etc.


The main editors integrate the *Language Server Protocol* (also known as LSP), refer to the LSP_ section for more details (`Flycheck <https://www.flycheck.org>`_ can be another option).


Source files should be formatted for a 80-character width: no character should be present after the 79th column of a line.

Except in very specific cases, only ASCII code should be used (ex: no accentuated characters).

Tabulations should be preferred to series of spaces, and the text should be formatted according to 4-character tabulations.

All redundant whitespaces should be removed, preferably automatically (see the Emacs ``whitespace-cleanup`` command). This is why, with the `emacs settings`_ that we recommend, pressing the F8 key removes for example the yellow areas in the current buffer by replacing any series of four spaces by a corresponding tabulation.

We would prefer that all files (especially source ones; including the contributed ones) are "whitespace-clean" before being committed. As mentioned, such a transformation can be done directly from Emacs. If using another editor, please ensure that the `fix-whitespaces.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/fix-whitespaces.sh>`_ script has been run on the target sources (possibly automatically thanks to a VCS hook) *before* committing them; the `fix-whitespaces-in-tree.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/fix-whitespaces-in-tree.sh>`_ script may be also used, in order to perform a bulk transformation.

All elements of documentation should be written in English, possibly translated to other languages. Spell-checking is recommended.



Coding Practices
================

In terms of coding style, we would like that the sources remain as uniform as possible, regarding naming, spacing, code/comments/blank line ratios.

We would like that, roughly and on average, the same ratio applies for blank lines, comments and lines of code.

For that one may use the either directly ``make stats`` from the root of the layer or the `make-code-stats.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/make-code-stats.sh>`_ script.

For example::

  In the Erlang source code found from XXX/ceylan/myriad, we have:
  + 208 source files (*.erl), 36 header files (*.hrl)
  + a grand total of 118666 lines:
	- 35959 of which (30.3%) are blank lines
	- 37187 of which (31.3%) are comments
	- 45520 of which (38.3%) are code

The most obvious conventions are:

- the **settings of the build chain** should be used (ex: with regard to compiler flags) and adapted/completed if needed; the (possibly-specialised) ``GNUmakesettings.inc``,  ``GNUmakerules.inc`` and ``GNUmakevars.inc`` files should be relied upon

- **no warning should be tolerated**; anyway our build chain treats warnings as (blocking) errors

- **test cases** should be developed alongside most if not all modules; ex: if developing ``src/class_X.erl``, then probably the ``test/class_X_test.erl`` testing code should be developed, after or, even preferably, before the implementation of the tested code; test success should be evaluated automatically, by the code (ex: thanks to pattern matching), not by the person running the test (ex: who would have to compare visually the actual results with the expected ones); in some cases, only **integrated tests** can be devised in practice; tests should be gathered in **test suites**, that should be runnable automatically (``make test``, recursively through child directories) and fail loudly (and in a blocking manner) at the first error met

- **multiple levels of quality documentation** should be made available to the code user, and probably be written in parallel to the code; there are at least three documentation levels:

  - lower-level documentation: code should always be **densely commented**, with documentation headers added to all functions, inlined comments (not paraphrasing the code) and self-describing symbols: function names, variable names (ex: ``RegisteredState = ...`` to be preferred to ``NewState = ...``), etc.; more generally all names shall be long enough to be descriptive (clarity preferred over compactness); type specifications also pertain to this low-level documentation effort

  - higher-level **design and/or implementation notes**: they should be available as a set of paragraphs in each source file, before the function definitions, to describe the context and constraints, and help understanding how the features are implemented, and why

  - high-level **developer and user documentation** should be made available, targeting at least HTML and PDF outputs, possibly offering a wiki access as well

- more generally, **comments** should be clear and precise, numerous, rich and complete (overall, in terms of line counts, we target roughly 1/3 of code, 1/3 of blank lines and 1/3 of comments); all comments shall be written in UK English, start with a single ``%`` and be properly word-wrapped (use ``meta-q`` with our Emacs settings to take care of that automatically)

- **indentation** should respect, as already explained, the 80-character width and 4-space tabulation; however the default built-in Erlang indentation mode of ``emacs`` can hardly be used for that, as it leads to huge width offsets (we may in the future provide a variation of the ``elisp`` code for the emacs indentation rules)

- **spacing homogeneity** across source files should be enforced; for example three blank lines should exist between two function definitions, one between the clauses of any given function (possibly two in case of longer clauses); for the sake of "visual parsing", arguments should be separated by spaces (ex: ``f( X ) -> ...``, not ``f(X) -> ...``), especially if they are a bit complex (``f( A={U,V}, B, _C ) -> ...``, not ``f(A={U,V},B,_C) -> ...`` or the usual ``f(A={U,V}, B, _C) -> ...``)

- for **type-related conventions**, at least all exported functions shall have a ``-spec`` declaration; if an actual type is referenced more than once (notably in a given module), a specific user-defined type shall be defined; types shall be defined in "semantic" terms rather than on technical ones (ex: ``-type temperature() :: ...`` than ``float()``; developers may refer to, or enrich, ``myriad/src/utils/unit_utils.erl`` for that)

- the **latest stable version of Erlang** should be used, preferably built thanks to our ``myriad/conf/install-erlang.sh`` script

- the official *Programming Rules and Conventions* should be enforced, as defined `here <http://www.erlang.se/doc/programming_rules.shtml>`_

- the function definitions shall follow **the same order** as the one of their exports

- helper functions **shall preferably be identified as such**, with an ``(helper)`` comment

- if an helper function is specific to an exported function, it shall be defined just after this function; otherwise it should be defined in the **helper section**, placed just after the definition of the exported functions

- defining distinct (non-overlapping), explicit (with a clear-enough name), numerous (statically-defined) **atoms** is cheap; each atom found in the sources is generally to be involved in at least one type definition

- the use of ``case ... of ... end`` should be preferred to the use of ``if`` (never used in our code base)

- we also prefer that the various patterns of a case are indented with exactly one tabulation, and that the closing ``end`` lies as much as possible on the left (ex: if having specified ``MyVar = case ... end``, then ``end`` should begin at the same column as ``MyVar``); the same applies to ``try ... catch ... end`` clauses

- when a term is ignored, instead of using simply ``_``, one should define a **named mute variable** in order to provide more information about this term (ex: ``_TimeManagerPid``); one should then to accidental matching of such names (now a warning is emitted)

- some conventional variable names are, and may be, extensively used: ``Res`` for result, ``H`` and ``T`` for respectively the head and tail of a list on which we recursively iterate

.. _indices:

- indices shall, as much as possible, start at index ``1`` (rather than 0); this is a general Erlang convention (`for lists <https://erlang.org/doc/man/lists.html#description>`_, like with ``lists:nth/2``, for tuples, etc. - unlike `arrays <https://erlang.org/doc/man/array.html#description>`_, though); see ``basic_utils:positive_index/0``

- when needing an **associative table**, use the ``table`` pseudo-module; a key/value pair shall be designated as a table *entry* (ex: variable named as ``RoadEntry``)

- regarding the in-code management of **text**:

  - if a text is to be rather static (constant) and/or if it is to be exchanged between processes, then it should be a UTF8 ``binary``, and its type shall be declared as ``text_utils:bin_string()``
  - other, a plain string (``text_utils:ustring()``) shall be used

- when defining a non-trivial datastructure, a **record** shall be used (rather than, say, a mere ad-hoc tuple or a map of undocumented structure...), a corresponding **type** should be then defined (ex: a ``foobar`` record leading to a ``foobar()`` type), and a **function to describe it** as text shall be provided (ex: ``-spec foobar_to_string(foobar()) -> text_utils:string()``)

  - **mute variables** should be used as well to document actual parameters; for example ``f(3,7,10)`` could preferably be written as a clearer ``f(_Min=3,_Max=7,_Deviation=10)``


.. Note:: Mute variables are however actually bound, thus if for example there is in the same scope ``_Min=3`` and later ``_Min=4``, then a badmatch will be triggered at runtime; therefore names of mute variables should be generally kept unique in a given scope.

 - type shorthands may be defined; for example, if using repeatedly within a module ``text_utils:ustring()``, a local, non-exported type shorthand (``-type ustring() :: text_utils:ustring()``) may be defined so that all other uses of this type become simply ``ustring()``

As not all typos may be detected at compilation-time (ex: wrong spelling for a module), we recommend, for source code, the use of additional static checkers, as discussed in the `type-checking`_ section.


.. _`execution target`:

Execution Targets
=================

Two execution target modes have been defined:

- ``development`` (the default): meant to simplify the task of developers and maintainers by reporting as much information and context as possible, even at the expense of some performances and reliability (ex: no retry in case of failure, shorter time-outs not to wait too long in case of errors, more checks, etc.)
- ``production``: mostly the reciprocal of the ``development`` mode, whose purpose is to favor efficient, bullet-proof operations

These execution targets are *compile-time* modes, i.e. they are set once for all when building the layer at hand (probably based, if using OTP, on the rebar corresponding modes - respectively ``dev`` and ``prod``).

See the ``EXECUTION_TARGET`` variable in ``GNUmakevars.inc`` to read and/or set them.

The current execution target is of course available at runtime on a per-layer level, see ``basic_utils:get_execution_target/0`` for more information.

This function shall be compiled once per layer to be accurate, in one of its modules. It is just a matter of adding the following include in such module::

 -include_lib("myriad/utils/basic_utils.hrl").


See also the (different) `Wings3D coding guidelines <https://github.com/dgud/wings/blob/master/CodingGuidelines>`_, that are interesting in their own right.



Tooling Conventions
===================


.. _LSP:

Erlang LS
---------

The `Language Server Protocol <https://en.wikipedia.org/wiki/Language_Server_Protocol>`_ (also known as LSP) may be used by one's editor of choice in order to provide various services facilitating the developments in various languages, including Erlang.

For that `Erlang LS <https://erlang-ls.github.io/>`_ should be used. We then recommend to rely on our `erlang_ls.config <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/erlang_ls.config>`_ configuration file, which may be installed that way:

.. code:: bash

 $ mkdir -p ~/.config/erlang_ls && cd ~/.config/erlang_ls/
 $ ln -sf ${CEYLAN_MYRIAD}/conf/erlang_ls.config

As for the installation of `Erlang LS <https://erlang-ls.github.io/>`_ itself, we rely on:

.. code:: bash

 $ mkdir -p ~/Software && cd ~/Software
 $ git clone https://github.com/erlang-ls/erlang_ls
 $ cd erlang_ls/
 $ make
 $ mkdir bin && cd bin
 $ ln -s ../_build/default/bin/erlang_ls

Then one would just have to ensure that ``~/Software/erlang_ls/bin`` is indeed in one's PATH.

Note that not all bells and whistles of LSP may be retained, knowing that at least some of them are confused by various elements, especially when applied to code that is parse-transformed; as a result we did find LS features much useful.

The Emacs configuration that we use (see the corresponding `init.el <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/init.el>`_) attempts to find some sweet spot in this matter.

Another option is to use ``ctags`` to generate Emacs' compliant `tags <https://www.emacswiki.org/emacs/EmacsTags>`_ (see the ``generate-tags`` make target); we did not find this solution very satisfactory either.



For Documentation Generation
----------------------------

Refer now to our `dedicated HOW-TO <http://howtos.esperide.org/DocGeneration.html>`_.





Other Conventions
=================

- for clarity, we tend to use longer variable names, in CamelCase
- we tend to use mute variables to clarify meanings and intents, as in ``_Acc=[]`` (beware, despite being muted, any variable in scope that bears the same name will be matched), ``Acc`` designating accumulators
- as there is much list-based recursion, a variable named ``H`` means ``Head`` and ``T`` means ``Tail`` (as in ``[Head|Tail]``)
- the string format specifier ``~s`` shall never be used; its Unicode-aware counterpart ``~ts`` must be used instead; similarly, for string operations, ``list_to_binary/1`` and ``binary_to_list/1`` must no be used either; prefer anyway the primitives in ``text_utils``

.. See also the few hints regarding contribution_.
