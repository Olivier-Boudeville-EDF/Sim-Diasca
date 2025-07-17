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

Except in very specific cases, only ASCII code should be used (e.g. no accentuated characters).

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

- the **settings of the build chain** should be used (e.g. with regard to compiler flags) and adapted/completed if needed; the (possibly-specialised) ``GNUmakesettings.inc``,  ``GNUmakerules.inc`` and ``GNUmakevars.inc`` files should be relied upon

- **no warning should be tolerated**; anyway our build chain treats warnings as (blocking) errors

- **test cases** should be developed alongside most if not all modules; e.g. if developing ``src/foobar.erl``, then probably the ``test/foobar_test.erl`` testing code should be developed, after or, even preferably, before the implementation of the tested code; test success should be evaluated automatically, by the code (e.g. thanks to pattern matching), not by the person running the test (e.g. who would have to compare visually the actual results with the expected ones); in some cases, only **integrated tests** can be devised in practice; tests should be gathered in **test suites**, that should be runnable automatically (``make test``, recursively through child directories) and fail loudly (and in a blocking manner) at the first error met

- **multiple levels of quality documentation** should be made available to the code user, and probably be written in parallel to the code; there are at least three documentation levels:

  - lower-level documentation: code should always be **densely commented**, with documentation headers added to all functions, inlined comments (not paraphrasing the code) and self-describing symbols: function names, variable names (e.g. ``RegisteredState = ...`` to be preferred to ``NewState = ...``), etc.; more generally all names shall be long enough to be descriptive (clarity preferred over compactness); type specifications also pertain to this low-level documentation effort

  - higher-level **design and/or implementation notes**: they should be available as a set of paragraphs in each source file, before the function definitions, to describe the context and constraints, and help understanding how the features are implemented, and why

  - high-level **developer and user documentation** should be made available, targeting at least HTML and PDF outputs, possibly offering a wiki access as well

- more generally, **comments** should be clear and precise, numerous, rich and complete (overall, in terms of line counts, we target roughly 1/3 of code, 1/3 of blank lines and 1/3 of comments); all comments shall be written in UK English, start with a single ``%`` and be properly word-wrapped (use ``meta-q`` with our Emacs settings to take care of that automatically)

- **indentation** should respect, as already explained, the 80-character width and 4-space tabulation; however the default built-in Erlang indentation mode of ``emacs`` can hardly be used for that, as it leads to huge width offsets (we may in the future provide a variation of the ``elisp`` code for the emacs indentation rules)

- **spacing homogeneity** across source files should be enforced; for example three blank lines should exist between two function definitions, one between the clauses of any given function (possibly two in case of longer clauses); for the sake of "visual parsing", arguments should be separated by spaces (e.g. ``f( X ) -> ...``, not ``f(X) -> ...``), especially if they are a bit complex (``f( A={U,V}, B, _C ) -> ...``, not ``f(A={U,V},B,_C) -> ...`` or the usual ``f(A={U,V}, B, _C) -> ...``)

- for **type-related conventions**, at least all exported functions shall have a ``-spec`` declaration; if an actual type is referenced more than once (notably in a given module), a specific user-defined type shall be defined; types shall be defined in "semantic" terms rather than on technical ones (e.g. ``-type temperature() :: ...`` than ``float()``; developers may refer to, or enrich, ``myriad/src/utils/unit_utils.erl`` for that)

- the **latest stable version of Erlang** should be used, preferably built thanks to our ``myriad/conf/install-erlang.sh`` script

- the official *Programming Rules and Conventions* should be enforced, as defined `here <http://www.erlang.se/doc/programming_rules.shtml>`_ (possibly a dead link now; one may try `this mirror <https://docs.jj1bdx.tokyo/Erlang_Programming_Rules.html>`_ instead)

- the function definitions shall follow **the same order** as the one of their exports

- helper functions **shall preferably be identified as such**, with an ``(helper)`` comment

- if an helper function is specific to an exported function, it shall be defined just after this function; otherwise it should be defined in the **helper section**, placed just after the definition of the exported functions

- defining distinct (non-overlapping), explicit (with a clear-enough name), numerous (statically-defined) **atoms** is cheap; each atom found in the sources is generally to be involved in at least one type definition

- the use of ``case EXPR of ... end`` should be preferred to the use of ``if`` (never used in our code base); when only one branch may apply and does not depend on the actual value of EXPR, one-armed expressions based on ``andalso`` or ``orelse`` should be preferred to a ``case`` expression (e.g. ``DoDisplay andalso render(), ...``)

- we also prefer that the various patterns of a case are indented with exactly one tabulation, and that the closing ``end`` lies as much as possible on the left (e.g. if having specified ``MyVar = case ... end``, then ``end`` should begin at the same column as ``MyVar``); the same applies to ``try ... catch ... end`` clauses

- when a term is ignored, instead of using simply ``_``, one should define a **named mute variable** in order to provide more information about this term (e.g. ``_TimeManagerPid``); one should then to accidental matching of such names (now a warning is emitted)

- some conventional variable names are, and may be, extensively used: ``Res`` for result, ``H`` and ``T`` for respectively the head and tail of a list on which we recursively iterate

- generally, a plural variable name (e.g. ``Elements``) designates a list (e.g. ``[element()]``); consequently, a list of lists of ``element()`` (thus ``[[element()]]``, like ``[[E1,E2], [], [E3]]``) may be designated with the ``Elementss`` variable name

.. _indices:

- indices shall, as much as possible, start at index ``1`` (rather than 0); this is a general Erlang convention (`for lists <https://erlang.org/doc/man/lists.html#description>`_, like with ``lists:nth/2``, for tuples, etc. - unlike `arrays <https://erlang.org/doc/man/array.html#description>`_, though); see ``basic_utils:positive_index/0``

- when needing an **associative table**, use the ``table`` pseudo-module; a key/value pair shall be designated as a table *entry* (e.g. variable named as ``RoadEntry``)

- regarding the in-code management of **text**:

  - if a text is to be rather static (constant) and/or if it is to be exchanged between processes, then it should be a UTF8 ``binary``, and its type shall be declared as ``text_utils:bin_string()``
  - other, a plain string (``text_utils:ustring()``) shall be used

- when defining a non-trivial datastructure, a **record** shall be used (rather than, say, a mere ad-hoc tuple or a map of undocumented structure...), a corresponding **type** should be then defined (e.g. a ``foobar`` record leading to a ``foobar()`` type), and a **function to describe it** as text shall be provided (e.g. ``-spec foobar_to_string(foobar()) -> text_utils:ustring()``)

  - **mute variables** should be used as well to document actual parameters; for example ``f(3,7,10)`` could preferably be written as a clearer ``f(_Min=3,_Max=7,_Deviation=10)``


.. Note:: Mute variables are however actually bound, thus if for example there is in the same scope ``_Min=3`` and later ``_Min=4``, then a badmatch will be triggered at runtime; therefore names of mute variables should be generally kept unique in a given scope.

- as opposed to records, types shall never defined in header files (``*.hrl``): a given type shall be defined once, as a reference, and exported by its module; other modules may then just refer to it

- type shorthands may be defined; for example, if using repeatedly within a module ``text_utils:ustring()``, a local, non-exported type shorthand (``-type ustring() :: text_utils:ustring()``) may be defined so that all other uses of this type become simply ``ustring()`` in this module

As not all typos may be detected at compilation-time (e.g. wrong spelling for a module), we recommend, for source code, the use of additional static checkers, as discussed in the `type-checking`_ section.


.. _`execution target`:

Execution Targets
=================

Two execution target modes have been defined:

- ``development`` (the default): meant to simplify the task of developers and maintainers by reporting as much information and context as possible, even at the expense of some performances and reliability (e.g. no retry in case of failure, shorter time-outs not to wait too long in case of errors, more checks, etc.)
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

The `Language Server Protocol <https://en.wikipedia.org/wiki/Language_Server_Protocol>`_ (also known as LSP) may be used by one's editor of choice in order to provide various services facilitating the developments in various languages, including Erlang, thanks to `Erlang LS <https://erlang-ls.github.io/>`_.

Another option is to use ``ctags`` to generate Emacs' compliant `tags <https://www.emacswiki.org/emacs/EmacsTags>`_ (see the ``generate-tags`` make target) - however this solution is probably now superseded by Erlang LS.


Installing Erlang LS
....................

For that we rely on:

.. code:: bash

 $ mkdir -p ~/Software && cd ~/Software
 $ git clone https://github.com/erlang-ls/erlang_ls
 $ cd erlang_ls/
 $ make
 $ mkdir bin && cd bin
 $ ln -s ../_build/default/bin/erlang_ls

Then one would just have to ensure that ``~/Software/erlang_ls/bin`` is indeed in one's PATH.


Configuring  Erlang LS
......................

We then recommend to rely on our `erlang_ls.config <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/erlang_ls.config>`_ configuration file, which preferably is at the root of the project it applies to [#]_ (hence the symbolic link at this root, pointing to the actual file in the ``conf`` subdirectory).

.. [#] Rather than being centralised in the *user* configuration directory, typically in ``~/.config/erlang_ls/erlang_ls.config``.


.. It may be installed that way:

.. .. code:: bash

..  $ mkdir -p ~/.config/erlang_ls && cd ~/.config/erlang_ls/
..  $ ln -sf ${CEYLAN_MYRIAD}/conf/erlang_ls.config

As we understand when reading the `Erlang LS documentation <https://erlang-ls.github.io/configuration/>`_, in this YAML-based ``erlang_ls.config``:

- the current project is referenced by ``apps_dirs``, whose default value must contain ``.``; hence nothing needs to be done to designate our project
- our convention being that all layers above Myriad are expected to be found as sibling directories (e.g. ``wooper`` having the same parent directory as ``myriad``), possibly as symbolic links, in order to designate a prerequisite of the current layer it should be enough to include, in the ``deps_dirs`` entry, a relative path to that specific directory (e.g. ``../wooper``)


Using Erlang LS
...............

Note that not all bells and whistles of LSP may be retained, knowing that at least some of them are confused by various elements, especially when applied to code that is parse-transformed (as most tools operate on sources rather than on BEAM files); as a result, we did not find all LS features useful.

The Emacs configuration on which we rely (see the corresponding `init.el <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/init.el>`_) attempts to find some sweet spot in this matter.



For Documentation Generation
----------------------------


Generation of API documentation
...............................

Since Erlang/OTP 27, Myriad relies on the `overhauled documentation system <https://www.erlang.org/doc/system/documentation.html>`_ (stemming from `EEP 59 <https://www.erlang.org/eeps/eep-0059>`_) and on the `Markdown <https://en.wikipedia.org/wiki/Markdown>`_ syntax.

This produces doc chunks, and `ExDoc <https://hexdocs.pm/ex_doc/readme.html>`_ is used (as a command-line tool) to generate the actual documentation out of it.

As ExDoc is in Elixir, it is to be installed thanks to ``mix``, which can be installed on Arch thanks to ``pacman -S elixir``.

Then ExDoc can be installed as an escript: ``mix escript.install hex ex_doc``; it becomes then available as ``~/.mix/escripts/ex_doc``, that may be added in one's ``PATH``. Refer to our ``generate-api-doc`` make target that automates the generation of the API documentation of the current layer.


Writing API documentation
.........................

Short reminders for the writing of a proper corresponding documentation (see also the `Erlang reference guide <https://www.erlang.org/doc/system/documentation.html>`_ about it):

- the documentation regarding an element must come just *before* that element
- for each module file, first comes a ``-moduledoc`` (module-level) attribute
- then as many ``-doc`` as there are elements that shall be documented: user-defined types (for ``-type`` and ``-opaque``), behaviour module attributes (``-callback``) and functions (``-spec``)
- each of these documentation attributes (``-moduledoc`` / ``-doc``) can be followed by a single-quoted or a `triple-quoted string <https://www.erlang.org/blog/highlights-otp-27/#triple-quoted-strings>`_; this entry should start with a short paragraph describing the purpose of the documented element, and then go into greater detail if needed; we recommend the MarkDown syntax for it (see `this reference <https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax>`_); for example:


.. code:: erlang

 -moduledoc """
 A module for **basic arithmetic**.

 It is based on *XXX* and performs *YYY*.

 ZZZ is of interest, see [this page](http://www.foobar.org).
 See `sub/2`, <http://www.foobar.org#hello> and `arith:sub/2` for more details.
 """.


and

.. code:: erlang

 -doc "Adds two numbers."

(note that both simple and triple quotes *must* be followed by a dot)

Let's name an *element specification* the documentation attribute (``-doc``), possibly its type spec (``-spec``) and its actual (code-based) definition.

We recommend that:

- element specifications are separated by three blank lines
- no blank line exists between a document attribute and the rest of the corresponding element specification


.. comment For pick and paste:

  -doc ".".

  -doc """

   """.



For other documentation topics, refer to our `dedicated HOW-TO <http://howtos.esperide.org/DocGeneration.html>`_.



Release Conventions
===================

These conventions apply to the release of any Myriad-based package, i.e. either Myriad itself or packages depending, directly or not, from it.

The recommended procedure is (while being at the root of a clone):

#. ensure that your version of Erlang (see `install-erlang.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/install-erlang.sh>`_), of rebar3 (see `install-rebar3.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/install-rebar3.sh>`_) and possibly of erlang_ls (see `this section <https://howtos.esperide.org/Emacs.html#regarding-erlang>`_) are up to date
#. merge all new developments in the ``master`` (or ``main``) branch
#. possibly update dependencies, then:

   - in the corresponding ``GNUmakevars.inc`` settings
   - in any ``priv/bin/deploy-*-native-build.sh`` script
   - in ``conf/rebar.config.template``; in which case then run ``make set-rebar-conf``
#. in ``GNUmakevars.inc``:

   - ensure that all debug/check flags (like, for Myriad: ``MYRIAD_DEBUG_FLAGS += -Dmyriad_debug_code_path``) are disabled, and that non-release elements (e.g. ``MYRIAD_LCO_OPT``) and optional ones are disabled as well
   - bump the version of this local package (e.g. in ``MYRIAD_VERSION``)

#. for packages having dependencies: upgrade their reference known of rebar3, with ``make rebar3-upgrade-lock``
#. rebuild and test all from the root: ``make rebuild test``, fix any problem
#. optional: perform `static code checking <#type-checking-myriad>`_
#. recommended: update the documentation: ``cd doc && make export-doc``; check the result (`example for Myriad <https://myriad.esperide.org/>`_; this includes ensuring that no error is displayed `at the bottom <https://myriad.esperide.org/#myriad_bottom>`_ of the page, and that the `corresponding PDF <https://myriad.esperide.org/Ceylan-Myriad-Layer-technical-manual-english.pdf>`_ is well-formed and has a proper table of contents)
#. if all went well, ensure that all files are committed (including ``ebin/THIS_PACKAGE.app`` and ``rebar.lock``)
#. push them, it will trigger the CI/CD services; ensure that everything is correct there as well
#. go back to a development branch and merge/rebase the master/main one there



Other Conventions
=================

- for clarity, we tend to use longer variable names, in CamelCase
- we tend to use mute variables to clarify meanings and intents, as in ``_Acc=[]`` (beware, despite being muted, any variable in scope that bears the same name will be matched), ``Acc`` designating accumulators
- as there is much list-based recursion, a variable named ``H`` means ``Head`` and ``T`` means ``Tail`` (as in ``[Head|Tail]``)
- the string format specifier ``~s`` shall never be used; its Unicode-aware counterpart ``~ts`` must be used instead; similarly, for string operations, ``list_to_binary/1`` and ``binary_to_list/1`` must no be used either; prefer anyway the primitives in ``text_utils``

.. See also the few hints regarding contribution_.
