---------------------------
Sim-Diasca Code Conventions
---------------------------


Foreword
========

At all levels of the technical architecture, we tried to enforce a few conventions, that are detailed below.

Some of them are necessary, others are mere good practices, a few are arbitrary (notably in terms of style), but we believe that, for the sake of clarity and homogeneity, all of them should be respected in the code of the Sim-Diasca stack, and preferably also in code using it (typically models, tools, etc.).


:raw-html:`<img src="xkcd-code_quality.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-code_quality.png}`

(see the credits_ section about the comic strips)


We believe also that these conventions have been fairly well enforced in our own Sim-Diasca code base (which thus might be used as an example thereof). Please tell us if you do not think so, or if you identified interesting other conventions that could be listed here and applied.


:raw-html:`<img src="xkcd-donald_knuth.png"></img>`
:raw-latex:`\includegraphics[scale=4]{xkcd-donald_knuth.png}`


.. Finally duplicated with Myriad, so that we can still use XKCD here.
.. For all basic conventions that are transverse to at least all the Erlang developments that we do, we rely on the `main conventions <https://olivier-boudeville.github.io/Ceylan-Myriad/#main-conventions>`_ that the Ceylan-Myriad project defined.


Text Conventions
================

Any text editor can be used, provided that it saves source files with the UNIX, not DOS, conventions (i.e. lines terminating by the LF character, not by the CRLF characters).

The use of syntax highlighting is encouraged.

Recommended text editors are:

- emacs / xemacs
- nedit
- ErlIDE (based on Eclipse)
- gedit


:raw-html:`<img src="xkcd-real_programmers.png"></img>`
:raw-latex:`\includegraphics[scale=0.45]{xkcd-real_programmers.png}`

Source files should be formatted for a 80-character width: no character should be present after the 79th column of a line.

Tabulations should be preferred to series of spaces, and the text should be formatted according to 4-character tabulations.

All redundant whitespaces should be removed, preferably automatically (see the Emacs ``whitespace-cleanup`` command). This is why, on Emacs, with our settings, pressing the F8 key removes for example the yellow areas in the current buffer by replacing any series of four spaces by a corresponding tabulation.

For that, with emacs, in addition to our ``init.el``, we rely on ``acme-search.el``, ``flyspell-guess.el`` and ``whitespace.el`` (all in the ``~/.emacs.d`` directory - check that no other initialisation file collide, like a stray ``~/.emacs`` file), and it leads to a display like:

:raw-html:`<img src="emacs-and-code.png"></img>`
:raw-latex:`\includegraphics[scale=0.55]{emacs-and-code.png}`



Only ASCII code should be used (ex: no accentuated characters).

All elements of documentation should be written in English, possibly translated to other languages.

Spell-checking is recommended; ensure you typed everything properly:

:raw-html:`<img src="xkcd-the_important_field.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-the_important_field.png}`


As not all typos may be detected at compilation-time (ex: wrong spelling for a module), we recommend the use of additional static checkers, as discussed `here <https://myriad.esperide.org/#type-checking-myriad>`_.





:raw-latex:`\pagebreak`

General View of the Software Stack
==================================

We see Sim-Diasca as a stack of layers, so that a given layer only depends on the ones below it, and never from the ones above.

Top-to-bottom, we have:

 =============== ==================================================
 Layer Name      Role
 =============== ==================================================
 Sim-Diasca      This simulation engine
 Ceylan-Traces   The distributed trace system
 Ceylan-WOOPER   The object-oriented layer
 Ceylan-Myriad   The base library offering general-purpose services
 Erlang          The base language and environment
 =============== ==================================================

Thus, there is not upward dependency, for example WOOPER depends on Myriad and Erlang but not on Traces or on Sim-Diasca.

The other way round, bottom-up one can see:

- `Erlang <https://www.erlang.org/>`_, which provides the way of defining and running concurrently a large number of processes
- `Myriad <https://olivier-boudeville.github.io/Ceylan-Myriad/>`_, which gathers all common services that are needed, in terms of data-structures, lower-level constructs, most frequent processings, etc.
- `WOOPER <https://olivier-boudeville.github.io/Ceylan-Myriad/>`_, which transforms Erlang processes into instances of classes with multiple inheritance, still running concurrently
- `Traces <https://olivier-boudeville.github.io/Ceylan-Myriad/>`_, which allows each distributed instance to send appropriate traces
- `Sim-Diasca <https://olivier-boudeville-edf.github.io/Sim-Diasca/>`_, which transforms a distributed object-oriented application into a simulation


On top of that stack, which provides the simulation engine, there are at least:

- a set of models integrated into the Sim-Diasca framework
- a simulation case, which makes use of these models and organises them in the context of a scenario to be simulated, a virtual experiment


The simulation engine being itself absolutely generic as long as discrete-time simulations are involved, it may be convenient to define, on top of Sim-Diasca and below the actual models themselves, a domain-specific layer that specialises the framework in order to ease the development of models.

For example, a telecom-centric simulation could define building blocks like service queues, and mother classes like communicating nodes, network interfaces, packet loss models, etc.




:raw-latex:`\pagebreak`


Erlang Conventions
==================

:raw-html:`<img src="xkcd-parenthesis.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{xkcd-parenthesis.png}`


The most obvious conventions are:

- the **settings of the build chain** should be used (ex: with regard to compiler flags) and adapted/completed if needed; the (possibly-specialised) ``GNUmakesettings.inc``,  ``GNUmakerules.inc`` and ``GNUmakevars.inc`` files should be relied upon

- **no warning should be tolerated**; anyway now our build chain treats warnings as (blocking) errors

- **test cases** should be developed alongside most if not all modules; ex: if developing ``class_X.erl``, then probably the ``class_X_test.erl`` testing code should be developed, after or, preferably, before the implementation of the tested code; test success should be evaluated automatically, by the code (ex: thanks to pattern matching), not by the person running the test (ex: who would have to compare visually the actual results with the expected ones); in some cases, only **integrated tests** can be devised in practice; tests should be gathered in **test suites**, that should be runnable automatically (``make test``) and fail loudly (and in a blocking manner) at the first error met

- **multiple levels of quality documentation** should be made available to the code user, and probably be written in parallel to the code; there are at least three documentation levels:

  - lower-level documentation: code should always be **densely commented**, with headers added to all functions, inlined comments (not paraphrasing the code) and self-describing symbols: function names, variable names (ex: ``RegisteredState=...`` to be preferred to ``NewState=...``), etc.; more generally all names shall be long enough to be descriptive (clarity preferred over compactness); type specifications also pertain to this low-level documentation effort

  - higher-level **design and/or implementation notes**: they should be available as a set of paragraphs in each source file, before the function definitions, to help understanding how the features are implemented, and why

  - high-level **developer and user documentation** should be made available, targeting at least a PDF output, possibly offering a wiki access as well

- more generally, **comments** should be clear and precise, numerous, rich and complete (overall, in terms of line counts, we target roughly 1/3 of code, 1/3 of blank lines and 1/3 of comments); all comments shall be written in UK English, start with a single ``%`` and be properly word-wrapped (use ``meta-q`` with our Emacs settings)

- **indentation** should respect, as already explained, the 80-character width and 4-space tabulation; however the default built-in Erlang indentation mode of ``emacs`` can hardly be used for that, as it leads to huge width offsets (the ``elisp`` code for the emacs indentation will be modified for our need, in the future); the Sim-Diasca conventional indentation should be enforced, preferably automatically (ex: thanks to ``emacs``)

- **spacing homogeneity** across Sim-Diasca source files should be enforced; for example three blank lines should exist between two function definitions, one between the clauses of any given function (possibly two in case of longer clauses), arguments should be separated by spaces (ex: ``f( X ) -> ...``, not ``f(X) -> ...``), especially if they are a bit complex (``f( A={U,V}, B, _C ) -> ...``, not ``f(A={U,V},B,_C) -> ...``)

- see the `Using Type Specifications With Sim-Diasca`_ section for **type-related conventions**; at least all exported functions shall have a ``-spec`` declaration; if an actual type is referenced more than once (notably in a given module), a specific user-defined type shall be defined; types shall be defined in "semantic" terms rather than on technical ones (ex: ``-type temperature() :: ...`` than ``float()``); developers may refer to, or enrich, ``myriad/src/utils/unit_utils.erl`` for that

- the **latest stable version of Erlang** should be used, preferably built thanks to our ``myriad/conf/install-erlang.sh`` script

- the official *Programming Rules and Conventions* should be enforced, as defined `here <http://www.erlang.se/doc/programming_rules.shtml>`_

:raw-html:`<img src="xkcd-functional.png"></img>`
:raw-latex:`\includegraphics[scale=0.7]{xkcd-functional.png}`

- the function definitions shall follow **the same order** as the one of their exports

- helper functions **shall be identified as such**, with an ``(helper)`` comment; the same stands for all other kinds of functions mentioned in next sections

- if an helper function is specific to an exported function, it shall be defined just after this function; otherwise it should be defined in the **helper section**, placed just after the definition of the exported functions

- defining distinct (non-overlapping), explicit (with a clear enough name), numerous (statically-defined) **atoms** is cheap; they are generally to be involved in at least one type definition

- the use of ``case ... of ... end`` should be preferred to the use of ``if`` (never used in our code base)

- we also prefer that the various patterns of a case are indented with exactly one tabulation, and that the closing ``end`` lies as much as possible on the left (ex: if having specified ``MyVar = case ... end``, then ``end`` should begin at the same column as ``MyVar``); the same applies to ``try ... catch ... end`` clauses

- when a term is ignored, instead of using simply ``_``, one should define a **named mute variable** in order to provide more information about this term (ex: ``_TimeManagerPid``); one should then to accidental matching of such names

- some conventional variable names are, and may be, extensively used: ``Res`` for result, ``H`` and ``T`` for respectively the head and tail of a list

- when needing an **associative table**, use the ``table`` pseudo-module; if needing to store such an instance in an attribute, its name shall be suffixed with ``_table`` (ex: ``road_table``); a key/value pair shall be designated as a table *entry* (ex: variable named as ``RoadEntry``)

- regarding **text**:

 - if a text is to be rather static (constant) and/or if it is to be exchanged between processes, then it should be a ``binary``, and its type shall be declared as ``text_utils:bin_string()``
 - other, a plain string (``string()``) shall be used

- when defining a non-trivial datastructure, a **record** shall be used (rather than, say, a mere ad-hoc tuple), a corresponding **type** should be then defined (ex: a ``foobar`` record leading to a ``foobar()`` type), and a **function to describe it** as text shall be provided (ex: ``-spec foobar_to_string(foobar()) -> string()``)

 - **mute variables** should be used as well to document actual parameters; for example ``f(3,7,10)`` could preferably be written as a clearer ``f(_Min=3,_Max=7,_Deviation=10)``


.. Note:: Mute variables are however actually bound, thus if for example there is in the same scope ``_Min=3`` and later ``_Min=4``, then a badmatch will be triggered at runtime; therefore names of mute variables should be generally kept unique in a given scope.


For the sake of clarity, we try to avoid too compact code, and code too poorly understandable for everyone but its original creator. Thus we want to enforce a minimum ratio of blank lines and comments.

For example, as of May 2017, we have for the Sim-Diasca stack (i.e. from ``myriad`` to ``sim-diasca``):

- 326 source files (``*.erl``), 86 header files (``*.hrl``)
- a grand total of 178980 lines:
 - 57814 of which (32.3%) are blank lines
 - 56548 of which (31.5%) are comments
 - 64618 of which (36.1%) are code

These information can be obtained by running ``make stats`` from the root of a Sim-Diasca install.


Other recommended good practices are:

- **peer review**: before committing code, or before issuing a release, it should be reviewed by someone who is not the one who introduced the corresponding changes

- write **type specifications** and run regularly **Dialyzer**


:raw-html:`<img src="xkcd-good_code.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-good_code.png}`


Another piece of advice we maybe should apply more frequently:

:raw-html:`<img src="xkcd-optimization.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-optimization.png}`


 Of course we cannot stress enough that securing a sufficient code quality is essential for the other developers to come, and also often even for one's future self; for that reason we recommend pair-programming, or at least the aforementioned review before new sources are incorporated in the code base:

:raw-html:`<img src="xkcd-code_quality_3.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-code_quality_3.png}`




:raw-latex:`\pagebreak`


Myriad Conventions
==================

The general goal is to collect recurring generic lower-level patterns and solutions in that layer.

When an helper mechanism is already available in ``Myriad``, it should be used instead of being defined multiple times in the software stack.

Reciprocally, when a well-defined generic sequence of instructions is used more than once, it should be integrated (commented and tested) in that ``Myriad`` layer.

Main services are:

- support of generic data-structures, like hashtables, option lists, etc. (in ``data-management``)
- some helpers for GUI programming (in ``user-interface``)
- support of some math-related operations, mostly linear (in ``maths``)
- various helpers, for system-related operations, text management, network operations, executable support, unit management, etc. (in ``utils``)

For further information, please refer to the **Technical Manual of the Myriad Layer** [#]_.

.. [#] It can be generated by running ``make full-doc`` from ``myriad/doc``.



WOOPER Conventions
==================

One should respect the WOOPER conventions (please refer to the `WOOPER <http://ceylan.sourceforge.net/main/documentation/wooper/>`_ documentation).

For example,  ``oneway``, ``request`` or ``helper function`` (possibly with qualifiers like ``const``) should be specified in each method header.

WOOPER type conventions shall be used as well, for example::

 -spec getFoo( wooper:state() ) -> request_return( foo() ).


Method names should be spelled in ``CamelCase`` (ex: ``getColorOf``, not ``get_color_of``).

Variables bound to a WOOPER state shall have their name suffixed with ``State``; ex: ``NewState``, ``UpdatedState``, etc.

To better discriminate between methods and functions (ex: helpers):

- the latter shall have their name spelled in ``snake_case`` (ex: ``update_table``, not ``updateTable``)
- should an helper function have among its parameters the state of an instance (type: ``wooper:state()``; typically either to access directly to attributes or to use trace primitives), this parameter should be listed last (ex: ``update_table(X,Table,State)`` instead of ``update_table(State,X,Table)``), so that helpers can be more easily discriminated from member methods, which have such a state as first parameter
- all class-specific attributes shall be documented in a specific section, before the constructor, describing: their name, type, meaning and role


Example::

 % The class-specific attributes of an actor instance are:
 %
 % - actor_abstract_id :: aai() corresponds to this actor abstract
 % identifier (AAI), as assigned by the load balancer
 %
 % - ....



:raw-latex:`\pagebreak`

Traces Conventions
==================

So that log messages can be kept track of over time, a distributed trace system is provided, with relevant components: trace emitter, listener, supervisor, aggregator, etc.

At implementation time, one just has to choose:

- the trace channel on which the trace should be sent, among: ``fatal``, ``error``, ``warning``, ``info``, ``trace``, ``debug`` (from highest priority to lowest)
- if a constant message is to be sent (ex: ``?warning("This is a static message")``), or if it is determined at runtime (ex: ``?warning_fmt("There are ~B apples.",[Count])``)
- if the trace is sent from a method (ex: ``?info("Hello")``) or from a constructor (ex: ``?send_info(MyState,"Hello")`` where ``MyState`` is a state returned, directly or not, by a ``class_TraceEmitter`` constructor)
- there are other, less commonly used, information that can be specified, as the categorisation (``_cat`` variations, like in ``?debug_cat("Hello","core.greetings")``), additional timing information (``_full`` variations, like in ``?trace_full("Bye","core.greetings",_Tick=121)``), etc.


Once building the simulator, one can configure:

- whether traces should be disabled or enabled (the default), by commenting-out appropriately ``-define(TracingActivated,).`` in ``class_TraceEmitter.hrl``
- if enabled, what kind of trace output will be generated, among LogMX-compliant (a third-party log supervisor integrating a trace parser of our own), PDF output, or raw text output; this is to be set in ``traces.hrl`` (default is: LogMX-compliant)


 At execution time, the command-line option ``--batch`` can be specified, which causes all interactive elements to be disabled, including any trace supervisor (like the LogMX browser). It can be specified that way::

   $ make my_case_run CMD_LINE_OPT="--batch"

For convenience, often the developer defines in his shell environment file ``export BATCH='CMD_LINE_OPT="--batch"'``, so that he can make use of this shorthand instead::

   $ make my_case_run $BATCH

(this is very convenient when debugging brand new code: before having to peer at the traces, runtime errors may occur, and the relevant information, like the stack trace, the actual parameters and the current instance state will be displayed on the console)

The trace system has been designed with performance and scalability in mind, thus if disabled no per-instance overhead penalty will be incurred at all, and, if enabled, efforts have been made so that as large as possible a number of traces are to be managed by the trace system, for any given resources in terms of network, memory and processing.

Should numerous traces be sent, it could be interesting to create more than one instance of the trace aggregator class, for example:

- one for the technical traces, to ease lower-level debugging
- one for the model-centric traces, to ease the debugging of the behaviours and interactions of actors

An overloaded trace aggregator will notify the user, so that the verbosity of later runs can be decreased.

.. include:: SimDiasca-model-conventions-english.rst
