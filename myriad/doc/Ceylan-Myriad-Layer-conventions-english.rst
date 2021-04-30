:raw-latex:`\pagebreak`

.. _`main conventions`:


``Myriad`` Main Conventions
===========================


Text Conventions
----------------

The purpose here is to ensure a sufficient **code homogeneity**; for example in all source files are in such a "canonical form", analysing their differences (``diff``) is made simpler.

Any text editor can be used, provided that it saves source files with the UNIX, not DOS, conventions (i.e. lines terminating by the LF character, not by the CRLF characters).

The use of syntax highlighting is encouraged.

Recommended text editors are:

- emacs / xemacs
- ErlIDE (based on Eclipse)
- gedit
- nedit

Source files should be formatted for a 80-character width: no character should be present after the 79th column of a line.

Except in very specific cases, only ASCII code should be used (ex: no accentuated characters).

Tabulations should be preferred to series of spaces, and the text should be formatted according to 4-character tabulations.

All redundant whitespaces should be removed, preferably automatically (see the Emacs ``whitespace-cleanup`` command). This is why, with the `emacs settings`_ that we recommend, pressing the F8 key removes for example the yellow areas in the current buffer by replacing any series of four spaces by a corresponding tabulation.

We would prefer that contributed files (especially source ones) are "whitespace-clean" before being committed. As mentioned, such a transformation can be done directly from Emacs. If using another editor, please ensure that the `fix-whitespaces.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/fix-whitespaces.sh>`_ script has been run on the target sources (possibly automatically thanks to a VCS hook) *before* committing them.

All elements of documentation should be written in English, possibly translated to other languages. Spell-checking is recommended.



Coding Practices
----------------

In terms of coding style, we would like that the sources remain as uniform as possible, regarding naming, spacing, code/comments/blank line ratios.

We would like that, roughly and on average, the same ratio applies for blank lines, comments and lines of code.

For that one may use the either directly ``make stats`` from the root of the layer or the `make-code-stats.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/make-code-stats.sh>`_ script.

For example::

  In the Erlang source code found from XXX/ceylan/myriad, we have:
  + 160 source files (*.erl), 54 header files (*.hrl)
  + a grand total of 89151 lines:
	- 27186 of which (30.4%) are blank lines
	- 28751 of which (32.2%) are comments
	- 33214 of which (37.2%) are code


As not all typos may be detected at compilation-time (ex: wrong spelling for a module), we recommend, for source code, the use of additional static checkers, as discussed in the `type-checking`_ section.



Execution Targets
-----------------

Two execution target modes have been defined:

- ``development`` (the default): meant to simplify the task of developers and maintainers by reporting as much information and context as possible, even at the expense of some performances and reliability (ex: no retry in case of failure, shorter time-outs not to wait too long in case of errors, more checks, etc.)
- ``production``: mostly the reciprocal of the ``development`` mode, whose purpose is to favor efficient, bullet-proof operations

These execution targets are *compile-time* modes, i.e. they are set once for all when building the layer at hand (probably based, if using OTP, on the rebar corresponding modes - respectively ``dev`` and ``prod``).

See ``EXECUTION_TARGET`` in ``GNUmakevars.inc`` to read and/or set them.

The current execution target is of course available at runtime on a per-layer level, see ``basic_utils:get_execution_target/0`` for more information.

This function shall be compiled once per layer to be accurate, in one of its modules. It is just a matter of adding the following include in such module::

 -include_lib("myriad/utils/basic_utils.hrl").



Other Conventions
-----------------

- for clarity, we tend to use longer variable names, in CamelCase
- we tend to use mute variables to clarify meanings and intents, as in ``_Acc=[]`` (beware, despite being muted, any variable in scope that bears the same name will be matched), ``Acc`` designating accumulators
- as there is much list-based recursion, a variable named ``H`` means ``Head`` and ``T`` means ``Tail`` (as in ``[Head|Tail]``)
- the string format specifier ``~s`` shall never be used; its Unicode-aware counterpart ``~ts`` must be used instead; similarly, for string operations, ``list_to_binary/1`` and ``binary_to_list/1`` must no be used either; prefer anyway the primitives in ``text_utils``

.. See also the few hints regarding contribution_.
