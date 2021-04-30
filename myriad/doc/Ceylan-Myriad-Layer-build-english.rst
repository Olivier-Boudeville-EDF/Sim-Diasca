
:raw-latex:`\pagebreak`

.. _`build structure`:


General Build Structure
=======================

Various elements are defined at the ``myriad`` level to set-up an appropriate build, based on `GNU Make <http://www.gnu.org/software/make/manual/make.html>`_.

This includes:

- a set of pre-defined Make **variables**, describing various settings that will be reused by generic rules (ex: to compile modules with relevant flags, to create source archives, to install an application, to manage the various paths, to perform test checking, to generate archives, installs and releases, etc.); these variables are defined in ``myriad/GNUmakevars.inc``

- a set of generic **rules**, to compile and run various modules and tests, to generate various elements of documentation, etc.; these rules are defined (still from the ``myriad`` root directory), in:

  - ``GNUmakerules-automatic.inc``, for all rules that apply generically to a some kinds of targets
  - ``GNUmakerules-explicit.inc``, for all "direct" rules, that are not pattern-based
  - ``doc/GNUmakerules-docutils.inc``, for all documentation-related rules

- finally, the whole is gathered in a unique file to include, ``GNUmakesettings.inc``, whose structure allows for a safe and sound combination of all these build element across a series of layers (the first of which being Myriad)

- **examples** of minimal Make files, which mostly specify the relative base path and only refer to the generic variables and rules; see ``myriad/src/GNUmakefile`` as an example

An example of this stacked structure is the ``Ceylan-WOOPER`` layer (see `official site <http://wooper.esperide.org>`_), which is directly built on top of ``Ceylan-Myriad`` (and itself a base layer for other layers and applications).

These build facilities are designed to be enriched in turn by all layers above, which may add or override variables and rules.

As an example, the ``shell`` make target allows to spawn an Erlang shell that is readily able to use the current layer and all the ones below (including thus Myriad); this way direct, hassle-free interactive testing can be performed - which may prove quite convenient.
