========================================================
Technical Manual of the **Sim-Diasca** Simulation Engine
========================================================


.. comment the Installation and User/Developer Guides are other documents now.


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. Unable to have proper table formatting without specialising:


.. Scale does not seem taken into account:
:raw-html:`<center><table><tr colspan="2"><center><img src="sim-diasca.png" style="width:500px"></tr><tr><td><center><img src="logo-EDF-english.png" style="width:150px"></td><td><center><img src="lgpl-v3-logo-bordered.png" style="width:140px"></td></tr></table>`


:raw-latex:`\begin{center} \begin{tabular}{c c c} \multicolumn{2}{c}{\includegraphics[scale=0.3]{sim-diasca.png}} \\  \includegraphics[scale=0.4]{logo-EDF-english.png} & \includegraphics[scale=0.8]{lgpl-v3-logo-bordered.png} \\ \end{tabular} \end{center}`



:Organisation: Copyright (C) 2008-2021 EDF R&D
:Author: Olivier Boudeville
:Contact: olivier.boudeville@edf.fr
:Lastly updated: Friday, February 19, 2021
:Creation date: Monday, February 8, 2010
:Status: Work in progress
:Version: 0.9.25
:Website: `http://sim-diasca.com <http://sim-diasca.com>`_
:Dedication:

	For people interested in the inner workings of the `Sim-Diasca` simulation engine.

:Abstract:

	The main design choices of the simulation engine are discussed here, from the requirements to the implementation, including the software architecture and the algorithmic approach. Its recommended use is also described.


:raw-html:`</center>`


.. meta::
   :keywords: Sim-Diasca, massive, simulation, multi-agent, development



:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 2

.. section-numbering::





:raw-latex:`\pagebreak`



.. Note::

  Before reading this document, we strongly advise to have a look first at the slides of the general-purpose presentation of Sim-Diasca.



------------------
Overview & Context
------------------


Sim-Diasca
==========

**Sim-Diasca** stands for `Simulation of Discrete Systems of All Scales`.

Sim-Diasca is a lightweight simulation platform, released by EDF R&D under the GNU LGPL_ licence, offering a set of simulation elements, including notably a simulation engine, to be applied to the simulation of discrete event-based systems made of potentially very numerous interacting parts.

This class of simulation encompasses a wide range of target systems, from ecosystems to information systems, i.e. it could be used for most applications in the so-called `Complex systems <http://en.wikipedia.org/wiki/Complex_systems>`_ scientific field.

As a matter of fact, a classical use case for Sim-Diasca is the simulation of industrial distributed systems federating a large number of networked devices.

The simulation elements provided by Sim-Diasca are mainly *base models* and *technical components*.

**Models** are designed to reproduce key behavioural traits of various elements of the system, notably business-specific objects, regarding a specific concern, i.e. a matter on which the simulator must provide an answer. An instance of a model is called here an *actor*. A simulation involves actors to be driven by a specific scenario implemented thanks to a *simulation case*.

Sim-Diasca includes built-in base models that can be further specialized to implement the actual business objects that are to be simulated.

**Technical components** allow the simulator to operate on models, so that their state and behaviour can be evaluated appropriately, in the context of the execution of an actual simulation.

Depending on the conventions that models and technical components respect, different properties of the simulation can be expected.

Thus the first question addressed in the context of Sim-Diasca has been the specification of the simulation needs that should be covered, i.e. its functional requirements.

Then, from these requirements, a relevant set of technical measures has been determined and then implemented.




Current Status
==============

Sim-Diasca and the first simulators making use of it are works in progress since the beginning of 2008, and the efforts dedicated to them remained light yet steady.

The Sim-Diasca engine is already fully functional [#]_ on GNU/Linux platforms, and provides all the needed basic underlying simulation mechanisms to model and run full simulations on various hardware solutions.

A set of models, specifically tied to the first two business projects making use of Sim-Diasca, have been developed successfully on top of it, and the corresponding business results were delivered appropriately to stakeholders.

.. [#] This is also a great pun, as the simulator is implemented thanks to the `Erlang <http://www.erlang.org>`_ language which, in terms of programming paradigm, is a *functional* language.

Some further enhancements to the Sim-Diasca engine are to be implemented (see `Sim-Diasca Future Enhancements`_).



Minimal Quick-Start For Early Technical Testing
===============================================

For the fearless users wanting an early glance at Sim-Diasca in action, here is the shortest path to testing, provided you already have the prerequisites (including a well-compiled, recent, Erlang interpreter on a GNU/Linux box) installed.

First, download the latest stable archive, for example: ``Sim-Diasca-x.y.z.tar.bz2``.

Extract it: ``tar xvjf Sim-Diasca-x.y.z.tar.bz2``.

Build it: ``cd Sim-Diasca-x.y.z && make all``.

Select your test case: ``cd sim-diasca/src/core/src/scheduling/tests/``, for example: ``scheduling_multiple_coupled_erratic_actors_test.erl``.

Optionally, if you want to run a *distributed* simulation, create in the current directory a file named ``sim-diasca-host-candidates.txt`` which lists the computing hosts you want to take part to the simulation, with one entry by line, like (do not forget the final dot on each line)::

  {'hurricane.foobar.org', "Computer of John (this is free text)."}.

Then run the test case::

  make scheduling_multiple_coupled_erratic_actors_run CMD_LINE_OPT="--batch"

The ``CMD_LINE_OPT="--batch"`` option was added as, for the sake of this quick-start, the trace supervisor is not expected to have already been installed.

For the more in-depth reference installation instructions, refer to the ``Sim-Diasca Installation Guide``.



-----------------------
How to Read This Manual
-----------------------

:raw-html:`<center><center><img src=xkcd-manuals.png></img></center>`
:raw-latex:`\includegraphics[scale=0.75]{xkcd-manuals.png}`


.. include:: SimDiasca-ontology-english.rst

.. include:: SimDiasca-specifications-english.rst

.. include:: SimDiasca-functional-coverage-english.rst

.. include:: SimDiasca-modelling-english.rst

.. include:: SimDiasca-simulation-services-overview-english.rst

.. include:: SimDiasca-time-management-english.rst

.. include:: SimDiasca-random-management-english.rst

.. include:: SimDiasca-input-management-english.rst

.. include:: SimDiasca-result-management-english.rst

.. include:: SimDiasca-reliability-english.rst

.. include:: SimDiasca-technical-architecture-english.rst


.. include:: SimDiasca-building-blocks-english.rst

.. include:: SimDiasca-helper-tools-english.rst



.. comment Non-existing yet: .. include:: SimDiasca-guided-tour-english.rst


.. comment Merged in modeller guide now: .. include:: SimDiasca-implementation-review-english.rst


.. include:: SimDiasca-modelling-instructions-english.rst

.. include:: SimDiasca-simulation-validation-english.rst

.. Moved to a separate document:
  .. include:: SimDiasca-installation-instructions-english.rst

.. include:: SimDiasca-cheat-sheet-english.rst

.. include:: SimDiasca-troubleshooting-english.rst

.. include:: SimDiasca-support-english.rst


.. commented out, too verbose and not useful enough: .. include:: SimDiasca-changes-english.rst


------------------
Sim-Diasca Changes
------------------

The detailed changes over versions are not included in this manual anymore, as they used to spread over too many pages, for little interest.

Please refer directly to the ``SimDiasca-changes-english.rst`` file instead.


.. include:: SimDiasca-future-enhancements-english.rst

.. include:: SimDiasca-hints-english.rst

.. include:: SimDiasca-bibliography-english.rst

.. include:: SimDiasca-credits-english.rst

.. include:: SimDiasca-license-english.rst

.. include:: SimDiasca-contributing-english.rst

.. include:: SimDiasca-footer-english.rst

:raw-latex:`\pagebreak`
