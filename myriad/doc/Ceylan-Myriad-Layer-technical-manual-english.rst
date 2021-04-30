.. _Top:


.. title:: Welcome to the Ceylan-Myriad documentation

.. comment stylesheet specified through GNUmakefile


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


:raw-html:`<a name="myriad_top"></a>`

:raw-html:`<div class="banner"><p><em>Myriad documentation</em> <a href="http://myriad.esperide.org">browse latest</a> <a href="https://olivier-boudeville.github.io/Ceylan-Myriad/index.html">browse mirror</a> <a href="Ceylan-Myriad-Layer-technical-manual-english.pdf">get PDF</a> <a href="#myriad_top">go to top</a> <a href="#myriad_bottom">go to bottom</a> <a href="https://github.com/Olivier-Boudeville/Ceylan-Myriad">go to project</a> <a href="mailto:about(dash)myriad(at)esperide(dot)com?subject=[Ceylan-Myriad]%20Remark">email us</a></p></div>`



:raw-html:`<center><img src="myriad-title.png" width="40%"></img></center>`
:raw-latex:`\centering \includegraphics[scale=0.15]{myriad-title.png}`



===============================================
Technical Manual of the ``Ceylan-Myriad`` Layer
===============================================


:Organisation: Copyright (C) 2008-2021 Olivier Boudeville
:Contact: about (dash) myriad (at) esperide (dot) com
:Creation date: Sunday, August 17, 2008
:Lastly updated: Sunday, April 4, 2021
:Status: Stable
:Version: 1.2.23
:Dedication: Users and maintainers of the ``Myriad`` layer.
:Abstract:

	The role of the `Myriad <http://myriad.esperide.org/>`_ layer (part of the `Ceylan <https://github.com/Olivier-Boudeville/Ceylan>`_ project) is to gather all `Erlang <http://erlang.org>`_ general-purpose base constructs that we found useful for (Erlang-based) developments.

	We present here a short overview of these services, to introduce them to newcomers.
	The next level of information is to read the corresponding `source files <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_, which are intensely commented and generally straightforward.


.. meta::
   :keywords: Myriad, generic, general-purpose, helper code, library, layer



The latest version of this documentation is to be found at the `official Ceylan-Myriad website <http://myriad.esperide.org>`_ (``http://myriad.esperide.org``).

:raw-html:`This Myriad documentation is also available in the PDF format (see <a href="Ceylan-Myriad-technical-manual-english.pdf">Ceylan-Myriad-technical-manual-english.pdf</a>), and mirrored <a href="http://olivier-boudeville.github.io/Ceylan-Myriad/">here</a>.`

:raw-latex:`The documentation is also mirrored \href{https://olivier-boudeville.github.io/Ceylan-Myriad/}{here}.`



:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 32

.. comment To avoid, otherwise title with a '1.':.. section-numbering::




.. include:: Ceylan-Myriad-Layer-overview-and-context-english.rst

.. include:: Ceylan-Myriad-Layer-usage-guidelines-english.rst

.. include:: Ceylan-Myriad-Layer-installation-english.rst

.. comment
  .. include:: Ceylan-Myriad-Layer-builder-english.rst


:raw-latex:`\pagebreak`


.. include:: Ceylan-Myriad-Layer-list-of-services-english.rst

.. include:: Ceylan-Myriad-Layer-build-english.rst

.. include:: Ceylan-Myriad-Layer-general-settings-english.rst

.. include:: Ceylan-Myriad-Layer-maths-services-english.rst

.. include:: Ceylan-Myriad-Layer-data-management-services-english.rst

.. include:: Ceylan-Myriad-Layer-conditional-code-english.rst

.. include:: Ceylan-Myriad-Layer-ui-english.rst

.. include:: Ceylan-Myriad-Layer-helper-scripts-english.rst

.. include:: Ceylan-Myriad-Layer-utility-toolbox-english.rst

.. include:: Ceylan-Myriad-Layer-metaprogramming-english.rst

.. include:: Ceylan-Myriad-Layer-management-of-units-english.rst

.. include:: Ceylan-Myriad-Layer-sql-support-english.rst

.. include:: Ceylan-Myriad-Layer-conventions-english.rst

.. include:: Ceylan-Myriad-Layer-gotchas-english.rst

.. include:: Ceylan-Myriad-Layer-support-english.rst



:raw-latex:`\pagebreak`


-------------
Please React!
-------------

If you have information more detailed or more recent than those presented in this document, if you noticed errors, neglects or points insufficiently discussed, drop us a line! (for that, follow the Support_ guidelines).


---------------------------
Contributions & Ending Word
---------------------------

Each time that you need a basic service that:

- seems neither provided by the Erlang `built-in modules <http://erlang.org/doc/man_index.html>`_ nor by this ``Myriad`` layer
- is generic-enough, simple and requires no special prerequisite

please either enrich our ``*_utils.erl`` `helpers <https://github.com/Olivier-Boudeville/Ceylan-Myriad/tree/master/src/utils>`_, or add new general services!


.. _contribution:

In such a case, we would prefer that, in contributed code, the Myriad `Text Conventions`_ and `Coding Practices`_ are respected.

Thanks in advance, and have fun with Ceylan-Myriad!

:raw-html:`<center><img src="myriad-title.png" width="35%"></img></center>`
:raw-latex:`\begin{figure}[h] \centering \includegraphics[scale=0.1]{myriad-title.png} \end{figure}`


:raw-html:`<a name="myriad_bottom"></a>`
