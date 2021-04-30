==========================
Sim-Diasca Developer Guide
==========================


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. Unable to have proper table formatting without specialising:


.. Scale does not seem taken into account:
:raw-html:`<center><table><tr colspan="2"><img src="sim-diasca.png" style="width:500px"></tr><tr><td><img src="logo-EDF-english.png" style="width:150px"></td><td><img src="lgpl-v3-logo-bordered.png" style="width:140px"></td></tr></table>`


:raw-latex:`\begin{center} \begin{tabular}{c c c} \multicolumn{2}{c}{\includegraphics[scale=0.3]{sim-diasca.png}} \\  \includegraphics[scale=0.4]{logo-EDF-english.png} & \includegraphics[scale=0.8]{lgpl-v3-logo-bordered.png} \\ \end{tabular} \end{center}`


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


:Organisation: Copyright (C) 2008-2021 EDF R&D
:Author: Olivier Boudeville
:Contact: olivier.boudeville@edf.fr
:Lastly Updated: Tuesday, March 9, 2021
:Creation Date: February 2009
:Status: Work in progress
:Version: 0.9.8
:Website: `http://sim-diasca.com <http://sim-diasca.com>`_
:Dedication: To the Sim-Diasca developers

:Abstract:

	This document summarizes the main conventions that should be respected when contributing code to Sim-Diasca and/or making use of Sim-Diasca.



:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 4

.. section-numbering::


:raw-latex:`\pagebreak`

.. Note::

 This document intends to gather information aimed at Sim-Diasca *maintainers* or *contributors* - not users. Most people should read the *Sim-Diasca Technical Manual* first, and possibly the *Sim-Diasca Modeller Guide* as well.

 However we require that the in-house authors of any simulation element making use of Sim-Diasca's services (ex: models, simulation cases, etc.) respect at least the conventions presented in the current document, for the sake of the clarity and homogeneity of the code base.

 As we believe that these conventions may benefit to third-party users, we share them as well.

.. include:: SimDiasca-code-conventions-english.rst

.. include:: SimDiasca-implementation-spotlights.rst

.. include:: SimDiasca-technical-gotchas.rst


:raw-latex:`\pagebreak`

.. include:: SimDiasca-SVN-conventions-english.rst

.. include:: SimDiasca-SVN-guide-english.rst



:raw-latex:`\pagebreak`

.. include:: SimDiasca-developer-hints-english.rst

.. include:: SimDiasca-about-type-specifications-english.rst

.. include:: SimDiasca-guide-footer-english.rst
