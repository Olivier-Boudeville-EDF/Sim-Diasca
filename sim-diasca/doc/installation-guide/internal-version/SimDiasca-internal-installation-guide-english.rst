
=============================
Sim-Diasca Installation Guide
=============================

--------------------
EDF Internal Version
--------------------


.. comment Defined here to allow for a version-specified banner:


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


:raw-html:`<a name="sim_diasca_top"></a>`

:raw-html:`<div class="banner"><p><em>Sim-Diasca EDF documentation</em> <a href="FIXME">browse latest internal</a> <a href="FIXME">browse public version</a> <a href="Sim-Diasca-internal-installation-guide-english.pdf">get PDF</a> <a href="#"sim_diasca_top">go to top</a> <a href="#"sim_diasca_bottom">go to bottom</a> <a href="https://gitlab.pleiade.edf.fr/Sim-Diasca/sim-diasca/-/wikis/home">internal project</a> <a href="https://github.com/Olivier-Boudeville-EDF/Sim-Diasca">public project</a><a href="mailto:olivier(dot)boudeville(at)edf(dot)fr?subject=[Sim-Diasca]%20Remark">email us</a></p></div>`


.. include:: SimDiasca-installation-guide-header-english.rst



Objective & Context
===================

The goal here is to set up a fully functional Sim-Diasca installation, to be able to test it and to develop with it, for example so that the implementation of new simulation cases and models can be directly experimented.

In the context of this (EDF) internal version, the installation is likely to target ``Calibre`` hosts running ``Scibian 9/10`` distributions.

No specific user permissions are needed (ex: no need to be ``sudoer``); some prerequisites will be needed that can be installed from the Calibre Software Library (a.k.a. the "Logithèque"). See `this page <https://gitlab.pleiade.edf.fr/Capitalisation/espace-principal/-/wikis/Calibre-&-compagnie#installations-de-logiciels>`_ for further information.

Still in this EDF context, the installation is to be performed directly from `the internal project GIT repository <https://gitlab.pleiade.edf.fr/Sim-Diasca/sim-diasca>`_. This allows to access to updates and send back changes quite easily; compared to the `public version <https://github.com/Olivier-Boudeville-EDF/Sim-Diasca>`_, the internal one is generally more recent and provides additional simulator examples.


.. include:: SimDiasca-installation-guide-base-english.rst
