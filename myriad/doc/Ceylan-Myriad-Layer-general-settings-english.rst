:raw-latex:`\pagebreak`

.. _settings:


General Settings
================

.. _`emacs settings`:

These general-purpose settings and helpers, gathered in Myriad's `conf <https://github.com/Olivier-Boudeville/Ceylan-Myriad/tree/master/conf>`_ directory, deal with:

- default CSS files (``Default-docutils.css``)
- our recommended versions of (commented) configuration files for various tools, notably ``Emacs``:  see our `init.el <https://github.com/Olivier-Boudeville/Ceylan-Myriad/tree/master/conf/init.el>`_, to be placed in the ``~/.emacs.d/`` directory; now our configuration is standalone (no need for extra files/packages) and cross-platform (at least Unices and Windows)

- our standard script to properly install Erlang (`install-erlang.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/tree/master/conf/install-erlang.sh>`_) with detailed comments and command-line options (use ``install-erlang.sh --help`` for more information)


.. obsolete: - for ``Nedit``: ``nedit.rc``, to be placed in the ``~/.nedit/`` directory

.. obsolete: in addition to our ``init.el`` configuration file, we rely on ``acme-search.el``, ``flyspell-guess.el`` and ``whitespace.el`` (all in the ``~/.emacs.d`` directory - check that they are not eclipsed by any other initialisation file, like a stray ``~/.emacs`` file; see also our `update-emacs-modules.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/update-emacs-modules.sh>`_ script)
