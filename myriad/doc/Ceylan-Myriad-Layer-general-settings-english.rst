:raw-latex:`\pagebreak`

.. _settings:


General Settings
================

.. _`emacs settings`:

These general-purpose settings and helpers, gathered in Myriad's `conf <https://github.com/Olivier-Boudeville/Ceylan-Myriad/tree/master/conf>`_ directory, deal with:

- default CSS files (``Default-docutils.css``)
- our recommended versions of (commented) configuration files for various tools:

  - for ``Emacs``: ``init.el``, to be placed in the ``~/.emacs.d/`` directory; in addition to our ``init.el`` configuration file, we rely on ``acme-search.el``, ``flyspell-guess.el`` and ``whitespace.el`` (all in the ``~/.emacs.d`` directory - check that they are not eclipsed by any other initialisation file, like a stray ``~/.emacs`` file; see also our `update-emacs-modules.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/update-emacs-modules.sh>`_ script)
  - for ``Nedit``: ``nedit.rc``, to be placed in the ``~/.nedit/`` directory

- our standard script to properly install Erlang (``install-erlang.sh``) with detailed comments and command-line options (use ``install-erlang.sh --help`` for more information)
