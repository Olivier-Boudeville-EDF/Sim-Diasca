
Installation From the Sim-Diasca Internal GIT Repository
........................................................

This is now the usual procedure.

Cloning
_______


The EDF-internal GIT repository is hosted by the PAM infrastructure, at the following location: `https://gitlab.pleiade.edf.fr/Sim-Diasca/sim-diasca <https://gitlab.pleiade.edf.fr/Sim-Diasca/sim-diasca>`_ [#]_.

.. [#] Knowing that the location of the former internal GitLab project was `this one <https://git.forge.pleiade.edf.fr/git/sim-diasca>`_ .


It can thus be obtained, on a GNU/Linux host connected to the RIN (EDF internal network), thanks to:

.. code:: bash

  $ git clone https://gitlab.pleiade.edf.fr/Sim-Diasca/sim-diasca sim-diasca-clone


From now on, non-absolute paths (ex: ``mock-simulators/soda-test/...``) must be understood as being relative to the root directory of (the current branch of) this clone.

Then a right branch or tag shall be selected, knowing that versions bear annotated tags now labelled as: ``sim-diasca-x.y.z-version``.

The ``master`` branch is the main one, and contains the current reference version of Sim-Diasca (preparation of the next release, as a rolling release candidate).

For further details regarding the Sim-Diasca conventions in terms of version control, please refer to the **Sim-Diasca Developer Guide**.



Selecting One's Branch as a Developer
_____________________________________

When multiple *developers* have to collaborate on the codebase, they shall rely on different branches, per-developer (designated by: first name, dash, last name, all in lowercase; like in the ``joe-dalton`` branch) if not per-developer and per-feature (ex: a ``joe-dalton-ui-rework`` branch).

So typically a new developer would select the branch from which to derive (ex: ``master``), and, if not already created, would derive his/her working branch from it:

.. code:: bash

  $ git checkout my_start_branch
  $ git checkout -branch joe-dalton



Selecting One's Branch as a User
________________________________

Most *users* will prefer sticking to the latest stable release, which can be obtained with:

.. code:: bash

 $ git tag -l "sim-diasca-*-version" | sort | head -n 1


For example, at the time of this writing, this results in the ``sim-diasca-2.3.10-version`` tag.

Then, for example:

.. code:: bash

  $ git checkout sim-diasca-2.3.10-version



Obtaining LogMX
_______________

We rely internally on the professional version of LogMX.

You should thus have been given a LogMX licence file (ex: ``license.properties``).

As for the LogMX archive (ex: ``LogMX_vx.y.z_pro.zip``), it can be downloaded from `the repository dedicated to the Sim-Diasca prerequisites <https://gitlab.pleiade.edf.fr/Sim-Diasca/prerequisites/-/tree/master/LogMX>`_ (just pick the latest LogMX version available).

.. Note:: Within EDF, both shall be used instead of the base ones that are mentioned later in this document.
