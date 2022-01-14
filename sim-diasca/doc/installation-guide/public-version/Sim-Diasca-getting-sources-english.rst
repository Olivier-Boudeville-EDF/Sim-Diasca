
Installation From the Sim-Diasca Public GIT Repository
......................................................


Unless you have access to the EDF-internal Sim-Diasca GIT repository (which should then be preferred, as it contains more content, and is by design more recent), you can clone the public GIT repository (which should be fine as well [#]_), at the following location: `https://github.com/Olivier-Boudeville-EDF/Sim-Diasca <https://github.com/Olivier-Boudeville-EDF/Sim-Diasca>`_.

.. [#] Please tell us (typically thanks to the e-mail address at the top of this document) if ever the current public version seems to date back a bit too much, as we do not always update the public version once an internal one has been released.

It can thus be obtained, typically from a GNU/Linux host, thanks to:

.. code:: bash

  $ git clone https://github.com/Olivier-Boudeville-EDF/Sim-Diasca

Then a right branch or tag shall be selected, knowing that versions bear annotated tags now labelled as: ``sim-diasca-x.y.z-version``.

The ``master`` branch is the main one, and contains the latest public version of Sim-Diasca that has been released, so most people will happily stick to this ``master`` branch.


.. Note:: The public repository is not the actual one that is used to develop Sim-Diasca, yet pull requests or any interaction may be based on it; if appropriate, we will take care of any integration that would be useful, and such improvements are to benefit to the community as a whole, in the next public versions to be released afterwards.
