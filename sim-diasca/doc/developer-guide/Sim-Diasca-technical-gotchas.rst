:raw-latex:`\pagebreak`


----------------------------
Sim-Diasca Technical Gotchas
----------------------------

We mention here the main technical sources of puzzlement that may affect the unwary developer.


The Code Was Updated, Yet Seems To Linger
=========================================

This may happen **if the code source has been changed yet has not been recompiled before launching a simulation**: Sim-Diasca, once executed, will attempt to compile it, and hopefully succeed.

Then the corresponding BEAM modules will be available in their newer version and, when they will be referenced for the first time, they will be loaded - thus in their newer form.

However, some modules may have already been loaded by the engine (for its internal mode of operation), *before* it triggered the compilation update.

As a result, even if a newer version of their BEAM file becomes available on disk, these modules have already been loaded (and will not be specifically reloaded) [#]_
; they will thus stick to their former version, and their newer version will be loaded only at the *next* Sim-Diasca run.

.. [#] Moreover, they may belong to the pioneer modules, in which case they will be deployed over the network on other nodes as well, instead of being read in an updated version from the simulation archive.


A solution is simply to ensure that any module whose source has been modified is recompiled afterwards (simply a matter of typing ``CTRL-P`` with our emacs settings), at least *before* a new simulation is run.
