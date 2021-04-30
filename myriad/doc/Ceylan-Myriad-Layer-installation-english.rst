

--------------
Getting Myriad
--------------


.. _prerequisites:

Prerequisites
=============

The **operating system** is supposed to be any not-so-old ``GNU/Linux`` distribution [#]_.

People reported uses of Myriad on ``macOS``, yet no extensive testing has been done there.

Whereas Erlang supports ``Windows`` and we tried to be as cross-platform as possible, even with tools like ``MSYS2`` / ``MinGW-w64`` we suppose quite a lot of special cases would have to be addressed (patches welcome, though!).

.. [#] For what it is worth, we prefer `Arch Linux <https://www.archlinux.org/>`_, but this does not really matter here.

.. _getting-erlang:

The main tool prerequisite is of course having the `Erlang <http://erlang.org>`_ environment available, in its ``23.0`` version [#]_ or more recent.

.. [#] Most probably that older versions of Erlang would be more than sufficient in order to build Myriad (possibly at the expense of minor changes in a few calls to standard modules having been deprecated since then). It is just that in general we prefer to stick to the latest stable versions of software such as Erlang, and advise you to do so.


There are various ways of obtaining it (from your distribution, from prebuilt packages, directly from the sources), one of which being the `install-erlang.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/install-erlang.sh>`_ script that we devised.

A simple use of it is:

.. code:: bash

 $ ./install-erlang.sh --doc-install --generate-plt


One may execute ``./install-erlang.sh --help`` for more guidance about how to configure it, notably in order to enable all modules of interest (``crypto``, ``wx``, etc.).



Getting Myriad's Sources
========================

This is pretty straightforward, based on the `project repository <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_ hosted by Github:

.. code:: bash

 $ git clone https://github.com/Olivier-Boudeville/Ceylan-Myriad.git myriad

This should download in your current directory the full Myriad repository. For OTP compliance, using for such a clone its short name (``myriad``) rather than its long one (``Ceylan-Myriad``) is recommended.

The Myriad ``master`` branch is meant to stick to the latest stable version: we try to ensure that this main line always stays functional (sorry for the pun). Evolutions are to take place in feature branches and to be merged only when ready.


.. _build:

Building Myriad
===============

If a relevant Erlang installation is available, this is as simple as:

.. code:: bash

 $ cd myriad
 $ make all


The parallel build of the whole layer (services and tests alike) shall complete successfully (if it is not the case, see our support_ section).

One may just run ``make`` by itself in order to list the main available options.

One may run ``make create-myriad-checkout`` in order to create, based on our conventions, a suitable ``_checkouts`` directory so that rebar3 can directly take into account local, directly available (in-development) dependencies (although Myriad does not have any, beside Erlang itself).

Alternatively to using ``make`` directly, one may execute ``rebar3 compile`` instead.


.. _testing:

Testing Myriad
==============

As Myriad has no prerequisite (besides Erlang itself of course), just run (possibly simply thanks to ``rebar3 compile`` after a ``git clone https://github.com/Olivier-Boudeville/Ceylan-Myriad.git``), still from the root directory of Myriad:

.. code:: bash

 $ make test

The testing shall complete successfully (if it is not the case, see our support_ section).

.. Note:: Myriad is built and tested at each commit through `continuous integration <https://github.com/Olivier-Boudeville/Ceylan-Myriad/actions?query=workflow%3A%22Erlang+CI%22>`_. The same holds for the projects based on it, directly (ex: `WOOPER <https://wooper.esperide.org>`_, `Seaplus <https://seaplus.esperide.org>`_) or not (ex: `Traces <https://traces.esperide.org/>`_, `Mobile <https://mobile.esperide.org/>`_, `US-Web <https://us-web.esperide.org/>`_), so in terms of usability, confidence should be high.





.. _`type-checking`:

Type-checking Myriad
====================

As Myriad is (by default) to enable debug information with a key-based protection of the resulting BEAM files, one should first have such key defined.

One way of doing so is, if wanted, to update the default key (see ``DEBUG_INFO_KEY`` in ``GNUmakevars.inc``) and to write in on disk (ex: ``make write-debug-key-file``), and to rebuild Myriad accordingly afterwards (ex: ``make rebuild``).

Then, still from the ``myriad`` root directory:

.. code:: bash

 $ make generate-local-plt self-check-against-plt

It will trigger a full type-checking of Myriad, done thanks to `Dialyzer <http://erlang.org/doc/man/dialyzer.html>`_.

This time-consuming phase will complete with a rather long list of notifications. Help us reducing it! These messages are numerous, but we do not think that most of them are so worrying.

Finally, to trigger in one go a full rebuild, testing and type-checking, one may run:

.. code:: bash

 $ make check



Maintaining Myriad and Deriving Projects with regard to rebar3
==============================================================

For Myriad as for all developments built on top of it (ex: specialisation layers or applications), any dependency may be specified in their ``rebar.config`` [#]_ through a branch of a GIT repository corresponding to that dependency.

.. [#] For example, with the conventions we rely on, ``rebar.config`` is generated from the ``conf/rebar.config.template`` file of the project of interest.

For example, Myriad itself does not require any specific dependency, but projects making use of Myriad (ex: `WOOPER <https://wooper.esperide.org>`_) may specify in their ``rebar.config``:

.. code:: erlang

  {deps, [{myriad, {git, "git://github.com/Olivier-Boudeville/Ceylan-Myriad",
										{branch, "master"}}}]}.

However, when having to build a dependency, rebar3 will not refer to the tip of the branch specified for it, but to any commit it may read from any pre-existing ``rebar.lock`` file at the root of the current project (the underlying goal being to allow for more reproducible builds).

As the `rebar3 recommandation <https://www.rebar3.org/docs/workflow/#setting-up-dependencies>`_ is to store a version of that lock file in source version control, **it shall be regularly updated** otherwise the dependencies of a given project will stick, for the worst, to an older version of their branch, designated by an obsolete reference (this can be detected for example when continuous integration breaks after a nevertheless legit commit of the project).

The solution is thus, for a project of interest, to regularly force an update of its dependencies referenced in its own lock file, and to commit the resulting version.

For example, one would issue from the root of the project of interest:

.. code:: bash

 $ rebar3 upgrade

This may update the ``ref`` entry of its dependencies (including Myriad) in its ``rebar.lock`` file, which shall then be committed for posterity.


:raw-html:`<a name="otp"></a>`

.. _`otp-build`:

OTP Build
=========

These build considerations apply to Myriad but also, more generally, to most if not all our Erlang developments.


Why Providing Two Different Build/Deploy/Run Systems
----------------------------------------------------

We felt that OTP build tools and Emakefiles were not expressive enough for our needs: as mentioned in `Building Myriad`_, a full, rather complete/complex/powerful build system based on `GNU make <https://www.gnu.org/software/make/manual/make.html>`_ is used by Ceylan-Myriad natively instead, and has been fully satisfactory for years (simple, lightweight, reliable, controllable, flexible, fast, etc.).

It allows to introduce all the generic rules we wanted, to define many conditional settings, to walk through an arbitrarily nested source tree, to integrate within a layered stack (notably alongside some other ``Ceylan-*`` libraries that depend on Ceylan-Myriad) and to perform a multi-stage build to accommodate the compilation and use of parse-transforms, with their own set of prerequisites.

More precisely we routinely (see `WOOPER <https://wooper.esperide.org>`_ or `Seaplus <https://seaplus.esperide.org>`_) rely on layers built on top of Myriad, which define their own parse transforms that are themselves parse-transformed by Myriad's one - and it works great.

However, to better integrate with other Erlang developments (which are mostly OTP-compliant), we added the (optional) possibility of generating a Myriad *OTP library application* out of the build tree, ready to be integrated into an (OTP) *release* and to be available as an Hex *package*. For that we rely on `rebar3 <https://www.rebar3.org/>`_, `relx <https://github.com/erlware/relx>`_ and `hex <https://hex.pm/>`_.

So currently all our Erlang-based developments can also be built and tested through rebar3, and this support is checked at each commit thanks to continuous integration.

We use less frequently releases (we rely on a basic deployment procedure of our own) and even less hex, yet they were supported once, so we believe that their integration should be at least fairly close to be operational (if not, patches welcome!).



Relying on Rebar3
-----------------

Despite the kind support of the rebar3 authors and much time spent on its integration, sometimes our build based on it (for Myriad and the projects built on top of it) has encountered issues or has been lagging behind our native one.


.. comment Ultimately we expect all pending issues to be solved

Now we believe that all pending issues have been solved (rebar3 is a neat tool), yet being able to switch back to another lighter, ad-hoc, more controlled build system is sometimes a relief - at least a welcome security. Anyway the user can choose between these two (native vs rebar3) build machineries. As for us, we still prefer our native build system, even if it leaves to the developer the task of installing the needed prerequisites by him/herself.

.. So most of the time one can choose between these two build machineries.

.. Nevertheless, as of end of 2020, after insisting a lot on using rebar3, we mainly switched back and relied on our own, native build system instead, so that we could concentrate on the code itself rather than on the build.

.. Since then the rebar3 support remains as it is (a priori at least mostly functional); maybe in the future we will reintroduce it as a native, possibly main, build option - but not today.


..
  commented-out for the moment:

  Rebar3-related Issues
  ---------------------

  Yet, after much struggle and full days spent on build issues, after a last regression that we could not track down to a change that we made, and despite the obvious qualities of rebar3, we are not as sure as before that we should rely on this tool for our own builds.

  It is difficult for us to tell whether rebar3 and/or hex and/or relx and/or even the OTP release system are overly complex and possibly fragile for the services they provide, and maybe we did not understood them enough or had too specific build procedures to implement, however we felt that the time spent over the years on mere build issues has been unacceptably high.




OTP Application
---------------

Myriad is not an *active* OTP application, and as such does not rely on, or provides, services running in the background; so no supervision tree or ``gen_server`` is involved here, just a *library* application ready for OTP integration [#]_.

.. [#] Speaking of OTP, in development mode, ``proc_lib``-based spawns used to be enabled, yet this led to longer error messages that were not that useful; see ``spawn_utils.hrl`` if wanting to re-enable them.


.. _`getting-rebar3`:

Getting rebar3
..............

There are `various ways <https://www.rebar3.org/docs/getting-started>`_  for obtaining ``rebar3``; we prefer::

  $ cd ~/Software && git clone https://github.com/erlang/rebar3.git
	  && cd rebar3 && ./bootstrap

Alternatively, should you just want to update a (pre-existing) rebar3 install, first get the current version (``rebar3 -v``) to check it afterwards, then issue ``rebar3 local upgrade``; however this would involve running rebar from ``.cache/rebar3/bin``, so instead we prefer using (typically from ``~/Software/rebar3``)::

 $ git pull && ./bootstrap

Another option is to download a prebuilt version of rebar3.

Finally, one may prefer using the `install-rebar3.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/install-rebar3.sh>`_ script that we devised, which automates and enforces our conventions while letting the choice between an installation from sources or from a prebuilt version thereof (just un ``install-rebar3.sh --help`` for guidance).



Generating Ceylan-Myriad
........................

Then, from the root of a Myriad clone, to obtain the Ceylan-Myriad library *application*, one just has to enter::

 $ make rebar3-application

It will trigger ``rebar3``, resulting [#]_ in a full, OTP-compliant build tree created in ``_build`` (including a properly-generated ``_build/default/lib/myriad/ebin/myriad.app`` file), and more generally in a proper OTP application.

.. [#] The operation was previously done through a rebar pre-compile hook, so that the our native build system could be relied upon before injecting the produced BEAMs into rebar's ``_build`` tree. Because of extraneous, failing recompilations being triggered by rebar, now we rely on a build system parallel to - and directly inspired by - our native one, directly done from within rebar (once properly triggered by our user-oriented Make targets).


Testing Ceylan-Myriad
.....................

As a result, the OTP application support can be tested from the root of an (already-built, with ``make rebar3-application``) Myriad source tree:

.. code:: bash

 $ cd src/utils
 $ make myriad_otp_application_run
		Running unitary test myriad_otp_application_run (third form) from
		   myriad_otp_application_test

 --> Testing module myriad_otp_application_test.

 Starting the Myriad application.
 Myriad version: {1,0,11}.
 Current user name: 'stallone'.
 Stopping the Myriad application.
 Successful end of test of the Myriad application.
 =INFO REPORT==== 18-Jul-2019::22:37:24.779037 ===
	application: myriad
	exited: stopped
	type: temporary

 --> Successful end of test.

 (test finished, interpreter halted)


This support can be also tested manually, directly through the build tree used by rebar3; from the root of Myriad, after having run ``make rebar3-application``:

.. code:: bash

 $ erl -pz _build/default/lib/myriad/ebin/
 Erlang/OTP 22 [erts-10.4] [source] [64-bit] [smp:8:8] [...]

 Eshell V10.4  (abort with ^G)
 1> application:start(myriad).
 ok
 2> text_utils:format( "Hello ~s", [ world ] ).
 "Hello world"
 3> application:stop(myriad).
 =INFO REPORT==== 18-Jul-2019::22:47:36.429804 ===
	application: myriad
	exited: stopped
	type: temporary


When needing to include a Myriad header file (taking ``spawn_utils.hrl`` as an example) in one's code, OTP conventions mandate using::

 -include_lib("myriad/include/spawn_utils.hrl").

rather than::

 -include("spawn_utils.hrl").



OTP Release
-----------

Quite similarly, to obtain a Ceylan-Myriad OTP *release* (`relx <https://github.com/erlware/relx>`_ being used in the background), possibly for a given profile like ``default`` (development mode) or ``prod`` (production mode) - refer to ``REBAR_PROFILE`` in ``GNUmakevars.inc``, one just has to run, from the root of Myriad::

 $ make rebar3-release



Hex Package
-----------

The `hex <https://hex.pm/>`_ package manager relies on mix, which is commonly installed with `Elixir <https://elixir-lang.org/>`_ (another language built on top of the Erlang VM).

.. comment  As an example on Arch Linux, to obtain hex, one would do the following:: $ pacman -S elixir

Thanks to the rebar3 integration with the ``rebar3_hex`` plugin specified in Myriad's (generated) `rebar.config <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/rebar.config>`_, ``hex`` will be automatically installed and set up.

By following the publishing guidelines (`[1] <https://hex.pm/docs/rebar3_publish>`_, `[2] <https://www.rebar3.org/docs/package_management/publishing-packages/>`_), we were able to publish `Hex packages for Myriad <https://hex.pm/packages/myriad>`_ that can be freely used. And there was much rejoicing!

One just has to specify for example ``{deps,[myriad]}.`` in one's ``rebar.config``, and that's it.


.. Note:: Finally our workflow does not rely on Hex, so we do not update the Hex packages anymore. Just drop us an email if needing an updated one.


For more details, one may have a look at:

- `rebar.config.template <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/rebar.config.template>`_, the general rebar configuration file used when generating the Myriad OTP application and release
- `rebar-for-hex.config.template <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/rebar-for-hex.config.template>`_, to generate a corresponding Hex package for Myriad (whose structure and conventions is quite different from the previous OTP elements)
- `rebar-for-testing.config.template <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/conf/rebar-for-testing.config.template>`_, the simplest test of the previous Hex package: an empty rebar project having for sole dependency that Hex package



Other OTP-related Make Targets of Interest
------------------------------------------

To populate/update the OTP build tree (by default, from the GIT root, for example ``_build/default/lib/myriad/`` for Myriad) of the current Ceylan layer, one may use::

 $ make rebar3-compile

(this is especially useful in order to be able to use directly, from an OTP application, changes just performed in a Ceylan-based layer)


To update both the OTP build tree and the local ebin directory of each Ceylan layer on which the current layer depends, use::

 $ make rebar3-local-update

(note this will be a no-op from Myriad, as it does not depend on any Ceylan layer)


To publish an Hex package (once the proper version number has been set in ``GNUmakevars.inc``, see ``MYRIAD_VERSION``)::

 $ make rebar3-hex-publish


To test such a package::

 $ make test-hex-package


To populate directly the OTP local build tree with the Ceylan dependencies located alongside the current install (not useful for Myriad - which depends on none, but useful for upper layers) rather than fetching them through Hex (otherwise may more Hex packages would have to be published for testing during development)::

 $ make rebar3-local-update

Many more targets are defined in `GNUmakerules-explicit.inc <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/GNUmakerules-explicit.inc>`_.
