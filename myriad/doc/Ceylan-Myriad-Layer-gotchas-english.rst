:raw-latex:`\pagebreak`

----------------------------------
``Myriad``-related Troubleshooting
----------------------------------


Header/Module Dependencies
==========================

Only a very basic dependency between header files (``*.hrl``) and implementation files (``*.erl``) is managed.

As expected, if ``X.hrl`` changed, ``X.beam`` will be recompiled whether or not ``X.erl`` changed. However, any ``Y.erl`` that would include ``X.hrl`` would not be automatically recompiled.

Typically, when in doubt after having modified a record in a header file, just run ``make rebuild`` from the root of that layer (build is fast anyway, as quite parallel).



Third-Party Dependencies
========================

Let's suppose we have an application named ``Foo`` that relies on Myriad.

``Foo`` may define additional dependencies, which may be:

- either mandatory or optional
- needed starting from build-time (ex: if relying on their headers and/or modules - including parse-transforms), or only at runtime

For a given **optional** dependency (ex: regarding JSON), a USE make variable is to be defined in the layer that introduced this dependency (ex: ``USE_JSON``, introduced by Myriad, therefore to be listed in its ``GNUmakevars.inc``). This variable allows to have our native build system register the associated additional include and ebin directories.

The first step to enable such a dependency (ex: the JSON support) is to set its USE variable to ``true`` (ex: ``USE_JSON = true``), as it is expected to be disabled by default. Depending on the service at hand, a specific backend may have also to be selected (ex: either ``USE_JSX = true`` or ``USE_JIFFY = true`` to select a suitable JSON parser).

Finally, some supports may depend on others (ex: enabling ``USE_REST`` will enable ``USE_JSON`` in turn).



Runtime-only Third-Party Dependencies
-------------------------------------

The dependencies discussed here have to be found only when *executing* one's application; they can be installed:

- either manually, in which case the location of their ``ebin`` directory (typically an absolute path then) shall be specified in the code path (see, in ``GNUmakevars.inc``, the ``JSX_SOFTWARE_BASE`` make variable for an example)
- or thanks to rebar, in which case they shall obey the same rules as the `Build-time Third-Party Dependencies`_ discussed below



Build-time Third-Party Dependencies
-----------------------------------

Myriad does not have such dependencies, but layers above in the software stack (like a layer that would be named ``Foo``) may.

To have such dependencies (ex: let's suppose that the ``jsx`` JSON parser defined header files that one wants to include) *installed* as well when building one's project (ex: ``Foo``), one may rely on rebar, and list them in the project's ``foo/conf/rebar.config.template`` file (ex: ``{deps, [bar, jsx]}.``) from which the actual ``rebar.config`` is to be generated (use the ``make set-rebar-conf`` target for that).

The actual compilation will be done by our native build system in all cases, either directly (when running ``make all``) or when using ``rebar compile`` (rebar hooks will then ensure that in practice the application is compiled with our native rules anyway). Therefore appropriate make variables (ex: ``JSX_REBAR_BASE``, in ``myriad/GNUmakevars.inc``) shall be defined so that the corresponding BEAM files installed through rebar can be found in this native context as well (through the ``BEAM_DIRS`` make variable).

Finally, such dependencies may or may not be listed in the ``deps`` entry of the  ``conf/foo.app.src`` file [#]_, depending on whether they are optional or not.

.. [#] After having edited this file, run ``make rebar3-create-app-file`` afterwards in order to have the three other versions of it properly generated (namely ``./_build/lib/foo/ebin/foo.app``, ``ebin/foo.app`` and ``src/foo.app.src``).



Myriad-level Third-Party Dependencies
-------------------------------------

Myriad as such has no mandatory dependency (except Erlang itself of course), but *optional* ones may be enabled, for:

- a basic `JSON <https://en.wikipedia.org/wiki/JSON>`_ support (see our `json_utils <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/utils/json_utils.erl>`_ module), thanks to a suitable actual JSON parser, either `jsx <https://github.com/talentdeficit/jsx/>`_ or `jiffy <https://github.com/davisp/jiffy>`_; note that the detection and use of these parsers are done transparently at runtime, hence none of them is a declared dependency of Myriad, which will adapt to any choice made by the overall application that includes both Myriad and one of such parsers (provided, as mentioned above, that the proper ``USE_*`` make variables are set)
- a first-level support of the `HDF5 <https://www.hdfgroup.org/HDF5/>`_ file format (see our `hdf5_support <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/data-management/hdf5_support.erl>`_ module), based on - and thus requiring - the `enhanced fork <https://github.com/Olivier-Boudeville-EDF/erlhdf5>`_ that we made of `erlhdf5 <https://github.com/RomanShestakov/erlhdf5>`_
- `Python <https://en.wikipedia.org/wiki/Python_(programming_language)>`_ (see our `python_utils <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/utils/python_utils.erl>`_ module), thanks to `erlport <https://github.com/hdima/erlport>`_
- `SQLite <https://en.wikipedia.org/wiki/SQLite>`_ (see our `sql_support <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/data-management/sql_support.erl>`_ module), thanks to the SQLite 3 Erlang binding that we retained, `erlang-sqlite3 <https://github.com/alexeyr/erlang-sqlite3.git>`_


.. _`jsx install`:

As an example, let's suppose that we need a JSON support and that we want to rely on the ``jsx`` parser (our default choice) for that.

If applying our conventions, supposing that Erlang and Rebar3 are already installed (otherwise refer to the `getting Erlang`_ and `getting Rebar3`_ sections), ``jsx`` may be installed with:

.. code:: bash

 $ mkdir -p ~/Software/jsx
 $ cd ~/Software/jsx
 $ git clone https://github.com/talentdeficit/jsx.git
 $ ln -s jsx jsx-current-install
 $ cd jsx/
 $ rebar3 compile && rebar3 eunit

.. $ ln -s _build/default/lib/jsx/ebin



About the ``table`` module
==========================

This is a pseudo module, which is not meant to exist as such (no ``table.erl``, no ``table.beam``).

The ``Myriad`` parse transform replaces references to the ``table`` module by (generally) references to the ``map_hashtable`` module. See `table transformations`_ for more information.




Enabling the Interconnection of Erlang nodes
============================================

This is not a Myriad gotcha per se, but rather an Erlang one.

Way too often, for obscure reasons Erlang nodes fail to connect to each other (especially with long names), and little to no information is available to diagnose the issue.



Safety Measures
---------------

In order to maximise the chances that nodes are able to ``net_adm:ping/1`` successfully each other:

- at least for testing, run VMs spawned with preferably the same **version** of Erlang
- ensure that they rely on the same **EPMD** (TCP) port (default Erlang one is ``4369``, while Myriad default one is ``4506``); check for example that all launched nodes of interest can be seen with: ``epmd -port 4506 -names``
- check that they use the same **cookie**, either from the start (use the ``-setcookie MY_COOKIE`` command-line option) or after having changed it after the VM was launched
- ensure that no **firewall** gets in the way; one may take inspiration for example from our `iptables.rules-FullDisabling.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/iptables.rules-FullDisabling.sh>`_ script
- finally check that the local **DNS resolution** complies with the surprisingly picky constraints demanded by the Erlang VM

For this last point, ``/etc/hosts`` is often the scene of the disaster. If your hostname is ``hurricane`` and your domain is ``foobar.org``, then a line like the following one is known to work (whereas many variations of it may be deemed "incorrrect")::

  127.0.0.1  hurricane.foobar.org hurricane localhost.localdomain localhost

provided of course that, still in that file, you have not also a declaration such as::

  192.168.0.5 hurricane.foobar.org hurricane

(setting one's IP shall better be done in one's profile in ``/etc/netctl``, right?)



Testing & Troubleshooting
-------------------------

In order to **quick-check** whether long-name connectivity is available and to rule out the most obvious culprits, open two terminals.

In the first::

 # Check (with root permissions) that the firewall rules are safe; for example:
 $ iptables -nL
 Chain INPUT (policy ACCEPT)
 target     prot opt source               destination

 Chain FORWARD (policy ACCEPT)
 target     prot opt source               destination

 Chain OUTPUT (policy ACCEPT)
 target     prot opt source               destination

 # Just to be on the safer side for this test:
 $ killall beam.smp epmd

 # Then launch the target first node:
 $ ERL_EPMD_PORT=4032 erl -name n1 -setcookie aa
 Erlang/OTP 23 [erts-11.1.4] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

 Eshell V11.1.4  (abort with ^G)
 (n1@hurricane.foobar.org)1>


In the second terminal, try to find the previous node::

 $ ERL_EPMD_PORT=4032 erl -name n2 -setcookie aa
 Erlang/OTP 23 [erts-11.1.4] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

 Eshell V11.1.4  (abort with ^G)
 (n2@hurricane.foobar.org)1> net_adm:ping('n1@hurricane.foobar.org').
 pong


If you see ``pang`` here, run to the nearest altar and make a sacrifice to any Distribution God you may believe in (Norse ones being presumably the most effective here), and apply the hints listed in the `Enabling the Interconnection of Erlang nodes`_ section.
