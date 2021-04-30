:raw-latex:`\pagebreak`


------------------------------------
Sim-Diasca Implementation Spotlights
------------------------------------

In this section, various technical pieces of information (not of interest for users, but relevant for engine maintainers) will be discussed regarding the *mode of operation* of Sim-Diasca.


About Erlang Nodes and Simulation Identifiers
=============================================


How Many Erlang Nodes Are Involved in a Simulation?
---------------------------------------------------

By default (unless the case specifies otherwise), only the local host is involved, yet there are two VMs (Erlang virtual machines) running then: the one of the user node, and the one of a (local) computing node.

In the general case, distributed simulations running on ``N`` hosts will involve by default ``N+1`` nodes: one user node (on the user host) and ``N`` computing nodes (including one on the user host).

See the ``computing_hosts`` field in the ``deployment_settings`` record (defined in ``class_DeploymentManager.hrl``) for further options.



How Are Launched the Erlang nodes?
----------------------------------

By default, `long names <http://erlang.org/doc/reference_manual/distributed.html>`_ are used for all Sim-Diasca related nodes.

To avoid any possible cross-talk, we have to ensure that a simulation (live or post-mortem) remains fully self-contained and immune to interferences (notably from other simulations that would be run in parallel or afterwards, i.e. both at runtime and regarding the on-disk generated information).

Erlang ensures (thanks to EPMD) that, on any given host, regardless of the Erlang installations, of their version, of the UNIX users involved, no two nodes can bear the same (long) name (otherwise the second node will fail to launch).

The Sim-Diasca **user node** is launched from the generic makefile infrastructure, resulting in the ``myriad/src/scripts/launch-erl.sh`` helper script to be run with proper parameters.

The name of such a user node (made a distributed node programmatically, see `Node Naming`_) will follow the following format: ``Sim-Diasca-<name of the test or case>-<user name>-<simulation instance identifier>-user-node``.

For example, a case named ``soda_deterministic_case.erl`` run by a user ``john`` and relying on a `Simulation Instance Identifier`_ equal to ``43416933`` will result in a user node named ``Sim-Diasca-soda_deterministic_case-john-43416933-user-node``.

As for the (per-host) **computing nodes**, they will be launched each from their respective ``class_ComputingHostManager`` instance (driven by the ``class_DeploymentManager`` singleton created at start-up), and their name will follow that format::

  Sim-Diasca-<name of the test or case>-<user name>-<simulation instance
	identifier>-computing-node-on-<hostname>

The same example, running on host ``volt``, will thus result in a computing node to be created under the name::

  Sim-Diasca-soda_deterministic_case-john-43416933-computing-node-on-volt



.. _`Simulation Instance Identifier`:

What is the *Simulation Instance Identifier*?
---------------------------------------------

This information, whose shorthand is ``SII``, is a string made of alphanumerical characters, dashes (``-``) and underscores (``_``), and is meant to guarantee the uniqueness of a given instance of a simulation (i.e. no two simulations, run simultaneously or not, should ever bear the same SII).

By default the SII is automatically generated and managed by Sim-Diasca. It is based on a `UUID <https://en.wikipedia.org/wiki/Universally_unique_identifier>`_ (*Universally unique identifier*) determined at start-up [#]_. An example of a UUID is ``4f8fbacd-93d2-487d-86ff-23f75339c191``.

.. [#] A UUID is obtained thanks to any system-provided ``uuidgen`` command, otherwise our own implementation is used for that (refer, in the **Myriad** layer, to ``basic_utils:generate_uuid/0``).


As the acronym suggests, there is very little chance that two simulation instances may succeed in drawing the same UUID, so they provide an adequate guarantee of uniqueness.

These UUIDs may be deemed a bit too long to be very tractable for humans, so the engine shortens them thanks to hashing (thanks to ``erlang:phash2/1``), hopefully preserving a sufficient part of their underlying unicity.

The hash value of our example UUID corresponds to the ``43416933`` identifier in the previous section.

While such an *automatic identification* is convenient and transparent, for some uses it is possible and even desirable not to rely on such a randomly determined identifier, but to use instead one that is transmitted by a third party (typically if the engine is embedded in a simulation platform able to provide its own identifiers).

Then Sim-Diasca is able to use such an externally-obtained identifier thanks to its ``--simulation-instance-id`` command line option.

As a result, such a platform may run a simulation case with::

 $ make my_foobar_case
	 CMD_LINE_OPT="--batch --simulation-instance-id 117"

Then the specified SII will be used instead of the one that would be determined internally, at runtime, notably to designate:

- the user and computing nodes (ex: ``Sim-Diasca-Foobar_Case-john-117-user-node@volt``)
- the simulation result tree (ex: ``Foobar_Case-on-2016-6-14-at-17h-07m-28s-by-john-117``)
- the temporary directories (ex: ``/tmp/sim-diasca-Foobar_Case-2016-6-14-at-15h-12m-18s-117``)
- the simulation trace file (ex: ``Foobar_Case-john-117.traces``)


.. _`Node Naming`:

How Erlang nodes are named?
---------------------------

So the SII is either user-supplied or determined at runtime, by the engine itself. As a result, the name of the user node cannot be determined statically in the general case (the node must run so that it may draw its UUID then determine its SII).

Knowing that additionally a node created as a distributed one (here with the "long names" command-line option) cannot be renamed (``net_kernel:stop/0`` not allowed), the only relevant design is, from the Sim-Diasca layer onward (lower ones relying on long names) to start the user-node in non-distributed mode (thanks to the ``--nn`` option of ``launch-erl.sh``), establish the name it shall bear, and then only execute ``net_kernel:start/1``.




How Is It Ensured that No Two Simulations Can Interfere?
--------------------------------------------------------

The naming of nodes is a first-level security, which should prevent most accidental collisions.

If ever all other safety measures failed for any reason, a node naming clash will happen, yet it will be detected and will lead to making the clashing simulations fail, so no silent failure shall be feared.

This protection is obtained thanks to Erlang cookies using transparently the UUID mentioned in the previous section (UUIDs will be used in all cases for cookies, even if a third-party SII is specified - for an increased safety, should clashing SIIs be provided by mistake).
