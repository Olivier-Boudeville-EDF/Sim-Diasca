:raw-latex:`\pagebreak`

--------------------------
Sim-Diasca Troubleshooting
--------------------------


First Of All: Did You Read The Manuals?
=======================================

:raw-html:`<center><img src="xkcd-rtfm.png"></img></center>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-rtfm.png}`

Besides this document, a Sim-Diasca user should definitively read the *Sim-Diasca Developer Guide*, which is freely available as well. We are not providing ``man`` pages yet.



Troubleshooting Principles
==========================

First at all, everything is done so that:

- the simulation crashes as soon as a problem occurs (in the engine, in the models and/or in the simulation case), so that there is not silent error
- in debug mode, many additional runtime checkings are performed (ex: with regard to actor scheduling, termination, etc.)


As a consequence, models in development will crash early and often, and the diagnosis should be quite easy to obtain, thanks to the detailed crash information being given.

Each actor having its own sequential stream of instructions, sharing no data, relying only on its state variable and not having any pointer, exchanging only messages according to well-defined procedures should help a lot the debugging.

So, unlike other approaches like this one:

:raw-html:`<center><img src="xkcd-compiler_complaint.png"></img></center>`
:raw-latex:`\includegraphics[scale=0.5]{xkcd-compiler_complaint.png}`


this is a normal and easy process for the model developer to iterate simulation runs again and again (*let is crash* belongs to the Erlang principles), until having complete and correct models.

Of course, the fact that a model does not crash does not necessarily imply it respects its intended behaviour: it is of course part of the work of the model developer to check their implementation against their specification.




Most Common Issues
==================

The most common errors encountered that are not self-explanatory (please have a careful look at the console outputs and, should it be not sufficient, read thoroughly the simulation traces) while using Sim-Diasca are explained and solved here.

They are roughly sorted by chronological order of potential appearance.


:raw-html:`<center><img src="xkcd-computer_problems.png"></img></center>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-computer_problems.png}`



**Issue #1**: ``undefined parse transform 'myriad_parse_transform'``

	When a build recipe fails that way, this usually means that one attempted to compile elements of a layer depending on the ``Myriad`` layer whereas this latter layer is itself not already compiled. One should preferably run ``make`` from the root, to ensure that the full code-base is rebuilt (layers will then be compiled in a relevant order).

	Why is it so? Because a parse-transform (Erlang code modifying how Erlang code is compiled) defined in the ``Myriad`` layer is necessary to compile most of the BEAM files of all layers, hence it must be available prior to any of these builds.



**Issue #2**: ``Protocol: register error``

	If running a simulation and being notified of a strange error like::

		{error_logger,{{2008,11,25},{15,25,6}}, "Protocol: ~p: register
		error: ~p~n", ["inet_tcp",{{badmatch,{error,duplicate_name}},
		[{inet_tcp_dist,listen,1},{net_kernel,start_protos,4},
		{net_kernel,start_protos,3},{net_kernel,init_node,2},
		{net_kernel,init,1},{gen_server,init_it,6}, {proc_lib,init_p,5}]}]}
		[..]

	then it is the symptom that a similarly-named Erlang virtual machine is already running on the same host. Either it is an idle forgotten simulation still opened on another terminal [#]_ (in that case shutting down the corresponding virtual machine will correct the situation), or the user really wants to have two instances of the same simulation run simultaneously. In that case, the two virtual machines should be named differently, knowing that, with the default Sim-Diasca Make rules, the launched virtual machines are named according to the case that is run. For example, to run the simulation case defined in ``simulationAndScenario_test.erl`` from the host ``myhost.foobar.org``, one may just issue ``make simulationAndScenario_run``. The corresponding Erlang virtual machine will be then named ``simulationAndScenario_run@myhost.foobar.org``.

.. [#] This can happen if for example you issued ``CTRL-Z`` then put that task in the background (``bg``) and forgot that it was still running.

	Note that Sim-Diasca includes various mechanisms to ensure that no two runs can silently interfere by mistake (ex: using UUID-based cookies, uniquely named directories according to case, user and a generated identifier, etc.).



**Issue #3**: ``Can't set long node name! Please check your configuration``

	Such a problem may be reproduced simply by running on a given host::

	  $ erl -name my_test

	Instead of running the expected VM, like in::

	  Erlang/OTP 21 [erts-10.3] [source] [64-bit] [...]

	  Eshell V10.3  (abort with ^G)
	  (my_test@hurricane.foobar.org)1>

	the VM launcher may report that no long node naming can be used.

	This may happen whenever the network configuration of the local host is not consistent, at least from the point of view of the Erlang virtual machine. More specifically, it can happen if in the ``/etc/hosts`` file the first name to appear for the local host is not the expected proper FQDN (*Fully-Qualified Domain Name*) and/or when the domain is not correctly specified.

	For example, supposing that in ``/etc/resolv.conf`` a domain is specified as ``domain localdomain`` and that the local hostname is ``foo``, then a line in ``/etc/hosts`` like::

	  127.0.0.1 localhost.localdomain localhost foo.bar.org foo

	should be corrected into::

	  127.0.0.1 foo.bar.org foo localhost.localdomain localhost

	Typically, one of the simplest ``/etc/hosts`` could be in this context::

	  127.0.0.1 localhost.localdomain localhost
	  ::1       localhost
	  127.0.1.1 foo.localdomain foo


	Ping your local host, use ``hostname``, ``hostname -f`` and/or ``hostnamectl`` to check that the name resolution is correctly set. See also the related note about ``Domain configuration`` in the ``Sim-Diasca Installation Guide``.

	If you have for example a laptop making use of DHCP servers that assign over time different host/domain names and you find it impractical, you may reintroduce a stable naming (to be used at least by Sim-Diasca) by adding at the end of your ``/etc/hosts`` a line like::

	 127.0.2.1 a_host_name.a_domain_name a_host_name

  (where ``a_host_name`` and ``a_domain_name`` can be any network names of your choice)

	Then, in ``myriad/GNUmakevars.inc``, the FQDN information shall be set statically, accordingly by editing the corresponding section with::

	 FQDN := a_host_name.a_domain_name

(before ``ifdef FQDN [...]``)



**Issue #4**: The Deployment of a Sim-Diasca Module apparently failed

The corresponding symptom is an exception being thrown during deployment and including::

  {module_deployment_failed,SOME_MODULE,...


This may happen when running distributed simulations whereas hostname resolution is somehow failing.

For example, we encountered sometimes faulty network configurations (ex: w.r.t. to a stale domain name) where a host contacted as ``foo.bar.org`` was responding as ``foo.other.org``, and thus was never reported as available.

In other cases, a computing host was designated (either in a host file or directly in the simulation case) not, as expected, by its name (preferably FQDN) but, incorrectly, by its IP address (which is disallowed, see the ``computing_hosts`` field of the ``deployment_settings`` record).


**Issue #5**: Execution seems to be blocked right after having been triggered.

	This may happen (albeit now on very rare cases; or, possibly, never anymore) if using a virtualized environment (ex: VMWare or VirtualBox). Indeed there used to be, with some unspecified configurations, a general problem related to timers and message receiving, and apparently Sim-Diasca was not the culprit here (as unrelated applications were affected similarly). Erlang was maybe not guilty either, as possibly related issues were reported on the VMWare side.

	Anyway, because of these problems and of the incurred performance penalty, *the use of virtualized environments should be avoided* here; at least one should develop and test one's simulation on a real hardware before considering running it in a virtualized form.

	Another cause of a launched computing node not being found and resulting in a time-out might be an inconsistent name resolution (see issue #3).

	For example, beware of specifying in ``/etc/resolv.conf`` a wrong domain in the ``domain`` entry (ex: ``bar.org`` instead of ``foo.org``) . Otherwise your user node may try to reach ``A_COMPUTING_NODE_NAME@HOST.foo.org`` whereas this one will believe its own name actually is ``A_COMPUTING_NODE_NAME@HOST.bar.org`` and thus will not respond - leading to Sim-Diasca freezing at start-up before automatically timing-out. If in doubt and having the relevant permissions, one may comment-out the ``domain`` information, at least for a first troubleshooting.



**Issue #6**: At least one computing node times-out because it did not receive on time (from the user node) the deployment archive.

	The default deployment time-out is supposedly sufficient for most configuration settings.

	If for example relying on very slow hard-disks and/or having defined extra simulation data to deploy whose size exceeds a few dozens megabytes, then maybe indeed you may need to increase your deployment time-out, at least for this simulation case.

	For that, see the ``maximum_allowed_deployment_duration`` field of the ``deployment_settings`` record (defined in ``class_DeploymentManager.hrl``, in the ``sim-diasca/src/core/src/deployment`` directory).

	Such larger simulation archives may also result from user-level errors. A typical mistake was to run the Erlang installation script ``install-erlang.sh`` directly from its location (in ``myriad/conf``): then the full build tree of Erlang/OTP could still reside in this latter directory. In this case, the deployment manager, when scanning the ``Myriad`` package, would also detect the BEAM files of Erlang/OTP and include them in the simulation archive. Note that a specific checking has been since then introduced so that the specific case of a local build of the Erlang/OTP runtime should be correctly detected, but this issue may arise for other codebases as well.

	Of course including such duplicated BEAMs (as they shall be already available on the computing hosts) is not desirable at all, and results in larger simulation packages bound to trigger a deployment time-out.

	So: just remove then, from the overall Sim-Diasca codebase, all build trees that do not belong there!



**Issue #7**: At start-up, the rebuild of the simulator codebase fails, although the code is correct.

	This may happen if at least one source file (ex: ``myFile.erl``) is being edited without having been saved yet: some editors then create a temporary file like ``~myFile.erl`` or ``.#myFile.erl`` in the same directory. The make system will try to rebuild that file, but the compilation will fail necessarily, as this filename will not match the module name. A proper error message should have been sent in the simulation traces.



**Issue #8**: A ``noconnection`` error is triggered in the course of the execution.

	This usually means that at least one of the involved computing nodes unexpectedly crashed. The most likely reason is that its host was exceedingly loaded. This happens typically in the course of the creation of the initial actors: a too large simulation may then result on the exhaustion of the RAM (and, possibly, swap) of at least one computing host, crashing the whole simulation.

	Solution: opt for a less demanding simulation and/or use more hosts, ensuring they have roughly the same level of free resources (knowing that the load balancer tends to even the resource demands across the available hosts).



**Issue #9**: Apparently my newer code does not seem to be taken into account!

   More precisely, some changes to the source code have been made, yet the newer executions seem to correspond to the code that existed before the change rather than to the updated one. Or, more generally, the executed code does not seem to correspond to the specified one.

   This could happen when multiple BEAM versions of the same module can be found from the deployment root. For example, from some subdirectory in the sources, one may have issued ``cp -r foo_directory foo_directory-hidden``, to save temporarily its content while experimenting in-place in ``foo_directory``.

   The problem is that the deployment manager will scan for all BEAMs from the deployment root, and include them in the deployment archive. As a result, on each computing node, any BEAM found in ``foo_directory-hidden`` will be deployed as well and, depending on the code path, ``foo_directory-hidden/a_module.beam`` may be found before ``foo_directory/a_module.beam`` (unfortunately this tends to be often the case). As a consequence, the previous version of the code (the hidden one) would be wrongly executed.

   The solution is to avoid to perform back-ups directly in the source tree (ex: use ``git stash``) or, at the very least, to copy them once all BEAMs have been removed, to avoid that they silently collide.

   Another possible cause of not seeing a change when running Sim-Diasca (at least, not the first time it is then run) is to modify a source file without recompiling it afterwards: Sim-Diasca, during its deployment, will then recompile the whole (thus updating any BEAM file that requires it), yet the previous version of the BEAM may have already been loaded by the user node (and possibly sent over the network to other nodes). These changes would be visible only from the second run, not the first one. To avoid that, one should recompile a module when having modified it - anyway after a change we have to check that the module still compiles, isn't it?



**Issue #10**: My simulation seems to be finished, however it does not return to the shell, and it is still eating a lot of resources for quite long. What's happening?

	It may happen whenever a simulation is executed for a long time and/or with numerous actors, whereas the intensity of trace sendings has not been lowered: although all trace modes write down a trace directly as soon as possible once received, and none, except the PDF mode, incurs long processings at shutdown, nevertheless all trace modes can significantly delay this shutdown phase.

	The reason is that the trace aggregation process (see ``class_TraceAggregator``) could not cope with the speed at which traces are sent by the various emitters, including actors. Thus traces accumulate in the aggregator mailbox, and time is needed for them to be formatted and flushed on disk. Sending too many traces regarding the aggregator speed should be avoided, as accumulating messages in the mailbox may result in a huge RAM consumption, delayed shutdown, and risk that a simulation crash happens whereas the corresponding traces are not written yet.



**Issue #11**: At runtime, an exception like ``{unexpected_ack_from,APid,PidList,ATick,ActorPid}`` is thrown.

   Although it looks as if the engine was faulty, the cause must lie in the code of the class corresponding to the instance ``ActorPid`` refers to: most probably that an updated state was not taken into account into one of its methods, from where an actor message was sent (directly or not, like in the case of the creation of another actor) to the process corresponding to ``APid``.

   Indeed an actor message must have been sent, returning an updated state tracking that sending, whereas a previous state, unaware of that sending, was instead returned to WOOPER by that method. Thus when that actor received the acknowledgement corresponding to the actor message it sent, it does not correspond to any recorded sending, leading to the ``unexpected_ack_from`` exception to be triggered.



**Issue #12**: Simulation runs, but is slow.

   This is a difficult issue to tackle generically. Some slowness are more acceptable than others:

   :raw-html:`<center><img src="xkcd-long_light.png"></img></center>`
   :raw-latex:`\includegraphics[scale=6.0]{xkcd-long_light.png}`

   Most efficient solutions to increase speed are:

   - increase your computing resources (more nodes, more powerful, better network, etc.); check that you are never hitting the swap and, more generally, try to ensure that computing nodes stay well below a high load (performances in that case degrade swiftly)
   - make (a better) use of advanced scheduling (models seldom require all the same evaluation frequency)
   - selectively tune your models (ex: use ``etop`` and the traces to spot the most-demanding ones)
   - switch to more "exotic" solutions, like native compilation or the use of `NIFs <http://erlang.org/doc/tutorial/nif.html>`_ (i.e. *Native Implemented Functions*)
   - ultimately, if at all possible, reduce your problem size
   - improve your algorithms (ex: choose better data-structures):
   :raw-html:`<center><img src="xkcd-algorithms"></img></center>`
   :raw-latex:`\includegraphics[scale=0.5]{xkcd-algorithms.png}`



**Issue #13**: Simulation seems to freeze, or to be surprisingly slow, or more generally does not behave as expected, and I do not want to stick ``io:format`` calls everywhere to understand what is happening.

	If not using the simulation traces either to figure out what is happening, then a good approach could be to connect to the busiest computing nodes (use simply ``top`` on each host) to determine what they are doing; to do so, track in the console the line which reminds the user of the names of the computing nodes and of the simulation cookie, like in::

	  To connect to computing nodes [
	   'Scheduling_scalability_test-boudevil@server1',
	   'Scheduling_scalability_test-boudevil@server2',
	   'Scheduling_scalability_test-boudevil@server3'], use cookie
	   '1f793a6ba507-d389-2e11-5bd1-2f759320'.

	Then run a new node, connect to the computing node and run ``etop`` to inspect it, like in (maybe exporting ``DISPLAY`` and/or increasing the net tick time can help)::

	  erl -epmd_port 4506 -setcookie '1f793a6ba507-d389-2e11-5bd1-2f759320' -sname inspector
	  (inspector@tesla)1> net_adm:ping(
		'Scheduling_scalability_test-boudevil@server2').
	  pong

	Then hit CTRL-G and enter::

	  --> r 'Scheduling_scalability_test-boudevil@server2'
	  --> j
		1  {shell,start,[init]}
		2* {'Scheduling_scalability_test-boudevil@server2',shell,start,[]}
	  --> c 2
	  (Scheduling_scalability_test-boudevil@server2)1> etop:start().

	(note that the ping is not necessary, just issuing ``r 'Scheduling_scalability_test-boudevil@server2'`` then ``c`` would suffice)

	Then you are able to see something like:

:raw-html:`<center><img src="etop.png"></img></center>`
:raw-latex:`\includegraphics[scale=0.5]{etop.png}`

	You can also run ``observer`` instead::

	 (Scheduling_scalability_test-boudevil@server2)1> observer:start().

	And then we have:

:raw-html:`<center><img src="observer.png"></img></center>`
:raw-latex:`\includegraphics[scale=0.5]{observer.png}`



**Issue #14**: Simulation runs, but result generation fails.

	If the error message mentions ``unknown or ambiguous terminal type``, this means that ``gnuplot`` (used by probes to generate graphical outputs) is (surprisingly enough) *not* able to generate PNG files. Either rebuild it accordingly, or select a gnuplot package in your distribution whose PNG support has been enabled beforehand.



**Issue #15** [unlikely to happen anymore: cleaner script used by default now]: At start-up, no available computing node is found, each candidate node being apparently successfully launched, but not responding.

	This may happen if a previous simulation crashed and thus could not reach its clean-up phase: then pending Erlang nodes, spawned by the previous run, may linger for up to 10 minutes before their automatic shutdown, should the node cleaner script have been unable to remove them, for any reason (which must be *very* uncommon).

	Indeed their node name will be correct, so no attempt to launch them will be made, but the automatic authentication system of the engine, based on security cookies generated from a unique UUID, will prevent the connection to these preexisting nodes. They will thus be deemed unavailable and the simulation will stop, short of being able to rely on any computing node. The solution is then either to remove these pending nodes manually (one effective yet rough means of doing so being ``killall -9 ssh beam beam.smp``, to be run on all computing nodes) or to set the ``perform_initial_node_cleanup`` field in the ``deployment_settings`` record to true (see ``class_DeploymentManager.hrl``) and recompile, in which case any lingering node would be removed when colliding with a newer run; as this latter setting is now the default, this issue should not happen frequently anymore, or at all.



**Issue #16**: Simulation runs and fails with no specific error message in the traces.


	Of course this never happens usually, as it is precisely what we want to avoid.

	Such a behaviour may sum up to a message like::

 --- diasca {2200,2} still in progress at 2021/1/12 10:29:21 ---

being issued, then::

  <----------------
  [emergency] The 'Sim-Diasca-XXX-YYY-128694-computing-node@foobar.org'
  node disconnected, performing an emergency shutdown.
  ---------------->

  <----------------
  [emergency] EXIT message received for <11029.94.0>, whose exit
  reason was: noconnection, terminating now.
  ---------------->



The only case when such a behaviour was reported happened when a model developer created by mistake an infinite recursion [#]_; the induced RAM consumption resulted in instantly having the VM killed by the operating system.

.. [#] Precisely: from a given actor oneway A, instead of calling the version of its mother class with ``wooper:executeOnewayAs/4``, ``wooper:executeOneway/3`` was used, leading to A calling itself indefinitely and exploding the stack.

So chances are that this corresponds to a user implementation error.




**Issue #17** [now unlikely to happen, as ``run_erl`` not used by default anymore]: A simulation case is launched, yet it freezes just after the line telling the trace aggregator has been created, and stays unresponsive until CTRL-C is entered.

	This typically happens after a first failed launch: a virtual machine bearing the same name is already running on the background, thus preventing another one to be launched. The solution may be as simple as a brutal, yet efficient, ``killall -9 beam.smp``.

	This issue used to occur more frequently when the default launching mode was set to rely on ``run_erl`` (rather than a direct start from the command-line). No more ``{error_logger,T,"Protocol: ~tp: the name X@Ya seems to be in use by another Erlang node",["inet_tcp"]}`` was reported by the VM (as discussed in issue #1) yet, strangely enough, the issue discussed here could happen during the mass running of tests (ex: when executing ``make test`` from the root). ``run_erl`` was suspected here.



**Issue #18** Simulation is not reproducible.

	One may run, in reproducible mode, a simulation twice, and unfortunately realize that results happen to differ.

	Whether or not the technical setting changed (ex: local run versus a distributed one), it is abnormal and surely disturbing - moreover it tends to be among the issues that are the most difficult to investigate.

	Of course the engine might be the culprit, yet, for the moment at least, every time that reproducibility was lost, the cause was found to lie in the simulation itself, not in the engine.

	The actual culprit could be the simulation case (ex: see `Randomness Pitfalls`_) or the models. For example the implementor must remind that simulations are executed so that they are reproducible, while PIDs are expected to change from one run to another (a bit like pointers). Hence no operation, except equality testing, shall be performed on them. For reliable, stable actor identifiers, one must use AAIs instead.

.. Note::

  We encountered once a bug at this level, where an actor collected a list of other actors (possibly containing duplicates) and needed to select only one of them (of course in a reproducible manner) by applying some criterion.

  This operation should have been done on their AAI (even if it implied a conversion back and forth their PID), but it had been done on their PID instead. ``list_utils:uniquify/1`` was used to remove first the duplicates; the order of the resulting list was not specified, yet of course it could only be deterministically reordered.

  However this function happens to internally sort the elements of that list; as a consequence, removing duplicates from a list of non-reproducible PIDs resulted in a non-reproducible ordering, and the whole simulation started to behave differently from a run to the next...


	To considerably increase the chances of spotting that different outcomes stem from a simulation (without even looking at the results), now the total number of diascas elapsed and of instance schedulings is displayed on the console. As soon as at least one of them differ from a run to another, the simulation is known to introduce non-reproducible elements, and must be fixed.



**Issue #19** Problem when rebuilding the documentation.

	In some cases the generated documentation encountered problems, typically the table of contents of the technical manual was empty.

	This may come from some tools that insert Unicode characters (typically ``U+FEFF``) that are invisible in most editors (ex: ``emacs``) yet that are not supported by the documentation generators (based on docutils and the RST syntax).

	A solution is to check the output of the documentation tools (ex: ``rubber``) or to use editors like ``nedit``, which displays these characters that shall be removed.




Common Misconceptions
=====================

:raw-html:`<center><img src="xkcd-misconceptions.png"></img></center>`
:raw-latex:`\includegraphics[scale=0.6]{xkcd-misconceptions.png}`


Here is the list of most common misconceptions we spotted:


**Traces are part of simulation results**

  This is not what we promote: we see the distributed traces as a way of monitoring technically a simulation run. Results are typically probe reports. Moreover, for actual large-scale runs, we generally prefer to disable traces.


**The Performance Tracker is the one responsible for the progress information output on the terminal**

  No, the culprit is the `console tracker`_, which is a live lightweight Sim-Diasca built-in, whereas the `performance tracker`_ is an unrelated, optional, more complex post-mortem feature.
