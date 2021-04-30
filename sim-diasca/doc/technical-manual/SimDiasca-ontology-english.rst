:raw-latex:`\pagebreak`

---------------------------------
Let's Start With A Short Ontology
---------------------------------


What is an Ontology?
====================

As defined `here <http://en.wikipedia.org/wiki/Ontology_%28information_science%29>`_, *an ontology formally represents knowledge as a set of concepts within a domain, and the relationships among those concepts. It can be used to reason about the entities within that domain and may be used to describe the domain.*

We aim here to express a Sim-Diasca ontology about (discrete-time) simulations of complex systems, so that we can define relevant terms and share them once for all.


Simulation Concepts in Relation
===============================

Many additional relations could be defined, we just concentrated on the most influencial ones. Each concept is defined in turn at the bottom of this diagram.

:raw-latex:`\includegraphics[scale=0.5]{SimDiasca-ontology-english.png}`


Simulation Terms Defined
========================

The ontology is currently in a simpler form, the one of a glossary (terms, sorted alphabetically, and their definition).

The examples illustrating the definitions are taken from a hypothetical simulation case involving preys and predators (a typical use-case in this simulation domain).



Abstraction
	An ``Abstraction`` is a simplification of elements of the
	``Simulated World`` regarding traits of interest, i.e. how the
	``Simulation`` should represent a part of the various elements to be
	simulated. An ``Abstraction`` is a ``Schedulable``.
	Typically an ``Abstraction`` is either a ``Modlet`` or a
	``Scenario``.


Actor
	An ``Actor`` is an instance of a ``Modlet``.
	For example: "*This particular actor corresponds to this specific
	prey that we wanted to introduce in previous simulations, the one
	that is bound to be eaten first due to its starting location.*"


Experiment settings
	An ``Experiment settings`` is a set of runtime static parameters
	that applies to all instances of a given ``Abstraction`` in the
	context of a given experiment (i.e. ``Simulation``). Indeed, when
	constructed, ``Abstractions`` may accept a set of parameters that
	will be common to all their instances for that simulation. It is a
	way of overriding, at the level of a given simulation, the internal
	constants ruling the ``Abstraction`` behaviours. A key point is that
	these parameters are *static*, i.e. that, on a given simulation,
	they apply to all instances of a given ``Abstraction``, while
	another simulation may rely on other experiment settings (the
	default ones or other overriding parameters).
	``Experiment settings`` are known in some simulations as
	"*strategies*".
	For example an ``Experiment settings`` may define various constants
	that determine the efficiency of the technical elements making up a
	kind of photovoltaic panel. All instances of the corresponding
	``Modlet`` will share these ``Experiment settings``, but these
	settings may differ from one simulation to another (ex: a given
	technology could be made more cost-effective in another simulation
	relying on a different context).


Model
	``Model`` designates primarily an overall system (often the
	``Target system`` as a whole), like a city - even if in the
	simulation there is no specific instance corresponding directly to
	that name (ex: not even a ``City`` ``Modlet`` defined; for example a
	city could be then represented in such a simulation just as a set of
	building instances instead).
	By extension a ``Model`` may also be used as a synonym of ``Modlet``
	(see next entry), an actual simplification of an element of the
	``Target system``. It is a type (like a class), from which instances
	(``Actors``) can be created; it then defines notably their state and
	behaviour.
	For example, "*I am quite happy of the current predator model, it
	behaves nicely compared to what experience tells us. See how its
	instances are chasing these preys?*"
	As the target system, which is reproduced thanks to a collection of
	modlet instances (second acception of ``Model``), can be itself seen
	as a ``Model`` (as defined in the first acception), the term may be
	ambiguous in some contexts. In these cases, ``Modlet`` should be
	used only to designate the type of actors (the implemented,
	disaggregated parts of this overall model), while ``Model`` would be
	used for more conceptual descriptions (that are not instantiated as
	such).
	When there is really little ambiguity, ``Model`` may be used to
	refer to ``Modlet``, but it is not encouraged.


Modlet
	A ``Modlet`` is an atomic sub-model of a simulation, i.e. a modeling
	element that cannot be further simplified and corresponds directly
	to a type that can be instantiated.
	In following example there is no specific city instance as such:
	“\ *This tiny model of San-Francisco involves only a few building
	modlets, some road modlets and three bridge modlets.*\ ”
	A set of ``Modlets`` may form a more general ``Model``, possibly the
	overall one.


Probe
	A ``Probe`` is a producer of simulation results, based on
	information made available by a set of ``Abstractions``.
	For example, "*I need to monitor how many preys are killed by
	predators of this type. I will add a probe to track this
	information*".
	A probe may be fed by multiple abstractions.


Scenario
	A ``Scenario`` is a representation of the elements in the
	``Simulated world`` that are outside of the ``Target system`` but
	that may influence it and/or possibly be influenced by it.
	For example, "*My monsoon scenario, once combined to your epizootic
	scenario, shows unexpected results in terms of prey population.*"
	The set of all ``Scenario`` instances form the ``System context``.


Scenario instance
	A ``Scenario instance`` is simply an instance of a ``Scenario``,
	like an ``Actor`` is an instance of ``Modlet``.
	``Scenario instances`` and ``Actors`` are technically managed the
	same way; their difference lies in the viewpoint of the author of
	the simulation, who can arbitarily set the limit between the system
	of interest (the ``Target system``) and its surroundings
	(``System context``).


Schedulable
	A ``Schedulable`` designates any element that is driven, time-wise,
	by a ``Time manager``, like ``Abstractions``.


Simulated world
	The ``Simulated world`` corresponds to the full set of
	``Abstractions`` that are to be evaluated through the simulation;
	the ``Simulated world`` is the union of the ``Target system`` and
	its context (i.e. the ``System context``), i.e., respectively,
	``Actors`` and ``Scenario`` instances.
	For example, "*The simulated world is the whole savannah, including
	fauna and flora.*"


Simulation
	A ``Simulation`` corresponds to the execution of a
	``Simulation case``. It is an experiment typically done for
	decision-making.


Simulation case
	A ``Simulation case`` is the specification of a ``Simulation``. This
	encompasses:
	- Technical settings, like the properties to be enforced for this
	simulation (ex: reproducibility), the time-step to be used or the
	list of the eligible computing hosts
	- Domain-specific settings, like the description of the initial
	state of the simulation or its various termination criteria
	For example, "*This predator / prey simulation case, which must run
	on these following 3 computers, will rely on a 20 millisecond
	timestep; it takes place in this kind of savannah and starts with 2
	predators (that can mate) and 15 preys, on these specified
	locations. This is the weather scenario that I want to apply. I want
	the simulation to stop whenever either all animals are extinct or
	the elapsed duration (in simulation time) reaches one century. In
	terms of results, I want the simulation to keep track only of the
	predator population and of the number of preys that are born in its
	course.*"


Simulation engine
	The ``Simulation engine`` is, among other roles, in charge of managing the virtual time of a simulation; by scheduling the various ``Schedulable`` instances, it allows to enforce the properties expected from the simulation while making its (virtual) time progress, preferably in an efficient way. Typically a simulation engine includes a ``Time manager`` service, which is a key feature thereof.


Simulation inputs
	The ``Simulation inputs`` correspond to the data that is needed for
	the simulation to be ready to start. This encompasses notably the
	description of its initial state, i.e. the data allowing defining
	the state of the whole simulated world when the simulation is to
	begin.


Simulation outputs
	The ``Simulation outputs`` regroup the simulation results and the
	simulation traces (we consider here only *successful* simulations -
	failed ones output errors and traces).


Simulation results
	The ``Simulation results`` are the main by-product of a simulation,
	if not its only purpose. These are data that are computed based on
	information provided by ``Actors`` and ``Scenario`` instances, at
	various points in ``Simulation time``, and that are aggregated and
	managed by ``Probes``.


Simulation time
	There are at least two kinds of time that are useful in the context
	of a simulation: the wall-clock (user) time (i.e. the one we,
	humans, all experience) and the ``Simulation time`` that is known of
	the simulation, i.e. of actors and scenario instances (a.k.a. the
	virtual time the ``Simulated world`` is plunged into; at least a
	discretised version thereof). By default there is no link between
	the wall-clock time and the simulation one.


Simulation traces
	The ``Simulation traces`` correspond to the time-stamped (in
	wall-clock or simulation time) information emitted by the actors,
	scenario instances and the technical agents of the simulation,
	during its course (ex: probes, service providers). These are not
	simulation results, they are a technical means of following the
	events that happen in the course of a simulation, for example in
	order to troubleshoot the behaviour of models.


System context
	The ``System context`` gathers everything in the ``Simulated world``
	that is not the ``Target system``. It is made of all the
	``Scenario`` instances.


Target system
	The ``Target system`` is the system of interest, whose mode of
	operation is reproduced thanks to a set of models. Generally such a
	target system cannot be simulated without its context, i.e. parts of
	the reality that do not belong to the target system but must be
	taken into account to simulate it.
	For example, "*The target system is made of the preys and the
	predators. Its context is the weather and the savannah (vegetation
	and relief).*"
