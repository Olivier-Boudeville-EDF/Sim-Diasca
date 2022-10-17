[![Erlang CI](https://github.com/Olivier-Boudeville-EDF/Sim-Diasca/actions/workflows/erlang-ci.yml/badge.svg)](https://github.com/Olivier-Boudeville-EDF/Sim-Diasca/actions/workflows/erlang-ci.yml)
# Sim-Diasca

![](/sim-diasca/doc/common-elements/edf-related/sim-diasca.png)


## Purpose of this repository

This is the official repository of the Sim-Diasca simulation engine, which is developed and released by [EDF R&D](https://www.edf.fr/en/the-edf-group/inventing-the-future-of-energy/r-d-global-expertise).

The purpose of this public repository is to complement the [official page](https://www.edf.fr/en/the-edf-group/inventing-the-future-of-energy/r-d-global-expertise/our-offers/simulation-softwares/sim-diasca) and to share with the community the code and the [documentation](http://olivier-boudeville-edf.github.io/Sim-Diasca/) of Sim-Diasca.


## Sim-Diasca in a nutshell

Sim-Diasca (*Simulation of Discrete Systems of All Scales*) is a **discrete-time simulation engine** aiming for maximum concurrency, relying on a mode of operation that is both parallel and distributed.

The engine focuses notably on scalability, in order to handle simulation cases that may be very large (potentially involving millions of interacting instances of models), while still preserving essential simulation properties, like causality, total reproducibility and some form of ergodicity.

The simulation engine is implemented in the [Erlang](http://erlang.org) programming language and its execution platform of choice is GNU/Linux.

Sim-Diasca has been released since 2010 by EDF R&D under the LGPL licence.

<!--

For more information, please refer to the *Sim-Diasca Technical Manual*.

Until the various elements are available online, please [contact us](https://www.edf.fr/en/the-edf-group/world-s-largest-power-company/activities/research-and-development/scientific-communities/simulation-softwares?logiciel=10832) for an archived copy of the last stable version and its related documentation. -->


## Sim-Diasca documentation

Please refer to the full [Sim-Diasca official documentation](http://olivier-boudeville-edf.github.io/Sim-Diasca/) for further information.

One may also have a look at the [Sim-Diasca wiki](https://github.com/Olivier-Boudeville-EDF/Sim-Diasca/wiki).

This branch corresponds to the current stable version (2.4.4) of Sim-Diasca.
