

===================================
Overview of the Dataflow Unit Tests
===================================

Created on Thursday, April 12, 2018.

-----------
Description
-----------


These unit tests try to define a minimal number of minimalistic elements that are nevertheless sufficient to recreate and showcase specific contexts of interest for the purpose of testing separately the dataflow mechanisms.


-----------
Conventions
-----------

Following transverse conventions apply by default:

 - each tick lasts for 1 second
 - the simulation is to last for 50 ticks


-------------
Test elements
-------------

A ``BaseTestDataflowObject``, named ``MyBaseTestDataflowObject`` (``Obj1``, in short), has two (dataflow) attributes:

- ``foo``, of type ``integer``
- ``bar``, of type ``float``


A ``BaseTestProcessingUnit``, named ``MyBaseTestProcessingUnit`` (``PU1``, in short), has for activation policy ``activate_on_new_set`` and has:

- one input port, ``my_input_port``, of type ``integer``
- one output port, ``my_input_port``, of type ``integer``



----------------
Known Unit Tests
----------------


``reemission_on_connection_test``
=================================

Used to check whether output values are re-emitted as expected on new connections.

Actions:

1. Initially, directly from the simulation case:

 - a ``BaseTestDataflowObject`` instance (``Obj1``) is created, its ``foo`` attribute is set to ``1``
 - a ``BaseTestProcessingUnit`` instance (``PU1``) is created

2. at tick #5: ``Obj1:foo`` is set to ``42``

3. at tick #10: a channel is created (by the ``BaseUnitManager``) between ``Obj1:foo`` and ``PU1:my_input_port``

We expect ``PU1:my_input_port`` to be set to ``42`` from tick #10 onward.
