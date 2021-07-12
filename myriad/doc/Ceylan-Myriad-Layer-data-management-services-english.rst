

:raw-latex:`\pagebreak`

.. _`Data-Management`:


Data-Management Services
========================

.. _`table type`:

Some generic **data-structures**, in addition to the ones provided built-in with Erlang, are defined in ``myriad/src/data-management``, notably:

- a set of **associative tables**, with a rather complete interface (to create,
  update, enrich, search, query, list, map, fold, merge, display, etc.) and
  various implementations thereof, tests and benchmarks, in:

.. code:: erlang

  {hash,lazy_hash,list_,tracked_hash,map_hash}table.erl

- a ``table`` **pseudo-module** to abstract them out from the user's point of view; note that this is a fully virtual module, in the sense that neither ``table.erl`` nor ``table.beam`` exist (the ``Myriad`` parse transform replaces a call to the ``table`` module by, currently, a call to the ``map_table`` module; so, in order to consult the ``table`` API, please refer to ``map_table.erl``)
- a way of **generating a read-only associative table** whose key/value pairs can be read from any number (potentially extremely large) of readers very efficiently (``const_table.erl``)
- a specific support for **other datatypes** (``pair.erl``, ``option_list.erl``, ``preferences.erl``, ``tree.erl``; also: ``set_utils.erl`` and ``ring_utils.erl``)
- a built-in very basic support for the `CSV <https://en.wikipedia.org/wiki/Comma-separated_values>`_ (see ``csv_utils``) and `RDF <https://en.wikipedia.org/wiki/Resource_Description_Framework>`_ (see ``rdf_utils``) conventions is provided
- an optional support (as depending on third-party prerequisites) for JSON, HDF5, Python and SQLite; refer to the `Myriad-level Third-Party Dependencies`_ section for further information


Finally, the ``void/0``, ``maybe/1`` and ``fallible/{1,2}`` pseudo-builtin types are supported (thanks to the Myriad parse-transform).
