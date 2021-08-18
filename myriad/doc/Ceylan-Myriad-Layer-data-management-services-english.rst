

:raw-latex:`\pagebreak`

.. _`Data-Management`:


Data-Management Services
========================


Datatypes
---------


Some generic **data-structures**, in addition to the ones provided built-in with Erlang, are defined in ``myriad/src/data-management``, and described below.



.. _`table type`:


Table Types
...........


A set of types for **associative tables** is available, each offering a rather complete interface (to create, update, enrich, search, query, list, map, fold, merge, display, etc. a table - or entries thereof) and a different trade-off.

Various implementations are defined (with tests and benchmarks), in:

.. code:: erlang

  {hash,lazy_hash,list_,tracked_hash,map_hash}table.erl

A ``table`` **pseudo-module** is additionally provided, in order to abstract out these various options: the user may then rely exclusively on ``table``, regardless of the actual table type this pseudo-module will be translated at compilation-time.

Indeed the ``table`` module is a fully virtual one, in the sense that neither ``table.erl`` nor ``table.beam`` exists, and that the ``Myriad`` parse transform is to automatically replace a call to this ``table`` pseudo-module by a call to one of the aforementioned table types (and it will do the same replacement in type specifications as well).

By default, such ``table`` calls will be translated to corresponding calls to our ``map_hashtable`` module, which is generally the most efficient one (it relies on the more recently-introduced Erlang ``maps`` built-in datatype, which ``table`` now favors).

As a result, in order to consult the ``table`` API, please refer to ``map_hashtable.erl``.

Such a default implementation may be overridden on a per-module basis, thanks to a ``table_type`` define.

For example, specifying ``-table_type(list_table).`` will result in the current module to translate ``table`` to ``list_table``, instead of the default ``map_hashtable``.


.. _`const table`:

Finally, a way of **generating read-only associative tables** whose key/value pairs can be read very efficiently from any number (potentially extremely large) of readers (processes) is provided with ``const_table.erl``.

No ETS table, replication (ex: per-process table copy) or message sending is involved: thanks to meta-programming, a module is generated on-the-fly, exporting as many functions as there are different keys in the table of interest. Calling a function corresponding to a key returns its associated value.

More precisely, a module name (ex: ``foobar``) and a ``table:table(atom(), any())`` instance shall be provided to ``const_table:generate/2``; then, for each key/value pair in the specified table (ex: ``{baz, 42.0}``), a 0-arity function is generated and exported in that module, as if we had:

.. code:: erlang

  -module(foobar).

  [...]

  -export([baz/0]).

  -spec baz() -> term().
  baz() ->
	42.0.


Then third-party code can call for example ``foobar:foo()`` and have ``42.0`` returned. This is presumably the most efficient way of sharing constants in Erlang.

Note that no actual module file is generated (ex: no ``foobar.beam`` file is ever written in the filesystem): the operation remains fully in-memory (RAM).

Keys must be atoms, and the table of interest shall be immutable (const), even if, thanks to hot code upgrade, one may imagine updating the table at will, having any number of successive versions of it.

Generating a table of the same name more than once should be done with care, as if a given table is generated thrice, the initial table would first switch from "current" to "old", and then would be removed. Any process that would linger in a function of this module would then be terminated (see `code replacement <http://www.erlang.org/doc/reference_manual/code_loading.html>`_). However, due to the nature of these tables (just one-shot fully-qualified calls, no recursion or message-waiting construct), this is not expected to happen.


Other Datatypes
...............

They include ``pair.erl``, ``option_list.erl``, ``preferences.erl``, ``tree.erl``.

One may also refer to ``set_utils.erl`` and ``ring_utils.erl``.


Pseudo-Builtin Types
....................

Such types, as ``void/0`` (for functions only useful for their side-effects - this happens!), ``maybe/1`` (``maybe(T)`` is either ``T`` or ``undefined``), and ``fallible/{1,2}`` (an operation either is successful and returns a result, or returns an error) are supported, thanks to the Myriad parse-transform.



File Formats
------------


Basic File Formats
..................


A built-in very basic support for the `CSV <https://en.wikipedia.org/wiki/Comma-separated_values>`_ (see ``csv_utils``) and `RDF <https://en.wikipedia.org/wiki/Resource_Description_Framework>`_ (see ``rdf_utils``) conventions is provided.


Most Usual, Standard File Formats
.................................

An optional support (as it depends on third-party prerequisites) is proposed for:

- JSON
- HDF5
- SQLite

Refer to the `Myriad-level Third-Party Dependencies`_ section for further information.


.. _etf:


For Pure Erlang uses: the ETF File Format
.........................................

For many needs in terms of Erlang internal data storage (ex: regarding configuration settings), we recommend the use of the file format that `file:consult/1 <https://erlang.org/doc/man/file.html#consult-1>`_  can directly read, that we named, for reference purpose, ``ETF`` (for *Erlang Term Format*).

This is just a text format for which:

- a line starting with a ``%`` character is considered to be a comment, and is thus ignored
- other lines are terminated by a dot, and correspond each to an Erlang term (ex: ``{base_log_dir, "/var/log"}.``)

See `this example <https://github.com/Olivier-Boudeville/us-common/blob/master/priv/for-testing/us.config>`_ of a full ETF file.

A basic support for these ETF files is available in ``file_utils:{read,write}_etf_file/*``.


If expecting to read UTF-8 content from such a file, it should:

- have been then opened for writing typically while including the ``{encoding,utf8}`` option, or have been written with content already properly encoded (maybe more reliable that way)

- start with a ``%% -*- coding: utf-8 -*-`` header


