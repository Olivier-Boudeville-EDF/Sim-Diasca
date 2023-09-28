:raw-latex:`\pagebreak`

SQL support
===========


About SQL
---------

Some amount of `SQL <https://en.wikipedia.org/wiki/SQL>`_ (*Structured Query Language*) support for relational database operations is provided by the ``Myriad`` layer.

As this support relies on an optional prerequisite, this service is disabled by default.



Database Backends
-----------------

To perform SQL operations, a corresponding software solution must be available.

Two SQL database backends have been preferred here: **SQLite 3** for lighter needs, and **PostgreSQL** for heavier ones, notably whenever having to connect to a third-party instance thereof.



SQLite 3
........


SQLite Basics
*************

The `SQLite 3 <https://www.sqlite.org/about.html>`_ library provides a self-contained, serverless, zero-configuration, transactional SQL database. It is an embedded SQL database engine, as opposed to server-based ones like `PostgreSQL <https://en.wikipedia.org/wiki/PostgreSQL>`_ or `MariaDB <https://en.wikipedia.org/wiki/MariaDB>`_.

It can be installed on Debian thanks to the ``sqlite3`` and ``sqlite3-dev`` packages, ``sqlite`` on Arch Linux.

We require version 3.6.1 or higher (preferably: latest stable one). It can be checked thanks to ``sqlite3 --version``.

Various related tools are very convenient in order to interact with a SQLite database, including ``sqlitebrowser`` and ``sqliteman``.

On Arch Linux, one can thus use: ``pacman -Sy sqlite sqlitebrowser sqliteman``.

Testing the backend as it is:

.. code:: bash

 $ sqlite3 my_test

.. code:: sql

 SQLite version 3.13.0 2016-05-18 10:57:30
 Enter ".help" for usage hints.
 sqlite> create table tblone(one varchar(10), two smallint);
 sqlite> insert into tblone values('helloworld',20);
 sqlite> insert into tblone values('my_myriad', 30);
 sqlite> select * from tblone;
 helloworld|20
 my_myriad|30
 sqlite> .quit

A file ``my_test``, identified as ``SQLite 3.x database``, must have been created, and can be safely removed.



SQLite Binding
**************

This database system can be directly accessed (as a client) thanks to an Erlang binding.

Two of them have been identified as good candidates:

- `erlang-sqlite3 <https://github.com/alexeyr/erlang-sqlite3>`_: seems popular, with many contributors and users, actively maintained, based on a ``gen_server`` interacting with a C-node, involving only a few source files
- `esqlite <https://github.com/mmzeeman/esqlite>`_: based on a NIF, so more able to jeopardize the stability of the VM, yet potentially more efficient

Both are free software.

We finally preferred ``erlang-sqlite3``.

By default we consider that this backend has been installed in ``~/Software/erlang-sqlite3``. The ``SQLITE3_BASE`` variable in ``myriad/GNUmakevars.inc`` can be set to match any other install path.

Recommended installation process:

.. code:: bash

 $ mkdir ~/Software
 $ cd ~/Software
 $ git clone https://github.com/alexeyr/erlang-sqlite3.git
 Cloning into 'erlang-sqlite3'...
 remote: Counting objects: 1786, done.
 remote: Total 1786 (delta 0), reused 0 (delta 0), pack-reused 1786
 Receiving objects: 100% (1786/1786), 3.24 MiB | 570.00 KiB/s, done.
 Resolving deltas: 100% (865/865), done.
 Checking connectivity... done.
 $ cd erlang-sqlite3/
 $ make
 rm -rf deps ebin priv/*.so doc/* .eunit/* c_src/*.o config.tmp
 rm -f config.tmp
 echo "normal" > config.tmp
 ./rebar get-deps compile
 ==> erlang-sqlite3 (get-deps)
 ==> erlang-sqlite3 (compile)
 Compiled src/sqlite3_lib.erl
 Compiled src/sqlite3.erl
 Compiling c_src/sqlite3_drv.c
 [...]


Testing the binding:

.. code:: bash

  make test
  ./rebar get-deps compile eunit
  ==> erlang-sqlite3 (get-deps)
  ==> erlang-sqlite3 (compile)
  ==> erlang-sqlite3 (eunit)
  Compiled src/sqlite3.erl
  Compiled src/sqlite3_lib.erl
  Compiled test/sqlite3_test.erl
  ======================== EUnit ========================
  module 'sqlite3_test'
	sqlite3_test: all_test_ (basic_functionality)...[0.002 s] ok
	sqlite3_test: all_test_ (table_info)...ok
	[...]
	sqlite3_lib: delete_sql_test...ok
	sqlite3_lib: drop_table_sql_test...ok
	[done in 0.024 s]
	module 'sqlite3'
  =======================================================
  All 30 tests passed.
  Cover analysis: ~/Software/erlang-sqlite3/.eunit/index.html

Pretty reassuring.



PostgreSQL
..........


PostgreSQL Basics
*****************

`PostgreSQL <https://en.wikipedia.org/wiki/PostgreSQL>`_ is a well known, free and open-source (client/server) *Relational Database Management System* (RDBMS) emphasizing extensibility and SQL compliance. It is designed to handle a range of workloads, from single machines to larger services with many concurrent users.

This object-relational database can be enriched to support other datatypes, such as geographic objects with `PostGIS <https://en.wikipedia.org/wiki/PostGIS>`_.

It can be installed on Debian thanks to the ``postgresql-client`` package (and the database itself with the ``postgresql`` one), or ``postgresql`` on Arch Linux.



PostgreSQL Binding
******************

This database system can be directly accessed (as a client) thanks to either the PostgreSQL command-line client (``psql``; for a simpler, more limited approach) or to an Erlang binding, the one that we retained being `epgsql <https://github.com/epgsql/epgsql>`_ (for a more efficient, in-depth access).

By default we consider that this binding has been installed in ``~/Software/epgsql``. The ``POSTGRESQL_BASE`` variable in ``myriad/GNUmakevars.inc`` can be set to match any other install path.

Recommended installation process:

.. code:: bash

 $ mkdir -p ~/Software && cd $_
 $ git clone https://github.com/epgsql/epgsql.git
 $ cd epgsql && make all && ln -s ./_build/default/lib/epgsql/ebin



Myriad SQL Support
------------------

To enable this support, once the corresponding backend and binding (see `Database Backends`_) have been installed, the ``USE_SQLITE`` variable should be set to ``true`` in ``myriad/GNUmakevars.inc``, and ``Myriad`` shall be rebuilt.

Then the corresponding implementation (``sql_support.erl``) and test (``sql_support_test.erl``), both in ``myriad/src/data-management``, will be built (use ``make clean all`` from the root of ``Myriad``) and able to be run (execute ``make sql_support_run`` for that).

Testing it:

.. code:: bash

 $ cd myriad/src/data-management
 $ make sql_support_run
		Compiling module sql_support.erl
		Compiling module sql_support_test.erl
		Running unitary test sql_support_run
  [...]
  --> Testing module sql_support_test.
  Starting SQL support (based on SQLite3).
  [...]
  Closing database.
  Stopping SQL support.
  --> Successful end of test.
  (test finished, interpreter halted)


Looks good.


SQL-related Troubleshooting
---------------------------


Compiling module sql_support.erl : can't find include file "sqlite3.hrl"
........................................................................

- ``USE_SQLITE`` not set to ``true`` in ``myriad/GNUmakevars.inc``
- ``erlang-sqlite3`` backend not correctly installed (e.g. ``SQLITE3_BASE`` not pointing to a right path in ``myriad/GNUmakevars.inc``)




