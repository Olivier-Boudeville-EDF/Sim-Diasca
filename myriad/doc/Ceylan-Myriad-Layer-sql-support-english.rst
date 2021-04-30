:raw-latex:`\pagebreak`

SQL support
===========


About SQL
---------

Some amount of `SQL <https://en.wikipedia.org/wiki/SQL>`_ (*Structured Query Language*) support for relational database operations is provided by the ``Myriad`` layer.

As this support relies on an optional prerequisite, this service is disabled by default.



Database Back-end
-----------------

To perform SQL operations, a corresponding software solution must be available.

The SQL back-end chosen here is the `SQLite 3 <https://www.sqlite.org/about.html>`_ library. It provides a self-contained, serverless, zero-configuration, transactional SQL database. It is an embedded SQL database engine, as opposed to server-based ones, like `PostgreSQL <https://en.wikipedia.org/wiki/PostgreSQL>`_ or `MariaDB <https://en.wikipedia.org/wiki/MariaDB>`_.

It can be installed on Debian thanks to the ``sqlite3`` and ``sqlite3-dev`` packages, ``sqlite`` on Arch Linux..

We require version 3.6.1 or higher (preferably: latest stable one). It can be checked thanks to ``sqlite3 --version``.

Various related tools are very convenient in order to interact with a SQLite database, including ``sqlitebrowser`` and ``sqliteman``.

On Arch Linux, one can thus use: ``pacman -Sy sqlite sqlitebrowser sqliteman``.

Testing the back-end:

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



Erlang SQL Binding
------------------

This database system is directly accessed thanks to an Erlang binding.

Two of them have been identified as good candidates:

- `erlang-sqlite3 <https://github.com/alexeyr/erlang-sqlite3>`_: seems popular, with many contributors and users, actively maintained, based on a ``gen_server`` interacting with a C-node, involving only a few source files
- `esqlite <https://github.com/mmzeeman/esqlite>`_: based on a NIF, so more able to jeopardize the stability of the VM, yet potentially more efficient

Both are free software.

We finally preferred ``erlang-sqlite3``.

By default we consider that this back-end has been installed in ``~/Software/erlang-sqlite3``. The ``SQLITE3_BASE`` variable in ``myriad/GNUmakevars.inc`` can be set to match any other install path.

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



SQL Support Provided By the *Myriad* Layer
------------------------------------------

To enable this support, once the corresponding back-end (see `Database Back-end`_) and binding (see `Erlang SQL Binding`_) have been installed, the ``USE_SQLITE`` variable should be set to ``true`` in ``myriad/GNUmakevars.inc`` and ``Myriad`` shall be rebuilt.

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
- ``erlang-sqlite3`` back-end not correctly installed (ex: ``SQLITE3_BASE`` not pointing to a right path in ``myriad/GNUmakevars.inc``)
