:raw-latex:`\pagebreak`

``Myriad`` Gotchas
==================



Header dependencies
-------------------

Only a very basic dependency between header files (``*.hrl``) and implementation files (``*.erl``) is managed.

As expected, if ``X.hrl`` changed, ``X.beam`` will be recompiled whether or not ``X.erl`` changed. However, any ``Y.erl`` that would include ``X.hrl`` would not be automatically recompiled.

Typically, when in doubt after having modified a record in a header file, just run ``make rebuild`` from the root of that layer (build is fast anyway, as quite parallel).



About the ``table`` module
--------------------------

This is a pseudo module, which is not meant to exist as such (no ``table.erl``, no ``table.beam``).

The ``Myriad`` parse transform replaces references to the ``table`` module by references to the ``map_hashtable`` module. See `table transformations`_ for more information.
