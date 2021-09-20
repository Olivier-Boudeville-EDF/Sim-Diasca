:raw-latex:`\pagebreak`

------------------
Overview & Context
------------------

When using any programming language, there are always **recurring patterns** that prove useful.

Instead of writing them again and again, we preferred gathering them all in a **low-level layer** (mostly a modest **code library**), in their most convenient, reliable, efficient version, together with their specification, documentation and testing.

This layer provides its (generally lightweight, simple) services just on top of the `Erlang <http://erlang.org>`_ [#]_ language [#]_, as a relatively small (comprising currently about 80k lines), thin layer.

.. [#] If needing to discover/learn Erlang, we recommend browsing `Learn You Some Erlang for great good! <https://learnyousomeerlang.com>`_ or, even better, buying their book!

.. [#] Still in the domain of functional programming, an Haskell cookbook was started as well, as `Ceylan-Curry <https://curry.esperide.org>`_.


These services tend to stay away from introducing any new dependency. Should a key, generic service need a third-party prerequisite (ex: library to manage a complex data format, or to process specific data), that dependency should be made fully optional (see the `Third-Party Dependencies`_ section for more details).


.. comment Line count computed with: wc -l $(find . -name '*.?rl')`

As a consequence, for the `Ceylan <https://github.com/Olivier-Boudeville/Ceylan>`_ project, the first level of the Erlang-based software stack that we use relies on this ``Myriad`` layer - whose official, more specific name is the ``Ceylan-Myriad`` [#]_ layer.

The project repository is located `here <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_.


.. [#] It was formerly known as the ``Common`` layer.
