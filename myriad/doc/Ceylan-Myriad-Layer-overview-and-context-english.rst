:raw-latex:`\pagebreak`

------------------
Overview & Context
------------------

When using any programming language, there are always **recurring patterns** that prove useful.

Instead of writing them again and again, we preferred gathering them all in a **low-level layer** (mostly a modest yet organised, uniform, consistent **code library**), in their most convenient, reliable, efficient version, together with their specification, documentation and testing.

This **toolbox layer** provides its (generally lightweight, simple) services just on top of the `Erlang <http://erlang.org>`_ [#]_ language, as a relatively small (comprising currently about 115k lines of code, including blank ones and comments), thin layer. Supported platforms are most Unices (including of course GNU/linux) and also - but to a lesser extent, i.e. not tested on a daily basis - `Windows`_.

.. [#] If needing to discover/learn Erlang, we recommend browsing `Learn You Some Erlang for great good! <https://learnyousomeerlang.com>`_ or, even better, buying their book!

	   See also our `Erlang HOW-TO <http://howtos.esperide.org/Erlang.html>`_.

.. comment .. [#] Still in the domain of functional programming, an Haskell cookbook was started as well, as `Ceylan-Curry <https://curry.esperide.org>`_.


These services tend to stay away from introducing any new **dependency**; however many supports (of OpenGL, JSON, Protobuf, SQL, HDF5, etc.; also in terms of GUI or integration with other languages) may be useful, and are to rely on well-chosen and well-integrated backends.

So, should a key, generic service need a third-party prerequisite (ex: library to manage a complex data format, or to process specific data), that dependency should be made **fully optional** (see the `Third-Party Dependencies`_ section for more details), in a logic of containment: if, for any reason, a dependency has to be replaced, the goal is that the user code can remain, as much as realistically possible, happily unaware of it - only the Myriad integration layer being impacted.

The purpose of Myriad is not to fully integrate the many backends it *may* rely on (it would be an unachievable task) but, once a proper integration base has been defined for a given backend, to pragmatically increase its coverage based on the actual needs.

.. comment Line count computed with: wc -l $(find . -name '*.?rl')`

As a consequence, for the `Ceylan <https://github.com/Olivier-Boudeville/Ceylan>`_ project, the first level of the Erlang-based software stack that we use relies on this ``Myriad`` layer - whose official, more specific name is the ``Ceylan-Myriad`` layer.

The project repository is located `here <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_. We gathered also some more general Erlang-information in `our corresponding HOWTO <http://howtos.esperide.org/Erlang.html>`_.


.. comment .. [#] It was formerly known as the ``Common`` layer.
