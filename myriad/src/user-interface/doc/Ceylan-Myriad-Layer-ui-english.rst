:raw-latex:`\pagebreak`

.. _`user interface`:
.. _`graphical user interface`:


Helpers For User Interface Programming
======================================

Some services have been defined, in ``myriad/src/user-interface``, in order to handle more easily interactions with the user, i.e. to provide a user interface.


.. Note:: The user-interface services, as a whole, are currently *not* functional. A rewriting thereof as been started yet has not completed yet.


Various Flavours of User Interfaces
-----------------------------------

Such a user interface may be:

- either **text-only**, within a console, relying either on the very basic ``text_ui`` (for raw text) or its more advanced ``term_ui`` counterpart (for terminal-based outputs)
- or **graphical**, with ``gui``

Text-based user interfaces are quite useful, as they are lightweight, incur few dependencies (if any), and can be used with headless remote servers (``text_ui`` and ``term_ui`` work well through SSH, and require no X server nor mouse).

As for graphical-based user interfaces, they are the richest, most usual, and generally the most convenient, user-friendly interfaces.

The user interfaces provided by Myriad are stateful, they rely on a **state** that can be:

- either ``explicit``, in a functional way; thus having to be carried in all calls
- or ``implicit``, using - for that very specific need only - the process dictionary (even if we try to stay away of it as much as possible)

We tested the two approaches and preferred the latter (implicit) one, which was found considerably more flexible and thus finally fully superseded the (former) explicit one.

We made our best so that a lower-level API interface (relying on a more basic backend) is **strictly included** in the higher-level ones (ex: ``term_ui`` adds concepts - like the one of window or box - to the line-based ``text_ui``), in order that any program using a given user interface may use any of the next, upper ones as well (provided implicit states are used), in the following order: the ``text_ui`` API is included in the one of ``term_ui``, which is itself included in the one of ``gui``.

We also defined the **settings table**, which is a table gathering all the settings specified by the developer, which the current user interface does its best to accommodate.

Thanks to these "Matryoshka" APIs and the settings table, the definition of a more generic ``ui`` interface has been possible. It selects automatically, based on available local software dependencies, **the most advanced available backend**, with the most relevant settings.

For example a relevant backend will be automatically selected by:

.. code:: bash

 $ cd test/user-interface/src
 $ make ui_run


On the other hand, if wanting to select a specified backend:

.. code:: bash

 $ make ui_run CMD_LINE_OPT="--use-ui-backend term_ui"

(see the corresponding `GNUmakefile <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/test/user-interface/src/GNUmakefile>`_ for more information)



Raw Text User Interface: ``text_ui``
------------------------------------

This is the most basic, line-based monochrome textual interface, directly in raw text with no cursor control.

Located in ``{src,test}/user-interface/textual``, see ``text_ui.erl`` for its implementation, and ``text_ui_test.erl`` for an example of its use.



Terminal Text User Interface: ``term_ui``
-----------------------------------------

This is a more advanced textual interface than the previous one, with colors, dialog boxes, support of locales, etc., based on `dialog <https://en.wikipedia.org/wiki/Dialog_(software)>`_ (possibly `whiptail <https://en.wikipedia.org/wiki/Newt_(programming_library)>`_ could be supported as well). Such backend of course must be available on the execution host then.

For example, to secure these prerequisites:

.. code:: bash

 # On Arch Linux:
 $ pacman -S dialog

 # On Debian-like distros:
 $ apt-get install dialog


Located in ``{src,test}/user-interface/textual``, see ``term_ui.erl`` for its implementation, and ``term_ui_test.erl`` for an example of its use.



Graphical User Interface: ``gui``
---------------------------------

This interface relied initially on ``gs`` (now deprecated), now on `wx <http://erlang.org/doc/man/wx.html>`_ (a port of `wxWidgets <https://www.wxwidgets.org/>`_), maybe later in HTML 5 (possibly relying on the `Nitrogen web framework <http://nitrogenproject.com/>`_ for that). For the base dialogs, `Zenity <https://en.wikipedia.org/wiki/Zenity>`_ could have been on option.

.. Note:: GUI services are currently being reworked, to provide a ``gs``-like concurrent API while relying underneath on ``wx``, with some additions (such as canvases).


The goal is to provide a small, lightweight API (including message types) that are higher-level than ``wx``, and do not depend on any particular GUI backend (such as ``wx``, ``gs``, etc.) to avoid that user programs become obsolete too quickly, as backends for GUI rise and fall relatively often.

So for example the messages received by the user programs shall not mention ``wx``, and they should take a form compliant with `WOOPER <https://github.com/Olivier-Boudeville/Ceylan-WOOPER>`_ message conventions, to easily enable user code to rely on WOOPER if wanted.


Located in ``{src,test}/user-interface/graphical``, see ``gui.erl``, ``gui_color.erl``, ``gui_text.erl``, ``gui_canvas.erl``, etc., with a few tests (``gui_test.erl``, ``lorenz_test.erl``).



Related information of interest:

- wxErlang: `Getting started <https://arifishaq.files.wordpress.com/2017/12/wxerlang-getting-started.pdf>`_ and `Speeding up <https://arifishaq.files.wordpress.com/2018/04/wxerlang-speeding-up.pdf>`_, by Arif Ishaq

- http://wxerlang.dougedmunds.com/

.. comment 404: - http://www.idiom.com/~turner/wxtut/wxwidgets.html
