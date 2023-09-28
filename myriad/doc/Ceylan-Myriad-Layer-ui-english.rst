:raw-latex:`\pagebreak`

.. _`user interface`:
.. _`graphical user interface`:
.. _`MyriadGUI`:


MyriadUI: Helpers For User Interface Programming
================================================

Some services have been defined, in ``myriad/src/user-interface``, in order to handle more easily interactions with the user, i.e. to provide a user interface.

.. Note The user-interface services, as a whole, are currently *not* functional. A rewriting thereof as been started yet has not completed yet.

The spirit of **MyriadUI** is to offer, as much as possible, a high-level API (refer to the ``ui`` module) that can be seamlessly instrumented at runtime by different backends, depending on availability (e.g. is this dependency installed?) and context (e.g. is the program running in a terminal, or are graphical outputs possible?).

Unless the user forces the use of a given backend, the most advanced one that is locally available will be automatically selected.

An objective is to shelter user code from:

- the actual UI backend that will be selected ultimately on a given host
- the rise and fall of the various backends (thinking for example to ``gs`` having been quickly deprecated in favour of ``wx``); the idea is then that any new backend could be integrated, with *little to no change* in already-written code relying on MyriadUI

Of course not all features of all backends can be integrated (they have not the same expressivity, a common base must be enforced [#]_) and creating a uniform interface over all sorts of vastly different ways of displaying and interacting with the user would require a lot of efforts. So MyriadUI follows a pragmatic route: defining first basic, relevant, user-centric conventions and services able to cover most needs and to federate (hopefully) most backends, and to incrementally augment its implementation coverage on a per-need basis. As a consequence, efforts have been made so that adding any lacking element can be done at low cost.

.. [#] Yet optional, additional features may be defined, and each backend may choose to provide them or ignore them.


Various Flavours of User Interfaces
-----------------------------------

Such a user interface may be:

- either **text-only**, within a console, relying either on the very basic ``text_ui`` (for raw text) or its more advanced ``term_ui`` counterpart (for terminal-based outputs, with colours and text-based widgets)
- or **graphical**, with ``gui``
- (and/or, in a possible future, **audio**, with a ``audio_gui`` that could be added)

Text-based user interfaces are quite useful, as they are lightweight, incur few dependencies (if any), and can be used with headless remote servers (``text_ui`` and ``term_ui`` work well through SSH, and require no X server nor mouse).

As for graphical-based user interfaces, they are the richest, most usual, and generally the most convenient, user-friendly interfaces.

The user interfaces provided by Myriad are stateful, they rely on a **state** that can be:

- either ``explicit``, in a functional way; thus having to be carried in all calls
- or ``implicit``, using - for that very specific need only - the process dictionary (even if we try to stay away of it as much as possible)

We tested the two approaches and preferred the latter (implicit) one, which was found considerably more flexible and thus finally fully superseded the (former) explicit one.

We made our best so that a lower-level API interface (relying on a more basic backend) is **strictly included** in the higher-level ones (e.g. ``term_ui`` adds concepts - like the one of window or box - to the line-based ``text_ui``; similarly ``gui`` is richer than ``term_ui``), in order that any program using a given user interface may use any of the next, upper ones as well (provided implicit states are used), in the following order: the ``text_ui`` API is included in the one of ``term_ui``, which is itself included in the one of ``gui``.

We also defined the **settings table**, which is a table gathering all the settings specified by the developer, which the current user interface does its best to accommodate.

Thanks to these "Matryoshka" APIs and the settings table, the definition of a more generic ``ui`` interface has been possible. It selects automatically, based on available local software dependencies, **the most advanced available backend**, with the most relevant settings.

For example a relevant backend will be automatically selected by:

.. code:: bash

 $ cd test/user-interface
 $ make ui_run


On the other hand, if wanting to select a specified backend:

.. code:: bash

 $ make ui_run CMD_LINE_OPT="--use-ui-backend term_ui"

(see the corresponding `GNUmakefile <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/test/user-interface/GNUmakefile>`_ for more information)



Raw Text User Interface: ``text_ui``
------------------------------------

This is the most basic, line-based monochrome textual interface, directly in raw text with no cursor control.

It is located in ``{src,test}/user-interface/textual``; see ``text_ui.erl`` for its implementation, and ``text_ui_test.erl`` for an example of its use.



Terminal Text User Interface: ``term_ui``
-----------------------------------------

This is a more advanced textual interface than the previous one, with colors, dialog boxes, support of locales, etc., based on `dialog <https://en.wikipedia.org/wiki/Dialog_(software)>`_ (possibly `whiptail <https://en.wikipedia.org/wiki/Newt_(programming_library)>`_ could be supported as well). Such backend of course must be available on the execution host then.

For example, to secure these prerequisites:

.. code:: bash

 # On Arch Linux:
 $ pacman -S dialog

 # On Debian-like distros:
 $ apt-get install dialog


It is located in ``{src,test}/user-interface/textual``; see ``term_ui.erl`` for its implementation, and ``term_ui_test.erl`` for an example of its use.



.. _`gui`:_


Graphical User Interface: ``gui``
---------------------------------

The MyriadGUI modules (``gui*``) provide features like 2D/3D rendering, event handling, input management (keyboard/mouse), canvas services (basic or OpenGL - with textures, shaders, etc.), and the various related staples (management of images, texts and fonts, colors, window manager, etc.); refer to `the MyriadGUI sources <https://github.com/Olivier-Boudeville/Ceylan-Myriad/tree/master/src/user-interface/graphical>`_ for more complete information.



For Classical 2D Applications
.............................


Base GUI Backend
****************

This interface used to rely on (now deprecated) ``gs``, and now relies on `wx <http://erlang.org/doc/man/wx.html>`_ [#]_ [#]_ (a port of `wxWidgets <https://www.wxwidgets.org/>`_, which belongs to the same category as GTK or Qt). For the base dialogs, `Zenity <https://en.wikipedia.org/wiki/Zenity>`_ could have been an option.

.. [#] What are the main differences between MyriadGUI and wx? The MyriadGUI API is backend-agnostic (no trace of wx when using it), a bit higher-level (e.g. user-defined widget identifiers being atoms rather than integer constants; relying on more flexible options; integrating a few workarounds), and based on fewer modules. However, as a strict subset of wx, it is by design less complete - yet it is quite easy to extend on a per-need basis.

.. [#] Maybe later it will be based on HTML 5 (although we are not big fans of light clients and of using browsers for everything), possibly relying some day for that on the `Nitrogen web framework <http://nitrogenproject.com/>`_, on `N2O <https://ws.n2o.dev/>`_ or on any other relevant HTML5 framework.


We also borrowed elements from the truly impressive `Wings3D <http://www.wings3d.com/>`_ (see also `our HOWTO section about it <https://howtos.esperide.org/ThreeDimensional.html#wings3d>`_) modeller, and also on the remarkable `libSDL <https://libsdl.org/>`_ (2.0) library together with its `esdl2 <https://github.com/ninenines/esdl2>`_ Erlang binding.

If having very demanding 2D needs, one may refer to the `3D services`_ section (as it is meant to be hardware-accelerated, and the 2D services are a special cases thereof).


.. Note:: Currently MyriadGUI does not adhere yet to the ``ui`` conventions, but it will ultimately. MyriadGUI already provides many lower-level services and offers a graphical API (currently on top of ``wx``; see `our HOWTO <http://howtos.esperide.org/Erlang.html#using-wx>`_ for some information regarding that backend) that can be used in order to  develop one's GUI application hopefully in a future-proof way.


.. _`wx availability`:

As a consequence, `wxWidgets <https://www.wxwidgets.org/>`_ must be available on the host (otherwise a ``{load_driver,"No driver found"}`` exception will be raised on GUI start). This should correspond to the ``wxgtk3`` Arch Linux package, or the ``libwxgtk3.0-dev`` Debian one. This can be tested by executing ``wx-config --version`` on a shell.

``wxWidgets`` must be installed *prior* to building Erlang, so that it is detected by its configuration script and a proper ``wx`` module can be used afterwards. Running then ``wx:demo()`` is a good test of the actual support.



Purpose of ``gui``
******************

The goal is to provide a small, lightweight API (including message types) that are higher-level than ``wx`` and OpenGL (more integrated, more typed, possibly clearer, having more runtime checks - that can be toggled at build time), and do not depend on any particular GUI backend (such as ``wx``, ``gs``, etc.; so none of their includes, records, types or functions leak in the user realm), to avoid that user programs become obsolete too quickly because of the UI backend they rely on.

So for example the messages received by the user programs do not mention ``wx``, and respect only MyriadGUI conventions. These conventions are in line with the `WOOPER ones <https://wooper.esperide.org/#method-invocation>`_, enabling (in a fully optional manner) the user code to rely on WOOPER if wanted [#]_.

.. [#] Inspired from MyriadGUI, one could consider creating WOOPERGUI, which would provide basically the same services, yet relying on inheritance on the Erlang side as well.

	   That way for example a frame would be a special case (hence a child class) of window, and frames would automatically inherit all window operations; so the user would have just to handle a frame by itself, without having to take into account the fact that some operations of interest are actually defined at the window level instead.


The usual mode of operation is the following:

:raw-html:`<center><img src="myriad-gui-mode-of-operation.png" id="responsive-image-medium"></img></center>`
:raw-latex:`\begin{figure}[h] \centering \includegraphics[scale=0.4]{myriad-gui-mode-of-operation} \end{figure}`


1. From a user process (a test, an application, etc.), the GUI support is first started, with ``gui:start/{0,1}``
2. Then the various widgets (windows, frames, panels, buttons, etc.) are created (e.g. thanks to ``MainFrame = gui:create_frame(...``) and the user process subscribes to the events it is interested in (as a combination of an event type and a widget-as-an-event-emitter; for example:

.. code:: erlang

 gui:subscribe_to_events({onWindowClosed, MainFrame})

3. The user process also triggers any relevant operation (e.g. clearing widgets, setting various parameters), generally shows at least a main frame and records the GUI state that it needs for future use (typically containing at least the MyriadGUI references of the widgets that it created)
4. Then the user process enters its own (GUI-specific) main loop, from which it will receive the events that it subscribed to, and to which it will react by performing application-specific operations and/or GUI-related operations (creating, modifying, deleting widgets). Generally at least one condition is defined in order to leave that main loop and stop the GUI (``gui:stop/0``)

Such a scheme based on a "man-in-the-middle" (the MyriadGUI process) is necessary to abstract out for example the types of messages induced by a given GUI backend. If performances should not be an issue for user interaction, the integration must be carefully designed, notably because a 3-actor cooperation (user code, MyriadGUI one, backend one) opens the possibility of race conditions to occur (notably some operations, like event subscribing, must then be made synchronous, as the user process may trigger direct interactions with the backend; see implementation notes for more details).

Refer to the `gui_overall_test.erl <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/test/user-interface/graphical/gui_overall_test.erl>`_ and `lorenz_test.erl <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/test/user-interface/graphical/lorenz_test.erl>`_ test full, executable usage examples thereof.

Here is a screenshot of the former test, where a random polygon (in green) is generated, for which are determined both the convex hull (in blue) and the MEC (*Minimum Enclosing Circle*, in purple):

:raw-html:`<center><img src="myriad-minimal-enclosing-circle-test.png" id="responsive-image-medium"></img></center>`
:raw-latex:`\begin{figure}[h] \centering \includegraphics[scale=0.4]{myriad-minimal-enclosing-circle-test} \end{figure}`


Defining ``gui`` as an interface between the user code and a backend also allows to enrich said backend [#]_.

.. [#] For example, we needed to operate on a plain canvas, whereas ``wx`` (as we understand it) offers only panels with bitmaps (with ``wxDC``, ``wxWindowDC``, ``wxMemoryDC``, etc.), with no possibility to subclass them in order to add them features. So MyriadGUI transparently introduced ``gui_canvas`` to offer extended canvas services.


These services are located in ``{src,test}/user-interface/graphical`` (see ``gui.erl``, ``gui_color.erl``, ``gui_text.erl``, ``gui_canvas.erl``, etc.), with a few tests (``gui_test.erl``, ``lorenz_test.erl``) and will be enriched over time, on a per-need basis.

.. _`Lorenz test`:

This last ``lorenz_test.erl`` offers another complete example:

:raw-html:`<center><img src="myriad-lorenz-test.png" id="responsive-image-large"></img></center>`
:raw-latex:`\begin{figure}[h] \centering \includegraphics[scale=0.2]{myriad-lorenz-test} \end{figure}`





For 3D Applications
...................


Purpose
*******

In order to render 3D content, Myriad relies on `OpenGL <https://en.wikipedia.org/wiki/OpenGL>`_, a standard, cross-platform, uniform and well-designed programming interface that enables the use of video cards in order to deport most of the (2D or 3D) heavy-lifting there.

Sophisticated 3D rendering is not necessarily an area where Erlang shines (perhaps, on the context of a client/server multimedia application, the client could rely on an engine like `Godot <https://en.wikipedia.org/wiki/Godot_(game_engine)>`_ instead), yet at least some level of rendering capabilities is convenient whenever performing 3D computations, implementing a server-side 3D logic, processing meshes, etc.

MyriadGUI offers a basic support of old-style OpenGL (from its version 1.1 until version 3.0, when this API was still relying on ``glBegin``, ``glMatrix``, etc.; now available through the compatibility profile) and of modern OpenGL (the one based on shaders, GLSL, etc., with the core profile).

One may refer to our `3D mini-HOWTO <https://howtos.esperide.org/ThreeDimensional.html>`_ for general information regarding these topics.



Prerequisites
*************

So a prerequisite is that the local host enjoys at least some kind of **OpenGL support**, either in software or, most preferably, with an hardware acceleration.

.. _`OpenGL troubleshooting`:

Just run our ``gui_opengl_integration_test.erl`` test to have the detected local configuration examined. One should refer to our HOWTO section `about 3D operating system support <http://howtos.esperide.org/ThreeDimensional.html#os-support>`_ for detailed information and troubleshooting guidelines.

As for the **Erlang side** of this OpenGL support, one may refer to `this section <https://www.erlang.org/doc/man/wxglcanvas#description>`_ to ensure that the Erlang build at hand has indeed its OpenGL support enabled.



3D Services
***********


User API
________


The Myriad OpenGL utilities are defined in the ``gui_opengl`` module.

Shaders can be defined, in GLSL (see `this page <https://www.khronos.org/opengl/wiki/Core_Language_(GLSL)>`_ for more information).

Myriad recommends using the ``vertex.glsl`` extension for vertex shaders, the ``.tess-ctrl.glsl`` one for tessellation control shaders and ``.tess-eval.glsl`` for tessellation evaluation ones, ``.geometry.glsl`` for geometry shaders, ``fragment.glsl`` for fragment shaders and finally ``.compute.glsl`` for compute shaders.

The many OpenGL defines are available when having included ``gui_opengl.hrl`` (e.g. as ``?GL_QUAD_STRIP``).

Quite many higher-level primitives are provided, like ``gui_shader:assign_new_vbo_from_attribute_series/2``, which in one operation merges automatically an arbitrary number of vertex attribute series (vertices, normals, texture coordinates, etc.) of arbitrary component types and counts in a suitable VBO, and declares appropriately and enables the corresponding vertex attributes.

These utilities directly relate to Myriad's `spatial services and conventions`_ and to its support of the `glTF file format`_.

.. _octrees:

To manage larger 3D scenes, a basic support of `octrees <https://en.wikipedia.org/wiki/Octree>`_ is also available (see  `octree.erl <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/sr/data-management/octree.erl>`_); following conventions apply:

:raw-html:`<center><img src="myriad-octrees.png" id="responsive-image-reduced"></img></center>`
:raw-latex:`\begin{figure}[h] \centering \includegraphics[scale=0.2]{myriad-octrees} \end{figure}`


Various tests offer usage examples of the MyriadGUI API for 3D rendering:

- ``gui_opengl_minimal_test.erl`` runs a minimal test showcasing the proper local OpenGL support, based on normalised coordinates (in ``[0.0,1.0]``)
- ``gui_opengl_2D_test.erl`` is a 2D test operating with absolute (non-normalised) coordinates
- ``gui_opengl_integration_test.erl`` demonstrates more features (quadrics, textures, etc.)
- ``gui_opengl_mvc_test.erl`` proposes a MVC architecture (`Model-View-Controller <https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller>`_) where these three elements are uncoupled in separate processes yet are properly interlinked, the view relying on the MyriadGUI OpenGL support
- ``gui_opengl_minimal_shader_test.erl`` showcases the use of more recent OpenGL APIs (3.3 core [#]_), with GLSL shaders defined in ``gui_opengl_minimal_shader.{vertex,fragment}.glsl``

.. [#] Not compatible with all GPUs; notably Intel ones may only support older versions (e.g. 2.1).

.. Note:: Almost all OpenGL operations require that an OpenGL context already exists. When it is done, all GL/GLU operations can be done as usual.

		 So the point of MyriadGUI here is mostly to create a suitable OpenGL context, to offer a few additional, higher-level, stricter constructs to ease the integration and use (e.g. for the compilation of the various types of shaders and the linking of GLSL programs), and to connect this rendering capability to the rest of the GUI (e.g. regarding event management).

		 Modern OpenGL is supported (e.g. version 4.6), even though the compatibility context allows to use the API of OpenGL version 1.1.

		 See the `HOWTO section about OpenGL <https://howtos.esperide.org/ThreeDimensional.html#opengl-corner>`_ for more explanations.



Actual Use
__________

The MyriadGUI modules can be readily used.

The recommended way of, if needed, using the MyriadGUI includes is:

.. code:: erlang

 % The sole include that MyriadGUI user code shall reference:
 -include_lib("myriad/include/myriad_gui.hrl").



Configuration
_____________

In terms of error management, extensive verifications will apply iff the ``myriad_check_opengl_support`` flag is set.

Setting the ``myriad_debug_opengl_support`` flag will result in more runtime information to be reported.



Checking GLSL Shaders
_____________________

One's shader can be checked thanks to ``glslangValidator``, the *OpenGL / OpenGL ES Reference Compiler*.

For example, in order to check a vertex shader named ``foo.vertex.glsl``, just run ``make check-foo.vertex.glsl``; the GLSL reference compiler does not return output if it detects no error.

Refer to `our HOWTO section <https://howtos.esperide.org/ThreeDimensional.html#reference-glsl-compiler>`_ for more information.



Troubleshooting
_______________

Your textures include strange pure green areas? Most probably that your texture coordinates are wrong, as pure green is the default padding color that MyriadGUI uses (see the ``padding_color`` define in the ``gui_texture`` module) so that the dimensions of textures are powers of two.




Internal Implementation
_______________________

MyriadGUI is a wrapper on top of wx. What are the main differences between MyriadGUI and wx?

- preferred namings introduced (e.g. ``onWindowClosed`` events found clearer than ``close_window`` ones)
- widget identifiers are user-defined atoms in MyriadGUI (e.g. ``my_widget_id``) rather than numerical constants (e.g. ``-define(MY_WIDGET_ID, 2051)``) that have, with wx, to be defined, shared, uniquified accross user modules
- by default, events will propagate or be trapped by user-defined handlers depending on the type of these events (most of them being propagated by default; of course the user is able to override these defaults, either at subscription-time - using the ``propagate_event`` or ``trap_event`` option, or in one's handler - using the ``gui:propagate_event/1`` or ``gui:trap_event/1`` function); this contrasts with wx, in which by default all subscribed events are trapped, regardless of their type (then forgetting to propagate them explicitly may result in built-in mechanisms of wx to be disabled, like when resizing)
- code using MyriadGUI will not depend on wx, opening the possibility that, should the main Erlang GUI backend change, user code is nevertheless preserved

See also our little `Using wx <https://howtos.esperide.org/Erlang.html#using-wx>`_ HOWTO.

Regarding hardware acceleration, the MyriadGUI 2D/3D services rely on the related Erlang-native modules, namely `gl <https://www.erlang.org/doc/man/gl.html>`_ and `glu <https://www.erlang.org/doc/man/glu.html>`_, which are NIF-based bindings to the local OpenGL library.

As for the ``wx`` module (see the `wx availability`_ section), it provides a convenient solution in order to create a suitable OpenGL context.


.. _`SDL-based solution`:

`esdl <https://github.com/dgud/esdl>`_ used to be another solution to obtain an OpenGL context; it may be revived some day, as `SDL <https://www.libsdl.org/>`_ - i.e. *Simple DirectMedia Layer* - is still striving, and offers a full (yet low-level) access to multimedia and input devices; not all applications may have use of the rest of ``wx``.

These Erlang-native services can be easily tested by running ``wx:demo()`` from any Erlang shell and selecting then ``gl`` in the left example menu.

These platform-specific / backend-specific (e.g. wx or not, and which version thereof, e.g. wxWidget 2.8 vs 3.0 API) services shall remain fully invisible from MyriadGUI user code, so that it remains sheltered for good from any change at their level.

The goal is to wrap only the dependencies that may change in the future (e.g. wx); doing so for the ones considered (for good reasons) stable (such as gl or glu) would have no specific interest.


.. _`multimedia`:

For Multimedia Applications
...........................

Currently we provide only very preliminary support thereof with ``audio_utils``; for sound and music playback, refer to the `audio`_ section for more details.

.. _`TTS`:

.. _`speech synthesis`:

Speech synthesis (TTS, *Text-to-Speech*) is available thanks to ``speech_utils.erl``. In practice, for best results, the actual speech generation is delegated to cloud-based providers making use of AI (neural voices) for best fluidity.

The input text shall preferably comply with `SSML <https://en.wikipedia.org/wiki/Speech_Synthesis_Markup_Language>`_, typically so that it can be enriched with phonemes and prosody hints.

One can listen to this `French speech <speech-test-fr-FR.ogg.opus>`_ and this `English one <speech-test-en-US.ogg.opus>`_ (most browsers are now able to playback `Opus <https://en.wikipedia.org/wiki/Opus_(audio_format)>`_ content), that have been both generated by ``speech_utils_test.erl``.

Facilities to manage logical speeches (i.e. speeches designated by a base name such as ``hello`` and declined in as many locales as needed) are available (see ``speech_support:logical_speech/0``), as well as for related containers (see ``speech_support:speech_referential/0``).




For Interactive Applications
............................

Beyond the rendering of multimedia content, user interfaces have to **collect inputs from the user**, typically through mice, keyboards and joysticks.

Formerly, a port of `SDL <https://www.libsdl.org/>`_, `esdl <https://github.com/dgud/esdl>`_, was the best option, now using ``wx`` for that is recommended, as, through this port, the various input devices can be managed (at least to a large extent).



.. _`audio`:


Audio User Interface
--------------------

If the 2D/3D rendering can be done through ``wx``, apparently the **audio capabilities** (e.g. `[1] <https://docs.wxwidgets.org/3.0/group__group__class__media.html>`_, `[2] <https://docs.wxwidgets.org/3.0/classwx_sound.html>`_) of wxWidgets have not been made available to Erlang.

So an Erlang program needing audio output (e.g. sound special effects, musics) and/or input (e.g. microphone) will have to rely on another option, possibly in link, for audio rendering, to 3D-ready `eopenal <https://github.com/edescourtis/eopenal>`_ - an (Erlang) binding of `OpenAL <https://en.wikipedia.org/wiki/OpenAL>`_, or to a lower-level `SDL-based solution`_. Contributions welcome!

Currently only very basic support for audio output is available, as ``audio_utils:playback_file/{2,3}``.

See also our support for `speech synthesis`_.

