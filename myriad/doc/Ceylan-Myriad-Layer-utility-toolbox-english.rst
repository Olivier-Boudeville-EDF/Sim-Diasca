:raw-latex:`\pagebreak`

.. _toolbox:


Utility Toolbox
===============

This is the **core** of the ``Ceylan-Myriad`` library: a toolbox comprising many helper functions (with their tests), defined in the ``myriad/src/utils`` directory, often providing enhanced, more specialised services compared to the ones offered by the Erlang standard libraries.

These helpers (code and typing information) are thematically aggregated in modules that are generally suffixed by ``_utils``, and include:

- many **basic, general-purpose services**, defined in ``basic_utils.erl``, regarding:

  - the base types we defined
  - notifications
  - message handling
  - many miscellaneous functions

- **cipher**-related facilities (basic, a bit exotic chained symmetric encryptions, notably with Mealy machines), in ``cipher_utils.erl``
- functions to manage Erlang **compiled BEAM code** (``code_utils.erl``)
- services to manage the **execution of other programs** (``executable_utils.erl``), to:

  - locate said executables
  - to execute functional services (ex: display a PDF) regardless of the actual executable involved
- a few **test-related facilities**, in ``test_facilities.erl``
- services to handle more easily the (UNIX) shells and also the command-line arguments (a bit like ``getopt``), regardless of the interpreter or escript context (``shell_utils.erl``)

- helpers for **file-based** I/O operations (``file_utils.erl``); note that we now recommend not to open files with a specific encoding being set, but instead to encode the content before any writing thereof; refer to the ``Regarding encodings and Unicode`` section in ``file_utils.erl`` for further information
- services to manage identifiers of various sorts, including sortable ones (``id_utils.erl``)
- a very basic support of **Finite State Machines** (``fsm_utils.{e,h}rl``)
- a few operations defined on **graphs** (``graph_utils.erl``, with ``find_breadth_first/{3,4}``)
- extra operations defined on **lists** (``list_utils.erl``), including rings
- support for **network**-related concerns (``net_utils.erl.{e,h}rl``)
- services to offer **randomness** (``random_utils.erl``), with regard to various sources (the Erlang built-in algorithm, ``crypto``, newer ones like ``exsplus`` - our current default, ``exs64`` and ``exs1024``), for seeding, drawing, etc.
- very little support of **RDF** operations, standing for `Resource Description Framework <https://en.wikipedia.org/wiki/Resource_Description_Framework>`_ (``rdf_utils.erl``)
- facilities to handle content to the web, to HTTP, etc. (``web_utils.erl``) and to perform **REST calls** (``rest_utils.erl``), using built-in ``httpc`` and ``http_client``, including JSON services (``json_utils.erl``) based on any available parser backend, either `jsx <https://github.com/talentdeficit/jsx/>`_ or `jiffy <https://github.com/davisp/jiffy>`_ (note that their detection and use are done transparently at runtime, hence none of them is a declared dependency of Myriad, which will adapt to any choice made by the overall application that includes both Myriad and one of such parsers)
- elements for the sending of **SMS** (``sms_utils.erl``), based either on third-party providers providing REST APIs, or via a mobile phone (typically connected thanks to a USB cable); nothing as advanced as `Ceylan-Mobile <http://mobile.esperide.org/>`_, though
- support for operations at the **operating-system** level (``system_utils.{e,h}rl``)
- services to handle **text** (``text_utils.erl``)
- services to handle **binary information**, such as CRC (``bin_utils.erl``)
- functions to manage **time** information (``time_utils.erl``)
- a few helpers to ease the writing of `escripts <http://erlang.org/doc/man/escript.html>`_ relying on the Myriad layer (``script_utils.erl``)
- services addressed to the use of OTP, in ``otp_utils.erl``, allowing notably to run OTP applications out of an OTP context (typically through our native build/run system rather than through rebar3 and even OTP releases)
- services about all kinds of **units** (``unit_utils.erl``); refer to the `Management of Units`_ section below for more information
- support for **CSV** (Comma-Separated Values) files (``csv_utils.erl``) and **JSON** information (``json_utils.erl``)
- basic services for **trace emission** (a.k.a logging - not related to Erlang tracing), either directly through ``trace_utils.erl``, or thanks to ``trace_bridge.erl`` - typically to rely on more advanced trace systems such as `Ceylan-Traces <http://traces.esperide.org/>`_; now compliant with newer OTP logger and Syslog protocol as defined in `RFC 5424 <https://www.ietf.org/rfc/rfc5424.txt>`_, collecting bother userland traces and VM-level logs
- very basic facilities for **applications** (not in the sense of OTP ones), in ``app_facilities.{e,h}rl`` with an example (``most_basic_example_app.erl``)
- a bit of **locale management**, in ``locale_utils.erl``
- minor services about the **monitoring of Erlang processes**, in ``monitor_utils.erl`` and their **registering** in naming services, in ``naming_utils.erl``
- facilities to better **interface Erlang to other languages**, in ``language_utils.erl`` and ``{python,java}_utils.erl``; nothing as advanced as `Ceylan-Seaplus <http://seaplus.esperide.org/>`_, though
