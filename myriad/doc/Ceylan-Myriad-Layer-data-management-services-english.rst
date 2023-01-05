

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


.. _`bijective table`:

Another type of table is the ``bijective_table``, which allows efficient (runtime) bidirectional conversions between two sets, each having unique elements (no duplicates).

As a mere convention, when one set is dealing with internal identifiers and the other on third-party ones, we recommend that the internal identifiers are selected as the first elements, and the third party as second elements.


.. _`const table`:

Finally, a way of **generating read-only associative tables** whose key/value pairs can be read very efficiently from any number (potentially extremely large) of readers (processes) is provided with ``const_table.erl`` (refer to ``const_table_test.erl`` for a test thereof).

No ETS table, replication (ex: per-process table copy) or message sending is involved: thanks to meta-programming, a module is generated on-the-fly, exporting as many functions as there are different keys in the table of interest. Calling a function corresponding to a key returns its associated value.

More precisely, a module name (ex: ``foobar``) and a list of ``{atom(), type_utils:permanent_term()}`` [#]_ entries shall be provided to ``const_table:generate_in_{memory,file}/2``; then, for each key/value pair in the specified table (ex: ``{baz, 42.0}``), a 0-arity function is generated and exported in that module, as if we had:

.. code:: erlang

  -module(foobar).

  [...]

  -export([baz/0]).

  -spec baz() -> term().
  baz() ->
	42.0.

.. [#] Of course transient terms like PIDs, references, etc. cannot/should not stored in such tables.


Then third-party code can call for example ``foobar:foo()`` and have ``42.0`` returned. This is presumably the most efficient way of sharing constants in Erlang between many processes (supposedly at least on par with `persistent_term <https://www.erlang.org/doc/man/persistent_term.html>`_).

Note that with ``generate_in_memory/2`` no actual module file is created (ex: no ``foobar.beam`` file is ever written in the filesystem): the operation remains fully in-memory (RAM). With ``generate_in_file/{2,3}`` a suitable module file is written on disk, so that the corresponding module can be loaded in the future like any other module.

Keys must be atoms, and the table of interest shall be immutable (const), even if, thanks to hot code upgrade, one may imagine updating the table at will, having any number of successive versions of it.

Generating a table of the same name more than once should be done with care, as if a given table is generated thrice (hence updated twice), the initial table would first switch from "current" to "old", and then would be removed. Any process that would linger in a function of this module would then be terminated (see `code replacement <http://www.erlang.org/doc/reference_manual/code_loading.html>`_). However, due to the nature of these tables (just one-shot fully-qualified calls, no recursion or message-waiting construct), this is not expected to happen.

Finally, two extra table types exist:

- ``const_bijective_table``, like a crossbreeding of ``const_table`` and ``bijective_table``, to rely on module-supported const, bijective tables: a list of ``{type_utils:permanent_term(), type_utils:permanent_term()}`` entries can then provided so that a corresponding module (e.g. ``foobar``) is generated (either in-memory or as a file) that allows to resolve any element of a pair into the other one, thanks to two functions, ``foobar:get_first_for/1`` and ``foobar:get_second_for/1``, automatically defined in that module; this is especially useful as soon as having non-small, const, bijective tables (since typing all information twice, one in a first direction and one in the other, is time-consuming and error-prone); refer to ``const_bijective_table_test.erl`` for an example and a test thereof
- ``const_bijective_topics`` is the same as the previous type, except that it allows *multiple* of such (const, bijective) tables (named "topics" here) to be defined in the same module (e.g. ``foobar``); for that, each of such tables is designated by a topic (an atom, like: ``colour``, ``bar_identifier`` or ``font_style``) that is associated to a declared list of corresponding entries (here also each with no duplicate); then, for each of these topics (e.g. ``colour``), two functions are automatically defined: ``foobar:get_first_for_colour/1`` and ``foobar:get_second_for_colour/1``, returning respective elements of the specified pair, for the specified topic; refer to ``const_bijective_topics_test.erl`` for an example and a test thereof; the ability of defining multiple const, bijective tables in a single generated module can be useful typically when developping a binding (e.g. for a GUI) or when translating protocols (e.g. between a third-party library and internal conventions); refer to `Ceylan-Oceanic <http://oceanic.esperide.org>`_ for an example thereof, including about its build integration (based on the ``EXTRA_BEAM_FILES`` make variable)



Other Datatypes
...............

They include ``pair.erl``, ``option_list.erl``, ``preferences.erl``, ``tree.erl``.

One may also refer for operations on:

- sets: ``set_utils.erl``
- lists: ``list_utils.erl``
- rings (i.e. `circular buffers <https://en.wikipedia.org/wiki/Circular_buffer>`_): ``ring_utils.erl``
- binaries (i.e. raw binary information): ``bin_utils.erl``


Pseudo-Builtin Types
....................

Such types, as ``void/0`` (for functions only useful for their side-effects - this happens!), ``maybe/1`` (``maybe(T)`` is either ``T`` or ``undefined``), ``safe_maybe/1`` (either ``{just,T}`` or ``nothing``) and ``fallible/{1,2}`` (an operation either is successful and returns a result, or returns an error) are supported, thanks to `the Myriad parse-transform`_.



Environments & Preferences
..........................


Principle
*********

An (application) **environment** is a server-like process that stores static or dynamic information (possibly initialised from an ETF_ file), as key/value entries (not unlike an ETS table), on behalf of an application or of a subset of its components, and makes it available to client processes.



Sharing of Data
***************

An environment stores a set of entries. An entry is designated by a key (an atom), associated to a value (that can be any term) in a pair.

Environments hold application-specific or component-specific data, obtained from any source (ETF_ file included); they may also start blank and be exclusively fed at runtime by the application or the components. Environments are used afterwards to maintain these pieces of data (read/write), before possibly storing them on file at application exit or component stop.

As a whole, an environment server can be seen as a process holding state information meant to be potentially common to various processes of a given application or component.


File Storage
************

Environment data can optionally be read from or written to file(s) in the ETF_ format.

Example of content of an environment file:

.. code:: erlang

 {my_first_color, red}.
 {myHeight, 1.80}.
 {'My name', "Sylvester the cat"}.



Addressing Environment Servers
******************************

The server process corresponding to an environment is locally registered; as a consequence it can be designated either directly through its PID or through its conventional (atom) registration name, like in:

.. code:: erlang

 environment:get(my_first_color, my_foobar_env_server).


No specific global registration of servers is made.

A (single) explicit start (with one of the ``start*`` functions) shall be preferred to implicit ones (typically triggered thanks to the ``get*`` functions) to avoid any risk of race conditions (should multiple processes attempt concurrently to create the same environment server), and also to be able to request that the server is also linked to the calling process.

An environment is best designated as a PID, otherwise as a registered name, otherwise from any filename that it uses.



About the Caching of Environment Entries
****************************************

For faster accesses (not involving any inter-process messaging), and if considering that their changes are rather infrequent (or never happening), at least some entries managed by an environment server may be cached directly in client processes.

In this case, the process dictionary of these clients is used to store the cached entries, and when updating a cached key from a client process the corresponding environment server is updated in turn. However any other client process caching that key will not be aware of this change until it requests an update to this environment server.

So a client process should cache a key mainly if no other is expected to update that key, i.e. typically if the associated value is const, or if this process is considered as the owner (sole controller) of that key (or if some other organisation ensures, possibly thanks to ``sync/1``, that its cache is kept consistent with the corresponding environment server.

As soon as a key is declared to be cached, its value is set in the cache; there is thus always a value associated to a cached key (not a maybe-value), and thus cached values may be ``undefined``.

Multiple environments may be used concurrently. A specific case of environment corresponds to the user preferences. See our ``preferences`` module for that, whose default settings file is ``~/.ceylan-settings.etf``.




Resource Management
...................


Principle
*********

Applications may have to manage all kinds of **data resources**, be them of classical resource types such as images or sounds, or be them specific to a project at hand.

The goal is to keep track of resources of all origins (e.g. read from file or network, or generated) in a *resource holder*.

These resources may be obtained:

- either from the filesystem, in which case their identifier is their (preferably binary) **path** that is relative to any holder-specific root directory (the recommended option) otherwise to the current directory, or absolute
- or from any other means, and then are designated thanks to a user-specified atom-based identifier



Resource Holders
****************

Myriad provides, through its ``resource`` module, two types of holders so that resources of interest can be obtained once, returned as often as needed, and stored for as long as wanted:

- resource **referentials**, which are process-local terms akin to associative tables
- resource **servers**, i.e. dedicated processes sharing resources (especially `large-enough binaries <https://www.erlang.org/doc/efficiency_guide/binaryhandling.html#how-binaries-are-implemented>`_) between any number of consumer processes

See also the ``resource.hrl`` include and the ``resource_test`` testing module.



File Formats
------------


Basic File Formats
..................


A built-in very basic support for the `CSV <https://en.wikipedia.org/wiki/Comma-separated_values>`_, for *Comma-Separated Values* (see ``csv_utils``) and `RDF <https://en.wikipedia.org/wiki/Resource_Description_Framework>`_ (see ``rdf_utils``) conventions is provided.


Most Usual, Standard File Formats
.................................

Besides the support for XML, an optional support (as it depends on third-party prerequisites) is proposed for:

- JSON
- HDF5
- SQLite

.. _`XML use`:

Some useful information for **XML use**:

- Myriad's XML support is implemented by the ``xml_utils`` module (so one shall refer to ``xml_utils.{e,h}rl`` and ``xml_utils_test.erl``), which relies on the built-in ``xmerl`` modules
- XML documents can be parsed from strings (see ``string_to_xml/1``) or files (see ``parse_xml_file/1``), and conversely can be serialised to strings (see ``xml_to_string/{1,2}``)
- an XML document is made from a list of XML elements, that can exist as three different forms that can be freely mixed: as "simple-form", as IOLists and/or as XML (xmerl) records
- we recommend the use of the "simple-form", which should be sufficient for at least most cases

This last form is based on simple tags, used in order to easily have (Erlang) terms that are direct counterparts of XML tags.

For example the following two elements (respectively in simple-form and as an XML document) are equivalent (if using the default XML prolog):

.. code:: erlang

 XMLSimpleContent = [
   myFirstTag,
   {mySecondTag, [myNestedTag]},
   {myThirdTag, [{color, "red"}, {age, 71}], ["This is a text!"]}].


and:

.. code:: xml

 <?xml version="1.0" encoding="utf-8" ?>
 <myFirstTag/>
 <mySecondTag><myNestedTag/></mySecondTag>
 <myThirdTag color="red" age="71">This is a text!</myThirdTag>


Refer to the ``xml_utils`` module for further details.



.. _`JSON use`:

Some useful information for **JSON use**:

- the nesting of elements shall be done thanks to (Erlang) maps, whose keys are binary strings (``text_utils:bin_string/0``); their order should not matter
- it may thus be convenient to add ``-define(table_type, map_hashtable).`` in a user module, so that the ``table`` pseudo-module can be relied upon when building a ``json_term``, while being sure that the JSON parser at hand will be fed afterwards with the relevant datastructure
- no comments shall be specified (even though some parsers may be configured to support them)
- strings shall be specified as binary ones
- the actual JSON backend used are either `jsx <https://github.com/talentdeficit/jsx/>`_ or `jiffy <https://github.com/davisp/jiffy>`_; to better understand their (mostly common) mapping between Erlang and JSON, one may refer to the `this section <https://github.com/talentdeficit/jsx/#json---erlang-mapping>`_ of the jsx documentation  and to `this one <https://github.com/davisp/jiffy#data-format>`_ regarding jiffy

Example:

.. code:: erlang

 MyJSONTerm = table:add_entries([
   {<<"asset">>, #{<<"generator">> => <<"My Generator">>,
				   <<"version">> => <<"2.0">>}},
   {<<"other">>, 42}
								], table:new()),

 JSONString = json_utils:to_json(MyJSONTerm)


shall result in a JSON document like:


.. code:: json

 {
   "asset": {
	 "generator": "My Generator",
	 "version": "2.0"
   },
   "other": 42
 }


Hint: the `jq <https://stedolan.github.io/jq/>`_ command-line tool may be very convenient in JSON contexts.

Refer to the `Myriad-level Third-Party Dependencies`_ section for further information.



.. _etf:

For Pure Erlang uses: the ETF File Format
.........................................

For many needs in terms of Erlang internal data storage (ex: regarding configuration settings), we recommend the use of the file format that `file:consult/1 <https://erlang.org/doc/man/file.html#consult-1>`_  can directly read, that we named, for reference purpose, ``ETF`` (for *Erlang Term Format* [#]_). We recommend that ETF files have for extension ``.etf``, like in: ``~/.ceylan-settings.etf`` (see also our support for `user preferences`_).

.. [#] Not to be mixed up with the `Erlang External Term Format <https://www.erlang.org/doc/apps/erts/erl_ext_dist.html>`_, which is used for serialisation_.


ETF is just a text format for which:

- a line starting with a ``%`` character is considered to be a comment, and is thus ignored
- other lines are terminated by a dot, and correspond each to an Erlang term (ex: ``{base_log_dir, "/var/log"}.``)

Note that no mute variable can be used there (e.g. ``_Name="James Bond"`` cannot be specified in such a file; only terms like ``"James Bond"`` can be parsed); so, in order to add any information of interest, one shall use comment lines instead.

Records are not known either; however they can be specified as tagged tuples (e.g. instead of specifying ``#foo{ bar=7, ...}``, use ``{foo, 7, ...}``).

See `this example <https://github.com/Olivier-Boudeville/us-common/blob/master/priv/for-testing/us.config>`_ of a full ETF file.

A basic support for these ETF files is available in ``file_utils:{read,write}_etf_file/*``.

If expecting to read UTF-8 content from such a file, it should:

- have been then opened for writing typically while including the ``{encoding,utf8}`` option, or have been written with content already properly encoded (maybe more reliable that way)

- start with a ``%% -*- coding: utf-8 -*-`` header


ETF files are notably used as **configuration files**. In this case following extra conventions apply:

- their extension is preferably changed from ``.etf`` to ``.config``
- before each entry, a comment describing it in general terms shall be available, with typing information
- entries are pairs:

  - whose first element is an atom
  - their second element can be any value, typically of algebraic types; if a string value is included, for readability purpose it shall preferably be specified as a plain one (ex: ``"James Bond"``) rather than a binary one (ex: ``<<"James Bond">>``); it is up to the reading logic to accommodate both forms; it is tolerated to reference, in the comments of these configuration files, types that actually include *binary* strings (not plain ones, even though plain ones are used in the configuration files)


.. _`glTF file format`:

To Export 3D Scenes
...................

A basic support of `glTF <https://en.wikipedia.org/wiki/GlTF>`_ (*Graphics Language Transmission Format*) version 2.0 has been implemented in ``gltf_support.{hrl,erl}``.

The various elements associated to that model (scenes, nodes, meshes, primitives, materials, lights, cameras, buffers, buffer-views, accessors) can be handled from Erlang, in an already integrated way to Myriad's `spatial services and conventions`_.

See the `glTF 2.0 Reference Guide <https://www.khronos.org/files/gltf20-reference-guide.pdf>`_ and the `glTF 2.0 Specification <https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html>`_ for more information. See also our `HOW-TO about 3D <http://howtos.esperide.org/ThreeDimensional.html>`_ for both more general and practical considerations.



Regarding Data Exchange
-----------------------


.. _`serialisation`:


Serialisation: Marshalling / Demarshalling
..........................................


Purpose
*******

When trusted Erlang nodes and Erlang applications are to communicate, they are likely to rely on the (Erlang) `External Term Format <https://www.erlang.org/doc/apps/erts/erl_ext_dist.html>`_ for that.

To communicate with other systems (non-Erlang and/or non-trusted) over a network stream (over a transport protocol such as TCP/IP), a common `data-serialisation format <https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats>`_ must be chosen in order to marshall and demarshall the applicative data to be exchanged.

This format can be ad hoc (defined with one's conventions) or standard. We prefer here the latter solution, as a standard format favors interoperability and reduces tedious, error-prone transformations.

Moreover various well-supported standard options exist, like `XDR <https://en.wikipedia.org/wiki/External_Data_Representation>`_, `ASN.1 <https://en.wikipedia.org/wiki/ASN.1>`_, `Protobuf <https://en.wikipedia.org/wiki/Protocol_Buffers>`_ (a.k.a. *Protocol Buffer*), `Piqi <http://piqi.org/>`_ and many others.



Choice of format
****************

The two formats that we thought were the most suitable and standard were **ASN.1** (with a proper, efficient encoding selected), or **Protobuff**.

As ASN.1 has been defined for long and is properly supported by Erlang (natively), and that there are `apparently valid claims <https://reasonablypolymorphic.com/blog/protos-are-wrong/index.html>`_ that Protobuf has some flaws, ASN.1 seemed to us the more relevant technical choice.


About ASN.1
***********

Erlang supports, out of the box, `three main ASN.1 encodings <https://www.erlang.org/doc/man/asn1ct.html#compile-2>`_:

- BER (`Basic Encoding Rules <https://en.wikipedia.org/wiki/X.690#BER_encoding>`_): a type-length-value encoding, too basic to be compact; its DER (for *Distinguished Encoding Rules*) variation is also available
- PER (*Packed Encoding Rules*): a bit-level serialisation stream, either aligned to byte boundaries (PER) or not (UPER, for *Unaligned PER*); if both are very compact and complex to marshall/demarshall, it is especially true for the size/processing trade-off of UPER
- JER (*JSON Encoding Rules*), hence based on JSON_


Our preference goes towards first UPER, then PER. A strength of ASN.1 is the expected ability to switch encodings easily; so, should the OER encoding (*Octet Encoding Rules*; faster to decode/encode than BER and PER, and almost as compact as PER) be supported in the future, it could be adopted "transparently".


An issue of this approach is that, beyond Erlang, the (U)PER encoding does not seem so widely available as free software: besides commercial offers (like `this one <https://www.obj-sys.com/products/asn1c/index.php>`_), some languages could be covered to some extent (ex: `Python <https://github.com/eerimoq/asn1tools>`_, Java with `[1] <https://github.com/alexvoronov/gcdc-asn1/tree/master/asn1-uper>`_ or `[2] <https://github.com/ericsson-mts/mts-asn1>`_), but for example no such solution could be found for the .NET language family (ex: for C#); also the complexity of the encoding may lead to solutions supporting only a subset of the standard.

So, at least for the moment, we chose Protobuf.



About Protobuf
**************

Compared to ASN.1 UPER, Protobuf is probably simpler/more limited, and less compact - yet also less demanding in terms of processing regarding (de)marshalling.

Albeit Protobuf is considerably more recent, implementations of it in free software are rather widely available in terms of languages, with `reference implementations <https://developers.google.com/protocol-buffers/docs/reference/overview>`_ and third-party ones (example for `.NET <https://github.com/protobuf-net/protobuf-net>`_).

In the case of Erlang, Protobuf is not natively supported, yet various libraries offer such a support.

`gpb <https://github.com/tomas-abrahamsson/gpb>`_ seems to be the recommended option, this is therefore the backend that we retained. For increased performance, `enif_protobuf <https://github.com/jg513/enif_protobuf>`_ could be considered as a possible drop-in replacement.

Our procedure to install ``gpb``:

.. code:: bash

 $ cd ~/Software/gpb
 $ git clone git@github.com:tomas-abrahamsson/gpb.git
 $ ln -s gpb gpb-current-install
 $ cd gpb && make all

Then, so that ``protoc-erl`` is available on the shell, one may add in one's ``~/.bashrc``:

.. code:: bash

 # Erlang protobuf gpb support:
 export GPB_ROOT="${HOME}/Software/gpb/gpb-current-install"
 export PATH="${GPB_ROOT}/bin:${PATH}"



Our preferred settings (configurable, yet by default enforced natively by Myriad's build system) are: (between parentheses, the gbp API counterpart to the ``protoc-erl`` command-line options)

- ``proto3`` version rather than ``proto2`` (so ``{proto_defs_version,3}``)
- messages shall be decoded as tuples/records rather than maps (so not specifying the ``-maps`` / ``maps`` option, not even ``-mapfields-as-maps``) for a better compactness and a clearer, more statically-defined structure - even if it implies including the generated ``*.hrl`` files in the user code and complexifying the build (ex: tests having to compile with or without a Protobuff backend available, with or without generated headers; refer to ``protobuf_support_test.erl`` for a full, integrated example)
- decoded strings should be returned as binaries rather than plain ones (so specifying the ``-strbin`` / ``strings_as_binaries`` option)
- ``-pkgs`` /  ``use_packages`` (and ``{pkg_name, {prefix, "MyPackage"}``) to prefix a message name by its package (regardless of the ``.proto`` filename in which it is defined)
- ``-rename msg_fqname:snake_case`` then ``-rename msg_fqname:dots_to_underscores`` (in that order), so that a message type named ``Person`` defined in package ``myriad.protobuf.test`` results in the definition of a ``myriad_protobuf_test_person()`` type and in a ``#myriad_protobuf_test_person{}`` record
- ``-preserve-unknown-fields`` (thus ``preserve_unknown_fields``) will be set iff ``EXECUTION_TARGET`` has been set to ``development`` (``myriad_check_protobuf`` is enabled), and in this case will be checked so that a warning trace is sent if decoding unknown fields
- ``-MMD`` / ``list_deps_and_generate`` to generate a ``GNUmakedeps.protobuf`` makefile tracing dependencies between message types
- ``-v`` / ``verify`` set to  ``never``, unless ``EXECUTION_TARGET`` has been set to ``development`` (``myriad_check_protobuf`` is enabled), in which case it is set to  ``always``
- ``-vdrp`` / ``verify_decode_required_present`` set iff ``EXECUTION_TARGET`` has been set to ``development`` (``myriad_check_protobuf`` is enabled)
- ``-Werror`` / ``warnings_as_errors``, ``-W1`` / ``return_warnings``, ``return_errors`` (preferably to their ``report*`` counterparts)


We prefer generating Protobuff (Erlang) accessors thanks to the command-line rather than driving the generating through a specific Erlang program relying on the gpb API.

See our ``protobuf_support`` module for further information.


This support may be enabled from Myriad's ``GNUmakevars.inc``, thanks to the ``USE_PROTOBUF`` boolean variable that implies in turn the ``USE_GPB`` one.

One may also rely on our:

- ``GNUmakerules-protobuf.inc``, in ``src/data-management``, to include in turn any relevant dependency information; dependencies are by default automatically generated in a ``GNUmakedeps.protobuf`` file
- general explicit rules, for example ``generate-protobuf`` (to generate accessors), ``info-protobuf`` and ``clean-protobuf`` (to remove generated accessors)
- automatic rules, for example ``make X.beam`` when a ``X.proto`` exists in the current directory; applies our recommended settings)


One may note that:

- a Protobuff message, i.e. the (binary) serialised form of a term (here being a record), is generally smaller than this term (for example, ``protobuf_support_test`` reports a binary of 39 bytes, to be compared to the 112 bytes reported for the corresponding record/tuple)
- the encoding of the serialised form does not imply any specific obfuscation; for example binary strings comprised in the term to serialise may be directly readable from its binary serialisation, as clear text


References:

- `general Protobuf Wikipedia presentation <https://en.wikipedia.org/wiki/Protocol_Buffers>`_
- `official page of Protobuf <https://developers.google.com/protocol-buffers>`_
- `proto3 Language Guide <https://developers.google.com/protocol-buffers/docs/proto3>`_
- gpb-related information:

  - command-line options: ``protoc-erl -h``
  - `gpb API documentation <https://hexdocs.pm/gpb/>`_, notably the many options of `gpb_compile documentation <https://hexdocs.pm/gpb/gpb_compile.html#option-use_packages>`_ and the `Erlang-Protobuff mapping <https://hexdocs.pm/gpb/gpb_compile.html#description>`_



For Basic, Old-School Ciphering
...............................

The spirit here is to go another route than modern public-key cryptography: the classic, basic, chained, symmetric cryptography techniques used in this section apply to contexts where a preliminary, safe exchange *can* happen between peers (ex: based on a real-life communication).

Then any number of passes of low-key, unfashioned algorithms (including one based on a Mealy machine) are applied to the data that is to cypher or decypher.

We believe that, should the corresponding shared "key" (the combination of parameterised transformations to apply on the data) remain uncompromised, the encrypted data is at least as safe as if cyphered with the current, modern algorithms (which may be, intentionally or not, flawed, or may be specifically threatened by potential progresses for example in terms of quantum computing).

So this is surely an instance of "security by obscurity", a pragmatic strategy (which may be used in conjunction with the "security by design" and "open security" ones) discouraged by standards bodies, yet in our opinion likely - for data of lesser importance - to resist well (as we do not expect then attackers to specifically target our very own set of measures, since the specific efforts incurred would not be outweighed by the expected gains).

We thus see such old-school ciphering as a complementary measure to the standard, ubiquitous measures whose effectiveness is difficult to assess for individuals and thus require some level of trust.

Refer to ``cipher_utils`` and its associated test for more details, and also to our `mini-HOWTO regarding cybersecurity <http://howtos.esperide.org/Cybersecurity.html>`_.
