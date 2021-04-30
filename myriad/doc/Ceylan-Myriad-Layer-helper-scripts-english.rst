:raw-latex:`\pagebreak`

.. _`helper scripts`:


Myriad's Helper Shell Scripts
=============================


A small set of shell scripts has been defined, in ``myriad/src/scripts``, to provide generic facilities useful in the context of Myriad [#]_.

.. [#] For a more general collection of (different) scripts, one may refer to `Ceylan-Hull <http://hull.esperide.org>`_, notably the `ones to facilitate development <http://hull.esperide.org/#for-development>`_.


Erlang-Dedicated Scripts
------------------------


Searching for Erlang elements
.............................


`ergrep <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/ergrep>`_
  To search for a text pattern through an Erlang source tree.

  | Usage: ergrep [-v|--verbose] [-q|--quiet] [-f|--filenames-only] [-i|--insensitive] <Expression to be found in sources> [TARGET_BASE_DIR]: recursive grep in Erlang source files, either from the TARGET_BASE_DIR directory, if specified, otherwise from the current directory.

  | Options:
  | -v or --verbose: be specifically verbose
  |	-q or --quiet: be specifically quiet, just listing matches
  |	-f or --filenames-only: display only filenames, not also the matched patterns, and if there are multiple matches in the same file, its filename will be output only once (implies quiet); useful for scripts
  |	-i or --insensitive: perform case-insensitive searches in the content of files, and also in the searched Erlang filenames
  |  Example: ergrep -i 'list_to_form(' /tmp


`find-type-definition.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/find-type-definition.sh>`_
  To search for the definition of a type of interest.

  | Usage: find-type-definition.sh [-h|--help] A_TYPE [A_DIR]: attempts to find the definition of the specified Erlang type from the specified directory (if any), otherwise from the current one.



`find-function-specification.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/find-function-specification.sh>`_
  To search for a function spec of interest.

  | Usage: find-function-specification.sh [-h|--help] A_FUNCTION_NAME [A_DIR]: attempts to find the type specification for the specified Erlang function (even if the name is partial and/or includes an arity) from the specified directory (if any), otherwise from the current one.


`find-record-definition.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/find-record-definition.sh>`_
  To search for the definition of a record of interest.

  | Usage: find-record-definition.sh [-h|--help] A_RECORD_NAME [A_DIR]: attempts to find the definition of the specified Erlang record from the specified directory (if any), otherwise from the current one.



Regarding Typing
................


`list-available-types.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/list-available-types.sh>`_
  Lists all Erlang types found in specified tree.

  | Usage: list-available-types.sh [-h|--help] [ROOT_DIR]: lists all types (according to Erlang type specifications) defined from the ROOT_DIR directory (if specified) or from the current directory.


`add-deduced-type-specs.escript <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/add-deduced-type-specs.escript>`_
   Generates and adds the type specification of all functions in specified module(s).

   | Usage: add-deduced-type-specs.escript FS_ELEMENT

   | Adds, for each selected BEAM file (either specified directly as a file, or found recursively from a specified directory), in the corresponding source file(s), for each function, the type specification that could be deduced from its current implementation.

   | FS_ELEMENT is either the path of a BEAM file or a directory that will be scanned recursively for BEAM files.
   | Note that BEAM files must be already compiled, and with debug information (see the '+debug_info' compile flag).
   | Note also that generating type specs this way may not a good practice.



Regarding Basic Performance Measurement
.......................................

`etop.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/etop.sh>`_
  Monitors on the console the busiest Erlang processes of a VM.

  | Usage: etop.sh [-node NODE_NAME] [-setcookie COOKIE]: shows on the console the activity of the Erlang processes on specified Erlang node (enter CTRL-C twice to exit).
  | Example: etop.sh -node foobar@baz.org -setcookie 'my cookie'


`benchmark-command.escript <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/benchmark-command.escript>`_
  Returns a mean resource consumption for the specified shell command (one may prefer relying on `benchmark-command.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/src/scripts/benchmark-command.sh>`_).

  | Usage: benchmark-command.escript COMMAND: returns a mean resource consumption for the specified shell command.
  Example: benchmark-command.escript "my_script.sh 1 2"



Miscellaneous
.............


`make-code-stats.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/make-code-stats.sh>`_
  Outputs basic statistics about an Erlang code base.

  | Usage: make-code-stats.sh [-h|--help] [SOURCE_DIRECTORY]: evaluates various simple metrics of the Erlang code found from any specified root directory, otherwise from the current one.


`launch-erl.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/launch-erl.sh>`_
  The only shell script on which we rely in order to launch an Erlang VM.

  | Usage: launch-erl.sh [-v] [-c a_cookie] [--sn a_short_node_name | --ln a_long_node_name | --nn an_ignored_node_name ] [--tcp-range min_port max_port] [--epmd-port new_port] [--fqdn a_fqdn] [--max-process-count max_count] [--busy-limit kb_size] [--async-thread-count thread_count] [--background] [--non-interactive] [--eval an_expression] [--no-auto-start] [-h|--help] [--beam-dir a_path] [--beam-paths path_1 path_2] [-start-verbatim-options [...]]: launches the Erlang interpreter with specified settings.

  | Detailed options:
  |		-v: be verbose
  |		-c a_cookie: specify a cookie, otherwise no cookie will be specifically set
  |		--sn a_short_node_name: distributed node using specified short name (ex: 'my_short_name')
  |		--ln a_long_node_name: distributed node using specified long name (ex: 'my_long_name')
  |		--nn an_ignored_node_name: non-distributed node, specified name ignored (useful to just switch the naming options)
  |		--tcp-range min_port max_port: specify a TCP port range for inter-node communication (useful for firewalling issues)
  |		--epmd-port new_port: specify a specific EPMD port (default: 4369); only relevant if the VM is to be distributed (using short or long names), initially or at runtime
  |		--fqdn a_fqdn: specify the FQDN to be used
  |		--max-process-count max_count: specify the maximum number of processes per VM (default: 400000)
  |		--busy-limit size: specify the distribution buffer busy limit, in kB (default: 1024)
  |		--async-thread-count thread_count: specify the number of asynchronous threads for driver calls (default: 128)
  |		--background: run the launched interpreter in the background (ideal to run as a daemon, ex: on a server)
  |		--daemon: run the node as a daemon (relies on run_erl and implies --background)
  |		--non-interactive: run the launched interpreter with no shell nor input reading (ideal to run through a job manager, ex: on a cluster)
  |		--eval 'an Erlang expression': start by evaluating this expression
  |		--no-auto-start: disable the automatic execution at VM start-up
  |		-h or --help: display this help
  |		--beam-dir a_path: adds specified directory to the path searched for beam files (multiple --beam-dir options can be specified)
  |		--beam-paths first_path second_path ...: adds specified directories to the path searched for beam files (multiple paths can be specified; must be the last option)
  |		--log-dir: specify the directory in which the VM logs (if using run_erl) shall be written

  | Other options will be passed 'as are' to the interpreter with a warning, except if they are listed after a '-start-verbatim-options' option, in which case they will passed with no warning.

  | If neither '--sn' nor '--ln' is specified, then the node will not be a distributed one.

  | Example: launch-erl.sh -v --ln ceylan --eval 'class_TimeManager_test:run()'


`show-xml-file.escript <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/show-xml-file.escript>`_
  Displays the content of the specified XML file.

  | Usage: show_xml_file.escript XML_FILE_PATH
  | Displays sequentially in a {name,Value} tree the structure of specified XML file (XML elements along with their XML attributes).









More General Scripts
--------------------


To generate documentation
.........................

These scripts are mostly unrelated to Erlang, yet are useful to be available from our most basic layer (Myriad).

`generate-docutils.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/generate-docutils.sh>`_
  Generates a proper PDF and/or HTML file from specified RST (`reStructuredText <https://en.wikipedia.org/wiki/ReStructuredText>`_) one (main, standalone script).

  | Usage: generate-docutils.sh <target rst file> [--pdf|--all|<comma-separated path(s) to CSS file to be used, ex: common/css/XXX.css,other.css>] [--icon-file ICON_FILENAME]

  | Generates a final document from  specified docutils source file (*.rst).

  | If '--pdf' is specified, a PDF will be created, if '--all' is specified, all output formats (i.e. HTML and PDF) will be created, otherwise HTML files only will be generated, using any specified CSS file.


`generate-pdf-from-rst.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/scripts/generate-pdf-from-rst.sh>`_
  Generates a proper PDF and/or HTML file from specified RST (`reStructuredText <https://en.wikipedia.org/wiki/ReStructuredText>`_) one; the previous ``generate-docutils.sh`` script is often preferred to this one, which depends on Myriad.

  | Usage: generate-pdf-from-rst.sh RST_FILE: generates a PDF file from the specified RST file, overwriting any past file with that name.

  | Ex: 'generate-pdf-from-rst.sh my_file.rst' will attempt to generate a new 'my_file.pdf' file.



Script-based Apps
-----------------

These shell scripts are actually user-facing shell interfaces that plug directly on some more involved Erlang programs, i.e. applications that are `available here <https://github.com/Olivier-Boudeville/Ceylan-Myriad/tree/master/src/apps>`_.


`generate-password.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/apps/generate-password/generate-password.sh>`_
  Generates a proper random password respecting various rules, whose simple application can be transparently checked (probably at least more easily audited than most password managers - thus maybe more trustable).

  | Usage: generate-password.escript [-a ALPHABET|--alphabet ALPHABET]
  |        [-l MIN_LEN MAX_LEN|--length MIN_LEN MAX_LEN] [-h|--help]

  | Generates a suitable password, where:
  | - ALPHABET designates the set of characters to draw from (default one being 'extended'), among:
  |	   * 'base': alphanumeric letters, all cases [A-Za-z0-9]
  |	   * 'extended': 'base' + basic punctuation (i.e. '[](){}:,;-_.!?')
  |	   * 'full': 'base' + all punctuation (i.e. basic + '"'@ /&$*\^%=+|')
  | - MIN_LEN and MAX_LEN are the respective minimum and maximum numbers of characters
  | (bounds included) used to generate this password [default: between 15 and 20]

  See also: the `security section <https://hull.esperide.org/#for-security>`_ of Ceylan-Hull, for more general guidelines and tooling regarding the proper management of credentials.


`merge.sh <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/apps/merge-tool/merge.sh>`_
  Helps merging efficiently and reliably file trees; it is actually a rather involved text-based application that allows scanning/comparing/merging trees, typically in order to deduplicate file hierarchies that were cloned once yet may have since then diverged.

  | Usage: following operations can be triggered:
  |  - 'merge.sh --input INPUT_TREE --reference REFERENCE_TREE'
  |  - 'merge.sh --scan A_TREE'
  |  - 'merge.sh --rescan A_TREE'
  |  - 'merge.sh --resync A_TREE'
  |  - 'merge.sh --uniquify A_TREE'
  |  - 'merge.sh -h' or 'merge.sh --help'

  | Ensures, for the first form, that all the changes in a possibly more up-to-date, "newer" tree (INPUT_TREE) are merged back
  | to the reference tree (REFERENCE_TREE), from which the first tree may have derived. Once executed, only a refreshed,
  | complemented reference tree will exist, as the input tree will have been removed: all its original content (i.e. its content
  | that was not already in the reference tree) will have been transferred in the reference tree.
  |  In the reference tree, in-tree duplicated content will be either kept as it is, or removed as a whole (to keep only one
  | copy thereof), or replaced by symbolic links in order to keep only a single reference version of each actual content.
  |  At the root of the reference tree, a '.merge-tree.cache' file will be stored, in order to avoid any later recomputations of
  | the checksums of the files that it contains, should they have not changed. As a result, once a merge is done, the reference
  | tree may contain an uniquified version of the union of the two specified trees, and the input tree will not exist anymore
  | after the merge.
  |
  |  For the second form (--scan option), the specified tree will simply be inspected for duplicates, and a corresponding
  | '.merge-tree.cache' file will be created at its root (to be potentially reused by a later operation).

  |  For the third form (--rescan option), an attempt to rebuild an updated '.merge-tree.cache' file will be performed,
  | computing only the checksum of the files that were not already referenced, or whose timestamp or size changed.

  |  For the fourth form (--resync option), a rebuild even lighter than the previous rescan of '.merge-tree.cache' will be done,
  | checking only sizes (not timestamps), and updating these timestamps.

  |  For the fifth form (--uniquify option), the specified tree will be scanned first (see the corresponding operation), and

  | For the fifth form (--uniquify option), the specified tree will be scanned first (see the corresponding operation), and
  | then the user will be offered various actions regarding found duplicates (being kept as are, or removed, or replaced with
  | symbolic links), and once done a corresponding up-to-date '.merge-tree.cache' file will be created at its root (to be
  | potentially reused by a later operation).
  |
  |  For the sixth form (-h or --help option), displays this help.
  |
  |  Note that the --base-dir A_BASE_DIR option can be specified by the user to designate the base directory of all relative
  | paths mentioned. When a cache file is found, it can be either ignored (and thus recreated) or re-used, either as it is or
  | after a weak check, where only file existence, sizes and timestamps are then verified (not checksums).

  See also: the ``test-all`` target of the merge-related `makefile <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/apps/merge-tool/GNUmakefile>`_, to give it a try before applying such procedure to your data of interest.



