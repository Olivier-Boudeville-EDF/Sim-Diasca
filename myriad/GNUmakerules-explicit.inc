
# Here are gathered explicit (immediate, static) specific rules for Myriad.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: MYRIAD_TOP must be set.


.PHONY: .PHONY-myriad-explicit

.PHONY-myriad-explicit:  all all-pre-hook all-parallel-recurse all-recurse     \
			  all-parse-transforms bootstrap-modules make-all-parse-transforms \
			  build-debug rebuild test test-recurse doc-recurse build-tests    \
			  launch shell                                                     \
			  install-all install-announce install-ebin install-bin            \
			  install-src install-include install-examples install-test        \
			  install-doc info-install info-install-locations                  \
			  info-install-content create-myriad-checkout                      \
			  rebar3-application set-rebar-conf set-rebar-conf-for-hex         \
			  rebar3-make-include-links rebar3-clean-include-links             \
			  rebar3-release rebar3-development-release                        \
			  rebar3-production-release rebar3-hide-unsupported-sources        \
			  rebar3-restore-unsupported-sources                               \
			  rebar3-compile-pre-hook rebar3-compile-post-hook                 \
			  rebar3-bulletproof-compile-pre-hook                              \
			  rebar3-bulletproof-compile-post-hook                             \
			  rebar3-update rebar3-compile rebar3-create-tree                  \
			  rebar3-populate-build-tree rebar3-create-app-file                \
			  rebar3-copy-beams rebar3-clean rebar3-real-clean                 \
			  rebar-test-clean                                                 \
			  rebar3-cache-clean rebar3-info rebar3-hex-publish                \
			  rebar3-local-update                                              \
			  test-hex-package                                                 \
			  sync-to-server                                                   \
			  generate-list-of-erlang-types generate-list-of-local-types       \
			  generate-local-plt self-check-against-plt write-debug-key-file   \
			  check check-hook help-erl                                        \
			  clean clean-erlang clean-layer-ebin clean-python clean-database  \
			  clean-recurse clean-outputs clean-ast-debug real-clean track     \
			  info info-files info-meta info-erlang info-python info-java


#dummy:
#	@echo "You are executing the default, do-nothing make rule. You may want specify an actual target instead (ex: 'make foo')."



all:


# Defining the BASE_MAKEFILE variable in top makefiles allows to define a
# specific (parallel) 'all' rule for them, while other makefiles just recurse
# accordingly:
#
ifeq ($(BASE_MAKEFILE),true)

# CORE_COUNT defined in myriad/GNUmakevars.inc:
#
# (add 'echo " Entering module directory $$m"' if needing to troubleshoot)
#
all: all-pre-hook all-parse-transforms rebar3-make-include-links
	@echo "  Building all, in parallel over $(CORE_COUNT) core(s), from $(PWD)"
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && \
		$(MAKE) -s all-recurse -j $(CORE_COUNT) && cd .. ; else echo "     (directory $$m skipped)" ; \
	fi ) ; then exit 1; fi ; done


# Not removing rebar.config anymore, to avoid that GIT considers it deleted on
# purpose:
#

# Also deactivated, as any dependency in _build would then be lost, whereas we
# might use on our native build system while still relying on them (ex: for
# LEEC): -@/bin/rm -rf $(MYRIAD_REBAR_BUILD_ROOT)
#
# (thus use the rebar3-real-clean target instead, if wanting to remove
#  _build-like directories)
#
clean: clean-layer-ebin
	-@if [ -d doc ] ; then cd doc && $(MAKE) -s clean ; fi

else

all: all-recurse-pre-hook all-recurse $(BEAM_FILES) $(JAVA_CLASS_FILES)

endif


all-parse-transforms: bootstrap-modules make-all-parse-transforms

bootstrap-modules: $(BOOTSTRAP_MODULES)


make-all-parse-transforms:
	@echo "  Building first all layer-specific parse transforms"
	@for d in $(PARSE_TRANSFORM_DIRS) ; do ( cd $$d && \
	$(MAKE) -s local-parse-transforms ) ; done


LOCAL_TRANSFORM_SRC = $(wildcard *_parse_transform.erl)

LOCAL_TRANSFORM_BEAM = $(patsubst %.erl,%.beam,$(LOCAL_TRANSFORM_SRC))


# It is necessary to compile parse-transforms from their directory, as they may
# use bootstrapped BEAMs or lower layers, so the _TOP variable must be correct
# in order the -pz directories to be relevant:
#
local-parse-transforms: $(LOCAL_TRANSFORM_BEAM)



# all*-pre-hook targets allow to insert arbitrary rules *before* other specified
# prerequisite targets, at each level.
#
# Meant to be enriched with prerequisite targets by above layers:

# Called once per layer, when starting compiling it:
all-pre-hook:



# Called once per subdirectory of the current layer, when starting compiling it:
all-recurse-pre-hook:



# This rule is not used by default, as we use the -j option from the root
# makefile, letting then 'make' use its job server for that task:
all-parallel-recurse:
	@echo "  Building all on parallel over $(CORE_COUNT) cores (in "$(PWD) #$$(basename $(PWD))
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s all -j $(CORE_COUNT) CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ; else echo "    (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done



# We used to define '$(BEAM_FILES)' as prerequisites to this target; however
# then the "Building all" message was output after its directory was processed,
# not before.
#
# To avoid triggering the dummy target should no BEAM file be found, inner make
# command is conditional.
#
all-recurse: $(BEAM_FILES)
	@echo "  Building all in "$(PWD) #$$(basename $(PWD))
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s all CMD_LINE_OPT="${CMD_LINE_OPT}" USE_HDF5=$(USE_HDF5) USE_JSON=$(USE_JSON) USE_REST=$(USE_REST) USE_SQLITE=$(USE_SQLITE) && cd .. ; else echo "  (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done



# To interpret more easily compilation errors during development:
build-debug:
	@$(MAKE) -s all CORE_COUNT=1 | less


rebuild: clean all




# Bootstrap section.


# Bootstrapped modules must be special-cased, as they should not be built with
# parse-transforms (by design not available yet) and the resulting BEAM must be
# produced in the right directory (not at the root).
#
# Exactly the same processing applies to each of them.
#
# See also: in GNUmakevars.inc, the BOOTSTRAP_MODULES variable.

# We cherry-pick modules in various source directories, and select all modules
# in 'src/meta', which are then built generically.

# A difficulty is that we have to generate each BEAM file in the same directory
# as their source (hence the 'cd $$(dirname $@)'), however the automatically-set
# MYRIAD_TOP variable points only to the root of Myriad. As a result, paths
# (typically include ones) will not be correct, so we added BOOTSTRAP_INCLUDES
# to insert the correct ones.


# Note:
#
# - these rules are rather convoluted, as apparently the -o command-line option
#	of erlc ignores any path prior to the target, and thus writes the resulting
#	BEAM into the current directory; we have thus to go in the target directory
#	first
#
# - maybe these (identical) rules can be factored with just different sources
#	and targets


# In 'utils':

$(MYRIAD_TOP)/src/utils/basic_utils.beam: $(MYRIAD_TOP)/src/utils/basic_utils.erl $(MYRIAD_TOP)/include/utils/basic_utils.hrl
	@echo "		Compiling bootstrap module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )


$(MYRIAD_TOP)/src/utils/text_utils.beam: $(MYRIAD_TOP)/src/utils/text_utils.erl
	@echo "		Compiling bootstrap module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )


$(MYRIAD_TOP)/src/utils/id_utils.beam: $(MYRIAD_TOP)/src/utils/id_utils.erl
	@echo "		Compiling bootstrap module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )



# In 'meta':

# Real parse-transform-specific rules:
# (actually this is an automatic rule, but that is not a problem here)
#
$(MYRIAD_TOP)/src/meta/%.beam: $(MYRIAD_TOP)/src/meta/%.erl
	@echo "		Compiling bootstrap meta module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )



# In 'data-management':

#$(MYRIAD_TOP)/src/data-management/list_table.beam: $(MYRIAD_TOP)/src/data-management/list_table.erl
#	@echo "		Compiling bootstrap module $<"
#	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )


$(MYRIAD_TOP)/src/data-management/map_hashtable.beam: $(MYRIAD_TOP)/src/data-management/map_hashtable.erl
	@echo "		Compiling bootstrap module $<"
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )




test: all test-recurse
	@for t in $(TEST_TARGETS); do if ! $(MAKE) -s $$(echo $$t|sed 's|_test$$|_run|1') CMD_LINE_OPT="${CMD_LINE_OPT} --batch"; then echo "Test failed." 1>&2 ; exit 1; else echo ; fi ; done


test-recurse:
	@echo "   Testing all in $$(basename $(PWD))"
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s test CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ; else echo "	  (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done


# Best placed here rather than in GNUmakerules-docutils.inc:
doc: doc-recurse

doc-recurse:
	@echo "   Preparing documentation in "$(PWD) #$$(basename $(PWD))
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then cd $$m && $(MAKE) -s doc CMD_LINE_OPT="${CMD_LINE_OPT}" && cd .. ; else echo "	 (directory $$m skipped)" ; fi ) ; then exit 1; fi ; done



# Not used any more now that all beams are always built:
build-tests: $(BEAM_FILES)
	@for t in $(TEST_TARGETS); do $(MAKE) -s $$t CMD_LINE_OPT="${CMD_LINE_OPT}"; done


launch: Emakefile
	@echo "		Launching interpreter with default Ceylan settings"
	@${ERL_PARAMETERIZED_LAUNCHER} $(NODE_NAMING) test_shell-$$USER $(EXEC_INTERNAL_OPTIONS) $(CMD_LINE_OPT)


# To have an Erlang shell with the right configuration to rely on Myriad (ex:
# with regard to code path):
#
shell:
	@echo "		Launching an Erlang shell enabled for $(PROJECT_NAME)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) shell-for-$$USER $(INTERNAL_OPTIONS)



# Section regarding installation and OTP integration.


# All of them should be available only from the root of the layer:
ifeq ($(BASE_MAKEFILE),true)


# Installation section (in an ad-hoc way).


# The goal here is to perform an installation of this layer in a prefixed
# directory (mostly regardless of OTP conventions; see the rebar3-* targets for
# that).


# For a development-ready install:
install-package: all doc install-all


# For a production-ready install:
install-prod-standard: all
	@sudo $(MAKE) install-prod PACKAGE_INSTALLATION_PREFIX=$(DEFAULT_PRODUCTION_INSTALLATION_PREFIX)



# In a production setting, We do not want to generate documentation in
# production or to install extra elements (such as tests, examples, etc.):
#
install-prod: all install-prod-all




install-all: install-announce install-bin install-ebin install-src \
	install-include install-examples install-test install-doc


install-prod-all: install-prod-announce install-bin install-ebin \
	install-src install-include



install-announce:
	@echo " Installing this package ($(PACKAGE_NAME)) for development \
	in $(PACKAGE_INSTALLATION_PREFIX)"


install-prod-announce:
	@echo "  Installing this package ($(PACKAGE_NAME)) for production \
	in $(PACKAGE_INSTALLATION_PREFIX)"


# The -L option for cp is used to force the copy of the file which is pointed to
# by a symbolic link, instead of copying that link, which is generally relative
# and broken if copied in an installation 'as is'.


install-bin:
	@if [ -n "$(EXEC_TO_INSTALL)" ] ; then \
	mkdir -p $(BIN_INSTALLATION_PREFIX) ; \
	/bin/cp $(EXEC_TO_INSTALL) $(BIN_INSTALLATION_PREFIX) ; fi


install-ebin:
	@echo "  Installing BEAM files in $(EBIN_INSTALLATION_PREFIX)"
	@if [ -n "$(BEAMS_TO_INSTALL)" ] ; then \
	mkdir -p $(EBIN_INSTALLATION_PREFIX) ; \
	/bin/cp -L $(BEAMS_TO_INSTALL) $(EBIN_INSTALLATION_PREFIX) ; fi


install-src:
	@if [ -n "$(SOURCES_TO_INSTALL)" ] ; then \
	mkdir -p $(SRC_INSTALLATION_PREFIX) ; \
	/bin/cp -L $(SOURCES_TO_INSTALL) $(SRC_INSTALLATION_PREFIX) ; fi


install-include:
	@if [ -n "$(INCLUDES_TO_INSTALL)" ] ; then \
	mkdir -p $(INCLUDE_INSTALLATION_PREFIX) ; \
	/bin/cp -L $(INCLUDES_TO_INSTALL) \
	$(INCLUDE_INSTALLATION_PREFIX) ; fi


install-examples:
	@if [ -n "$(EXAMPLES_TO_INSTALL_BEAMS)" ] ; then \
	mkdir -p $(EXAMPLES_INSTALLATION_PREFIX)/ebin ; \
	/bin/cp -L $(EXAMPLES_TO_INSTALL_BEAMS) \
	$(EXAMPLES_INSTALLATION_PREFIX)/ebin ; fi
	@if [ -n "$(EXAMPLES_TO_INSTALL_SRC)" ] ; then \
	mkdir -p $(EXAMPLES_INSTALLATION_PREFIX)/src ; \
	/bin/cp -L $(EXAMPLES_TO_INSTALL_SRC) \
	$(EXAMPLES_INSTALLATION_PREFIX)/src ; fi


install-test:
	@if [ -n "$(TESTS_TO_INSTALL_BEAMS)" ] ; then \
	mkdir -p $(TEST_INSTALLATION_PREFIX)/ebin ; \
	cp -L $(TESTS_TO_INSTALL_BEAMS) \
	$(TEST_INSTALLATION_PREFIX)/ebin ; fi
	@if [ -n "$(TESTS_TO_INSTALL_SRC)" ] ; then \
	mkdir -p $(TEST_INSTALLATION_PREFIX)/src ; \
	cp -L $(TESTS_TO_INSTALL_SRC) \
	$(TEST_INSTALLATION_PREFIX)/src ; fi


install-doc:
	@if [ -n "$(DOC_TO_INSTALL)" ] ; then \
	mkdir -p $(DOC_INSTALLATION_PREFIX) ; \
	/bin/cp -r -L $(DOC_TO_INSTALL) $(DOC_INSTALLATION_PREFIX) ; fi



info-install: info-install-locations info-install-content


info-install-locations:
	@echo   "PACKAGE_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)"
	@echo   "BIN_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/bin"
	@echo   "EBIN_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/ebin"
	@echo   "SRC_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/src"
	@echo   "INCLUDE_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/include"
	@echo   "EXAMPLES_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/examples"
	@echo   "TEST_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/test"
	@echo   "DOC_INSTALLATION_PREFIX = $(PACKAGE_INSTALLATION_PREFIX)/doc"
	@echo   "DEFAULT_PRODUCTION_INSTALLATION_PREFIX = $(DEFAULT_PRODUCTION_INSTALLATION_PREFIX)"


info-install-content:
	@echo "  EXEC_TO_INSTALL = $(EXEC_TO_INSTALL)"
	@echo "  BEAMS_TO_INSTALL = $(BEAMS_TO_INSTALL)"
	@echo "  SOURCES_TO_INSTALL = $(SOURCES_TO_INSTALL)"
	@echo "  INCLUDES_TO_INSTALL = $(INCLUDES_TO_INSTALL)"
	@echo "  EXAMPLES_TO_INSTALL_BEAMS = $(EXAMPLES_TO_INSTALL_BEAMS)"
	@echo "  EXAMPLES_TO_INSTALL_SRC = $(EXAMPLES_TO_INSTALL_SRC)"
	@echo "  TESTS_TO_INSTALL_BEAMS = $(TESTS_TO_INSTALL_BEAMS)"
	@echo "  TESTS_TO_INSTALL_SRC = $(TESTS_TO_INSTALL_SRC)"
	@echo "  DOC_TO_INSTALL = $(DOC_TO_INSTALL)"



# Section for the support of a rebar3+relx+hex compliant build.


# Create a _checkouts directory in which all Ceylan dependencies for the current
# layer are referenced through a symlink.
#
# Note that, in order for a layer to be listed as checkout, its root directory,
# or at least a symlink pointing to it, shall already exist as a sibling of the
# root of this layer.
#
# (no specific dependency here)
#
create-myriad-checkout:
	@echo "  Creating a checkout directory for Myriad"
	@mkdir -p _checkouts


# The two main rebar-based targets for application and release are:
# 'rebar3-application' and 'rebar3-release':


# Apparently, a pre-existing rebar.lock file might cause version dependencies
# not to be respected (ex: sticking to an older hex version of a dependency,
# whereas a newer one was specified), so we remove it when appropriate.
#
# (creating a ebin/*.app file is necessary at this point, otherwise no module
# will be compiled, leading to an empty application; the same applies - silently
# - if the conf/*.app.src, hence the ebin/*.app file, are invalid - ex: cannot
# be parsed)
#
# ('remove-rebar-conf' target not kept, as rebar.config shall not be deleted in
# GIT)
#
rebar3-application: rebar3-real-clean clean rebar3-update set-rebar-conf      \
					rebar3-make-include-links rebar3-hide-unsupported-sources \
					rebar3-create-app-file
	@echo "  Triggering the overall creation by rebar3 of an OTP application corresponding to $(PROJECT_NAME)"
	-@/bin/rm -f rebar.lock 2>/dev/null
	@$(REBAR_DEBUG) rebar3 compile
	@$(MAKE) -s rebar3-restore-unsupported-sources


# If making a rebar application or release, a symlink suffices, yet not if
# publishing an hex package (conf/ not included in the package, hence no symlink
# shall point to it, hence an actual copy of rebar configuration file), so we
# prefer to copy it in all cases (note that 'make rebar.config' in 'conf' will
# generate a new file but will *not* update the one at the package root, hence
# the purpose of this target):
#
set-rebar-conf:
	@echo "  Setting rebar configuration"
	@/bin/rm -f rebar.config 2>/dev/null
	@cd conf && $(MAKE) -s rebar.config && /bin/cp -f rebar.config ../


set-rebar-conf-for-hex:
	@echo "  Setting rebar configuration for hex"
	@/bin/rm -f rebar.config 2>/dev/null
	@cd conf && $(MAKE) -s rebar-for-hex.config && /bin/cp -f rebar-for-hex.config ../rebar.config


# This target creates appropriate symlinks for rebar, while preserving our
# intented structure; this is not needed for the current layer, but is required
# by any upper layer in order to compile:
#
rebar3-make-include-links:
	@echo "  Populating the include symlinks"
	@cd include && for d in $$(find * -type d 2>/dev/null); do for h in $$(/bin/ls $$d/*.hrl 2>/dev/null) ; do ln -sf $$h ; done ; done



# Gets rid of include symlinks created for rebar (and only the symlinks, as a
# layer may rely on flat, actual includes):
#
rebar3-clean-include-links:
	@echo "  Cleaning the include symlinks"
	@cd include && find . -maxdepth 1 -type l -exec /bin/rm -f '{}' ';'


# Either a symlink or a copy (at least in the case of hex):
remove-rebar-conf:
	@echo "  Removing rebar configuration"
	@/bin/rm -f rebar.config


# Apparently no way of telling rebar to exclude some source files, so:
rebar3-hide-unsupported-sources:
	@echo "  Hiding unsupported sources"
	@for f in $(UNSUPPORTED_SOURCES); do /bin/mv -f $$f $$f-hidden ; done 2>/dev/null || true


rebar3-restore-unsupported-sources:
	@echo "  Restoring unsupported sources"
	-@for f in $(UNSUPPORTED_SOURCES); do /bin/mv -f $$f-hidden $$f ; done 2>/dev/null || true



# Targets triggered by rebar compile hooks:


# Now we just create a symlink for each (public) header, so that software
# depending on this layer just have to include one 'include' directory (and
# remain happily unaware of its internal structure):
#
# (it is done as a pre-hook, not a post-one, as these symlinks are needed to
# compile Myriad itself when taken as a dependency - as the relative directories
# specified in {i,..} options apply only when compiling from the root, not from
# a directory offset like _build/lib/..., not only the software depending on it)
#
# (unsupported sources are also hidden, otherwise their build will be attempted
# and will fail)
#
rebar3-compile-pre-hook: rebar3-hide-unsupported-sources \
						 rebar3-make-include-links
	@echo "  rebar3 pre-compile hook executed in $$(pwd), with the '$(MYRIAD_REBAR_PROFILE)' profile"
	@$(MAKE) -s all


rebar3-compile-post-hook:
	@echo "  rebar3 post-compile hook executed in $$(pwd), with the '$(MYRIAD_REBAR_PROFILE)' profile"



# In some cases, notably when parse transforms are involved, reasonable, lighter
# hooks will not suffice, and we have to opt for stronger solutions in order to
# bypass the rebar3 logic as a whole (as much as possible):
#
rebar3-bulletproof-compile-pre-hook:
	@echo "  rebar3 bullet-proof pre-compile hook executed in $$(pwd), with the '$(MYRIAD_REBAR_PROFILE)' profile, for $(PROJECT_NAME_FOR_REBAR)"
	@$(MYRIAD_TOP)/conf/fix-rebar-compile-pre-hook.sh $(PROJECT_NAME_FOR_REBAR)


rebar3-bulletproof-compile-post-hook:
	@echo "  rebar3 bullet-proof post-compile hook executed in $$(pwd), with the '$(MYRIAD_REBAR_PROFILE)' profile, for $(PROJECT_NAME_FOR_REBAR)"
	@$(MYRIAD_TOP)/conf/fix-rebar-compile-post-hook.sh $(PROJECT_NAME_FOR_REBAR)


# By default, development releases, not production ones:
rebar3-release: rebar3-development-release


# Creating a ebin/*.app file is necessary here, otherwise this layer is a
# dependency "not reachable by the system" (same error is the .app exists yet is
# invalid):
#
pre-release: rebar3-real-clean clean rebar3-update          \
			 rebar3-hide-unsupported-sources set-rebar-conf \
			 rebar3-create-app-file
	-@/bin/rm -f rebar.lock 2>/dev/null


# rebar.config not to be deleted, hence 'remove-rebar-conf' target removed:
post-release: rebar3-restore-unsupported-sources


rebar3-development-release: pre-release
	@echo "  Triggering the overall creation by rebar3 of a (relx-based) development release including $(PROJECT_NAME)"
	@$(REBAR_DEBUG) rebar3 release
	@$(MAKE) -s post-release


# Supposedly more functional/flexible than 'rebar3 release -d false' or 'rebar3
# release -n prod':
#
rebar3-production-release: pre-release
	@echo "  Triggering the overall creation by rebar3 of a (relx-based) production release including $(PROJECT_NAME)"
	@$(REBAR_DEBUG) rebar3 as prod release
	@$(MAKE) -s post-release


# Needed to update local package information according to (server-side) hex,
# otherwise rebar could stick locally to obsolete package versions (ex: if
# having just published a new version of a dependency)
#
rebar3-update:
	@echo "  Updating rebar-hex package repository"
	@$(REBAR_DEBUG) rebar3 update




# Below are listed targets - now not used anymore - called by hooks specified to
# rebar3 through the rebar.config file:


# This (now obsolete) overall target used to be called (thanks to a pre-hook)
# when running directly 'rebar3 compile' or indirectly (ex: 'rebar3 release')
#
# (note: we used to copy sources before compiling and copying BEAM files, so
# that rebar3 would not try to recompile modules by itself)
#
rebar3-compile:
	@echo "  Preparing $(PROJECT_NAME) for a rebar3-compliant build (in $(MYRIAD_REBAR_BUILD_DIR))"
	@$(MAKE) -s set-rebar-conf rebar3-clean rebar3-create-tree \
	rebar3-populate-build-tree rebar3-create-app-file all rebar3-copy-beams \
	MYRIAD_REBAR_PROFILE=$(MYRIAD_REBAR_PROFILE) MYRIAD_REBAR_BUILD_DIR=$(MYRIAD_REBAR_BUILD_DIR)


# See for more information:
# http://erlang.org/doc/design_principles/applications.html#directory-structure
#
# Note: we now create a symlink directly pointing to the real 'src' tree (most
# probably useless, as rebar creates it by itself), instead of copying the
# source files as a whole in the build tree. It is almost mandatory, as the URL
# above tells us that this src directory in _build should never be deeper than
# one level (whereas we have more levels than that), and apparently if rebar3 is
# not able to locate, for an ebin/X.beam, a X.erl, it will attempt to recompile
# it (and obviously fail).
#
# See https://www.rebar3.org/discuss/5d1c82951e0cbe00121f12db for more details.
#
rebar3-create-tree:
	@echo "  Creating rebar3 build tree in $(MYRIAD_REBAR_BUILD_DIR)"
	@mkdir -p $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)/{priv/examples,include,doc,test,ebin}
	@cd $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)/ && ln -sf ../../../../src


# Currently, 'priv' is not specifically populated, and all header files
# (including internal ones) are copied to 'include':
#
# (note that the sources of conditionally-compiled modules like sql_support are
# intentionally left out, otherwise the release as a whole will be considered as
# an unreachable_package: 'Dependency XXX is specified as a dependency but is
# not reachable by the system.'; they are also explicitly excluded in the
# exclude_files section of the .app.src file)
#
# The symptom can be better seen with REBAR_DEBUG=1 above; for example:
#
# ===> Resolving OTP Applications from directories:
# [...]
#
# ===> Missing beam file sql_support
#  [...]/_build/default/lib/myriad/ebin/sql_support.beam
#
# Solution: not listing such conditional BEAM files among the modules in the
# .app.src file.
#
rebar3-populate-build-tree:
	@echo "  Copying sources in rebar3 build tree"
	@true "Using a src symlink, hence disabled: find src -name '*.erl' -a ! -name '*_test.erl' -a ! -name sql_support.erl -a ! -name hdf5_support.erl -exec /bin/cp '{}' $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)/src/ ';'"
	@# As tests may be in a separate 'tests' directory for example, and we do not want to select the content of _build once copied, in order to be idempotent:
	@find src tests -name '*_test.erl' -exec /bin/cp '{}' $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)/test/ ';' 1>/dev/null 2>&1 | true
	@find src -name '*.hrl' -exec /bin/cp '{}' $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)/include/ ';'
	@find examples -name '*rl' -exec /bin/cp '{}' $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)/priv/examples ';' 1>/dev/null 2>&1 | true
	@/bin/cp LICENSE README.md $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)/doc


# We generate a .app file and place it both in the OTP build tree (by default:
# _build/lib/...) and in an ebin directory directly at the GIT root, as it is
# expected when having specified a GIT dependency with rebar, such as: {myriad,
# {git, "git://github.com/Olivier-Boudeville/Ceylan-Myriad"} }
#
# For the listed modules, this target removes paths, extension, replaces
# newlines with commas, and removes the last one.
#
# Not specifically removing source files, each .app.src does that thanks its
# 'exclude_files' entry, with MODULES_FOR_REBAR:
#
# -a ! -name sql_support.erl -a ! -name hdf5_support.erl
#
# A .app file seems to be necessary in all cases, to generate: an application
# (otherwise nothing is built), a release (otherwise dependency not reachable),
# and probably an hex package as well.
#
# Apparently, at least in some cases (ex: generating a standalone release), an
# (updated) *.app.src file is looked-up in src (not in ebin - try to create
# syntax error in it to check), so we add a symbolic link there as well.
#
# We have thus:
# ./conf/FOOBAR.app.src [only real source]
# ./_build/lib/FOOBAR/ebin.FOOBAR.app
# ./ebin/FOOBAR.app
# ./src/FOOBAR.app.src -> ../ebin/FOOBAR.app
#
rebar3-create-app-file:
	@echo "  Generating rebar3 $(PROJECT_NAME_FOR_REBAR).app file"
	@# This target may be called for hex for example:
	@mkdir -p $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)/ebin $(LAYER_TOP)/ebin
	@all_modules=$$(find src -name '*.erl' -a ! -name '*_test.erl' $(MYRIAD_REBAR_FIND_SRC_OPT) -exec echo '{}' ';' | sed 's|.*/||1' | sed 's|.erl$$||1' | sort | tr '\n' ','| sed 's|,|, |g' | sed 's|, $$||1' ) ; cat conf/$(PROJECT_NAME_FOR_REBAR).app.src | sed "s|VERSION_FOR_REBAR|$(VERSION_FOR_REBAR)|1" | sed "s|MODULES_FOR_REBAR|$$all_modules|1" > $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)/ebin/$(PROJECT_NAME_FOR_REBAR).app
	@/bin/cp -f $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)/ebin/$(PROJECT_NAME_FOR_REBAR).app $(LAYER_TOP)/ebin/
	@cd $(LAYER_TOP)/src ; ln -sf ../ebin/$(PROJECT_NAME_FOR_REBAR).app $(PROJECT_NAME_FOR_REBAR).app.src


# Currently excluding test BEAMs, for a smaller size:
rebar3-copy-beams:
	@echo "  Copying non-test BEAM modules in rebar3 build tree"
	@find src -name '*.beam' -a ! -name '*_test.beam' -exec /bin/cp '{}' $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)/ebin/ ';'


rebar3-clean: rebar3-clean-include-links rebar3-restore-unsupported-sources
	@echo "  Removing any prior rebar3 build tree for this project ($(MYRIAD_REBAR_BUILD_BASE))"
	-@/bin/rm -rf $(MYRIAD_REBAR_BUILD_BASE) 2>/dev/null
	-@/bin/rm -f ebin/$(PROJECT_NAME_FOR_REBAR).app


# Adding _build for increased safety:
rebar3-real-clean: rebar3-clean rebar-test-clean rebar3-cache-clean
	@echo "  Removing any prior rebar3 build root ($(MYRIAD_REBAR_BUILD_ROOT)), hence for all profiles"
	-@/bin/rm -rf $(MYRIAD_REBAR_BUILD_ROOT) _build


rebar-test-clean:
	@echo  "  Cleaning rebar3+hex test directory $(MYRIAD_REBAR_TEST_DIR)"
	-@/bin/rm -rf $(MYRIAD_REBAR_TEST_DIR)/_build
	-@/bin/rm -f $(MYRIAD_REBAR_TEST_DIR)/rebar.lock


# Otherwise could reuse wrong cached versions (ex: 1.O.6.tar instead of
# 1.0.7.tar), or a former version of a recently-submitted package:
# (actually triggering the rebar3-update target should suffice)
#
rebar3-cache-clean:
	@echo "  Removing rebar3 cache for $(PROJECT_NAME_FOR_REBAR)"
	@# Wiping dependencies and all as well: @/bin/rm -f $$HOME/.cache/rebar3/hex/hexpm/packages/$(PROJECT_NAME_FOR_REBAR)-*
	-@/bin/rm -f $$HOME/.cache/rebar3/hex/hexpm/packages/*


rebar3-info:
	@echo "REBAR_DEBUG = $(REBAR_DEBUG)"
	@echo "MYRIAD_REBAR_PROFILE = $(MYRIAD_REBAR_PROFILE)"
	@echo "MYRIAD_REBAR_BUILD_ROOT = $(MYRIAD_REBAR_BUILD_ROOT)"
	@echo "MYRIAD_REBAR_BUILD_DIR = $(MYRIAD_REBAR_BUILD_DIR)"
	@echo "MYRIAD_REBAR_FIND_SRC_EXCLUDES = $(MYRIAD_REBAR_FIND_SRC_EXCLUDES)"
	@echo "MYRIAD_REBAR_FIND_SRC_OPT = $(MYRIAD_REBAR_FIND_SRC_OPT)"
	@echo "MYRIAD_REBAR_TEST_DIR = $(MYRIAD_REBAR_TEST_DIR)"
	@echo "MYRIAD_REBAR_EXEC = $(MYRIAD_REBAR_EXEC)"
	@echo "VERSION_FOR_REBAR = $(VERSION_FOR_REBAR)"
	@echo "MYRIAD_REBAR_BUILD_BASE = $(MYRIAD_REBAR_BUILD_BASE)"
	@echo "MYRIAD_OTP_BEAM_DIR_FROM_USUAL = $(MYRIAD_OTP_BEAM_DIR_FROM_USUAL)"


# Changes had to be applied for hex packages that were very different from the
# ones for applications or releases. Now that we defined an (optional) rebar
# build, a specific rebar.config for the generation of hex packages is not
# needed anymore, and some targets below were simplified / are not used anymore
# (ex: no more hook needed).


# The simplest studying approach was to use the build of another hex package
# relying on Myriad to test how relevant is its package: 'make
# rebar3-application' done for WOOPER (which depends on Myriad, specified here
# as an hex dependency) shows notably that a
# ~/.cache/rebar3/hex/hexpm/packages/myriad-x.y.z.tar file is downloaded, which
# contains the VERSION, CHECKSUM, metadata.config and contents.tar.gz files.
#
# This last archive shows that the whole Myriad source tree is preserved, in its
# original, nested form, except for the root GNUmake* files (as a result, no
# rebar3 _build tree is used at all). So Myriad should copy them in priv/ and
# the compile hook is to copy them from priv/ to the root of the extracted
# package build tree.
#
# So, apparently:
#
# - a temporary priv/ is to be used
#
# - BEAMs shall not be included in the archive, as hex packages are source
#   containers, not binary ones
#
# - a correct, non-template, src/$(PROJECT_NAME_FOR_REBAR).app.src is needed
#
# To publish a package, 'rebar3 hex user auth' is expected to have been executed
# at least once on the publishing computer.
#
# We thought that rebar.lock could/should be removed before publishing a
# package, however it is a direct by-product of 'rebar3 hex publish', so nothing
# is done at this level.
#
# For an (hex) package, we:
#
# - copy all nested headers directly in the root of 'include', to ensure they
# will be found by users
#
# - hide temporarily the excluded sources from hex, to prevent their
# recompilation (hex does not take into account the .app.src file, hence does
# not rely on explicitly-specified modules to build)
#
# - remove rebar.lock, otherwise a past myriad dep (ex: GIT-based) will be
# reused
#
# - afterwards we restore the ebin/*.app and rebar.config files for standard
# releases, in order to avoid having them considered as deleted by GIT
#
rebar3-hex-publish: rebar3-clean clean set-rebar-conf-for-hex rebar3-create-app-file
	@/bin/cp -f $(MYRIAD_REBAR_BUILD_BASE)/ebin/$(PROJECT_NAME_FOR_REBAR).app src/$(PROJECT_NAME_FOR_REBAR).app.src
	@cd include && find * -mindepth 1 -a -name '*.hrl' -exec ln -sf '{}' ';'
	@cd src && for f in $(EXCLUDED_SOURCES); do find . -name "$$f" -exec /bin/mv -f '{}' "{}-hidden" ';' ; done
	@#mkdir -p priv/hex-packaging/{root,doc}
	@#/bin/cp -f $(MYRIAD_TOP)/conf/hex-compile-hook-script.sh priv/hex-packaging/
	@#/bin/cp -f GNUmake* priv/hex-packaging/root
	@#/bin/cp -f doc/GNUmake* priv/hex-packaging/doc
	-@/bin/rm -f rebar.lock 2>/dev/null
	@$(REBAR_DEBUG) rebar3 hex publish
	@cd src && to_rename=$$(for f in $(EXCLUDED_SOURCES); do find . -name "$$f-hidden" ; done) && for f in $$to_rename ; do /bin/mv -f $$f $$(echo $$f | sed 's|-hidden||1')  ; done
	@#/bin/rm -rf priv/
	@/bin/rm -f src/$(PROJECT_NAME_FOR_REBAR).app.src
	@$(MAKE) -s rebar3-clean rebar3-create-app-file set-rebar-conf


# Nothing to do for Myriad, not having dependency:
rebar3-local-update:


test-hex-package: rebar-test-clean
	@echo "  Testing $(PROJECT_NAME) hex package ('$(PROJECT_NAME_FOR_REBAR)' package, version $(VERSION_FOR_REBAR)) in $(MYRIAD_REBAR_TEST_DIR) using $(MYRIAD_REBAR_EXEC)"
	@cd conf && $(MAKE) -s rebar-for-testing.config && /bin/cp -f rebar-for-testing.config $(MYRIAD_REBAR_TEST_DIR)/rebar.config
	@cd $(MYRIAD_REBAR_TEST_DIR) && $(MYRIAD_REBAR_EXEC) update && $(MYRIAD_REBAR_EXEC) compile


# End of the longer rebar-related section.


# Synchronises the sources of the current layer to the specified server location.
#

# A rebuild is triggered just before, as the BEAM files are transferred as well:
# at arrival timestamps may not be respected and we want the right BEAMs to be
# executed.
#
# Note that only the source files are synchronised (ex: not the makefiles, not
# the configuration files, etc.).
#
sync-to-server: all
	@echo " Synchronising the $$(basename $$(pwd)) layer to $(WEB_SRV):$(CEYLAN_SYNC_TARGET_ROOT)"
	@if [ -n "$(WEB_SRV)" ]; then if [ -n "$(CEYLAN_SYNC_TARGET_ROOT)" ]; then $(SYNC_TOOL) $(SYNC_OPT) src include $(WEB_SRV):$(CEYLAN_SYNC_TARGET_ROOT)/$$(basename $$(pwd)); else echo "Error, no CEYLAN_SYNC_TARGET_ROOT variable set." 1>&2 ; exit 4 ; fi ; else echo "Error, no WEB_SRV variable set." 1>&2 ; exit 5 ; fi


# Allows to generate a file listing all the types defined in the specified
# Erlang source tree.
#
# Ex: 'make generate-list-of-erlang-types ERLANG_SOURCE_ROOT=~/otp_src_R15B/'
#
generate-list-of-erlang-types:
	@if [ -z "$$ERLANG_SOURCE_ROOT" ] ; then echo "  Error, no ERLANG_SOURCE_ROOT variable specified." ; exit 15 ; fi ; if [ ! -d "$$ERLANG_SOURCE_ROOT" ] ; then echo "  Error, specified directory (ERLANG_SOURCE_ROOT=$$ERLANG_SOURCE_ROOT) does not exist." ; exit 16 ; fi ; target_file="declared-types-in-Erlang.txt" ; erl="$(ERLANG_INTERPRETER)" ; target_path=$$(echo $$erl|sed "s|/bin/erl$$|/$$target_file|1") ; echo "  Generating the list of types declared in the Erlang runtime..." &&  $(MYRIAD_TOP)/src/scripts/list-available-types.sh "$$ERLANG_SOURCE_ROOT" > $$target_path && echo "  Types have been collected in '$$target_path'."


FAKE_OTP_ROOT := /tmp/ceylan-fake-otp-projects/$(PROJECT_NAME_FOR_REBAR)

# Section for newer OTP management: we create now automatically from our actual
# source tree (in GIT) a simpler, fake one that respects the conventions that
# rebar3 expects (instead of trying to have our actual build tree be somewhat
# compliant with rebar3, which proved utterly complicated and frustrating) .
#
# Unfortunately this will not work either, as even in this case rebar will try
# to recompile these BEAMs... (and fail; sigh).
#
create-fake-otp-project: all
	@echo "  Creating a fake OTP project for $(PROJECT_NAME_FOR_REBAR) in $(FAKE_OTP_ROOT)"
	-@/bin/rm -rf $(FAKE_OTP_ROOT)
	@mkdir -p $(FAKE_OTP_ROOT)/{src,include,priv,ebin}
	@find . -name '*.erl' -a ! -name '*_test.*' -exec /bin/cp -f '{}' $(FAKE_OTP_ROOT)/src/ ';'
	@find . -name '*.hrl' -a ! -name '*_test.*' -exec /bin/cp -f '{}' $(FAKE_OTP_ROOT)/include/ ';'
	@find . -name '*.beam' -a ! -name '*_test.*' -exec /bin/cp -f '{}' $(FAKE_OTP_ROOT)/ebin/ ';'
	# Copy rebar.config in root, myriad.app in ebin, etc.


real-clean: rebar3-real-clean

# End of the section regarding installation and OTP integration:
endif



generate-list-of-local-types:
	@echo "    Listing all types defined in package '$(PACKAGE_NAME)', in the $$(basename $(TYPE_LIST_FILE)) file"
	@$(MYRIAD_TOP)/src/scripts/list-available-types.sh > $(TYPE_LIST_FILE)


# Yes, this is an explicit, not automatic, rule:
$(PLT_FILE): generate-local-plt


# We do not want to issue an error if only warnings (code: 2) were emitted.
#
# Previously, the -r option was only aimed at the $(PACKAGE_TOP)/src directory,
# yet for some reason the same rule once applied to other packages, such as
# Traces, resulted in their tests, in $(PACKAGE_TOP)/test, to be type-checked as
# well. As some of these tests rely on functions exported by Myriad tests (which
# were not included in the PLT), their type-checking reported these functions as
# unknown. Now we include $(PACKAGE_TOP)/test as well (not the full package to
# avoid that Dialyzer potentially recurses in rebar3 trees such as _build or
# _checkouts)
#
generate-local-plt: add-prerequisite-plts
	@echo "    Generating PLT for $(PACKAGE_NAME) ($(PLT_FILE)), based on $(BASE_PLT)"
	@$(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(PACKAGE_TOP)/src $(PACKAGE_TOP)/test --plt $(BASE_PLT); if [ $$? -eq 1 ] ; then exit 1 ; else exit 0 ; fi


# Made to be enriched (with child targets) on a per-layer basis:
add-prerequisite-plts:
	@echo "Prerequisite PLTs added."


# If Dialyzer complains that it cannot get Core Erlang code for a set of BEAM
# files (ex: with "Recompile with +debug_info or analyze starting from source
# code"), then the element lacking is most probably not the +debug_info switch
# when compiling these BEAMs, but the debug_info key when running Dialyzer
# itself.
#
# Such a key can be for example specified in ~/.erlang.crypt, as:
# [{debug_info, des3_cbc, [], "Ceylan-Myriad"}].
#
# See also the write-debug-key-file target.
#
self-check-against-plt: all $(PLT_FILE)
	@echo " Checking $(PACKAGE_NAME) against its PLT ($(PLT_FILE))"
	@$(DIALYZER) $(DIALYZER_OPT) --plt $(PLT_FILE) -r .


# Stores on disk the current debug key for BEAMs, so that tools like Dialyzer
# can determine it:
#
write-debug-key-file:
	@echo "[{debug_info, des3_cbc, [], \""$(DEBUG_INFO_KEY)"\"}]." > ~/.erlang.crypt



# Far easier target name to remember:
# (hook added to block this target if not relevant from the current location)
#
check: check-hook clean all test generate-local-plt self-check-against-plt


check-hook:


help-erl:
	@echo "To test hello.erl: 'erl', then 'c(hello).'," \
		"then 'hello:world().', then CTRL-C CTRL-C"


clean: clean-erlang clean-python clean-java clean-database clean-override \
	   clean-recurse


# Not calling the 'clean-layer-ebin' target from here (too demanding):
clean-erlang:
	@echo "   Cleaning all in "$(PWD) #$$(basename $(PWD))
	-@/bin/rm -f *.beam *.jam erl_crash.dump erlang.log.*


# Otherwise older BEAM files may linger and cause hard to debug errors:
clean-layer-ebin:
	@echo "   Cleaning $(PROJECT_NAME) top ebin directory"
	-@/bin/rm -f $(LAYER_TOP)/ebin/*.beam


# We can have a Python binding:
clean-python:
	-@/bin/rm -rf '__pycache__'


# We can have a Java binding:
clean-java:
	-@/bin/rm -f *.class


clean-database:
	-@/bin/rm -rf Mnesia.*@*


# This target has been added in order to allow the packages making use of
# 'Myriad' to define their specific cleaning rules.
#
# They could have defined in their own GNUmakerules-explicit.inc something like:
#	clean: clean-mypackage
#	clean-mypackage:
#	  -@/bin/rm foo.mypackage
# but then this 'clean' target would become the default one, whereas we expect
# the 'all' default target of 'Myriad' to be triggered.
# Thus we finally allowed upper packages to specify their cleaning thanks to:
# 'FILES_TO_CLEAN += foo.mypackage' in their GNUmakevars.inc
clean-override:
	@#echo "FILES_TO_CLEAN = $(FILES_TO_CLEAN)"
	-@/bin/rm -f $(FILES_TO_CLEAN)



ifeq ($(ROOT_MAKEFILE),true)

# Just more talkative:
clean-recurse:
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then      \
	echo "  Cleaning in '$$(basename $$m)' layer" ; cd $$m &&      \
	$(MAKE) -s clean CMD_LINE_OPT="${CMD_LINE_OPT}" &&             \
	cd .. ; else echo "    (directory $$m skipped)" ; fi ) ; then  \
	exit 1; fi ; done

else

clean-recurse:
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then      \
	cd $$m && $(MAKE) -s clean CMD_LINE_OPT="${CMD_LINE_OPT}" &&   \
	cd .. ; else echo "    (directory $$m skipped)" ; fi ) ; then  \
	exit 1; fi ; done

endif


# Removes the debug files created about ASTs:
clean-ast-debug: clean-ast-debug-myriad

clean-ast-debug-myriad:
	@find . -name 'Myriad-*put-AST*.txt' -exec /bin/rm -f '{}' ';' 2>/dev/null || true




clean-outputs:


# To be enriched by user layers:
real-clean: clean clean-ast-debug real-clean-recurse


#	@echo "real-clean-recurse in $$(pwd)"
real-clean-recurse:
	@for m in $(MODULES_DIRS); do if ! ( if [ -d $$m ] ; then      \
	cd $$m && $(MAKE) -s real-clean CMD_LINE_OPT="${CMD_LINE_OPT}" &&   \
	cd .. ; else echo "     (directory $$m skipped)" ; fi ) ; then \
	exit 1; fi ; done


# We prefer tracking the HTML version thereof (rather than the PDF one):
track:
	@$(DOC_TRACKER) $(OVERALL_DOCUMENT_SOURCE)


info: info-files
	@echo "FQDN                   = $(FQDN)"
	@echo "BEAM_DIRS              = $(BEAM_DIRS)"
	@echo "BEAM_PATH_OPT          = $(BEAM_PATH_OPT)"
	@echo "ARCHIVE_FILE           = $(ARCHIVE_FILE)"
	@echo "PROJECT_NAME           = $(PROJECT_NAME)"
	@echo "PROJECT_VERSION        = $(PROJECT_VERSION)"
	@echo "PACKAGE_NAME           = $(PACKAGE_NAME)"
	@echo "PLT_FILE               = $(PLT_FILE)"
	@echo "PREDECESSOR_PLT        = $(PREDECESSOR_PLT)"


info-files:
	@echo "ERL_FILES       = $(ERL_FILES)"
	@echo "BEAM_FILES      = $(BEAM_FILES)"
	@echo "TEST_SOURCES    = $(TEST_SOURCES)"
	@echo "TEST_TARGETS    = $(TEST_TARGETS)"
	@echo "ERLANG_PLT_FILE = $(ERLANG_PLT_FILE)"
	@echo "PLT_FILE        = $(PLT_FILE)"


info-meta:
	@echo "META_SRC_FILES = $(META_SRC_FILES)"
	@echo "META_BEAM_FILES = $(META_BEAM_FILES)"


info-erlang:
	@echo "ERLANG_INTERPRETER     = $(ERLANG_INTERPRETER)"
	@echo "ERLANG_INTERPRETER_OPT = $(ERLANG_INTERPRETER_OPT)"
	@echo "ERLANG_ROOT            = $(ERLANG_ROOT)"
	@echo "ERLANG_SRC             = $(ERLANG_SRC)"
	@echo "VM_TEST_NAME           = $(VM_TEST_NAME)"


# No Python source or binary files to be specifically managed by the build
# infrastructure itself.
#
info-python:
	@echo "USE_PYTHON_BINDING = $(USE_PYTHON_BINDING)"


info-java:
	@echo "USE_JAVA_BINDING = $(USE_JAVA_BINDING)"
	@echo "JAVA_FILES = $(JAVA_FILES)"
	@echo "JAVA_CLASS_FILES = $(JAVA_CLASS_FILES)"
	@echo "JINTERFACE_ROOT = $(JINTERFACE_ROOT)"
	@echo "JAVAC_OPT = $(JAVAC_OPT)"


# Put here, as defining explicit targets:
include $(MYRIAD_TOP)/doc/GNUmakerules-docutils.inc
