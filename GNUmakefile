.PHONY: help help-traces                                                \
	all all-pre-hook-local overall-version-file all-bindings            \
	prerequisites rebuild generate-all-plt                              \
	generate-list-of-all-types link-host-candidates test-production     \
	check-hook check-hook-local full-check plt-check                    \
	release release-zip release-bz2 release-in-clone release-doc        \
	prepare-release remove-release-tree clean clean-checkouts           \
	clean-prerequisites clean-generated-files                           \
	clean-all-results real-clean real-clean-local                       \
	install to-archive to-archive-full vcs-archive archive              \
	update-third-party-mirror release stats                             \
	info info-files info-local-files info-paths info-build              \
	info-archive info-version info-release


# Root of the Sim-Diasca based software stack (absolute paths are preferred):
ROOT_TOP := .
#ROOT_TOP := $(PWD)


# Allows to strictly build a layer before the above one (hence it is a bottom-up
# sequential build), while each layer is internally built in parallel:

# Marks this makefile as the root one (hence different rules may apply):
ROOT_MAKEFILE := true

# We are not at the base of a layer here:
BASE_MAKEFILE := false


# As layers form a chain, we only have to specify where the one just below the
# current one is:
#
#MYRIAD_TOP            := $(ROOT_TOP)/myriad
#WOOPER_TOP            := $(ROOT_TOP)/wooper
#TRACES_TOP            := $(ROOT_TOP)/traces
#SIM_DIASCA_TOP        := $(ROOT_TOP)/sim-diasca
MOCK_SIMULATORS_TOP    := $(ROOT_TOP)/mock-simulators
SUSTAINABLE_CITIES_TOP := $(ROOT_TOP)/sustainable-cities
DIST_SYS_SIM_TOP       := $(ROOT_TOP)/distributed-system-simulations
ACME_TOP               := $(ROOT_TOP)/acme-model
PLANNING_SIM_TOP       := $(ROOT_TOP)/planning-simulations


PREREQUISITES_DIRS = $(MYRIAD_TOP) $(WOOPER_TOP) $(TRACES_TOP)


# Allow for conditional rules:

# Disabled, as currently not updated yet with regard to the newer WOOPER
# conventions:
#
#HAS_SUSTAINABLE_CITIES := $(shell if [ -d $(SUSTAINABLE_CITIES_TOP) ]; \
#	then echo "true" ; else echo "false" ; fi)

HAS_SUSTAINABLE_CITIES := false


HAS_DIST_SYS_SIM := $(shell if [ -d $(DIST_SYS_SIM_TOP) ]; \
	then echo "true" ; else echo "false" ; fi)

#HAS_DIST_SYS_SIM := true


HAS_ACME := $(shell if [ -d $(ACME_TOP) ]; \
	then echo "true" ; else echo "false" ; fi)

#HAS_ACME := true


HAS_PLANNING_SIM := $(shell if [ -d $(PLANNING_SIM_TOP) ]; \
	then echo "true" ; else echo "false" ; fi)

#HAS_PLANNING_SIM := true


# Modules which will be built iff they are available on this distribution:

ifeq ($(HAS_SUSTAINABLE_CITIES),true)
	OPTIONAL_MODULES_DIRS += $(SUSTAINABLE_CITIES_TOP)
endif


ifeq ($(HAS_DIST_SYS_SIM),true)
	OPTIONAL_MODULES_DIRS += $(DIST_SYS_SIM_TOP)
endif


ifeq ($(HAS_ACME),true)
	OPTIONAL_MODULES_DIRS += $(ACME_TOP)
endif


ifeq ($(HAS_PLANNING_SIM),true)
	OPTIONAL_MODULES_DIRS += $(PLANNING_SIM_TOP)
endif




# 'Myriad', 'WOOPER', 'Traces' not listed anymore here, as deemed to be external
# prerequisites.
# 'applications' not listed anymore here, as deemed to be only Sim-Diasca using
# code.
#
MODULES_DIRS = $(PREREQUISITES_DIRS) $(SIM_DIASCA_TOP) $(MOCK_SIMULATORS_TOP) \
			   $(OPTIONAL_MODULES_DIRS)


# We ought to install the tool base, not its related examples (knowing that
# moreover mock-simulators and sustainable-cities would each centralise all the
# BEAMs from their various cases in a single location, leading to name clashes;
# ex: class_City.beam)
#
INSTALLED_DIRS = $(PREREQUISITES_DIRS) $(SIM_DIASCA_TOP)


# For all standalone applications in Mock Simulators:
MOCK_DIRS = $(MOCK_SIMULATORS_TOP)/soda-test    \
			$(MOCK_SIMULATORS_TOP)/ssi-test     \
			$(MOCK_SIMULATORS_TOP)/city-example



# So that for example the archive file is named correspondingly:
PROJECT_NAME = Sim-Diasca


# We do not want to include the 'mock-simulators' directory here as such, as we
# have to include directly some of its subdirectories instead:
#
PLT_TARGETS = $(PREREQUISITES_DIRS) $(SIM_DIASCA_TOP) $(MOCK_DIRS)


# Default target:
help: help-intro help-root


include $(ROOT_TOP)/GNUmakesettings.inc



help-intro:
	@echo " Following main make targets are available from this root of the Sim-Diasca code base:"
	@echo "  - 'rebuild':       cleans, builds and generates list of all types"
	@echo "  - 'release':       generates a source release for the $(SIM_DIASCA_VERSION) version "
	@echo "  - 'release-doc':   generates a documentation package for the $(SIM_DIASCA_VERSION) version "


help-root:
	@cd $(SIM_DIASCA_TOP) && $(MAKE) -s help-sim-diasca


# Ensures that the package version file is, if needed, recreated first:
all-recurse-pre-hook: overall-version-file

overall-version-file: $(VERSION_FILE)


# Ensures that, whether or not the bindings are enabled, they are built.
all-bindings:
	@$(MAKE) -s all USE_PYTHON_BINDING=true USE_JAVA_BINDING=true


# No generic 'register-version-in-header' target can be defined, as the Erlang
# defines may be arbitrarily defined by each layer.
#
# Do not use 'echo -e' here, a faulty header would be generated.
#
$(VERSION_FILE):
	@mkdir -p $$(dirname $(VERSION_FILE))
	@echo "   Generating the header file collecting all package versions" \
	"($$(basename $(VERSION_FILE)))"
	@echo "% Header automatically generated by the 'all-pre-hook'"  > $(VERSION_FILE)
	@echo "% make target." >> $(VERSION_FILE)
	@echo "" >> $(VERSION_FILE)
	@for m in $(MODULES_DIRS); do (cd $$m && echo " - generating version file in $$(basename $$(pwd))" && $(MAKE) -s register-version-in-header VERSION_FILE=$(PWD)/$(VERSION_FILE) || exit 1); done


prerequisites:
	@echo "   Making prerequisites: $(PREREQUISITES_DESCRIPTION)"
	@for m in $(PREREQUISITES_DIRS); do if ! ( if [ -d $$m ]; then cd $$m && \
	$(MAKE) -s all && cd .. ;                                                 \
	else echo "     (non-existing directory $$m skipped)" ; fi ) ; then       \
	exit 1; fi ; done


rebuild: clean all generate-list-of-all-types


generate-all-plt: all
	@echo "   Generating now PLTs for the full Sim-Diasca based stack:"
	@for m in $(PLT_TARGETS) ; do \
	( cd $$m ; $(MAKE) -s generate-local-plt ) ; done


generate-list-of-all-types:
	@echo "   Listing now all types defined in the full Sim-Diasca based stack:"
	@for m in $(PLT_TARGETS) ; do \
	( cd $$m ; $(MAKE) -s generate-list-of-local-types ) ; done


# Creates all relevant links in the source tree to the central, single, host
# candidate configuration file:
#
link-host-candidates:
	@echo "   Creating symbolic links to $(SIM_DIASCA_HOST_FILE) \
	in the full source tree"
	@for f in $(SIM_DIASCA_TOP) $(MOCK_SIMULATORS_TOP) ; do \
	( cd $$f && $(MAKE) -s make-config-links-recurse ) ; done



# The WOOPER Platypus test is hidden, as its test of deletion time-out would
# last, in production mode, for 30 minutes:
#
test-production:
	@echo "   Rebuilding and testing all in the 'production' execution target"
	@$(MAKE) -s clean
	-@/bin/mv -f wooper/priv/examples/class_Platypus_test.erl wooper/priv/examples/class_Platypus_test.erl-hidden
	@$(MAKE) -s all EXECUTION_TARGET=production
	@$(MAKE) -s test
	@/bin/mv -f wooper/priv/examples/class_Platypus_test.erl-hidden wooper/priv/examples/class_Platypus_test.erl


check-hook: check-hook-local


check-hook-local:
	@echo "   One should run 'make full-check' from this root instead." 1>&2
	@exit 5


# 'check' would not suffice as we want to rebuild also prerequisites:
full-check: rebuild
	@echo "   Performing now a full check (tests + static analyses) of \
	the Sim-Diasca based stack:"
	@for m in $(PLT_TARGETS); do \
	( cd $$m ; $(MAKE) -s test generate-local-plt self-check-against-plt ) ; \
	done


plt-check: all
	@echo "   Performing now a static analysis for each layer of \
	the Sim-Diasca based stack (with no forced rebuild):"
	@for m in $(PLT_TARGETS); do \
	( cd $$m ; $(MAKE) -s generate-local-plt self-check-against-plt ) ; \
	done


doc:
	@for m in $(SIM_DIASCA_TOP) $(MOCK_DIRS); do \
	( cd $$m ; $(MAKE) -s doc ) ; done



# Release section.


# Not release-xz, as not popular enough among potential users:
release: release-zip release-bz2 release-doc
	@$(MAKE) -s remove-release-tree # Final cleanup


release-zip: prepare-file-release
	@echo "     Creating Sim-Diasca release archive \
	$(SIM_DIASCA_RELEASE_ARCHIVE_ZIP)"
	@cd .. && zip -r $(SIM_DIASCA_RELEASE_ARCHIVE_ZIP) \
	$(SIM_DIASCA_RELEASE_BASENAME) \
	&& echo "     Archive $(SIM_DIASCA_RELEASE_ARCHIVE_ZIP) ready in $$(pwd)"


# No more 'h' option to avoid duplicating the content of symlinks:
release-xz: prepare-file-release
	@echo "     Creating Sim-Diasca release archive \
	$(SIM_DIASCA_RELEASE_ARCHIVE_XZ)"
	@cd .. && tar cvJf $(SIM_DIASCA_RELEASE_ARCHIVE_XZ) \
	$(SIM_DIASCA_RELEASE_BASENAME) \
	&& echo "     Archive $(SIM_DIASCA_RELEASE_ARCHIVE_XZ) ready in $$(pwd)"


# No more 'h' option to avoid duplicating the content of symlinks:
release-bz2: prepare-file-release
	@echo "     Creating Sim-Diasca release archive \
	$(SIM_DIASCA_RELEASE_ARCHIVE_BZ2)"
	@cd .. && tar cvjf $(SIM_DIASCA_RELEASE_ARCHIVE_BZ2) \
	$(SIM_DIASCA_RELEASE_BASENAME) \
	&& echo "     Archive $(SIM_DIASCA_RELEASE_ARCHIVE_BZ2) ready in $$(pwd)"



# SIM_DIASCA_PUBLIC_CLONE_ROOT to be set in one's environment.
#
# Not using 'git checkout --orphan' as we want to keep the ancestry link with
# the master branch.
#
# Note that if multiple attempts of releases are made, the corresponding branch
# might already exist.
#
# For such a clone, we prefer to start from scratch.
#
release-in-clone:
	@if [ -z "$(SIM_DIASCA_PUBLIC_CLONE_ROOT)" ]; then echo "Error, the SIM_DIASCA_PUBLIC_CLONE_ROOT environment variable is not set." 1>&2 ; exit 55 ; fi
	@if [ ! -d "$(SIM_DIASCA_PUBLIC_CLONE_ROOT)" ]; then echo "Error, the SIM_DIASCA_PUBLIC_CLONE_ROOT=$(SIM_DIASCA_PUBLIC_CLONE_ROOT) directory does not exist." 1>&2 ; exit 56 ; fi
	@if [ ! -d "$(SIM_DIASCA_PUBLIC_CLONE_ROOT)/.git" ]; then echo "Error, the SIM_DIASCA_PUBLIC_CLONE_ROOT=$(SIM_DIASCA_PUBLIC_CLONE_ROOT) directory is not a GIT clone." 1>&2 ; exit 57 ; fi
	@echo "     Releasing Sim-Diasca public version in the 'sim-diasca-$(SIM_DIASCA_BASE_VERSION)' branch in clone '$(SIM_DIASCA_PUBLIC_CLONE_ROOT)'"
	@cd $(SIM_DIASCA_PUBLIC_CLONE_ROOT) && ( (git checkout -b sim-diasca-$(SIM_DIASCA_BASE_VERSION) 2>/dev/null || true) ; (git rm -rf * 2>/dev/null || true) ; /bin/rm -rf $(SIM_DIASCA_PUBLIC_CLONE_ROOT)/*)
	@cd $(SIM_DIASCA_PUBLIC_CLONE_ROOT) && (git reset HEAD README.md LICENSE; git checkout README.md LICENSE; sed -i "s|This branch corresponds.*$$|This branch corresponds to the version **$(SIM_DIASCA_VERSION)** of Sim-Diasca.|1" README.md)
	@/bin/rm -f $(SIM_DIASCA_PUBLIC_CLONE_ROOT)/sim-diasca/doc/installation-guide/internal-version/*.rst
	@$(MAKE) -s prepare-release SIM_DIASCA_RELEASE_BASE=$(SIM_DIASCA_PUBLIC_CLONE_ROOT) && cd $(SIM_DIASCA_PUBLIC_CLONE_ROOT) && git add .
	@echo "After a check, you can run, from $(SIM_DIASCA_PUBLIC_CLONE_ROOT): 'git commit -m \"Sharing of the public version $(SIM_DIASCA_VERSION) of Sim-Diasca.\"', then after some testing: 'git tag -a sim-diasca-version-$(SIM_DIASCA_VERSION) -m \"Release of the public version $(SIM_DIASCA_VERSION) of Sim-Diasca.\"'; finally you might consider: 'git checkout master && git rebase sim-diasca-version-$(SIM_DIASCA_VERSION) && git push --all'."


release-doc:
	@cd $(SIM_DIASCA_TOP)/doc && $(MAKE) -s doc-package



# Also possible:
#   @for p in $(PREREQUISITES_DIRS) ; do ( cd $$p/doc && $(MAKE) -s full-doc VIEW_PDF=no ) ; done


HOST_SAMPLE_PATH := sim-diasca/conf
HOST_SAMPLE_FILE := $(HOST_SAMPLE_PATH)/sim-diasca-host-candidates-sample.txt


# Pre-cleanup and base preparation:
prepare-file-release: remove-release-tree clean-file-release prepare-release


clean-file-release:
	@echo "  Cleaning file release in '$(SIM_DIASCA_RELEASE_BASE)'"
	-@cd .. && /bin/rm -f $(SIM_DIASCA_RELEASE_ARCHIVE_ZIP) \
	$(SIM_DIASCA_RELEASE_ARCHIVE_BZ2) $(SIM_DIASCA_RELEASE_ARCHIVE_XZ)
	-@find $(SIM_DIASCA_RELEASE_BASE) -type d -a \( -name '.git' -o -name 'tmp-rst' \) -exec /bin/rm -rf '{}' ';' 2>/dev/null || true
	@/bin/rm -rf $(SIM_DIASCA_RELEASE_BASE)/sim-diasca/doc/installation-guide/internal-version


# The '-L' option with cp is used so that symbolic links are replaced by their
# actual target file, otherwise tar would include dead links in releases.
#
# sim-diasca.png is added back as it is needed by for example the web manager.
#
prepare-release: #real-clean
	@echo "     Preparing release for Sim-Diasca $(SIM_DIASCA_VERSION) in $(SIM_DIASCA_RELEASE_BASE)"
	@mkdir -p $(SIM_DIASCA_RELEASE_BASE) && \
	/bin/cp -L -r $(SIM_DIASCA_PACKAGE_ELEMENTS) $(SIM_DIASCA_RELEASE_BASE)
	@/bin/rm -rf $(SIM_DIASCA_RELEASE_BASE)/sim-diasca/src/core/src/dataflow/bindings/python/src/sim-diasca-dataflow-env
	-@cd $(SIM_DIASCA_RELEASE_BASE) && /bin/rm -rf */_checkouts && /bin/rm -rf */_build
	-@cd $(SIM_DIASCA_RELEASE_BASE) && find . -type f -a \
	\( -name '*.beam' -o -name 'sim-diasca-host-candidates*.txt'           \
	-o -name '*.png' -o -name 'erl_crash.dump' -o -name '*.dia~' -o -name '*.log' -o -name '*.aux' -o -name '*.toc' -o -name '*.tex' -o -name rebar.lock \) -exec /bin/rm -f '{}' ';' 2>/dev/null || true
	@for f in $(HOST_SAMPLE_FILE) sim-diasca/doc/common-elements/edf-related/sim-diasca.png; do /bin/cp -f $$f $(SIM_DIASCA_RELEASE_BASE)/$$f ; done
	@cat sim-diasca/README.txt.template | sed -s "s|SIM_DIASCA_VERSION|$(SIM_DIASCA_VERSION)|g" > $(SIM_DIASCA_RELEASE_BASE)/README.txt
	@echo "Release generated in $$(realpath $(SIM_DIASCA_RELEASE_BASE))."


remove-release-tree:
	@echo "     Removing any past release archive tree"
	-@cd .. && /bin/rm -rf $(SIM_DIASCA_RELEASE_BASENAME)


clean: clean-checkouts clean-prerequisites clean-generated-files clean-all-results


clean-checkouts:
	@echo "   Cleaning all _checkouts"
	@/bin/rm -rf */_checkouts


clean-prerequisites:
	@echo "   Cleaning all prerequisites first"
	@for m in $(PREREQUISITES_DIRS); do if ! ( if [ -d $$m ]; then cd $$m && \
	echo "   Cleaning prerequisite '$$(basename $$m)'" ;                       \
	$(MAKE) -s clean && cd .. ;                                               \
	else echo "     (non-existing directory $$m skipped)" ; fi ) ;            \
	then exit 1; fi ; done


clean-generated-files:
	@echo "   Cleaning all (non-BEAM) generated files"
	-@for top in $(PLT_TARGETS) ; do \
	/bin/rm -f $$top/*.plt $$top/declared-types-in-*.txt ; done
	-@cd sim-diasca/doc ; /bin/rm -f Sim-Diasca-*-doc.tar.bz2 \
	Sim-Diasca-*-doc.zip
	-@/bin/rm -f $(VERSION_FILE)
	-@find $(ROOT_TOP) -type d -a -name 'tmp-rst' -exec /bin/rm -rf '{}' ';' \
	 2>/dev/null || true


clean-all-results:
	@echo "   Cleaning recursively all results from $$(pwd)"
	-@find . -name "*-on-*-*-*-at-*h-*m-*s-by-$$USER-*" \
		-exec /bin/rm -rf '{}' 2>/dev/null ';' || true


real-clean: real-clean-local


real-clean-local:
	@cd $(SIM_DIASCA_TOP) && $(MAKE) -s clean-host-candidate-files
	@cd $(MOCK_SIMULATORS_TOP) && $(MAKE) -s clean-host-candidate-files


# In the current process we do not use this 'install' target.
#
# (target 'all' implied by generic rules)
#
install:
	@echo "   Installing all in $(INSTALLATION_PREFIX), from $$(basename $(PWD))"
	@for m in $(INSTALLED_DIRS); do if ! ( if [ -d $$m ]; then cd $$m && \
	$(MAKE) -s install-package INSTALLATION_PREFIX="$(INSTALLATION_PREFIX)" &&  \
	cd .. ; else echo "     (directory $$m skipped)" ; fi ) ;           \
	then exit 1; fi ; done && \
	echo " The full Sim-Diasca code base has been successfully installed in '$(INSTALLATION_PREFIX)'."



# Sends full content to the server:
to-archive: archive
	@echo "    Transferring full archive $(ARCHIVE_FILE) to $(ARCHIVE_SERVER)"
	@scp $(ARCHIVE_FILE) $(ARCHIVE_LOCATION)


# Includes all the first-level prerequisites, only with the files in VCS:
to-vcs-archive: vcs-archive
	@echo "    Transferring VCS archive $(VCS_ARCHIVE_FILE) to $(ARCHIVE_SERVER)"
	@scp $(VCS_ARCHIVE_FILE) $(ARCHIVE_LOCATION)


# Brutal archive of the current branch, including all untracked files and VCS
# state. Mmanages symbolic links, but not dead ones.
#
# (tar: no more 'v' for verbose, so that any error can be spotted; 'h' was used
# to create a non-empty archive from a root directory that was actually a
# symlink, yet it resulted also in internal symlinks to be replaced with copies
# of files, which is not wanted)
#
archive: clean
	@echo "    Creating an archive of the Sim-Diasca-specific repository \
	(including all files and directories in their current state, \
	and VCS information)"
	@mkdir -p $(ARCHIVE_ROOT); SRC_DIR=$$(basename $(realpath .)); \
	cd $(realpath ..) && tar cJf $(ARCHIVE_FILE) $$SRC_DIR
	@echo && echo "Sim-Diasca full archive stored in $(ARCHIVE_FILE)"



# Notes:
#
# - here the non-staged files are *not* archived (use 'make archive' for that)
#
# - 'git bundle' is already sufficiently compressed (no need for a *.bz2 or a
# *.xz)
#
vcs-archive: clean
	@echo "    Creating a VCS archive (bundle) of the full Sim-Diasca repository"
	@echo "(to retrieve that archive content, use 'git clone $(VCS_ARCHIVE_FILE)' and then 'git remote set-url origin <authoritative URL>'); note that uncommitted changes and files in the working tree will NOT be included in that archive (use 'make archive' for that)"
	@git bundle create $(VCS_ARCHIVE_FILE) --all \
	&& echo "Full Sim-Diasca VCS archive stored in $(VCS_ARCHIVE_FILE)"


# Archive the bleeding edge of sources with no VCS state (hence just the tip of
# the current branch, for a smaller size):
#
light-archive: clean
	@echo "    Creating a light archive of the Sim-Diasca-specific repository \
	(including all files and directories in their current state, \
	but no VCS information)"
	@mkdir -p $(ARCHIVE_ROOT); SRC_DIR=$$(basename $(realpath .)) ; \
	cd $(realpath ..) && tar cJ --exclude-vcs -f $(ARCHIVE_FILE) $$SRC_DIR
	@echo && echo "Sim-Diasca light archive stored in $(ARCHIVE_FILE)"



# To update the mirror of a third-party project with this current
# branch of the Sim-Diasca sources:

THIRD_PARTY_ROOT="$$HOME/Projects/RELEASE"

THIRD_PARTY_SYNC_TARGET="$(THIRD_PARTY_ROOT)/github-repository/RELEASE/Research/Benchmarks/sim-diasca-benchmark-target/sim-diasca-latest"

update-third-party-mirror:
	@sim-diasca/conf/update-third-party-repository-from-tree.sh . $(THIRD_PARTY_SYNC_TARGET)



stats:
	@$(MAKE_CODE_STATS) $(PWD)


info: info-files info-paths info-archive info-release


info-files: info-local-files


info-local-files:
	@echo "VERSION_FILE = $(VERSION_FILE)"


info-paths:
	@echo "ROOT_TOP               = $(ROOT_TOP)"
	@echo "MYRIAD_TOP             = $(MYRIAD_TOP)"
	@echo "WOOPER_TOP             = $(WOOPER_TOP)"
	@echo "TRACES_TOP             = $(TRACES_TOP)"
	@echo "SIM_DIASCA_TOP         = $(SIM_DIASCA_TOP)"
	@echo "MOCK_SIMULATORS_TOP    = $(MOCK_SIMULATORS_TOP)"
	@echo "SUSTAINABLE_CITIES_TOP = $(SUSTAINABLE_CITIES_TOP)"
	@echo "DIST_SYS_SIM_TOP       = $(DIST_SYS_SIM_TOP)"
	@echo "ACME_TOP               = $(ACME_TOP)"
	@echo "PLANNING_SIM_TOP       = $(PLANNING_SIM_TOP)"
	@echo "PLT_TARGETS            = $(PLT_TARGETS)"


info-build:
	@echo "MODULES_DIRS = $(MODULES_DIRS)"
	@echo "OPTIONAL_MODULES_DIRS = $(OPTIONAL_MODULES_DIRS)"
	@echo "HAS_SUSTAINABLE_CITIES = $(HAS_SUSTAINABLE_CITIES)"
	@echo "HAS_DIST_SYS_SIM = $(HAS_DIST_SYS_SIM)"
	@echo "HAS_ACME = $(HAS_ACME)"
	@echo "HAS_PLANNING_SIM = $(HAS_PLANNING_SIM)"
	@echo "USE_HDF5 = $(USE_HDF5)"
	@echo "USE_REST = $(USE_REST)"
	@echo "USE_PYTHON_BINDING = $(USE_PYTHON_BINDING)"
	@echo "USE_JAVA_BINDING = $(USE_JAVA_BINDING)"


info-archive:
	@echo "ARCHIVE_ROOT        = $(ARCHIVE_ROOT)"
	@echo "ARCHIVE_SERVER      = $(ARCHIVE_SERVER)"
	@echo "ARCHIVE_LOCATION    = $(ARCHIVE_LOCATION)"
	@echo "ARCHIVE_FILE        = $(ARCHIVE_FILE)"
	@echo "ARCHIVE_BRANCH_FILE = $(ARCHIVE_BRANCH_FILE)"


info-version:
	@echo "This is Sim-Diasca version $(SIM_DIASCA_VERSION)."


info-release:
	@echo "SIM_DIASCA_RELEASE_BASENAME    = $(SIM_DIASCA_RELEASE_BASENAME)"
	@echo "SIM_DIASCA_RELEASE_BASE        = $(SIM_DIASCA_RELEASE_BASE)"
	@echo "SIM_DIASCA_PACKAGE_ELEMENTS    = $(SIM_DIASCA_PACKAGE_ELEMENTS)"
	@echo "SIM_DIASCA_RELEASE_ARCHIVE_ZIP = $(SIM_DIASCA_RELEASE_ARCHIVE_ZIP)"
	@echo "SIM_DIASCA_RELEASE_ARCHIVE_BZ2 = $(SIM_DIASCA_RELEASE_ARCHIVE_BZ2)"
	@echo "SIM_DIASCA_RELEASE_ARCHIVE_XZ  = $(SIM_DIASCA_RELEASE_ARCHIVE_XZ)"
