SIM_DIASCA_TOP = ..

# Listing make targets for the known scopes:

DOC_ELEMENTS_INTERNAL := documentation-overview-internal   \
						 installation-guide-internal

DOC_ELEMENTS_PUBLIC := documentation-overview-public       \
					   installation-guide-public

DOC_ELEMENTS_COMMON := technical-manual developer-guide modeller-guide \
					   coupling-howto dataflow-howto


# Listing documentation directories:
DOC_DIRS := documentation-overview installation-guide $(DOC_ELEMENTS_COMMON)


.PHONY: all release-doc doc-release doc-package doc-elements $(DOC_DIRS) \
		populate-doc-dir-common populate-doc-dir-internal                \
		populate-doc-dir-public copy-erlang-doc                          \
		generate-doc-common generate-doc-internal generate-doc-public    \
		create-lower-layer-referentials create-myriad-referential        \
		create-wooper-referential create-traces-referential              \
		export-internal export-public                                    \
		clean-public-doc-branch clean-doc-dir-public clean clean-doc     \
		clean-doc-local                                                  \
		info-doc info-common-doc info-internal-doc info-public-doc


MODULES_DIRS = $(DOC_DIRS) training

DOC_PACKAGE_PREFIX_INTERNAL = Sim-Diasca-$(SIM_DIASCA_VERSION)-internal-doc
DOC_PACKAGE_PREFIX_PUBLIC = Sim-Diasca-$(SIM_DIASCA_VERSION)-public-doc


DOC_PACKAGE_INTERNAL_ARCHIVE_ZIP = $(DOC_PACKAGE_PREFIX_INTERNAL).zip
DOC_PACKAGE_INTERNAL_ARCHIVE_BZ2 = $(DOC_PACKAGE_PREFIX_INTERNAL).tar.bz2
DOC_PACKAGE_INTERNAL_ARCHIVE_XZ  = $(DOC_PACKAGE_PREFIX_INTERNAL).tar.xz

DOC_PACKAGE_PUBLIC_ARCHIVE_ZIP = $(DOC_PACKAGE_PREFIX_PUBLIC).zip
DOC_PACKAGE_PUBLIC_ARCHIVE_BZ2 = $(DOC_PACKAGE_PREFIX_PUBLIC).tar.bz2
DOC_PACKAGE_PUBLIC_ARCHIVE_XZ  = $(DOC_PACKAGE_PREFIX_PUBLIC).tar.xz


# Intentional: no default for DOC_TARGET_DIR.

DOC_TARGET_INTERNAL_DIR = $(DOC_PACKAGE_PREFIX_INTERNAL)
DOC_TARGET_PUBLIC_DIR = $(DOC_PACKAGE_PREFIX_PUBLIC)



#COMMON_ELEMENTS = $(COMMON_PDF_ELEMENTS)


#COMMON_PDF_ELEMENTS = \
#   $(SD_TECH_MANUAL_DIR)/tmp-rst/$(SD_TECH_MANUAL_PDF)       \
#   $(SD_DEV_GUIDE_DIR)/tmp-rst/$(SD_DEV_GUIDE_PDF)           \
#   $(SD_MOD_GUIDE_DIR)/tmp-rst/$(SD_MOD_GUIDE_PDF)           \
#   $(SD_COUPLING_HOWTO_DIR)/tmp-rst/$(SD_COUPLING_HOWTO_PDF) \
#   $(SD_DATAFLOW_HOWTO_DIR)/tmp-rst/$(SD_DATAFLOW_HOWTO_PDF)


#INTERNAL_PDF_ELEMENTS = \
#   $(SD_INST_GUIDE_DIR)/tmp-rst/$(SD_INST_INTERNAL_GUIDE_PDF)

#PUBLIC_PDF_ELEMENTS = \
#   $(SD_INST_GUIDE_DIR)/tmp-rst/$(SD_INST_PUBLIC_GUIDE_PDF)


#WEBSITE_EXTENSIONS := {html,css,png,jpeg}
#WEBSITE_CONTENT := tmp-rst/*.$(WEBSITE_EXTENSIONS)

WEBSITE_EXTENSIONS := pdf html css png jpeg


COMMON_SUBDIRS = $(SD_TECH_MANUAL_DIR) $(SD_DEV_GUIDE_DIR) $(SD_MOD_GUIDE_DIR) $(SD_COUPLING_HOWTO_DIR) $(SD_DATAFLOW_HOWTO_DIR)

INTERNAL_SUBDIRS = $(SD_INST_GUIDE_INTERNAL_DIR) $(COMMON_SUBDIRS)

PUBLIC_SUBDIRS = $(SD_INST_GUIDE_PUBLIC_DIR) $(COMMON_SUBDIRS)


# For the internal version:
ERLANG_DOC_DIR = $$HOME/Software/Erlang/Erlang-current-documentation/doc


# Default do-nothing target:
all:
	@echo "  Run 'make doc-package' if wanting to generate a public documentation \
	package (or 'make full-doc' to generate only the technical manual)."


doc:


doc-package: create-public-package

# Alternate named targets:
release-doc: doc-package


# XZ target not activated, not usual enough:

create-internal-package: create-internal-referential
	@tar cvjf $(DOC_PACKAGE_INTERNAL_ARCHIVE_BZ2) $(DOC_TARGET_INTERNAL_DIR)
	@zip -r $(DOC_PACKAGE_INTERNAL_ARCHIVE_ZIP) $(DOC_TARGET_INTERNAL_DIR)
	@echo "   $(DOC_PACKAGE_INTERNAL_ARCHIVE_BZ2) and $(DOC_PACKAGE_INTERNAL_ARCHIVE_ZIP) generated)"
	-@/bin/rm -rf $(DOC_TARGET_INTERNAL_DIR)


create-public-package: create-public-referential
	@tar cvjf $(DOC_PACKAGE_PUBLIC_ARCHIVE_BZ2) $(DOC_TARGET_PUBLIC_DIR)
	@zip -r $(DOC_PACKAGE_PUBLIC_ARCHIVE_ZIP) $(DOC_TARGET_PUBLIC_DIR)
	@echo "   $(DOC_PACKAGE_PUBLIC_ARCHIVE_BZ2) and $(DOC_PACKAGE_PUBLIC_ARCHIVE_ZIP) generated"
	-@/bin/rm -rf $(DOC_TARGET_PUBLIC_DIR)



create-internal-referential: clean-doc-dir-internal populate-doc-dir-internal
	@$(MAKE) -s populate-doc-dir-common DOC_TARGET_DIR=$(DOC_TARGET_INTERNAL_DIR) && echo "Internal documentation referential created in $(DOC_TARGET_INTERNAL_DIR)"


create-public-referential: clean-doc-dir-public populate-doc-dir-public
	@$(MAKE) -s populate-doc-dir-common DOC_TARGET_DIR=$(DOC_TARGET_PUBLIC_DIR) && echo "Public documentation referential created in $(DOC_TARGET_PUBLIC_DIR)"



doc-elements-internal: $(DOC_ELEMENTS_INTERNAL)

doc-elements-public: $(DOC_ELEMENTS_PUBLIC)



documentation-overview-internal:
	@cd $(SD_DOC_OVERVIEW_INTERNAL_DIR) && $(MAKE) -s doc


documentation-overview-public:
	@cd $(SD_DOC_OVERVIEW_PUBLIC_DIR) && $(MAKE) -s doc



technical-manual:
	@cd $(SD_TECH_MANUAL_DIR) && $(MAKE) -s full-doc



installation-guide-internal:
	@cd $(SD_INST_GUIDE_INTERNAL_DIR) && $(MAKE) -s local-doc


installation-guide-public:
	@cd $(SD_INST_GUIDE_PUBLIC_DIR) && $(MAKE) -s local-doc



developer-guide:
	@cd $(SD_DEV_GUIDE_DIR) && $(MAKE) -s full-doc


modeller-guide:
	@cd $(SD_MOD_GUIDE_DIR) && $(MAKE) -s full-doc


coupling-howto:
	@cd $(SD_COUPLING_HOWTO_DIR) && $(MAKE) -s full-doc


dataflow-howto:
	@cd $(SD_DATAFLOW_HOWTO_DIR) && $(MAKE) -s full-doc


# For /bin/cp, the -L option allows to transform symlinks in their targets
# (otherwise CSS files will be dead links as they are relative to an out-of-tree
# target)
#
populate-doc-dir-common: generate-doc-common create-lower-layer-referentials
	@if [ -z "$(DOC_TARGET_DIR)" ]; then echo "Error, 'DOC_TARGET_DIR' not set." 1>&2; exit 5; fi
	@echo "  Populating $(DOC_TARGET_DIR) with common elements"
	@for d in $(DOC_DIRS); do /bin/mkdir -p $(DOC_TARGET_DIR)/$$d; done
	@for ext in $(WEBSITE_EXTENSIONS); do (/bin/cp -rL $(SD_TECH_MANUAL_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_DIR)/$(SD_TECH_MANUAL_DIR) 2>/dev/null || true); done
	@for ext in $(WEBSITE_EXTENSIONS); do (/bin/cp -rL $(SD_DEV_GUIDE_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_DIR)/$(SD_DEV_GUIDE_DIR) 2>/dev/null || true); done
	@for ext in $(WEBSITE_EXTENSIONS); do (/bin/cp -rL $(SD_MOD_GUIDE_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_DIR)/$(SD_MOD_GUIDE_DIR) 2>/dev/null || true); done
	@for ext in $(WEBSITE_EXTENSIONS); do (/bin/cp -rL $(SD_COUPLING_HOWTO_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_DIR)/$(SD_COUPLING_HOWTO_DIR) 2>/dev/null || true); done
	@for ext in $(WEBSITE_EXTENSIONS); do (/bin/cp -rL $(SD_DATAFLOW_HOWTO_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_DIR)/$(SD_DATAFLOW_HOWTO_DIR) 2>/dev/null || true); done


populate-doc-dir-internal: generate-doc-internal
	@echo "  Populating $(DOC_TARGET_INTERNAL_DIR) with internal elements"
	@/bin/mkdir -p $(DOC_TARGET_INTERNAL_DIR)/$(SD_INST_GUIDE_BASE_DIR)
	@for ext in $(WEBSITE_EXTENSIONS); do (/bin/cp -rL $(SD_INST_GUIDE_INTERNAL_DIR)/*.$$ext $(DOC_TARGET_INTERNAL_DIR)/$(SD_INST_GUIDE_BASE_DIR) 2>/dev/null || true); done
	@for ext in $(WEBSITE_EXTENSIONS); do (/bin/cp -rL $(SD_DOC_OVERVIEW_INTERNAL_DIR)/*.$$ext $(DOC_TARGET_INTERNAL_DIR) 2>/dev/null || true); done
	@$(MAKE) -s copy-erlang-doc DOC_TARGET_DIR=$(DOC_TARGET_INTERNAL_DIR)
	@cd $(DOC_TARGET_INTERNAL_DIR) && ln -sf $(SD_DOC_OVERVIEW_INTERNAL_BASE).html index.html


populate-doc-dir-public: generate-doc-public
	@echo "  Populating $(DOC_TARGET_PUBLIC_DIR) with public elements"
	@/bin/mkdir -p $(DOC_TARGET_PUBLIC_DIR)/$(SD_INST_GUIDE_BASE_DIR)
	@for ext in $(WEBSITE_EXTENSIONS); do (/bin/cp -rL $(SD_INST_GUIDE_PUBLIC_DIR)/*.$$ext $(DOC_TARGET_PUBLIC_DIR)/$(SD_INST_GUIDE_BASE_DIR) 2>/dev/null || true); done
	@for ext in $(WEBSITE_EXTENSIONS); do (/bin/cp -rL $(SD_DOC_OVERVIEW_PUBLIC_DIR)/*.$$ext $(DOC_TARGET_PUBLIC_DIR) 2>/dev/null || true); done
	@cd $(DOC_TARGET_PUBLIC_DIR) && ln -sf $(SD_DOC_OVERVIEW_PUBLIC_BASE).html index.html


copy-erlang-doc:
	@if [ -z "$(DOC_TARGET_DIR)" ]; then echo "  Error, 'DOC_TARGET_DIR' not set." 1>&2; exit 15; fi
	@if [ -d "$(ERLANG_DOC_DIR)" ]; then echo "  Creating a mirror of the current local Erlang documentation in $(DOC_TARGET_DIR)" ; mkdir -p $(DOC_TARGET_DIR)/erlang && /bin/cp -rf "$(ERLANG_DOC_DIR)"/* $(DOC_TARGET_DIR)/erlang/; else echo " (no local Erlang documentation found, no mirror created) "; fi



# Actually the PDF generation triggers the HTML one as well:
generate-doc-common: $(DOC_ELEMENTS_COMMON)

generate-doc-internal: $(DOC_ELEMENTS_INTERNAL)
	@$(MAKE) -s DOC_TARGET_DIR=$(DOC_TARGET_INTERNAL_DIR)

generate-doc-public: $(DOC_ELEMENTS_PUBLIC)
	@$(MAKE) -s DOC_TARGET_DIR=$(DOC_TARGET_PUBLIC_DIR)


create-lower-layer-referentials:
	@echo "  Creating documentation for lower-layers in $(DOC_TARGET_DIR)"
	@$(MAKE) -s create-myriad-referential create-wooper-referential create-traces-referential


create-myriad-referential:
	@if [ -z "$(DOC_TARGET_DIR)" ]; then echo "  Error, 'DOC_TARGET_DIR' not set." 1>&2; exit 5; fi
	@echo "  Creating myriad documentation in $(DOC_TARGET_DIR) tree"
	@myriad_target_dir=$$(realpath $(DOC_TARGET_DIR))/myriad; mkdir -p $$myriad_target_dir && cd $(MYRIAD_TOP)/doc && $(MAKE) -s create-local-referential DOC_TARGET_DIR="$$myriad_target_dir"


create-wooper-referential:
	@if [ -z "$(DOC_TARGET_DIR)" ]; then echo "  Error, 'DOC_TARGET_DIR' not set." 1>&2; exit 6; fi
	@echo "  Creating wooper documentation in $(DOC_TARGET_DIR) tree"
	@wooper_target_dir=$$(realpath $(DOC_TARGET_DIR))/wooper; mkdir -p $$wooper_target_dir && cd $(WOOPER_TOP)/doc && $(MAKE) -s create-local-referential DOC_TARGET_DIR="$$wooper_target_dir"


create-traces-referential:
	@if [ -z "$(DOC_TARGET_DIR)" ]; then echo "  Error, 'DOC_TARGET_DIR' not set." 1>&2; exit 7; fi
	@echo "  Creating traces documentation in $(DOC_TARGET_DIR) tree"
	@traces_target_dir=$$(realpath $(DOC_TARGET_DIR))/traces; mkdir -p $$traces_target_dir && cd $(TRACES_TOP)/doc && $(MAKE) -s create-local-referential DOC_TARGET_DIR="$$traces_target_dir"


export-internal:

export-public: clean-public-doc-branch create-public-referential
	@echo "  Exporting the public Sim-Diasca documentation to official website in $(SIM_DIASCA_WEB_MIRROR_BRANCH)"
	@/bin/cp -rfH $(DOC_TARGET_PUBLIC_DIR)/* $(SIM_DIASCA_PUBLIC_WEB_BRANCH) && cd $(SIM_DIASCA_PUBLIC_WEB_BRANCH) && git add . && git merge -s ours && git commit -m "Sim-Diasca public doc updated." && git push && git pull --ff-only
	@echo "Public version exported in $(SIM_DIASCA_PUBLIC_WEB_BRANCH)."


clean-public-doc-branch:
	@echo "  Cleaning public doc branch $(SIM_DIASCA_PUBLIC_WEB_BRANCH)"
	@if [ ! -d "$(SIM_DIASCA_PUBLIC_WEB_BRANCH)" ]; then echo " Error, no target public branch (SIM_DIASCA_PUBLIC_WEB_BRANCH) defined."; exit 20; else /bin/rm -rf $(SIM_DIASCA_PUBLIC_WEB_BRANCH)/*; fi



# Main page at the root:
clean-doc-dir-internal:
	-@for d in $(INTERNAL_SUBDIRS); do /bin/rm -rf $(DOC_TARGET_INTERNAL_DIR)/$$d; done
	-@for ext in $(WEBSITE_EXTENSIONS); do /bin/rm -f $(DOC_TARGET_INTERNAL_DIR)/*.$$ext; done


clean-doc-dir-public:
	-@for d in $(PUBLIC_SUBDIRS); do /bin/rm -rf $(DOC_TARGET_PUBLIC_DIR)/$$d; done
	-@for ext in $(WEBSITE_EXTENSIONS); do /bin/rm -f $(DOC_TARGET_PUBLIC_DIR)/*.$$ext; done


clean: clean-doc


clean-doc: clean-doc-local


# Not using DOC_PACKAGE_* as any version must be removed:
clean-doc-local:
	-@/bin/rm -f Sim-Diasca-*-doc.zip Sim-Diasca-*-doc.tar.bz2 Sim-Diasca-*-doc.tar.xz
	-@/bin/rm -rf Sim-Diasca-*-doc


info-doc: info-common-doc info-internal-doc info-public-doc


info-common-doc:
	@echo "DOC_ELEMENTS_COMMON = $(DOC_ELEMENTS_COMMON)"


info-internal-doc:
	@echo "DOC_PACKAGE_PREFIX_INTERNAL      = $(DOC_PACKAGE_PREFIX_INTERNAL)"
	@echo "DOC_PACKAGE_INTERNAL_ARCHIVE_ZIP = $(DOC_PACKAGE_INTERNAL_ARCHIVE_ZIP)"
	@echo "DOC_PACKAGE_INTERNAL_ARCHIVE_BZ2 = $(DOC_PACKAGE_INTERNAL_ARCHIVE_BZ2)"
	@echo "DOC_PACKAGE_INTERNAL_ARCHIVE_XZ  = $(DOC_PACKAGE_INTERNAL_ARCHIVE_XZ)"
	@echo "DOC_ELEMENTS_INTERNAL = $(DOC_ELEMENTS_INTERNAL)"
	@echo "DOC_TARGET_INTERNAL_DIR = $(DOC_TARGET_INTERNAL_DIR)"


info-public-doc:
	@echo "DOC_PACKAGE_PREFIX_PUBLIC      = $(DOC_PACKAGE_PREFIX_PUBLIC)"
	@echo "DOC_PACKAGE_PUBLIC_ARCHIVE_ZIP = $(DOC_PACKAGE_PUBLIC_ARCHIVE_ZIP)"
	@echo "DOC_PACKAGE_PUBLIC_ARCHIVE_BZ2 = $(DOC_PACKAGE_PUBLIC_ARCHIVE_BZ2)"
	@echo "DOC_PACKAGE_PUBLIC_ARCHIVE_XZ  = $(DOC_PACKAGE_PUBLIC_ARCHIVE_XZ)"
	@echo "DOC_ELEMENTS_PUBLIC = $(DOC_ELEMENTS_PUBLIC)"
	@echo "DOC_TARGET_PUBLIC_DIR = $(DOC_TARGET_PUBLIC_DIR)"



# Root specified as we want to catch the doc in src/core as well:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
