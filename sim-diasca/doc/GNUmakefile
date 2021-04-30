SIM_DIASCA_TOP = ..

# Listing make targets for the known scopes:

DOC_ELEMENTS_INTERNAL := installation-guide-internal
DOC_ELEMENTS_PUBLIC := installation-guide-public

DOC_ELEMENTS_COMMON := technical-manual developer-guide modeller-guide \
					   coupling-howto dataflow-howto


# Listing documentation directories:
DOC_DIRS := installation-guide $(DOC_ELEMENTS_COMMON)


.PHONY: all release-doc doc-release doc-package doc-elements $(DOC_DIRS) \
		info-doc info-common-doc info-internal-doc info-public-doc
		clean clean-doc \


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
#	$(SD_TECH_MANUAL_DIR)/tmp-rst/$(SD_TECH_MANUAL_PDF)       \
#	$(SD_DEV_GUIDE_DIR)/tmp-rst/$(SD_DEV_GUIDE_PDF)           \
#	$(SD_MOD_GUIDE_DIR)/tmp-rst/$(SD_MOD_GUIDE_PDF)           \
#	$(SD_COUPLING_HOWTO_DIR)/tmp-rst/$(SD_COUPLING_HOWTO_PDF) \
#	$(SD_DATAFLOW_HOWTO_DIR)/tmp-rst/$(SD_DATAFLOW_HOWTO_PDF)


#INTERNAL_PDF_ELEMENTS = \
#	$(SD_INST_GUIDE_DIR)/tmp-rst/$(SD_INST_INTERNAL_GUIDE_PDF)

#PUBLIC_PDF_ELEMENTS = \
#	$(SD_INST_GUIDE_DIR)/tmp-rst/$(SD_INST_PUBLIC_GUIDE_PDF)

#website_extensions := {html,css,png,jpeg}
#website_content := tmp-rst/*.$(website_extensions)

website_extensions := html css png jpeg



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


technical-manual:
	@cd $(SD_TECH_MANUAL_DIR) && $(MAKE) -s full-doc



installation-guide-internal:
	@cd $(SD_INST_GUIDE_INTERNAL_DIR) && $(MAKE) -s full-doc


installation-guide-public:
	@cd $(SD_INST_GUIDE_PUBLIC_DIR) && $(MAKE) -s full-doc



developer-guide:
	@cd $(SD_DEV_GUIDE_DIR) && $(MAKE) -s full-doc


modeller-guide:
	@cd $(SD_MOD_GUIDE_DIR) && $(MAKE) -s full-doc


coupling-howto:
	@cd $(SD_COUPLING_HOWTO_DIR) && $(MAKE) -s full-doc


dataflow-howto:
	@cd $(SD_DATAFLOW_HOWTO_DIR) && $(MAKE) -s full-doc


populate-doc-dir-common: generate-doc-common
	@if [ -z $(DOC_TARGET_DIR) ]; then echo "Error, 'DOC_TARGET_DIR' not set." 1>&2 ; exit 5 ; fi
	@echo "  Populating $(DOC_TARGET_DIR) with common elements"
	@for d in $(DOC_DIRS); do /bin/mkdir -p $(DOC_TARGET_DIR)/$$d ; done
	@for ext in $(website_extensions); do ( /bin/cp -r $(SD_TECH_MANUAL_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_DIR)/$(SD_TECH_MANUAL_DIR) 2>/dev/null || true ) ; done
	@for ext in $(website_extensions); do ( /bin/cp -r $(SD_DEV_GUIDE_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_DIR)/$(SD_DEV_GUIDE_DIR) 2>/dev/null || true ) ; done
	@for ext in $(website_extensions); do ( /bin/cp -r $(SD_MOD_GUIDE_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_DIR)/$(SD_MOD_GUIDE_DIR) 2>/dev/null || true ) ; done
	@for ext in $(website_extensions); do ( /bin/cp -r $(SD_COUPLING_HOWTO_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_DIR)/$(SD_COUPLING_HOWTO_DIR) 2>/dev/null || true ) ; done
	@for ext in $(website_extensions); do ( /bin/cp -r $(SD_DATAFLOW_HOWTO_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_DIR)/$(SD_DATAFLOW_HOWTO_DIR) 2>/dev/null || true ) ; done


populate-doc-dir-internal: generate-doc-internal
	@echo "  Populating $(DOC_TARGET_INTERNAL_DIR) with internal elements"
	@/bin/mkdir -p $(DOC_TARGET_INTERNAL_DIR)/$(SD_INST_GUIDE_BASE_DIR)
	@for ext in $(website_extensions); do ( /bin/cp -r $(SD_INST_GUIDE_INTERNAL_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_INTERNAL_DIR)/$(SD_INST_GUIDE_BASE_DIR) 2>/dev/null || true ) ; done


populate-doc-dir-public: generate-doc-public
	@echo "  Populating $(DOC_TARGET_PUBLIC_DIR) with public elements"
	@/bin/mkdir -p $(DOC_TARGET_PUBLIC_DIR)/$(SD_INST_GUIDE_BASE_DIR)
	@for ext in $(website_extensions); do ( /bin/cp -r $(SD_INST_GUIDE_PUBLIC_DIR)/tmp-rst/*.$$ext $(DOC_TARGET_PUBLIC_DIR)/$(SD_INST_GUIDE_BASE_DIR) 2>/dev/null || true ) ; done



clean-doc-dir-internal:
	-@/bin/rm -rf $(DOC_TARGET_INTERNAL_DIR)


clean-doc-dir-public:
	-@/bin/rm -rf $(DOC_TARGET_PUBLIC_DIR)


# Actually the PDF generation triggers the HTML one as well:
generate-doc-common: $(DOC_ELEMENTS_COMMON)

generate-doc-internal: $(DOC_ELEMENTS_INTERNAL)

generate-doc-public: $(DOC_ELEMENTS_PUBLIC)



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


clean: clean-doc


# Not using DOC_PACKAGE_* as any version must be removed:
clean-doc:
	-@/bin/rm -f Sim-Diasca-*-doc.zip Sim-Diasca-*-doc.tar.bz2 \
		Sim-Diasca-*-doc.tar.xz
	-@/bin/rm -rf Sim-Diasca-*-doc


# Root specified as we want to catch the doc in src/core as well:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
