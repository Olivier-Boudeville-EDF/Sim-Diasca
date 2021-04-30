WOOPER_TOP = ..


.PHONY: info-rebar clean-local clean-rebar


# Typically to generate various flavours (normal, for testing, etc.) of
# rebar.config files.
#
# Note that:
# - the Hex version, rebar-for-hex.config.template, is now useless
# - this rule is not centralised in a GNUmakerules-automatic file for all
# layers, as substitutions are layer-specific
# - 'rebar.config' is created in this 'conf' directory; any 'rebar.config' at
# the root of this package has not been affected
#
%: %.template $(WOOPER_TOP)/GNUmakevars.inc
	@echo "  Generating $@ from $< (VERSION_FOR_REBAR being $(VERSION_FOR_REBAR), MYRIAD_VERSION_FOR_REBAR being $(MYRIAD_VERSION)); this file should be copied to the root of this layer in order to be taken into account"
	@cat $< | sed "s|\"MYRIAD_VERSION_FOR_REBAR\"|\"$(MYRIAD_VERSION)\"|g" | sed "s|\"VERSION_FOR_REBAR\"|\"$(VERSION_FOR_REBAR)\"|g" | sed "s|FIRST_FILES_FOR_REBAR|$(MYRIAD_REBAR_FIRST_FILES)|g" | sed "s|DEBUG_KEY_FOR_REBAR|$(DEBUG_INFO_KEY)|g"| sed "s|INCS_FOR_REBAR|$(MYRIAD_REBAR_INCS)|g" > $@


info-rebar:
	@echo "MYRIAD_REBAR_FIRST_FILES = $(MYRIAD_REBAR_FIRST_FILES)"
	@echo "MYRIAD_REBAR_INCS = $(MYRIAD_REBAR_INCS)"


clean: clean-local


clean-local: clean-rebar
	-@/bin/rm -f $(MYRIAD_REBAR_CONF_FILES)


# Removed: -@/bin/rm -rf priv/
clean-rebar:
	-@/bin/rm -f rebar.config


include $(WOOPER_TOP)/GNUmakesettings.inc
