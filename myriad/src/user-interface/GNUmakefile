MYRIAD_TOP = ../..

.PHONY:prepare-gui-generation

MODULES_DIRS = textual graphical audio


all: prepare-gui-generation


# Introduced to be able to force the generation of modules (typically
# *_constants ones) before other (e.g. *_generated ones and, above all, the
# modules that will generate the latter modules for the former ones, see gui for
# an example thereof):
#
prepare-gui-generation:
	@echo "  Preparing for gui generated modules"
	@cd graphical && $(MAKE) -s gui_constants.beam gui_opengl_constants.beam


include $(MYRIAD_TOP)/GNUmakesettings.inc
