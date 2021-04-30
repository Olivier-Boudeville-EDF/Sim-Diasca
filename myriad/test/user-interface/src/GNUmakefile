MYRIAD_TOP = ../../..

.PHONY: all test-interactive test-non-existing-backend




MODULES_DIRS = textual graphical

# gui not ready yet:
#TESTED_GUI_BACKENDS := text_ui term_ui gui
#TESTED_GUI_BACKENDS := text_ui term_ui
TESTED_GUI_BACKENDS := term_ui


all:

test:


# In addition to parameter-less, batch test:
test-interactive:
	@for b in $(TESTED_GUI_BACKENDS) ; do echo ; echo "With backend '$$b' selected:" ; $(MAKE) -s ui_run CMD_LINE_OPT="-extra --use-ui-backend $$b" ; done


test-non-existing-backend:
	@if ! $(MAKE) ui_run CMD_LINE_OPT="-extra --use-ui-backend non_existing_gui" 2>/dev/null ; then exit 0 ; else exit 5 ; fi


include $(MYRIAD_TOP)/GNUmakesettings.inc
