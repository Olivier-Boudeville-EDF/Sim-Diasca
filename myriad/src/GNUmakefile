MYRIAD_TOP = ..

.PHONY: real-clean

# Not based on the alphabetical order, as for example generating the
# gui_constants module in 'gui' requires 'utils' to be already built, hence
# going mostly from base modules to more applicative ones:
#
MODULES_DIRS = utils data-management meta maths user-interface scripts apps

real-clean:
	-@/bin/rm -f $(PROJECT_NAME_FOR_OTP).app.src

include $(MYRIAD_TOP)/GNUmakesettings.inc
