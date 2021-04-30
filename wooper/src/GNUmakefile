WOOPER_TOP = ..

.PHONY: all real-clean

# As shall be the default rule:
all:

real-clean:
	-@/bin/rm -f wooper.app.src


include $(WOOPER_TOP)/GNUmakesettings.inc


# Helper modules (ex: from Myriad like basic_utils.beam, or directly from WOOPER
# like wooper_class_manager.beam, etc.) are not listed here as dependencies,
# their possible lacking will be detected at runtime.


wooper_class_manager_test: \
	wooper_class_manager_test.beam \
	wooper_class_manager.beam


class_WOOPER_attribute_test: \
	class_WOOPER_attribute_test.beam \
	wooper_class_manager.beam
