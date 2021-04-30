SIM_DIASCA_TOP = ../../../../../../..

.PHONY: all java-support java-test java-actual-test info-bindings

MODULES_DIRS = common sim_diasca


JAVA_BINDING_ELEMENTS := SimDiascaJavaRuntimeContainer.class

# Build the Java runtime support iff this binding is enabled:
ifeq ($(USE_JAVA_BINDING),true)

	JAVA_TARGET := $(JAVA_BINDING_ELEMENTS)

else

	JAVA_TARGET :=

endif


all: $(JAVA_TARGET)


# Explicit target that enables the binding if needed:
java-support:
	@$(MAKE) -s $(JAVA_BINDING_ELEMENTS) USE_JAVA_BINDING=true
	@for m in $(MODULES_DIRS); do \
	( cd $$m && $(MAKE) -s all USE_JAVA_BINDING=true ) ; done


# If wanting, just for the sake of testing, to launch the binding JVM manually
# (knowing the engine is to do it by itself):
#
java-test: java-support
	@$(MAKE) -s java-actual-test USE_JAVA_BINDING=true


# Will expect at least a VM running:
# (first step: 'erl -sname test_node_name -setcookie test_cookie')
#
java-actual-test:
	@$(JAVA) $(JAVA_OPT) SimDiascaJavaRuntimeContainer --cookie test_cookie --peer-node-name test_node_name --epmd-port 4369


info-bindings:
	@echo "USE_PYTHON_BINDING = $(USE_PYTHON_BINDING)"
	@echo "USE_JAVA_BINDING = $(USE_JAVA_BINDING)"
	@echo "JAVA_TARGET = $(JAVA_TARGET)"


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
