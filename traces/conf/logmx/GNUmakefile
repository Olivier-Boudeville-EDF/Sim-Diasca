

.PHONY: all clean clean-java info


JAVAC := $(shell which javac)

JAVAC_OPT := -classpath $$HOME/Software/LogMX/LogMX-current-install/jar/logmx.jar

TARGET_CLASSES := CeylanTraceParser.class


all: $(TARGET_CLASSES)


clean: clean-java


clean-java:
	@echo "  Cleaning java files"
	-@/bin/rm -f $(TARGET_CLASSES)


%.class: %.java
	@echo "  Compiling $<"
	@$(JAVAC) $(JAVAC_OPT) $<


info:
	@echo "JAVAC = $(JAVAC)"
	@echo "JAVAC_OPT = $(JAVAC_OPT)"
	@echo "TARGET_CLASSES = $(TARGET_CLASSES)"
