MYRIAD_TOP = ../..

.PHONY: clean-local

# So that it remains the default target:
all:

clean: clean-local


clean-local:
	-@/bin/rm -f my-content-file my-content-file.encrypted \
		my-content-file.decrypted my-test-key-file.cipher .cipher-*


include $(MYRIAD_TOP)/GNUmakesettings.inc
