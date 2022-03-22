MYRIAD_TOP = ../..

.PHONY: all test-hdf5 info-local clean clean-local


all:


# These tests require prerequisites to be enabled:

test-hdf5:
	@$(MAKE) raw_hdf5_run && h5dump raw_hdf5_test.h5


# Note: by default the generated sqlite3 file is automatically removed by the
# test, hence its removal would need to be commented-out for this target to be
# useful:
#
test-sql:
	@$(MAKE) sql_support_run && sqlitebrowser sql_support_test.sqlite3


# Order matters, as may define 'USE_PROTOBUF = true' (not with ':='):
include $(MYRIAD_TOP)/GNUmakesettings.inc


# Needed so that the generation of the example accessor can done:
ifeq ($(USE_PROTOBUF),true)

protobuf_support_test.beam: myriad_example.hrl

endif


info-local:
	@echo "USE_PROTOBUF = $(USE_PROTOBUF)"


clean: clean-local

clean-local:
	-@/bin/rm -f raw_hdf5_test-fixture-*.hdf5 hdf5_support_test.hdf5 \
				 sql_support_test.sqlite3 test-environment.etf
