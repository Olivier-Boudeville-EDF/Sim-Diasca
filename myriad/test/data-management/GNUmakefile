MYRIAD_TOP = ../..

.PHONY: all test-hdf5 clean clean-local


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



clean: clean-local

clean-local:
	-@/bin/rm -f raw_hdf5_test-fixture-*.hdf5 hdf5_support_test.hdf5 \
				 sql_support_test.sqlite3



include $(MYRIAD_TOP)/GNUmakesettings.inc
