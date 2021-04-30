MYRIAD_TOP = ../../..


.PHONY: test-local test-initial-scan test-extra-scan test-rescan test-resync \
		test-uniquify test-merge uniquify                                    \
		create-test-trees create-content-tree create-reference-tree          \
		scan-test-tree clean-test-trees                                      \
		post-test kill-epmd process-test-log reset-keyboard-mode


# Previously run as an escript, but due to an issue with term_ui (i.e. dialog),
# now run as a shell script (way better: more convenient for the user, and more
# robust):
#
#MERGE_SCRIPT := ./merge-tree.escript
MERGE_SCRIPT := ./merge.sh


# For new content to possibly merge in reference:
TEST_CONTENT_TREE := "myriad-test-content-tree"

TEST_REFERENCE_TREE := "myriad-test-reference-tree"

TEST_TREES := $(TEST_CONTENT_TREE) $(TEST_REFERENCE_TREE)


TEST_LOG_FILE := "merge-tree.log"

#UI_BACKEND := text_ui
UI_BACKEND := term_ui



all:

test-all: test-local test-uniquify test-merge


test-local: test-initial-scan test-extra-scan test-rescan test-resync


# Meant to create a cache file for the content tree:
test-initial-scan: create-test-trees pre-test scan-test-tree post-test


# Meant to perform a light check of the pre-existing cache file for the content
# tree:
#
test-extra-scan: pre-test scan-test-tree post-test


test-rescan: pre-test rescan-test-tree post-test

test-resync: pre-test resync-test-tree post-test

test-uniquify: pre-test create-test-trees
	@$(MERGE_SCRIPT) --uniquify $(TEST_CONTENT_TREE) --use-ui-backend $(UI_BACKEND) # ; $(MAKE) -s post-test


test-merge: pre-test create-test-trees
	@$(MERGE_SCRIPT) --input $(TEST_CONTENT_TREE) --reference $(TEST_REFERENCE_TREE) --use-ui-backend $(UI_BACKEND) # ; $(MAKE) -s post-test



# The input test tree will thus have 3 different contents in 6 regular files
# (plus 2 symlinks).
#
# The reference test tree starts with 2 different contents, each with 2
# duplicated, one of these contents being also in the input test tree.
#
create-test-trees: clean-test-trees create-content-tree create-reference-tree

# Mixing various locations, colliding filenames and content, internal and
# external symlinks, and dead ones:
#
create-content-tree:
	@echo " Creating test content tree (3 contents of its own)"
	@mkdir -p $(TEST_CONTENT_TREE)/foo/bar
	@mkdir $(TEST_CONTENT_TREE)/foo/baz $(TEST_CONTENT_TREE)/buz
	@echo "Content A" > $(TEST_CONTENT_TREE)/foo/bar/a.txt
	@echo "Content A" > $(TEST_CONTENT_TREE)/buz/b.txt
	@echo "Content B" > $(TEST_CONTENT_TREE)/buz/a.txt
	@echo "Content C" > $(TEST_CONTENT_TREE)/foo/bar/c.txt
	@echo "Content A" > $(TEST_CONTENT_TREE)/foo/baz/a.txt
	@echo "Content B" > $(TEST_CONTENT_TREE)/foo/baz/c.txt
	@cd $(TEST_CONTENT_TREE)/foo && ln -s bar/c.txt && ln -s bar/a.txt d.txt && ln -s buz/non_existing_target deadlink
	@cd $(TEST_CONTENT_TREE) && ln -s /tmp
	@tree $(TEST_CONTENT_TREE)


create-reference-tree:
	@echo " Creating test reference tree (3 contents of its own, 1 unique)"
	@mkdir -p $(TEST_REFERENCE_TREE)/wilbur $(TEST_REFERENCE_TREE)/kinabe
	@echo "Content A" > $(TEST_REFERENCE_TREE)/b.txt
	@echo "Content B" > $(TEST_REFERENCE_TREE)/wilbur/c.txt
	@echo "Content B" > $(TEST_REFERENCE_TREE)/kinabe/c.txt
	@echo "Unique content" > $(TEST_REFERENCE_TREE)/wilbur/d.txt
	@echo "Unique content" > $(TEST_REFERENCE_TREE)/e.txt
	@tree $(TEST_REFERENCE_TREE)



pre-test:
	@$(MAKE) -s all
	@/bin/rm -f "$(TEST_LOG_FILE)" || true


scan-test-tree:
	@echo "  Running a scan of $(TEST_CONTENT_TREE)"
	@$(MERGE_SCRIPT) --scan $(TEST_CONTENT_TREE) --use-ui-backend $(UI_BACKEND)


# Smart update of a preexisting cache file:
rescan-test-tree:
	@echo "  Running a rescan of $(TEST_CONTENT_TREE)"
	@$(MERGE_SCRIPT) --rescan $(TEST_CONTENT_TREE) --use-ui-backend $(UI_BACKEND)


# Weak update (only based on size) of a preexisting cache file:
resync-test-tree:
	@echo "  Running a resync of $(TEST_CONTENT_TREE)"
	@$(MERGE_SCRIPT) --resync $(TEST_CONTENT_TREE) --use-ui-backend $(UI_BACKEND)


debug:
	@/bin/rm -f "$(TEST_LOG_FILE)" # ; $(MAKE) -s test post-test



run-direct: merge_exec


# Not useful anymore:
# post-test: process-test-log reset-keyboard-mode kill-epmd
post-test:


# Should it prevent new test launches:
kill-epmd:
	@killall epmd 2>/dev/null


process-test-log:
	@if [ -f "$(TEST_LOG_FILE)" ] ; then echo ; echo "#### Content of $(TEST_LOG_FILE) follows:" ; more "$(TEST_LOG_FILE)" ; fi


# Some configurations seem to screw up the terminal (weirdly, most keystrokes
# are then lost)
#
# (note: script found in Ceylan-Hull; still not sufficient here apparently)
#
reset-keyboard-mode:
	@reset-keyboard-mode.sh


help:
	@$(MERGE_SCRIPT) --help --use-ui-backend $(UI_BACKEND)


clean: clean-test-trees


clean-test-trees:
	@echo "   Cleaning test trees $(TEST_TREES)"
	-@/bin/rm -rf $(TEST_TREES)


include $(MYRIAD_TOP)/GNUmakesettings.inc
