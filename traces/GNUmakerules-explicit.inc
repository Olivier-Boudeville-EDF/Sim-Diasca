# Here are gathered explicit (immediate, static) rules for Traces.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: TRACES_TOP must be set.


.PHONY: default-traces-rule create-traces-checkout                   \
		rebar3-local-update rebar3-local-update-for-traces           \
		clean clean-traces


# The first non-generic, therefore default, rule:
default-traces-rule: all


# Create a _checkouts directory in which all Ceylan dependencies for the current
# layer are referenced through a symlink.
#
create-traces-checkout:
	@echo "  Creating a checkout directory for Traces"
	@$(MAKE) -s create-wooper-checkout
	@cd _checkouts && ln -sf ../$(WOOPER_TOP)


# Allows to update in the current OTP _build tree WOOPER files from a supposedly
# local built tree (hence not packaged; obtained thanks to a 'make
# rebar3-compile' there) alongside the current one (through a checkout directory
# or a sibling one).
#
# Allows to rely on a local, updated WOOPER without having to publish yet an Hex
# package for it.
#
rebar3-local-update: rebar3-local-update-for-traces


# As called by upper layers:
#
# (used to depend beforehand on the rebar3-local-sibling-update-for-traces
#  target):
#
rebar3-local-update-for-traces: rebar3-local-checkout-update-for-traces


# Supposing typically that a _checkouts/wooper symlinks exists, pointing to an
# actual WOOPER clone:
#
rebar3-local-checkout-update-for-traces:
	@echo "  Updating WOOPER from local checkout build tree $(WOOPER_CHECKOUT_BUILD)"
	@cd $(WOOPER_CHECKOUT_BUILD) && $(MAKE) -s rebar3-compile
	@mkdir -p $(WOOPER_LOCAL_EBIN)
	@/bin/cp -f $(WOOPER_CHECKOUT_BUILD)/_build/default/lib/wooper/ebin/* $(WOOPER_LOCAL_EBIN)


# Supposing a wooper symlink exists at the same level (sibling) of the traces
# root one.
#
# Mostly superseded by a more convenient solution based on a _checkouts
# directory (see rebar3-local-checkout-update-for-traces above).
#
rebar3-local-sibling-update-for-traces:
	@echo "  Updating WOOPER from local sibling build tree $(WOOPER_SIBLING_BUILD)"
	@cd $(WOOPER_SIBLING_BUILD) && $(MAKE) -s rebar3-compile
	@mkdir -p $(WOOPER_LOCAL_EBIN)
	@/bin/cp -f $(WOOPER_SIBLING_BUILD)/_build/default/lib/wooper/ebin/* $(WOOPER_LOCAL_EBIN)



# Not wanting to define a default rule:
clean: clean-traces

clean-traces:
	-@/bin/rm -f $(TRACES_FILES)
