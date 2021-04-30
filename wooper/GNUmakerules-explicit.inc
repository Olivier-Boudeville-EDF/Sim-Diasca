# Here are gathered explicit (immediate, static) rules for WOOPER.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: WOOPER_TOP must be set.


.PHONY: default-wooper-rule clean-ast-debug clean-ast-debug-wooper    \
		create-wooper-checkout                                        \
		rebar3-local-update rebar3-local-update-for-wooper            \
		rebar3-local-checkout-update-for-wooper                       \
		rebar3-local-sibling-update-for-wooper


# The first non-generic, therefore default, rule:
default-wooper-rule: all


# Removes the debug files created about ASTs:
clean-ast-debug: clean-ast-debug-wooper

clean-ast-debug-wooper:
	@find . -name 'WOOPER-*put-AST*.txt' -exec /bin/rm '{}' ';' 2>/dev/null || true


# Create a _checkouts directory in which all Ceylan dependencies for the current
# layer are referenced through a symlink.
#
create-wooper-checkout:
	@echo "  Creating a checkout directory for WOOPER"
	@$(MAKE) -s create-myriad-checkout
	@cd _checkouts && ln -sf ../$(MYRIAD_TOP)


# Allows to update in the current OTP _build tree Myriad files from a supposedly
# local built tree (hence not packaged; obtained thanks to a 'make
# rebar3-compile' there) alongside the current one (through a checkout directory
# or a sibling one).
#
# Allows to rely on a local, updated Myriad without having to publish yet an Hex
# package for it.
#
rebar3-local-update: rebar3-local-update-for-wooper


# As called by upper layers:
#
# (used to depend beforehand on the rebar3-local-sibling-update-for-wooper
#  target):
#
rebar3-local-update-for-wooper: rebar3-local-checkout-update-for-wooper


# Supposing typically that a _checkouts/myriad symlinks exists, pointing to an
# actual Myriad clone:
#
rebar3-local-checkout-update-for-wooper:
	@echo "  Updating Myriad from local checkout build tree $(MYRIAD_CHECKOUT_BUILD)"
	@cd $(MYRIAD_CHECKOUT_BUILD) && $(MAKE) -s rebar3-compile
	@mkdir -p $(MYRIAD_LOCAL_EBIN)
	@/bin/cp -f $(MYRIAD_CHECKOUT_BUILD)/_build/default/lib/myriad/ebin/* $(MYRIAD_LOCAL_EBIN)


# Supposing a myriad symlink exists at the same level (sibling) of the wooper
# root one.
#
# Mostly superseded by a more convenient solution based on a _checkouts
# directory (see rebar3-local-checkout-update-for-wooper above).
#
rebar3-local-sibling-update-for-wooper:
	@echo "  Updating Myriad from local sibling build tree $(MYRIAD_SIBLING_BUILD)"
	@cd $(MYRIAD_SIBLING_BUILD) && $(MAKE) -s rebar3-compile
	@mkdir -p $(MYRIAD_LOCAL_EBIN)
	@/bin/cp -f $(MYRIAD_SIBLING_BUILD)/_build/default/lib/myriad/ebin/* $(MYRIAD_LOCAL_EBIN)



# Bootstrap section.


# Bootstrapped modules must be special-cased, as they should be built prior to
# any parse-transform (here, the WOOPER one) using them, and the resulting BEAM
# must be produced in the right directory (not at the root).

# See also: in GNUmakevars.inc, the BOOTSTRAP_MODULES variable.


# We still need a special build here, so that the BEAM is generated in src
# rather than at the root of WOOPER:
#
$(WOOPER_TOP)/src/wooper_info.beam: $(WOOPER_TOP)/src/wooper_info.erl
	@echo "     Compiling bootstrap module $<"
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o src $<
