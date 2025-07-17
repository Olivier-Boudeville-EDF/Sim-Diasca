# Here are gathered automatic (generic, pattern-based) rules for Traces.
#
# See GNUmakerules-explicit.inc for the explicit (immediate, static)
# counterparts.


# Prerequisite: TRACES_TOP must be set.


# Add all Traces-specific rules below.


# Outside prerequisites: the headers in $(TRACES_TOP)/include and the modules
# from Ceylan-WOOPER and Ceylan-Myriad.


# Allows any upper layer to override as a whole the Traces rules used to compile
# classes and other modules:
#
#ifndef OVERRIDE_TRACES_BUILD_RULES

# No specific rules needed for Traces (as macros resolve in
# text_utils:format/2).

# endif
