# Here are gathered automatic (generic, pattern-based) rules for Sim-Diasca.
#
# See GNUmakerules-explicit.inc for the explicit (immediate, static)
# counterparts.


# Prerequisite: SIM_DIASCA_TOP must be set.



# Now, to accommodate the SII, the user node is launched in non-distributed mode
# (i.e. no more '--ln $@' in the rules):

%_run: %_sim.beam %.beam %_run_dependencies
	@echo "     Running simulation $@ (two prerequisites) with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) --eval $$(echo $@ | sed 's|_run$$|_sim:run()|1') $(ALL_CMD_LINE_OPTIONS)


%_run: %_sim.beam %_run_dependencies
	@echo "     Running simulation $@ (one prerequisite) with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) --eval $$(echo $@ | sed 's|_run$$|_sim:run()|1') $(ALL_CMD_LINE_OPTIONS)


%_run: %_sim.beam %.beam
	@echo "     Running simulation $@ (two prerequisites) with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) --eval $$(echo $@ | sed 's|_run$$|_sim:run()|1') $(ALL_CMD_LINE_OPTIONS)



%_case: %.beam %_case.beam
	@# Even just a comment is needed here, to force rebuild.


%_case: %_case.beam
	@# Even just a comment is needed here, to force rebuild.



# To run executions labelled as (simulation) cases, rather than as mere tests:
%_run: %_case
	@echo "     Running simulation case $^"
	@#echo $(ERL_PARAMETERIZED_LAUNCHER) --eval $$(echo $@ | sed 's|_run$$|_case:run()|1') $(ALL_CMD_LINE_OPTIONS)
	@$(ERL_PARAMETERIZED_LAUNCHER) --eval $$(echo $@ | sed 's|_run$$|_case:run()|1') $(ALL_CMD_LINE_OPTIONS)



# Rules adapted from the WOOPER ones, overridden thanks to the
# OVERRIDE_WOOPER_BUILD_RULES flag:

class_%_test.beam: class_%_test.erl class_%_test.hrl
	@echo "		Compiling Sim-Diasca test with header $<"
	@#echo $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<


# Most frequently-used rule for tests:
class_%_test.beam: class_%_test.erl
	@echo "		Compiling Sim-Diasca test $<"
	@#echo $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<


class_%.beam: class_%.erl class_%.hrl class_%_dependencies
	@echo "		Compiling Sim-Diasca class with header $<"
	@#echo $(ERLANG_COMPILER) $(COMPILER_OPT_FOR_SIM_DIASCA_CLASSES) -o $@ $<
	@$(ERLANG_COMPILER) $(COMPILER_OPT_FOR_SIM_DIASCA_CLASSES) -o $@ $<


class_%.beam: class_%.erl class_%_dependencies
	@echo "		Compiling Sim-Diasca class $<"
	@#echo $(ERLANG_COMPILER) $(COMPILER_OPT_FOR_SIM_DIASCA_CLASSES) -o $@ $<
	@$(ERLANG_COMPILER) $(COMPILER_OPT_FOR_SIM_DIASCA_CLASSES) -o $@ $<


class_%.beam: class_%.erl class_%.hrl
	@echo "		Compiling Sim-Diasca no-dependency class with header $<"
	@#echo $(ERLANG_COMPILER) $(COMPILER_OPT_FOR_SIM_DIASCA_CLASSES) -o $@ $<
	@$(ERLANG_COMPILER) $(COMPILER_OPT_FOR_SIM_DIASCA_CLASSES) -o $@ $<


# Most frequently-used rule:
class_%.beam: class_%.erl
	@echo "		Compiling Sim-Diasca no-dependency class $<"
	@#echo $(ERLANG_COMPILER) $(COMPILER_OPT_FOR_SIM_DIASCA_CLASSES) -o $@ $<
	@$(ERLANG_COMPILER) $(COMPILER_OPT_FOR_SIM_DIASCA_CLASSES) -o $@ $<
