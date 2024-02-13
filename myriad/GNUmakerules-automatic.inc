# Here are gathered automatic (generic, pattern-based) rules for Myriad.


# From the point of view of the make tool, they come first, yet the rules that
# are actually selected are the ones with the longer name match, so any explicit
# rule will override them.
#
# See GNUmakerules-explicit.inc for the explicit (immediate, static)
# counterparts.

# From here all variables are expected to be set/updated by the current project,
# as first all the variables of englobing projects should be set, then only
# rules based on that variables should be defined.


# Prerequisite: MYRIAD_TOP must be set.

# Now defining the generic rules operating on the variables.


.PHONY: .PHONY-myriad-automatic

.PHONY-myriad-automatic: $(MODULES_DIRS)



# Note: long lines in rules should not be word-wrapped (with \), as their
# displaying on the console would print such characters, leading to poorly dense
# outputs, that are not pleasing to the eye.



# Compilation section.


# We must first go into the directory in which the source of the parse transform
# lies, otherwise the BEAM file will be created in the current (e.g. root)
# directory:
#
# (note that we want that parse transforms are themselves parse-transformed by
# the ones of the lower layers)
#
%_parse_transform.beam: %_parse_transform.erl
	@echo "		Compiling parse transform $<"
	@$(ERLANG_COMPILER_ENV) "$(ERLANG_COMPILER)" $(ERLANG_COMPILER_OPT_FOR_PT) -o $@ $<


# Below two variations coexist in order to run an Erlang program:
# - a legacy (commented-out) one based on -eval
# - a newer one based on -run
#
# We have a slight preference for the latter, as no interpretation (as with
# eval) is needed and the stacktraces may be a little shorter.


# A given project (e.g. Foobar) may rely on generated "support modules",
# typically to share constants based on the const_{table, bijective_table,
# bijective_topics} facilities.
#
# For that we generally rely on the convention that the main module of that
# project (e.g. foobar) is then to define and export a
# generate_support_modules/0 function (whose spec is: generate_support_modules()
# -> no_return()) in charge of the generation of such support modules (as BEAM
# files), including at least a 'foobar_generated' module.
#
# The role of the present make target is to call that function: 'make
# foobar_generate_support_modules' will run foobar:generate_support_modules/0,
# which is expected to generate in turn said modules that will be needed (only)
# at runtime; this target is generally set as a local dependency of the 'all'
# target, or even preferably, is listed in the EXTRA_BEAM_FILES make
# variable. Refer to Ceylan-Oceanic for an example thereof.
#
# We kept the -eval based legacy variation here, which is simpler (as the -run
# variation expects the function name to be 'run'):
#
%_generated.beam: %_constants.erl %.beam
	@echo "         Generating support modules based on $<"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $< | sed 's|\.erl$$||1')-generator-for-$${USER} --eval $$(echo $@ | sed 's|_generated\.beam$$|:generate_support_modules()|1') $(ALL_CMD_LINE_OPTIONS)


# Standard general compilation rules:


# We consider that, for tests, header files are to lie in the same (test)
# directory as the corresponding sources:
#
%_test.beam: %_test.erl %_test.hrl
	@echo "		Compiling test module with header $<"
	@$(ERLANG_COMPILER_ENV) "$(ERLANG_COMPILER)" $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) $(ERLANG_COMPILER_OPT_FOR_TESTS) -o $@ $<


%_test.beam: %_test.erl
	@echo "		Compiling test module $<"
	@$(ERLANG_COMPILER_ENV) "$(ERLANG_COMPILER)" $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) $(ERLANG_COMPILER_OPT_FOR_TESTS) -o $@ $<


# You probably want to uncomment the next rule, not this one.
%.beam: %.erl $(LAYER_TOP)/%.hrl
	@echo "		Compiling standard module with header $<"
	@$(ERLANG_COMPILER_ENV) "$(ERLANG_COMPILER)" $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<


# The main, most usual rule, with its most useful printout:
%.beam: %.erl
	@echo "		Compiling standard module $<"
	@$(ERLANG_COMPILER_ENV) "$(ERLANG_COMPILER)" $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<



# Applications are meant to be executed, whereas tests are meant to be run: they
# have different suffixes (_app vs _test) and ways of being launched (make
# X_exec / make X_run) otherwise, for example, my_server_app.erl would shadow
# my_server_test.erl if an ambiguous 'make my_server_run' was issued.


%.class: %.java
	@echo "		Compiling the $< Java class"
	@"$(JAVAC)" $(JAVAC_OPT) $<



ifeq ($(USE_PROTOBUF),true)

# Single execution, two targets generated:
# Not compatible with older make (e.g. 4.1): %.erl %.hrl &: %.proto
%.erl %.hrl: %.proto
	@echo "		Generating $@ from Protobuf spec $<"
	@"$(PB_COMPILER)" $(PB_OPT) $< $(GPB_SUFFIX_OPT)


endif



# Application section.


# 'X_exec' becomes 'X_app:exec()':


%_exec: %_app.beam %.beam
	@echo "		Executing application $^ (first form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --run basic_utils exec $$(echo $@ | sed 's|_exec$$|_app)|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --eval $$(echo $@ | sed 's|_exec$$|_app:exec()|1') $(ALL_CMD_LINE_OPTIONS)


%_exec: %_app.beam
	@echo "		Executing application $^ (second form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --run basic_utils exec $$(echo $@ | sed 's|_exec$$|_app|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --eval $$(echo $@ | sed 's|_exec$$|_app:exec()|1') $(ALL_CMD_LINE_OPTIONS)


# 'X_non_interactive_exec' becomes 'X_app:exec()' and is launched as a
# non-interactive application (typically to remain synchronous yet not to block
# systemctl because of a prompt offering a post-mortem study after a crash):
#
%_non_interactive_exec: %_app.beam %.beam
	@echo "		Executing application $^ (first form, in a non-interactive variant)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --run basic_utils exec $$(echo $@ | sed 's|_non_interactive_exec$$|_app|1') --non-interactive $(ALL_CMD_LINE_OPTIONS)

# @$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --eval $$(echo $@ | sed 's|_non_interactive_exec$$|_app:exec()|1') --non-interactive $(ALL_CMD_LINE_OPTIONS)


%_non_interactive_exec: %_app.beam
	@echo "		Executing application $^ (second form, in a non-interactive variant)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --run basic_utils exec $$(echo $@ | sed 's|_non_interactive_exec$$|_app|1') --non-interactive $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --eval $$(echo $@ | sed 's|_non_interactive_exec$$|_app:exec()|1') --non-interactive $(ALL_CMD_LINE_OPTIONS)



# To run in the background (somewhat like a daemon):
#
# Note: the specified long name (with $(NODE_NAMING)) has its '_background'
# suffix removed so that the node name of an executable does not depend from how
# it is launched, otherwise clients needing to connect to that node would
# themselves depend on that.

%_exec_background: %_app.beam %.beam
	@echo "		Executing application $^ in the background (first form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_background$$||1')-$${USER} --background --run basic_utils exec $$(echo $@ | sed 's|_exec_background$$|_app|1') $(ALL_CMD_LINE_OPTIONS) &

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_background$$||1')-$${USER} --background --eval $$(echo $@ | sed 's|_exec_background$$|_app:exec()|1') $(ALL_CMD_LINE_OPTIONS) &


%_exec_background: %_app.beam
	@echo "		Executing application $^ in the background (second form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_background$$||1')-$${USER} --background --run basic_utils exec $$(echo $@ | sed 's|_exec_background$$|_app|1') $(ALL_CMD_LINE_OPTIONS) &

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_background$$||1')-$${USER} --background --eval $$(echo $@ | sed 's|_exec_background$$|_app:exec()|1') $(ALL_CMD_LINE_OPTIONS) &


# The two sets of rules below are functional, yet they use very different
# solutions (run_erl/eval) that can be tested independently.


# To run an application as a service.
#
# Exactly like when running as %_exec_background, except services are shared
# hence their node name must no depend on any user name; they are directly named
# as their core target (e.g. 'make X_exec_service' results in a node named
# 'X@LOCALHOSTNAME').
#
# (these rules uses run_erl, that runs in the background)


%_exec_service: %_app.beam %.beam
	@echo "		Executing application $^ as a service (first form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_exec_service$$||1') --daemon --run basic_utils exec $$(echo $@ | sed 's|_exec_service$$|_app|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_exec_service$$||1') --daemon --eval $$(echo $@ | sed 's|_exec_service$$|_app:exec()|1') $(ALL_CMD_LINE_OPTIONS)


%_exec_service: %_app.beam
	@echo "		Executing application $^ as a service (second form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_exec_service$$||1') --daemon --run basic_utils exec $$(echo $@ | sed 's|_exec_service$$|_app|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_exec_service$$||1') --daemon --eval $$(echo $@ | sed 's|_exec_service$$|_app:exec()|1') $(ALL_CMD_LINE_OPTIONS)


# Interactive variants, for service debugging.
#
# These rules used to rely on a standard 'erl -eval' put in the background, with
# the --background option, i.e so that the VM was run in detached mode (yet
# without run_erl).
#
# There was also a final '&', leading to a crash ("*** Terminating erlang...")
# most probably because the VM was twice set in background.
#
# One may get rid in all cases of that '&', and the --background option may or
# may not be added. Its main drawback is that no output or activity is then
# shown on the console.
#
%_exec_service_debug: %_app.beam %.beam
	@echo "		Executing application $^ as a debugged service (first form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_exec_service_debug$$||1') --run basic_utils exec $$(echo $@ | sed 's|_exec_service_debug$$|_app|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_exec_service_debug$$||1') --eval $$(echo $@ | sed 's|_exec_service_debug$$|_app:exec()|1') $(ALL_CMD_LINE_OPTIONS)


%_exec_service_debug: %_app.beam
	@echo "		Executing application $^ as a debugged service (second form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_exec_service_debug$$||1') --run basic_utils exec $$(echo $@ | sed 's|_exec_service_debug$$|_app|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_exec_service_debug$$||1') --eval $$(echo $@ | sed 's|_exec_service_debug$$|_app:exec()|1') $(ALL_CMD_LINE_OPTIONS)



# Test section.


#%_test: %.beam %_test.beam
#	@echo "    Running test function $(STARTUP_FUNCTION) in module $@"
#	@"$(ERLANG_INTERPRETER)" $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION)


#%_test: %_test.beam
#	@echo "    Running test function $(STARTUP_FUNCTION) in module $@"
#	@"$(ERLANG_INTERPRETER)" $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION)



%_test: %.beam %_test.beam
	@# Even just a comment is needed here, to force rebuild.


%_test: %_test.beam
	@# Even just a comment is needed here, to force rebuild.


%_interactive_test: %.beam %_interactive_test.beam
	@echo "		Executing interactively test function "
	"$(STARTUP_FUNCTION) in module $@"
	@"$(ERLANG_INTERPRETER)" $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION) $(ALL_CMD_LINE_OPTIONS)


%_batch_test: %.beam %_batch_test.beam
	@echo "		Executing non-interactively test function "
	"$(STARTUP_FUNCTION) in module $@"
	@"$(ERLANG_INTERPRETER)" $(ERLANG_INTERPRETER_OPT) -run $@ $(STARTUP_FUNCTION) $(ALL_CMD_LINE_OPTIONS)



# _integration prefix added not to match instead of the next rule.
# %_integration_dependencies target is a phony target, so that test dependencies
# can be specified.
#
%_integration_run: %_integration_test.beam %_integration_dependencies
	@echo "		Running integration test $@ from $^ with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --run basic_utils run $$(echo $@ | sed 's|_integration_run$$|_integration_test|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --eval $$(echo $@ | sed 's|_integration_run$$|_integration_test:run()|1') $(ALL_CMD_LINE_OPTIONS)



# To run on a cluster is basically running with no shell nor interactive input,
# but without being put in the background (no ending '&', no detached option,
# etc.)


%_cluster_run: %_test.beam %.beam
	@echo "		Executing test $^ for the cluster (first form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_cluster_run$$|_run|1')-$${USER} --non-interactive --run basic_utils run $$(echo $@ | sed 's|_cluster_run$$|_test|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_cluster_run$$|_run|1')-$${USER} --non-interactive --eval $$(echo $@ | sed 's|_cluster_run$$|_test:run()|1') $(ALL_CMD_LINE_OPTIONS)



%_cluster_run: %_test.beam
	@echo "		Executing test $^ for the cluster (second form)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_cluster_run$$|_run|1')-$${USER} --non-interactive --run basic_utils run $$(echo $@ | sed 's|_cluster_run$$|_test|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $$(echo $@ | sed 's|_cluster_run$$|_run|1')-$${USER} --non-interactive --eval $$(echo $@ | sed 's|_cluster_run$$|_test:run()|1') $(ALL_CMD_LINE_OPTIONS)




# 'X_run' becomes 'X_test:run()':


%_run: %_test %_test_dependencies
	@echo "		Running unitary test $@ (first form) from $^, with $(ERL_PARAMETERIZED_LAUNCHER)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --run basic_utils run $$(echo $@ | sed 's|_run$$|_test|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
# @$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --eval $$(echo $@ | sed 's|_run$$|_test:run()|1') $(ALL_CMD_LINE_OPTIONS)


%_run: %_test %.beam
	@echo "		Running unitary test $@ (second form) from $^"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --run basic_utils run $$(echo $@ | sed 's|_run$$|_test|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --eval $$(echo $@ | sed 's|_run$$|_test:run()|1') $(ALL_CMD_LINE_OPTIONS)


# Again and again one will remove the '#' from the second line, in order to
# inspect the actual command-line:
#
%_run: %_test
	@echo "		Running unitary test $@ (third form) from $^"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --run basic_utils run $$(echo $@ | sed 's|_run$$|_test|1') $(ALL_CMD_LINE_OPTIONS)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --eval $$(echo $@ | sed 's|_run$$|_test:run()|1') $(ALL_CMD_LINE_OPTIONS)

# Without relying on basic_utils:run/1:
#	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --run $$(echo $@ | sed 's|_run$$|_test run|1') $(ALL_CMD_LINE_OPTIONS)


%_run_with_shell: %_test
	@echo "		Running unitary test with shell from $^"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --run basic_utils run $$(echo $@ | sed 's|_run_with_shell$$|_test|1') $(ALL_CMD_LINE_OPTIONS_FOR_SHELL)

# Legacy -eval based variation:
#@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} --eval $$(echo $@ | sed 's|_run_with_shell$$|_test:run()|1') $(ALL_CMD_LINE_OPTIONS_FOR_SHELL)



# Not wanting here the options for standard executions (such as -noinput or
# alike):
#
%_shell: %_test
	@echo "		Launching a shell for test $@ from $^"
	@echo "  (one may then enter: '$<:run().' to execute the test)"
	@$(ERL_PARAMETERIZED_LAUNCHER) $(NODE_NAMING) $@-$${USER} $(INTERNAL_OPTIONS) -extra $(CMD_LINE_OPT)


%.plt: %.beam
	@echo "		Checking module '$<' against relevant PLT ($(PLT_FILE))"
	@"$(DIALYZER)" $(DIALYZER_OPTS) --plt $(PLT_FILE) $<


# Does not trigger a full rebuild.
#
# Example of use: 'make eqwalize-text_utils'.
#
# (see also the check-with-eqwalizer explicit target)
#
eqwalize-%: ebin/%.beam
	@echo "		Checking module '$<' with eqWAlizer"
	@$(EQWALIZER) eqwalize $* $(EQWALIZER_OPTS)



# GLSL shader section.

# The GLSL reference compiler does not return output if it detects no error.

check-%.vertex.glsl: %.vertex.glsl
	@echo "    Checking vertex shader $<"
	@$(GLSL_REF_COMPILER) $(GLSL_REF_COMPILER_OPT) -S vert $<


check-%.tess-ctrl.glsl: %.tess-ctrl.glsl
	@echo "    Checking tessellation control shader $<"
	@$(GLSL_REF_COMPILER) $(GLSL_REF_COMPILER_OPT) -S tesc $<


check-%.tess-eval.glsl: %.tess-eval.glsl
	@echo "    Checking tessellation evaluation shader $<"
	@$(GLSL_REF_COMPILER) $(GLSL_REF_COMPILER_OPT) -S tese $<


check-%.geometry.glsl: %.geometry.glsl
	@echo "    Checking geometry shader $<"
	@$(GLSL_REF_COMPILER) $(GLSL_REF_COMPILER_OPT) -S geom $<


check-%.fragment.glsl: %.fragment.glsl
	@echo "    Checking fragment shader $<"
	@$(GLSL_REF_COMPILER) $(GLSL_REF_COMPILER_OPT) -S frag $<


check-%.compute.glsl: %.compute.glsl
	@echo "    Checking compute shader $<"
	@$(GLSL_REF_COMPILER) $(GLSL_REF_COMPILER_OPT) -S comp $<