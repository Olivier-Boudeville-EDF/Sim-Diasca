SIM_DIASCA_TOP = ../..

.PHONY: all pdf toolbox-archive info-archive clean-local

DIAGRAM_ARCHIVE := dataflow-diagram-toolbox.zip

DIAGRAMS := dataflow-city-example.dia dataflow-basic-example.graph    \
			dataflow-example.dia dataflow-legend.dia                  \
			bus-and-channel-example.dia port-iteration.dia README.txt \
			$(SIM_DIASCA_TOP)/../mock-simulators/dataflow-urban-example/doc/urban-example-dataflow.dia


.PHONY: all toolbox-archive track info-tool info-archive clean-local


OVERALL_DOCUMENT_SOURCE := SimDiasca-dataflow-howto-english.rst


# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' to generate the "\
	"dataflow HOWTO ($(DATAFLOW_HOWTO))."
	@echo "  Run 'make toolbox-archive' to generate the "\
	"dataflow diagram archive."


pdf: full-doc

toolbox-archive: clean-local $(DIAGRAM_ARCHIVE)


# Omit the input tree hierarchy:
$(DIAGRAM_ARCHIVE): README.txt
	@zip --junk-paths $(DIAGRAM_ARCHIVE) $(DIAGRAMS) && \
	echo "Archive $@ is available."



README.txt:
	@echo "This archive contains a small collection of diagrams that can be serve as inputs either of the Dia editor (https://sourceforge.net/projects/dia-installer/) or of the Graphviz dot tool (http://www.graphviz.org/). Their extension is respectively '.dia' and '.graph'." > README.txt
	@echo "They can be used as a starting basis when devising one's dataflow (one may preferably rely on 'urban-example-dataflow.dia' for that)." >> README.txt


# Root specified as we want to catch the doc in src/core as well:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)


info-archive:
	@echo "DIAGRAM_ARCHIVE = $(DIAGRAM_ARCHIVE)"
	@echo "DIAGRAMS = $(DIAGRAMS)"


clean-local:
	@/bin/rm -f $(DIAGRAM_ARCHIVE) README.txt


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
