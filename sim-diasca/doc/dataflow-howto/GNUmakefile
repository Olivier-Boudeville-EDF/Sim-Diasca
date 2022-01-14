SIM_DIASCA_TOP = ../..


.PHONY: doc html pdf toolbox-archive info-archive                \
		clean-local info-archive clean-local


DIAGRAM_ARCHIVE := dataflow-diagram-toolbox.zip

DIAGRAMS := dataflow-city-example.dia dataflow-basic-example.graph    \
			dataflow-example.dia dataflow-legend.dia                  \
			bus-and-channel-example.dia port-iteration.dia README.txt \
			$(SIM_DIASCA_TOP)/../mock-simulators/dataflow-urban-example/doc/urban-example-dataflow.dia


.PHONY: doc html pdf toolbox-archive clean-local \
		info-tool info-archive info-doc info-local-doc


OVERALL_DOCUMENT_SOURCE := Sim-Diasca-dataflow-howto-english.rst


# Default do-nothing target:
doc:
	@echo "  Run 'make local-doc' to generate the "\
	"dataflow HOWTO ($(DATAFLOW_HOWTO)) documents."
	@echo "  Run 'make toolbox-archive' to generate the "\
	"dataflow diagram archive."


html: local-html

pdf: local-pdf


toolbox-archive: clean-local $(DIAGRAM_ARCHIVE)


# Omit the input tree hierarchy:
$(DIAGRAM_ARCHIVE): README.txt
	@zip --junk-paths $(DIAGRAM_ARCHIVE) $(DIAGRAMS) && \
	echo "Archive $@ is available."



README.txt:
	@echo "This archive contains a small collection of diagrams that can be serve as templates either of the Dia editor (https://sourceforge.net/projects/dia-installer/) or of the Graphviz dot tool (http://www.graphviz.org/). Their extension is respectively '.dia' and '.graph'." > README.txt
	@echo "They can be used as a starting basis when devising one's dataflow (one may preferably rely on 'urban-example-dataflow.dia' for that)." >> README.txt


# Root specified as we want to catch the doc in src/core as well:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)


clean-local:
	@/bin/rm -f $(DIAGRAM_ARCHIVE) README.txt


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


info-archive:
	@echo "DIAGRAM_ARCHIVE = $(DIAGRAM_ARCHIVE)"
	@echo "DIAGRAMS = $(DIAGRAMS)"


info-doc: info-local-doc

info-local-doc:
	@echo "SD_DATAFLOW_HOWTO_NAME = $(SD_DATAFLOW_HOWTO_NAME)"
	@echo "SD_DATAFLOW_HOWTO_DIR = $(SD_DATAFLOW_HOWTO_DIR)"
	@echo "SD_DATAFLOW_HOWTO_BASE = $(SD_DATAFLOW_HOWTO_BASE)"
	@echo "SD_DATAFLOW_HOWTO_SRC = $(SD_DATAFLOW_HOWTO_SRC)"
	@echo "SD_DATAFLOW_HOWTO_HTML = $(SD_DATAFLOW_HOWTO_HTML)"
	@echo "SD_DATAFLOW_HOWTO_PDF = $(SD_DATAFLOW_HOWTO_PDF)"
