SIM_DIASCA_TOP = ../../..

MYRIAD_TOP = $(SIM_DIASCA_TOP)/../myriad


.PHONY: all diagrams-from-dia clean


DIAGRAMS_FROM_DOT := simulation-decision-process-bordered.png           \
                     simulation-decision-process-french-bordered.png    \
                     simulation-experiment-bordered.png                 \
                     simulation-experiment-french-bordered.png


DIAGRAMS_FROM_DIA:= elements-of-virtual-experiments-bordered.png        \
                    elements-of-virtual-experiments-french-bordered.png


include $(MYRIAD_TOP)/doc/GNUmakerules-docutils.inc


# For dot-generated diagrams:
BORDER_COLOR = '\#3232dd'


all: $(DIAGRAMS_FROM_DOT) diagrams-from-dia


diagrams-from-dia:
	@for f in $(DIAGRAMS_FROM_DIA); do $(MAKE) -s $$f MARGIN_WIDTH=15 BORDER_COLOR=Black BORDER_WIDTH=3; done


# One may run for example: 'make simulation-decision-process.png VIEW_GRAPH='.
#
# Use also the inherited 'diagrams', 'info-diagrams', 'clean-diagrams' targets.


clean: clean-diagrams
