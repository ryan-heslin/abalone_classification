# Largely copied from https://www.r-bloggers.com/2015/03/makefiles-and-rmarkdown/
ROOT_DIR = .
DATA_DIR = $(ROOT_DIR)/data
R_DIR = $(ROOT_DIR)/R
OUTPUT_FORMAT = pdf_document
OUTPUT_YAML = $(DATA_DIR)/rmd_out.yaml
OUTPUTS_DIR = $(ROOT_DIR)/outputs

R_FILES = $(wildcard $(R_DIR)/[0-9][0-9]*.R)
RMD_FILES = $(R_FILES:.R=.Rmd)
PDF_FILES = $(OUTPUTS_DIR)/$(basename $(R_FILES)).pdf

SPIN = R -e 'sapply(unlist(strsplit("$^", split = " ")), knitr::spin, knit = FALSE, precious = TRUE)'
KNIT = R -e  'sapply(unlist(strsplit("$^", split = " ")), rmarkdown::render, output_format = "$(OUTPUT_FORMAT)", output_dir = "$(OUTPUTS_DIR)", output_yaml = "$(OUTPUT_YAML)",  envir = parent.frame())'

$(PDF_FILES) : $(RMD_FILES)
	$(KNIT)
$(RMD_FILES) : $(R_FILES)
	$(SPIN)

clean:
	rm R/*.Rmd outputs/*.pdf *.html *.md
