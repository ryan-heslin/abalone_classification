# Largely copied from https://www.r-bloggers.com/2015/03/makefiles-and-rmarkdown/
ROOT_DIR = .
DATA_DIR = $(ROOT_DIR)/data
R_DIR = $(ROOT_DIR)/R
OUTPUT_FORMAT = pdf_document
OUTPUT_YAML = $(DATA_DIR)/rmd_out.yaml
RMD_HEADER = $(DATA_DIR)/header.Rmd
OUTPUTS_DIR = $(ROOT_DIR)/outputs

R_FILES = $(wildcard $(R_DIR)/[0-9][0-9]*.R)
RMD_FILES = $(R_FILES:.R=.Rmd)
.PHONY: RMD_FILES $(RMD_FILES)
PDF_FILES = $(OUTPUTS_DIR)/$(basename $(R_FILES)).pdf
SPIN = R -e 'sapply(unlist(strsplit("$^", split = " ")), knitr::spin, knit = FALSE, precious = TRUE)'

#for path in $^ ; do \
	   		#R -e "knitr::spin(hair = '""$$path""', knit = FALSE, precious = TRUE)";  \
			#echo "$$(cat -v $(RMD_HEADER))$$(cat -v $${path%.R}.Rmd)" > "$${path%.R}.Rmd"; \
			#sed -i -e 's/\r//g' "$${path%.R}.Rmd"; \
	   #done
#
KNIT = R -e  'sapply(unlist(strsplit("$^", split = " ")), rmarkdown::render,  output_dir = "$(OUTPUTS_DIR)", envir = parent.frame())'

$(PDF_FILES) : $(RMD_FILES)
	$(KNIT)
# Assert existence
#RMD_FILES : $(RMD_FILES)
$(RMD_FILES) : $(R_FILES)
	$(SPIN)

.PHONY: clean
clean:
	rm -f $(R_DIR)/*.Rmd $(OUTPUTS_DIR)/*.pdf $(ROOT_DIR)/**/*.html $(ROOT_DIR)/**/*.md
