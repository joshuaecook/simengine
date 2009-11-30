# Makefile for Documents
# Copyright 2008, Simatra Modeling Technologies, L.L.C.

LATEX = pdflatex
RM = rm -rf

DOCUMENTS = developersLanguageGuide.pdf

help:
	@echo " "
	@echo "**********************************************************"
	@echo "****           Simatra Documents Makefile             ****"
	@echo "**********************************************************"
	@echo "****                                                  ****"
	@echo "****  make all - generate all documents               ****"
	@echo "****                                                  ****"
	@echo "**********************************************************"

all: 
	$(MAKE) $(DOCUMENTS)
	@echo " "
	@echo "Run 'acroread $(DOCUMENTS)' to see generated files"

%.pdf: %.tex
	@echo "Creating $@ from $? ..."
	@$(LATEX) $?
	@$(LATEX) $?

clean:
	@echo "Removing all generated files"
	@$(RM) *.toc *.pdf *.out *.log *.aux

