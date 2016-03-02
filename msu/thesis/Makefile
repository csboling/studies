TOPLEVEL = thesis
TEXFILES = $(shell find . -name '\*.tex')
BIBFILES = $(shell find . -name '\*.bib')

all: $(TOPLEVEL).pdf

thesis.pdf: $(TEXFILES)
	pdflatex $(TOPLEVEL).tex
	bibtex   $(TOPLEVEL)
	pdflatex $(TOPLEVEL).tex
	pdflatex $(TOPLEVEL).tex

clean:
	rm $(TOPLEVEL).pdf
	rm $(TOPLEVEL).out
	rm $(TOPLEVEL).aux
	rm $(TOPLEVEL).bbl
	rm $(TOPLEVEL).blg
	rm $(TOPLEVEL).log
