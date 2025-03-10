PACKAGE=main

PDF = $(PACKAGE).pdf

all:  pdf

pdf:
	pdflatex $(PACKAGE)
	- bibtex $(PACKAGE)
	pdflatex $(PACKAGE)
	pdflatex $(PACKAGE)
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $(PACKAGE).log) \
	do pdflatex $(PACKAGE); done

docclean:
	$(RM)  *.log *.aux \
	*.cfg *.glo *.idx *.toc \
	*.ilg *.ind *.out *.lof \
	*.lot *.bbl *.blg *.gls *.cut *.hd \
	*.dvi *.ps *.thm *.tgz *.zip *.rpi

clean: docclean

distclean: clean
	$(RM)  *.pdf

.PHONY: all docclean clean distclean
.SILENT: pdf
