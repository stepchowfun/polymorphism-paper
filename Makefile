PDF = main.pdf

all: ${PDF}

%.pdf: %.dtx acmart.cls
	pdflatex $<
	- bibtex $*
	pdflatex $<
	- makeindex -s gind.ist -o $*.ind $*.idx
	- makeindex -s gglo.ist -o $*.gls $*.glo
	pdflatex $<
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $*.log) \
	do pdflatex $<; done

%.cls: %.ins %.dtx
	pdflatex $<

%.pdf: %.tex acmart.cls ACM-Reference-Format.bst
	pdflatex $<
	- bibtex $*
	pdflatex $<
	pdflatex $<
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $*.log) \
	do pdflatex $<; done

main.pdf: main.tex

.PRECIOUS: acmart.cfg acmart.cls

clean:
	$(RM) acmart.cls *.log *.aux \
	*.cfg *.glo *.idx *.toc \
	*.ilg *.ind *.out *.lof \
	*.lot *.bbl *.blg *.gls *.cut *.hd \
	*.dvi *.ps *.thm *.tgz *.zip *.rpi

distclean: clean
	$(RM) $(PDF)

#
# Archive for the distribution. Includes typeset documentation
#
archive: all clean
	tar -C .. -czvf acmart.tgz --exclude '*~' --exclude '*.tgz' --exclude '*.zip' --exclude CVS --exclude '.git*' acmart

zip: all clean
	zip -r acmart.zip * -x '*~' -x '*.tgz' -x '*.zip' -x CVS -x 'CVS/*'
