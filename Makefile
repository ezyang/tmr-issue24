issue = Issue24

#lhssources = 
texsources = Editorial.tex predicates/Predicates.tex

default: $(issue).pdf

$(issue).tex : $(issue).lhs $(texsources) $(lhssources)
	lhs2TeX $(issue).lhs > $(issue).tex

%.pdf: %.tex force
	pdflatex $<

predicates/Predicates.raw.tex: predicates/Predicates.lhs
	lhs2TeX $< -o $@

predicates/Predicates.tex: predicates/Predicates.raw.tex
	runhaskell predicates/FixupDataKinds.hs predicates/Predicates.raw.tex $@

%.tex: %.lhs
	lhs2TeX $< -o $@

clean:
	rm -f *.log *.aux *.toc *.out *.blg *.bbl *.ptb *~
	rm -f $(issue).tex

# put .bib files here
bib :
#	bibtex articlename

final : $(issue).pdf bib
	pdflatex $(issue).tex
	pdflatex $(issue).tex
	pdflatex $(issue).tex

.PHONY : force
