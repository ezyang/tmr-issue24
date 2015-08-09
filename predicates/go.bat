lhs2TeX Predicates.lhs -o temp.tex
runhaskell FixupDataKinds.hs temp.tex Predicates.tex
del temp.tex
pdflatex Predicates.tex
