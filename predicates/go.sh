lhs2tex Predicates.lhs -o temp.tex
runhaskell FixupDataKinds.hs temp.tex Predicates.tex
rm temp.tex
pdflatex Predicates.tex
