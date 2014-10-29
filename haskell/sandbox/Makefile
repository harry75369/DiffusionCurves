all: csv diagrams

csv: csv.hs
	ghc --make csv

diagrams: diagrams.hs
	ghc --make diagrams

clean:
	rm -rf *.hi *.o

cleanall: clean
	rm -rf csv diagrams
	rm -rf *.svg *.png *.pdf
