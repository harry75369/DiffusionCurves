all: main

main: main.hs
	ghc --make main

csv: csv.hs
	ghc --make csv

clean:
	rm -rf *.hi *.o

cleanall: clean
	rm -rf main csv
