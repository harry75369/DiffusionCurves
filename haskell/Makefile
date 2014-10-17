all: main

main: main.hs
	ghc --make main

csv: csv.hs
	ghc --make csv

clean:
	rm -rf *.hi *.o main.svg main.png main.pdf

cleanall: clean
	rm -rf main csv
