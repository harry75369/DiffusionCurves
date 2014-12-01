all: core

core: Main.hs
	ghc --make Main

gui: Viewer.hs
	ghc --make Viewer

clean:
	rm -rf *.hi *.o

cleanall: clean
	rm -rf Main Viewer
