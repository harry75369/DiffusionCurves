all: core

core: Main.hs
	ghc --make -O2 Main

gui: Viewer.hs
	ghc --make Viewer

clean:
	rm -rf *.hi *.o

cleanall: clean
	rm -rf Main Viewer

profile: Main.hs
	ghc --make -O2 -auto-all -caf-all -fforce-recomp -prof Main
