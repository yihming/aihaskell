all:	AbstractSyntax.hs FrontEnd.hs Main.hs
	ghc --make -o Interproc Main.hs
remove:
	rm *.o *.hi Interproc