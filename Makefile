all:	AbstractSyntax.hs FrontEnd.hs Main.hs
	ghc --make -Wall -o Interproc Main.hs
clean:
	rm *.o *.hi Interproc