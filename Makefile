LIBS = -lppl_c -lppl -lgmpxx -lgmp -lstdc++

PPL_C_H = /usr/local/include/ppl_c.h

SRCS = AbstractSyntax.hs FrontEnd.hs PPL.hs HaskellPPL.hs

all:	Main.hs errhandler.o $(SRCS)
		rm -f InterProc
		ghc --make -o InterProc Main.hs errhandler.o $(LIBS)


GenPPL:	GenPPL.hs
	ghc --make GenPPL.hs -o GenPPL

errhandler.o:	errhandler.c
	cc -c -o errhandler.o errhandler.c

PPL.hsc: PPL.hsc.in GenPPL $(PPL_C_H)
	cpp $(PPL_C_H) > ppl_c.i
	./GenPPL	

PPL.hs:	PPL.hsc
	hsc2hs PPL.hsc

clean:
	rm -f *~ *.hi *.o InterProc GenPPL PPL.hs PPL.hsc ppl_c.i
