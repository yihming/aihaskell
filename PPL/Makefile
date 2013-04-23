
HC      = ghc

HC_OPTS = #-fffi -fglasgow-exts 

HC_OOPTS = 

HSC2HS = hsc2hs

PPL_C_H = /usr/local/include/ppl_c.h

PRG= demo

SRCS= PPL.hs HaskellPPL.hs Main.hs errhandler.c

OBJS =  PPL.o HaskellPPL.o Main.o errhandler.o

.SUFFIXES : .o .hs .hi .lhs .hc .s

#
# uses libmyppl_c.a, libmyppl.a libmygmpxx.a libmygmp.a
# these are modified by the librename.tcl to use a private instance of the GMP
#
LIBS= -lppl_c -lppl -lgmpxx -lgmp -lstdc++

$(PRG) : $(OBJS) 
	rm -f $(PRG)
	$(HC) $(HC_OPTS) -o $(PRG) $(OBJS) $(LIBS)


GenPPL : GenPPL.hs
	$(HC) --make GenPPL.hs -o GenPPL

PPL.hs : PPL.hsc
	$(HSC2HS) PPL.hsc

PPL.hsc : PPL.hsc.in $(PPL_C_H) GenPPL
	cpp $(PPL_C_H) > ppl_c.i
	./GenPPL

# Standard suffix rules
.o.hi:
	@:

.lhs.o:
	$(HC) -c $< $(HC_OPTS) $(HC_OOPTS)

.hs.o:
	$(HC) -c $< $(HC_OPTS) $(HC_OOPTS)

.o-boot.hi-boot:
	@:

.lhs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS) $(HC_OOPTS)

.hs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS) $(HC_OOPTS)

depend:
	$(HC) -M $(HC_OPTS) $(SRCS)


clean: 
	rm -f $(PRG) $(OBJS)



