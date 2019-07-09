PROGRAMS =  FizzBuzzEx1  FizzBuzzEx2  FizzBuzzEx3
PROGRAMS += FizzBuzzRev0 FizzBuzzRev1 FizzBuzzRev2
PROGRAMS += FizzBuzzRev3 FizzBuzzRev4 FizzBuzzRev5

COMPILER = ghc
OPTARGS  = -O2
NOPTARGS = -O0

N ?= 10000000

all:
	@printf 'Use "dumb" or "optimised".\n'

dumb:
	@for i in $(PROGRAMS); do $(COMPILER) $(NOPTARGS) "$$i"; done

optimised:
	@for i in $(PROGRAMS); do $(COMPILER) $(OPTARGS) "$$i"; done

run:
	@for i in $(PROGRAMS); do echo "$$i $(N):"; time ./"$$i" $(N) > /dev/null; done

clean:
	rm -f $(PROGRAMS)
	rm -f *.hi *.o *~ "#"*

.PHONY: all clean dumb optimised run
