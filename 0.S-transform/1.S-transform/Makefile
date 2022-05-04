#!/bin/csh
#
CMP = gfortran
BIN = /vvncodes/bin 
SRC = ./src/
#
MAIN = $(SRC)lsac2ssac.f
OBJ = lsac2ssac.o rwsac.o greg2doy.o
#
#
greg2doy.o: $(SRC)greg2doy.f90
	$(CMP) $(SRC)greg2doy.f90 -c

rwsac.o: $(SRC)rwsac.f
	$(CMP) $(SRC)rwsac.f -c

lsac2ssac.o: $(MAIN)
	$(CMP) $(MAIN)  -c

lsac2ssac: $(OBJ)
	$(CMP) $(OBJ) -o $@
#	cp cube2sac ../
#	cp cube2sac /vvncodes/bin
#

clean:
	rm -f *.o *~ *.ps

