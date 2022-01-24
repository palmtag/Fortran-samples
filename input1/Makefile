
FC = gfortran
FFLAGS = -O2 -fbacktrace -Wall -fbounds-check
 
.SUFFIXES :
.SUFFIXES : .f90 .o
 
default: main.exe
 
main.exe: Mod_input.o main.o
	$(FC) $(FFLAGS) $^ -o $@

.f90.o :
	$(FC) $(FFLAGS) -c $*.f90

clean:
	rm -f main.exe *.mod *.o

main.o: Mod_input.o
