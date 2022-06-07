#Name - Sheel Nidhan
#Date - 20th June, 2019

programs = main.f90 io_data_files.f90 pchip.f90
files    = main.o io_data_files.o pchip.o

OPT = -O3 
#DEBUG = -g -traceback
EXTRA = -heap-arrays 100000
fcomps= ifort $(OPT) $(EXTRA) $(DEBUG)

# Making the Makefile

output: $(files) 
	$(fcomps) $(INCL) -o output $(files)

io_data_files.o: io_data_files.f90 
	$(fcomps) -c io_data_files.f90

pchip.o: pchip.f90
	$(fcomps) -c pchip.f90

main.o: main.f90 
	$(fcomps) -c main.f90

clean:
	rm -f *.o *.mod output
