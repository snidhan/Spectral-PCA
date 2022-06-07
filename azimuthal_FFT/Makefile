#Name - Sheel Nidhan
#Date - 30th June, 2019

programs = main.f90 read_file.f90 fftpack5.1d.f90 write_ffted_var.f90
files = main.o read_file.o fftpack5.1d.o write_ffted_var.o

OPT = -O3 
#DEBUG = -g -traceback
EXTRA = -heap-arrays 100000

fcomps= ifort $(OPT) $(EXTRA) $(DEBUG)

# Making the Makefile

output: $(files) 
	$(fcomps) $(INCL) -o output $(files) $(LIB1) $(LIB2) $(LIB3)

read_file.o: read_file.f90 
	$(fcomps) -c read_file.f90

write_ffted_var.o: write_ffted_var.f90 
	$(fcomps) -c write_ffted_var.f90

fftpack5.1d.o: fftpack5.1d.f90 
	$(fcomps) -c fftpack5.1d.f90


main.o: main.f90 
	$(fcomps) -c main.f90

clean: 
	rm -f *.o *.mod *.bin output 
                                       
