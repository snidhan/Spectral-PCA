#Name - Sheel Nidhan
#Date - 27th June, 2019


programs = main.f90 io_spod_files.f90 io_slice_files.f90 subtract_qmean.f90 weighting.f90 windowing.f90
files = main.o io_spod_files.o io_slice_files.o subtract_qmean.o weighting.o windowing.o

OPT = -O3
#DEBUG = -g -traceback
EXTRA = -heap-arrays 100000
#INCL= -I/opt/fftw-3.3.8/api
#LIB1 = -L/usr/local/lib -lfftw3 -lm 
#LIB2 = -L/usr/lib/gcc/x86_64-linux-gnu/5.4.0 -lgfortran 
LIB1 = -L/usr/local/lib -lfftw3 -lm
LIB2 =  -L/home/sheel/packages/lapack-3.8.0 -llapack -L/home/sheel/packages/BLAS-3.8.0 -lblas


fcomps= ifort $(OPT) $(EXTRA) $(DEBUG)

# Making the Makefile

output: $(files) 
	$(fcomps) $(INCL) -o output $(files) $(LIB1) $(LIB2)

io_slice_files.o: io_slice_files.f90 
	$(fcomps) -c io_slice_files.f90

io_spod_files.o: io_spod_files.f90 
	$(fcomps) -c io_spod_files.f90

subtract_qmean.o: subtract_qmean.f90 
	$(fcomps) -c subtract_qmean.f90

windowing.o: windowing.f90 
	$(fcomps) -c windowing.f90

weighting.o: weighting.f90
	$(fcomps) -c weighting.f90

main.o: main.f90 
	$(fcomps) -c main.f90


## cleaning
clean: 
	rm -f *.o *.mod *.bin output
