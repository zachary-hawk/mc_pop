FCFLAGS= -O3  -funroll-loops -p -g 
BUILD_DIR ?= ./../build
current_dir = '"'$(shell pwd)'"'




#Edit this line to Toggle between "mpi" and "serial" 
#COMMS_ARCH:=mpi



ifeq ($(COMMS_ARCH),mpi)

mc_pop.mpi: pop.o io.o trace.o comms_mpi.o life.o
	$(MPI_F90) -o mc_pop.mpi  pop.o io.o trace.o comms_mpi.o life.o $(FCFLAGS)
# Here are the compile steps

pop.o:pop.f90 io.o trace.o comms_mpi.o life.o
	$(MPI_F90) -c  pop.f90 $(FCFLAGS)

io.o:./io.f90 trace.o comms_mpi.o
	$(MPI_F90) -c  ./io.f90 -cpp -D life_dir=$(current_dir) $(FCFLAGS)

trace.o:./trace.f90
	$(MPI_F90) -c  ./trace.f90 $(FCFLAGS)

comms_mpi.o:./comms_mpi.f90 trace.o
	$(MPI_F90) -c  ./comms_mpi.f90 $(FCFLAGS)

life.o:./life.f90 comms_mpi.o io.o trace.o
	$(MPI_F90) -c  ./life.f90 $(FCFLAGS)


endif


ifeq ($(COMMS_ARCH),serial)

mc_pop.serial: pop.o io.o trace.o comms_serial.o life.o
	$(F90) -o mc_pop.serial  pop.o io.o trace.o comms_serial.o life.o 
# Here are the compile steps

pop.o:pop.f90 io.o trace.o comms_serial.o life.o
	$(F90) -c  pop.f90

io.o:./io.f90 trace.o comms_serial.o
	$(F90) -c  ./io.f90

trace.o:./trace.f90
	$(F90) -c  ./trace.f90

comms_serial.o:./comms_serial.f90 trace.o
	$(F90) -c  ./comms_serial.f90

life.o:./life.f90 comms_serial.o io.o trace.o
	$(F90) -c  ./life.f90

mandelbrot.serial: Mandelbrot.o fractal.o colour.o IO.o COMMS_SERIAL.o trace.o
	$(SERIAL_FC) -o mandelbrot.serial  Mandelbrot.o fractal.o colour.o IO.o COMMS_SERIAL.o trace.o  $(FCFLAGS)


endif
