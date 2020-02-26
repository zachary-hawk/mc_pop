#########################################
#   TOP LEVEL MAKEFILE                  #
#########################################

SOURCE = ./Source
BUILD_DIR = ./Build

#########################################
#User editable options

#Communications architectiure: mpi,serial
COMMS_ARCH:=mpi

MPI_F90:=mpif90

SERIAL_F90:=gfortran

########################################

export COMMS_ARCH
export MPI_F90
export SERIAL_F90

ifeq ($(COMMS_ARCH),mpi)


subsystem:
	export $(COMMS_ARCH)
	$(MAKE) -C $(SOURCE)
	install -m 557 $(SOURCE)/mc_pop.mpi $(BUILD_DIR)
	rm -f $(SOURCE)/mc_pop.mpi

.phony: install

clean:
	rm -f $(SOURCE)/*.o $(objects)  $(BUILD_DIR)/mc_pop.mpi $(SOURCE)/*.mod



endif 


ifeq ($(COMMS_ARCH),serial)


subsystem:
	export $(COMMS_ARCH)
	$(MAKE) -C $(SOURCE)
	install -m 557 $(SOURCE)/mc_pop.serial $(BUILD_DIR)
	rm -f $(SOURCE)/mc_pop.serial
.phony: install



clean:
	rm -f $(SOURCE)/*.o $(objects)  $(SOURCE)/*.mod


endif
clean_all:
	rm -f $(BUILD_DIR)/mc_pop.* $(SOURCE)/*.o $(objects)  $(SOURCE)/*.mod

dist:
	tar  --exclude="./.git" --exclude="./test" --exclude="./*/*.mpi" --exclude="./*/*.serial" --exclude="./Source/*.o" --exclude="./Source/*.mod" -cvf MC_POP.tar .

