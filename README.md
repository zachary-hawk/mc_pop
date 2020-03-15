# MC_POP 
A programme for Monte Carlo population simulations

MC_POP is a fully parallel program using the Message Passing Interface (MPI). It is written in modern fortran.

Prerequisites
=============
- Modern fortran compiler (tested with ifort and gfortran)
- MPI libraries (a serial version can be compiled if these are unavailible)
- Python 3 for tools


Running a Calculation
=====================

MC_POP reads a parameters file called params.pop. Parameters are colon or equals separated. The executable is names 'mc_pop.mpi' or 'mc_pop.serial' for the parallel and serial version respectively; it is installed in the Build/ direactory, this should be added to the PATH to be availible system wide.

Analysis
========

Two analysis scripts are provided in the Build/ directory, population.py and demographics.py. Standard libraries are required for each with the addition for the plotly.offline for deomgraphics.py.