BFiles, notes, tests regarding ASCOT5 and IMAS.
This repository is supposed to be the single source of the format of how to read and write the 3D-magnetic field in The ITER Integrated Modelling & Analysis Suite (IMAS) data scheme equilibrium Interface Data Structure (IDS).
This is a work-around because the grid service library (GSL) for Generalised Grid Description (GGD) is available only in Fortran.

In the folder `grid-service-library/` there is 
 * a prototype magnetic field writing with the ggd service library: `write_b3d_ggd.f90` 
 * The routine doing the work is defined in `b3d_ggd.f90`. This could be called from "any" code calculating the magnetic field.

 In the folder `python/` there is
 * an example routine (`b3d_ggd.py`) that reads the magnetic field written by `write_b3d_ggd`.


An example test run on the gateway machine:

```
module purge ; module load cineca ; module load imasenv ; module load itm-python/3.6
cd imas-ggd-b3d/grid-service-library/
make
cd ../python
python b3d_ggd.py

```