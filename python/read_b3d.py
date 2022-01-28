# coding: utf-8

# module purge ; module load cineca ; module load imasenv

import b3d_ggd
import numpy as np
idsdata = b3d_ggd.open_b3d_example()

ggd  = idsdata.time_slice[0].ggd[0]
grid = idsdata.grids_ggd[0].grid[0]

# Check we have R,phi,z grid:
if grid.identifier.index != 10:
    print("Expecting index 10 in grid identifier, instead of {}.".format(grid.identifier.index) )
if len(grid.space) != 3 :
    print("Should be 3-dimensional grid.")
if grid.space[0].coordinates_type[0] != 4 :
    print("Coordinate type mismatch [R]")
if grid.space[1].coordinates_type[0] != 6 :
    print("Coordinate type mismatch [phi]")
if grid.space[2].coordinates_type[0] != 5 :
    print("Coordinate type mismatch [z]")


nR   = len(grid.space[0].objects_per_dimension[0].object)
nphi = len(grid.space[1].objects_per_dimension[0].object)
nz   = len(grid.space[2].objects_per_dimension[0].object)

R   = np.zeros(shape=(  nR, ) )
phi = np.zeros(shape=(nphi, ) )
z   = np.zeros(shape=(  nz, ) )

for i in range(nR):
    R[i]   = grid.space[0].objects_per_dimension[0].object[i].geometry[0]
for i in range(nphi):
    phi[i] = grid.space[1].objects_per_dimension[0].object[i].geometry[0]
for i in range(nz):
    z[i]   = grid.space[2].objects_per_dimension[0].object[i].geometry[0]

shape = (nR, nphi, nz)
order = 'F'

B_R   = np.reshape( ggd.b_field_r[   0 ].values, newshape=shape, order=order )
B_tor = np.reshape( ggd.b_field_tor[ 0 ].values, newshape=shape, order=order )
B_z   = np.reshape( ggd.b_field_z[   0 ].values, newshape=shape, order=order )

print( "(nR,nphi,nz)=({},{},{})".format(nR,nphi,nz) )
print( "shape of B_R:", B_R.shape)

print('R', R )
print('phi',phi)
print('z', z )
print('B_R\n',B_R)
print('B_tor\n',B_tor)
print('B_z\n',B_z)
print('100x iR + 10x iPhi + iz ')
