# On the gateway
# module purge ; module load cineca ; module load imasenv ; module load itm-python/3.6

import imas
import getpass
import numpy as np



def open_ids(     shot       : int, 
                  tokamak    : str,
                  run        : int,
                  user       : str,
                  occurrence : int = 0,
                  ids_name   : str = "equilibrium" ,
                  version    : str = '3' 
            ):

    ids = imas.ids(shot, run)
    ids.open_env(user, tokamak, version)



    idsdata = ids.__dict__[ids_name]
    if 'get' not in dir(idsdata):
        idsdata = ids.__dict__[self.ids_name + 'Array']

    idsdata.get(occurrence)


    return idsdata



def read_b3d(     shot       : int, 
                  tokamak    : str,
                  run        : int,
                  user       : str,
                  occurrence : int = 0,
                  ids_name   : str = "equilibrium" ,
                  version    : str = '3' 
            ):

    idsdata = open_ids(     shot       = shot,
                            tokamak    = tokamak,
                            run        = run,
                            user       = user,
                            occurrence = occurrence,
                            ids_name   = ids_name,
                            version    = version
                        )


    ggd  = idsdata.time_slice[0].ggd[0]
    grid = idsdata.grids_ggd[0].grid[0]

    # Check we have R,phi,z grid:
    if grid.identifier.index != 10:
        raise ValueError("Expecting index 10 in grid identifier, instead of {}.".format(grid.identifier.index))
    if len(grid.space) != 3 :
        raise ValueError("Should be 3-dimensional grid.")
    if grid.space[0].coordinates_type[0] != 4 :
        raise ValueError("Coordinate type mismatch [R]")
    if grid.space[1].coordinates_type[0] != 6 :
        raise ValueError("Coordinate type mismatch [phi]")
    if grid.space[2].coordinates_type[0] != 5 :
        raise ValueError("Coordinate type mismatch [z]")

    
    nR   = len(grid.space[0].objects_per_dimension[0].object)
    nphi = len(grid.space[1].objects_per_dimension[0].object)
    nz   = len(grid.space[2].objects_per_dimension[0].object)
    if nR   < 1 :
        raise ValueError("R is zero length")
    if nphi < 1 :
        raise ValueError("phi is zero length")
    if nz   < 1 :
        raise ValueError("z is zero length")


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

    ldata = nR * nphi * nz
    if  len( ggd.b_field_r[   0 ].values) != ldata :
         raise ValueError("B_R size does not match R,phi,z size") 
    if  len( ggd.b_field_tor[   0 ].values) != ldata :
         raise ValueError("B_tor size does not match R,phi,z size") 
    if  len( ggd.b_field_z[   0 ].values) != ldata :
         raise ValueError("B_z size does not match R,phi,z size") 

    B_R   = np.reshape( ggd.b_field_r[   0 ].values, newshape=shape, order=order )
    B_tor = np.reshape( ggd.b_field_tor[ 0 ].values, newshape=shape, order=order )
    B_z   = np.reshape( ggd.b_field_z[   0 ].values, newshape=shape, order=order )

    if len( ggd.psi ) > 0 :
        if  len( ggd.psi[   0 ].values) != ldata :
            raise ValueError("psi data size does not match R,phi,z size") 
        psi_arr   = np.reshape( ggd.psi[   0 ].values, newshape=shape, order=order )
    else:
        psi_arr   = None

    if len( ggd.phi) > 0 :
        if  len( ggd.phi[   0 ].values) != ldata :
            raise ValueError("phi data size does not match R,phi,z size") 
        phi_arr   = np.reshape( ggd.phi[   0 ].values, newshape=shape, order=order )
    else:
        phi_arr   = None

    if len( ggd.theta) > 0 :
        if  len( ggd.theta[   0 ].values) != ldata :
            raise ValueError("theta data size does not match R,phi,z size") 
        theta_arr = np.reshape( ggd.theta[ 0 ].values, newshape=shape, order=order )
    else:
        theta_arr = None

    return {'R'        : R, 
            'phi'      : phi,
            'z'        : z,
            'B_R_arr'  : B_R,
            'B_tor_arr': B_tor,
            'B_z_arr'  : B_z,
            'psi_arr'  : psi_arr,
            'phi_arr'  : phi_arr,
            'theta_arr': theta_arr,
    }







def print_data_dict(data):


    print( "(nR,nphi,nz)=({},{},{})".format(len(data['R']), len(data['phi']), len(data['z'])) )
    print( "shape of B_R:", data['B_R_arr'].shape)
    
    print('R',      data[ 'R' ])
    print('phi',    data['phi'])
    print('z',      data[ 'z' ])
    print('B_R\n',  data[  'B_R_arr' ])
    print('B_tor\n',data['B_tor_arr' ])
    print('B_z\n',  data[  'B_z_arr' ])
    print('psi\n',  data[  'psi_arr' ])
    print('phi',    data[  'phi_arr' ])
    print('theta\n',data['theta_arr' ])
    print('100x iR + 10x iPhi + iz ')



if __name__ == "__main__":


    shot       = 18
    tokamak    = "ggdtest"
    run        = 3
    version    = "3"
    user       = getpass.getuser()
    occurrence = 0
    ids_name   = "equilibrium"

    data = read_b3d(     shot       = shot, 
                         tokamak    = tokamak,
                         run        = run,
                         user       = user,
                         occurrence = occurrence,
                         ids_name   = ids_name,
                         version    = version, 
            )
    print_data_dict(data)
