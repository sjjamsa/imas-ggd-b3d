

# module purge ; module load cineca ; module load imasenv


"""
  integer, parameter  ::  shotNum  = 15
  integer, parameter  ::  runNum   = 2
  character(len=24)   ::  treename = "ids"        !! Treename
  character(len=24)   ::  username                !! current login username.
                                                  !! Will be provided with
                                                  !! getlog() Fortran routine.

  character(len=24)   ::  machine  = "ggdtest"    !! Name of the database/machine
                                                  !! Note: database must exist
                                                  !! before running this example!
  character(len=24)   ::  version  = "3"          !! IMAS major version
"""

def open_b3d_example():

    import imas
    import getpass
    from imas import imasdef

    shot       = 16
    tokamak    = "ggdtest"
    run        = 2
    version    = "3"
    user       = getpass.getuser()
    occurrence = 0
    ids_name   = "equilibrium"

    ids = imas.ids(shot, run)
    ids.open_env(user, tokamak, version)



    idsdata = ids.__dict__[ids_name]
    if 'get' not in dir(idsdata):
        idsdata = ids.__dict__[self.ids_name + 'Array']
        idsdata.get(occurrence)
        #if idsdata.array:
        #    for slice in idsdata.array:
        #        print(slice)
    else:
        idsdata.get(occurrence)


    return idsdata
