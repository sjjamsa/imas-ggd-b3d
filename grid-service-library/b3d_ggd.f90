module b3d_ggd


  implicit none


contains
  
  subroutine write_b3d( shotnum,runnum,treename, username, machine, time,&
       r, phi, z,  data_Br, data_Bphi, data_Bz, code_name, comment)

    use ids_types           !! IMAS IDS standard Fortran data types, e.g.
                            !! real(IDS_real)
    use ids_schemas         !! IMAS IDS data structure/definitions
    use ids_routines        !! IMAS IDS GGD routines
    use ids_grid_common     !! IMAS IDS grid service library: constant
                            !! definitions like COORDTYPE_R, COORDTYPE_Z
    use ids_assert
    use ids_grid_subgrid
    use ids_grid_objectlist
    use ids_grid_structured
    use ids_grid_data
    use ids_utility


    implicit none

    !! parameters
    !! Set IDS parameters
    integer,           intent(in)  ::  shotNum  
    integer,           intent(in)  ::  runNum 
    character(len=24), intent(in)  ::  treename    !! Treename
    character(len=24), intent(in)  ::  username    !! current login username.
    character(len=24), intent(in)  ::  machine     !! Name of the database/machine
                                                    !! Note: database must exist
                                                    !! before running this example!
    character(len=24), parameter   ::  version  = "3"              !! IMAS major version

    character(len=*), intent(in) :: code_name
    character(len=*), intent(in) :: comment

    !! variables for the grid
    type(ids_equilibrium) ::  equilibrium
    type(ids_generic_grid_dynamic), pointer :: grid

    integer, parameter        :: ndim = 3                       !! Dimensionality of our data
    integer, parameter        :: c1 = COORDTYPE_R               !! Coordinate types of the axes
    integer, parameter        :: c2 = COORDTYPE_PHI
    integer, parameter        :: c3 = COORDTYPE_Z

    real(IDS_real), dimension(:), intent(in) :: r, phi, z   !! Node coordinates on the axies
    integer,        dimension(1), parameter  :: periodicSpaces = (/ 2 /)     ! phi is periodic
    real(IDS_real),  intent(in) :: time 

    integer :: refstatus
    integer :: idx
    logical, parameter :: createGridSubsets = .false.

    !! variables for writing the data
    type(ids_generic_grid_scalar)    :: idsField
    real(IDS_real), dimension(:,:,:), intent(in) :: data_Br, data_Bphi, data_Bz
    integer :: grid_index

    integer, parameter  :: gridSubset_index = 1 !nodes 

    type(ids_generic_grid_scalar), pointer :: B_r, B_phi, B_z


    integer, parameter :: homogenous_time = 1



    !! Preparing database for writing
    !! Through practice it was disclosed that there are some mandatory
    !! steps to be done in order to assure for data to be successfully
    !! written to IDS. Without going through those steps errors and failed
    !! process of writing to IDS are to be expected.
    !! 1. (was 1d-profiles for edge, unknown for equilibrium)
    !! 2. Set homogeneous_time to 0 or 1
    equilibrium%ids_properties%homogeneous_time = homogenous_time
    !! 3. Allocate edge_profiles.time and set it to desired value
    allocate(equilibrium%time(1))
    equilibrium%time(1) = time

    !! Continue to set and fill IDS with data

    !! Set General Grid Description substructure

    allocate( equilibrium%grids_ggd(1) )
    allocate( equilibrium%grids_ggd(1)%grid(1) )
    equilibrium%grids_ggd(1)%time = time
    grid  =>  equilibrium%grids_ggd(1)%grid(1)
 
    allocate( equilibrium%time_slice(1) )
    allocate( equilibrium%time_slice(1)%ggd(1) )
    allocate( equilibrium%time_slice(1)%ggd(1)%b_field_r(1) )
    allocate( equilibrium%time_slice(1)%ggd(1)%b_field_tor(1) )
    allocate( equilibrium%time_slice(1)%ggd(1)%b_field_z(1) )

!    grid  => equilibrium%time_slice(1)%ggd(1)%grid
    B_r   => equilibrium%time_slice(1)%ggd(1)%b_field_r(1)
    B_phi => equilibrium%time_slice(1)%ggd(1)%b_field_tor(1)
    B_z   => equilibrium%time_slice(1)%ggd(1)%b_field_z(1)

    !! Set IDS comment under ids_properties substructure
    allocate( equilibrium%ids_properties%comment(1) )
    !! Fill IDS comment
    equilibrium%ids_properties%comment(1) = &
        &   comment
    !! Set IDS code name
    allocate( equilibrium%code%name(1) )
    !! Fill IDS code name
    equilibrium%code%name(1) = code_name




    !! === Set up grid ===

    !! We are creating a 3D magnetic field in R,phi,z coordinates:






    call gridSetupStructuredSep( &
         grid=grid, ndim=ndim, &
         c1=c1, x1=r, c2=c2, x2=phi, c3=c3, x3=z, &
         id='structured_spaces', &
         periodicSpaces=periodicspaces, createGridSubsets=createGridSubsets )
    grid%identifier%index = 10


    !! === Write some fake scalar data to the cells ===




    grid_index = 1 ! No clue what is the significance of it. One seems to be a safe value, eh?


    ! Will be resolved as gridStructWriteData3d_Dynamic
    call gridStructWriteData( grid, B_r,   grid_index, gridSubset_index, data_Br )
    call gridStructWriteData( grid, B_phi, grid_index, gridSubset_index, data_Bphi )
    call gridStructWriteData( grid, B_z,   grid_index, gridSubset_index, data_Bz )


    ! === Write the equilibrium IDS ===

    write (*,*) "write_b3d_ggd: writing to shot ", shotNum, ", run ", runNum

    write(*,*) "Creating new input IDS"
    write(*,*) "shot: ", shotNum
    write(*,*) "run: ", runNum
    call imas_create_env(treename, shotnum, runNum, 0, 0, idx, username,  &
            & machine, version, refstatus)


    !! Set and write general description of the dataset
    call fillDatasetDescription( idx, &
        &   comment='IDS written by GGD library example write_b3d_ggd.', &
        &   source='Simppa Akaslompolo')


    !! Write data to the database
    write (*,*) "Writing the data to equilibrium IDS: Started."
    call ids_put( idx, "equilibrium", equilibrium )
    !! Close the IDS
    call imas_close( idx )
    !! Deallocate set data
    call ids_deallocate(equilibrium)
    write (*,*) "Writing the data to equilibrium IDS: Finished."



  end subroutine write_b3d

end module b3d_ggd
