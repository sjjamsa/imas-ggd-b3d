module b3d_ggd


  implicit none


contains
  
  subroutine write_b3d( shotnum,runnum,treename, username, machine, time, &
       r, phi, z,  data_Br, data_Bphi, data_Bz, code_name, comment, &
       data_psi, data_phi, data_theta, &
       data_r_axis, data_z_axis)

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
    type(ids_generic_grid_dynamic), pointer :: grid, grid_axis

    integer, parameter        :: ndim = 3                       !! Dimensionality of our data
    integer, parameter        :: c1 = COORDTYPE_R               !! Coordinate types of the axes
    integer, parameter        :: c2 = COORDTYPE_PHI
    integer, parameter        :: c3 = COORDTYPE_Z

    real(IDS_real), dimension(:), intent(in) :: r, phi, z   !! Node coordinates on the axies
    integer,        dimension(1), parameter  :: periodicSpaces = (/ 2 /)     ! phi is periodic
    real(IDS_real),  intent(in) :: time 

    integer :: refstatus, status
    integer :: idx
    logical, parameter :: createGridSubsets = .true.

    !! variables for writing the data
    type(ids_generic_grid_scalar)    :: idsField
    real(IDS_real), dimension(:,:,:), intent(in) :: data_Br, data_Bphi, data_Bz
    real(IDS_real), dimension(:,:,:), intent(in), optional :: data_psi   !! Values of the poloidal flux, given on various grid subsets [Wb]
    real(IDS_real), dimension(:,:,:), intent(in), optional :: data_phi   !! Values of the toroidal flux, given on various grid subsets [Wb]
    real(IDS_real), dimension(:,:,:), intent(in), optional :: data_theta !! Values of the poloidal angle, given on various grid subsets [rad]
    real(IDS_real), dimension(:),     intent(in), optional :: data_r_axis, data_z_axis  !! Magnetic axis coordinates as a function of phi [m]
    integer :: grid_index

    integer, parameter  :: gridSubset_index = 1 !nodes 

    type(ids_generic_grid_scalar), pointer :: B_r, B_phi, B_z, phi_gs, psi_gs, theta_gs


    integer, parameter :: homogenous_time = 1

    logical :: axis_given
    

    axis_given = present(data_r_axis) .and. present(data_z_axis) 

    
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
    if ( axis_given) then
       allocate( equilibrium%grids_ggd(1)%grid(2) )
       grid_axis => equilibrium%grids_ggd(1)%grid(2)

    else
       allocate( equilibrium%grids_ggd(1)%grid(1) )
    end if
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




    grid_index = grid%identifier%index ! No clue what is the significance of it. Still needs to match the grid.


    ! Will be resolved as gridStructWriteData3d_Dynamic
    call gridStructWriteData( grid, B_r,   grid_index, gridSubset_index, data_Br )
    call gridStructWriteData( grid, B_phi, grid_index, gridSubset_index, data_Bphi )
    call gridStructWriteData( grid, B_z,   grid_index, gridSubset_index, data_Bz )

    if ( present(data_psi)   ) then
       allocate( equilibrium%time_slice(1)%ggd(1)%psi(1) )
       psi_gs    => equilibrium%time_slice(1)%ggd(1)%psi(1)
       call gridStructWriteData( grid, psi_gs,     grid_index, gridSubset_index, data_psi   )
    end if
    if ( present(data_phi)   ) then
       allocate( equilibrium%time_slice(1)%ggd(1)%phi(1) )
       phi_gs    => equilibrium%time_slice(1)%ggd(1)%phi(1)
       call gridStructWriteData( grid, phi_gs,     grid_index, gridSubset_index, data_phi   )
    end if
    if ( present(data_theta) ) then 
       allocate( equilibrium%time_slice(1)%ggd(1)%theta(1) )
       theta_gs  => equilibrium%time_slice(1)%ggd(1)%theta(1)
       call gridStructWriteData( grid, theta_gs,   grid_index, gridSubset_index, data_theta )
    end if


    if ( axis_given ) then
       grid_index = 2 
       call write_axis_ggd( grid_axis, grid_index, phi, data_r_axis, data_z_axis, status )
       if (status /= 0 ) write(*,*) 'Could not save magnetic axis to ggd.'
    end if



    
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

  subroutine write_axis_ggd(grid,grid_index,phi,data_r_axis,data_z_axis, status)

    use ids_types           !! IMAS IDS standard Fortran data types, e.g.
                            !! real(IDS_real)
    use ids_schemas         !! IMAS IDS data structure/definitions
    use ids_grid_common     !! IMAS IDS grid service library: constant
                            !! definitions like COORDTYPE_R, COORDTYPE_Z


    implicit none

    real(IDS_real), dimension(:),     intent(in) :: data_r_axis, data_z_axis  !! Magnetic axis coordinates as a function of phi [m]
    real(IDS_real), dimension(:),     intent(in) :: phi !! Node coordinates on the axis (geometrical toroidal angle)
    type(ids_generic_grid_dynamic), pointer :: grid

    integer, parameter        :: ndim = 3                       !! Dimensionality of our data
    integer, parameter        :: c1 = COORDTYPE_R               !! Coordinate types of the axes
    integer, parameter        :: c2 = COORDTYPE_PHI
    integer, parameter        :: c3 = COORDTYPE_Z

    integer, intent(in) :: grid_index
    integer, intent(out) :: status

    
    integer :: i,n

    status = 0
    
    n = size(phi)
    if ( size(data_r_axis) /= n .or. size(data_z_axis) /= n ) then 
       write(*,*) "size of magnetix axis R and z data must match the size of the phi-data"
       status = 1
       return
    end if
    
    grid % identifier % index       = 1 ! linear
    grid % identifier % name        = "magnetic axis"
    grid % identifier % description = "The R,z points as a function of toroidal angle"

    ! We have a one-dimensional, toroidal direction space.
    allocate( grid % space(1) )

    grid % space(1) % identifier % index = 1 ! Primary space defining the standard grid

    grid % space(1) % identifier % name = "toroidal angle" 

    allocate ( grid % space(1) % coordinates_type(ndim) )
    grid % space(1) % coordinates_type(1) = c1
    grid % space(1) % coordinates_type(2) = c2
    grid % space(1) % coordinates_type(3) = c3

    
    allocate ( grid % space(1) % objects_per_dimension(2) ) ! one for nodes, one for edges

    ! Save nodes
    !--------------
    allocate ( grid % space(1) % objects_per_dimension(1) % object(n) )
    
    do i=1,n
       allocate ( grid % space(1) % objects_per_dimension(1) % object(i) % geometry(ndim) )
       grid % space(1) % objects_per_dimension(1) % object(i) % geometry(1:3) = [data_r_axis(i), phi(i), data_z_axis(i) ]
       allocate ( grid % space(1) % objects_per_dimension(1) % object(i) % nodes(1) )
       grid % space(1) % objects_per_dimension(1) % object(i) % nodes(1) = i
    end do

    ! Save edges
    !--------------
    allocate ( grid % space(1) % objects_per_dimension(2) % object(1) )
    allocate ( grid % space(1) % objects_per_dimension(2) % object(1) % nodes(n) )
    do i=1,n
       grid % space(1) % objects_per_dimension(1) % object(1) % nodes(i) = i
    end do

    ! Mark the subset as magnetic axis
    !--------------
    allocate ( grid % grid_subset(1) )
    grid % grid_subset(1) % identifier % index = 100 !magnetic_axis
    grid % grid_subset(1) % dimension = 2                          ! dimension leaf (INT_0D), defining dimension of the grid subset elements,
    allocate ( grid % grid_subset(1) % element(1) )
    allocate ( grid % grid_subset(1) % element(1) % object (1) )   ! We only have one edge,
    grid % grid_subset(1) % element(1) % object(1) % space = 1     ! ...
    grid % grid_subset(1) % element(1) % object(1) % dimension = 2 ! which is in the list of edges.
    

    grid % grid_subset(1) % element(1) % object (1) % index  = 1 

   
  end subroutine write_axis_ggd


  
end module b3d_ggd
