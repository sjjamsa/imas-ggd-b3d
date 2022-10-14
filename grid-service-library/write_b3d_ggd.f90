!-------------------------------------------------------------------------------

!

!> DESCRIPTION:
!> @authors: Simppa Akaslompolo 
!>
!> This example is modified from
!>
!> From https://git.iter.org/scm/imex/ggd.git
!>      examples/f90/ids_grid_example14_2dstructured_servicelibrary.f90
!>
!> and it demonstrates writing a 3D magnetic field in IDS.
!>
!-------------------------------------------------------------------------------

program write_b3d_ggd

  use b3d_ggd
  use ids_types

  implicit none

  integer, parameter  ::  shotNum  = 16
  integer, parameter  ::  runNum   = 2
  character(len=24)   ::  treename = "ids"        !! Treename
  character(len=24)   ::  username                !! current login username.
                                                  !! Will be provided with
                                                  !! getlog() Fortran routine.

  character(len=24)   ::  machine  = "ggdtest"    !! Name of the database/machine
                                                  !! Note: database must exist
                                                  !! before running this example!
  character(len=24)   ::  version  = "3"          !! IMAS major version


  real(IDS_real), dimension(:), allocatable :: r, phi, z   !! Node coordinates on the axies
  real(IDS_real), dimension(:,:,:), allocatable :: data_Br, data_Bphi, data_Bz, data_phi, data_psi, data_theta
  real(IDS_real), dimension(:), allocatable :: data_r_axis, data_z_axis 
  real(IDS_real) :: time 

  real(ids_real), parameter :: PI = 4.0_ids_real * ATAN(1.0_ids_real)

  integer :: i1, i2, i3

  call getlog(username)

  time = 2.2222

  allocate( r(3) )
  allocate( phi(4) )
  allocate( z(5) )
  
  ! R
  r = (/ 3.0, 4.0, 5.0 /)
  ! phi in radians. In this case we have a 40 degree periodic module. The last index is not repeated
  phi = (/ 0.0, 10.0, 20.0, 30.0 /)
  phi = phi * 2.0 * pi / 360.0
  ! z
  z = (/ -2.0, -1.0, 0.0, 1.0, 2.0 /)
  
  allocate( data_Br(   size(r), size(phi), size(z) ) )
  allocate( data_Bphi( size(r), size(phi), size(z) ) )
  allocate( data_Bz(   size(r), size(phi), size(z) ) )
  allocate( data_phi(  size(r), size(phi), size(z) ) )
  allocate( data_psi(  size(r), size(phi), size(z) ) )
  allocate( data_theta(size(r), size(phi), size(z) ) )
  allocate( data_r_axis( size(phi) ), data_z_axis( size(phi) ) )
  do i2=1,size(phi)
     do i1=1,size(r)
        do i3=1,size(z)
           data_Br(   i1,i2,i3) = 100.0 * i1 + 10.0 * i2 + 1.0 * i3 + 0.10
           data_Bphi( i1,i2,i3) = 100.0 * i1 + 10.0 * i2 + 1.0 * i3 + 0.20
           data_Bz(   i1,i2,i3) = 100.0 * i1 + 10.0 * i2 + 1.0 * i3 + 0.30
           data_psi(  i1,i2,i3) = 100.0 * i1 + 10.0 * i2 + 1.0 * i3 + 0.50
           data_phi(  i1,i2,i3) = 100.0 * i1 + 10.0 * i2 + 1.0 * i3 + 0.60
           data_theta(i1,i2,i3) = 100.0 * i1 + 10.0 * i2 + 1.0 * i3 + 0.70
        end do
     end do
     data_r_axis( i2) =   i2 + 0.10 
     data_z_axis( i2) =   i2 + 0.30 
  end do
    
  call write_b3d( shotnum,runnum,treename, username, machine, time,&
       r, phi, z,  data_Br, data_Bphi, data_Bz, code_name='write_b3d_ggd.f90', comment='writing B3D with grid service library', &
       data_psi=data_psi, data_phi=data_phi, data_theta=data_theta, &
         data_r_axis=data_r_axis, data_z_axis=data_z_axis)


end program write_b3d_ggd

