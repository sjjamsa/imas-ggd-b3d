program write
  use hdf5
  use h5lt
  implicit none



  
  CHARACTER(LEN=*), PARAMETER :: filename = "file.h5" 
  CHARACTER(LEN=*), PARAMETER :: ds1 = "ds1" 

  INTEGER(HID_T) :: file_id 
  INTEGER(HID_T) :: ds1_id  
  INTEGER(HID_T) :: dsp1_id 

  integer, parameter :: nR=150, nPhi=180, nZ=201, nn=4

  integer, parameter :: rank = 4
  INTEGER(HSIZE_T), DIMENSION(rank), parameter :: dims = (/nR,nPhi,nZ,nn/)

  double precision, DIMENSION(nR,nPhi,nZ,nn) :: data

  integer :: i,j,k,m
  INTEGER :: error

  write(*,*) 'hello world!'

  do i=1,nR
     do j=1,nPhi
        do k=1,nZ
           do m=1,nn
              data(i,j,k,m) = real(i-1) + real(j-1) + real(k-1) + real(m-1)
           end do
        end do
     end do
  end do

  CALL h5open_f(error)   
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
        
  call h5ltmake_dataset_double_f(file_id, ds1, rank, dims, data, error)
  CALL h5fclose_f(file_id, error)
  call h5close_f(error)

end program write
