subroutine mpiinit()

!  Call the initialization routines and initialize the process grid
!  in the x-z plane

  use precision_mod
  use mpicomms_mod
  implicit none
  integer, dimension(3) :: intarr, coords
  logical :: reorder
  logical, dimension(3) :: tmplog

  integer :: color, key
  integer :: size_real,size_dp
  integer :: size_complex, size_complex_dp

  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

  print*, "Total number of processors are", numprocs   
  print*, "Produce of px, py, pz", px*py*pz

  if (numprocs.ne.(px*pz*py)) stop ' Number or processors numprocs is not equal to px*py*pz'
    
  ! Set MPI working precision - WP
  call MPI_TYPE_SIZE(MPI_REAL,size_real,ierr)
  call MPI_TYPE_SIZE(MPI_DOUBLE_PRECISION,size_dp,ierr)

  call MPI_TYPE_SIZE(MPI_COMPLEX,size_complex,ierr)
  call MPI_TYPE_SIZE(MPI_DOUBLE_COMPLEX,size_complex_dp,ierr)


  if (WP .eq. size_real) then
     MPI_REAL_WP = MPI_REAL

  else if (WP .eq. size_dp) then
     MPI_REAL_WP = MPI_DOUBLE_PRECISION

  else

     write(*,*) 'Error in mpi_setup: no WP equivalent in MPI'
     call MPI_ABORT(MPI_COMM_WORLD,0,ierr)

  end if
  
  if (WP .eq. size_complex) then
     MPI_COMPLEX_WP = MPI_COMPLEX

  else if (WP .eq. size_complex_dp) then
     MPI_COMPLEX_WP = MPI_DOUBLE_COMPLEX

  else

     write(*,*) 'Error in mpi_setup: no WP equivalent in MPI'
     call MPI_ABORT(MPI_COMM_WORLD,0,ierr)

  end if

  ! Set MPI single precision
  call MPI_TYPE_SIZE(MPI_REAL,size_real,ierr)
  call MPI_TYPE_SIZE(MPI_DOUBLE_PRECISION,size_dp,ierr)

  if (SP .eq. size_real) then
     MPI_REAL_SP = MPI_REAL
     !MPI_COMPLEX_SP = MPI_COMPLEX

  else if (SP .eq. size_dp) then
     MPI_REAL_SP = MPI_DOUBLE_PRECISION
     !MPI_COMPLEX_SP = MPI_DOUBLE_COMPLEX

  else
     write(*,*) 'Error in mpi_setup: no SP equivalent in MPI'
     call MPI_ABORT(MPI_COMM_WORLD,0,ierr)
  end if
  
  ! Now create the 3-D process grid in x, y and z directions
  ! with (px,py,pz) number of processors.
  
  intarr(1) = px
  intarr(2) = py
  intarr(3) = pz
  tmplog(1) = .true.
  tmplog(2) = .true.
  tmplog(3) = .true.
  reorder   = .false.

  call MPI_CART_CREATE(MPI_COMM_WORLD, 3, intarr, tmplog, reorder, MPI_XYZ_COMM, ierr)
  call MPI_CART_GET(MPI_XYZ_COMM, 3, intarr, tmplog, coords, ierr)

  irank  = coords(1)
  jrank  = coords(2)
  krank  = coords(3)

  print*,' Processor indices', myrank, irank, jrank, krank

  return
end subroutine mpiinit

