module precision_mod
  implicit none
  integer, parameter  :: SP = kind(1.0)                  ! 4 byte storage
  integer, parameter, private :: DP = kind(1.0d0)        ! 8 byte storage
  integer, parameter  :: WP = DP                         ! WP = 8

end module precision_mod


!=======================================================================
module mpidefs_mod
!  MPI parameters like number of processors (px,py,pz) and number of
!  grid points in each processor (Nxp,Nyp,Nzp). Note that the total
!  number of processors numprocs=px*py*pz
!
!  (px,py,pz)    = Number of processors in x,y, and z directions
!                  (py=1)
!  (Nxp,Nyp,Nzp) = Number of grid points in x,y, and z directions
!                  in the local processor
!  numprocs      = Total number of processors (obtained from the -np
!                  flag in mpirun).  This must equal px*py*pz
!  myrank        = Rank of local processor

     include 'mpif.h'
     integer, parameter   :: px=1, py=4, pz=1
     integer              :: myrank, numprocs
     integer              :: irank, jrank, krank
     integer              :: ierr, status(MPI_STATUS_SIZE)   ! status is an array of size MPI_STATUS_SIZE
     integer              :: MPI_REAL_WP, MPI_REAL_SP        ! MPI_REAL == 32 bit floating point 
     integer              :: MPI_COMPLEX_WP, MPI_COMPLEX_SP
end module mpidefs_mod


!======================================================================= 
!=======================================================================
module mpicomms_mod
    use mpidefs_mod
    integer :: MPI_X_COMM, MPI_Z_COMM, MPI_XYZ_COMM
end module mpicomms_mod


!=======================================================================
!=======================================================================
module intcoeffs_mod
  use precision_mod
  real(WP) :: alfa_int, p_int, q_int
  real(WP) :: alfahalfR, ahalfR, bhalfR, chalfR, dhalfR
  real(WP) :: alfa3halfR, q3halfR, p3halfR
  
  real(WP) :: alfazeroL, azeroL, bzeroL, czeroL, dzeroL
  real(WP) :: alfaoneL, poneL, qoneL
end module intcoeffs_mod



!=======================================================================
module dercoeffs_mod
  use precision_mod  
  
  real(WP) :: alfa_intd, p_intd, q_intd
  real(WP) :: alfahalfRd, ahalfRd, bhalfRd, chalfRd, dhalfRd
  real(WP) :: alfa3halfRd, q3halfRd, p3halfRd
  
  real(WP) :: alfazeroLd, azeroLd, bzeroLd, czeroLd, dzeroLd
  real(WP) :: alfaoneLd, poneLd, qoneLd
  real(WP) :: alfatwoLd, ptwoLd, qtwoLd
  
  
end module dercoeffs_mod


!=======================================================================
