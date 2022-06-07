   
!***********************************************************************
program main
! ***********************************************************************
!   Sheel Nidhan
!   Based on Sayadi and Schmid TCFD 
!   Program for testing the compact scheme using the parallel tridiagonal
!   solver from ScaLAPACK
! ***********************************************************************
 
    implicit none
 
    call mpiinit()
  
    ! Preprocessing the data
    call postproc_spod()
  
    stop 
 
end program main

