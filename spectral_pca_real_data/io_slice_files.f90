subroutine io_slice_files(filename, nr, ntheta, nx, pr) 

    implicit none
    character (len=160) :: filename
    integer (kind=4)    :: nr, ntheta, nx, i, j, k
    real (kind=8)       :: pr(nr+2, ntheta+2, nx), pr_full(521,258,1)
    
     open(unit=500, file=filename,  status='old', form='unformatted', access='stream', action='read')
     read(500) pr_full
     close (500)

     pr(1:nr+2,1:ntheta+2,1:nx) = pr_full(1:nr+2,1:ntheta+2,1:nx)
    
     return

end subroutine io_slice_files


