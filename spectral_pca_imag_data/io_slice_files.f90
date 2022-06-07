subroutine io_slice_files(filename, nr, ntheta, nx, pr, pi) 

    implicit none
    character (len=160) :: filename
    integer (kind=4)    :: nr, ntheta, nx, i, j, k
    real (kind=8)       :: pr(nr, ntheta, nx), pi(nr, ntheta, nx)

    open(unit=500, file=filename, status='old', form='unformatted', action='read')
    read(500) pr
    read(500) pi
    close (500)

    return
end subroutine io_slice_files


