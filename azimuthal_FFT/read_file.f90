subroutine read_file(filename, nr, ntheta, p) 

    implicit none
    character (len=160) :: filename
    integer   (kind=4)  :: nr, ntheta
    real      (kind=8)  :: p(nr,ntheta)         
    
    open(unit=500, file=filename,  status='old', form='unformatted', access='stream', action='read')
    read(500) p
    close (500)
    return

end subroutine read_file


