subroutine hamming_window(N, window)
            
    implicit none
    real (kind=8), parameter ::  pi=3.1415926535897932              
    real (kind=8) :: window(N)
    integer (kind=4) :: i, N
            
    do i=1,N
        window(i)=0.54-0.46*cos(2*pi*(i-1)/(N-1))
    end do
    
    return
end subroutine   





