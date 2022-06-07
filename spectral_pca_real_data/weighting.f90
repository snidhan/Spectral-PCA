subroutine weighting(weightDIR, weightfile, Q_ktemp, Stemp, Nblk, Nrows, Nx, Ny, ntheta, numvar, Fr)


    real    (kind=8),    parameter ::  pi         = 3.141592653589793238462643383279502884197d0   
    character (len = 160)             :: weightDIR, weightfile, filename
    integer   (kind = 4)              :: Nblk, Nrows, ntheta, numvar, i, j, Nx, Ny
    complex   (kind = 8)              :: Q_ktemp(Nrows, Nblk), Stemp(Nblk, Nblk), Q_kweight(Nrows, Nblk)
    real      (kind = 8), allocatable :: W(:)
    real      (kind = 8)              :: Fr

!!!!!!!!!!!! Weight matrix calculation !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    write(filename, '(a,a)') trim(weightDIR), trim(weightfile)  

    allocate (W(Nx))

    open(unit=500, file=filename, status = 'old', form = 'formatted', action = 'read')

    do i = 1, Nx
        read(500, *) W(i)        
    end do

    close(500)  

    W = W*(2*pi)/(ntheta)

    do i = 1, numvar
        if (i .le. 3) then
            do k = 1, Ny
                do j = 1, Nx
                    Q_kweight((i-1)*Nx*Ny + (k-1)*Nx + j, :) = W(j)*Q_ktemp((i-1)*Nx*Ny + (k-1)*Nx + j, :)
                end do
            end do
        elseif (i .eq. 4) then
            do k = 1, Ny
                do j = 1, Nx
                    Q_kweight((i-1)*Nx*Ny + (k-1)*Nx + j, :) = W(j)*Q_ktemp((i-1)*Nx*Ny + (k-1)*Nx + j, :)/(Fr**2)      !! TAPE - Total available
                    !Q_kweight((i-1)*nr*ntheta + j, :) =  Q_ktemp((i-1)*nr*ntheta + j, :)                               !! TAPE - Total available
                                                                                                                        !! potential energy
                                                                                                                        !! Refer Chongsiripinyo and
                                                                                                                        !! Sarkar 2019 
                end do
            end do
        endif

    end do

    Stemp = matmul(transpose(conjg(Q_ktemp)),Q_kweight) 
    
    return
end subroutine weighting
