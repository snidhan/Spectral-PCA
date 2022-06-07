subroutine weighting(weightDIR, weightfile, Q_ktemp, Stemp, Nblk, Nrows, nr, numvar)

    character (len = 160)             :: weightDIR, weightfile, filename
    integer   (kind = 4)              :: Nblk, Nrows, nr, numvar, i, j
    complex   (kind = 8)              :: Q_ktemp(Nrows, Nblk), Stemp(Nblk, Nblk), Q_kweight(Nrows, Nblk)
    real      (kind = 8), allocatable :: W(:)
     

!!!!!!!!!!!! Weight matrix calculation !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
    write(filename, '(a,a)') trim(weightDIR), trim(weightfile)  

    allocate (W(nr))

    open(unit=500, file=filename, status = 'old', form = 'formatted', action = 'read')

    do i = 1, nr
        read(500, *) W(i)        
    end do

    close(500)  

    do i = 1, numvar
        do j = 1, nr
            Q_kweight((i-1)*nr + j, :) = W(j)*Q_ktemp((i-1)*nr + j, :)
        end do
    end do

    Stemp = matmul(transpose(conjg(Q_ktemp)),Q_kweight) 
    
    return
end subroutine weighting
