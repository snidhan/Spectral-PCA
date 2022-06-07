subroutine subtract_qmean(Q, Nrows, N, Q_mean, Q_sub)

    implicit none
 
    integer (kind=4) ::  Nrows, N, i, j, k
    real    (kind=8) ::  Q(Nrows, N), Q_mean(Nrows), Q_sub(Nrows, N)
        
    !! Intitializing the matrix 
    Q_mean = 0.0d0

    do i = 1, Nrows
        do j = 1, N
            Q_mean(i) = Q_mean(i) + Q(i,j)   
        end do 
    end do
        
    Q_mean = Q_mean/(N*1.0)

    do i = 1, Nrows
        do j = 1, N
            Q_sub(i,j) = Q(i,j) - Q_mean(i)
        end do 
    end do
    
    return
end subroutine subtract_qmean


