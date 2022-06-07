subroutine io_spod_files(Lambda, Lambda_invsq, Eigen_V, Q_k, outDIR, Nrows, Nblk, Nfreq, mode)

    implicit none
    character (len = 160) :: outDIR, filename, basename
    complex (kind = 8)    :: Eigen_V(Nblk, Nblk, Nfreq), Lambda(Nblk, Nblk, Nfreq), Lambda_invsq(Nblk, Nblk, Nfreq), Q_k(Nrows, Nblk, Nfreq)
    integer (kind = 4)    :: Nrows, Nblk, Nfreq, i, k, j, mode
    complex (kind = 8)    :: Psi(Nrows, Nblk, Nfreq), Psi_temp(Nblk, Nblk)

    !!!!!!! Writing out eigenvalues of SPOD!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    print*, 'Writing out the eigenvalues to text files'
    print*, Nrows, Nblk, Nfreq

    do i = 1, Nfreq
        basename = 'eigenvalues_freq'
        write(filename,'(a,a,i4.4,a,i3.3,a)') trim(outDIR), trim(basename)//"_", i, "_", mode, ".txt"
        open(unit=500, file=filename,  status='replace', form='formatted', access='stream', action='write')
            do j = 1, Nblk
                write(500,*) dble(Lambda(j,j,i)), dimag(Lambda(j,j,i))
            end do
        close(500) 
    end do
    
    print*, 'Written out the eigenvalues to text files'
    
    !!!!!!!!!!! Obtaining SPOD modes from CSD !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do i = 1, Nfreq    
       Psi_temp   =   matmul(Eigen_V(:,:,i), dble(Lambda_invsq(:,:,i))) 
       Psi(:,:,i) =   matmul(Q_k(:,:,i), Psi_temp)    
       !call cgemm('N','N', Nblk, Nblk, Nblk, alpha,Eigen_V(:,:,i), Nblk, real(Lambda_invsq(:,:,i)),Nblk, 0, C, Nblk)         
       !call cgemm('N','N', Nrows, Nblk, Nblk, alpha1,Q_k(:,:,i),Nrows, C, Nblk,0, Psi(:,:,i), Nrows)         
    end do    
    
    Psi=Psi/(sqrt(Nblk*1.0))

    !!!!!!!!! Writing out the modes of SPOD for the axisymmetric case !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
    print*, 'Writing out the eigenmodes to text files'
   

    do k = 1, Nfreq
        basename = 'eigenmode'
        write(filename,'(a,a,a,i4.4,a,i3.3,a)') trim(outDIR), trim(basename)//"_","freq_", k, "_", mode, ".mod"
        open(unit=500, file=filename,  status='replace', form='formatted', access='stream', action='write')
        do j = 1, Nblk
            do i = 1, Nrows
                write(500,*) dble(Psi(i,j,k)), dimag(Psi(i,j,k))
             end do
        end do
        close(500)
    end do
    print*, 'Written out the eigenmodes to text files'

return
end subroutine io_spod_files
