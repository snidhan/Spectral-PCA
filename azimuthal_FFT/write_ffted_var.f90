subroutine write_ffted_var(ffted_var, ntheta, nr_trunc, nit, outDIR, basename, modestart, modeend, slice_idx)

    implicit none
    complex (kind=8)    :: ffted_var(nr_trunc, ntheta-2)
    integer (kind=4)    :: nr_trunc, ntheta, modestart, modeend, k, nit
    character (len=160) :: outDIR, basename, slice_idx, filename
    real (kind=8)       :: pr(nr_trunc), pi(nr_trunc)
    
    do k = modestart, modeend

        print*, 'for mode ', k-1
        
        print*, minval(dble(ffted_var(:,k)))
        print*, maxval(dble(ffted_var(:,k)))
        print*, minval(dimag(ffted_var(:,k)))
        print*, maxval(dimag(ffted_var(:,k)))

        pr = dble(ffted_var(:,k))
        pi = dimag(ffted_var(:,k))
    
        write(filename,'(a,a,a,i8.8,a,a,a,i3.3,a)') trim(outDIR), trim(basename), "_", nit, "_", trim(slice_idx), "_m", k-1, ".res"
        open(unit=500, file=filename, status='replace', form='unformatted', action='write')
    
        write(500) pr
        write(500) pi
     
        close(500)

    end do
return
end subroutine
