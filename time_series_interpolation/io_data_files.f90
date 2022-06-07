subroutine io_data_files(inDIR,outDIR,slice_idx,nr,ntheta,Nrows,nstart,nend,stride,N,vartype,var_data,io_type)

    implicit none

    character (len=160) :: inDIR, outDIR, vartype, slice_idx, filename
    integer (kind=4)    :: nr, ntheta, nstart, nend, stride, N, Nrows, namef, i, j, io_type
    real (kind=8)       :: var_data(N,Nrows), p(nr,ntheta) 
              

    if (io_type .eq. 0) then 
        do i = 1,N
            print*, 'File number ', i            
            namef = nstart + (i-1)*stride
            write(filename,'(a,a,i8.8,a,a,a)') trim(inDIR),trim(vartype)//"_",namef,"_",trim(slice_idx), ".res"
            open(unit=500, file=filename,  status='old', form='unformatted', access='stream', action='read')
            read(500) p
            close (500)

            do j=1,ntheta
                var_data(i, 1 + nr*(j-1):nr*j) = p(1:nr,j)
            end do
        end do

    elseif (io_type .eq. 1) then

        do i = 1, N
            print*, 'File number ', i, N            
            namef = nstart + (i-1)*stride                
            
            do j = 1, ntheta
                p(1:nr,j) = var_data(i, 1 + nr*(j-1):nr*j)
            end do
            
            write(filename,'(a,a,i8.8,a,a,a,a)') trim(outDIR),trim(vartype)//"_",namef,"_",trim(slice_idx), "_uniform_pchip",".res"
            open(unit=500, file=filename,  status='replace', form='unformatted', access='stream', action='write')
            write(500) p
            close (500)

        end do    

    end if

    return
end subroutine io_data_files

