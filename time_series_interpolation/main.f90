! Writtem by Sheel Nidhan
! Date 20th 
! Interpolation of non-uniform time interval files to uniform interval files

!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT COMMENTS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 1. The code takes in slices of data at non-uniform interval and map it to uniform temporal interval
! 2. PCHIP is used for the temporal interpolation of data
! 3. More details about interpolation scheme can be found on $$https://people.sc.fsu.edu/~jburkardt/f_src/pchip/pchip.html$$
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program main

    implicit none

    !!! Input parameters for interpolation
    integer (kind=4), parameter    ::  nr = 356        !no of points in radial direction
    integer (kind=4), parameter    ::  ntheta = 258    !no of points in azimuthal direction
    integer (kind=4), parameter    ::  N = 7207        !no of snapshots
    integer (kind=4), parameter    ::  nstart = 1892600   !beginning of iteration
    integer (kind=4), parameter    ::  nend = 2613200     !end of iteration
    integer (kind=4), parameter    ::  stride = 100   !stride for data files
    character (len=160), parameter ::  inDIR = '/mnt/RAID5/sheel/spod_re5e4/frinf/wp_slice/x_D_3/'    !directory of input slices
    character (len=160), parameter ::  outDIR = '/mnt/RAID5/sheel/spod_re5e4/frinf/data_files_uniform/x_D_3/wp/'   !directory of output slices
    character (len=160), parameter ::  timeDIR = './'   !directory of output slices
    character (len=160), parameter ::  vartype = 'wp'   !variable that is interpolated
    character (len=160), parameter ::  slice_idx = '3.0'   !variable that is interpolated
  
    !!! Temporary variables

    integer (kind=4)               ::  i, j, k, nwk, ierr
    real (kind=8), allocatable     ::  inDATA(:), outDATA(:)
    real (kind=8), allocatable     ::  dd(:), wk(:), di(:)
    character (len=160)            ::  filename, fullfile

    !!! Data variables

    real (kind=8), allocatable     ::  var_data(:,:), time(:), time_interp(:)
    integer (kind=4)               ::  Nrows, io_type
    logical                        ::  spline
    
    !!! Reading the data files into var_data

    io_type = 0
    Nrows = nr*ntheta
    print*, 'Nrows ', Nrows
    allocate(var_data(N,Nrows))
    call io_data_files(inDIR,outDIR,slice_idx,nr,ntheta,Nrows,nstart,nend,stride,N,vartype,var_data,io_type)

    print*, 'Read all the data into var_data matrix'
    print*, 'Minval of var_data matrix', minval(var_data)
    print*, 'Maxval of  var_data matrix', maxval(var_data)

    !!! Reading the original and uniform time stamps file

    print*, 'Timestamps file are in ', timeDIR

    allocate(time(N), time_interp(N))

    filename = 'time_stamp_1892600_2613200_2.txt'
    write(fullfile,'(a,a)') trim(timeDIR), trim(filename)

    open(unit=100, file=fullfile, status = 'old', form = 'formatted', action = 'read')
    
    do i = 1,N
        read(100,*) time(i)    
    end do

    filename = 'time_stamp_1892600_2613200_uniform.txt'
    write(fullfile,'(a,a)') trim(timeDIR), trim(filename)

    open(unit=100, file=fullfile, status = 'old', form = 'formatted', action = 'read')
    
    do i = 1,N
        read(100,*) time_interp(i)    
    end do

    print*, 'Read all the time into time matrix'
    print*, 'Minval of time matrix', minval(time)
    print*, 'Maxval of time matrix', maxval(time)

    print*, 'Read all the time into time_interp matrix'
    print*, 'Minval of time_interp matrix', minval(time_interp)
    print*, 'Maxval of time_interp matrix', maxval(time_interp)


    !!! Interpolating each spatial location in time

    allocate(inDATA(N))
    allocate(outDATA(N))
    
    nwk = 2*N
    allocate(dd(N), di(N))    
    allocate(wk(nwk))    

    
    spline = .false.  ! spline = .false. sets the interpolation to PCHIP, otherwise the interpolation is SPLINE
    print*, 'PCHIP interpolation is done in time'

    do j = 1,Nrows
        print*, 'For row number ', j
        inDATA(1:N) = var_data(:,j)
        print*, 'Minval and maxval of inDATA ', minval(inDATA), maxval(inDATA)
        outDATA = 0.0d0
        wk = 0.0d0
        call dpchez(N,time,inDATA,dd,spline,wk,nwk,ierr)
        call dpchev(N,time,inDATA,dd,N,time_interp,outDATA,di,ierr)
        print*, 'Minval and maxval of outDATA ', minval(outDATA), maxval(outDATA)
        var_data(:,j) = outDATA(1:N)       
    end do

    print*, 'Interpolated var_data matrix'
    print*, 'Minval of var_data matrix', minval(var_data)
    print*, 'Maxval of  var_data matrix', maxval(var_data)


    io_type = 1
    call io_data_files(inDIR,outDIR,slice_idx,nr,ntheta,Nrows,nstart,nend,stride,N,vartype,var_data,io_type)

end program main
