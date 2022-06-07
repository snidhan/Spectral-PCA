! Written by Sheel Nidhan
! Date - 30th November, 2019

! Reads in 2D slices of velocities and density at axial cuts 
! Calculates the azimuthal FFT
! Writes out the complex data files to binary to read to SPOD code


program main
    
    implicit none
    
    ! Parameters 
    integer (kind=4), parameter         :: nr          = 356
    integer (kind=4), parameter         :: nr_trunc    = 354
    integer (kind=4), parameter         :: ntheta      = 258
    integer (kind=4), parameter         :: N           = 1
    integer (kind=4), parameter         :: nstart      = 1892600
    integer (kind=4), parameter         :: nend        = 2613200
    integer (kind=4), parameter         :: stride      = 100
    integer (kind=4), parameter         :: modestart   = 1
    integer (kind=4), parameter         :: modeend     = 20
    character (len = 160), parameter    :: slice_idx   = '0.5'
    character (len = 160), parameter    :: inDIR       = '/mnt/RAID5/sheel/spod_re5e4/frinf/data_files_uniform/x_D_005/'
    character (len = 160), parameter    :: outDIR      = '/mnt/RAID5/sheel/spod_re5e4/frinf/azimuthal_fft_data/x_D_005/'
    real    (kind=8), parameter         :: pi          = 3.141592653589793238462643383279502884197d0    

    ! Temporary Variables
    integer (kind=4)    :: i, j, k, l, m, namef   
    character (len=160) :: filename, filename1, basename, mode

    ! Data Variables
    integer (kind=4)              :: Nrows, ier, iflag, lensav, lenwrk, lenc
    integer (kind=4)              :: dtheta
    real (kind=8), allocatable    :: var(:,:), var_trunc(:,:)
    real (kind=8), allocatable    :: work(:), wsave(:)
    complex (kind=8), allocatable :: in1(:), out1(:)
    complex (kind=8)              :: mean_in1
    complex (kind=8), allocatable :: ffted_var(:,:) 


    lenwrk = 2*(ntheta-2)
    lensav = 2*(ntheta-2) + int (log(real(ntheta-2, kind = 8))/log(2.0D+00)) + 4
    lenc   = ntheta-2
    allocate (work(1:lenwrk))
    allocate (wsave(1:lensav))
    
    allocate(var(nr, ntheta))
    allocate(var_trunc(nr_trunc, ntheta-2))
    allocate(ffted_var(nr_trunc, ntheta-2))
    allocate(in1(ntheta-2), out1(ntheta-2))
    

    var       = 0.0d0
    var_trunc = 0.0d0
    dtheta    = 1

    !!!!!!!!!!!!!!!!!!!!!!!!!! Azimuthal FFT of u velocity (radial velocity) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    basename = 'up'

    do i = nstart, nend, stride
        write(filename,'(a,a,a,a,a,i8.8,a,a,a)') trim(inDIR), trim(basename), "/", trim(basename),"_", i, "_", trim(slice_idx), "_uniform_pchip.res" 
        write(6,*) "filename = ", filename
        call read_file(filename, nr, ntheta, var)

        do k = 2, nr-1
            var_trunc(k-1,:) = 0.50d0*(var(k, 2:ntheta-1) + var(k-1, 2:ntheta-1))  !!!!!! Centered the u velocity field !!!!!!!!!!!!!!!!!!!!!!!!
        end do        

        print*, 'Minval of var_trunc ', minval(var_trunc(:,:))
        print*, 'Maxval of var_trunc ', maxval(var_trunc(:,:))

       open(unit=20, file='u_1892600_15.txt', status='replace', form='formatted', access='stream', action='write')
           
       do m = 1, ntheta-2
           write(20,*) var_trunc(15,m)
       end do
        
       close(20)
    
        do j = 1, nr_trunc
            
            in1 = dcmplx(var_trunc(j,:),0)
            !mean_in1 = sum(in1)/(ntheta-2)
            !in1 = in1 - mean_in1
            call cfft1i (ntheta-2, wsave, lensav, ier)
            call cfft1f (ntheta-2, dtheta, in1, lenc, wsave, lensav, work, lenwrk, ier)
            ffted_var(j,:) = in1

        end do     
        
        print*, 'Minval of real transformed_var', ffted_var(15,1)
        print*, 'Maxval of real transformed_var', ffted_var(15,2)
        print*, 'Minval of imag transformed_var', ffted_var(15,3)
        print*, 'Maxval of imag transformed_var', ffted_var(15,4)
        print*, 'Maxval of imag transformed_var', ffted_var(15,5)

        open(unit=20, file='u_1892600_15fft.txt', status='replace', form='formatted', access='stream', action='write')
           
        do m = 1, ntheta-2
            write(20,*) dble(ffted_var(15,m)), dimag(ffted_var(15,m))
        end do
         
        close(20)
 
        !!! Writing out the transformed_var to different modes

        call write_ffted_var(ffted_var, ntheta, nr_trunc, i, outDIR, basename, modestart, modeend, slice_idx)
    end do

    
    !!!!!!!!!!!!!!!!!!!!!!!!!! Azimuthal FFT of v velocity (azimuthal velocity) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    basename = 'vp'
 
    do i = nstart, nend, stride
        write(filename,'(a,a,a,a,a,i8.8,a,a,a)') trim(inDIR), trim(basename), "/", trim(basename),"_", i, "_", trim(slice_idx), "_uniform_pchip.res" 
        write(6,*) "filename = ", filename
        call read_file(filename, nr, ntheta, var)

        do j = 2, ntheta-1
            var_trunc(:, j-1) = 0.50d0*(var(2:nr-1, j) + var(2:nr-1, j-1))  !!!!!! Centered the v velocity field !!!!!!!!!!!!!!!!!!!!!!!!
        end do        

        print*, 'Minval of var_trunc ', minval(var_trunc(:,:))
        print*, 'Maxval of var_trunc ', maxval(var_trunc(:,:))

       open(unit=20, file='v_1892600_15.txt', status='replace', form='formatted', access='stream', action='write')
           
       do m = 1, ntheta-2
           write(20,*) var_trunc(15,m)
       end do
        
       close(20)
    
        do j = 1, nr_trunc
            
            in1 = dcmplx(var_trunc(j,:),0)
            !mean_in1 = sum(in1)/(ntheta-2)
            !in1 = in1 - mean_in1
            call cfft1i (ntheta-2, wsave, lensav, ier)
            call cfft1f (ntheta-2, dtheta, in1, lenc, wsave, lensav, work, lenwrk, ier)
            ffted_var(j,:) = in1

        end do     

        open(unit=20, file='v_1892600_15fft.txt', status='replace', form='formatted', access='stream', action='write')
           
        do m = 1, ntheta-2
            write(20,*) dble(ffted_var(15,m)), dimag(ffted_var(15,m))
        end do
         
        close(20)
        
        print*, 'Minval of real transformed_var', ffted_var(15,1)
        print*, 'Maxval of real transformed_var', ffted_var(15,2)
        print*, 'Minval of imag transformed_var', ffted_var(15,3)
        print*, 'Maxval of imag transformed_var', ffted_var(15,4)
        print*, 'Maxval of imag transformed_var', ffted_var(15,5)
            
        !!! Writing out the transformed_var to different modes

        call write_ffted_var(ffted_var, ntheta, nr_trunc, i, outDIR, basename, modestart, modeend, slice_idx)
    end do
   

    !!!!!!!!!!!!!!!!!!!!!!!!!! Azimuthal FFT of w velocity (axial velocity) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    basename = 'wp'
 
    do i = nstart, nend, stride
        write(filename,'(a,a,a,a,a,i8.8,a,a,a)') trim(inDIR), trim(basename), "/", trim(basename),"_", i, "_", trim(slice_idx), "_uniform_pchip.res" 
        write(6,*) "filename = ", filename

        call read_file(filename, nr, ntheta, var)

        var_trunc(:,:) = var(2:nr-1, 2:ntheta-1)                      !!!! Truncating the data file for w velocity
            
        print*, 'Minval of var_trunc ', minval(var_trunc(:,:))
        print*, 'Maxval of var_trunc ', maxval(var_trunc(:,:))

        open(unit=20, file='w_1892600_15.txt', status='replace', form='formatted', access='stream', action='write')
           
        do m = 1, ntheta-2
            write(20,*) var_trunc(15,m)
        end do
         
        close(20)
    
        do j = 1, nr_trunc
            
            in1 = dcmplx(var_trunc(j,:),0)
            !mean_in1 = sum(in1)/(ntheta-2)
            !in1 = in1 - mean_in1
            call cfft1i (ntheta-2, wsave, lensav, ier)
            call cfft1f (ntheta-2, dtheta, in1, lenc, wsave, lensav, work, lenwrk, ier)
            ffted_var(j,:) = in1

        end do     

        open(unit=20, file='w_1892600_15fft.txt', status='replace', form='formatted', access='stream', action='write')
           
        do m = 1, ntheta-2
            write(20,*) dble(ffted_var(15,m)), dimag(ffted_var(15,m))
        end do
         
        close(20)
        
        print*, 'Minval of real transformed_var', ffted_var(15,1)
        print*, 'Maxval of real transformed_var', ffted_var(15,2)
        print*, 'Minval of imag transformed_var', ffted_var(15,3)
        print*, 'Maxval of imag transformed_var', ffted_var(15,4)
        print*, 'Maxval of imag transformed_var', ffted_var(15,7)
            
        !!! Writing out the transformed_var to different modes

        call write_ffted_var(ffted_var, ntheta, nr_trunc, i, outDIR, basename, modestart, modeend, slice_idx)
    end do


end program main
