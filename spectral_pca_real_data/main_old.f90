!!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT COMMENTS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 1. It goes over a set of snapshots to generate 2D SPOD modes
!
! 2. There ghost is removed in the SPOD code itself
!
! 3. Weight matrix and time stamp is provided from an external file
!
! 4. Normalizations are based on the procedure given in 
!    https://github.com/SpectralPOD/spod_matlab
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program main

    use precision_mod
    use mpicomms_mod

    use, intrinsic :: iso_c_binding  
    implicit none
    include 'fftw3.f03'  

    !!! Parameters
    real    (kind=8),    parameter ::  pi         = 3.141592653589793238462643383279502884197d0   
    real    (kind=8),    parameter ::  Fr         = 2.0d0
    character (len=160), parameter ::  inDIR      = '/mnt/RAID5/sheel/spod_re5e4/fr2/'
    character (len=160), parameter ::  weightDIR  = './'
    character (len=160), parameter ::  weightfile = 'weight_fr2_slice_var_truncated_r_D_10.txt'
    character (len=160), parameter ::  outDIR     = '/mnt/RAID5/sheel/spod_re5e4/fr2/spod_data/x_D_10/'
    character (len=160), parameter ::  slice_idx  = '10'
    integer (kind=4)               ::  nr, ntheta, nx, numvar, N, stride, Nfreq, Novlp, idx
    integer (kind=4)               ::  nstart
    real    (kind=8)               ::  dt

    !!! Temporary Variables
    integer (kind=4)               :: i, j, k, iflag, ier, z, info, rst, mode, np
    real    (kind=8)               :: d, winweight
    real    (kind=8)               :: eps
    character(len=160)             :: fileDIR, filename, basename, folder_name
    integer                        :: namef
    real(kind=8),    allocatable   :: Qtemp(:,:), Qtemp1(:,:)
    complex(kind=8), allocatable   :: out1(:), in1(:)
 
    !!! Data Variables
    integer(kind=4)                :: Nrows, Nblk, Nblk_sampled, Nfreq_sampled, tot_transfer
    integer(kind=4), allocatable   :: qstart(:), qend(:)                             
    real(kind=8),    allocatable   :: Q(:,:), Q_sub(:,:), Q_mean(:), freq(:), Qblk(:,:,:), P(:,:,:,:)
    real(kind=8),    allocatable   :: Pr(:,:,:), P_trunc(:,:,:), window(:), P_global(:,:,:), P_recv(:,:,:), P_local(:,:,:), Pr_full(:,:,:)
    complex(kind=8), allocatable   :: Qout(:,:,:), Q_k(:,:,:), Eigen_V(:,:,:)
    complex(kind=8), allocatable   :: Lambda(:,:,:), Lambda_invsq(:,:,:), Stemp(:,:)
                                
    !!! FFT Variables
    !complex (kind=8), allocatable :: Q_inv(:,:,:), inv1(:,:)
    complex                        :: alpha, alpha1      
    integer (kind=8)               :: plan, plan_forward, plan_backward
   
    !!! LAPACK Variables
    integer                        :: lwork
    real(kind=8),    allocatable   :: rwork(:)
    complex(kind=8), allocatable   :: work(:)
    complex(kind=8), allocatable   :: vl(:,:), vr(:,:), S_Nfreq(:,:), S(:,:,:), S_global(:,:,:), S_recv(:,:,:)                             
    real(kind=8), allocatable      :: L_Nfreq(:)   ! Using zheev
    !real(kind=8), allocatable  ::   L_Nfreq(:)   ! Using zgeev


    !!! MPI Setup
    integer (kind=4) :: tag, Nxp, Nyp, Nzp, tot_size, sender
    integer (kind=4) :: gsizes(3), lsizes(3), start(3), start_loc(3), start_loc_recv(3)
    integer (kind=4 ):: Ifirst, Jfirst, Kfirst, Ilast, Jlast, Klast, Ibegin, Iend, Jbegin, Jend, Kbegin, Kend 
    integer (kind=4), allocatable :: Imatrix(:,:), Jmatrix(:,:), Kmatrix(:,:)    
    integer(kind=MPI_Offset_kind) :: SP_MOK, WP_MOK,  Nx_MOK, Ny_MOK , Nz_MOK


    call mpiinit()
    !!!!!!!!!!!!!! Reading the input data  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    open(unit=110,file='spod_parameters.input',form="formatted")

    read (110,*) nr
    read (110,*) ntheta
    read (110,*) nx
    read (110,*) numvar
    read (110,*) dt
    read (110,*) N
    read (110,*) nstart
    read (110,*) stride
    read (110,*) Nfreq
    read (110,*) Nblk_sampled
    read (110,*) Nfreq_sampled
    read (110,*) Novlp

    close(110)

    print*,  nr
    print*,  ntheta
    print*,  nx
    print*,  numvar
    print*,  dt
    print*,  N
    print*,  nstart
    print*,  stride
    print*,  Nblk_sampled
    print*,  Nfreq_sampled
    print*,  Nfreq
    print*,  Novlp


    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Setting up the parallel environment !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (mod(nr,px).ne.0) then
        write(*,*) 'Nx_max is not devisable by px'
        call MPI_ABORT(MPI_COMM_WORLD,0,ierr)
     end if
 
     Nxp = nr/px
 
     if (mod(ntheta,py).ne.0) then
         write(*,*) 'Ny_max is not devisable by py'
         call MPI_ABORT(MPI_COMM_WORLD,0,ierr)
     end if
 
     Nyp = ntheta/py
 
     if (mod(nx,pz).ne.0) then
         write(*,*) 'Nz_max is not devisable by pz'
         call MPI_ABORT(MPI_COMM_WORLD,0,ierr)
     end if
 
     Nzp = nx/pz
 
 
     if (myrank .eq. 0) then
         write(*,*) "Nxp,Nyp,Nzp calculated", Nxp, Nyp, Nzp, numvar
     endif

  !--------------------------------------------------------------------------------------
     ! The parallel computation has been parallelized in 3-D and hence the Ifirst and Ilast
     ! basically assign the portion of the cube which will be handled by each processor. 
     ! Same goes for Jfirst and the likes 
     !--------------------------------------------------------------------------------------
 
     Ifirst = irank*Nxp + 1; Ilast  = Ifirst + Nxp - 1
     Jfirst = jrank*Nyp + 1; Jlast  = Jfirst + Nyp - 1
     Kfirst = krank*Nzp + 1; Klast  = Kfirst + Nzp - 1
 
     start_loc(1) = Ifirst
     start_loc(2) = Jfirst
     start_loc(3) = Kfirst
 
     !--------------------------------Setting environment to store beginning and end points for each individual processor---------------------------------
 
     if (myrank == 0) then
         allocate (Imatrix(numprocs,3))
         allocate (Jmatrix(numprocs,3))
         allocate (Kmatrix(numprocs,3))
         Imatrix(1,1) = Ifirst
         Imatrix(1,2) = Ilast
         Imatrix(1,3) = 0
         Jmatrix(1,1) = Jfirst
         Jmatrix(1,2) = Jlast
         Jmatrix(1,3) = 0
         Kmatrix(1,1) = Kfirst
         Kmatrix(1,2) = Klast
         Kmatrix(1,3) = 0
     end if
 
     if (myrank .ne. 0) then
         call MPI_SEND(start_loc, 3, MPI_INTEGER, 0, tag, MPI_COMM_WORLD, ierr)
     end if
 
     call MPI_BARRIER(MPI_COMM_WORLD, ierr)
 
     if (myrank == 0) then
         do i = 1,numprocs-1
             call MPI_RECV(start_loc_recv, 3, MPI_INTEGER, i, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
             sender = status(MPI_SOURCE)
 
             Imatrix(i+1,1) = start_loc_recv(1)
             Imatrix(i+1,2) = start_loc_recv(1) + Nxp - 1
             Imatrix(i+1,3) = i
             Jmatrix(i+1,1) = start_loc_recv(2)
             Jmatrix(i+1,2) = start_loc_recv(2) + Nyp - 1
             Jmatrix(i+1,3) = i
             Kmatrix(i+1,1) = start_loc_recv(3)
             Kmatrix(i+1,2) = start_loc_recv(3) + Nzp - 1
             Kmatrix(i+1,3) = i
         end do
 
         print*, size(Imatrix,1), size(Imatrix,2), size(Jmatrix,1), size(Jmatrix,2), size(Kmatrix,1), size(Kmatrix,2)
     end if
 
     !!!---------------------------------------- Set up the pointers--------------------------------------------------------------
 

     !---------------------------------------------------------------------------------------------------------------------------------------------------------------
 
     call MPI_BARRIER(MPI_COMM_WORLD, ierr)
     ! Setting the view for phi                              ! Still to read
 
     gsizes(1)  = nr       ! Overall size 
     gsizes(2)  = ntheta
     gsizes(3)  = nx
     lsizes(1)  = Nxp          ! Size to be written by each processors
     lsizes(2)  = Nyp
     lsizes(3)  = Nzp
     start(1)   = Ifirst - 1   ! Pointer locations for writing  to output 
     start(2)   = Jfirst - 1
     start(3)   = Kfirst - 1
 
     !call MPI_TYPE_CREATE_SUBARRAY(3,gsizes,lsizes,start,MPI_ORDER_FORTRAN,MPI_REAL_SP,view,ierr)
     !call MPI_TYPE_COMMIT(view,ierr)
     !call MPI_TYPE_CREATE_SUBARRAY(3,gsizes,lsizes,start,MPI_ORDER_FORTRAN,MPI_REAL_WP,view1,ierr)
     !call MPI_TYPE_COMMIT(view1,ierr)
 
     WP_MOK     = int(8,               MPI_Offset_kind)      ! Sets WP_MOK = 8
     Nx_MOK     = int(nr,          MPI_Offset_kind)      ! Sets Nx_MOK = Nx_max
     Ny_MOK     = int(ntheta,          MPI_Offset_kind)      ! Sets Ny_MOK = Ny_max
     Nz_MOK     = int(nx,          MPI_Offset_kind)      ! Sets Nz_MOK = Nz_max
 
     if (myrank.eq.0) then
         print*, WP_MOK, Nx_MOK, Ny_MOK, Nz_MOK
     endif

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    Nrows = Nxp*Nyp*Nzp*numvar              ! No. of rows in the snapshot matrix 
    tot_transfer = Nxp*Nyp*Nzp
    !!!!!!!!!!!!! Reading data files in an array !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    allocate (Q(Nrows,N))                    ! Data snpshots arrandged columnwise
    allocate (Q_mean(Nrows))                 ! Mean of data snapshots
    allocate (Q_sub(Nrows,N))                ! Data snapshots after ssubtraction of mean
    allocate (Pr(nr+2,ntheta+2,nx))          ! real part of the data 
    allocate (P_trunc(nr,ntheta,nx))         ! real part of the data 
    allocate (P_global(nr,ntheta,nx))        ! real part of the data 
    allocate (P_recv(Nxp,Nyp,Nzp))           ! real part of the data 
    allocate (P_local(Nxp,Nyp,Nzp))           ! real part of the data 
    allocate (P(Nxp,Nyp,Nzp,numvar))     
    allocate (Pr_full(521,258,1))

    do rst = 1, N
    
    
        namef = nstart + (rst-1)*stride

    
        if (myrank .eq. 0) then

            print*, 'Reading the file ', namef
            !!!!! Reading the up files
            folder_name = 'up_slice'
            basename = 'up'
            write(filename,'(a,a,a,a,a,a,a,a,i8.8,a,a,a)') trim(inDIR), trim(folder_name), "/", "x_D_", trim(slice_idx), "/", trim(basename), "_", namef, "_", &
                                                 trim(slice_idx), ".5p"
           
            print*, filename, nr, ntheta, nx
            !call io_slice_files(filename, nr, ntheta, nx, Pr)
            
            open(unit=500, file=filename,  status='old', form='unformatted', access='stream', action='read')
            read(500) pr_full
            close (500)

            print*, pr_full(1,:,1)

            pr(1:nr+2,1:ntheta+2,1:nx) = pr_full(1:nr+2,1:ntheta+2,1:nx)

            do k = 1, nr
                P_trunc(k, 1:ntheta, :) = 0.50d0*(Pr(k+1, 2:ntheta+1, :) + Pr(k, 2:ntheta+1, :))  !!!!!! Centered the u velocity field !!!!!!!!!!!!!!!!!!!!!!!!
            end do

            P_global(1:nr,1:ntheta,:) = P_trunc
            P(:,:,:,1) = P_global(1:Nxp, 1:Nyp, 1:Nzp)
            
            do np = 1, numprocs-1
                Ibegin = Imatrix(np+1,1); Iend = Imatrix(np+1,2)
                Jbegin = Jmatrix(np+1,1); Jend = Jmatrix(np+1,2)
                Kbegin = Kmatrix(np+1,1); Kend = Kmatrix(np+1,2)
                P_local(1:Nxp, 1:Nyp, 1:Nzp) = P_global(Ibegin:Iend, Jbegin:Jend, Kbegin:Kend)
                call MPI_SEND(P_local(1,1,1), tot_transfer, MPI_REAL_WP, np, tag, MPI_COMM_WORLD, status, ierr)
            end do
        

        elseif (myrank .ne. 0) then
                call MPI_RECV(P_recv(1,1,1), tot_transfer,  MPI_REAL_WP, 0, tag, MPI_COMM_WORLD, status, ierr)
                P(:,:,:,1) = P_recv
        endif

        call MPI_BARRIER(MPI_COMM_WORLD,ierr)

        if (myrank .eq. 0) then

            !!!!! Reading the vp files
            folder_name = 'vp_slice'
            basename = 'vp'
            write(filename,'(a,a,a,a,a,a,a,a,i8.8,a,a,a)') trim(inDIR), trim(folder_name), "/", "x_D_", trim(slice_idx), "/", trim(basename), "_", namef, "_", &
                                                 trim(slice_idx), ".5p"
            call io_slice_files(filename, nr, ntheta, nx, Pr)
            P(1:nr,1:ntheta,:,2) = Pr

            do k = 1, ntheta
                P_trunc(1:nr, k, :) = 0.50d0*(pr(2:nr+1, k, :) + Pr(2:nr+1, k+1, :))          !!!!!! Centered the v velocity field !!!!!!!!!!!!!!!!!!!!!!!!
            end do

            P_global(1:nr, 1:ntheta, :) = P_trunc
        
            P(:,:,:,2) = P_global(1:Nxp, 1:Nyp, 1:Nzp)
            
            do np = 1, numprocs-1
                Ibegin = Imatrix(np+1,1); Iend = Imatrix(np+1,2)
                Jbegin = Jmatrix(np+1,1); Jend = Jmatrix(np+1,2)
                Kbegin = Kmatrix(np+1,1); Kend = Kmatrix(np+1,2)
                P_local(1:Nxp, 1:Nyp, 1:Nzp) = P_global(Ibegin:Iend, Jbegin:Jend, Kbegin:Kend)
                call MPI_SEND(P_local(1,1,1), tot_transfer, MPI_REAL_WP, np, tag, MPI_COMM_WORLD, status, ierr)
            end do
        

        elseif (myrank .ne. 0) then
                call MPI_RECV(P_recv(1,1,1), tot_transfer, MPI_REAL_WP, 0, tag, MPI_COMM_WORLD, status, ierr)
                P(:,:,:,2) = P_recv
        endif

        call MPI_BARRIER(MPI_COMM_WORLD,ierr)

        if (myrank .eq. 0) then
            !!!!! Reading the wp files
            folder_name = 'wp_slice'
            basename = 'wp'
            write(filename,'(a,a,a,a,a,a,a,a,i8.8,a,a,a)') trim(inDIR), trim(folder_name), "/",  "x_D_", trim(slice_idx), "/", trim(basename), "_", namef, "_", &
                                                 trim(slice_idx), ".5p"
            call io_slice_files(filename, nr, ntheta, nx, Pr)
            P_trunc(1:nr,1:ntheta,:) = Pr(2:nr+1, 2:ntheta+1, :)                             !!!!!! Centered the w velocity field !!!!!!!!!!!!!!!!!!!!!!!!
            P_global(1:nr, 1:ntheta, :) = P_trunc


            P(:,:,:,3) = P_global(1:Nxp, 1:Nyp, 1:Nzp)
            
            do np = 1, numprocs-1
                Ibegin = Imatrix(np+1,1); Iend = Imatrix(np+1,2)
                Jbegin = Jmatrix(np+1,1); Jend = Jmatrix(np+1,2)
                Kbegin = Kmatrix(np+1,1); Kend = Kmatrix(np+1,2)
                P_local(1:Nxp, 1:Nyp, 1:Nzp) = P_global(Ibegin:Iend, Jbegin:Jend, Kbegin:Kend)
                call MPI_SEND(P_local(1,1,1), tot_transfer, MPI_REAL_WP, np, tag, MPI_COMM_WORLD, status, ierr)
            end do
        

        elseif (myrank .ne. 0) then
                call MPI_RECV(P_recv(1,1,1), tot_transfer, MPI_REAL_WP, 0, tag, MPI_COMM_WORLD, status, ierr)
                P(:,:,:,3) = P_recv
        endif

        call MPI_BARRIER(MPI_COMM_WORLD,ierr)

        if (myrank .eq. 0) then
            !!!!! Reading the densp files
            folder_name = 'densp_slice'
            basename = 'densp'
            write(filename,'(a,a,a,a,a,a,a,a,i8.8,a,a,a)') trim(inDIR), trim(folder_name), "/", "x_D_", trim(slice_idx), "/", trim(basename), "_", namef, "_", &
                                                 trim(slice_idx), ".5p"
            call io_slice_files(filename, nr, ntheta, nx, Pr)
            P_trunc(1:nr,1:ntheta,:) = Pr(2:nr+1, 2:ntheta+1, :)                              !!!!!! Centered the dens field !!!!!!!!!!!!!!!!!!!!!!!!
            P_global(1:nr, 1:ntheta, :) = P_trunc

            P(:,:,:,4) = P_global(1:Nxp, 1:Nyp, 1:Nzp)
            
            do np = 1, numprocs-1
                Ibegin = Imatrix(np+1,1); Iend = Imatrix(np+1,2)
                Jbegin = Jmatrix(np+1,1); Jend = Jmatrix(np+1,2)
                Kbegin = Kmatrix(np+1,1); Kend = Kmatrix(np+1,2)
                P_local(1:Nxp, 1:Nyp, 1:Nzp) = P_global(Ibegin:Iend, Jbegin:Jend, Kbegin:Kend)
                call MPI_SEND(P_local(1,1,1), tot_transfer, MPI_REAL_WP, np, tag, MPI_COMM_WORLD, status, ierr)
            end do
        
        elseif (myrank .ne. 0) then
            call MPI_RECV(P_recv(1,1,1), tot_transfer, MPI_REAL_WP, 0, tag, MPI_COMM_WORLD, status, ierr)
            P(:,:,:,4) = P_recv
        endif

        call MPI_BARRIER(MPI_COMM_WORLD,ierr)

        !!! Arranging all the data in Q - snapshot matrix
        do i=1,numvar
            do k=1,Nzp
                do j=1,Nyp
                    Q((1 + Nxp*(j-1) + Nxp*Nyp*(k-1) + Nxp*Nyp*Nzp*(i-1)):(Nxp*j + Nxp*Nyp*(k-1) + Nxp*Nyp*Nzp*(i-1)),rst) = P(1:Nxp,j,k,i)
                end do
            end do
        end do

        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    
    end do
    
    print*, 'Shape of Q ',                     shape(Q), myrank       
    print*, 'minval of real part of Q ',       minval((Q(1:Nxp*Nyp,:))), myrank    
    print*, 'maxval of real part of Q ',       maxval((Q(1:Nxp*Nyp,:))), myrank
    
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    
    !!!!!!!!!!! Subtracting the mean from snapshot matrix !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    call subtract_qmean(Q, Nrows, N, Q_mean, Q_sub)      
    
    deallocate(Q_mean,Q)
    !deallocate(Pr, P, P_global, P_recv, P_trunc)
    
    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    
    print*, 'Rowwise mean subtracted'
    print*, 'minval of real part of  Q_sub ',      minval((Q_sub(:,:)))     
    print*, 'maxval of real part of  Q_sub ',      maxval((Q_sub(:,:)))     
      
    !!!!!!!!!! Dividing data into blocks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
    if (Novlp > Nfreq) then 
        print*, "Error: Overlap too large"
    end if 
 
    !!!!!!!!! Calculating the number of blocks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    d = ((N-Novlp)/(Nfreq-Novlp))
    Nblk = floor(d)       
    print*, 'Number of blocks ', Nblk
    
    allocate (qstart(Nblk))
    allocate (qend(Nblk))
       
    !!!!!!!! Start and End of Each Block !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do i = 1, Nblk
        qstart(i) = (i-1)*(Nfreq-Novlp)+1
        qend(i)   = qstart(i)+Nfreq-1
    end do

    allocate (Qtemp(Nrows, Nfreq))
    allocate (in1(Nfreq))
    allocate (out1(Nfreq))
    print*, '2. Allocating the most memory consuming array Qblk'
    allocate (Qblk(Nrows, Nfreq, Nblk))
    allocate (window(Nfreq))

    !allocate (Qtemp1(Nfreq,Nrows))
    !allocate (Q_inv(Nfreq,Nrows,Nblk)) 
    !allocate (inv1(Nfreq,Nrows))
    
    !!!!!!!!! Seperating Q into blocks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do i = 1, Nblk
        Qblk(:,1:Nfreq,i) = Q_sub(:,qstart(i):qend(i))  
    end do    

    deallocate(Q_sub)

    !!!!!!!! Calling window function !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call hamming_window(Nfreq, window)
    print*, 'Constructed the hamming window'    
    print*, 'Minval of window', minval(window(:))
    print*, 'Maxval of window', maxval(window(:))
    
    !!!!!!! Windowing the Q block !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    do j = 1, Nblk
        do i = 1, Nfreq
            Qblk(:,i,j) = Qblk(:,i,j)*window(i)
        end do
    end do      
    
    print*, 'Windowed Qblk'    

    !!!!!!! Normalizing the Q block !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !winweight = Nfreq/(sum(window))           !!! This is done to preserve the amplitude of windowed signal, doesn't preserve energy
    winweight = 1.587845072201937             !!! This factor for hamming window preserves the energy of windowed signal
    Qblk = (winweight/Nfreq)*Qblk   
    
    print*, 'minval of real Qblk ', minval((Qblk(:,:,:)))     
    print*, 'maxval of real Qblk ', maxval((Qblk(:,:,:)))     

    deallocate(window)  
    
    print*, '1. Allocating the most memory consuming array Qout'
    allocate (Qout(Nrows, Nfreq, Nblk))
 
    !!!!!!!!!!!!!!!! Row wise FFT of individual blocks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do i = 1,Nblk
        print*, 'In Nblk ', i
        Qtemp(1:Nrows,1:Nfreq) = Qblk(1:Nrows,1:Nfreq,i)
        
        do j = 1, Nrows
            in1 = Qtemp(j,:)    
            call dfftw_plan_dft_1d(plan_forward, Nfreq, in1, out1, FFTW_FORWARD, FFTW_ESTIMATE)
            call dfftw_execute(plan_forward, in1, out1)
            call dfftw_destroy_plan(plan_forward)
            Qout(j,:,i) = out1
        end do
        !!!! Backward transform to verify
        !call dfftw_plan_dft_c2r_2d(plan_backward,Nfreq,Nrows,out1,inv1,FFTW_ESTIMATE)
        !call dfftw_execute(plan_backward)
        !call dfftw_destroy_plan(plan_backward) 
        !   Q_inv(:,:,i) = inv1
    end do
   
    
    deallocate (Qtemp, Qblk)       
    deallocate (in1, out1)
    deallocate (qstart, qend) 
    allocate   (Q_k(Nrows,Nblk,Nfreq)) 

    !!!!!!!!!!!! Sorting the FFT matrix at each frequency k !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    do i = 1, Nblk
        do j = 1, Nfreq
            Q_k(:,i,j) = Qout(:,j,i)
        end do
    end do 

    print*, 'minval of imag Q_k ', minval(aimag(Q_k(:,:,:)))      
    print*, 'maxval of imag Q_k ', maxval(aimag(Q_k(:,:,:)))      
    print*, 'minval of real Q_k ', minval(real(Q_k(:,:,:)))     
    print*, 'maxval of real Q_k ', maxval(real(Q_k(:,:,:)))     

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)

    deallocate(Qout)
    !!!!!!!!!!!!!!!! BLOCK FOR DEBUGGING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! filename = 'Q_kreal_1.txt'
    ! open(unit=500, file=filename,  status='replace', & 
    !      form='formatted', access='stream', action='write')

    !  do j = 1,Nblk
    !      do i = 1, Nrows
    !          write(500,*) real(Q_k(i,j,1))
    !      enddo
    !  enddo

    !  close(500)
    
    ! filename = 'Q_kimag_1.txt'
    ! open(unit=500, file=filename,  status='replace', & 
    !      form='formatted', access='stream', action='write')

    !  do j = 1,Nblk
    !      do i = 1,Nrows
    !          write(500,*) aimag(Q_k(i,j,1))
    !      enddo
    !  enddo

    !  close(500)
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !!!!!!!!!!!! Calculating the Cross specral Density !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    allocate (S(Nblk,Nblk,Nfreq))
    allocate (Stemp(Nblk, Nblk))
   
    do i = 1, Nfreq
        call weighting(weightDIR, weightfile, Q_k(:,:,i), Stemp, Nblk, Nrows, Nxp, Nyp, ntheta, numvar,Fr)
        S(:,:,i) = Stemp
    end do

    !!!!!!! Normalizing the Cross Spectral Density !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
    S = S/(Nblk)                             ! For ensuring the weighted cross spectral density that is formed
    
    print*, 'minval of imaginary S ', minval(aimag(S(:,:,:)))      
    print*, 'maxval of imaginary S ', maxval(aimag(S(:,:,:)))      
    print*, 'minval of real S ',      minval(real(S(:,:,:)))     
    print*, 'maxval of real S ',      maxval(real(S(:,:,:)))     
    

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)

    !!!!!!! Collecting S from all the processors to zero processor !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    tot_size = Nblk*Nblk*Nfreq
    
    tag = 0

    if (myrank .eq. 0) then 
        allocate(S_recv(Nblk, Nblk, Nfreq))
        allocate(S_global(Nblk, Nblk, Nfreq))
        S_global = (0.0d0, 0.0d0)

        print*, 'Entering the processor ', myrank
        
        !S_global = S_global + S
        do j = 1, Nfreq
            S_global(:,:,j) = S_global(:,:,j) + S(:,:,j)
        enddo

        print*, 'S_global added from 0 processor'
        do i = 1, numprocs-1
            call MPI_RECV(S_recv(1,1,1),tot_size,MPI_REAL_WP,i,tag,MPI_COMM_WORLD,status,ierr)
            print*, 'Recieved data from ', i
            
            do j = 1, Nfreq
                S_global(:,:,j) = S_global(:,:,j) + S_recv(:,:,j)
            enddo
        enddo
        print*, 'Exiting the processor ', myrank
    endif


    if (myrank .ne. 0) then
        print*, 'Entering the processor ', myrank
        call MPI_SEND(S(1,1,1),tot_size,MPI_REAL_WP,0,tag,MPI_COMM_WORLD,status,ierr)
        print*, 'Sent to 0 from the processor ', myrank
    endif

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)

    !!!!!!! Calculating the eigenvalues and eigen vectors !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (myrank .eq. 0) then

        allocate (Lambda(Nblk,Nblk,Nfreq))
        allocate (Lambda_invsq(Nblk,Nblk,Nfreq))
        allocate (L_Nfreq(Nblk))
    
        Lambda = 0.0d0
        Lambda_invsq = 0.d0

        !! LAPACK Parameters for  eigen values and eigen vectors 
        lwork = 2*Nblk-1              !zheev
        !lwork = 2*Nblk               !zgeev
        !allocate (rwork(2*Nblk))     !zgeev
        allocate (rwork(3*Nblk-2))    !zheev
        allocate (work(lwork))
        allocate (S_Nfreq(Nblk,Nblk))
        allocate (Eigen_V(Nblk,Nblk,Nfreq))
        !allocate (vr(Nblk,Nblk)) ! zheev
        !allocate (vl(Nblk,Nblk)) ! zgeev

        !!!!!!! Performing the Eigenvalue decomposition of CSD !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        do i = 1, Nfreq
        
            print*, 'In Nfreq ', i
        
            S_Nfreq(1:Nblk,1:Nblk) = S_global(1:Nblk,1:Nblk,i) 
                    
            print*, 'minval of imaginary S_Nfreq ', minval(aimag(S_Nfreq(:,:)))      
            print*, 'maxval of imaginary S_Nfreq ', maxval(aimag(S_Nfreq(:,:)))      
            print*, 'minval of real S_Nfreq ',      minval(real(S_Nfreq(:,:)))     
            print*, 'maxval of real S_Nfreq ',      maxval(real(S_Nfreq(:,:)))     
            print*, 'Shape of S_Nfreq ',            shape(S_Nfreq)

            call zheev('V','L', Nblk, S_Nfreq, Nblk, L_Nfreq, work, lwork, rwork, info)    
            !call zgeev('N','V', Nblk, S_Nfreq, Nblk, L_Nfreq, vl, 1, vr, Nblk, work, lwork, rwork, info)
    
            do j = 1, Nblk
                Lambda(j,j,i)       = L_Nfreq(j)
                Lambda_invsq(j,j,i) = 1/sqrt(L_Nfreq(j)) 
            end do

            Eigen_V(:,:,i) = S_Nfreq(:,:)

        end do

        call io_spod_files(Lambda, Lambda_invsq, Eigen_V, Q_k, outDIR, Nrows, Nblk, Nblk_sampled, Nfreq, Nfreq_sampled, mode)
    
        deallocate(S, S_Nfreq)
   
    end if 

    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    
    !!!!!!!!!!!!!!! Checking the eigenvectors !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !filename = 'eigenvector_Nfreq_real.txt'
    !open(unit=500, file=filename,  status='replace', form='formatted', access='stream', action='write')

    !do j = 1,Nblk
    !    do i = 1,Nblk
    !        write(500,*) real(Eigen_V(i,j,Nfreq))
    !    enddo
    !enddo

    !close(500)
    !     
    !filename = 'eigenvector_Nfreq_imag.txt'
    !open(unit=500, file=filename,  status='replace', form='formatted', access='stream', action='write')

    !do j = 1,Nblk
    !    do i = 1,Nblk
    !        write(500,*) aimag(Eigen_V(i,j,Nfreq))
    !    enddo
    !enddo

    !close(500)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !!!! Writing out eigenvalues, computing eigenmodes and writing them to file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 
   ! deallocate (Lambda, Lambda_invsq, L_Nfreq)
   ! deallocate (Q_k)
   ! deallocate (rwork)     !zgeev
   ! deallocate (work)
   ! deallocate (Eigen_V)
   ! deallocate (Stemp)

    call MPI_FINALIZE (ierr)
    
end program main 
