       PROGRAM TEST1
       integer n,i,ier,nptsq,iprint,iguess,k,qsize
c       double precision betam,qwork
       COMPLEX zsc,ww,zz,zero,zi,wwex,zzex,
     &          wtmp,ztmp
       real errest,tol,err,accuracy

       integer npoints

       double complex dpoints, dretpoints,dw,dz,dwc,dc

       allocatable dz(:),dw(:)
       allocatable betam(:)
       allocatable qwork(:)
       allocatable dpoints(:)
       allocatable dretpoints(:)


       ! SET UP PROBLEM:
       N = 4
       nptsq=5
       dwc = dcmplx(0.,0.)


       allocate(dz(n))
       allocate(dw(n))
       allocate(betam(n))
       dw(1) = (10.,0.)
       dw(2) = (0.,10.)
       dw(3) = (-10.,0.)
       dw(4) = (0.,-10.)
       BETAM(1) = 1.
       BETAM(2) = -.5
       BETAM(3) = -2.
       BETAM(4) = -.5

       ! call the angles routine if we wanted to find betam
C       CALL ANGLES(N,W,BETAM)

       qsize=nptsq*(2*n+3)
       allocate(qwork(qsize))

       err=.00001

       ! call interface code to calculate the map
       call scint(n,betam,dw,dz,dc,dwc,nptsq,err,qsize,qwork) 

       ! Test the mapint code
       npoints=6
       allocate(dpoints(npoints))
       allocate(dretpoints(npoints))

       ! COMPARE ZSC(W) TO EXACT VALUES FOR VARIOUS W:
       do  i = 1,npoints
         dpoints(i) = dcmplx(I-2.,SQRT(I+1.))
         print*,"dpoint:",dpoints(i)
       end do

       accuracy=.00001


      call mapint(dpoints,npoints,n,betam,nptsq,qwork,qsize,
     &            accuracy,dz,dw,dc,dwc,dretpoints)


      !print*,dretpoints(:)

      end program
