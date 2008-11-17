c this will act as an interface between Fortran and R and avoid 
c having to hack about with LINPACK. Hopefully.


      subroutine scint(n,betam,dw,dz,dc,dwc,nptsq,errest,qsize,qwork)
c put in a description of the args


         double complex dw,dz,dc,dwc

         integer n,i,ier,nptsq,iprint,iguess,k,qsize
         COMPLEX c,w,zsc,wsc,z,ww,zz,zero,zi,wc,wwex,zzex,wtmp,
     &          ztmp
         real err,qwork,errest,tol
c        double precision betam
         dimension Z(n),W(n),BETAM(n)
         dimension QWORK(qsize)
         dimension dw(n), dz(n)


c leave these in for the moment
      ZERO = (0.,0.)
      ZI = (0.,1.)

c convert double complex back singles
      z=cmplx(dz)
      w=cmplx(dw)
      wc=cmplx(dwc)
      c=cmplx(dc)

c we can calculate the angles if we wish
      CALL ANGLES(N,W,BETAM)


C COMPUTE NODES AND WEIGHTS FOR PARAMETER PROBLEM:
      CALL QINIT(N,BETAM,NPTSQ,QWORK,qsize)

C
C SOLVE PARAMETER PROBLEM:
C   (INITIAL GUESS MUST BE GIVEN TO AVOID ACCIDENTAL EXACT SOLUTION)
      IPRINT = 0
      IGUESS = 1

      DO K = 1,n
         Z(K) = EXP(CMPLX(0.,K-4.))
      end do


      TOL = 1.E-6

c do the actual call to scsolv
      CALL SCSOLV(IPRINT,IGUESS,TOL,ERREST,N,C,Z,
     &  WC,W,BETAM,NPTSQ,QWORK)


c     set the types back...
      dz=dcmplx(z)
      dw=dcmplx(w)
      dwc=dcmplx(wc)
      dc=dcmplx(c)


      return

      end subroutine
