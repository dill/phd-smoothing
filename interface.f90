c this will act as an interface between Fortran and R and avoid 
c having to hack about with LINPACK. Hopefully.


      subroutine scint(n,betam,w,z,c,qsize)

         integer n,i,ier,nptsq,iprint,iguess,k,qsize
         COMPLEX c,w,zsc,wsc,z,ww,zz,zero,zi,wc,wwex,zzex,wtmp,
     &          ztmp
         real err,qwork,errest,tol
c        double precision betam
         dimension Z(n),W(n),BETAM(n)
         dimension QWORK(qsize)

c set the points per quadrature stuff
      nptsq=5

c leave these in for the moment
      ZERO = (0.,0.)
      ZI = (0.,1.)
      WC = CMPLX(0.,SQRT(2.))




c we can calculate the angles if we wish
c      CALL ANGLES(N,W,BETAM)


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


C COMPARE WSC(Z) TO EXACT VALUES FOR VARIOUS Z:
      DO 10 I = 1,4
        ZZ = (.3,0.) * CMPLX(I-2.,.2*I+.5)
        WW = WSC(ZZ,0,ZERO,WC,0,N,C,Z,BETAM,NPTSQ,QWORK)
        ZTMP = -ZI * (ZZ-ZI) / (ZZ+ZI)
        WWEX = ZI * SQRT(-ZTMP**2 + (1.,0.))
        ERR = ABS(WW-WWEX)
        WRITE (6,201) ZZ,WW,WWEX,ERR
   10   CONTINUE
      WRITE (6,200)
C
C COMPARE ZSC(W) TO EXACT VALUES FOR VARIOUS W:
      DO 20 I = 1,6
        WW = CMPLX(I-2.,SQRT(I+1.))
        IER = 0
        ZZ = ZSC(WW,0,ZERO,ZERO,WC,0,TOL,IER,
     &    N,C,Z,WC,W,BETAM,NPTSQ,QWORK)
        WTMP = ZI * SQRT((-1.,0.)-WW**2)
        ZZEX = -ZI * (WTMP-ZI) / (WTMP+ZI)
        ERR = ABS(ZZ-ZZEX)
        WRITE (6,202) WW,ZZ,ZZEX,ERR
   20   CONTINUE
C
      STOP
  200 FORMAT (1X)
  201 FORMAT (' Z,W,WEX,ERR: ',3('(',F6.3,',',F6.3,') '),D11.4)
  202 FORMAT (' W,Z,ZEX,ERR: ',3('(',F6.3,',',F6.3,') '),D11.4)

      end subroutine
