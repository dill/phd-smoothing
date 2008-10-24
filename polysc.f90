      PROGRAM TEST1
      integer n,i,ier,nptsq,iprint,iguess,k,qsize
      double precision betam(n),qwork
      double COMPLEX c,w,zsc,wsc,z,ww,zz,zero,zi,wc,wwex,zzex,wtmp,
     &          ztmp
      double precision err,qwork,errest,tol
      dimension Z(n),W(n),BETAM(n),QWORK
      ZERO = (0.,0.)
      ZI = (0.,1.)


C
C SET UP PROBLEM:
      N = 4
      WC = CMPLX(0.,SQRT(2.))
      W(1) = (10.,0.)
      W(2) = (0.,10.)
      W(3) = (-10.,0.)
      W(4) = (0.,-10.)
      BETAM(1) = 1.
      BETAM(2) = -.5
      BETAM(3) = -2.
      BETAM(4) = -.5

C      CALL ANGLES(N,W,BETAM)

      qsize=nptsq*(2*n+3)

c do everything using the interface
      call scint(N,betam,w,z,ci,qsize)











C
C COMPUTE NODES AND WEIGHTS FOR PARAMETER PROBLEM:
C      NPTSQ = 5
c      CALL QINIT(N,BETAM,NPTSQ,QWORK)
C
C SOLVE PARAMETER PROBLEM:
C   (INITIAL GUESS MUST BE GIVEN TO AVOID ACCIDENTAL EXACT SOLUTION)
c      IPRINT = 0
c      IGUESS = 1
c      DO 1 K = 1,4
c    1 Z(K) = EXP(CMPLX(0.,K-4.))
c      TOL = 1.E-6
c      CALL SCSOLV(IPRINT,IGUESS,TOL,ERREST,N,C,Z,
c     &  WC,W,BETAM,NPTSQ,QWORK)
C
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
      END
