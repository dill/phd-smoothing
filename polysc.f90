      PROGRAM TEST1
      IMPLICIT COMPLEX(C,W,Z)
      DIMENSION Z(20),W(20),BETAM(20),QWORK(344),t(4),s(4)
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
C      BETAM(1) = 1.
C      BETAM(2) = -.5
C      BETAM(3) = -2.
C      BETAM(4) = -.5

      CALL ANGLES(N,W,BETAM)
C
C COMPUTE NODES AND WEIGHTS FOR PARAMETER PROBLEM:
      NPTSQ = 5
      CALL QINIT(N,BETAM,NPTSQ,QWORK)
C
C SOLVE PARAMETER PROBLEM:
C   (INITIAL GUESS MUST BE GIVEN TO AVOID ACCIDENTAL EXACT SOLUTION)
      IPRINT = 0
      IGUESS = 1
      DO 1 K = 1,4
    1 Z(K) = EXP(CMPLX(0.,K-4.))
      TOL = 1.E-6
      CALL SCSOLV(IPRINT,IGUESS,TOL,ERREST,N,C,Z,
     &  WC,W,BETAM,NPTSQ,QWORK)
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
