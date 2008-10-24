      PROGRAM TEST1
      IMPLICIT none
      double COMPLEX C,W,zsc,wsc,Z,ww,zz,zero,zi,wc,wwex,zzex,wtmp,ztmp
      integer n,i,ier,nptsq,iprint,iguess,k,iqwork
      double precision betam,err,qwork,errest,tol

      DIMENSION Z(20),W(20),BETAM(4),qwork(344)
c      dimension, allocatable QWORK(:)
C,t(4),s(4)

      iqwork=344
c      allocate(qwork(iqwork))


      ZERO = (0.,0.)
      ZI = (0.,1.)
C SET UP PROBLEM:
      N = 4
      WC = CMPLX(0.,SQRT(2.))
      W(1) = ZI
      W(2) = ZERO
      W(3) = (1.E20,1.E20)
      W(4) = ZERO

      BETAM(1) = 1.
      BETAM(2) = -.5
      BETAM(3) = -2.
      BETAM(4) = -.5

C:      CALL ANGLES(N,W,BETAM)


C
C COMPUTE NODES AND WEIGHTS FOR PARAMETER PROBLEM:
      NPTSQ = 5
      CALL QINIT(N,BETAM,NPTSQ,QWORK,iqwork)
C
C SOLVE PARAMETER PROBLEM:
C   (INITIAL GUESS MUST BE GIVEN TO AVOID ACCIDENTAL EXACT SOLUTION)
      IPRINT = 0
      IGUESS = 1
      DO 1 K = 1,4
    1 Z(K) = EXP(CMPLX(0.,K-4.))
      TOL = 1.E-6
      CALL SCSOLV(IPRINT,IGUESS,TOL,ERREST,N,C,Z,
     &  WC,W,BETAM,NPTSQ,QWORK,iqwork)
C
C COMPARE WSC(Z) TO EXACT VALUES FOR VARIOUS Z:
      DO 10 I = 1,4
        ZZ = (.3,0.) * CMPLX(I-2.,.2*I+.5)
        WW = WSC(ZZ,0,ZERO,WC,0,N,C,Z,BETAM,NPTSQ,QWORK,iqwork)
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
     &    N,C,Z,WC,W,BETAM,NPTSQ,QWORK,iqwork)
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
