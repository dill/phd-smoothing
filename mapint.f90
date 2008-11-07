

      subroutine mapint(dpoints,npoints,n,betam,nptsq,qwork,qsize,
     &            accuracy,dz,dw,dc,dwc,dretpoints)
c put in a description of the args

C"NEARW",WW=this.point,ZN=z0,WN=w0,KN=k0,N=nvertices,Z=prevertices,WC=centre,W=polyvertices,BETAM=angles)

C      map.ret<-.Fortran("ZSC",WW=this.point,IGUESS=as.integer(2),ZINIT=nearest$ZN,Z0=nearest$ZN,W0=nearest$WN,K0=nearest$KN,EPS=as.single(accuracy),IER=as.integer(1),N=as.integer(nvertices),C=complex.scale.factor,Z=prevertices,WC=centre,W=polyvertices,BETAM=angles,NPTSQ=as.integer(nptsq),QWORK=qwork,EVALED=evaled)


         double complex dpoints,dw,dz,dc,dwc,dretpoints
         complex points,w,z,c,wc,retpoints
         integer n, nptsq, qsize,npoints,i
         real betam, qwork, accuracy
         dimension betam(n), qwork(qsize), w(n),z(n),points(npoints),
     &         retpoints(npoints)
         dimension dw(n), dz(n),dpoints(npoints),dretpoints(npoints)
         
C        internal vars
         complex zn,wn,kn
         integer ier


c leave these in for the moment
      ZERO = (0.,0.)
      ZI = (0.,1.)

c convert double complex to singles
      z=cmplx(dz)
      w=cmplx(dw)
      wc=cmplx(dwc)
      c=cmplx(dc)
      points(:)=cmplx(dpoints(:))

      print*,qwork, qsize
c     Call qinit to setup the qwork... debug
      CALL QINIT(N,BETAM,NPTSQ,QWORK,qsize)

      ier=0

      do i=1,npoints

         call nearw(points(i),zn,wn,kn,n,z,wc,w,betam)

         retpoints(i)=zsc(points(i),iguess,zinit,zn,wn,kn,accuracy,
     &          ier,n,c,z,wc,w,betam,nptsq,qwork)


      end do

C     Convert back to doubles   
      dretpoints(:)=dcmplx(retpoints(:))


      return

      end subroutine
