

      subroutine mapint(dpoints,npoints,n,betam,nptsq,qwork,qsize,
     &            accuracy,dz,dw,dc,dwc,dretpoints)
C
C       dpoints      vector of points to evaluate (double complex)
C       npoints      length of the above (int)
C       n            number of vertices in the polygon (int)
C       betam        vector of external angles (real(n))
C       nptsq        number of points 
C



         double complex dpoints,dw,dz,dc,dwc,dretpoints
         complex points,w,z,c,wc,retpoints
         integer n, nptsq, qsize,npoints,i
         real betam, qwork, accuracy
         dimension betam(n), qwork(qsize), w(n),z(n),points(npoints),
     &         retpoints(npoints)
         dimension dw(n), dz(n),dpoints(npoints),dretpoints(npoints)
         
C        internal vars
         complex zn,wn,tmp,zsc
         integer kn
         integer ier,iguess


         ! leave these in for the moment
         ZERO = (0.,0.)
         ZI = (0.,1.)

         ! convert double complex to singles
         z=cmplx(dz)
         w=cmplx(dw)
         wc=cmplx(dwc)
         c=cmplx(dc)
         points(:)=cmplx(dpoints(:))

         ! error code, will !=0 if an error has ocurred.
         ier=0

         ! we don't supply a guess so set iguess to be != 2
         iguess=2

         do i=1,npoints
            call nearw(points(i),zn,wn,kn,n,z,wc,w,betam)
            retpoints(i)=zsc(points(i),iguess,zinit,zn,wn,kn,accuracy,
     &                         ier,n,c,z,wc,w,betam,nptsq,qwork)

            if(ier.ne.0) then
              print*,"Error occurred mapping:",points(i)
            end if

         end do

         ! Convert back to doubles   
         dretpoints(:)=dcmplx(retpoints(:))
      return

      end subroutine
