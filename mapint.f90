C     Wrapper function for the mapping from the polygon to the unit
C     disk.

      subroutine mapint(dpoints,npoints,n,betam,nptsq,qwork,qsize,
     &            accuracy,dz,dw,dc,dwc,dretpoints,reterrors)
C
C        dpoints      vector of points to evaluate (double complex)
C        npoints      length of the above (int)
C        n            number of vertices in the polygon (int)
C        betam        vector of external angles (real(n))
C        nptsq        number of points (int) 
C        qwork        work array (single)
C        qsize        length of above (int)
C        accuracy     accuracy of the results (single)
C        dz
C        dw
C        dc
C        dwc
C        dretpoints   mapping of dpoints into the unit disk (length
C                     npoints, double complex)
C        reterrors    binary array of length npoints, flagging those
C                     points which had errors (int)

         implicit none

         double complex dpoints,dw,dz,dc,dwc,dretpoints
         complex points,w,z,c,wc,retpoints
         integer n, nptsq, qsize,npoints,i,reterrors
         real betam, qwork, accuracy
         dimension betam(n), qwork(qsize), w(n),z(n),points(npoints),
     &         retpoints(npoints),reterrors(npoints)
         dimension dw(n), dz(n),dpoints(npoints),dretpoints(npoints)
         
C        internal vars
         complex zn,wn,tmp,zsc,zinit
         integer kn
         integer ier,iguess
       

      integer jcount, icount,wccount

      jcount=0
      icount=0
      wccount=0


         ! convert double complex to singles
         z=cmplx(dz)
         w=cmplx(dw)
         wc=cmplx(dwc)
         c=cmplx(dc)
         points(:)=cmplx(dpoints(:))

         ! error code, will !=0 if an error has ocurred.
         ! see below for error handling.
         ier=0

         ! we don't supply a guess so set iguess to be != 1
         iguess=2

         ! zinit doesn't matter, so just set it to zero
         zinit = (0.0,0.0)

         ! set reterrors
         reterrors(:)=0


         ! loop over the points
         do i=1,npoints
            call nearw(points(i),zn,wn,kn,n,z,wc,w,betam)

            retpoints(i)=zsc(points(i),iguess,zinit,zn,wn,kn,accuracy,
     &                         ier,n,c,z,wc,w,betam,nptsq,qwork)


            ! Let the user know if there were any errors
            if(ier.ne.0) then



            if(CMPLX(0,0).eq.zn) then
               wccount=wccount+1
            end if
   

          icount=icount+1
                  reterrors(i)=1
               ier=0
               retpoints(i)=zsc(points(i),iguess,zinit,retpoints(i-1),
     &                            points(i-1),0,
     &                        accuracy,ier,n,c,z,wc,w,betam,nptsq,qwork)
               if(ier.ne.0) then
                  print*," *** An error occurred mapping:",points(i)
C                  reterrors(i)=1
             jcount=jcount+1
               end if
               ier=0
            end if
         end do


      PRINT*,"first=",icount,"second=",jcount
      PRINT*,"wccount=",wccount,"npoints=",npoints
         ! Convert back to doubles   
         dretpoints(:)=dcmplx(retpoints(:))
      return

      end subroutine
