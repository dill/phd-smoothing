# Script to call the SCPACK fortran routine 
# The following files are needed from netib
#   http://www.netlib.org/conformal/scpack
#   http://www.netlib.org/conformal/sclib

# Need to follow the same pattern as specified in
# http://www.netlib.org/conformal/scdoc

# First load the shared object file
# You can create the object by running:
# R CMD SHLIB scpack.f90 sclib.f90
dyn.load("scpack.so")


### Create the variables

# Number of vertices
nvertices<-4

# Position of vertices
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-10,0)
polyvertices[2]<-complex(1,0,10)
polyvertices[3]<-complex(1,10,0)
polyvertices[4]<-complex(1,0,-10)

# set number of quadrature points per subinterval
nptsq<-5

# We use the fortran function ANGLES to compute the angles we need
betam<-vector("numeric",nvertices)
angle.ret<-.Fortran("ANGLES",N=as.integer(nvertices),W=polyvertices,BETAM=as.numeric(betam))
betam<-angle.ret$BETAM

# Now use qinit to initialise the Gauss-Jacobi quadrature
qwork<-vector("numeric",nptsq*(2*nvertices+3))
qinit.ret<-.Fortran("QINIT",N=as.integer(nvertices),BETAM=as.numeric(betam),NPTSQ=as.integer(nptsq),QWORK=as.numeric(qwork))
qwork<-qinit.ret$QWORK

# Finally call the SCSOLV routine and find the parameters
# IPRINT and IGUESS must be given to "avoid accidental exact solution"
errest<-vector("numeric",1)
c.const<-vector("complex",1)
wc<-complex(1,0,sqrt(2))
z<-vector("complex",nvertices)
ret<-.Fortran("SCSOLV",IPRINT=as.numeric(0),IGUESS=as.numeric(1),TOL=as.numeric(1e-6),ERREST=as.numeric(errest),N=as.integer(nvertices),C=as.complex(c.const),Z=z,WC=as.complex(wc),W=as.complex(polyvertices),BETAM=as.numeric(betam),NPTSQ=as.integer(nptsq),QWORK=as.numeric(qwork))


# Set some variables
prevertices<-ret$Z
angles<-ret$BETAM
centre<-ret$WC
complex.scale.factor<-ret$C

# Wrapper for the forwards map
# Forwards is disk->polygon
sc.map.forwards<-function(){
   

}

# And the backwards map
# backwards is polygon->disk
sc.map.backwards<-function(points,nvertices,betam,nptsq,qwork,accuracy=1e-6,prevertices,polyvertices,angles,complex.scale.factor,centre){
   # Arguments
   #  points                  vector of points at which we want to evaluate the map
   #  nvertices               number of vertices (not >20)
   #  betam                   array of external angles
   #  nptsq                   number of points per subinterval
   #  qwork                   quadrature work array
   #  accuracy                desired accuracy in output (default 1e-6)
   #  prevertices             complex vector of prevertices
   #  polyvertices            complex vector of polgyon vertices
   #  angles                  exterior angles of the polygon
   #  complex.scale.factor    complex scaling factor
   #  centre                  approximate centre of the polygon

   ############## IGNORE AT THE MOMENT ##############
	#   iguess (input)
	#          .eq.1 - initial guess is supplied as parameter zinit
	#          .ne.1 - get initial guess from program ode (slower).
	#                  for this the segment wc-ww must lie within
	#                  the polygon.
	#
	#   zinit  initial guess if iguess.eq.1, otherwise ignored (input).
	#          may not be a prevertex z(k)
	#
	#   ier    error flag (input and output).
	#          on input, give ier.ne.0 to suppress error messages.
	#          on output, ier.ne.0 indicates unsuccessful computation --
	#          try again with a better initial guess.
   #
	################ END IGNORE #######################

   # The fortran->R link for complex variables is not vectorised, 
   # so we do this....

   evaluated.points<-complex(length(points))

   for (i in 1:length(points)){
      # Find a near point
         # This sets the variables z0, w0 and k0 for the fortran routine ZSC.
         #   z0     point in the disk near z(ww) at which w(z0) is known and
    	   #          finite (input).
     	   #
   	   #   w0     w(z0)  (input).  the line segment from w0 to ww must
   	   #          lie entirely within the closed polygon.
   	   #
   	   #   k0     k if z0 = z(k) for some k, otherwise 0 (input)
      z0<-vector("complex",1)
      k0<-vector("integer",1)
      w0<-vector("complex",1)
      evaled<-complex(1)

#cat(points[i],z0,w0,as.integer(k0),as.integer(nvertices),prevertices,centre,polyvertices,as.numeric(angles),"\n",sep="\n*")
      cat("point:",points[i],"\n")


      nearest<-.Fortran("NEARW",WW=points[i],ZN=z0,WN=w0,KN=k0,N=as.integer(nvertices),Z=as.complex(prevertices),WC=as.complex(centre),W=as.complex(polyvertices),BETAM=as.numeric(angles))


cat("near found\n")

#   cat(as.numeric(this.point),as.numeric(2),as.numeric(nearest$ZN),as.numeric(nearest$ZN),as.numeric(nearest$WN),"---",as.numeric(nearest$KN),"---",as.numeric(accuracy),as.numeric(1),as.integer(nvertices),as.numeric(complex.scale.factor),as.numeric(Re(prevertices)),as.numeric(Im(prevertices)),as.numeric(centre),as.numeric(Re(polyvertices)),as.numeric(Im(polyvertices)),as.numeric(betam),as.numeric(nptsq),as.numeric(qwork),"---",as.numeric(evaled),"\n",sep="\n*")


      map.ret<-.Fortran("ZSC",WW=points[i],IGUESS=as.numeric(2),ZINIT=as.complex(nearest$ZN),Z0=as.complex(nearest$ZN),W0=as.complex(nearest$WN),K0=as.integer(nearest$KN),EPS=as.numeric(accuracy),IER=as.numeric(1),N=as.integer(nvertices),C=as.complex(complex.scale.factor),Z=as.complex(prevertices),WC=as.complex(centre),W=as.complex(polyvertices),BETAM=as.numeric(angles),NPTSQ=as.integer(nptsq),QWORK=as.numeric(qwork),EVALED=as.complex(evaled))
  
cat("mapped\n")
      # Push the value into the vector
      evaluated.points[i]<-map.ret$EVALED
      cat(i,": original point: ",points[i]," new point: ",map.ret$EVALED,"\n")
   }

   # Return the vector
   return(evaluated.points)


}


# Test evaluation of the map
some.points<-complex(4,c(0.01,0.03,0.002,0),c(0.4,0.00002,0.001,0))

#cat(some.points,nvertices,betam,nptsq,qwork,accuracy=1e-6,prevertices,polyvertices,angles,complex.scale.factor,centre,sep="\n*")

retval<-sc.map.backwards(some.points,nvertices,betam,nptsq,qwork,accuracy=1e-6,prevertices,polyvertices,angles,complex.scale.factor,centre)

# Now we want to create some random points in the polygon and see how our map
# works. We can do this using the splancs library...

#library(splancs)


# Create some data
#poly.rand.data<-csr(as.points(Re(polyvertices),Im(polyvertices)),100)
#complex.poly.rand.data<-complex(50,poly.rand.data[,1],poly.rand.data[,2])


par(mfrow=c(2,1))
#plot(complex.poly.rand.data)

plot(some.points)
#retval<-sc.map.backwards(complex.poly.rand.data,nvertices,betam,nptsq,qwork,accuracy=1e-6,prevertices,polyvertices,angles,complex.scale.factor,centre)
plot(retval)








