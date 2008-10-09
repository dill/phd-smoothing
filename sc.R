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
vertices<-4

# Position of vertices
w<-vector("complex",vertices)
w[1]<-complex(1,10,0)
w[2]<-complex(1,0,10)
w[3]<-complex(1,-10,0)
w[4]<-complex(1,0,-10)

# set number of quadrature points per subinterval
nptsq<-5

# We use the fortran function ANGLES to compute the angles we need
betam<-vector("numeric",vertices)
angle.ret<-.Fortran("ANGLES",N=as.integer(vertices),WRE=as.numeric(Re(w)),WIM=as.numeric(Im(w)),BETAM=as.numeric(betam))
betam<-angle.ret$BETAM

# Now use qinit to initialise the Gauss-Jacobi quadrature
qwork<-vector("numeric",nptsq*(2*vertices+3))
qinit.ret<-.Fortran("QINIT",N=as.integer(vertices),BETAM=as.numeric(betam),NPTSQ=as.integer(nptsq),QWORK=as.numeric(qwork))
qwork<-qinit.ret$QWORK

# Finally call the SCSOLV routine and find the parameters
# IPRINT and IGUESS must be given to "avoid accidental exact solution"
errest<-vector("numeric",1)
c.const<-vector("numeric",2)
wc<-c(0,sqrt(2))
zre<-vector("numeric",vertices)
zim<-vector("numeric",vertices)
ret<-.Fortran("SCSOLV",IPRINT=as.numeric(0),IGUESS=as.numeric(1),TOL=as.numeric(1e-6),ERREST=as.numeric(errest),N=as.integer(vertices),C=as.numeric(c.const),ZRE=as.numeric(zre),ZIM=as.numeric(zim),WC=as.numeric(wc),WRE=as.numeric(Re(w)),WIM=as.numeric(Im(w)),BETAM=as.numeric(betam),NPTSQ=as.integer(nptsq),QWORK=as.numeric(qwork))


# Set some variables
prevertices<-ret$Z

# Wrapper for the forwards map
# Forwards is disk->polygon
sc.map.forwards<-function(){
   

}

# And the backwards map
# backwards is polygon->disk
sc.map.backwards<-function(points,nvertices,betam,nptsq,qwork,accuracy=1e-6i,prevertices,polyvertices,angles,complex.scale.factor){
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
      #z0<-c()
      #w0<-c()
      #k0<-c()
      nearest<-.Fortran("nearw",ww=as.numeric(points[i]),zn=as.numeric(z0),wn=as.numeric(w0),kn=as.numeric(k0),n=as.integer(nvertices),zre=as.numeric(Re(prevertices)),zim=as.numeric(Im(prevertices)),wc=c(),wre=as.numeric(Re(polyvertices)),wim=as.numeric(Im(polyvertices)),betam=as.numeric(angles))


      map.ret<-.Fortran("zsc",ww=as.numeric(points[i]),iguess=as.numeric(2),zinit=as.numeric(2),z0=as.numeric(nearest$z0,w0=as.numeric(nearest$w0),k0=as.numeric(nearest$k0),eps=as.numeric(accuracy),ier=as.numeric(1),n=as.integer(vertices),c=as.numeric(complex.scale.factor),z=prevertices,wc=c(0,0),wre=as.nmumeric(Re(w)),wim=as.numeric(Im(w)),betam=as.numeric(betam),nptsq=as.numeric(nptsq),qwork=as.numeric(qwork),evaled=as.numeric(evaled))
  
      # Push the value into the vector 
      evaluated.points[i]<-map.ret$evaled
   }

   # Return the vector
   return(evaluated.points)


}

# Now we want to create some random points in the polygon and see how our map
# works. We can do this using the splancs library...

library(splancs)


# Create some data
poly.rand.data<-csr(as.points(Re(w),Im(w)),100)













