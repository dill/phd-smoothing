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
z<-vector("numeric",vertices)
ret<-.Fortran("SCSOLV",IPRINT=as.numeric(0),IGUESS=as.numeric(1),TOL=as.numeric(1e-6),ERREST=as.numeric(errest),N=as.integer(vertices),C=as.numeric(c.const),Z=as.numeric(z),WC=as.numeric(wc),WRE=as.numeric(Re(w)),WIM=as.numeric(Im(w)),BETAM=as.numeric(betam),NPTSQ=as.integer(nptsq),QWORK=as.numeric(qwork))



# Wrapper for the forwards map
# Forwards is disk->polygon
sc.map.forwards<-function(){
   

}

# And the backwards map
# backwards is polygon->disk
sc.map.backwards<-function(point){
	#   ww     point in the polygon at which z(ww) is desired (input)
	#
	#   iguess (input)
	#          .eq.1 - initial guess is supplied as parameter zinit
	#          .ne.1 - get initial guess from program ode (slower).
	#                  for this the segment wc-ww must lie within
	#                  the polygon.
	#
	#   zinit  initial guess if iguess.eq.1, otherwise ignored (input).
	#          may not be a prevertex z(k)
	#
	#   z0     point in the disk near z(ww) at which w(z0) is known and
	#          finite (input).
	#
	#   w0     w(z0)  (input).  the line segment from w0 to ww must
	#          lie entirely within the closed polygon.
	#
	#   k0     k if z0 = z(k) for some k, otherwise 0 (input)
	#
	#   eps    desired accuracy in answer z(ww)  (input)
	#
	#   ier    error flag (input and output).
	#          on input, give ier.ne.0 to suppress error messages.
	#          on output, ier.ne.0 indicates unsuccessful computation --
	#          try again with a better initial guess.
	#   n,c,z,wc,w,betam,nptsq,qwork     as in scsolv (input)


   # Find a near w
   nearest<-.Fortran()

   map.ret<-.Fortran("zsc",ww=as.numeric(ww),iguess=as.numeric(2),zinit=as.numeric(2),z0,w0,k0,eps,ier,n,c,z,wc,w,betam,nptsq,qwork)

}

# Now we want to create some random points in the polygon and see how our map
# works. We can do this using the splancs library...

library(splancs)


# Create some data
poly.rand.data<-csr(as.points(Re(w),Im(w)),100)













