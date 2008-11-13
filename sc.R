# Script to call the SCPACK fortran routine 
# The following files are needed from netlib
#   http://www.netlib.org/conformal/scpack
#   http://www.netlib.org/conformal/sclib

# Need to follow the same pattern as specified in
# http://www.netlib.org/conformal/scdoc

# First load the shared object file
dyn.load("scpack.so")

### Create the variables
# Number of vertices
nvertices<-4
# Position of vertices
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,10,0)
polyvertices[2]<-complex(1,0,10)
polyvertices[3]<-complex(1,-10,0)
polyvertices[4]<-complex(1,0,-10)
attr(polyvertices,"Csingle")

wc<-complex(1,0,sqrt(2))
betam<-vector("numeric",nvertices)
betam[1]<-1.0
betam[2]<--0.5
betam[3]<--2.0
betam[4]<--0.5
betam<-as.single(betam)
# Check to see if the ANGLES routine gives the same answer as
# above.
#.Fortran("ANGLES",N=as.integer(nvertices),W=polyvertices,BETAM=as.single(betam))


#################################
### Test problem - L-shape
#nvertices<-6
#polyvertices<-vector("complex",nvertices)
#polyvertices[1]<-complex(1,0,0)
#polyvertices[2]<-complex(1,2,0)
#polyvertices[3]<-complex(1,2,1)
#polyvertices[4]<-complex(1,1,1)
#polyvertices[5]<-complex(1,1,2)
#polyvertices[6]<-complex(1,0,2)
#####
#betam<-vector("numeric",nvertices)
#angle.ret<-.Fortran("ANGLES",N=as.integer(nvertices),W=polyvertices,BETAM=as.single(betam))
#betam<-angle.ret$BETAM
#wc<-complex(1,0.5,0.5)
#################################


#################################
### Test problem 2 - from scdoc
#nvertices<-4
#
#polyvertices<-vector("complex",nvertices)
#polyvertices[1]<-complex(1,0,1)
#polyvertices[2]<-complex(1,0,0)
#polyvertices[3]<-complex(1,1e20,1e20)
#polyvertices[4]<-complex(1,0,0)
#
## approximate centre of w
#wc<-complex(1,0,sqrt(2))
#
## angles
#betam<-vector("numeric",nvertices)
#betam[1]<-1
#betam[2]<--0.5
#betam[3]<--2
#betam[4]<--0.5
#################################


# set number of quadrature points per subinterval
nptsq<-5

# Size of the work array
qsize<-as.integer(nptsq*(2*nvertices+3))

# setup some output variables
errest<-vector("numeric",1)
c.const<-vector("complex",1)
attr(c.const,"Csingle")
z<-vector("complex",nvertices)
attr(z,"Csingle")
qwork<-vector("numeric",qsize)
qwork<-as.single(qwork)

sc.solution<-.Fortran("scint",N=as.integer(nvertices),BETAM=betam,W=polyvertices,Z=z,C=c.const,WC=wc,NPTSQ=as.integer(nptsq),ERREST=errest,QSIZE=qsize,QWORK=qwork)


# Set some variables
prevertices<-sc.solution$Z
centre<-sc.solution$WC
complex.scale.factor<-sc.solution$C
betam<-sc.solution$BETAM
qwork<-sc.solution$QWORK

# Wrapper for the forwards map
# Forwards is disk->polygon
sc.map.forwards<-function(){
   

}

# And the backwards map
# backwards is polygon->disk
sc.map.backwards<-function(points,nvertices,betam,nptsq,qwork,accuracy=1e-3,prevertices,polyvertices,complex.scale.factor,centre){
   # Arguments
   #  points                  vector of points at which we want to evaluate the map
   #  nvertices               number of vertices (not >20)
   #  betam                   array of external angles
   #  nptsq                   number of points per subinterval
   #  qwork                   quadrature work array
   #  accuracy                desired accuracy in output (default 1e-3)
   #  prevertices             complex vector of prevertices
   #  polyvertices            complex vector of polgyon vertices
   #  complex.scale.factor    complex scaling factor
   #  centre                  approximate centre of the polygon

   ############## IGNORE AT THE MOMENT ##############
	#   iguess (input)
	#          .eq.1 - initial guess is supplied as parameter zinit
	#          .ne.1 - get initial guess from program ode (slower).
	#                  for this the segment wc-ww must lie within
	#                  the polygon.
	#   zinit  initial guess if iguess.eq.1, otherwise ignored (input).
	#          may not be a prevertex z(k)
	#   ier    error flag (input and output).
	#          on input, give ier.ne.0 to suppress error messages.
	#          on output, ier.ne.0 indicates unsuccessful computation --
	#          try again with a better initial guess.
	################ END IGNORE #######################

   # The fortran->R link for complex variables is not vectorised, 
   # so we do this....
   evaluated.points<-complex(length(points))
#   attr(evaluated.points,"Csingle")   
#   attr(points,"Csingle")   
#   attr(c,"Csingle")   
#   attr(prevertices,"Csingle")
#   attr(polyvertices,"Csingle")
#   attr(wc,"Csingle")
   
   mapint.call<-.Fortran("mapint",dpoints=points,npoints=as.integer(length(points)),n=as.integer(nvertices),betam=as.single(betam),nptsq=as.integer(nptsq),qwork=as.single(qwork),qsize=as.integer(length(qwork)),accuracy=as.single(accuracy),dz=prevertices,dw=polyvertices,c=complex.scale.factor,wc=centre,dretpoints=evaluated.points)

   # Return the vector
   return(evaluated.points)
}


# Test evaluation of the map
#some.points<-complex(4,c(0.01,0.03,0.002,0),c(0.4,0.00002,0.001,0))

# L shape test
#some.points<-complex(4,c(0.5,1,0.5,0.5),c(0.5,0.5,1,1.5))

#cat(some.points,nvertices,betam,nptsq,qwork,accuracy=1e-6,prevertices,polyvertices,angles,complex.scale.factor,centre,sep="\n*")

#retval<-sc.map.backwards(some.points,nvertices,betam,nptsq,qwork,accuracy=1e-6,prevertices,polyvertices,angles,complex.scale.factor,centre)

# Now we want to create some random points in the polygon and see how our map
# works. We can do this using the splancs library...

library(splancs)


# Create some data
poly.rand.data<-csr(as.points(Re(polyvertices),Im(polyvertices)),100)
complex.poly.rand.data<-complex(100,poly.rand.data[,1],poly.rand.data[,2])


par(mfrow=c(2,1))
plot(complex.poly.rand.data)

#plot(some.points)
accuracy<-1e-3
retval<-sc.map.backwards(complex.poly.rand.data,nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)
plot(retval)


