# Script to call the SCPACK fortran routine 
# The following files are needed from netlib
#   http://www.netlib.org/conformal/scpack
#   http://www.netlib.org/conformal/sclib

# First load the shared object file
dyn.load("scpack.so")

# set number of quadrature points per subinterval
nptsq<-5

# Size of the work array
qsize<-as.integer(nptsq*(2*nvertices+3))

# setup some output variables
errest<-vector("numeric",1)
c.const<-vector("complex",1)
z<-vector("complex",nvertices)
qwork<-vector("numeric",qsize)
qwork<-as.single(qwork)
betam<-vector("numeric",nvertices)

# Compute the map
sc.solution<-.Fortran("scint",N=as.integer(nvertices),BETAM=as.single(betam),W=polyvertices,Z=z,C=c.const,WC=wc,NPTSQ=as.integer(nptsq),ERREST=errest,QSIZE=qsize,QWORK=qwork)

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
sc.map.backwards<-function(points,nvertices,betam,nptsq,qwork,accuracy=as.single(0.0001),prevertices,polyvertices,complex.scale.factor,centre){
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

   # Set up a var to catch the response
   evaluated.points<-complex(length(points))
   
   mapint.call<-.Fortran("mapint",dpoints=points,npoints=as.integer(length(points)),n=as.integer(nvertices),betam=as.single(betam),nptsq=as.integer(nptsq),qwork=as.single(qwork),qsize=as.integer(length(qwork)),accuracy=as.single(accuracy),dz=prevertices,dw=polyvertices,c=complex.scale.factor,wc=centre,dretpoints=evaluated.points,reterrors=as.integer(rep(0,length(points))))

   # Return the vector of points and errors
   ret<-list()
   
   # errors, TRUE if there was an error
   ret$errors<-as.logical(mapint.call$reterrors)
   # return points that did not have errors
   ret$eval.points<-mapint.call$dretpoints[!as.logical(mapint.call$reterrors)]
   # return what did happen for those with errors
   ret$error.points<-mapint.call$dretpoints[as.logical(mapint.call$reterrors)]

   return(ret)
}

