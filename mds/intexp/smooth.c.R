# thin plate spline with squash
# taken from smooth.r in mgcv

## The constructor for a tprs basis object with MDS modifications.
smooth.construct.mdstp.smooth.spec<-function(object,data,knots){
 
   # make the tprs object as usual
   object<-smooth.construct.tp.smooth.spec(object,data,knots)

   # recreate the S object
   # use finite difference to find the second derivatives
   eps<- (1e-15)^(1/4)
 
   # set some limits
   a<- 0
   b<- 1
 
   N<-1000000
 
   # make the candidate x values
   xs<-a+(1:N -0.5)*(b-a)/N
 
   # second difference
   fd<-(Predict.matrix(object,data.frame(x=xs+2*eps))-
       2*Predict.matrix(object,data.frame(x=xs+eps))+
       Predict.matrix(object,data.frame(x=xs)))/eps^2

   oldS<-object$S[[1]] 
   
   object$S[[1]]<-t(fd)%*%fd
   object$S[[1]]<-((b-a)/N)*object$S[[1]]

   # enforce symmetry (from smooth.construct.tp...)
   object$S[[1]] <- (object$S[[1]] + t(object$S[[1]]))/2

   # zero the last two rows and cols
   k<-dim(object$S[[1]])[1]

   object$S[[1]][(k-1):k,]<-rep(0,k*2)
   object$S[[1]][,(k-1):k]<-rep(0,k*2)

   cat("max diff=",max(abs(oldS-object$S[[1]])),"\n")
 
   object
}

