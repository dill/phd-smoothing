# thin plate spline with squash
# taken from smooth.r in mgcv

## The constructor for a tprs basis object with MDS modifications.
smooth.construct.mdstp.smooth.spec<-function(object,data,knots){
 
   # make the tprs object as usual
   object<-smooth.construct.tp.smooth.spec(object,data,knots)

   # recreate the S matrix
   # use finite difference to find the second derivatives
   eps<- (1e-15)^(1/4)
 

   if(!is.null(object$xt$lims)){

      lims<-object$xt$lims
      sq<-object$xt$sq
      oldS<-object$S[[1]] 

      k<-dim(object$S[[1]])[1]
      S<-matrix(0,k,k)

      for(i in 1:(length(lims)-1)){

         # set some limits
         a<- lims[i]
         b<- lims[i+1]
       
         N<-1000000
       
         # make the candidate x values
         xs<-a+(1:N -0.5)*(b-a)/N
       
         # second difference
         fd<-(Predict.matrix(object,data.frame(x=xs+2*eps))-
             2*Predict.matrix(object,data.frame(x=xs+eps))+
             Predict.matrix(object,data.frame(x=xs)))/eps^2

      # zero the last two rows and cols
      fd[(k-1):k,]<-rep(0,k*2)
      fd[,(k-1):k]<-rep(0,k*2)

#         fd<-fd*sqrt(sq[i]^3)
         fd<-fd*sqrt(sq[i])

         # do the integration
         D<-t(fd)%*%fd
         D<-((b-a)/N)*D
      
         # enforce symmetry (from smooth.construct.tp...)
         D <- (D + t(D))/2
      
         S<-S+D
      }

      object$S[[1]]<-S

      # zero the last two rows and cols
      object$S[[1]][(k-1):k,]<-rep(0,k*2)
      object$S[[1]][,(k-1):k]<-rep(0,k*2)

      cat("max diff=",max(abs(oldS-object$S[[1]])),"\n")
   }

   object
}

