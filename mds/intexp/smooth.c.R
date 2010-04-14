# thin plate spline with squash
# taken from smooth.r in mgcv

## The constructor for a tprs basis object with MDS modifications.
smooth.construct.mdstp.smooth.spec<-function(object,data,knots){

#   dens.est<-kde(data$x,h=hpi(data$x),eval.points=data$x)
#   dat<-data$x
#   data$x<-data$x*(dens.est$estimate)^2

   # make the tprs object as usual
   object<-smooth.construct.tp.smooth.spec(object,data,knots)

   # recreate the S matrix
   # use finite difference to find the second derivatives
   eps<- (1e-15)^(1/4)

   oldS<-object$S[[1]] 

   k<-dim(object$S[[1]])[1]
   S<-matrix(0,k,k)

   # integration limits
   a<-0
   b<-1
   a<-min(data$x)
   b<-max(data$x)

   N<-1000
   
   # make the candidate x values
   xs<-a+(1:N -0.5)*(b-a)/N
   
   # auto ks based adjustment
   dens.est<-kde(data$x,h=hpi(data$x),eval.points=xs)
   sq<-dens.est$estimate
   
   # second difference
   fd<-(Predict.matrix(object,data.frame(x=xs+2*eps))-
       2*Predict.matrix(object,data.frame(x=xs+eps))+
       Predict.matrix(object,data.frame(x=xs)))/eps^2

   # zero the last row
   #fd[(k-1):k,]<-rep(0,k*2)
   #fd[,(k-1):k]<-rep(0,k*2)
   #fd[k,]<-rep(0,k)
   #fd[,k]<-rep(0,k)

   # do something here to adjust lambda
   #fd<-fd*100000
   fd<-fd*sqrt((1/sq)^3)
#   fd<-fd*(sq)^2

   # do the integration
   S<-t(fd)%*%fd
   S<-((b-a)/N)*S
   
   # enforce symmetry (from smooth.construct.tp...)
   S <- (S + t(S))/2

   object$S[[1]]<-S

   # zero the last two rows and cols
   #object$S[[1]][(k-1):k,]<-rep(0,k*2)
   #object$S[[1]][,(k-1):k]<-rep(0,k*2)
   object$S[[1]][k,]<-rep(0,k)
   object$S[[1]][,k]<-rep(0,k)

   cat("max diff=",max(abs(oldS-object$S[[1]])),"\n")

   class(object)<-"mdstp.smooth"
   object
}

# prediction matrix method
Predict.matrix.mdstp.smooth<-function(object,data){
   Predict.matrix.tprs.smooth(object,data)
}
