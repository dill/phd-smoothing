# thin plate spline with squash
# taken from smooth.r in mgcv

# 2D version!

## The constructor for a tprs basis object with MDS modifications.
smooth.construct.mdstp.smooth.spec<-function(object,data,knots){

   if(length(names(data))!=2){
      cat("mdstp can only be used with 2D smooths!\n")
      return(1)
   }

   # make the tprs object as usual
   object<-smooth.construct.tp.smooth.spec(object,data,knots)

   ## recreate the S matrix
   # use finite difference to find the second derivatives
   eps<- (1e-15)^(1/4)
   oldS<-object$S[[1]] 

   k<-dim(object$S[[1]])[1]
   S<-matrix(0,k,k)

   N<-100

   # mesh function
   mesh <- function(x,d,w=1/length(x)+x*0) { 
      n <- length(x) 
      W <- X <- matrix(0,n^d,d) 
      for (i in 1:d) {
         X[,i] <- x;W[,i] <- w
         x<- rep(x,rep(n,length(x))) 
         w <- rep(w,rep(n,length(w)))
      } 
      w <- exp(rowSums(log(W))) ## column product of W gives weights 
      list(X=X,w=w) ## each row of X gives co-ordinates of a node
   }

   # evaluation points
   ep <- mesh((1:N-.5)/N*2-1,2,rep(2/N,N))

   # let's create some matrices
   Dx<-ep$w*(Predict.matrix(object,data.frame(x=ep$X[,1]+2*eps,y=ep$X[,2]))-
         2*Predict.matrix(object,data.frame(x=ep$X[,1]+eps,y=ep$X[,2]))+
         Predict.matrix(object,data.frame(x=ep$X[,1],y=ep$X[,2])))/eps^2
   Dy<-ep$w*(Predict.matrix(object,data.frame(x=ep$X[,1],y=ep$X[,2]+2*eps))-
         2*Predict.matrix(object,data.frame(x=ep$X[,1],y=ep$X[,2]+eps))+
         Predict.matrix(object,data.frame(x=ep$X[,1],y=ep$X[,2])))/eps^2

   ## auto ks based adjustment
   dat<-matrix(c(data$x,data$y),length(data$x),2)
   dens.est<-kde(dat,H=Hpi(dat),eval.points=ep$X)
   sq<-sqrt((1/dens.est$estimate^3))

#   Dx<-sq*Dx
#   Dy<-sq*Dy

   # actually do the integration
   fd<-t(Dx)%*%Dx + t(Dx)%*%Dy + t(Dy)%*%Dy

   ## do the integration
   #S<-t(fd)%*%fd
   #S<-((b-a)/N)*S
   
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
