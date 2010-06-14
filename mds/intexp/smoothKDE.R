# thin plate spline with squash
# taken from smooth.r in mgcv

# 2D version! KDE hack

## The constructor for a tprs basis object with MDS modifications.
smooth.construct.mdstp.smooth.spec<-function(object,data,knots){
library(ks)
   if(length(names(data))!=2){
      cat("mdstp can only be used with 2D smooths!\n")
      return(1)
   }

   # make the tprs object as usual
   object<-smooth.construct.tp.smooth.spec(object,data,knots)

   ## recreate the S matrix
   # use finite difference to find the second derivatives
   eps<-(1e-15)^(1/4)
   oldS<-object$S[[1]] 

   k<-dim(object$S[[1]])[1]

   N<-100

   ### first need to create the mesh we want to integrate over
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

   # take the boundary
   # map it into the space
   bnd<-object$xt$bnd
   int.bnd<-bnd


   bnd.mds<-insert.mds(int.bnd,object$xt$op,object$xt$mds.obj,bnd,faster=0)#,debug=1)
   bnd.mds<-data.frame(x=bnd.mds[,1],y=bnd.mds[,2])
   #plot(bnd.mds,type="l")

   # set the integration limits
   # just make an overly big bounding box
   a<-min(c(bnd.mds$x,bnd.mds$y))
   b<-max(c(bnd.mds$x,bnd.mds$y))
   # take a grid in the mds space
   ip <- mesh(a+(1:N-.5)/N*(b-a),2,rep(2/N,N))

   # knock out those points outside the boundary
   onoff<-inSide(bnd.mds,ip$X[,1],ip$X[,2])

   ep<-list()
   ep$X<-ip$X[onoff,]
   ep$w<-ip$w[onoff]

   # plot the integration grid
   #plot(ep$X,pch=19,cex=0.3)
   #lines(bnd.mds,type="l",col="red")
   #X11()

   # root the weights, since we square them in a bit
   ep$w<-sqrt(ep$w)

   # let's create some matrices
   # finite second differences wrt x and y
   dxee<-Predict.matrix(object,data.frame(x=ep$X[,1]+2*eps,y=ep$X[,2]))
   dxe <-Predict.matrix(object,data.frame(x=ep$X[,1]+eps,y=ep$X[,2]))
   dyee<-Predict.matrix(object,data.frame(x=ep$X[,1],y=ep$X[,2]+2*eps))
   dye <-Predict.matrix(object,data.frame(x=ep$X[,1],y=ep$X[,2]+eps))
   dxy <-Predict.matrix(object,data.frame(x=ep$X[,1],y=ep$X[,2]))
   dxye<-Predict.matrix(object,data.frame(x=ep$X[,1]+eps,y=ep$X[,2]+eps))

   Dx<-ep$w*(dxee-2*dxe+dxy)/eps^2
   Dy<-ep$w*(dyee-2*dye+dxy)/eps^2
   Dxy<-ep$w*(dxye-dxe-dye+dxy)/eps^2

   #this.dat<-eval(parse(text=paste("data$",object$term,sep="")))
   this.dat<-matrix(c(data$x,data$y),length(data$x),2)
   dens.est<-kde(this.dat,H=Hpi(this.dat),eval.points=ep$X)
   sq<-sqrt(dens.est$estimate^3)



   #################################################
   # do the squashing
#   sq<-sqrt((dens.est)^3)
   Dx<-sq*Dx
   Dy<-sq*Dy
   Dxy<-sq*Dxy

   # actually do the integration
   S<-t(Dx)%*%Dx + t(Dxy)%*%Dxy + t(Dy)%*%Dy

   # enforce symmetry (from smooth.construct.tp...)
   S <- (S + t(S))/2

   # store the object
   object$S[[1]]<-S

   # zero the last three rows and cols
   object$S[[1]][(k-2):k,]<-rep(0,k*3)
   object$S[[1]][,(k-2):k]<-rep(0,k*3)

   # uncomment to return the old version of S
   #object$oldS<-oldS

   class(object)<-"mdstp.smooth"
   object
}

# prediction matrix method
Predict.matrix.mdstp.smooth<-function(object,data){
   Predict.matrix.tprs.smooth(object,data)
}
