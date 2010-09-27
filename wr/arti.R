# calculate the artifactiness of a smooth...

arti<-function(fit.obj,true.func,bnd){

   ## recreate the S matrix
   # use finite difference to find the second derivatives
   eps<-(1e-15)^(1/4)

   k<-length(fit.obj$coef)

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


   # set the integration limits
   # make an overly big bounding square
   a<-min(c(bnd$x,bnd$y))
   b<-max(c(bnd$x,bnd$y))
   # take a grid in the mds space
   ip <- mesh(a+(1:N-.5)/N*(b-a),2,rep(2/N,N))
   # knock out those points outside the boundary
   onoff<-inSide(bnd.mds,ip$X[,1],ip$X[,2])
   ep<-list()
   ep$X<-ip$X[onoff,]
   ep$w<-ip$w[onoff]

   # root the weights, since we square them in a bit
   ep$w<-sqrt(ep$w)

   # let's create some matrices
   # finite second differences wrt x and y
   dxee<-predict(fit.obj,data.frame(x=ep$X[,1]+2*eps,y=ep$X[,2]))
   dxe <-predict(fit.obj,data.frame(x=ep$X[,1]+eps,y=ep$X[,2]))
   dyee<-predict(fit.obj,data.frame(x=ep$X[,1],y=ep$X[,2]+2*eps))
   dye <-predict(fit.obj,data.frame(x=ep$X[,1],y=ep$X[,2]+eps))
   dxy <-predict(fit.obj,data.frame(x=ep$X[,1],y=ep$X[,2]))
   dxye<-predict(fit.obj,data.frame(x=ep$X[,1]+eps,y=ep$X[,2]+eps))

   Dx<-ep$w*(dxee-2*dxe+dxy)/eps^2
   Dy<-ep$w*(dyee-2*dye+dxy)/eps^2
   Dxy<-ep$w*(dxye-dxe-dye+dxy)/eps^2

   # actually do the integration
   S<-t(Dx)%*%Dx + 2*t(Dxy)%*%Dxy + t(Dy)%*%Dy

   # enforce symmetry (from smooth.construct.tp...)
   S <- (S + t(S))/2

   # zero the last three rows and cols
   S[[1]][(k-2):k,]<-rep(0,k*3)
   S[[1]][,(k-2):k]<-rep(0,k*3)

   # calculate beta^T S beta?


}
