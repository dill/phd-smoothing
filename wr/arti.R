# calculate the artifactiness of a smooth...
arti<-function(fit.obj,true.func,bnd){
   ## recreate the S matrix
   # use finite difference to find the second derivatives
   eps<-(1e-15)^(1/4)

   if(any(class(fit.obj)=="gam")){
      k<-length(fit.obj$coef)
   }else{
      k<-length(fit.obj)
   }

   N<-55
   N<-115
   N<-99 # errors?

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
   source("inSide.R")
   onoff<-in.poly(bnd,ip$X[,1],ip$X[,2])
   ep<-list()
   ep$X<-ip$X[onoff,]
   ep$w<-ip$w[onoff]

   # root the weights, since we square them in a bit
   ep$w<-sqrt(ep$w)

   # let's create some matrices
   # finite second differences wrt x and y

   xee<-list(x=ep$X[,1]+2*eps,y=ep$X[,2])
   xe <-list(x=ep$X[,1]+eps,y=ep$X[,2])
   yee<-list(x=ep$X[,1],y=ep$X[,2]+2*eps)
   ye <-list(x=ep$X[,1],y=ep$X[,2]+eps)
   xy <-list(x=ep$X[,1],y=ep$X[,2])
   xye<-list(x=ep$X[,1]+eps,y=ep$X[,2]+eps)
   
   if(any(class(fit.obj)=="wrtps")){
      # deriv evaluation points, this is corny storage, but saves me time...
      devalp<-list()
      devalp[[1]]<-xee
      devalp[[2]]<-xe 
      devalp[[3]]<-yee
      devalp[[4]]<-ye 
      devalp[[5]]<-xy 
      devalp[[6]]<-xye
      
      D.devalp<-list()
      
      knots<-attr(fit.obj,"knots")
   
      for(i in 1:6){
         D.devalp[[i]]<-create_distance_matrix(
                               c(devalp[[i]]$x,knots$x),
                               c(devalp[[i]]$y,knots$y),bnd,
                               start=length(devalp[[i]]$x))
      }
   }else{
      D.devalp<-NULL
   }

   if(any(class(fit.obj)=="gam")){
      # use mgcv's Predict.Matrix if we have a gam object
      dxee<-Predict.matrix(fit.obj$smooth[[1]],xee)
      dxe <-Predict.matrix(fit.obj$smooth[[1]],xe)
      dyee<-Predict.matrix(fit.obj$smooth[[1]],yee)
      dye <-Predict.matrix(fit.obj$smooth[[1]],ye)
      dxy <-Predict.matrix(fit.obj$smooth[[1]],xy)
      dxye<-Predict.matrix(fit.obj$smooth[[1]],xye)
   }else{
      # if we're using the hacked tps, do something similar
      dxee<-Predict.matrix.tps(fit.obj,xee,D.devalp[[1]])
      dxe <-Predict.matrix.tps(fit.obj,xe, D.devalp[[2]])
      dyee<-Predict.matrix.tps(fit.obj,yee,D.devalp[[3]])
      dye <-Predict.matrix.tps(fit.obj,ye, D.devalp[[4]])
      dxy <-Predict.matrix.tps(fit.obj,xy, D.devalp[[5]])
      dxye<-Predict.matrix.tps(fit.obj,xye,D.devalp[[6]])
   }

   # take truth-deriv...
   fdxee<-rep(true.func(ep$X[,1]+2*eps,ep$X[,2]),dim(dxee)[2])
   fdxe <-rep(true.func(ep$X[,1]+eps,ep$X[,2]),dim(dxe)[2])
   fdyee<-rep(true.func(ep$X[,1],ep$X[,2]+2*eps),dim(dyee)[2])
   fdye <-rep(true.func(ep$X[,1],ep$X[,2]+eps),dim(dye)[2])
   fdxy <-rep(true.func(ep$X[,1],ep$X[,2]),dim(dxy)[2])
   fdxye<-rep(true.func(ep$X[,1]+eps,ep$X[,2]+eps),dim(dxye)[2])

   # model derivs
   Dx<-ep$w*(dxee-2*dxe+dxy)/eps^2
   Dy<-ep$w*(dyee-2*dye+dxy)/eps^2
   Dxy<-ep$w*(dxye-dxe-dye+dxy)/eps^2

   # true derivs
   fDx<-ep$w*(fdxee-2*fdxe+fdxy)/eps^2
   fDy<-ep$w*(fdyee-2*fdye+fdxy)/eps^2
   fDxy<-ep$w*(fdxye-fdxe-fdye+fdxy)/eps^2

   # difference derivs
   dDx<-fDx-Dx
   dDy<-fDy-Dy
   dDxy<-fDxy-Dxy

   # actually do the integration
   S<-t(dDx)%*%dDx + 2*t(dDxy)%*%dDxy + t(dDy)%*%dDy 

   # enforce symmetry (from smooth.construct.tp...)
   S <- (S + t(S))/2

   # zero the last three rows and cols
   S[(k-2):k,]<-rep(0,k*3)
   S[,(k-2):k]<-rep(0,k*3)

   # calculate beta^T S beta
   if(any(class(fit.obj)=="gam")){
      beta<-fit.obj$coef
   }else{
      beta<-fit.obj
   }

   int<-t(beta)%*%S%*%beta

   return(int)

}
