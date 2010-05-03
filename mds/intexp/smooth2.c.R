# thin plate spline with squash
# taken from smooth.r in mgcv

# 2D version!

## The constructor for a tprs basis object with MDS modifications.
smooth.construct.mdstp.smooth.spec<-function(object,data,knots){

   if(length(names(data))!=2){
      cat("mdstp can only be used with 2D smooths!\n")
      return(1)
   }

   #first do the MDS stuff
   # NOT TESTED YET!!
   # set up some objects and throw some errors if we need to

#   if(object$xt$bnd){
#      bnd<-object$xt$bnd
#   }else{
#      stop("No boundary supplied!\n")
#   }
#
#   if(is.na(object$xt$refgrid.n){
#      refgridsize<-120
#   }else{
#      refgridsize<-object$xt$refgrid.n
#   }
#
#   if(object$xt$faster){
#      faster<-object$xt$faster
#   }else{
#      faster<-1
#   }
#
#   # create the grid
#   my.grid<-create_refgrid(bnd,120)
#   
#   ## do the MDS on the grid 
#   # create D
#   D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster)
#   
#   # perform mds on D
#   grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)
#   
#   # sample points insertion
#   datanames<-names(data)
#   names(data)<-c("x","y")
#   samp.mds<-insert.mds(data,my.grid,grid.mds,bnd,faster)
#   names(data)<-datanames



   # make the tprs object as usual
   object<-smooth.construct.tp.smooth.spec(object,data,knots)

   ## recreate the S matrix
   # use finite difference to find the second derivatives
   eps<- (1e-15)^(1/4)
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


   # take the boundary (actually something a bit smaller?)
   # mapped it into the space
   bnd<-object$xt$bnd # passed in pre-mapped!

   # set the integration limits
   a<-min(c(bnd$x,bnd$y))
   b<-max(c(bnd$x,bnd$y))
   # take a grid in the mds space
   ip <- mesh(a+(1:N-.5)/N*(b-a),2,rep(2/N,N))

   # knock out those points outside the boundary
   onoff<-inSide(bnd,ip$X[,1],ip$X[,2])

   ep<-list()
   ep$X<-ip$X[onoff,]
   ep$w<-ip$w[onoff]

   # root the weights, since we square them in a bit
   ep$w<-sqrt(ep$w)
   # done?
   #ep$w<-sqrt(1/sum(onoff))

   # let's create some matrices
   # finite second differences wrt x and y
   dxee<-Predict.matrix(object,data.frame(x=ep$X[,1]+2*eps,y=ep$X[,2]))
   dxe <-Predict.matrix(object,data.frame(x=ep$X[,1]+eps,y=ep$X[,2]))
   dxy <-Predict.matrix(object,data.frame(x=ep$X[,1],y=ep$X[,2]))
   dxye<-Predict.matrix(object,data.frame(x=ep$X[,1]+eps,y=ep$X[,2]+eps))
   dyee<-Predict.matrix(object,data.frame(x=ep$X[,1],y=ep$X[,2]+2*eps))
   dye <-Predict.matrix(object,data.frame(x=ep$X[,1],y=ep$X[,2]+eps))

   Dx<-ep$w*(dxee-2*dxe+dxy)/eps^2
   Dy<-ep$w*(dyee-2*dye+dxy)/eps^2
   Dxy<-ep$w*(dxye-dxe-dye+dxy)/eps^2

   # zero some rows
   Dx[(k-1):k,]<-rep(0,k*2)
   Dy[(k-1):k,]<-rep(0,k*2)
   Dxy[(k-1):k,]<-rep(0,k*2)

   ## auto ks based adjustment
   #dat<-matrix(c(data$x,data$y),length(data$x),2)
   #dens.est<-kde(dat,H=Hpi(dat),eval.points=ep$X)
   #dens.est<-1/dens.est$estimate

   # now do the adjustment based on the point density

   # first work out the density at resolution dres
   dres<-N#/1.5
   dgrid<-mesh(a+(1:dres-.5)/dres*(b-a),2,rep(2/dres,dres))
   
   # extract the points we're going to use to calculate the density
   dpoints<-object$xt$dens.points

   # find the grid cells they lie in
   xstart<-min(dgrid$X[,1]); ystart<-min(dgrid$X[,2])
   xdel<-diff(unique(dgrid$X[,1]))[1]
   ydel<-diff(unique(dgrid$X[,2]))[1]
   dxi<-abs(floor((dpoints$x-xstart)/xdel))
   dyj<-abs(floor((dpoints$y-ystart)/ydel))

   # now find the grid cell the integration meshpoints lie in...
   mxi<-abs(floor((ip$X[,1]-xstart)/xdel))
   myj<-abs(floor((ip$X[,2]-ystart)/ydel))

   mxi<-mxi[onoff]
   myj<-myj[onoff]

   dens.est<-table(dxi,dyj)[mxi+sqrt(length(myj))*myj]

   # do the squashing
   sq<-sqrt((dens.est)^3)
   Dx<-sq*Dx
   Dy<-sq*Dy

   # actually do the integration
   fd<-t(Dx)%*%Dx + t(Dxy)%*%Dxy + t(Dy)%*%Dy

   S<-fd

   # enforce symmetry (from smooth.construct.tp...)
   S <- (S + t(S))/2

   object$S[[1]]<-S

   # zero the last two rows and cols
   object$S[[1]][(k-1):k,]<-rep(0,k*2)
   object$S[[1]][,(k-1):k]<-rep(0,k*2)

   cat("max diff=",max(abs(oldS-object$S[[1]])),"\n")

   class(object)<-"mdstp.smooth"
   object
}

# prediction matrix method
Predict.matrix.mdstp.smooth<-function(object,data){
   Predict.matrix.tprs.smooth(object,data)
}
