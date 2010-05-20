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

   # take the boundary
   # map it into the space
#   bnd.mds<-insert.mds(object$xt$bnd,object$xt$op,object$xt$mds.obj,bnd,faster=0)
#   bnd.mds<-data.frame(x=bnd.mds[,1],y=bnd.mds[,2])
   bnd.mds<-object$xt$bnd.mds
   #plot(bnd.mds,type="l")

   # set the integration limits
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
   #plot(bnd.mds,type="l",col="red")
   #points(ep$X,pch=19,cex=0.3)


   # root the weights, since we square them in a bit
#   ep$w<-sqrt(ep$w)
   # done?
   #ep$w<-sqrt(1/sum(onoff))

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

   # zero some rows
#   Dx[(k-1):k,]<-rep(0,k*2)
#   Dy[(k-1):k,]<-rep(0,k*2)
#   Dxy[(k-1):k,]<-rep(0,k*2)

   ## auto ks based adjustment
   #dat<-matrix(c(data$x,data$y),length(data$x),2)
   #dens.est<-kde(dat,H=Hpi(dat),eval.points=ep$X)
   #dens.est<-1/dens.est$estimate

#   ##################################################
#   # now do the adjustment based on the point density
#
#   # lets generate some grids
#
#   # create base grid
#   m<-25;n<-25 # need to set these somewhere
#   xmin<-min(bnd$x)
#   ymin<-min(bnd$y)
#   xmax<-max(bnd$x)
#   ymax<-max(bnd$y)
#   # create the grid
#   xm <- seq(xmin,xmax,length=m)
#   yn<-seq(ymin,ymax,length=n)
#
#   # one extra grid cell bigger on all sides
#   xdel<-diff(xm)[1]
#   ydel<-diff(yn)[1]
#   blx<-xm[1]-xdel # big left x
#   brx<-xm[length(xm)]+xdel # big right x
#   bby<-yn[1]-ydel # big bottom y
#   bty<-yn[length(yn)]+ydel # big top y
#
#   # now create 4 grids, one for each corner
#   # top left, top right, bottom left, bottom right
#   tlg<-list(x=rep(c(blx,xm),n+1),y=rep(c(bty,yn),rep(m+1,n+1)))
#   trg<-list(x=rep(c(xm,brx),n+1),y=rep(c(bty,yn),rep(m+1,n+1)))
#   blg<-list(x=rep(c(blx,xm),n+1),y=rep(c(yn,bby),rep(m+1,n+1)))
#   brg<-list(x=rep(c(xm,brx),n+1),y=rep(c(yn,bby),rep(m+1,n+1)))
#
#   # now just take the full squares that are inside
#   onoff<-inSide(bnd,tlg$x,tlg$y)
#   onoff<-onoff & inSide(bnd,trg$x,trg$y)
#   onoff<-onoff & inSide(bnd,brg$x,brg$y)
#   onoff<-onoff & inSide(bnd,blg$x,blg$y)
#
#   tlg<-pe(tlg,onoff)
#   trg<-pe(trg,onoff)
#   blg<-pe(blg,onoff)
#   brg<-pe(brg,onoff)
#
#   # check that this was okay...
#   #plot(tlg,pch=19)
#   #points(trg,pch=19,col="green",cex=0.9)
#   #points(brg,pch=19,col="blue",cex=0.8)
#   #points(blg,pch=19,col="orange",cex=0.7)
#
#   # make a list of all these points
#   biglist<-list(x=c(tlg$x,trg$x,brg$x,blg$x),
#                 y=c(tlg$y,trg$y,brg$y,blg$y))
#
#   # MDS these points...
#   biglist.mds<-insert.mds(biglist,object$xt$op,object$xt$mds.obj,bnd,faster=1)
#
#   # pull them back out in the right order
#   len<-length(tlg$x)
#   mtlg<-biglist.mds[1:len,]
#   mtrg<-biglist.mds[(len+1):(2*len),]
#   mbrg<-biglist.mds[(2*len+1):(3*len),]
#   mblg<-biglist.mds[(3*len+1):(4*len),]
#
#   # again, check that worked!
#   #plot(mtlg,pch=19)
#   #points(mtrg,pch=19,col="green",cex=0.9)
#   #points(mbrg,pch=19,col="blue",cex=0.8)
#   #points(mblg,pch=19,col="orange",cex=0.7)
#
#   # grid resolution - number of divisions of the other grid
#   # to make
#   gres<-10
#
#   pts.x<-c()
#   pts.y<-c()
#
#   # for every quadrilateral
#   for(i in 1:len){
#      # create the divisions on the top and bottom
#      tlx<-seq(mtlg[i,1],mtrg[i,1],len=gres)
#      tly<-seq(mtlg[i,2],mtrg[i,2],len=gres)
#      blx<-seq(mblg[i,1],mbrg[i,1],len=gres)
#      bly<-seq(mblg[i,2],mbrg[i,2],len=gres)
#
#      # split those divisions
#      for(j in 1:gres){
#         pts.x<-c(pts.x,seq(tlx[j],blx[j],len=gres))
#         pts.y<-c(pts.y,seq(tly[j],bly[j],len=gres))
#      }
#   }
#
#   dpoints<-list(x=pts.x,y=pts.y)
#
#   # work out the density at resolution dres
#   # at the moment ths is just the same as doing this for the
#   # integration grid, so we can replace that eventually...
#   dres<-N#/1.5
#   dgrid<-mesh(a+(1:dres-.5)/dres*(b-a),2,rep(2/dres,dres))
#
#   # find the grid cells they lie in
#   xstart<-min(dgrid$X[,1]); ystart<-min(dgrid$X[,2])
#   xdel<-diff(unique(dgrid$X[,1]))[1]
#   ydel<-diff(unique(dgrid$X[,2]))[1]
#   dxi<-abs(floor((dpoints$x-xstart)/xdel))
#   dyj<-abs(floor((dpoints$y-ystart)/ydel))
#
#   # now find the grid cell the integration meshpoints lie in...
#   mxi<-abs(floor((ip$X[,1]-xstart)/xdel))
#   myj<-abs(floor((ip$X[,2]-ystart)/ydel))
#
#   onoff<-inSide(bnd.mds,ip$X[,1],ip$X[,2])
#   mxi<-mxi[onoff]
#   myj<-myj[onoff]
#
#   dens.est<-table(dxi,dyj)[mxi+sqrt(length(myj))*myj]
#
#   # image of the density function
##   X11()
##   denf<-table(dxi,dyj)
##   denf[-(mxi+sqrt(length(myj))*myj)]<-NA
##   image(denf,col=heat.colors(1000))
#
#
#   #################################################
#
#   # do the squashing
#   sq<-sqrt((dens.est)^3)
#   Dx<-sq*Dx
#   Dy<-sq*Dy
#   Dxy<-sq*Dxy

   # actually do the integration
   S<-t(Dx)%*%Dx + t(Dxy)%*%Dxy + t(Dy)%*%Dy

   # enforce symmetry (from smooth.construct.tp...)
   S <- (S + t(S))/2

   object$S[[1]]<-S

   # zero the last two rows and cols
   object$S[[1]][(k-1):k,]<-rep(0,k*2)
   object$S[[1]][,(k-1):k]<-rep(0,k*2)

   cat("max diff=",max(abs(oldS-object$S[[1]])),"\n")
#   hist(oldS-object$S[[1]])
#   print(quantile(oldS-object$S[[1]]))
#   cat("mean diff=",mean(oldS-object$S[[1]]),"\n")
#   X11()
#   par(mfrow=c(1,2))
#   image(oldS,col=heat.colors(1000))
#   image(object$S[[1]],col=heat.colors(1000))

   object$oldS<-oldS

   class(object)<-"mdstp.smooth"
   object
}

# prediction matrix method
Predict.matrix.mdstp.smooth<-function(object,data){
   Predict.matrix.tprs.smooth(object,data)
}
