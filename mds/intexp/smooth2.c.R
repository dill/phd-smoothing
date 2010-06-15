# thin plate spline with squash
# taken from smooth.r in mgcv

# 2D version!

## The constructor for a tprs basis object with MDS modifications.
smooth.construct.mdstp.smooth.spec<-function(object,data,knots){

   if(length(names(data))!=2){
      cat("mdstp can only be used with 2D smooths!\n")
      return(1)
   }

   # set true to create thesis diagram
   # REMOVE in production version :)
   dia.densmap<-FALSE
   dia.densmap<-TRUE

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

   # interpolate the boundary
   # weird things happen here
   #xb<-matrix(c(bnd$x,bnd$x[c(2:length(bnd$x),1)]),length(bnd$x),2)[-length(bnd$x),] 
   #xb<-xb[-dim(xb),]
   #yb<-matrix(c(bnd$y,bnd$y[c(2:length(bnd$y),1)]),length(bnd$y),2)[-length(bnd$y),] 
   #yb<-yb[-dim(yb),]
   #int.bnd<-list(x=vecseq(xb,10),
   #              y=vecseq(yb,10))

   bnd.mds<-insert.mds(int.bnd,object$xt$op,object$xt$mds.obj,bnd,faster=0)#,debug=1)
   bnd.mds<-data.frame(x=bnd.mds[,1],y=bnd.mds[,2])
   #plot(bnd.mds,type="l")

   # set the integration limits
   # just make an overly big bounding square
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

   ##################################################
   # now do the adjustment based on the point density
   # almost all of this section consists of generating grids 

   # create base MDS grid
   if(is.null(object$xt$b.grid)){
      m<-25;n<-25 # need to set these somewhere
   }else{
      m<-object$xt$b.grid[1]
      n<-object$xt$b.grid[2]
   }

   xmin<-min(bnd$x)
   ymin<-min(bnd$y)
   xmax<-max(bnd$x)
   ymax<-max(bnd$y)
   # create the grid
   xm<-seq(xmin,xmax,length=m)
   yn<-seq(ymin,ymax,length=n)

   # one extra grid cell bigger on all sides
   xdel<-diff(xm)[1]
   ydel<-diff(yn)[1]
   blx<-xm[1]-xdel # big left x
   brx<-xm[length(xm)]+xdel # big right x
   bby<-yn[1]-ydel # big bottom y
   bty<-yn[length(yn)]+ydel # big top y

   # now create 4 grids, one for each corner
   # top left, top right, bottom left, bottom right
   tlg<-list(x=rep(c(blx,xm),n+1),y=rep(c(bty,yn),rep(m+1,n+1)))
   trg<-list(x=rep(c(xm,brx),n+1),y=rep(c(bty,yn),rep(m+1,n+1)))
   blg<-list(x=rep(c(blx,xm),n+1),y=rep(c(yn,bby),rep(m+1,n+1)))
   brg<-list(x=rep(c(xm,brx),n+1),y=rep(c(yn,bby),rep(m+1,n+1)))

   # now just take the squares that are totally inside
   onoff<-inSide(bnd,tlg$x,tlg$y)
   onoff<-onoff & inSide(bnd,trg$x,trg$y)
   onoff<-onoff & inSide(bnd,brg$x,brg$y)
   onoff<-onoff & inSide(bnd,blg$x,blg$y)

   tlg<-pe(tlg,onoff)
   trg<-pe(trg,onoff)
   blg<-pe(blg,onoff)
   brg<-pe(brg,onoff)

   # thesis diagam - density map
   if(dia.densmap){
      pdf(file="densgrid.pdf",width=5,height=5)
      #X11()
      par(mfrow=c(2,2),mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7,las=1)
      xlims<-c(min(tlg$x,trg$x,brg$x,blg$x),max(tlg$x,trg$x,brg$x,blg$x))
      ylims<-c(min(tlg$y,trg$y,brg$y,blg$y),max(tlg$y,trg$y,brg$y,blg$y))
      plot(bnd,lwd=2,type="l",asp=1,xlab="x",ylab="y",xlim=xlims,ylim=ylims)
      points(tlg,pch=19,cex=0.2,col="red")
      points(trg,pch=19,cex=0.2,col="red")
      points(brg,pch=19,cex=0.2,col="red")
      points(blg,pch=19,cex=0.2,col="red")
   }

   # check that this was okay...
   #plot(tlg,pch=19,asp=1)
   #points(trg,pch=19,col="green",cex=0.9)
   #points(brg,pch=19,col="blue",cex=0.8)
   #points(blg,pch=19,col="orange",cex=0.7)

   # make a list of all these points
   biglist<-list(x=c(tlg$x,trg$x,brg$x,blg$x),
                 y=c(tlg$y,trg$y,brg$y,blg$y))

   # MDS these points...
   biglist.mds<-insert.mds(biglist,object$xt$op,object$xt$mds.obj,bnd,faster=1)

   # pull them back out in the right order
   len<-length(tlg$x)
   mtlg<-biglist.mds[1:len,]
   mtrg<-biglist.mds[(len+1):(2*len),]
   mbrg<-biglist.mds[(2*len+1):(3*len),]
   mblg<-biglist.mds[(3*len+1):(4*len),]

   # thesis diagam - density map
   if(dia.densmap){
      xlims<-c(min(bnd.mds[,1]),max(bnd.mds[,1]))
      ylims<-c(min(bnd.mds[,2]),max(bnd.mds[,2]))
      plot(mtlg,pch=19,asp=1,cex=0.2,las=1,xlab="x*",ylab="y*",xlim=xlims,ylim=ylims,col="red")
      lines(bnd.mds,lwd=2)
      points(mtrg,pch=19,cex=0.2,col="red")
      points(mbrg,pch=19,cex=0.2,col="red")
      points(mblg,pch=19,cex=0.2,col="red")
   }

   # again, check that worked!
   #plot(mtlg,pch=19,asp=1)
   #points(mtrg,pch=19,col="green",cex=0.9)
   #points(mbrg,pch=19,col="blue",cex=0.8)
   #points(mblg,pch=19,col="orange",cex=0.7)

   # grid resolution - number of divisions of the other grid
   # to make
   gres<-10

   # do the interpolation, cut the boxes into 10 on each side,
   # then join up the lines...

   # do something clever with vectorised seq()
   # put mtlg[,1] and mtrg[,1] into a 2xlength matrix,
   # same for the others
   txmat<-matrix(c(mtlg[,1],mtrg[,1]),length(mtrg[,1]),2)
   tymat<-matrix(c(mtlg[,2],mtrg[,2]),length(mtrg[,2]),2)
   bxmat<-matrix(c(mblg[,1],mbrg[,1]),length(mbrg[,1]),2)
   bymat<-matrix(c(mblg[,2],mbrg[,2]),length(mbrg[,2]),2)

   # do some vecseq magic...
   tlx<-vecseq(txmat,gres)
   tly<-vecseq(tymat,gres)
   blx<-vecseq(bxmat,gres)
   bly<-vecseq(bymat,gres)

   pts.x<-c()
   pts.y<-c()

   for(i in 1:gres){
      xs<-vecseq(matrix(c(tlx[,i],blx[,i]),length(blx[,i]),2),gres)
      pts.x<-c(pts.x,c(t(xs)))

      ys<-vecseq(matrix(c(tly[,i],bly[,i]),length(bly[,i]),2),gres)
      pts.y<-c(pts.y,c(t(ys)))
   }

   # there are lots of duplicates here at the corners and edges, so remove them
   dunip<-unique(matrix(c(pts.x,pts.y),length(pts.x),2),MARGIN=1)
   dpoints<-list(x=dunip[,1],y=dunip[,2])

   #dpoints<-list(x=pts.x,y=pts.y)

#   if(dia.densmap){
#      # check that the interpolation worked...
#      #X11()
#      plot(dpoints,pch=19,cex=0.3,col="green",xlab="x*",ylab="y*",asp=1,xlim=xlims,ylim=ylims)
#      lines(bnd.mds,lwd=2)
#      #X11()
#   }
   # work out the density at resolution dres
   # at the moment ths is just the same as doing this for the
   # integration grid, so we can replace that eventually...
   dres<-N#*0.75
   dgrid<-mesh(a+(1:dres-.5)/dres*(b-a),2,rep(2/dres,dres))

   # find the grid cells they lie in
   xstart<-min(dgrid$X[,1]); ystart<-min(dgrid$X[,2])
   xdel<-diff(unique(dgrid$X[,1]))[1]
   ydel<-diff(unique(dgrid$X[,2]))[1]
   dxi<-abs(floor((dpoints$x-xstart)/xdel))+1
   dyj<-abs(floor((dpoints$y-ystart)/ydel))+1

   # find the grid cells the integration points lie in
   # these points are where we will evaluate K
   mxi<-abs(floor((ep$X[,1]-xstart)/xdel))+1
   myj<-abs(floor((ep$X[,2]-ystart)/ydel))

   # so now we have our function K(x,y)
   K<-table(dxi,dyj)

   # table doesn't return an NxN table, so this hack
   # makes K (dres)x(dres)...
   x.names<-as.numeric(attr(K,"dimnames")$dxi)
   y.names<-as.numeric(attr(K,"dimnames")$dyj)
   Kt<-matrix(0,dres,dres)
   Kt[x.names,y.names]<-K
   K<-Kt/max(Kt)
   
   ### Evaluate K!
   # make sure that K>0 everywhere, so we don't kill any elements
   # this is fine since it should get absorbed into lambda
   dens.est<-K[mxi+dres*myj]

   # thesis diagam - density map
   if(dia.densmap){
      points(ep$X,cex=0.1,pch=19)

      # image plot of the density function
      denf<-K
      denf[-c(mxi+dres*myj)]<-NA
      #onoff<-inSide(bnd.mds,dgrid$X[,1],dgrid$X[,2])
      #denf[!onoff]<-NA
      image(z=denf,
            x=sort(unique(dgrid$X[,1])),
            y=sort(unique(dgrid$X[,2])),
            col=heat.colors(1000),asp=1,xlab="x*",ylab="y*")
      lines(bnd.mds,lwd=2)
      hist(denf)
      #cat("max=",max(K),"min=",min(K),"\n")
      dev.off()
      #X11()
   }

   #################################################
   # do the squashing
   sq<-sqrt((dens.est)^3)
   sq1<-sqrt((dens.est))
   #sq<-1
   Dx<-sq*Dx
   Dy<-sq*Dy
   Dxy<-sq1*Dxy

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
