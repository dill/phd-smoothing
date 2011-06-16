smooth.construct.msg.smooth.spec<-function(object,data,knots){

   # this is the msg smooth.spec file
   # this does most of the work

   # for now TODO:
   #  just get spatial smoothing working
   #     do the clever stuff with previous objects as in gam.mds.R
   # THEN:
   #  if no bnd supplied do the general thing
   #  also do the s(.) thing
   


   # extract the boundary
   bnd<-object$xt$bnd

   if(!is.null(old.obj)){
      # object to store all the results for later
      new.obj<-old.obj

      # pull out the grid D matrix
      D.grid<-old.obj$D
      my.grid<-old.obj$grid

      # also the pred and sample D matrices if
      # they are there
      if(!is.null(old.obj$D.samp)){
         D.samp<-old.obj$D.samp
      }else{
         D.samp<-NULL
      }
      if(!is.null(old.obj$D.pred)){
         D.pred<-old.obj$D.pred
      }else{
         D.pred<-NULL
      }

      if(!is.null(old.obj$m)){
         m<-old.obj$m
      }
      if(!is.null(old.obj$bs)){
         bs<-old.obj$bs
      }
      if(!is.null(old.obj$k)){
         k<-old.obj$k
      }

      if(!is.null(old.obj$mds.dim)){
         mds.dim<-old.obj$mds.dim
      }

   }else{
      # object to store all the results for later
      new.obj<-list()

      # create the grid
      #grid.obj<-calc.grid(bnd,grid.res)
      grid.obj<-create_refgrid(bnd,grid.res)
      D.grid<-create_distance_matrix(grid.obj$x,grid.obj$y,bnd)
      grid.obj<-list(D=D.grid,grid=list(x=grid.obj$x,y=grid.obj$y))

      D.grid<-grid.obj$D
      my.grid<-grid.obj$grid
      # store!
      new.obj$D<-D.grid
      new.obj$grid<-my.grid

      D.samp<-NULL
      D.pred<-NULL
   }


























   if(length(names(data))!=2){
      cat("msg can only be used with 2D smooths!\n")
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

   int.bnd<-bnd


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
      m<-30;n<-30 # need to set these somewhere
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
      points(tlg,pch=".",col="red")
      points(trg,pch=".",col="red")
      points(brg,pch=".",col="red")
      points(blg,pch=".",col="red")
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
      plot(mtlg,pch=".",asp=1,las=1,xlab="x*",ylab="y*",
           xlim=xlims,ylim=ylims,col="red")
      lines(bnd.mds,lwd=2)
      points(mtrg,pch=".",col="red")
      points(mbrg,pch=".",col="red")
      points(mblg,pch=".",col="red")
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

   if(dia.densmap){
      # check that the interpolation worked...
      #X11()
      plot(dpoints,pch=".",col="green",xlab="x*",ylab="y*",
            asp=1,xlim=xlims,ylim=ylims)
      lines(bnd.mds,lwd=2)
      #X11()
   }
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
   myj<-abs(floor((ep$X[,2]-ystart)/ydel))+1

   # so now we have our function K(x,y)
   K<-table(dxi,dyj)

   # table doesn't return an NxN table, so this hack
   # makes K (dres)x(dres)...
   x.names<-as.numeric(attr(K,"dimnames")$dxi)
   y.names<-as.numeric(attr(K,"dimnames")$dyj)
   Kt<-matrix(0,dres,dres)
   Kt[x.names,y.names]<-K
   K<-Kt
   
   ### Evaluate K!
   dens.est<-1/(1+K[cbind(mxi,myj)])

   # thesis diagam - density map
   if(dia.densmap){
      points(ep$X,pch=".")

      # image plot of the density function
      denf<-K
      denf[!inSide(bnd.mds,dgrid$X[,1],dgrid$X[,2])]<-NA
      image(z=denf,
            x=sort(unique(dgrid$X[,1])),
            y=sort(unique(dgrid$X[,2])),
            col=heat.colors(1000),asp=1,xlab="x*",ylab="y*")
      lines(bnd.mds,lwd=1)
      #points(ep$X,pch=".")
      #hist(denf[cbind(mxi,myj)])
      #cat("max=",max(K),"min=",min(K),"\n")
      dev.off()
      #X11()
   }

   #################################################
   # do the squashing
   sq<-sqrt((dens.est)^(3/2))
#   sq<-dens.est^(3/2)
   Dx<-sq*Dx
   Dy<-sq*Dy
   Dxy<-sq*Dxy
   #Dxy<-sqrt(dens.est)*Dxy

   # actually do the integration
   S<-t(Dx)%*%Dx + 2*t(Dxy)%*%Dxy + t(Dy)%*%Dy

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
