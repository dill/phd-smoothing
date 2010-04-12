# function to do the smoothing on the spiral
# Copyright David Lawrence Miller 2010
source("mds.R")
source("spiral/make_spiral.R")
source("spiral/sp_test.R")

spiral_test<-function(samp.size=250,noise.level=0.05,plot.it=FALSE,faster=0){

   grid.m<-50

   # create boundary
   spir.dat<-make_spiral(25,grid.m)
   bnd<-spir.dat$bnd
   xx<-spir.dat$dat$x
   yy<-spir.dat$dat$y


   # map the grid
   spir.grid<-make_spiral(25,n.grid=15)
   my.grid<-list(x=spir.grid$dat$x,y=spir.grid$dat$y)
   D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=1)
   grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)

   # make the sample
   samp.ind<-sample(1:length(xx),samp.size)

   # add noise
   noise<-rnorm(samp.size)*noise.level

   samp.data<-data.frame(x=xx[samp.ind],y=yy[samp.ind],
                         z=sp_test(xx[samp.ind],yy[samp.ind])+noise)

   # insert the sample
   samp.mds<-insert.mds(samp.data,my.grid,grid.mds,bnd,faster=faster)
   samp.data.mds<-data.frame(x=samp.mds[,1],y=samp.mds[,2],z=samp.data$z)

   ### create prediction data
   # non-mapped prediction data
   pred.data<-data.frame(x=xx[-samp.ind],y=yy[-samp.ind],
                         z=sp_test(xx[-samp.ind],yy[-samp.ind]))

   # new MDS coords for the prediction points
   pred.mds<-insert.mds(pred.data,my.grid,grid.mds,bnd,faster=faster)

   # put this in the correct format 
   pred.size<-dim(pred.data)[1]+dim(samp.data)[1]
   pred.data.mds<-list(x=rep(0,pred.size),y=rep(0,pred.size))
   pred.data.mds$x[samp.ind]<-samp.data.mds$x  # need to add in the sample points too
   pred.data.mds$x[-samp.ind]<-pred.mds[,1]
   pred.data.mds$y[samp.ind]<-samp.data.mds$y  # need to add in the sample points too
   pred.data.mds$y[-samp.ind]<-pred.mds[,2]

   # prediction data for non mds'd
   pred.data<-list(x=xx,y=yy,z=sp_test(xx,yy))

   # boundary, only for drawing the line around the outside
   bnd<-spir.dat$bnd
   
   # truth
   z.truth<-spir.dat$mat
   
   ### mapping
   b.mapped<-gam(z~s(x,y,k=100),data=samp.data.mds)
   fv.mapped <- predict(b.mapped,newdata=pred.data.mds)
   
   ### normal tprs
   b.tprs<-gam(z~s(x,y,k=100),data=samp.data)
   fv.tprs <- predict(b.tprs,newdata=pred.data)
   
   ### soap
   # create some internal knots...
   knots<-make_soap_grid(bnd,20)
   knots.out<-c(15, 30, 31, 33, 35, 38, 41, 61, 62, 67,109,117,121,122)
   knots<-pe(knots,-knots.out)



   b.soap<-gam(z~s(x,y,k=39,bs="so",xt=list(bnd=list(bnd))),knots=knots,data=samp.data)
   fv.soap<-predict(b.soap,newdata=pred.data,block.size=-1)
   

   # plot
   if(plot.it){
      par(mfrow=c(2,2))
      par(mar=c(3,3,3,3))

      # truth
      image(z.truth,col=heat.colors(100),xlab="x",ylab="y",main="truth",las=1,asp=1)
      contour(z.truth,levels=seq(-5,5,by=.25),add=TRUE)
      lines(bnd,lwd=2)

      # mapped
      pred.mat<-matrix(NA,grid.m,grid.m)
      pred.mat[onoff]<-fv.mapped
      image(pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="MDS",las=1,asp=1)
      contour(pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
      lines(bnd,lwd=2)

      # tprs
      pred.mat<-matrix(NA,grid.m,grid.m)
      pred.mat[onoff]<-fv.tprs
      image(pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tprs",las=1,asp=1)
      contour(pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
      lines(bnd,lwd=2)

      # soap
      pred.mat<-matrix(NA,grid.m,grid.m)
      pred.mat[onoff]<-fv.soap
      image(pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="soap",las=1,asp=1)
      contour(pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
      lines(bnd,lwd=2)

   }

   ### calculate MSEs
   mses<-list(mds=mean((fv.mapped-pred.data$z)^2,na.rm=T),
              tprs=mean((fv.tprs-pred.data$z)^2,na.rm=T),
              soap=mean((fv.soap-pred.data$z)^2,na.rm=T))

   # print them
   #cat("mds MSE=" ,mses$mds,"\n")
   #cat("tprs MSE=",mses$tprs,"\n")
   #cat("soap MSE=",mses$soap,"\n")

   ## pred object
   #preds<-list(tprs=fv.tprs,soap=fv.soap,mds=fv.mapped,truth=z.truth)

   ## lets return an object...
   #ret<-list(samp.mds=samp.data,pred.mds=pred.data,samp=samp.data,
   #          pred=npred.data,mses=mses,pred=preds)

   return(mses)
}
