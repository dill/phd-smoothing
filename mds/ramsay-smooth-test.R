# function to do the smoothing on the Ramsay horseshoe 
# Copyright David Lawrence Miller 2009
source("mds.R")

ramsay_smooth_test<-function(samp.size=250,noise.level=0.05,logfilename=NA, plot.it=FALSE){
   ## create a boundary...
   bnd <- fs.boundary()
   bnd<-pe(bnd,seq(1,length(bnd$x),8))
   bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))
   # create points within the boundary 
   m<-45;n<-25
   xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
   xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
   onoff<-inSide(bnd,xx,yy)
   xx<-xx[onoff];yy<-yy[onoff]
   onoff<-inSide(list(x=-bnd$x,y=-bnd$y),-xx,-yy)
   xx<-xx[onoff];yy<-yy[onoff]
   
   # make the sample
   samp.ind<-sample(1:length(xx),samp.size)

   # add noise
   noise<-rnorm(samp.size)*noise.level

   samp.data<-data.frame(x=xx[samp.ind],y=yy[samp.ind],
                         z=fs.test(xx[samp.ind],yy[samp.ind])+noise)
   
   # map the prediction grid
   D.samp<-create_distance_matrix(xx,yy,bnd)

   samp.mds<-cmdscale(D.samp,eig=TRUE,x.ret=TRUE)

   samp.data.mds<-data.frame(x=samp.mds$points[,1],y=samp.mds$points[,2],
                             z=samp.data$z)

   ### create prediction data
   # non-mapped prediction data
   pred.data<-data.frame(x=xx[-samp.ind],y=yy[-samp.ind],
                         z=fs.test(xx[-samp.ind],yy[-samp.ind]))

   # new MDS coords for the prediction points
cat("before D.pred\n")
   pred.mds<-insert.mds(pred.data,samp.data,samp.mds)
cat("created D.pred\n")

   # put this in the correct format 
   pred.data<-list(x=rep(0,dim(D)[2]),y=rep(0,dim(D)[2]))
   pred.data$x[samp.ind]<-samp.data$x  # need to add in the sample points too
   pred.data$x[-samp.ind]<-pred.mds[,1]
   pred.data$y[samp.ind]<-samp.data$y  # need to add in the sample points too
   pred.data$y[-samp.ind]<-pred.mds[,2]

   # boundary, only for drawing the line around the outside
   fsb <- fs.boundary()
   
   # truth
   z.truth<-matrix(NA,m,n)
   z.truth[onoff]<-new.data$z
   
   ### mapping
   b.mapped<-gam(z~s(x,y,k=49),data=samp.data.t)
   fv.mapped <- predict(b.mapped,newdata=new.data.mapped)
   
   ### normal tprs
   b.tprs<-gam(z~s(x,y,k=49),data=samp.data.n)
   fv.tprs <- predict(b.tprs,newdata=new.data)
   
   ### soap
   # create some internal knots...
   knots <- data.frame(x=rep(seq(-.5,3,by=.5),4),
                       y=rep(c(-.6,-.3,.3,.6),rep(8,4)))
   knots.ind<-inSide(bnd,x=knots$x,y=knots$y)
   knots<-list(x=knots$x[knots.ind],y=knots$y[knots.ind])
   b.soap<-gam(z~s(x,y,k=20,bs="so",xt=list(bnd=list(bnd))),knots=knots,data=samp.data.n)
   fv.soap<-predict(b.soap,newdata=new.data,block.size=-1)
   
   ### calculate MSEs
   cat("tprs MSE=",mean((fv.tprs-new.data$z)^2,na.rm=TRUE),"\n")
   cat("soap MSE=",mean((fv.soap-new.data$z)^2,na.rm=TRUE),"\n")
   cat("mapped MSE=",mean((fv.mapped-new.data$z)^2,na.rm=TRUE),"\n")

   # plot
   if(plot.it){
      par(mfrow=c(2,2))

      # truth
      image(xm,yn,z.truth,col=heat.colors(100),xlab="x",ylab="y",main="truth",las=1,asp=1)
      contour(xm,yn,z.truth,levels=seq(-5,5,by=.25),add=TRUE)
      lines(fsb,lwd=2)

      # mapped
      pred.mat<-matrix(NA,m,n)
      pred.mat[onoff]<-fv.mapped
      image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="MDS",las=1,asp=1)
      contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
      lines(fsb,lwd=2)

      # tprs
      pred.mat<-matrix(NA,m,n)
      pred.mat[onoff]<-fv.tprs
      image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="tprs",las=1,asp=1)
      contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
      lines(fsb,lwd=2)

      # soap
      pred.mat<-matrix(NA,m,n)
      pred.mat[onoff]<-fv.soap
      image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="soap",las=1,asp=1)
      contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE)
      lines(fsb,lwd=2)

   }

}



