# function to do the smoothing on the Ramsay horseshoe 
# Copyright David Lawrence Miller 2009
source("mds.R")

ramsay_smooth_test<-function(samp.size=250,noise.level=0.05,plot.it=FALSE,
                             bnd=bnd,my.grid=my.grid,grid.mds=grid.mds,
                             xx,yy,onoff){

   # make the sample
   samp.ind<-sample(1:length(xx),samp.size)

   # add noise
   noise<-rnorm(samp.size)*noise.level

   samp.data<-data.frame(x=xx[samp.ind],y=yy[samp.ind],
                         z=fs.test(xx[samp.ind],yy[samp.ind])+noise)

   # insert the sample
   samp.mds<-insert.mds(samp.data,my.grid,grid.mds,bnd)
   samp.data.mds<-data.frame(x=samp.mds[,1],y=samp.mds[,2],z=samp.data$z)

   ### create prediction data
   # non-mapped prediction data
   pred.data<-data.frame(x=xx[-samp.ind],y=yy[-samp.ind],
                         z=fs.test(xx[-samp.ind],yy[-samp.ind]))

   # new MDS coords for the prediction points
   pred.mds<-insert.mds(pred.data,my.grid,grid.mds,bnd)

   # put this in the correct format 
   pred.size<-dim(pred.data)[1]+dim(samp.data)[1]
   pred.data.mds<-list(x=rep(0,pred.size),y=rep(0,pred.size))
   pred.data.mds$x[samp.ind]<-samp.data.mds$x  # need to add in the sample points too
   pred.data.mds$x[-samp.ind]<-pred.mds[,1]
   pred.data.mds$y[samp.ind]<-samp.data.mds$y  # need to add in the sample points too
   pred.data.mds$y[-samp.ind]<-pred.mds[,2]

   # prediction data for non mds'd
   pred.data<-list(x=xx,y=yy,z=fs.test(xx,yy))

   # truth
   z.truth<-matrix(NA,m,n)
   z.truth[onoff]<-fs.test(xx,yy)
   
   ### mapping
   b.mapped<-gam(z~s(x,y,k=100),data=samp.data.mds)
   fv.mapped <- predict(b.mapped,newdata=pred.data.mds)

   ### normal tprs
   b.tprs<-gam(z~s(x,y,k=100),data=samp.data)
   fv.tprs <- predict(b.tprs,newdata=pred.data)
   
   ### soap
   # create some internal knots...
   knots <- data.frame(x=rep(seq(-.5,3,by=.5),4),
                       y=rep(c(-.6,-.3,.3,.6),rep(8,4)))
   knots.ind<-inSide(bnd,x=knots$x,y=knots$y)
   knots<-list(x=knots$x[knots.ind],y=knots$y[knots.ind])
   b.soap<-gam(z~s(x,y,k=39,bs="so",xt=list(bnd=list(bnd))),knots=knots,data=samp.data)
   fv.soap<-predict(b.soap,newdata=pred.data,block.size=-1)
   
   ### calculate MSEs and EDFs
   res<-list(mds=mean((fv.mapped-pred.data$z)^2,na.rm=T),
              tprs=mean((fv.tprs-pred.data$z)^2,na.rm=T),
              soap=mean((fv.soap-pred.data$z)^2,na.rm=T),
              mds.edf=sum(b.mapped$edf),
              tprs.edf=sum(b.tprs$edf),
              soap.edf=sum(b.soap$edf))

   return(res)
}
