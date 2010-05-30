# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
source("mds.R")
 
wt2_smooth_test<-function(samp.size=250,noise.level=0.05,plot.it=FALSE,
                          gendata,bnd,grid.mds,grid.mds3,my.grid,soap.knots,predd){
 
   # create the sample index
   samp.ind<-sample(1:length(gendata$x),samp.size)

   ## create the sample
   gendata.samp<- list(x=gendata$x[samp.ind],
                       y=gendata$y[samp.ind],
                       z=gendata$z[samp.ind])

   gendata<-list(x=gendata$x[-samp.ind],
                  y=gendata$y[-samp.ind],
                  z=gendata$z[-samp.ind])

   # sample points insertion
   samp.mds<-insert.mds(gendata.samp,my.grid,grid.mds,bnd,faster=1)
   # 3d case
   samp.mds3<-insert.mds(gendata.samp,my.grid,grid.mds3,bnd,faster=1)

   # prediction points insertion
   pred.mds<-insert.mds(gendata,my.grid,grid.mds,bnd,faster=1)
   # 3d case
   pred.mds3<-insert.mds(gendata,my.grid,grid.mds3,bnd,faster=1)

   # add noise
   noise<-noise.level*rnorm(length(samp.ind))
 
   #######################################
   # sample data
   #######################################
   # mapped sample data
   samp.data<-list(x=c(),y=c(),z=c())
   samp.data$x<-samp.mds[,1]
   samp.data$y<-samp.mds[,2]
   samp.data$z<-gendata.samp$z+noise
   # in 3D
   samp.data.3d<-list(w=c(),x=c(),y=c(),z=c())
   samp.data.3d$w<-samp.mds3[,1]
   samp.data.3d$x<-samp.mds3[,2]
   samp.data.3d$y<-samp.mds3[,3]
   samp.data.3d$z<-gendata.samp$z+noise
 
   # non-mapped sample data
   nsamp.data<-list(x=c(),y=c(),z=c())
   nsamp.data$x<-gendata.samp$x
   nsamp.data$y<-gendata.samp$y
   nsamp.data$z<-gendata.samp$z+noise
 
   #######################################
   # prediction data
   #######################################
   # mapped pred data 
   pred.data<-list(x=rep(0,length(gendata$x)+length(samp.data$x)),
                   y=rep(0,length(gendata$x)+length(samp.data$x)))
   pred.data$x[-samp.ind]<-pred.mds[,1]
   pred.data$y[-samp.ind]<-pred.mds[,2]
   pred.data$x[samp.ind]<-samp.mds[,1]
   pred.data$y[samp.ind]<-samp.mds[,2]
   # 3d case
   pred.data.3d<-list(w=rep(0,length(gendata$x)+length(samp.data$x)),
                   x=rep(0,length(gendata$x)+length(samp.data$x)),
                   y=rep(0,length(gendata$x)+length(samp.data$x)))
   pred.data.3d$w[-samp.ind]<-pred.mds3[,1]
   pred.data.3d$x[-samp.ind]<-pred.mds3[,2]
   pred.data.3d$y[-samp.ind]<-pred.mds3[,3]
   pred.data.3d$w[samp.ind]<-samp.mds3[,1]
   pred.data.3d$x[samp.ind]<-samp.mds3[,2]
   pred.data.3d$y[samp.ind]<-samp.mds3[,3]

   # non-mapped prediction data
   npred.data<-list(x=rep(0,length(gendata$x)+length(samp.data$x)),
                   y=rep(0,length(gendata$x)+length(samp.data$y)))
   npred.data$x[-samp.ind]<-gendata$x
   npred.data$y[-samp.ind]<-gendata$y
   npred.data$x[samp.ind]<-nsamp.data$x
   npred.data$y[samp.ind]<-nsamp.data$y
 
   #######################################
   ### Now do some fitting and prediction
   #######################################

   #  tprs
   b.tp<-gam(z~s(x,y,k=140),data=nsamp.data)
   fv.tp<-predict(b.tp,newdata=npred.data)

   # mds+tp
   b.mdstp<-gam(z~s(x,y,k=140),data=samp.data)
   fv.mdstp<-predict(b.mdstp,newdata=pred.data)

   # mds+cr
   b.mdscr<-gam(z~te(x,y,k=c(15,15),bs="cr"),data=samp.data)
   fv.mdscr<-predict(b.mdscr,newdata=pred.data)

   # mds+tp (3D)
   b.mds.3d<-gam(z~s(w,x,y,k=140),data=samp.data.3d)
   fv.mds.3d<-predict(b.mds.3d,newdata=pred.data.3d)

   # mds+tp+adj
   b.mds.adj<-gam(z~s(x,y,k=140,bs="mdstp",xt=list(bnd=bnd,op=my.grid,mds.obj=grid.mds)),data=samp.data)
   fv.mds.adj<-predict(b.mds.adj,newdata=pred.data)
   

   # soap
   b.soap<-gam(z~s(x,y,k=60,bs="so",xt=list(bnd=list(bnd))),knots=soap.knots,data=nsamp.data)
   fv.soap <- predict(b.soap,newdata=npred.data)
 
   ### calculate MSEs

   mses<-c(mean((fv.tp-predd)^2,na.rm=T),
           mean((fv.mdstp-predd)^2,na.rm=T),
           mean((fv.mdscr-predd)^2,na.rm=T),
           mean((fv.mds.3d-predd)^2,na.rm=T),
           mean((fv.mds.adj-predd)^2,na.rm=T),
           mean((fv.soap-predd)^2,na.rm=T))

   edfs<-c(sum(b.tp$edf),
           sum(b.mdstp$edf),
           sum(b.mdscr$edf),
           sum(b.mds.3d$edf),
           sum(b.mds.adj$edf),
           sum(b.soap$edf))

   res<-list(mse=mses,edf=edfs)

   return(res)
}
