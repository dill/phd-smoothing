# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
source("mds.R")
 
wt2_smooth_test<-function(samp.size=250,noise.level=0.05,plot.it=FALSE,
                          gendata,bnd,grid.mds,my.grid,soap.knots,predd,faster){
 
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
   ins.time.samp<-system.time(samp.mds<-insert.mds(gendata.samp,my.grid,grid.mds,bnd,faster=faster))[3]

   # prediction points insertion
   ins.time.pred<-system.time(pred.mds<-insert.mds(gendata,my.grid,grid.mds,bnd,faster=faster))[3]

   # add noise
   noise<-noise.level*rnorm(length(samp.ind))
   #> summary(gendata$z)
   # Min. 1st Qu. Median Mean 3rd Qu. Max.
   #0.000000 0.000236 0.269300 0.276300 0.479600 0.850000
 
   # mapped sample data
   samp.data<-list(x=c(),y=c(),z=c())
   samp.data$x<-samp.mds[,1]
   samp.data$y<-samp.mds[,2]
   samp.data$z<-gendata.samp$z+noise
 
   # non-mapped sample data
   nsamp.data<-list(x=c(),y=c(),z=c())
   nsamp.data$x<-gendata.samp$x
   nsamp.data$y<-gendata.samp$y
   nsamp.data$z<-gendata.samp$z+noise
 
   ### create prediction data
   # non-mapped prediction data
   npred.data<-list(x=rep(0,length(gendata$x)+length(samp.data$x)),
                   y=rep(0,length(gendata$x)+length(samp.data$y)))
   npred.data$x[-samp.ind]<-gendata$x
   npred.data$y[-samp.ind]<-gendata$y
   npred.data$x[samp.ind]<-nsamp.data$x
   npred.data$y[samp.ind]<-nsamp.data$y
 
   # put this in the correct format
   pred.data<-list(x=rep(0,length(gendata$x)+length(samp.data$x)),
                   y=rep(0,length(gendata$x)+length(samp.data$y)))
   pred.data$x[-samp.ind]<-pred.mds[,1]
   pred.data$y[-samp.ind]<-pred.mds[,2]
   pred.data$x[samp.ind]<-samp.mds[,1]
   pred.data$y[samp.ind]<-samp.mds[,2]
 
   ### Now do some fitting and prediction
   ### mapping
   # thin plate
   mds.gam<-system.time(b.mapped<-gam(z~s(x,y,k=100),data=samp.data))[3]
   mds.pred<-system.time(fv <- predict(b.mapped,newdata=pred.data))[3]

   # tensor product of thin plate
   mdstp.gam<-system.time(b.mapped.tp<-gam(z~te(x,y,k=12),data=samp.data))[3]
   mdstp.pred<-system.time(fv.tp <- predict(b.mapped.tp,newdata=pred.data))[3]
   
   ### normal tprs
   tprs.gam<-system.time(b.tprs<-gam(z~s(x,y,k=100),data=nsamp.data))[3]
   tprs.pred<-system.time(fv.tprs <- predict(b.tprs,newdata=npred.data))[3]

   ### soap
   soap.gam<-system.time(b.soap<-gam(z~s(x,y,k=60,bs="so",xt=list(bnd=list(bnd))),
                                          knots=soap.knots,data=nsamp.data))[3]
   soap.pred<-system.time(fv.soap <- predict(b.soap,newdata=npred.data))[3]
 
#soap.gam<-0
#soap.pred<-0
#tprs.gam<-0
#tprs.pred<-0

   ### calculate MSEs
   times<-list(mds=c(mds.gam+ins.time.samp,mds.pred+ins.time.pred),
               mdstp=c(mdstp.gam+ins.time.samp,mdstp.pred+ins.time.pred),
               tprs=c(tprs.gam,tprs.pred) ,
               soap=c(soap.gam,soap.pred) )
 
   return(times)
}
