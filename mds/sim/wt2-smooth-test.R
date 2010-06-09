# function to run simulations on the wigglytop 2 domain
# Copyright David Lawrence Miller 2009.
source("mds.R")
 
wt2_smooth_test<-function(samp.size=250,noise.level=0.05,plot.it=FALSE,
                          gendata,bnd,grid.mds,my.grid,soap.knots){#,predd,bnd.mds){
 
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

   # prediction points insertion
   pred.mds<-insert.mds(gendata,my.grid,grid.mds,bnd,faster=1)

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
   b.mapped<-gam(z~s(x,y,k=140),data=samp.data)
   fv <- predict(b.mapped,newdata=pred.data)

   # tensor product of thin plate
#   b.mapped.mod<-gam(z~s(x,y,k=140,bs="mdstp",xt=list(bnd=bnd.mds,dens.points=pred.data)),data=samp.data)
#   fv.mod <- predict(b.mapped.mod,newdata=pred.data)
   
   ### normal tprs
   b.tprs<-gam(z~s(x,y,k=140),data=nsamp.data)
   fv.tprs <- predict(b.tprs,newdata=npred.data)

   ### soap
   b.soap<-gam(z~s(x,y,k=60,bs="so",xt=list(bnd=list(bnd))),knots=soap.knots,data=nsamp.data)
   fv.soap <- predict(b.soap,newdata=npred.data)
 
   ### calculate MSEs
   res<-list(mds=mean((fv-predd)^2,na.rm=T),
 #            mdsmod=mean((fv.mod-predd)^2,na.rm=T),
             tprs=mean((fv.tprs-predd)^2,na.rm=T),
             soap=mean((fv.soap-predd)^2,na.rm=T),
             mds.edf=sum(b.mapped$edf),
#             mdsmod.edf=sum(b.mapped.mod$edf),
             tprs.edf=sum(b.tprs$edf),
             soap.edf=sum(b.soap$edf))
 
   return(res)
}
