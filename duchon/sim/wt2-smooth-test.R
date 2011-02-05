# function to run simulations on the wigglytop 2 domain
# David Lawrence Miller 2009-2011
 
wt2_smooth_test<-function(samp.size=250,noise.level=0.05,plot.it=FALSE,
                          gendata,bnd,base.mds,soap.knots){
 
   # create the sample index
   samp.ind<-sample(1:length(gendata$x),samp.size)

   ## create the sample
   gendata.samp<- list(x=gendata$x[samp.ind],
                       y=gendata$y[samp.ind],
                       z=gendata$z[samp.ind]+noise.level*rnorm(length(samp.ind)))

   gendata<-list(x=gendata$x[-samp.ind],
                  y=gendata$y[-samp.ind],
                  z=gendata$z[-samp.ind])


   #  tprs
   b.tp<-gam(z~s(x,y,k=140),data=gendata.samp)
   fv.tp<-predict(b.tp,newdata=gendata)

   # fit MDS 2D
   mds.2d<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=2,old.obj=base.mds)
   fv.mds.2d<-mds.2d$pred
   
   # fit MDS/Duchon 3D
   mds.3d.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=3,m=c(2,0.5),
                      bs="ds",old.obj=base.mds)
   fv.mds.3d.ds<-mds.3d.ds$pred

   # fit MDS/Duchon 85%
   mds.85.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=0.85,
                      bs="ds",old.obj=base.mds)
   fv.mds.85.ds<-mds.85.ds$pred

   # fit MDS/Duchon 90%
   mds.90.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=0.9,
                      bs="ds",old.obj=base.mds)
   fv.mds.90.ds<-mds.90.ds$pred

   # fit MDS/Duchon 95%
   mds.95.ds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,mds.dim=0.95,
                      bs="ds",old.obj=base.mds)
   fv.mds.95.ds<-mds.95.ds$pred

   # soap
   b.soap<-gam(z~s(x,y,k=60,bs="so",xt=list(bnd=list(bnd))),knots=soap.knots,data=gendata.samp)
   fv.soap <- predict(b.soap,newdata=gendata)

   ### calculate MSEs
   mses<-c(mean((fv.tp-gendata$z)^2,na.rm=T), 
           mean((fv.mds.2d-gendata$z)^2,na.rm=T), 
           mean((fv.mds.3d.ds-gendata$z)^2,na.rm=T), 
           mean((fv.mds.85.ds-gendata$z)^2,na.rm=T), 
           mean((fv.mds.90.ds-gendata$z)^2,na.rm=T), 
           mean((fv.mds.95.ds-gendata$z)^2,na.rm=T), 
           mean((fv.soap-gendata$z)^2,na.rm=T)) 

   res<-list(mse=mses)#,edf=edfs)

   return(res)
}
