# Aral Sea data set simulation
# IDEA: fit soap to the Aral sea data set then predict over the
# whole area. Add noise to that prediction over a simulation.

# run from phd-smoothing/mds

# storage for the MSEs
mses<-matrix(0,sim.size,length(modnames)*2)


n.samp<-samp.size[i.s]

for(i in 1:sim.size){

   ## take the sample
   samp.ind<-sample(1:length(new.truth),n.samp)

   # add some noise
   noise<-rgamma(rep(1,n.samp),shape=1/disp[i.n],scale=1/(1/(disp[i.n]*exp(eta[i.n]))))

   samp<-data.frame(x=pred.points$x[samp.ind],
                    y=pred.points$y[samp.ind],
                    chl=as.numeric(new.truth[samp.ind]+noise))
  
   ### fit some models
   # tprs
   tp.fit<-gam(chl~s(x,y,k=70),data=samp,family=Gamma(link="log"))

   # soap
   soap.fit<-gam(chl~s(x,y,k=25,bs="so",xt=list(bnd=list(bnd))),knots=s.grid,
            family=Gamma(link="log"),data=samp)

   # MDS (tp)
   samp.mds<-insert.mds(samp,mds.grid,grid.mds,bnd)
   samp.mds<-list(x=samp.mds[,1],y=samp.mds[,2],chl=samp$chl)
   mds.fit<-gam(chl~s(x,y,k=70),data=samp.mds,family=Gamma(link="log"))

   # MDS (3D)
   #samp.mds3<-insert.mds(samp,mds.grid,grid.mds3,bnd)
   #samp.mds3<-list(x=samp.mds3[,1],y=samp.mds3[,2],z=samp.mds3[,3],chl=samp$chl)
   #mds3.fit<-gam(chl~s(x,y,z,k=70),data=samp.mds3,family=Gamma(link="log"))

   ### do some prediction
   tp.pred<-predict(tp.fit,newdata=pred.points,type="response")
   mds.pred<-predict(mds.fit,newdata=pred.mds,type="response")
   #mds3.pred<-predict(mds3.fit,newdata=pred.mds3,type="response")
   soap.pred<-predict(soap.fit,newdata=pred.points,type="response")

   # calculate the MSE
   mses[i,]<-c(mean((  tp.pred[!ind2]  -new.truth[!ind2])^2,na.rm=T),
               mean((  tp.pred[ind2]  -new.truth[ind2])^2,na.rm=T),
               mean(( mds.pred[!ind2] -new.truth[!ind2])^2,na.rm=T),
               mean(( mds.pred[ind2] -new.truth[ind2])^2,na.rm=T),
    #           mean((mds3.pred[!ind2]-new.truth[!ind2])^2,na.rm=T),
    #           mean((mds3.pred[ind2]-new.truth[ind2])^2,na.rm=T),
               mean((soap.pred[!ind2]-new.truth[!ind2])^2,na.rm=T),
               mean((soap.pred[ind2]-new.truth[ind2])^2,na.rm=T))

}

mses<-as.data.frame(mses)
names(mses)<-modnames

write.csv(mses,file=paste("aral/split/sim-mse-",n.samp,"-",snrs[i.n],".csv",sep=""))

