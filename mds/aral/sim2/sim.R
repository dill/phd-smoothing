# Aral Sea data set simulation
# IDEA: fit soap to the Aral sea data set then predict over the
# whole area. Add noise to that prediction over a simulation.

# run from phd-smoothing/mds

# storage for the MSEs and EDFs
mses<-matrix(0,sim.size,length(modnames))
edfs<-matrix(0,sim.size,length(modnames))


n.samp<-samp.size[i.s]

for(i in 1:sim.size){

   ## take the sample
   samp.ind<-sample(1:length(new.truth),n.samp)

   # add some noise
   noise<-rgamma(rep(1,n.samp),shape=1/disp[i.n],scale=1/(1/(disp[i.n]*exp(eta[i.n]))))

   samp<-data.frame(x=pred.points$x[samp.ind],
                    y=pred.points$y[samp.ind],
                    chl=as.numeric(new.truth[samp.ind]+noise))

   samp.mds<-insert.mds(samp,mds.grid,grid.mds,bnd)
   samp.mds<-list(x=samp.mds[,1],y=samp.mds[,2],chl=samp$chl)

  
   # MDS (adj)
   mdsadj.fit<-gam(chl~s(x,y,k=70,bs="mdstp",
                   xt=list(bnd=bnd,op=mds.grid,mds.obj=grid.mds)),
                   data=samp.mds,family=Gamma(link="log"))

   ### do some prediction
   mdsadj.pred<-predict(mdsadj.fit,newdata=pred.mds,type="response")

   # calculate the MSE
   mses[i,]<-c(mean((mdsadj.pred-new.truth)^2,na.rm=T))

   # calculate the EDFs
   edfs[i,]<-c(sum(mdsadj.fit$edf))

}

mses<-as.data.frame(mses)
names(mses)<-modnames
edfs<-as.data.frame(edfs)
names(edfs)<-modnames

write.csv(mses,file=paste("aral/sim2/sim-mse-",n.samp,"-",snrs[i.n],".csv",sep=""))
write.csv(edfs,file=paste("aral/sim2/sim-edf-",n.samp,"-",snrs[i.n],".csv",sep=""))

