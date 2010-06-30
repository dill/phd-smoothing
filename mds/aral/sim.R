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

   # test correlation
   #disp<-1.05
   #eta<-1.7
   #n<-length(aral.dat$chl[!is.na(aral.dat$chl)])
   #g    <- exp(eta)
   #cc<-c()
   #for(i in 1:100){
   #   y    <- rgamma( rep(1,n) , shape=1/disp,  scale=1/ (1/ (disp*g) ) )
   #   cc<-c(cc,cor(aral.dat$chl[!is.na(aral.dat$chl)]+y,aral.dat$chl[!is.na(aral.dat$chl)]))
   #}
   #cat("cor=",mean(cc),"\n")

   # add some noise
   #noise<- rgamma(n.samp,2,3.5)
   noise<-rgamma(rep(1,n.samp),shape=1/disp[i.n],scale=1/(1/(disp[i.n]*exp(eta[i.n]))))
   #noise<-rep(0,n.samp)

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

   # MDS (cr)
   mdscr.fit<-gam(chl~te(x,y,k=c(9,9),bs="cr"),data=samp.mds,family=Gamma(link="log"))

   # MDS (3D)
   samp.mds3<-insert.mds(samp,mds.grid,grid.mds3,bnd)
   samp.mds3<-list(x=samp.mds3[,1],y=samp.mds3[,2],z=samp.mds3[,3],chl=samp$chl)
   mds3.fit<-gam(chl~s(x,y,z,k=70),data=samp.mds3,family=Gamma(link="log"))

   # MDS (adj)
#   mdsadj.fit<-gam(chl~s(x,y,k=70,bs="mdstp",
#                   xt=list(bnd=bnd,op=mds.grid,mds.obj=grid.mds)),
#                   data=samp.mds,family=Gamma(link="log"))

   ### do some prediction
   tp.pred<-predict(tp.fit,newdata=pred.points,type="response")
   mds.pred<-predict(mds.fit,newdata=pred.mds,type="response")
   mdscr.pred<-predict(mdscr.fit,newdata=pred.mds,type="response")
   mds3.pred<-predict(mds3.fit,newdata=pred.mds3,type="response")
   mdsadj.pred<-predict(mdsadj.fit,newdata=pred.mds,type="response")
   soap.pred<-predict(soap.fit,newdata=pred.points,type="response")



   # calculate the MSE
   mses[i,]<-c(mean((tp.pred-new.truth)^2,na.rm=T),
               mean((mds.pred-new.truth)^2,na.rm=T),
               mean((mdscr.pred-new.truth)^2,na.rm=T),
               mean((mds3.pred-new.truth)^2,na.rm=T),
               mean((mdsadj.pred-new.truth)^2,na.rm=T),
               mean((soap.pred-new.truth)^2,na.rm=T))

   # calculate the EDFs
   edfs[i,]<-c(sum(tp.fit$edf),
               sum(mds.fit$edf),
               sum(mdscr.fit$edf),
               sum(mds3.fit$edf),
               sum(mdsadj.fit$edf),
               sum(soap.fit$edf))
}

mses<-as.data.frame(mses)
names(mses)<-modnames
edfs<-as.data.frame(edfs)
names(edfs)<-modnames

write.csv(mses,file=paste("aral/sim-mse-",n.samp,"-",snrs[i.n],".csv",sep=""))
write.csv(edfs,file=paste("aral/sim-edf-",n.samp,"-",snrs[i.n],".csv",sep=""))

