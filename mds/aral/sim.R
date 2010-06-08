# Aral Sea data set simulation
# IDEA: fit soap to the Aral sea data set then predict over the
# whole area. Add noise to that prediction over a simulation.

# run from phd-smoothing/mds

# storage for the MSEs and EDFs
mses<-matrix(0,n.sim,3)
edfs<-matrix(0,n.sim,3)

for(i in 1:n.sim){

   ## take the sample
   samp.ind<-sample(1:length(new.truth),n.samp)

   # add some noise
   # SNR = 
   disp<-c(0.2,0.6,)
   eta<-c(1,1.25,)

   disp<-0.8
   eta<-1.1
   n<-length(aral.dat$chl[!is.na(aral.dat$chl)])
   g    <- exp(eta)
   cc<-c()
   for(i in 1:100){
      y    <- rgamma( rep(1,n) , shape=1/disp,  scale=1/ (1/ (disp*g) ) )
      cc<-c(cc,cor(aral.dat$chl[!is.na(aral.dat$chl)]+y,aral.dat$chl[!is.na(aral.dat$chl)]))
   }
   cat("cor=",mean(cc),"\n")

   #noise<- rgamma(n.samp,2,3.5)
   noise<-rep(0,n.samp)

   samp<-data.frame(x=pred.points$x[samp.ind],
                    y=pred.points$y[samp.ind],
                    chl=as.numeric(new.truth[samp.ind]+noise))
  
   ### fit some models
   # tprs
   tp.fit<-gam(chl~s(x,y,k=80),data=samp,family=Gamma(link="log"))

   # soap
   soap.fit<-gam(chl~s(x,y,k=40,bs="so",xt=list(bnd=list(bnd))),knots=s.grid,
            family=Gamma(link="log"),data=samp)

   # MDS
   samp.mds<-insert.mds(samp,mds.grid,grid.mds,bnd)
   samp.mds<-list(x=samp.mds[,1],y=samp.mds[,2],chl=samp$chl)
   mds.fit<-gam(chl~s(x,y,k=80),data=samp.mds,family=Gamma(link="log"))


   ### do some prediction
   tp.pred<-predict(tp.fit,newdata=pred.points,type="response")
   soap.pred<-predict(soap.fit,newdata=pred.points,type="response")
   mds.pred<-predict(mds.fit,newdata=pred.mds,type="response")

   if(plot.it==TRUE){

      pred.mat[!is.na(pred.mat)]<-tp.pred
      image(pred.mat,main="tprs",zlim=c(0,20))
      contour(pred.mat,add=TRUE,zlim=c(0,20))

      pred.mat[!is.na(pred.mat)]<-soap.pred
      image(pred.mat,main="soap",zlim=c(0,20))
      contour(pred.mat,add=TRUE,zlim=c(0,20))

      pred.mat[!is.na(pred.mat)]<-mds.pred
      image(pred.mat,main="mds",zlim=c(0,20))
      contour(pred.mat,add=TRUE,zlim=c(0,20))

   }


   # calculate the MSE
   mses[i,1]<-mean((tp.pred-new.truth)^2,na.rm=T)
   mses[i,2]<-mean((soap.pred-new.truth)^2,na.rm=T)
   mses[i,3]<-mean((mds.pred-new.truth)^2,na.rm=T)

   # calculate the EDFs
   edfs[i,1]<-sum(tp.fit$edf)
   edfs[i,2]<-sum(soap.fit$edf)
   edfs[i,3]<-sum(mds.fit$edf)
}


write.csv(res.mse,file=paste("aral/sim-mse-",samp.size,"-",noise.level,".csv",sep=""))
write.csv(res.edf,file=paste("aral/sim-edf-",samp.size,"-",noise.level,".csv",sep=""))

