# do large scale simulations for wt2
# Copyright David Lawrence Miller 2009.

source("mds.R")
source("wt2-smooth-test.R")

samp.size=250
noise.level=0.05


res.mse.mds<-rep(0,1000)
res.mse.soap<-rep(0,1000)
res.mse.tprs<-rep(0,1000)

for(i in 1:1000){
   wt2.res<-wt2_smooth_test(samp.size=samp.size,noise.level=noise.level,logfilename=NA, plot.it=FALSE)
   res.mse.mds[i]<-wt2.res$mses$mds
   res.mse.soap[i]<-wt2.res$mses$soap
   res.mse.tprs[i]<-wt2.res$mses$tprs
}


