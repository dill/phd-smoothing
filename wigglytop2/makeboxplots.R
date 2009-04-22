# make some boxplots

par(mfrow=c(4,2))

for (samp.size in c(1000,100)){
   for(noise.level in c(0.02, 2)){

      dat<-read.csv(paste("mses-",samp.size,"-",noise.level,".txt",sep=""))

      # uncomment for log scale
      tprs.mse.orig<-dat$tprs.mse
      sctprs.mse.orig<-dat$sctprs.mse
      dat$tprs.mse<-log(dat$tprs.mse)
      dat$sctprs.mse<-log(dat$sctprs.mse)

      boxplot(dat$tprs.mse,main=paste(samp.size,",",noise.level,"tprs"),ylim=ylims,
               xlab=paste("MSE=",signif(mean(tprs.mse.orig,na.rm=TRUE),5),"se(MSE)=",
               signif(sd(tprs.mse.orig,na.rm=TRUE),5)))
      boxplot(dat$sctprs.mse,main=paste(samp.size,",",noise.level,"sc+tprs"),ylim=ylims,
               xlab=paste("MSE=",signif(mean(sctprs.mse.orig,na.rm=TRUE),5),"se(MSE)=",
               signif(sd(sctprs.mse.orig,na.rm=TRUE),5)))

   }
}

