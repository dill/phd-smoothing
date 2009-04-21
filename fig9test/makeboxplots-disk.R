# make some boxplots

par(mfrow=c(4,3))

for (samp.size in c(1000,100)){
   for(noise.level in c(0.02, 2)){

      dat<-read.csv(paste("mses-disk-",samp.size,"-",noise.level,".txt",sep=""))

      # uncomment for log scale
      tprs.mse.orig<-dat$tprs.mse
      sctprs.mse.orig<-dat$sctprs.mse
      soap.mse.orig<-dat$soap.mse
      dat$tprs.mse<-log(dat$tprs.mse)
      dat$sctprs.mse<-log(dat$sctprs.mse)
      dat$soap.mse<-log(dat$soap.mse)

      ylims<-c(min(dat$tprs.mse,dat$sctprs.mse,dat$soap.mse),
               max(dat$tprs.mse,dat$sctprs.mse,dat$soap.mse))

      boxplot(dat$tprs.mse,main=paste(samp.size,",",noise.level,"tprs"),ylim=ylims,
               xlab=paste("MSE=",signif(mean(tprs.mse.orig,na.rm=TRUE),5),"se(MSE)=",
               signif(sd(tprs.mse.orig,na.rm=TRUE),5)))
      boxplot(dat$sctprs.mse,main=paste(samp.size,",",noise.level,"sc+tprs"),ylim=ylims,
               xlab=paste("MSE=",signif(mean(sctprs.mse.orig,na.rm=TRUE),5),"se(MSE)=",
               signif(sd(sctprs.mse.orig,na.rm=TRUE),5)))
      boxplot(dat$soap.mse,main=paste(samp.size,",",noise.level,"soap"),ylim=ylims,
               xlab=paste("MSE=",signif(mean(soap.mse.orig,na.rm=TRUE),5),"se(MSE)=",
               signif(sd(soap.mse.orig,na.rm=TRUE),5)))

   }
}

