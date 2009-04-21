# make some boxplots

par(mfrow=c(4,2))
par(cex.lab=0.5)
for (samp.size in c(1000,100)){
   for(noise.level in c(0.02, 2)){

      dat<-read.csv(paste("mses-",samp.size,"-",noise.level,".txt",sep=""))

      ylims<-c(min(log(dat$tprs.mse),log(dat$sctprs.mse)),max(log(dat$tprs.mse),log(dat$sctprs.mse)))

      boxplot(log(dat$tprs.mse),main=paste(samp.size,",",noise.level,"tprs"),ylim=ylims,
               xlab=paste("MSE=",signif(mean(dat$tprs.mse,na.rm=TRUE),5),"se(MSE)=",
               signif(sd(dat$tprs.mse,na.rm=TRUE),5)))
      boxplot(log(dat$sctprs.mse),main=paste(samp.size,",",noise.level,"sc+tprs"),ylim=ylims,
               xlab=paste("MSE=",signif(mean(dat$sctprs.mse,na.rm=TRUE),5),"se(MSE)=",
               signif(sd(dat$sctprs.mse,na.rm=TRUE),5)))

   }
}

