# make some boxplots
# plots on log scale, MSES reported underneath are _not_

#pdf(file="merged-mses-boxplot.pdf",width=4,height=10)

par(mfrow=c(4,1))
# make the text look better for printout
par(cex.lab=0.75)

# b l t r
par(mar=c(7,3,2,1)+.1)
par(mgp=c(6,2,0))

for (samp.size in c(1000,500)){
   for(noise.level in c(0.02, 0.005)){

      dat.1<-read.csv(paste("mses-",samp.size,"-",noise.level,".txt",sep=""))
      dat.2<-read.csv(paste("../wigglytop2-bbox/bboxmses-",samp.size,"-",noise.level,".txt",sep=""))
      
      dat<-rbind(dat.1,dat.2)


#      mses<-c(signif(mean(dat$mse[dat$labs=="SC+TPRS"],na.rm=TRUE),5),
#              signif(mean(dat$mse[dat$labs=="SC+TPRSbbox"],na.rm=TRUE),5),
#              signif(mean(dat$mse[dat$labs=="TPRS"],na.rm=TRUE),5),
#              signif(mean(dat$mse[dat$labs=="soap"],na.rm=TRUE),5)
#             )
#
#      ses<-c(signif(sd(dat$mse[dat$labs=="SC+TPRS"],na.rm=TRUE),5),
#             signif(sd(dat$mse[dat$labs=="SC+TPRSbbox"],na.rm=TRUE),5),
#              signif(sd(dat$mse[dat$labs=="TPRS"],na.rm=TRUE),5),
#              signif(sd(dat$mse[dat$labs=="soap"],na.rm=TRUE),5)
#            )
#
#      xlab=paste("SC+TPRS: MSE=",mses[1],"se(MSE)=",ses[1],"\n",
#                 "SC+TPRS (bounding box): MSE=",mses[1],"se(MSE)=",ses[1],"\n",
#                 "TPRS: MSE=",mses[2],"se(MSE)=",ses[2],"\n",
#                 "soap: MSE=",mses[3],"se(MSE)=",ses[3],"\n"
#                )

      boxplot(log(mse)~labs,data=dat,main=paste(samp.size," samples, noise=",noise.level,sep=""))#,xlab=xlab)
   }
}

#dev.off()
