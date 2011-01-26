# make some boxplots

pdf(file="mds-ramsay-boxplot.pdf",width=6,height=5)

# make the text look better for printout
par(cex.lab=0.75)

# b l t r
#par(mar=c(7,3,2,1)+.1)
#par(mgp=c(0,2,1))
par(las=1,cex=0.9,mfrow=c(1,3))


# ordering is mds, duchon, soap, tprs

for(err.lev in c("0.1","1","10")){

   mse<-read.csv(paste("ramsay-mse-250-",err.lev,".csv",sep=""))
   mse<-log(mse[,-1])
#   mse<-mse[,-1]
   
#   mses<-c(signif(mean(mse[,1],na.rm=TRUE),3),
#           signif(mean(mse[,2],na.rm=TRUE),3),
#           signif(mean(mse[,3],na.rm=TRUE),3))
#   
#   ses<-c(signif(sd(mse[,1],na.rm=TRUE),3),
#          signif(sd(mse[,2],na.rm=TRUE),3),
#          signif(sd(mse[,3],na.rm=TRUE),3))
#   
#   xlab=paste("MDS: MSE=",mses[1],"se(MSE)=",ses[1],"\n",
#              "soap: MSE=",mses[2],"se(MSE)=",ses[2],"\n",
#              "TPRS: MSE=",mses[3],"se(MSE)=",ses[3],"\n")
   
   boxplot(mse,main="")#,xlab=xlab)

}

dev.off()
