# make some boxplots

pdf(file="big-mds-wt2-boxplot.pdf",width=13.3,height=7.88)

# make the text look better for printout
par(cex.axis=0.75,las=1,mgp=c(2,0.75,0),mar=c(2,3,1,1))

# stick all of the results into one matrix
mses<-matrix(NA,200,0)

for(err.lev in c("0.35","0.9","1.55")){

   mse<-read.csv(paste("wt2-mse-250-",err.lev,".csv",sep=""))
   mse<-mse[,-1]
   mses<-cbind(mses,mse)

}

# log the results
mses<-log(mses)

# model names
mod.names<-c("tprs","mds+tp","mds+cr","mds 3D","mds+adj","soap")

# do the plot
boxplot(mses,main="",names=rep(mod.names,3),
      xlab="",
      ylab="log(mean MSE per realisation)")



dev.off()
