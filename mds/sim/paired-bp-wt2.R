# make some boxplots

#pdf(file="paired-mds-wt2-boxplot.pdf",width=13.3,height=7.88)

# make the text look better for printout
par(cex.axis=0.75,las=1,mgp=c(2,0.75,0),mar=c(2,4,1,1))

# stick all of the results into one matrix
mses<-matrix(NA,200,0)

for(err.lev in c("0.35","0.9","1.55")){
#err.lev<-0.35

   mse<-read.csv(paste("wt2-mse-250-",err.lev,".csv",sep=""))
   mse<-mse[,-1]
   mse<-mse[,1:5]-mse[,6]
   mses<-cbind(mses,mse)

}

# log the results
#mses<-log(mses)

# model names
mod.names<-c("tprs","mds+tp","mds+cr","mds 3D","mds+adj")#,"soap")

# do the plot
boxplot(mses,main="",
      names=rep(mod.names,3),
#      names=mod.names,
      xlab="",
      ylab="Difference between mean soap MSE and\n mean model MSE per realisation")

abline(h=0,col="red")


#dev.off()
