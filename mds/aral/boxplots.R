# make some boxplots

pdf(file="big-aral-boxplot.pdf",width=6.5,height=8.25)

# make the text look better for printout
par(mfrow=c(3,1),cex.axis=0.65,las=1,mgp=c(2,0.75,0),mar=c(2,3,1,1))


for(sampsize in c(100,250,500)){
   # stick all of the results into one matrix
   mses<-matrix(NA,200,0)
   cols<-c()

   for(err.lev in c("0.95","0.75","0.5")){
   
      mse<-read.csv(paste("sim-mse-",sampsize,"-",err.lev,".csv",sep=""))
      mse<-mse[,-1]
      mses<-cbind(mses,mse)
   
      # extra Wilcoxon test stuff
      for(i in 1:5){
         if(wilcox.test(mse[,i],mse[,6],paired=TRUE)$p.value<0.01){
            cols<-c(cols,"grey")
         }else{
            cols<-c(cols,"white")
         }
      }
      cols<-c(cols,"white")
   }
   
   # log the results
   mses<-log(mses)
   
   # model names
#   mod.names<-c("tprs","mds+tp","mds+cr","mds 3D","mds+adj","soap")
   mod.names<-c("tprs","mds+tp","mds+cr","mds3D","mds+adj","soap")
   
   # do the plot
   boxplot(mses,main="",names=rep(mod.names,3),
         col=cols,
         medlwd=1,
         xlab="",
         ylab="log(mean MSE per realisation)")
}



dev.off()
