# make some boxplots

#pdf(file="big-aral-boxplot.pdf",width=6.5,height=8.25)

# make the text look better for printout
par(mfrow=c(1,1),cex.axis=0.65,las=1,mgp=c(2,0.75,0),mar=c(2,3,1,1))

modnames<-c("tprs","tprsin","mdstp","mdstpin","soap","soapin")

#for(sampsize in c(100,250,500)){
sampsize<-100
   # stick all of the results into one matrix
   mses<-matrix(NA,100,0)
   cols<-c()

#   for(err.lev in c("0.95","0.75","0.5")){
err.lev<-0.75   

      mse<-read.csv(paste("sim-mse-",sampsize,"-",err.lev,".csv",sep=""))
      mse<-mse[,-1]
      mse<-mse[,1:length(modnames)]
      mse<-mse[1:100,]
      mses<-cbind(mses,mse)
   
      cols<-c(cols,"white")
#   }
   
   # log the results
   mses<-log(mses)
   
   # model names
#   mod.names<-c("tprs","mds+tp","mds+cr","mds 3D","mds+adj","soap")
#   mod.names<-c("tprs","tprs (in)","mds+tp","mds+tp (in)","mds+3d","mds+3d (in)","soap","soap (in)")
   

   # do the plot
   boxplot(mses,main="",names=modnames,
         col=cols,
         medlwd=1,
         xlab="",
         ylab="log(mean MSE per realisation)")
#}



#dev.off()
