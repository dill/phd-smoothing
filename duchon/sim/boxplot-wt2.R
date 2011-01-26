# make some boxplots

pdf(file="big-mds-wt2-boxplot.pdf",width=13.3,height=7.88)

# make the text look better for printout
par(cex.axis=0.75,las=1,mgp=c(2,0.75,0),mar=c(2,3,1,1))

# stick all of the results into one matrix
mses<-matrix(NA,100,0)

# model names
mod.names<-c("tprs","mds+tp","mds+cr","mds 3D","mds+adj","soap","duchon")

for(err.lev in c("0.35","0.9","1.55")){

   mse<-read.csv(paste("wt2-mse-250-",err.lev,".csv",sep=""))
   mse<-mse[,-1]
   mse<-mse[1:100,]
   mses<-cbind(mses,mse)

#   cols<-c()
#
#   # extra Wilcoxon test stuff
#   for(i in 1:5){
#      pv<-wilcox.test(mse[,6],mse[,i],paired=TRUE)$p.value
#      med<-median(mse[,i]-mse[,6])
#      if(pv<0.01 & med>0){
#         cols<-c(cols,"red")
#      }else if(pv<0.01 & med<0){
#         cols<-c(cols,"green")
#      }else{
#         cols<-c(cols,"white")
#      }
#      cat("soap vs. ",mod.names[i],pv,"\n")
#   }
#   cols<-c(cols,"white")

}


# 3 spurious soap results, remove them
#spurious<-c(111,135,157)
#mses<-mses[-spurious,]


# log the results
mses<-log(mses)


# do the plot
boxplot(mses,main="",names=rep(mod.names,3),
      xlab="",
#      col=cols,
      medlwd=1,
      ylab="log(mean MSE per realisation)")



#dev.off()
