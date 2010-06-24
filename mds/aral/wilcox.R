# perform Wilcoxon test to see if the MSEs are
# significantly different from soap


# model names
mod.names<-c("tprs","mds+tp","mds+cr","mds 3D","mds+adj")#,"soap")

# stick all of the results into one matrix
mses<-matrix(NA,200,0)

for(samp.size in c("100","250","500")){
   for(err.lev in c("0.5","0.75","0.95")){
   #err.lev<-0.35
   
      mse<-read.csv(paste("sim-mse-",samp.size,"-",err.lev,".csv",sep=""))
      mse<-mse[,-1]
      #mse<-mse[,1:5]-mse[,6]
      #mses<-cbind(mses,mse)
   
      cat("sample size=",samp.size,"\n")
      cat("noise level=",err.lev,"\n")
      for(i in 1:5){
         cat(mod.names[i],"vs. soap, p-value=",
             wilcox.test(mse[,i],mse[,6],paired=TRUE)$p.value,
             "\n")
      }
   }
}

