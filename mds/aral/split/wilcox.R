# perform Wilcoxon test to see if the MSEs are
# significantly different from soap


# model names
mod.names<-c("tprs","mdstp","soap")

# stick all of the results into one matrix
mses<-matrix(NA,200,0)

#for(samp.size in c("100","250","500")){
#   for(err.lev in c("0.5","0.75","0.95")){
samp.size<-100
err.lev<-0.75
   #err.lev<-0.35
   
      mse<-read.csv(paste("sim-mse-",samp.size,"-",err.lev,".csv",sep=""))
      mse<-mse[,-1]
      #mse<-mse[,1:5]-mse[,6]
      #mses<-cbind(mses,mse)
   
      cat("sample size=",samp.size,"\n")
      cat("noise level=",err.lev,"\n")
      for(i in 1:3){
         cat(mod.names[i]," in vs. out, p-value=",
             wilcox.test(mse[,i],mse[,i+1],paired=TRUE)$p.value,
             "\n")
      }
#   }
#}

