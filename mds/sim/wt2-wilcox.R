# perform Wilcoxon test to see if the MSEs are
# significantly different from soap


# model names
mod.names<-c("tprs","mds+tp","mds+cr","mds 3D","mds+adj")#,"soap")

# stick all of the results into one matrix
mses<-matrix(NA,200,0)

for(err.lev in c("0.35","0.9","1.55")){
#err.lev<-0.35

   mse<-read.csv(paste("wt2-mse-250-",err.lev,".csv",sep=""))
   mse<-mse[,-1]

# not sure if I need this, but makes the result more significant
#   # here I remove the three simulations where the PDE solution grid and knots
#   # line up and cause soap to fail
#   mse<-mse[-which(mse$soap>10),]



   #mse<-mse[,1:5]-mse[,6]
   #mses<-cbind(mses,mse)

   cat("noise level=",err.lev,"\n")
   for(i in 1:5){
      cat(mod.names[i],"vs. soap, p-value=",
          wilcox.test(mse[,i],mse[,6],paired=TRUE)$p.value,
          "\n")
   }
}

