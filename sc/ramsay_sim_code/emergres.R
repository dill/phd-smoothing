for(j in 1:length(nl)){

   # set the noise level
   noise.level<-nl[j]
   samp.size<-samps[j]

   mses<-read.csv(file=paste("tp-",noise.level,"-",samp.size,".results.txt",sep=""))

   


}

