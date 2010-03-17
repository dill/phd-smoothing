# make some boxplots
# plots on log scale, MSES reported underneath are _not_

for (domain in c("disk","rect")){
   for (samp.size in c(1000,500)){
      for(noise.level in c(0.02, 0.005)){
   
         dat<-read.csv(paste("mses-",domain,"-",samp.size,"-",noise.level,".txt",sep=""))
         
         mses<-c(signif(mean(dat$mse[dat$labs=="SC+TPRS"],na.rm=TRUE),5),
                 signif(mean(dat$mse[dat$labs=="TPRS"],na.rm=TRUE),5),
                 signif(mean(dat$mse[dat$labs=="soap"],na.rm=TRUE),5))
   
         ses<-c(signif(sd(dat$mse[dat$labs=="SC+TPRS"],na.rm=TRUE),5),
                 signif(sd(dat$mse[dat$labs=="TPRS"],na.rm=TRUE),5),
                 signif(sd(dat$mse[dat$labs=="soap"],na.rm=TRUE),5))
   
         # Method & Noise level & Sample size & MSE & se(MSE)\\
   
         this.line<-paste(
                  "SC+TPRS & ",noise.level," & ",samp.size," & ",mses[1]," & ",ses[1],"\\\\\n",
                  "TPRS & ",noise.level," & ",samp.size," & ",mses[2]," & ",ses[2],"\\\\\n",
                  "soap & ",noise.level," & ",samp.size," & ",mses[3]," & ",ses[3],"\\\\\n",
                  sep="")
   
         cat(this.line) 
         
   
      }
   }
   cat("\n\n")
}
