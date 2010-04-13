# make some tables

par(mfrow=c(1,3))

#ramsay
basefilename<-"ramsay-mse-250-"
errlevs<-c(0.1,1,10)

for(errlev in errlevs){

   dat<-read.csv(paste(basefilename,errlev,".csv",sep=""))
   dat<-dat[-1]
   boxplot(dat)

}

dev.copy2pdf(file="ramsay-mds-boxplots.pdf")

#wt2
basefilename<-"wt2-mse-250-"
errlevs<-c(0.05,0.5,5)

for(errlev in errlevs){

   dat<-read.csv(paste(basefilename,errlev,".csv",sep=""))

   # here I remove the two simulations where the PDE solution grid and knots
   # line up and cause soap to fail

   dat$soap[dat$soap>10]<-NA

   dat<-dat[-1]
   boxplot(dat,main=paste("sigma=",errlev))
}
dev.copy2pdf(file="wt2-mds-boxplots.pdf")
