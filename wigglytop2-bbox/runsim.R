#  simulation experiment

# should be run via runsims.R

# log this
#sink(paste("simlog-",samp.size,"-",noise.level,".txt",sep=""))

cat("Simulation starts!\n")
cat("Samples per iteration:",samp.size,"\n")
cat("Noise level:",noise.level,"\n")

# vars to hold the mses
sctprs.mse<-c()

# do 500 replicates
for (i in 1:500){

   # make a sample index
   this.sample<-sample(c(1:dim(true.vals)[1]),samp.size)
   
   # noise
   ran<-rnorm(samp.size)*noise.level
   
   # take the points from the true and true mapped
   samp.data<-data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1],z=true.vals$z[true.vals$inside==1])
   samp.data<-data.frame(x=samp.data$x[this.sample],y=samp.data$y[this.sample],z=samp.data$z[this.sample]+ran)
   samp.data.mapped<-data.frame(x=true.vals.mapped$x[this.sample],y=true.vals.mapped$y[this.sample],z=true.vals.mapped$z[this.sample]+ran)
   
   
   ### mapping
   b.mapped<-gam(z~s(x,y,k=100),data=samp.data.mapped)
   fv <- predict(b.mapped,newdata=data.frame(x=true.vals.mapped$x,y=true.vals.mapped$y))
   
   ### calculate the MSEs
   sctprs.mse<-c(sctprs.mse,mean((true.vals$z[true.vals$inside==1]-fv)^2,na.rm=T))
   
}

mses<-c(sctprs.mse)
labs<-c(rep("SC+TPRSbbox",length(sctprs.mse)))

mse.data<-list(mse=mses,labs=labs)

cat("writing to file:",paste("bboxmses-",samp.size,"-",noise.level,".txt\n",sep=""))
write.csv(mse.data,file=paste("bboxmses-",samp.size,"-",noise.level,".txt",sep=""))

cat("data written!\n")
 
cat("simulation done!\n")

#sink(NULL)
