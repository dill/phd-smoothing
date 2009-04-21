#  simulation experiment

# load some libraries
library(mgcv)
library(soap)

# load some data...
true.vals<-read.csv("fig9truth.csv",header=TRUE)
true.vals.mapped<-read.csv(paste("fig9truemapped-",domain,".csv",sep=""),header=FALSE)
names(true.vals.mapped)<-c("x","y","z")

# load the vertices data
verts<-read.csv("figverts.csv",header=FALSE)
names(verts)<-c("x","y")

# setup knots
# this is a faff
knots.x<-rep(seq(-10,10,length.out=5),5)
knots.y<-rep(seq(-10,10,length.out=5),rep(5,5))
insideknots<-inSide(verts,knots.x,knots.y)
knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])


#### OPTIONS TO SET ####
# these are set in the file above at the moment
# how many points to sample
#samp.size<-10000
# noise level
#noise.level<-0.02
# domain
#domain<-"disk"

#### END OF OPTIONS ####

# log this
sink(paste("simlog-",domain,"-",samp.size,"-",noise.level,".txt",sep=""))

cat("Simulation starts!\n")
cat("Samples per iteration:",samp.size,"\n")
cat("Noise level:",noise.level,"\n")
cat("Domain:",domain,"\n")

# vars to hold the mses
sctprs.mse<-c()
tprs.mse<-c()
soap.mse<-c()

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
   b.mapped<-gam(z~s(x)+s(y),data=samp.data.mapped)
   fv <- predict(b.mapped,newdata=data.frame(x=true.vals.mapped$x,y=true.vals.mapped$y))
   
   ### normal tprs
   b.tprs<-gam(z~s(x)+s(y),data=samp.data)
   fv.tprs <- predict(b.tprs,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))
   
   
   ### soap fit
   b.soap<-gam(z~s(x,y,bs="so",xt=list(bnd=list(verts)),k=25),data=samp.data,knots=knots)
   fv.soap <- predict(b.soap,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))
   
   ### calculate the MSEs
   sctprs.mse<-c(sctprs.mse,mean((true.vals$z[true.vals$inside==1]-fv)^2,na.rm=T))
   tprs.mse<-c(tprs.mse,mean((true.vals$z[true.vals$inside==1]-fv.tprs)^2,na.rm=T))
   soap.mse<-c(soap.mse,mean((true.vals$z[true.vals$inside==1]-fv.soap)^2,na.rm=T))
   
}

mse.data<-cbind(sctprs.mse,tprs.mse,soap.mse)

cat("writing to file:",paste("mses-",domain,"-",samp.size,"-",noise.level,".txt",sep=""))
write.csv(mse.data,file=paste("mses-",domain,"-",samp.size,"-",noise.level,".txt",sep=""))

cat("data written!\n")
 
cat("simulation done!\n")

sink(NULL)
