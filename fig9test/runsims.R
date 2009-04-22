# run all the simulations
# load some libraries
library(mgcv)
library(soap)

# load some data...
true.vals<-read.csv("fig9truth.csv",header=TRUE)
# load the vertices data
verts<-read.csv("figverts.csv",header=FALSE)
names(verts)<-c("x","y")

# setup knots for soap
# this is a faff
knots.x<-rep(seq(-10,10,length.out=7),7)
knots.y<-rep(seq(-10,10,length.out=7),rep(7,7))
insideknots<-inSide(verts,knots.x,knots.y)
knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])

for (domain in c("disk","rect")){

   true.vals.mapped<-read.csv(paste("fig9truemapped-",domain,".csv",sep=""),header=FALSE)
   names(true.vals.mapped)<-c("x","y","z")

   for (samp.size in c(1000,500)){
      for(noise.level in c(0.02, 0.005)){
         source("runsim.R")

      }
   }

   # make the plots at this point
   source("makeboxplots.R")

}



