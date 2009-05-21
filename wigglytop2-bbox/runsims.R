# run all the simulations
# load some libraries
library(mgcv)
library(soap)

# load some data...
true.vals<-read.csv("wtbbtruth.csv",header=TRUE)
true.vals.mapped<-read.csv("wtbbtruemapped.csv",header=FALSE)
names(true.vals.mapped)<-c("x","y","z")
# load the vertices data
verts<-read.csv("figverts.csv",header=FALSE)
names(verts)<-c("x","y")

knots.x<-rep(seq(-2.9,2.9,length.out=15),15)
knots.y<-rep(seq(-2.9,3.6,length.out=15),rep(15,15))
insideknots<-inSide(verts,knots.x,knots.y)
insideknots[158]<-FALSE;insideknots[56]<-FALSE;insideknots[141]<-FALSE # for 15x15
boundary.knots<-49


#knots.x<-rep(seq(-2.9,2.9,length.out=10),10)
#knots.y<-rep(seq(-2.9,3.6,length.out=10),rep(10,10))
#insideknots<-inSide(verts,knots.x,knots.y)
#insideknots[59]<-FALSE
#boundary.knots<-30

knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])

# this should be pretty obvious

for (samp.size in c(1000,500)){
   for(noise.level in c(0.02, 0.005)){
      source("runsim.R")
   }
}


#source("../generic_sim_code/makeboxplots.R")

cat("*** Remember you need to merge graphs!")

