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

knots.x<-rep(seq(-2.9,2.9,length.out=10),10)
knots.y<-rep(seq(-2.9,3.6,length.out=10),rep(10,10))
insideknots<-inSide(verts,knots.x,knots.y)
insideknots[59]<-FALSE
knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])

# this should be pretty obvious

for (samp.size in c(1000,500)){
   for(noise.level in c(0.02, 0.005)){
      source("../generic_sim_code/runsim.R")
   }
}


source("../generic_sim_code/makeboxplots.R")

