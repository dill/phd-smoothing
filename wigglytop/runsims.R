# run all the simulations

# load some libraries
library(mgcv)
library(soap)

# load some data...
true.vals<-read.csv("wttruth.csv",header=TRUE)
true.vals.mapped<-read.csv("wttruemapped.csv",header=FALSE)
names(true.vals.mapped)<-c("x","y","z")
# load the vertices data
verts<-read.csv("figverts.csv",header=FALSE)
names(verts)<-c("x","y")

# setup knots
knots.x<-rep(seq(-2.9,2.9,length.out=7),7)
knots.y<-rep(seq(-1.9,3.6,length.out=7),rep(7,7))
insideknots<-inSide(verts,knots.x,knots.y)
knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])

# this should be pretty obvious

for (samp.size in c(1000,500)){
   for(noise.level in c(0.02, 0.005)){
      source("../generic_sim_code/runsim.R")
   }
}

source("../generic_sim_code/makeboxplots.R")


