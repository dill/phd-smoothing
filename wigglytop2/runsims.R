# run all the simulations

# this should be pretty obvious

# load some libraries
library(mgcv)
library(soap)

# load some data...
true.vals<-read.csv("wt2truth.csv",header=TRUE)
true.vals.mapped<-read.csv("wt2truemapped.csv",header=FALSE)
names(true.vals.mapped)<-c("x","y","z")
# load the vertices data
verts<-read.csv("figverts.csv",header=FALSE)
names(verts)<-c("x","y")

# setup knots
# this is a faff
knots.x<-rep(seq(-2.9,2.9,length.out=10),10)
knots.y<-rep(seq(-2.9,3.6,length.out=10),rep(10,10))
insideknots<-inSide(verts,knots.x,knots.y)
# just get rid of one knot on the boundary, this is a bit horrible, look away now...
insideknots[59]<-FALSE
knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])
#plot(verts,type="l");points(knots,col="red");text(knots,labels=c(1:dim(knots)[1]))
boundary.knots<-20

for (samp.size in c(1000,500)){
   for(noise.level in c(0.02, 0.005)){
      source("../generic_sim_code/runsim.R")
   }
}


source("../generic_sim_code/makeboxplots.R")

