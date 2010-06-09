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

# this should be pretty obvious
for (samp.size in c(1000,500)){
   for(noise.level in c(0.02, 0.005)){
      source("runsim.R")
   }
}


#source("../generic_sim_code/makeboxplots.R")

cat("*** Remember you need to merge graphs!")

