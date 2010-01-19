# Aral Sea data set analysis

# run from phd-smoothing/mds


# libraries
library(mgcv)
library(soap)

# mds files
source("mds.R")

# load the data and boundary
aral<-read.csv("aral/aral.dat",sep=" ")
bnd<-read.csv("aral/aralbnd.csv")
bnd<-bnd[,c(2,3)]

image(x=unique(aral$la),y=unique(aral$lo),z=matrix(aral$chl,46,46),asp=1)

# first cut out the crap using inSide
onoff<-inSide(bnd,aral$la,aral$lo)

aral.dat<-data.frame(la=aral$la[onoff],
                     lo=aral$lo[onoff],
                     chl=aral$chl[onoff])




