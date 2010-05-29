# do large scale simulations for wt2
# Copyright David Lawrence Miller 2009-2010.

source("mds.R")
source("sim/wt2-smooth-test.R")
source("intexp/smooth2.c.R")

######################################################
# The following models get tested:
#
#     * tprs
#     * mds+tp
#     * mds+cr
#     * mds+tp (3d)
#     * mds+tp + penalty adjustments
#     * soap
#     * 


######################################################
# OPTIONS
sim.size<-1#200
samp.size<-250
# noise levels = 0.35,0.9,1.55
# snr = 0.95,0.75,0.50
noise.level<-1.55



######################################################
# initial setup
# this pre-calculates some things to make life easier

## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE)

names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
gendata<-read.csv("wt2truth.csv",header=TRUE)

gendata<-list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

na.ind<-!(is.na(gendata$x)&is.na(gendata$y)&is.na(gendata$z))

gendata<-list(x=gendata$x[na.ind],
               y=gendata$y[na.ind],
               z=gendata$z[na.ind])

# attempt to get around the inside bug
bnd.neg<-list(x=-bnd$x,y=-bnd$y)
onoff<-inSide(bnd.neg,-gendata$x,-gendata$y)

gendata<-list(x=gendata$x[onoff],
               y=gendata$y[onoff],
               z=gendata$z[onoff])

# create the grid
my.grid<-make_soap_grid(bnd,n=10)

## do the MDS on the grid 
# create D
D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=1)

# perform mds on D
grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)
# same for 3d
grid.mds3<-cmdscale(D.grid,eig=TRUE,k=3,x.ret=TRUE)

### setup the soap knots
knots.x<-rep(seq(-2.9,2.9,length.out=15),15)
knots.y<-rep(seq(-2.9,3.6,length.out=15),rep(15,15))
insideknots<-inSide(bnd,knots.x,knots.y)
soap.knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])
soap.knots<-pe(soap.knots,-c(55,96,108))

### prediction data
gendata.ind <- read.csv("wt2truth.csv",header=TRUE)
ind<-c(1:length(gendata.ind$x))
pred.mat<-rep(NA,length(gendata.ind$x))
ind<-ind[gendata.ind$inside==1]
na.ind<-!(is.na(gendata.ind$x[gendata.ind$inside==1])&is.na(gendata.ind$y[gendata.ind$inside==1])&is.na(gendata.ind$z[gendata.ind$inside==1]))
ind<-ind[na.ind]
ind<-ind[onoff]
predd<-gendata.ind$z[ind]

#################################
# actually do the work now... 

# set up some containers
res.mse<-matrix(NA,sim.size,6)
res.edf<-matrix(NA,sim.size,6)

# do the sims
for(i in 1:sim.size){
   res<-wt2_smooth_test(samp.size=samp.size,noise.level=noise.level,plot.it=FALSE,
                          gendata,bnd,grid.mds,grid.mds3,my.grid,soap.knots,predd)

   res.mse[i,]<-res$mse
   res.edf[i,]<-res$edf
}

# put it all in a nice data frame
res.mse<-as.data.frame(res.mse)
names(res.mse)<-c("tprs","mds+tp","mds+cr","mds+tp(3D)","mds+tp+adj","soap")
res.edf<-as.data.frame(res.edf)
names(res.edf)<-c("tprs","mds+tp","mds+cr","mds+tp(3D)","mds+tp+adj","soap")

# write the files...
write.csv(res.mse,file=paste("sim/wt2-mse-",samp.size,"-",noise.level,".csv",sep=""))
write.csv(res.edf,file=paste("sim/wt2-edf-",samp.size,"-",noise.level,".csv",sep=""))
