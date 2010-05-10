# do large scale simulations for wt2
# Copyright David Lawrence Miller 2009.

source("mds.R")
source("sim/wt2-smooth-test.R")
source("intexp/smooth2.c.R")

###############################
# initial setup

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

# mapped boundary...
#bnd.mds<-read.csv(file="wt2-bnd-in.csv")
#bnd.mds$X<-NULL
#bnd.mds[29,]<-bnd.mds[1,]
#bnd.mds<-insert.mds(bnd.mds,my.grid,grid.mds,bnd,faster=1)
#bnd.mds<-list(x=bnd.mds[,1],y=bnd.mds[,2])

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

sim.size<-200
samp.size<-250
# noise levels = 0.35,0.9,1.55
# snr = 0.95,0.75,0.50
noise.level<-1.55

res.mse<-list(mds=rep(0,sim.size), mdstp=rep(0,sim.size), 
              soap=rep(0,sim.size),tprs=rep(0,sim.size))

res.edf<-list(mds=rep(0,sim.size), mdstp=rep(0,sim.size), 
              soap=rep(0,sim.size),tprs=rep(0,sim.size))

for(i in 1:sim.size){
   res<-wt2_smooth_test(samp.size=samp.size,noise.level=noise.level,plot.it=FALSE,
                          gendata,bnd,grid.mds,my.grid,soap.knots)#,predd,bnd.mds)
   res.mse$mds[i]<- res$mds
#   res.mse$mdsmod[i]<- res$mdsmod
   res.mse$soap[i]<-res$soap
   res.mse$tprs[i]<-res$tprs
   res.edf$mds[i]<- res$mds.edf
#   res.edf$mdsmod[i]<- res$mdsmod.edf
   res.edf$soap[i]<-res$soap.edf
   res.edf$tprs[i]<-res$tprs.edf

}
write.csv(res.mse,file=paste("sim/wt2-mse-",samp.size,"-",noise.level,".csv",sep=""))
write.csv(res.edf,file=paste("sim/wt2-edf-",samp.size,"-",noise.level,".csv",sep=""))


