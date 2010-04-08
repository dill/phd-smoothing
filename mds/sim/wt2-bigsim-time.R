# do large scale simulations for wt2
# Copyright David Lawrence Miller 2009.

source("mds.R")
source("wt2-smooth-test-time.R")


faster<-0

###############################
# initial setup

## create a boundary...
bnd <- read.csv("../wt2-verts.csv",header=FALSE)

names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
gendata<-read.csv("../wt2truth.csv",header=TRUE)

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
source("../wt2-create-grid.R")
grid.time<-system.time(my.grid<-wt2_create_grid(40))[3]

## do the MDS on the grid 
# create D
grid.time<-grid.time+system.time(D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=faster))[3]

# perform mds on D
grid.time<-grid.time+system.time(grid.mds<-cmdscale(D.grid,
                                 eig=TRUE,k=2,x.ret=TRUE))[3]


### setup the soap knots
knots.x<-rep(seq(-2.9,2.9,length.out=15),15)
knots.y<-rep(seq(-2.9,3.6,length.out=15),rep(15,15))
insideknots<-inSide(bnd,knots.x,knots.y)
soap.knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])
soap.knots<-pe(soap.knots,-c(55,96,108)) # kill bad knots

### prediction data
gendata.ind <- read.csv("../wt2truth.csv",header=TRUE)
ind<-c(1:length(gendata.ind$x))
pred.mat<-rep(NA,length(gendata.ind$x))
ind<-ind[gendata.ind$inside==1]
na.ind<-!(is.na(gendata.ind$x[gendata.ind$inside==1])&is.na(gendata.ind$y[gendata.ind$inside==1])&is.na(gendata.ind$z[gendata.ind$inside==1]))
ind<-ind[na.ind]
ind<-ind[onoff]
predd<-gendata.ind$z[ind]




#################################

sim.size<-10
samp.size<-250
noise.level<-0.5

gam.times<-list(mds=rep(0,sim.size), mdstp=rep(0,sim.size), 
              soap=rep(0,sim.size),tprs=rep(0,sim.size))

pred.times<-list(mds=rep(0,sim.size), mdstp=rep(0,sim.size), 
              soap=rep(0,sim.size),tprs=rep(0,sim.size))

for(i in 1:sim.size){
   res<-wt2_smooth_test(samp.size=samp.size,noise.level=noise.level,plot.it=FALSE,
                          gendata,bnd,grid.mds,my.grid,soap.knots,predd,faster)
   gam.times$mds[i]<- res$mds[1]+grid.time
   gam.times$mdstp[i]<- res$mdstp[1]+grid.time
   gam.times$soap[i]<-res$soap[1]
   gam.times$tprs[i]<-res$tprs[1]


   pred.times$mds[i]<- res$mds[2]+grid.time
   pred.times$mdstp[i]<- res$mdstp[2]+grid.time
   pred.times$soap[i]<-res$soap[2]
   pred.times$tprs[i]<-res$tprs[2]

}
#write.csv(res.mse,file=paste("wt2-",samp.size,"-",noise.level,".csv",sep=""))

#sink("wt2-times.txt")
cat(mean(gam.times$mds),"\n")
cat(mean(gam.times$mdstp),"\n")
cat(mean(gam.times$soap),"\n")
cat(mean(gam.times$tprs),"\n")
cat(mean(pred.times$mds),"\n")
cat(mean(pred.times$mdstp),"\n")
cat(mean(pred.times$soap),"\n")
cat(mean(pred.times$tprs),"\n")
#sink()
