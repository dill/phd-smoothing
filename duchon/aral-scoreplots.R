# Aral Sea data set analysis - evolce Duchon...

load("rly.RData")
# libraries
library(mgcv)
library(soap)

library(dillhandy)
library(mdspack)

# load the data and boundary
aral<-read.csv("aral.dat",sep=" ")
bnd<-read.csv("aralbnd.csv")

#zlims<-c(1.905461, 19.275249)
zlims<-c(1, 17)
z.levels<-pretty(zlims,15)

# first cut out the crap using inSide
onoff<-inSide(bnd,aral$lo,aral$la)

# converstion to km
aral.km<-latlong2km(aral$lo[onoff],aral$la[onoff],59.5,45)

aral.dat<-data.frame(x=aral.km$km.e,
                     y=aral.km$km.n,
                     chl=as.numeric(aral$chl[onoff]))

names(aral.dat)<-c("x","y","z")

# convert boundary to northings and eastings
bnd.km<-latlong2km(bnd[,2],bnd[,3],59.5,45)
bnd<-list(x=bnd.km$km.e,y=bnd.km$km.n)


# prediction grid
gm<-50;gn<-50
gxm <- seq(min(aral.dat$x),max(aral.dat$x),length=gm)
gyn<-seq(min(aral.dat$y),max(aral.dat$y),length=gn)
gxx <- rep(gxm,gn)
gyy<-rep(gyn,rep(gm,gn))
pred.onoff<-inSide(bnd,gxx,gyy)
pred.grid<-data.frame(x=gxx[pred.onoff],y=gyy[pred.onoff])

######################################################################
# plot setup
par(las=1,mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)

# set the x and y values for the image plot
aral.lab<-latlong2km(unique(sort(aral$lo)),unique(sort(aral$la)),59.5,45)
# set the plot limits
xlims<-c(min(aral.dat$x)-10,max(aral.dat$x)+10)
ylims<-c(min(aral.dat$y)-10,max(aral.dat$y)+10)


######################################################################
#### MDS

names(aral.dat)<-c("x","y","z")

plot.it<-function(dat,main.title){
   zlims<-c(1, 20)
   pred.mat<-matrix(NA,gm,gn)
   pred.mat[pred.onoff]<-dat$pred
   image(pred.mat,x=unique(gxx),y=unique(gyy),main=main.title,
         xlab="km (East)",ylab="km (North)",xlim=xlims,ylim=ylims,asp=1,zlim=zlims)
   contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,zlim=zlims,levels=z.levels)
   lines(bnd,lwd=2)
}

mds.fit<-gam.mds(aral.dat,pred.grid,bnd,grid.res=c(20,20),family=Gamma(link="log"),gam.method="ML") 
#plot.it(mds.fit,paste("mds ",mds.fit$mds.dim,"D",sep=""))


mds.fit2<-gam.mds(aral.dat,pred.grid,bnd,grid.res=c(20,20),family=Gamma(link="log"),gam.method="GCV.Cp") 


par(mfrow=c(1,2))

#par(las=1,mgp=c(2,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)
plot(mds.fit2$gcv.dim,type="l",xlab="MDS projection dimension",ylab="GCV")
plot(mds.fit$gcv.dim,type="l",
     xlab="MDS projection dimension",ylab=expression(ML[p]))

