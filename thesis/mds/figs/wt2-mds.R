# wt2 MDS diagram

source("mds.R")
bnd <- read.csv("wt2-verts.csv",header=FALSE)
names(bnd)<-c("x","y")

# data
gendata <- read.csv("wt2truth.csv",header=TRUE)

gendata<- list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

na.ind<-!(is.na(gendata$x)&is.na(gendata$y)&is.na(gendata$z))

gendata<- list(x=gendata$x[na.ind],
               y=gendata$y[na.ind],
               z=gendata$z[na.ind])

# attempt to get around the inside bug
bnd.neg<-list(x=-bnd$x,y=-bnd$y)
onoff<-inSide(bnd.neg,-gendata$x,-gendata$y)

gendata<- list(x=gendata$x[onoff],
               y=gendata$y[onoff],
               z=gendata$z[onoff])

# find the distances
D<-create_distance_matrix(gendata$x,gendata$y,bnd)



# 2d case
mds2<-cmdscale(D,eig=TRUE,x.ret=TRUE,k=2)

## plot it
par(mfrow=c(1,2))
plot(gendata$x,gendata$y,asp=1,pch=19,cex=0.3,xlab="x",ylab="y")
lines(bnd)
plot(mds2$points[,1],mds2$points[,2],asp=1,pch=19,cex=0.3,xlab=expression(tilde(x)),ylab=expression(tilde(y)))

# 3d case
mds3<-cmdscale(D,eig=TRUE,x.ret=TRUE,k=3)

## plot it
par(mfrow=c(1,3))
plot(mds3$points[,1],mds3$points[,2],asp=1,pch=19,cex=0.3,xlab=expression(tilde(x)),ylab=expression(tilde(y)))
plot(mds3$points[,2],mds3$points[,3],asp=1,pch=19,cex=0.3,xlab=expression(tilde(y)),ylab=expression(tilde(z)))
plot(mds3$points[,1],mds3$points[,3],asp=1,pch=19,cex=0.3,xlab=expression(tilde(x)),ylab=expression(tilde(z)))
