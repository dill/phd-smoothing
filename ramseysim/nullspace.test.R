# nullspace testing...

library(soap)

fsb <- list(fs.boundary())

# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

## truth
tru <- fs.test(xx,yy) 

names(fsb[[1]]) <- c("v","w") ## correct boundary names

### first take a line that runs along the centre of the horseshoe
### leg x coordinates (these are the same for both)
leg.x<-seq(0,max(fsb[[1]]$v),0.01)
#reversed version
r.leg.x<-leg.x[length(leg.x):1]
# y coords
leg.y.t<-rep(0.5,length(leg.x))
leg.y.b<- -leg.y.t
# middle curve
# create a circle first
# by default r=0.5, use this.
r<-0.5
t<-seq(0,2*pi,0.01)
curve.x<-r*sin(t)
curve.y<-r*cos(t)
# discard the right half
curve.y<-curve.y[curve.x<0]
curve.x<-curve.x[curve.x<0]
# reverse
curve.y<-curve.y[length(curve.y):1]
curve.x<-curve.x[length(curve.x):1]

# container
fs.centreline<-list(x=c(r.leg.x,curve.x,leg.x),y=c(leg.y.t,curve.y,leg.y.b))

# test
pdf("../sc-writeup/figs/horseshoecentreline.pdf",6,3)
par(mfrow=c(1,2))
fsb <- fs.boundary()
m<-300;n<-150 
xm <- seq(-1,4,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
tru <- matrix(fs.test(xx,yy),m,n) ## truth
image(xm,yn,tru,col=heat.colors(100),xlab="",ylab="",asp=1,main="",axes=FALSE)
lines(fs.centreline,lty=2)
lines(fsb,lwd=3)

# let evaluate these points using the fs.test function
fs.centre.eval<-fs.test(fs.centreline$x,fs.centreline$y)

# this line is equiv. to...
# export to matlab
#write.csv(fs.centreline,"centreline.csv",row.names=FALSE)


# read the points in
mapped.centreline<-read.csv("centrelinemapped.csv",header=F)
names(mapped.centreline)<-c("x","y")

# load the prevertices, as mapped by matlab
preverts<-read.csv("ramsayprevertices.csv",header=F)
names(preverts)<-c("x","y") 

# load data
predback.real<-read.csv("../matlab/preal.csv",header=F)
predback.imag<-read.csv("../matlab/pimag.csv",header=F)
predgrid<-data.frame(v=predback.real[[1]],w=predback.imag[[1]])


# plot the morphed line
plot(mapped.centreline$y,mapped.centreline$x,asp=1,type="l",lty=2,axes=FALSE,xlab="",ylab="")
lines(c(preverts$y,preverts$y[1]),c(preverts$x,preverts$x[1]),lwd=3,asp=1)

dev.off()

# output
pdf("../sc-writeup/figs/centrelinelineplots.pdf",5,3)

## now do some plotting of these...
# in the original domain
par(mfrow=c(1,3),cex=0.5)
#plot(fs.centre.eval,type="l",main="",ylab="",xlab="")
#plot(fs.centreline$x,fs.centre.eval,type="l",main="")
plot(fs.centreline$y,fs.centre.eval,type="l",main="",asp=1,xlab="y",ylab="f")

# in the transformed domain
#plot(fs.centre.eval,type="l",main="",ylab="",xlab="")
#plot(mapped.centreline$x,fs.centre.eval,type="l",main="")
plot(mapped.centreline$y,fs.centre.eval,type="l",main="",asp=1,xlab="y*",ylab="f")


# finally do this for the "natural" coordinates
source("../fs.test.debug.R")
adlocs<-fs.test.debug(fs.centreline$x,fs.centreline$y)
plot(adlocs$a[length(adlocs$a):1],adlocs$f,type="l",main="",asp=1,xlab="a",ylab="f")

#off
dev.off()


