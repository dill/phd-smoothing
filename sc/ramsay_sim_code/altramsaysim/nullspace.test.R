# nullspace testing...

library(soap)

fsb <- list(fs.boundary())

# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

## truth
source("../ramsay.alt.R")
fs.test<-ramsay.alt
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
#pdf("../sc-writeup/figs/horseshoecentreline.pdf",6,3)
fsb <- fs.boundary()
m<-300;n<-150 
xm <- seq(-1,4,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
tru <- matrix(fs.test(xx,yy),m,n) ## truth

# let evaluate these points using the fs.test function
fs.centre.eval<-fs.test(fs.centreline$x,fs.centreline$y)

# read the points in
mapped.centreline<-read.csv("../ramsaysim/centrelinemapped.csv",header=F)
names(mapped.centreline)<-c("x","y")

# load the prevertices, as mapped by matlab
preverts<-read.csv("../ramsaysim/ramsayprevertices.csv",header=F)
names(preverts)<-c("x","y") 

# load data
predback.real<-read.csv("../../matlab/preal.csv",header=F)
predback.imag<-read.csv("../../matlab/pimag.csv",header=F)
predgrid<-data.frame(v=predback.real[[1]],w=predback.imag[[1]])

# output
#pdf("../sc-writeup/figs/altcentrelinelineplots.pdf",5,2)
pdf("altcentrelinelineplots.pdf",6,3)

## now do some plotting of these...
# in the original domain
par(mfrow=c(1,3),cex=0.7,las=1)
plot(fs.centreline$y,fs.centre.eval,type="l",main="",asp=1,xlab=expression(x[2]),ylab="f")

# in the transformed domain
plot(mapped.centreline$y,fs.centre.eval,type="l",main="",xlab=expression(x[2]^"*"),ylab="f")

# finally do this for the "natural" coordinates
source("../ramsay.alt.debug.R")
adlocs<-ramsay.alt.debug(fs.centreline$x,fs.centreline$y)
plot(adlocs$a[length(adlocs$a):1],adlocs$f,type="l",main="",xlab="a",ylab="f")

#off
dev.off()


