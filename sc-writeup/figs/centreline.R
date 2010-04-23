# centreline fig


library(soap)

fsb <- list(fs.boundary())

# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

## truth
tru <- fs.test(xx,yy)

names(fsb[[1]]) <- c("v","w") ## correct boundary names


# first take a line that runs along the centre of the horseshoe
# can do this using the transform in fs.test

# leg x coordinates (these are the same for both)
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
pdf("horseshoecentreline.pdf",4,4)
fsb <- fs.boundary()
m<-300;n<-150 
xm <- seq(-1,4,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
tru <- matrix(fs.test(xx,yy),m,n) ## truth
image(xm,yn,tru,col=heat.colors(100),xlab="",ylab="",asp=1,main="",axes=FALSE)
lines(fs.centreline,lty=2)
lines(fsb,lwd=3)
dev.off()



