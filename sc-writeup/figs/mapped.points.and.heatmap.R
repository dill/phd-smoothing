### plotting code for the mapped heatmap
# and the mapped points

library(soap)
## create a boundary...
fsb <- list(fs.boundary())

# make the prediction grid over the W domain
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

# values on the horseshoe
hs.vals<-fs.test(xx,yy)

# make the grid over the Z domain
# (read from Matlab)

# load data
predback.real<-read.csv("matlab/preal.csv",header=F)
predback.imag<-read.csv("matlab/pimag.csv",header=F)


# find the inside and outside points
# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names



par(mfrow=c(1,2))
image(xm,yn,matrix(fv,m,n),col=heat.colors(100),xlab="v",ylab="w")
contour(xm,yn,matrix(fv,m,n),levels=seq(-5,5,by=.25),add=TRUE)
image(xm,yn,matrix(fv.insiders,m,n),col=heat.colors(100),xlab="v",ylab="w")
contour(xm,yn,matrix(fv.insiders,m,n),levels=seq(-5,5,by=.25),add=TRUE)





