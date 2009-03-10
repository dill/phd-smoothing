# Show the mapping of a grid on the horseshoe

library(soap)

fsb <- list(fs.boundary())

# untransformed data
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

names(fsb[[1]]) <- c("x","y")
insiders<-inSide(fsb,x=xx,y=yy)

# load transformed data
predback.real<-read.csv("../../matlab/preal.csv",header=F)
predback.imag<-read.csv("../../matlab/pimag.csv",header=F)
grid<-data.frame(x=predback.real,y=predback.imag)
names(grid) <- c("x","y")


# do the plotting
pdf("hsgridmapping.pdf",6,4)
par(mfrow=c(1,2))

# plot the horseshoe
plot(xx[insiders],yy[insiders],pch=".",asp=1,xlab="",ylab="",cex=0.5,cex.axis=0.5)
lines(fsb[[1]],lwd=3)


# plot the transformed horseshoe
plot(grid$x[insiders],grid$y[insiders],pch=".",asp=1,xlab="",ylab="",cex=0.5,cex.axis=0.5)
fsb.mapped<-read.csv("fsbmapped.csv",header=F)
lines(fsb.mapped,lwd=3)

dev.off()
