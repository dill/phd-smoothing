# plot the alternative horseshoe for the doc

library(soap)

## create a boundary...
fsb <- list(fs.boundary())

## Plot the true function...
# This takes a while to plot but looks nicer
m<-300;n<-150
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

source("../../ramsay.alt.R")

tru <- matrix(ramsay.alt(xx,yy),m,n) ## truth

# output to pdf
pdf("altramsayhorseshoe.pdf",6,3)

image(xm,yn,tru,col=heat.colors(100),xlab="",ylab="",
      main="",axes=FALSE,asp=1)
lines(fsb[[1]],lwd=3)
contour(xm,yn,tru,levels=seq(-1,1,by=.1),add=TRUE)

dev.off()


