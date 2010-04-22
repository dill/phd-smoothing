# draw a horshoe with bounding box

library(soap)
fsb <- list(fs.boundary())
names(fsb[[1]]) <- c("v","w") ## correct boundary names

# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

## truth
tru <- matrix(fs.test(xx,yy),m,n)

# define the bounding box
horse<-list(x=c(),y=c())
horse$x[1]<-max(fsb[[1]]$v)
horse$y[1]<-max(fsb[[1]]$w)
horse$x[2]<-min(fsb[[1]]$v)
horse$y[2]<-max(fsb[[1]]$w)
horse$x[3]<-min(fsb[[1]]$v)
horse$y[3]<-min(fsb[[1]]$w)
horse$x[4]<-max(fsb[[1]]$v)
horse$y[4]<-min(fsb[[1]]$w)
horse$x[5]<-max(fsb[[1]]$v)
horse$y[5]<-max(fsb[[1]]$w[fsb[[1]]$w<0 & fsb[[1]]$v>0.5])
horse$x[6]<-max(fsb[[1]]$v[fsb[[1]]$v<0])
horse$y[6]<-max(fsb[[1]]$w[fsb[[1]]$w<0 & fsb[[1]]$v>0.5])
horse$x[7]<-max(fsb[[1]]$v[fsb[[1]]$v<0])
horse$y[7]<-min(fsb[[1]]$w[fsb[[1]]$w>0 & fsb[[1]]$v>0.5])
horse$x[8]<-max(fsb[[1]]$v)
horse$y[8]<-min(fsb[[1]]$w[fsb[[1]]$w>0 & fsb[[1]]$v>0.5])


pdf("hswithboundingbox.pdf",6,3)
# plot the figure
image(xm,yn,tru,col=heat.colors(100),xlab="",ylab="",asp=1,main="",axes=FALSE,xlim=c(-2,5))
lines(fsb[[1]],lwd=3)

# vertex labels
text(horse$x[c(1,4)],horse$y[c(1,4)],labels=c(1,4),offset=0.3,cex=0.8,adj=c(-0.5,0))
text(horse$x[c(8)],horse$y[c(8)],labels=c(8),offset=0.3,cex=0.8,adj=c(-0.5,0))
text(horse$x[c(5)],horse$y[c(5)],labels=c(5),offset=0.3,cex=0.8,adj=c(-0.5,1))
text(horse$x[c(2,3)],horse$y[c(2,3)],labels=c(2,3),pos=2,offset=0.2,cex=0.8)
text(horse$x[c(6)],horse$y[c(6)],labels=c(6),pos=1,offset=0.3,cex=0.8)
text(horse$x[c(7)],horse$y[c(7)],labels=c(7),pos=3,offset=0.2,cex=0.8)

# bounding box
lines(horse$x[c(1,8)],horse$y[c(1,8)],lwd=3)
lines(horse,lwd=3)

dev.off()


