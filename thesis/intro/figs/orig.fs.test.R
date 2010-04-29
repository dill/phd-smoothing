# plot the original fs.test function from Ramsay's paper

library(soap)

fs.test <- function(x,y,r0=.1,r=.5,l=3,b=1,exclude=TRUE)
## test function based on Tim Ramsay (2002) J.R.Statist. Soc. B
## 64(2):307-319 "Spline smoothing over difficult regions"
{ q <- pi*r/2 ## 1/2 length of semi-circle part of centre curve
  a <- d <- x*0 ## along and distance to arrays

  ## convert x,y to along curve and distance to curve (a,d) 
  ## co-ordinates. 0 distance along is at (x=-r,y=0)  

  ind <- x>=0 & y>0
  a[ind] <- q + x[ind]
  d[ind] <- y[ind]-r

  ind <- x>=0 & y<=0
  a[ind] <- -q - x[ind]
  d[ind] <- -r - y[ind]


  ind <- x < 0
  a[ind] <- -atan(y[ind]/x[ind])*r
  d[ind] <- sqrt(x[ind]^2+y[ind]^2) - r

  ## create exclusion index

  ind <- abs(d)>r-r0 | (x>l & (x-l)^2+d^2 > (r-r0)^2)

  f <- a*b # the original
  #f <- a*b+d^2

  if (exclude) f[ind] <- NA
  attr(f,"exclude") <- ind
  f
}

## create a boundary...
bnd <- fs.boundary()
# create points within the boundary 
m<-100;n<-100
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]

# truth
z.truth<-matrix(NA,m,n)
z.truth[onoff]<-fs.test(xx,yy)


pdf(file="orig-fs.pdf",2.5,1.25)

par(mar=c(0.2,1,0.2,1))
image(xm,yn,z.truth,col=heat.colors(100),xlab="",ylab="",main="",las=1,asp=1,lwd=2, axes=FALSE)
contour(xm,yn,z.truth,add=TRUE,levels=seq(-5,5,by=.25),labcex=0.3,lwd=0.5)
lines(bnd)

dev.off()

