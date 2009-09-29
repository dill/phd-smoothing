# load soap
library(soap)
source("mds.R")


## create a boundary...
bnd <- fs.boundary()
bnd<-pe(bnd,seq(1,length(bnd$x),8))
bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))

## Simulate some fitting data, inside boundary...
#n<-1000
#x <- runif(n)*5-1; y<-runif(n)*2-1
#ind <- inSide(bnd,x=x,y=y) ## remove outsiders
#x<-x[ind];y <- y[ind]

# fitting grid 
m<-45;n<-25
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]
onoff.fix<-inSide(list(x=-bnd$x,y=-bnd$y),-xx,-yy)
x<-xx[onoff.fix];y<-yy[onoff.fix]
n<-length(xx)


#x<-c(-0.284091,0.022727)
#y<-c(-0.750000,0.5)


D<-create_distance_matrix(x,y,bnd)
