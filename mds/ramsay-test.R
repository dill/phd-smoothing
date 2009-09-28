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


# instead, creating a set of test points
#x<-c(1.4784634,1.0423811,2.0319526,3.1473170,3.1137722,-0.5845415,-0.6767896, -0.2840909 , 1.556818)
#y<-c(0.4050137,-0.5244773,-0.5244773,-0.5663463,0.5976109,0.2375378,-0.4574870,0.08333333 , -0.75 )

#x=c(-0.386364,0.329545);
#y=c(0.333333,-0.5);



# doesn't work?
#x=c( -0.2840909 , 1.556818 )
#y=c( 0.08333333 , -0.75 )


#x=c( 1.556818 , -0.3863636, -0.2840909 )
#y=c( -0.75 , 0.08333333, 0.08333333)

#x=c(0.984388, -0.421584)
#y=c(-0.297362,   0.188796)

#x=c(0.227273,-0.590909);
#y=c(-0.166667,0.25); 

D<-create_distance_matrix(x,y,bnd)
