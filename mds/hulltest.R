# testing for the convex hull code

source("convexhull.R")

# square
#xx<-rep(seq(-10,10,length.out=10),10)
#yy<-rep(seq(-10,10,length.out=10),rep(10,10))
#my.points<-list(x=xx,y=yy)

# random
my.points<-list(x=runif(1000),y=runif(1000))

plot(my.points,pch=".")

ch<-convex_hull(my.points)
points(ch,col="blue")
lines(ch,col="red")
text(ch,labels=c(1:length(ch$x)),pos=1)
