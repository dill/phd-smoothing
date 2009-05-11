# esting for the convex hull code

source("convexhull.R")

# square
#xx<-rep(seq(-10,10,length.out=10),10)
#yy<-rep(seq(-10,10,length.out=10),rep(10,10))
#my.points<-list(x=xx,y=yy)

# random
my.points<-list(x=runif(1000),y=runif(1000))

plot(my.points,pch=".")

ch<-convex_hull(my.points)
#points(ch,col="blue")
lines(ch,col="red")
#text(ch,labels=c(1:length(ch$x)),pos=1)


library(soap)
onoff<-!inSide(bnd=ch,x=my.points$x,y=my.points$y)
missed<-list(x=my.points$x[onoff],y=my.points$y[onoff])
points(missed,col="red")



#for(times in c(1:500)){
#
#   my.points<-list(x=runif(1000),y=runif(1000))
#   ch<-convex_hull(my.points)
#   
#      
#
#
#}


