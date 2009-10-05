# diagram to explain the calculation of the within-area distance
# calculation

# this needs to be run within the mds directory!

# first source in the correct files
source("mds.R")

# W vertices
bnd <- read.csv("doubleyuh-verts.csv",header=FALSE)
names(bnd)<-c("x","y")

# points to show
x<-c(1,5.3)
y<-c(3,3.3)



# need 6 plots, format as 2x3
par(mfrow=c(2,3))

# create the initial path
source("R-prototype/wood.R")
source("R-prototype/utils.R")
bndpath<-make_bnd_path(list(x=x[1],y=y[1]),list(x=x[2],y=y[2]),bnd)

#### first part, line between p1 and p2
plot(bnd,type="l",axes=FALSE,xlab="",ylab="",lwd=2,asp=1)
points(x=x[1],y=y[1],pch=19);points(x=x[2],y=y[2],pch=19)
# line from left point to bnd
lines(x=c(x[1],bndpath$path.1$x[1]),y=c(y[1],bndpath$path.1$y[1]),col="green",lwd=2)
# bnd to bnd
lines(x=c(bndpath$path.1$x[1],bndpath$path.1$x[length(bndpath$path.1$x)]),y=c(bndpath$path.1$y[1],bndpath$path.1$y[length(bndpath$path.1$x)]),col="green",lwd=2,lty=3)
# bnd to right
lines(x=c(x[2],bndpath$path.1$x[length(bndpath$path.1$x)]),y=c(y[2],bndpath$path.1$y[length(bndpath$path.1$x)]),col="green",lwd=2)


#### second, both paths
plot(bnd,type="l",axes=FALSE,xlab="",ylab="",lwd=2,asp=1)
points(x=x[1],y=y[1],pch=19);points(x=x[2],y=y[2],pch=19)
lines(bndpath$path.1,lwd=2,col="green")
lines(bndpath$path.2,lwd=2,col="green",lty=2)


#### third, initial path 
plot(bnd,type="l",axes=FALSE,xlab="",ylab="",lwd=2,asp=1)
points(x=x[1],y=y[1],pch=19);points(x=x[2],y=y[2],pch=19)
# line from left point to bnd
lines(x=c(x[1],bndpath$path.1$x[1]),y=c(y[1],bndpath$path.1$y[1]),col="green",lwd=2)
# bnd to right
lines(x=c(x[2],bndpath$path.1$x[length(bndpath$path.1$x)]),y=c(y[2],bndpath$path.1$y[length(bndpath$path.1$x)]),col="green",lwd=2)
lines(bndpath$path.1,lwd=2,col="green")


#### fourth, first delete 
plot(bnd,type="l",axes=FALSE,xlab="",ylab="",lwd=2,asp=1)
points(x=x[1],y=y[1],pch=19);points(x=x[2],y=y[2],pch=19)
lines(x=c(x[1],bndpath$path.1$x[2:(length(bndpath$path.1$x)-1)],x[2]),y=c(y[1],bndpath$path.1$y[2:(length(bndpath$path.1$x)-1)],y[2]),col="green",lwd=2)


#### fifth, alter step
alt<-make_bnd_path(p1=pe(bndpath$path.1,2),p2=pe(bndpath$path.1,length(bndpath$path.1$x)-1),bnd)
plot(bnd,type="l",axes=FALSE,xlab="",ylab="",lwd=2,asp=1)
points(x=x[1],y=y[1],pch=19);points(x=x[2],y=y[2],pch=19)
lines(x=c(x[1],bndpath$path.1$x[2],alt$path.1$x,bndpath$path.1$x[length(bndpath$path.1$x)-1],x[2]),y=c(y[1],bndpath$path.1$y[2],alt$path.1$y,bndpath$path.1$y[length(bndpath$path.1$y)-1],y[2]),,col="green",lwd=2)


#### sixth, done! 
wp<-wood_path(list(x=x[1],y=y[1]),list(x=x[2],y=y[2]),bnd)
plot(bnd,type="l",axes=FALSE,xlab="",ylab="",lwd=2,asp=1)
points(x=x[1],y=y[1],pch=19);points(x=x[2],y=y[2],pch=19)
# line from left point to bnd
lines(wp,col="green",lwd=2)




