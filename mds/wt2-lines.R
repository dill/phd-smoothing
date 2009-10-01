# test of lines across the domain for wt2


source("mds.R")


## create a boundary...
bnd <- read.csv("wt2-verts.csv",header=FALSE)
names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
gendata <- read.csv("wt2truth.csv",header=TRUE)

gendata<- list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

na.ind<-!(is.na(gendata$x)&is.na(gendata$y)&is.na(gendata$z))

gendata<- list(x=gendata$x[na.ind],
               y=gendata$y[na.ind],
               z=gendata$z[na.ind])

# attempt to get around the inside bug
bnd.neg<-list(x=-bnd$x,y=-bnd$y)
onoff<-inSide(bnd.neg,-gendata$x,-gendata$y)

gendata<- list(x=gendata$x[onoff],
               y=gendata$y[onoff],
               z=gendata$z[onoff])

# find some lines
# store the indices
my.line.index<-c(48:94,471:517,699:742,1003:1025,1101:1110)


# create D
D<-create_distance_matrix(gendata$x,gendata$y,bnd,logfile=logfilename)

# perform mds
samp.mds<-cmdscale(D,eig=TRUE,x.ret=TRUE)


# code to plot the lines we want to investigate
par(mfrow=c(1,2))
plot(gendata$x,gendata$y,xlab="x",ylab="y",asp=1)
lines(x=gendata$x[48:94],y=gendata$y[48:94],lwd=2,col="red")
lines(x=gendata$x[471:517],y=gendata$y[471:517],lwd=2,col="red")
lines(x=gendata$x[699:742],y=gendata$y[699:742],lwd=2,col="red")
lines(x=gendata$x[1003:1025],y=gendata$y[1003:1025],lwd=2,col="red")
lines(x=gendata$x[1101:1110],y=gendata$y[1101:1110],lwd=2,col="red")

# mapped points
plot(samp.mds$points,asp=1)
lines(x=samp.mds$points[48:94,1],y=samp.mds$points[48:94,2],lwd=2,col="red")
lines(x=samp.mds$points[471:517,1],y=samp.mds$points[471:517,2],lwd=2,col="red")
lines(x=samp.mds$points[699:742,1],y=samp.mds$points[699:742,2],lwd=2,col="red")
lines(x=samp.mds$points[1003:1025,1],y=samp.mds$points[1003:1025,2],lwd=2,col="red")
lines(x=samp.mds$points[1101:1110,1],y=samp.mds$points[1101:1110,2],lwd=2,col="red")



