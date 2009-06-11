# simple test for the mds methods.

# load my code
source("mds.R")

# approximation to the horseshoe
nvertices<-11
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-2.5,-0.5)
polyvertices[2]<-complex(1,2.5,-0.5)
polyvertices[3]<-complex(1,3.5,0.5)
polyvertices[4]<-complex(1,3.5,2)
polyvertices[5]<-complex(1,2.5,3)
polyvertices[6]<-complex(1,-2.5,3)
polyvertices[7]<-complex(1,-2.5,1.5)
polyvertices[8]<-complex(1,2,1.5)
polyvertices[9]<-complex(1,2,1)
polyvertices[10]<-complex(1,-2.5,1)
polyvertices[11]<-polyvertices[1]

# Generate bivariate normal distribution
# load MASS
require(MASS)

# things that will eventually be args
lims<-c(-2.5,3.5,-0.5,3)
resolution<-20 # number of points in the kernel density estimate

### generate first MV normal
# this is for the "leg"
bivn.1 <- mvrnorm(2500, mu = c(-1, 1.5), Sigma = matrix(c(3, 1, 2, 3), 2))
bivn.kde.1 <- kde2d(bivn.1[,1], bivn.1[,2], n =resolution, lims=lims)


bivn.2 <- mvrnorm(2500, mu = c(-1, 0), Sigma = matrix(c(3, 1, 2, 3), 2))
bivn.kde.2 <- kde2d(bivn.2[,1], bivn.2[,2], n =resolution, lims=lims)


# make a grid
xn<-length(bivn.kde.1$x); yn<-length(bivn.kde.1$y)
xx<-rep(bivn.kde.1$x,yn); yy<-rep(bivn.kde.1$y,rep(xn,yn))

# blank the bottom 
bivn.kde.1$z[yy<1]<-0
bivn.kde.2$z[yy>1]<-0


# add them...
surf<-list(x=c(),y=c(),z=c())
surf$x<-bivn.kde.1$x
surf$y<-bivn.kde.1$y
surf$z<-100*(bivn.kde.1$z-bivn.kde.2$z)
# find the inside points...
library(soap)

# create the boundary
bnd<-list()
bnd$x<-Re(polyvertices)
bnd$y<-Im(polyvertices)


# find the inside points
inside.points.1<-inSide(bnd,xx,yy)
# attempt to get around the inside bug
bnd.neg<-list(x=-bnd$x,y=-bnd$y)
inside.points.2<-inSide(bnd.neg,-xx,-yy)
surf$z[!(inside.points.1&inside.points.2)]<-NA

# check that it looks okay...
axis.vals<-list(x=sort(unique(surf$x)),y=sort(unique(surf$y)))
image(axis.vals$x,axis.vals$y,matrix(surf$z,resolution,resolution))
contour(axis.vals$x,axis.vals$y,matrix(surf$z,resolution,resolution),add=TRUE)


#a<-scan()

#################### MESSY CODE ############################

# just map everything
x<-xx[inside.points.1&inside.points.2]
y<-yy[inside.points.1&inside.points.2]
z<-surf$z
D<-create_distance_matrix(x,y,bnd)
new.coords<-cmdscale(D)
data.mapped<-data.frame(x=new.coords[,1],y=new.coords[,2],z=z)
write.csv(data.mapped,file="simple.complete.csv")

# take a sample
data.mapped<-read.csv(file="simple-mapped.csv")
samp.ind<-sample(1:length(data.mapped$z),250)

x<-data.mapped$x[samp.ind]
y<-data.mapped$y[samp.ind]
z<-data.mapped$z[samp.ind]
samp.data<-data.frame(x=x,y=y,z=z)

# plot the results
par(mfrow=c(1,3))
surf<-read.csv(file="simple-truth.csv")
res<-20
axis.vals<-list(x=sort(unique(surf$x)),y=sort(unique(surf$y)))
image(axis.vals$x,axis.vals$y,matrix(surf$z,resolution,res),main="truth",asp=1)
contour(axis.vals$x,axis.vals$y,matrix(surf$z,resolution,res),add=TRUE)

#onoff<-inSide(bnd,surf$x,surf$y)&inSide(bnd.neg,-surf$x,-surf$y)

### mapping
b.mapped<-gam(z~s(x,y,k=49),data=samp.data)
fv <- predict(b.mapped,newdata=data.mapped)

pred.mat<-matrix(NA,res,res)
pred.mat[onoff]<-fv

image(axis.vals$x,axis.vals$y,pred.mat,main="transform+tprs",asp=1)
contour(axis.vals$x,axis.vals$y,pred.mat,add=T)


# tprs
b.tprs <- gam(z~s(x,y,k=49),data=data.frame(x=surf$x[samp.ind],y=surf$y[samp.ind],z=surf$z[samp.ind]))
fv.tprs <- predict(b.tprs,newdata=data.frame(x=surf$x,y=surf$y))

pred.mat<-matrix(fv.tprs,20,20)
pred.mat[!onoff]<-NA

image(axis.vals$x,axis.vals$y,pred.mat,main="thin plate",asp=1)
contour(axis.vals$x,axis.vals$y,pred.mat,add=T)


