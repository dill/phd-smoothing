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
resolution<-50 # number of points in the kernel density estimate

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
inside.points<-inSide(bnd,xx,yy)
surf$z[!inside.points]<-NA

# attempt to get around the inside bug
bnd.neg<-list(x=-bnd$x,y=-bnd$y)
inside.points<-inSide(bnd.neg,-xx,-yy)
surf$z[!inside.points]<-NA

# check that it looks okay...
axis.vals<-list(x=sort(unique(surf$x)),y=sort(unique(surf$y)))
image(axis.vals$x,axis.vals$y,matrix(surf$z,resolution,resolution))
contour(axis.vals$x,axis.vals$y,matrix(surf$z,resolution,resolution),add=TRUE)

# take a sample
poss.samp<-c(1:length(surf$z))[!is.na(surf$z)]
samp.ind<-sample(poss.samp,250)

x<-xx[samp.ind]
y<-yy[samp.ind]
z<-surf$z[samp.ind]

plot(x,y,type="p")

D<-create_distance_matrix(x,y,bnd)

new.coords<-cmdscale(D)

data.mapped<-data.frame(x=new.coords[,1],y=new.coords[,2],z=z)



### mapping
b.mapped<-gam(z~s(x,y,k=49),data=data.mapped)
fv <- predict(b.mapped,newdata=data.mapped)


