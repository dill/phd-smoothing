# Try the SC transform with a less easy domain.

# Irregular domain
# Number of vertices
nvertices<-9
# Position of vertices
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,8.5921,0.3638116)
polyvertices[2]<-complex(1,8.82615,6.103527)
polyvertices[3]<-complex(1,3.403966,6.233975)
polyvertices[4]<-complex(1,2.896856,8.799454)
polyvertices[5]<-complex(1,-3.617567,7.755869)
polyvertices[6]<-complex(1,-2.837397,2.755360)
polyvertices[7]<-complex(1,-9.35182,0.9290866)
polyvertices[8]<-complex(1,-6.270146,-6.202075)
polyvertices[9]<-complex(1,5.900512,-7.637003)
polyvertices[10]<-polyvertices[1]


# now generate some data
# Test to see what smoothing looks like



# Generate bivariate normal distribution
# load MASS
require(MASS)

# things that will eventually be args
lims<-rep(c(-10,10),2)
resolution<-500 # number of points in the kernel density estimate

# generate first MV normal
bivn.1 <- mvrnorm(2500, mu = c(5, 5), Sigma = matrix(c(10, 0, 0, 10), 2))
bivn.kde.1 <- kde2d(bivn.1[,1], bivn.1[,2], n =resolution, lims=lims)
# and the second...
bivn.2 <- mvrnorm(2500, mu = c(0, -5), Sigma = matrix(c(10, 0, 0, 10), 2))
bivn.kde.2 <- kde2d(bivn.2[,1], bivn.2[,2], n =resolution, lims=lims)

# add them...
surf<-list(x=c(),y=c(),z=c())
surf$x<-bivn.kde.1$x
surf$y<-bivn.kde.1$y
surf$z<-bivn.kde.1$z+bivn.kde.2$z

# find the inside points...
library(soap)

# create the boundary
bnd<-list()
bnd$x<-Re(polyvertices)
bnd$y<-Im(polyvertices)

# make a grid
xn<-length(surf$x); yn<-length(surf$y)
xx<-rep(surf$x,yn); yy<-rep(surf$y,rep(xn,yn))

# find the inside points, make it 0/1 rather than T/F
inside.points<-inSide(bnd,xx,yy)
ind<-rep(0,length(inside.points))
ind[inside.points]<-1


# check that it looks okay...
#z=as.vector(surf$z)
#z[!inside.points]<-NA
#image(matrix(z,resolution,resolution)); contour(matrix(z,resolution,resolution),add=TRUE)

# write out truth to a file
write.csv(list(x=xx,y=yy,z=as.vector(surf$z),inside=ind),"fig9truth.csv",row.names=FALSE)

