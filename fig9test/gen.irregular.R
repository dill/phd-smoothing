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

lims<-rep(c(-5,5),2)

# generate first MV normal
bivn.1 <- mvrnorm(2500, mu = c(2, 2), Sigma = matrix(c(3, 0, 0, 3), 2))
bivn.kde.1 <- kde2d(bivn.1[,1], bivn.1[,2], n =100, lims=lims)
# and the second...
bivn.2 <- mvrnorm(2500, mu = c(-2, -2), Sigma = matrix(c(3, 0, 0, 3), 2))
bivn.kde.2 <- kde2d(bivn.2[,1], bivn.2[,2], n =100, lims=lims)

# add them...
surf<-list(x=c(),y=c(),z=c())
surf$x<-bivn.kde.1$x+bivn.kde.2$x
surf$y<-bivn.kde.1$y+bivn.kde.2$y
surf$z<-bivn.kde.1$z+bivn.kde.2$z

# check that it looks okay...
#image(surf); contour(surf, add = T)


# find the inside points...
library(soap)

# create the boundary
bnd<-list()
bnd$x<-Re(polyvertices)
bnd$y<-Im(polyvertices)

xn<-length(surf$x); yn<-length(surf$y)
xx<-rep(surf$x,yn); yy<-rep(surf$y,rep(xn,yn))


inside.points<-inSide(bnd,xx,yy)

#surf$x[!inside.points]<-surf$y[!inside.points]<-NA
surf$z[!inside.points]<-NA


# pick some points and sent to matlab




# do an mgcv fit
library(mgcv)



