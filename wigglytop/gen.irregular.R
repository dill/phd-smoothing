# Try the SC transform with a wiggly top

# Irregular domain
nvertices<-19
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-3,-2)
polyvertices[2]<-complex(1,3,-2)
polyvertices[3]<-complex(1,3,0.5)
polyvertices[4]<-complex(1,2.5,1)
polyvertices[5]<-complex(1,2.5,0.5)
polyvertices[6]<-complex(1,2,1)
polyvertices[7]<-complex(1,1.5,0.5)
polyvertices[8]<-complex(1,1,1)
polyvertices[9]<-complex(1,0.5,0.5)
polyvertices[10]<-complex(1,0,1)
polyvertices[11]<-complex(1,-0.5,0.5)
polyvertices[12]<-complex(1,-0.5,1)
polyvertices[13]<-complex(1,-1,0.5)
polyvertices[14]<-complex(1,-2,1.5)
polyvertices[15]<-complex(1,-2.5,3)
polyvertices[16]<-complex(1,-3,3.5)
polyvertices[17]<-complex(1,-2.5,1.5)
polyvertices[18]<-complex(1,-3,0.5)
polyvertices[19]<-polyvertices[1]

# write the vertives out to file
write.csv(list(x=Re(polyvertices),y=Im(polyvertices)),"figverts.csv",row.names=FALSE)

# now generate some data

# Generate bivariate normal distribution
# load MASS
require(MASS)

# things that will eventually be args
lims<-c(-3,3,-2,4)
resolution<-500 # number of points in the kernel density estimate

# generate first MV normal
bivn.1 <- mvrnorm(2500, mu = c(-2, 0), Sigma = matrix(c(2, 0, 0, 2), 2))
bivn.kde.1 <- kde2d(bivn.1[,1], bivn.1[,2], n =resolution, lims=lims)
# and the second...
bivn.2 <- mvrnorm(2500, mu = c(2, -1), Sigma = matrix(c(2, 0, 0, 2), 2))
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
z=as.vector(surf$z)
z[!inside.points]<-NA
image(matrix(z,resolution,resolution)); contour(matrix(z,resolution,resolution),add=TRUE)

# write out truth to a file
#write.csv(list(x=xx,y=yy,z=as.vector(surf$z),inside=ind),"fig9truth.csv",row.names=FALSE)

