# Try the SC transform with a wiggly top

# Irregular domain
nvertices<-29
polyvertices<-vector("complex",nvertices)

polyvertices[1]<-complex(1,-3.0000, -3.000)
polyvertices[2]<-complex(1,3.2500, -3.000)
polyvertices[3]<-complex(1,3.2500, -0.750)
polyvertices[4]<-complex(1,3.0000, -0.250)
polyvertices[5]<-complex(1,2.7500, -0.750)
polyvertices[6]<-complex(1,2.2500, 0.750)
polyvertices[7]<-complex(1,2.0000, -1.000)
polyvertices[8]<-complex(1,1.5000, 0.2002)
polyvertices[9]<-complex(1,1.2500, -1.500)
polyvertices[10]<-complex(1,0.7500, 0.750)
polyvertices[11]<-complex(1,0.2500, -0.250)
polyvertices[12]<-complex(1,0, 1.750)
polyvertices[13]<-complex(1,-0.5000, -1.250)
polyvertices[14]<-complex(1,-1.0000,1.1840)
polyvertices[15]<-complex(1,-0.7500, 1.250)
polyvertices[16]<-complex(1,-0.7500, 2.000)
polyvertices[17]<-complex(1,-1.2500, 3.000)
polyvertices[18]<-complex(1,-1.5000, 1.500)
polyvertices[19]<-complex(1,-1.5000, 0.250)
polyvertices[20]<-complex(1,-1.5000, -1.000)
polyvertices[21]<-complex(1,-1.7500, -1.000)
polyvertices[22]<-complex(1,-2.5000, 0.500)
polyvertices[23]<-complex(1,-2.5000, 1.500)
polyvertices[24]<-complex(1,-1.7500, 3.000)
polyvertices[25]<-complex(1,-2.7500, 2.500)
polyvertices[26]<-complex(1,-3.2500, 0.750)
polyvertices[27]<-complex(1,-2.7500, -1.000)
polyvertices[28]<-complex(1,-3.0000, -2.250)
polyvertices[29]<-polyvertices[1]

# write the vertives out to file
#write.csv(list(x=Re(polyvertices),y=Im(polyvertices)),"figverts.csv",row.names=FALSE)

# now generate some data

# Generate bivariate normal distribution
# load MASS
require(MASS)

# things that will eventually be args
lims<-c(-3,3.5,-3,3)
resolution<-50 # number of points in the kernel density estimate

surf<-list(x=c(),y=c(),z=c())
surf$x<-seq(lims[1],lims[2],length=50)
surf$y<-seq(lims[3],lims[4],length=50)
surf$z<-rep(0,2500)

surf$z[xx > -1.5 & xx < -0.5]<-surf$z[xx > -1.5 & xx < -0.5]+seq(0,4,length.out=length(surf$z[xx > -1.5 & xx < -0.5]))
surf$z[xx < -1.5]<-surf$z[xx < -1.5]+seq(0,-4,length.out=length(surf$z[xx < -1.5]))

# two extra bits
bivn.4 <- mvrnorm(2500, mu = c(0.244, -0.425), Sigma = matrix(c(0.2, 0, 0, 0.2), 2))
bivn.kde.4 <- kde2d(bivn.4[,1], bivn.4[,2], n =resolution, lims=lims)
bivn.5 <- mvrnorm(2500, mu = c(2.247, -1.91), Sigma = matrix(c(1, 0, 0, 1), 2))
bivn.kde.5 <- kde2d(bivn.5[,1], bivn.5[,2], n =resolution, lims=lims)
#bivn.kde.5$z<-bivn.kde.5$z*5

surf$z<-surf$z+8*bivn.kde.4$z+16*bivn.kde.5$z


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
## axis values
#axis.vals<-list(x=sort(unique(surf$x)),y=sort(unique(surf$y)))
#image(axis.vals$x,axis.vals$y,matrix(z,resolution,resolution),col=heat.colors(100))
#contour(axis.vals$x,axis.vals$y,matrix(z,resolution,resolution),add=TRUE)

# write out truth to a file
write.csv(list(x=xx,y=yy,z=as.vector(surf$z),inside=ind),"wt2truth.csv",row.names=FALSE)

