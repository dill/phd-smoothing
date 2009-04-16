
library(mgcv)
library(soap)
# load some data...
#true.vals<-read.csv("fig9truth.csv",header=TRUE)
# load the vertices data
verts<-read.csv("figverts.csv",header=FALSE)
names(verts)<-c("x","y")




# Generate bivariate normal distribution
# load MASS
require(MASS)

# things that will eventually be args
lims<-c(-3,3,-2,4)
resolution<-100 # number of points in the kernel density estimate

# generate first MV normal
bivn.1 <- mvrnorm(500, mu = c(-2, 0), Sigma = matrix(c(2, 0, 0, 2), 2))
bivn.kde.1 <- kde2d(bivn.1[,1], bivn.1[,2], n =resolution, lims=lims)
# and the second...
bivn.2 <- mvrnorm(500, mu = c(2, -1), Sigma = matrix(c(2, 0, 0, 2), 2))
bivn.kde.2 <- kde2d(bivn.2[,1], bivn.2[,2], n =resolution, lims=lims)

# add them...
surf<-list(x=c(),y=c(),z=c())
surf$x<-bivn.kde.1$x
surf$y<-bivn.kde.1$y
surf$z<-bivn.kde.1$z+bivn.kde.2$z

# make a grid
xn<-length(surf$x); yn<-length(surf$y)
xx<-rep(surf$x,yn); yy<-rep(surf$y,rep(xn,yn))

# find the inside points, make it 0/1 rather than T/F
inside.points<-inSide(verts,xx,yy)
ind<-rep(0,length(inside.points))
ind[inside.points]<-1

# put it all together
true.vals<-data.frame(x=xx,y=yy,z=as.vector(surf$z),inside=ind)



# how many points to sample
samp.size<-100

# make a sample index
this.sample<-sample(c(1:dim(true.vals)[1]),samp.size)

# noise
ran<-rnorm(samp.size)*0.02

samp.data<-data.frame(x=true.vals$x[this.sample],y=true.vals$y[this.sample],z=true.vals$z[this.sample]+ran)

# setup knots
# this is a faff
knots.x<-rep(seq(-3,3,length.out=10),10)
knots.y<-rep(seq(-2,3.5,length.out=10),rep(10,10))

insideknots<-inSide(verts,knots.x,knots.y)

knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])

# get only the inside points
inside.points<-inSide(verts,samp.data$x,samp.data$y)
samp.data<-data.frame(x=samp.data$x[inside.points],y=samp.data$y[inside.points],z=samp.data$z[inside.points])

# fit
b.soap<-gam(z~s(x,y,bs="so",xt=list(bnd=list(verts)),k=25),data=samp.data,knots=knots)

# plot

fv.soap <- predict(b.soap,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))

pred.grid.soap<-matrix(c(0),res,res)
pred.grid.soap[true.vals$inside==1]<-fv.soap
pred.grid.soap[true.vals$inside==0]<-NA
image(axis.vals$x,axis.vals$y,pred.grid.soap,col=heat.colors(100),xlab="x",ylab="y",main="soap prediction",asp=1)
contour(axis.vals$x,axis.vals$y,pred.grid.soap,add=T)
lines(verts,lwd=2)




