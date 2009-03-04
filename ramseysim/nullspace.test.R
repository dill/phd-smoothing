
library(soap)


# need to create the prediction grid here!!!


fsb <- list(fs.boundary())


# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

## truth
tru <- fs.test(xx,yy) 

# read in the predicition grid from matlab
predback.real<-read.csv("./matlab/preal.csv",header=F)
predback.imag<-read.csv("./matlab/pimag.csv",header=F)
prediction.grid<-data.frame(v=predback.real[[1]],w=predback.imag[[1]])


# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names


# first take a line that runs along the centre of the horseshoe
# can do thi using the transform in fs.test


# leg x coordinates (these are the same for both)
leg.x<-seq(0,max(fsb[[1]]$v),0.01)
#reversed version
r.leg.x<-leg.x[length(leg.x):1]

# y coords
leg.y.t<-rep(0.5,length(leg.x))
leg.y.b<- -leg.y.t

# middle curve
# by default r=0.5, use this.
r<-0.5
curve.x<-seq(0,0.5,0.01)
curve.x<-sqrt(r^2-curve.x^2)

curve.y<-seq(0,0.5,length.out=length(curve.x))
curve.y<-sqrt(r^2-curve.y^2)
#curve.y<-curve.y[length(curve.y):1]

# we've done a quarter, do the other quarter
curve.x<--c(curve.x[length(curve.x):1],curve.x)
curve.y<-c(curve.y,-curve.y[length(curve.y):1])

# container
fs.centreline<-list(x=c(r.leg.x,curve.x,leg.x),y=c(leg.y.t,curve.y,leg.y.b))

# test
fsb <- fs.boundary()
m<-300;n<-150 
xm <- seq(-1,4,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
tru <- matrix(fs.test(xx,yy),m,n) ## truth
image(xm,yn,tru,col=heat.colors(100),xlab="x",ylab="y")
points(fs.centreline,pch=".")


# let evaluate these points using the fs.test function



#

fs.centre.eval<-fs.test(fs.centreline$x,fs.centreline$y)





# this line is equiv. to...
# export to matlab
write.csv(fs.centreline,"centreline.csv",row.names=FALSE)


# take those points, map them

# evaluate the pointsusing fs.test


# linear fit of x,y against f.





#linear.fit<-lm(tru~prediction.grid$v*prediction.grid$w)
linear.fit<-lm(tru~prediction.grid$v+prediction.grid$w)



#pred<-predict(linear.fit,prediction.grid)
#pred[!insiders]<-NA


# predict with w set and vary v?
# then the other way around?








#put the predicted linear points z values onto the horseshoe




image(matrix(pred,m,n),col=heat.colors(100))
contour(matrix(pred,m,n),nlevels=50,add=TRUE)






