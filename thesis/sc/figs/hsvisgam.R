# plot the vis.gam in the transformed domain
library(soap)
fsb <- list(fs.boundary())

# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

## truth
tru <- fs.test(xx,yy) 

# read in the predicition grid from matlab
predback.real<-read.csv("../../../sc/matlab/preal.csv",header=F)
predback.imag<-read.csv("../../../sc/matlab/pimag.csv",header=F)
prediction.grid<-data.frame(v=predback.real[[1]],w=predback.imag[[1]])


# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names


# load the original data set
orig.data<-read.csv("../../../ramsay_sim_code/ramsaysim/ramsey-1.csv",header=T)

# load the mapped data set
mapped.data<-read.csv("../../../ramsay_sim_code/ramsaysim/ramsey-mapped-1.csv",header=F)
# add in the y column
mapped.data<-cbind(orig.data[[1]],mapped.data)
# correct titles
names(mapped.data) <- c("y","v","w")

# thin plate too
b.tp.mapped<-gam(y~s(v,w),data=mapped.data)

# plotting
pdf("hsvisgam.pdf",3,4)
par(mfrow=c(1,2))

# plot the true function...

### first take a line that runs along the centre of the horseshoe

rads<-seq(0.1,0.9,0.01)

fse<-list(x=c(),y=c())

for(rad in rads){
   ### leg x coordinates (these are the same for both)
   leg.x<-seq(0,max(fsb[[1]]$v),0.01)
   #reversed version
   r.leg.x<-leg.x[length(leg.x):1]
   # y coords
   leg.y.t<-rep(rad,length(leg.x))
   leg.y.b<- -leg.y.t
   
   # middle curve
   # create a circle first
   # by default r=0.5, use this.
   r<-rad
   t<-seq(0,2*pi,0.01)
   curve.x<-r*sin(t)
   curve.y<-r*cos(t)
   # discard the right half
   curve.y<-curve.y[curve.x<0]
   curve.x<-curve.x[curve.x<0]
   # reverse
   curve.y<-curve.y[length(curve.y):1]
   curve.x<-curve.x[length(curve.x):1]

   # container
   fse<-list(x=c(fse$x,r.leg.x,curve.x,leg.x),
             y=c(fse$y,leg.y.t,curve.y,leg.y.b))
}

#plot(fs.centreline)

im<-matrix(fs.test(fse$x,fse$y),length(c(r.leg.x,curve.x,leg.x)),length(rads))

image(z=t(im),x=rads,y=seq(0,3.25*2+pi/2,len=length(c(leg.y.t,curve.y,leg.y.b))),col=heat.colors(100),xlab="",ylab="",cex.axis=0.3,asp=1)

contour(z=t(im),x=rads,y=seq(0,3.25*2+pi/2,len=length(c(leg.y.t,curve.y,leg.y.b))),add=TRUE)





# plot the fit on the transformed domain
vis.gam(b.tp.mapped,main="",plot.type="contour",ylab="",xlab="",asp=1,cex.axis=0.3,contour.col="black")
dev.off()


