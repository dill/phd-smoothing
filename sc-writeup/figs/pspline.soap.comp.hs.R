# plot an example of the p-spline fit on the transformed
# domain for the Ramsay horseshoe.
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
predback.real<-read.csv("../../matlab/preal.csv",header=F)
predback.imag<-read.csv("../../matlab/pimag.csv",header=F)
prediction.grid<-data.frame(v=predback.real[[1]],w=predback.imag[[1]])


# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names


### create the knots for the p-spline
# they need to be wider than the data so that we can do prediction over the
# whole grid.
knots.sc<-list(v=seq(min(prediction.grid$v),max(prediction.grid$v)
               ,length.out=4),w=seq(min(prediction.grid$w),
               max(prediction.grid$w),length.out=8))

v.spacing<-abs(knots.sc$v[2]-knots.sc$v[1])
w.spacing<-abs(knots.sc$w[2]-knots.sc$w[1])
   
knots.sc$v<-unique(sort(c(seq(from=min(knots.sc$v),by=-v.spacing,length.out=4) ,
               knots.sc$v,seq(from=max(knots.sc$v),by=v.spacing,length.out=4))))
knots.sc$w<-unique(sort(c(seq(from=min(knots.sc$w),by=-w.spacing,length.out=4) ,
               knots.sc$w,seq(from=max(knots.sc$w),by=w.spacing,length.out=4))))

# load the original data set
orig.data<-read.csv("ramsey-1.csv",header=T)


# load the mapped data set
mapped.data<-read.csv("ramsey-mapped-1.csv",header=F)
# add in the y column
mapped.data<-cbind(orig.data[[1]],mapped.data)
# correct titles
names(mapped.data) <- c("y","v","w")


# fit with sc
# using the p-spline basis
# ie. m[1]
pspline.order<-2

b.mapped<-gam(y~te(v,w,bs="ps",m=pspline.order,k=c(6,10)),data=mapped.data,knots=knots.sc)

# get predictions
fv.mapped <- predict(b.mapped,prediction.grid)

# get rid of the points that are not in the grid
fv.mapped[!insiders]<-NA


pdf("compsmooth.pdf",6,3)
par(mfrow=c(1,2))


# sc mapping example
image(xm,yn,matrix(fv.mapped,m,n),col=heat.colors(100),xlab="w",ylab="v",asp=1,main="")
contour(xm,yn,matrix(fv.mapped,m,n),levels=seq(-5,5,by=.25),add=TRUE)
names(fsb[[1]])<-c("x","y")
lines(fsb[[1]],lwd=3)


# soap example
knots <- data.frame(v=rep(seq(-.5,3,by=.5),4),
                    w=rep(c(-.6,-.3,.3,.6),rep(8,4)))
names(fsb[[1]]) <- c("v","w") 
b <- gam(y~s(v,w,k=40,bs="so",xt=list(bnd=fsb)),knots=knots,data=orig.data)
fv <- predict(b,newdata=data.frame(v=xx,w=yy),block.size=-1)

image(xm,yn,matrix(fv,m,n),col=heat.colors(100),xlab="v",ylab="w",main="",asp=1)
contour(xm,yn,matrix(fv,m,n),levels=seq(-5,5,by=.25),add=TRUE)
names(fsb[[1]]) <- c("x","y")
lines(fsb[[1]],lwd=3)


dev.off()





