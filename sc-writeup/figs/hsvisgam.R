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

# fit with sc using the p-spline basis
# ie. m[1]
pspline.order<-2
b.mapped<-gam(y~te(v,w,bs="ps",m=pspline.order,k=c(6,10)),data=mapped.data,knots=knots.sc)

# thin plate too
b.tp.mapped<-gam(y~s(v,w),data=mapped.data)

pdf("hsvisgam.pdf",3,4)

par(mfrow=c(1,2))

# plot the fit on the transformed domain
vis.gam(b.mapped,main="",plot.type="contour",ylab="",xlab="",asp=1,cex.axis=0.3)
vis.gam(b.tp.mapped,main="",plot.type="contour",ylab="",xlab="",asp=1,cex.axis=0.3)
dev.off()


