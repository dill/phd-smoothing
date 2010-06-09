# compare the heatmaps of soap/mapped/true for alternative Ramsay figure

# set seed
set.seed(1)

library(soap)
fsb <- list(fs.boundary())

# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

# load the true function
source("ramsay.alt.R")

## truth
tru <- ramsay.alt(xx,yy) 

# read in the predicition grid from matlab
predback.real<-read.csv("../../../sc/matlab/preal.csv",header=F)
predback.imag<-read.csv("../../../sc/matlab/pimag.csv",header=F)
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


# knots for the soap film
knots <- data.frame(v=rep(seq(-.5,3,by=.5),4),
                      w=rep(c(-.6,-.3,.3,.6),rep(8,4)))

# dia with 
#i<-1
#i<-150
# load the original data set
#orig.data<-read.csv(paste("../../altramsaysim/ramsey-",i,".csv",sep=""),header=T)
orig.data<-read.csv("ramsey-alt-1.csv",header=T)

# create the sample index
sind<-sample(1:length(orig.data$v),250)

orig.data<-list(v=orig.data$v[sind],w=orig.data$w[sind],y=orig.data$y[sind])

# load the mapped data set
#mapped.data<-read.csv(paste("../../altramsaysim/ramsey-mapped-",i,".csv",sep=""),header=F)
mapped.data<-read.csv("ramsey-mapped-alt-1.csv",header=FALSE)
# add in the y column
#mapped.data<-cbind(orig.data[[1]],mapped.data)
mapped.data<-list(v=mapped.data[sind,1],w=mapped.data[sind,2],y=orig.data$y)

### sc code
# fit with sc
# using the p-spline basis

# ie. m[1]
pspline.order<-2
b.mapped<-gam(y~te(v,w,bs="ps",m=pspline.order,k=c(6,10)),data=mapped.data,knots=knots.sc)
# get predictions
fv.mapped <- predict(b.mapped,prediction.grid)
# get rid of the points that are not in the grid
fv.mapped[!insiders]<-NA

# for the thin plate
b.tp.mapped<-gam(y~s(v,w),data=mapped.data)
# get predictions
fv.tp.mapped <- predict(b.tp.mapped,prediction.grid)
# get rid of the points that are not in the grid
fv.tp.mapped[!insiders]<-NA

# fit with soap
b.soap <- gam(y~s(v,w,k=40,bs="so",xt=list(bnd=fsb)),data=orig.data,knots=knots)
# get predictions
fv.soap <- predict(b.soap,newdata=data.frame(v=xx,w=yy),block.size=-1)

#### end of setup


### actually do the plotting
pdf("altramsaycomp.pdf",width=4,height=2.9)
par(mfrow=c(2,2),mar=c(1.8,1.5,1.8,1.5))

names(fsb[[1]]) <- c("x","y")

image(xm,yn,matrix(tru,m,n),col=heat.colors(100),xlab="",ylab="",
      asp=1,main="truth",las=1,cex.axis=0.5)
contour(xm,yn,matrix(tru,m,n),levels=seq(0,0.4,by=.1),add=TRUE,labcex=0.3,lwd=0.5)
lines(fsb[[1]],lwd=2)

# sc+ps mapping example
image(xm,yn,matrix(fv.mapped,m,n),col=heat.colors(100),xlab="",ylab="",
      cex.axis=0.5,asp=1,main="sc+ps",las=1)
contour(xm,yn,matrix(fv.mapped,m,n),levels=seq(-0,0.4,by=.1),add=TRUE,labcex=0.3,lwd=0.5)
lines(fsb[[1]],lwd=2)

# sc+tp
image(xm,yn,matrix(fv.tp.mapped,m,n),col=heat.colors(100),xlab="",ylab="",
      cex.axis=0.5,asp=1,main="sc+tp",las=1)
contour(xm,yn,matrix(fv.tp.mapped,m,n),levels=seq(0,0.4,by=.1),add=TRUE,labcex=0.3,lwd=0.5)
lines(fsb[[1]],lwd=2)

# soap
image(xm,yn,matrix(fv.soap,m,n),col=heat.colors(100),xlab="",ylab="",
      cex.axis=0.5,main="soap",asp=1,las=1)
contour(xm,yn,matrix(fv.soap,m,n),levels=seq(0,0.4,by=.1),add=TRUE,labcex=0.3,lwd=0.5)
lines(fsb[[1]],lwd=2)




dev.off()


