# Fit a gam to the irregular region once it's been mapped
# run mgcv
library(mgcv)
library(soap)

# load some data...
true.vals<-read.csv("wt2truth.csv",header=TRUE)
true.vals.mapped<-read.csv("wt2truemapped.csv",header=FALSE)
names(true.vals.mapped)<-c("x","y","z")

verts<-read.csv("figverts.csv",header=FALSE)
names(verts)<-c("x","y")

# sample from the matrix

# how many points to sample
samp.size<-500

# make a sample index
this.sample<-sample(c(1:dim(true.vals)[1]),samp.size)

# noise
ran<-rnorm(samp.size)*0.02

true.vals$z[true.vals$inside==0]<-NA

# take the points from the true and true mapped
samp.data<-data.frame(x=true.vals$x[this.sample],y=true.vals$y[this.sample],z=true.vals$z[this.sample]+ran)
samp.data.mapped<-data.frame(x=true.vals.mapped$x[this.sample],y=true.vals.mapped$y[this.sample],z=true.vals.mapped$z[this.sample]+ran)

res<-sqrt(length(true.vals$x))

pdf("wigglytop2-real.pdf",width=4,height=2.9)
par(mfrow=c(2,2),mar=c(1.8,1.5,1.8,1.5),las=1)


#surf[!inside.points]<-NA

### first truth
tru<-matrix(c(0),res,res)
tru[true.vals$inside==1]<-true.vals$z[true.vals$inside==1]
tru[true.vals$inside==0]<-NA
image(z=tru,x=seq(min(true.vals$x),max(true.vals$x),len=res),y=seq(min(true.vals$x),max(true.vals$x),len=res),col=heat.colors(100),xlab="",ylab="",main="truth",asp=1,cex.axis=0.5)
contour(z=tru,x=seq(min(true.vals$x),max(true.vals$x),len=res),y=seq(min(true.vals$x),max(true.vals$x),len=res),add=T,labcex=0.3,lwd=0.5)

### sc prediction w. tprs 
b.mapped<-gam(z~s(x)+s(y),data=samp.data.mapped)
# try to predict over the whole domain
fv <- predict(b.mapped,newdata=data.frame(x=true.vals.mapped$x,y=true.vals.mapped$y))
# do some faffing for the plots
pred.grid<-matrix(c(0),res,res)
pred.grid[true.vals$inside==1]<-fv
pred.grid[true.vals$inside==0]<-NA

image(pred.grid,x=seq(min(true.vals$x),max(true.vals$x),len=res),y=seq(min(true.vals$x),max(true.vals$x),len=res),col=heat.colors(100),xlab="",ylab="",main="sc+tprs",asp=1,,cex.axis=0.5)
contour(z=pred.grid,x=seq(min(true.vals$x),max(true.vals$x),len=res),y=seq(min(true.vals$x),max(true.vals$x),len=res),add=T,labcex=0.3,lwd=0.5)

### normal tprs
b.tprs<-gam(z~s(x)+s(y),data=samp.data)
fv.tprs <- predict(b.tprs,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))

pred.grid.tprs<-matrix(c(0),res,res)
pred.grid.tprs[true.vals$inside==1]<-fv.tprs
pred.grid.tprs[true.vals$inside==0]<-NA
image(pred.grid.tprs,x=seq(min(true.vals$x),max(true.vals$x),len=res),y=seq(min(true.vals$x),max(true.vals$x),len=res),col=heat.colors(100),xlab="x",ylab="y",main="tprs",asp=1,cex.axis=0.5)
contour(z=pred.grid.tprs,x=seq(min(true.vals$x),max(true.vals$x),len=res),y=seq(min(true.vals$x),max(true.vals$x),len=res),add=T,labcex=0.3,lwd=0.5)


### soap
# setup knots
# this is a faff
knots.x<-rep(seq(-2.9,2.9,length.out=8),8)
knots.y<-rep(seq(-2.9,3.6,length.out=8),rep(8,8))
insideknots<-inSide(verts,knots.x,knots.y)
# just get rid of one knot on the boundary, this is a bit horrible, look away now...
insideknots[59]<-FALSE
knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])
#plot(verts,type="l");points(knots,col="red");text(knots,labels=c(1:dim(knots)[1]))

# get only the inside points
inside.points<-inSide(verts,samp.data$x,samp.data$y)
samp.data<-data.frame(x=samp.data$x[inside.points],y=samp.data$y[inside.points],z=samp.data$z[inside.points])

# fit + predict 
b.soap<-gam(z~s(x,y,bs="so",xt=list(bnd=list(verts)),k=20),data=samp.data,knots=knots)
fv.soap <- predict(b.soap,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))

# plot
pred.grid.soap<-matrix(c(0),res,res)
pred.grid.soap[true.vals$inside==1]<-fv.soap
pred.grid.soap[true.vals$inside==0]<-NA
image(pred.grid.soap,x=seq(min(true.vals$x),max(true.vals$x),len=res),y=seq(min(true.vals$x),max(true.vals$x),len=res),col=heat.colors(100),xlab="x",ylab="y",main="soap",asp=1,cex.axis=0.5)
contour(z=pred.grid.soap,x=seq(min(true.vals$x),max(true.vals$x),len=res),y=seq(min(true.vals$x),max(true.vals$x),len=res),add=T,labcex=0.3,lwd=0.5)



### calculate the MSEs

#cat("sc+tprs",mean((true.vals$z[true.vals$inside==1]-fv)^2,na.rm=T),"\n")
#cat("tprs",mean((true.vals$z[true.vals$inside==1]-fv.tprs)^2,na.rm=T),"\n")
#cat("soap",mean((true.vals$z[true.vals$inside==1]-fv.soap)^2,na.rm=T),"\n")


dev.off()
