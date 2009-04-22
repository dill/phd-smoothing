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
samp.size<-1000

# make a sample index
this.sample<-sample(c(1:dim(true.vals)[1]),samp.size)

# noise
ran<-rnorm(samp.size)*0.02

true.vals$z[true.vals$inside==0]<-NA

# take the points from the true and true mapped
samp.data<-data.frame(x=true.vals$x[this.sample],y=true.vals$y[this.sample],z=true.vals$z[this.sample]+ran)
samp.data.mapped<-data.frame(x=true.vals.mapped$x[this.sample],y=true.vals.mapped$y[this.sample],z=true.vals.mapped$z[this.sample]+ran)

res<-sqrt(length(true.vals$x))
par(mfrow=c(2,2))

#surf[!inside.points]<-NA

### first truth
tru<-matrix(c(0),res,res)
tru[true.vals$inside==1]<-true.vals$z[true.vals$inside==1]
tru[true.vals$inside==0]<-NA
image(tru,col=heat.colors(100),xlab="x",ylab="y",main="truth",asp=1)
contour(tru,add=T)

### sc prediction w. tprs 
b.mapped<-gam(z~s(x)+s(y),data=samp.data.mapped)
# try to predict over the whole domain
fv <- predict(b.mapped,newdata=data.frame(x=true.vals.mapped$x,y=true.vals.mapped$y))
# do some faffing for the plots
pred.grid<-matrix(c(0),res,res)
pred.grid[true.vals$inside==1]<-fv
pred.grid[true.vals$inside==0]<-NA

image(pred.grid,col=heat.colors(100),xlab="x",ylab="y",main="sc+tprs prediction",asp=1)
contour(pred.grid,add=T)

### normal tprs
b.tprs<-gam(z~s(x)+s(y),data=samp.data)
fv.tprs <- predict(b.tprs,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))

pred.grid.tprs<-matrix(c(0),res,res)
pred.grid.tprs[true.vals$inside==1]<-fv.tprs
pred.grid.tprs[true.vals$inside==0]<-NA
image(pred.grid.tprs,col=heat.colors(100),xlab="x",ylab="y",main="tprs prediction",asp=1)
contour(pred.grid.tprs,add=T)


### soap
# setup knots
# this is a faff
knots.x<-rep(seq(-2.9,2.9,length.out=10),10)
knots.y<-rep(seq(-2.9,3.6,length.out=10),rep(10,10))
insideknots<-inSide(verts,knots.x,knots.y)
# just get rid of one knot on the boundary, this is a bit horrible, look away now...
insideknots[59]<-FALSE
knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])
#plot(verts,type="l");points(knots,col="red");text(knots,labels=c(1:dim(knots)[1]))

# get only the inside points
inside.points<-inSide(verts,samp.data$x,samp.data$y)
samp.data<-data.frame(x=samp.data$x[inside.points],y=samp.data$y[inside.points],z=samp.data$z[inside.points])

# fit + predict 
b.soap<-gam(z~s(x,y,bs="so",xt=list(bnd=list(verts)),k=49),data=samp.data,knots=knots)
fv.soap <- predict(b.soap,newdata=data.frame(x=true.vals$x[true.vals$inside==1],y=true.vals$y[true.vals$inside==1]))

# plot
pred.grid.soap<-matrix(c(0),res,res)
pred.grid.soap[true.vals$inside==1]<-fv.soap
pred.grid.soap[true.vals$inside==0]<-NA
image(pred.grid.soap,col=heat.colors(100),xlab="x",ylab="y",main="soap prediction",asp=1)
contour(pred.grid.soap,add=T)



### calculate the MSEs

cat("sc+tprs",mean((true.vals$z[true.vals$inside==1]-fv)^2,na.rm=T),"\n")
cat("tprs",mean((true.vals$z[true.vals$inside==1]-fv.tprs)^2,na.rm=T),"\n")
cat("soap",mean((true.vals$z[true.vals$inside==1]-fv.soap)^2,na.rm=T),"\n")

