# Simon's code taken straight from soap-package.


## create a boundary...
fsb <- list(fs.boundary())
 
## create some internal knots...
knots <- data.frame(v=rep(seq(-.5,3,by=.5),4),
                    w=rep(c(-.6,-.3,.3,.6),rep(8,4)))

## Simulate some fitting data, inside boundary...
n<-1000
v <- runif(n)*5-1;w<-runif(n)*2-1
y <- fs.test(v,w,b=1)
ind <- inSide(fsb,x=v,y=w) ## remove outsiders
y <- y[ind];v <- v[ind]; w <- w[ind] 
n <- length(y)
y <- y + rnorm(n)*.3 ## add noise


# First generate the polygon boundary in matlab format
horse<-list(x=c(),y=c())
horse$x[1]<-max(fsb[[1]]$v)
horse$y[1]<-max(fsb[[1]]$w)
horse$x[2]<-min(fsb[[1]]$v)
horse$y[2]<-max(fsb[[1]]$w)
horse$x[3]<-min(fsb[[1]]$v)
horse$y[3]<-min(fsb[[1]]$w)
horse$x[4]<-max(fsb[[1]]$v)
horse$y[4]<-min(fsb[[1]]$w)
horse$x[5]<-max(fsb[[1]]$v)
horse$y[5]<-max(fsb[[1]]$w[fsb[[1]]$w<0 & fsb[[1]]$v>0.5])
horse$x[6]<-max(fsb[[1]]$v[fsb[[1]]$v<0])
horse$y[6]<-max(fsb[[1]]$w[fsb[[1]]$w<0 & fsb[[1]]$v>0.5])
horse$x[7]<-max(fsb[[1]]$v[fsb[[1]]$v<0])
horse$y[7]<-min(fsb[[1]]$w[fsb[[1]]$w>0 & fsb[[1]]$v>0.5])
horse$x[8]<-max(fsb[[1]]$v)
horse$y[8]<-min(fsb[[1]]$w[fsb[[1]]$w>0 & fsb[[1]]$v>0.5])

# put it into matlab format
horseshoe<-c()
for (i in 1:length(horse$x)){
  horseshoe[i]<-paste(horse$x[i]," ",horse$y[i],"; ",sep="")
}
cat("horseshoedata=[",horseshoe,"]\n",sep="")





### from the simulated data:
# put the points we want to map into matlab format
# v and w give the points and y the response
horseshoe.data<-c()
for (i in 1:length(w)){
  horseshoe.data[i]<-paste(v[i]," ",w[i],"; ",sep="")
}
cat("poly=[",horseshoe.data,"]\n",sep="")

# get the points we want to map back from matlab
mapped.matlab.points<-read.csv("phd-smoothing/mappedpoints.csv",header=F)
mapped.frame<-list(y=y,v=mapped.matlab.points[,1],w=mapped.matlab.points[,2]) 


# do a normal gam() fit.
b <- gam(y~s(v,w,bs="tp"),data=mapped.frame)


### predict over a grid
m<-100;n<-50 
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

# put the grid back into matlab
predict.data<-c()
for (i in 1:length(xx)){
  predict.data[i]<-paste(xx[i]," ",yy[i],"; ",sep="")
}
cat("predicted=[",predict.data,"]\n",sep="")

### funky matlab stuff happens

# load data
predback.real<-read.csv("phd-smoothing/matlab/real.csv",header=F)
predback.imag<-read.csv("phd-smoothing/matlab/imag.csv",header=F)


#do the prediction
fv <- predict(b,newdata=data.frame(v=predback.real[[1]],w=predback.imag[[1]])

insiders<-inSide(fsb,x=xx,y=yy)
fv.insiders<-fv
fv.insiders[!insiders]<-NA

pdf("myattempt.pdf",8,4)
par(mfrow=c(1,2))
image(xm,yn,matrix(fv,m,n),col=heat.colors(100),xlab="v",ylab="w")
contour(xm,yn,matrix(fv,m,n),levels=seq(-5,5,by=.25),add=TRUE)
image(xm,yn,matrix(fv.insiders,m,n),col=heat.colors(100),xlab="v",ylab="w")
contour(xm,yn,matrix(fv.insiders,m,n),levels=seq(-5,5,by=.25),add=TRUE)
dev.off()



# can probably ignore this...

par(mfrow=c(3,2))
## plot boundary with knot and data locations
plot(fsb[[1]],type="l");points(knots,pch=20,col=2)
points(v,w,pch=".");

names(fsb[[1]]) <- c("v","w") ## correct boundary names

## Now fit the soap film smoother...
b <- gam(y~s(v,w,k=40,bs="so",xt=list(bnd=fsb)),knots=knots)
plot(b) ## default plot
vis.gam(b,plot.type="contour") ## nicer visualization

## Plot the true function...
m<-100;n<-50 
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

tru <- matrix(fs.test(xx,yy),m,n) ## truth
image(xm,yn,tru,col=heat.colors(100),xlab="v",ylab="w",
      main="truth")
lines(fsb[[1]],lwd=3)
contour(xm,yn,tru,levels=seq(-5,5,by=.25),add=TRUE)

## Do a known boundary example (note no `k' needed)
## First define the value for the smooth at each 
## supplied boundary point...
fsb[[1]]$f <- fs.test(fsb[[1]]$v,fsb[[1]]$w,b=1,exclude=FALSE)

## Now fit the smooth...
bk <- gam(y~s(v,w,bs="so",xt=list(bnd=fsb)),knots=knots)
plot(bk) ## default plot

## Produce a prettier plot of the fit, by predicting on a fine grid...
fv <- predict(bk,newdata=data.frame(v=xx,w=yy),block.size=-1)
## ... `block.size=-1' stops `predict.gam' from splitting `newdata'
## in to several chuncks for predicting: this would involve expensive
## duplication of PDE solving.

## The actual plot...
image(xm,yn,matrix(fv,m,n),col=heat.colors(100),xlab="v",ylab="w",
      main="known boundary soap film")
contour(xm,yn,matrix(fv,m,n),levels=seq(-5,5,by=.25),add=TRUE)
