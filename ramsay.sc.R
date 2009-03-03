# Simon's code taken straight from soap-package.

# load soap
library(soap)

## create a boundary...
fsb <- list(fs.boundary())
names(fsb[[1]]) <- c("v","w") ## correct boundary names
 
## Simulate some fitting data, inside boundary...
n<-1000
v <- runif(n)*5-1;w<-runif(n)*2-1
y <- fs.test(v,w,b=1)
ind <- inSide(fsb,x=v,y=w) ## remove outsiders
y <- y[ind];v <- v[ind]; w <- w[ind] 
n <- length(y)
y <- y + rnorm(n)*.3 ## add noise


# First generate the polygon boundary in matlab format
# this is a really rough boundary that covers a much great area than
# the actual horseshoe.

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
cat("poly=[",horseshoe,"]\n",sep="")

##########################
### put this into matlab!
##########################

### from the simulated data:
# put the points we want to map into matlab format
# v and w give the points and y the response
horseshoe.data<-c()
for (i in 1:length(w)){
  horseshoe.data[i]<-paste(v[i]," ",w[i],"; ",sep="")
}
cat("horseshoedata=[",horseshoe.data,"]\n",sep="")

#########################
### more matlab here
#########################


# get the points we want to map back from matlab
mapped.real<-read.csv("matlab/real.csv",header=F)
mapped.imag<-read.csv("matlab/imag.csv",header=F)


#do the prediction
mapped.frame<-list(y=y,v=mapped.real[[1]],w=mapped.imag[[1]]) 


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
sink(file="matlab/predictpoints.m")
cat("predicted=[",predict.data,"]\n",sep="")
sink(file=NULL)

### funky matlab stuff happens

# load data
predback.real<-read.csv("matlab/preal.csv",header=F)
predback.imag<-read.csv("matlab/pimag.csv",header=F)

#do the prediction
fv <- predict(b,newdata=data.frame(v=predback.real[[1]],w=predback.imag[[1]]))

# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names

fv.insiders<-fv
fv.insiders[!insiders]<-NA

#pdf("myattempt.pdf",8,4)
par(mfrow=c(1,2))
image(xm,yn,matrix(fv,m,n),col=heat.colors(100),xlab="v",ylab="w")
contour(xm,yn,matrix(fv,m,n),levels=seq(-5,5,by=.25),add=TRUE)
image(xm,yn,matrix(fv.insiders,m,n),col=heat.colors(100),xlab="v",ylab="w")
contour(xm,yn,matrix(fv.insiders,m,n),levels=seq(-5,5,by=.25),add=TRUE)
#dev.off()



