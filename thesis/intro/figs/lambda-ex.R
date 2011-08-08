# example of varying lambda

library(mgcv)

set.seed(12)

# generate some data
dat <- gamSim(1,n=100,dist="normal",scale=2)

dat$y<-dat$y-(dat$f1+dat$f0+dat$f3)

true<-data.frame(x=sort(dat$x2),y=dat$f2[order(dat$x2)]-5)


pdf(file="lambda-ex.pdf",height=3,width=6.5)

par(mfrow=c(1,3),las=1,mgp=c(2,1,0))

# optimal
b<-gam(y~s(x2,k=100),data=dat)
plot(b,se=FALSE,rug=FALSE,residuals=TRUE,ylab=expression(hat(f)(x)),xlab="x",ylim=c(-9,12))
lines(true$x,true$y,type="l",col="blue")


# lambda=0
b.0<-gam(y~s(x2,k=100),data=dat,sp=0)
plot(b.0,se=FALSE,rug=FALSE,residuals=TRUE,ylab=expression(hat(f)(x)),xlab="x",ylim=c(-9,12))
lines(true$x,true$y,type="l",col="blue")


# lambda=infinity
b.inf<-gam(y~s(x2,k=100),data=dat,sp=1e10)
plot(b.inf,se=FALSE,rug=FALSE,residuals=TRUE,ylab=expression(hat(f)(x)),xlab="x",ylim=c(-9,12))
lines(true$x,true$y,type="l",col="blue")


dev.off()
