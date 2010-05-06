# see what models are fit when they have the same EDF

#set.seed(1)

# import some libraries and do the setup as in tpintexp.R


library(mgcv)
# code to do the squashing
source("squash.R")
# code for the thin plate adjustment
source("smooth.c.R")

n<-100
x<-seq(0,1,len=n)
y <- 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10*x)^3 * (1 - x)^10

k<-80
method<-"GCV.Cp"
#method<-"REML"
basis<-"mdstp"

noise<-rep(0,length(x))
noise<-rnorm(length(x))*0.4

# plot vals for noise?
# 0.05,


dat<-data.frame(x=x,y=y+noise)

###### now do some squashing...
lims<-c(0,0.4,0.6,0.8,1)
sq<-c(1/0.09,1,1/20,1)
# do the squashing
x.m<-squash(x,lims,sq)
dat.m<-data.frame(x=x.m,y=y+noise)

# x values for prediction 
newdat<-data.frame(x=seq(0,1,len=500))
newdat.m<-data.frame(x=squash(seq(0,1,len=500),lims,sq))

##### fixing...
alim<-min(x)
blim<-max(x)
N<-1000
xs<-alim+(1:N -0.5)*(blim-alim)/N
dens<-c(rep(sq[1],sum(xs<=lims[2])),
        rep(sq[2],sum(xs>lims[2] & xs<=lims[3])),
        rep(sq[3],sum(xs>lims[3] & xs<=lims[4])),
        rep(sq[4],sum(xs>lims[4] & xs<=lims[5])))


# now time to fit some models

par(mfrow=c(3,2))
#par(mfrow=c(1,2))



# set sp

# 1) EDF=71 (ish) 2) EDF= 19 3) EDF=42
sp.fix<-c(2.3e-06, 0.4, 0.0013) 
sp.s<-c(0.000000001,0.00024,0.00000025)
edf<-c(71,19,42)


for (i in 1:3){
   b.fix<-gam(y~s(x,k=k,bs=basis,xt=list(dens=dens)),data=dat.m,method=method,sp=sp.fix[i])
   
   b.s<-gam(y~s(x,k=k),data=dat.m,method=method,sp=sp.s[i])
   
   cat("unfixed EDF=",sum(b.s$edf),"\n")
   cat("fixed EDF=",sum(b.fix$edf),"\n")
   cat("correlation=",cor(predict(b.s,newdat.m),predict(b.fix,newdat.m)),"\n")
   
   # plot in squashed space
   plot(x=x.m,y=y+noise,main="adjusted fit (green), data (black),\n unadjusted fit (blue)",type="n",xlim=c(0,max(x.m)),xlab="x",ylab="y")
   lines(x=newdat.m$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
   lines(x=newdat.m$x,y=predict(b.fix,newdat.m),col="green",lwd=2)
   points(x.m,y+noise,pch=19,cex=0.3)
   legend(1,6,paste("unfixed EDF =",round(sum(b.s$edf),1),
                    "\nfixed EDF     =",round(sum(b.fix$edf),1)),bty="n")


   # unsquashed space
   plot(x=x,y=y,main="adjusted fit (green), truth (red), \nunadjusted fit (blue)",type="n",xlim=c(0,1),xlab="x",ylab="y")
   lines(x=x,y=y,lwd=2,col="red")
   lines(x=newdat$x,y=predict(b.s,newdat.m),col="blue",lwd=2)
   lines(x=newdat$x,y=predict(b.fix,newdat.m),col="green",lwd=2)
   points(x=x,y=y+noise,pch=19,cex=0.3)
}


# check that \beta^TS\beta is correct...
pr<-seq(0,max(x.m),len=1000)
eps<-1e-6

fd<-(predict(b.fix,list(x=pr+2*eps))-
     2*predict(b.fix,list(x=pr+eps))+
     predict(b.fix,list(x=pr)))/eps^2

sbs1<-sum(fd^2)

beta<-b.fix$coefficients[2:length(b.fix$coefficients)]
S<-b.fix$smooth[[1]]$S[[1]]

sbs2<-t(beta)%*%S%*%beta

cat("diff in penalty term =",abs(sbs1-sbs2),"\n")


