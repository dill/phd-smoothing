# integration experiment (thin plate version)
library(mgcv)
library(ks)

x<-seq(0,1,len=30)
y<-x^2

dat<-data.frame(x=x,y=y)

# x values for prediction 
newdat<-data.frame(x=seq(0,1,len=120)) 


b<-gam(y~s(x,k=10),data=dat)

par(mfrow=c(2,2))
plot(x=newdat$x,y=predict(b,newdat),main="no squash",type="l",asp=1,xlim=c(0,1))


# now crazy things happen
# move around the values of x and xk
xk<-seq(1/8,7/8,len=8) #choose some knots 
xp<-seq(0,1,len=100) # xvaluesforprediction 

# function to do the squashing
squash<-function(x,lims,sq){
   # squash the points in x between lims[1] and lims[2] by a factor of sq

   x.ret<-c() # return vector

   x.tmp<-x[(x>=lims[1]) & (x<=lims[2])]
   x.tmp<-x.tmp-(lims[1]+lims[2])/2
   x.tmp<-x.tmp/sq[1]
   x.tmp<-x.tmp+(lims[1]+lims[2])/2
   
   x.ret<-c(x.ret,x.tmp)
   
   if(length(sq)>=2){
      for(i in 2:(length(sq))){
         x.tmp<-x[(x>lims[i]) & (x<=lims[i+1])]
         x.tmp<-(x.tmp-lims[i])/sq[i] +lims[i]

         x.ret<-c(x.ret,x.tmp)
      }
   }
   return(x.ret)
}

lims<-c(0,0.5,0.7,1)
sq<-c(1,1/0.1,1/2)

lims<-c(0,0.5,1)
sq<-c(1,1/2)

# do the squashing
x.m<-squash(x,lims,sq)
dat.m<-data.frame(x=x.m,y=y)


b<-gam(y~s(x,k=10),data=dat.m)

plot(x=newdat$x,y=predict(b,newdat),main="squash fit",type="l",asp=1,xlim=c(0,1))
plot(x.m,y,main="raw squash data",pch=19,cex=0.3,asp=1,xlim=c(0,1))

##### fixing...
source("smooth.c.R")

#library(sm)
#dens<-sm.density(x.m,display="none")
#sq<-dens$estimate[dens$eval.points>0 & dens$eval.points<1]
#lims<-seq(0,1,by=diff(dens$eval.points)[1])

#dens.est<-kde(x.m,h=hpi(x.m),eval.points=seq(0,1,len=10))
#sq<-dens.est$estimate
#lims<-dens.est$eval.points

#lims<-squash(lims,lims,sq)

#dat.m$x<-squash(dat.m$x,lims,1/sq)

#b.fix<-gam(y~s(x,k=10,xt=list(lims=lims,sq=sq),bs="mdstp"),data=dat.m)
b.fix<-gam(y~s(x,k=10,bs="mdstp"),data=dat.m)

#newdat$x<-squash(newdat$x,lims,sq)

plot(x=newdat$x,y=predict(b.fix,newdat),main="fixed fit",type="l",asp=1,xlim=c(0,1))
points(x.m,y)

