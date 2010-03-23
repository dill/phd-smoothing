# integration experiment (thin plate version)
library(mgcv)

x<-seq(0,1,len=30)
y<-x^2


xp<-seq(0,1,len=120) # xvaluesforprediction 

b<-gam(y~s(x,k=10))

par(mfrow=c(2,2))
plot(b,main="no squash")


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
         x.tmp<-x.tmp-(lims[i+1]+lims[i])/2
         x.tmp<-x.tmp/sq[i]
         x.tmp<-x.tmp+(lims[i+1]+lims[i])/2
   
         x.ret<-c(x.ret,x.tmp)
      }
   }
   return(x.ret)
}

lims<-c(0,0.5,0.7,1)
sq<-c(1,1/0.1,1/2)

lims<-c(0,0.5,1)
sq<-c(1,1/0.5)

# do the squashing
x.m<-squash(x,lims,sq)
dat<-data.frame(x=x.m,y=y)


b<-gam(y~s(x,k=10),data=dat)

plot(b,main="squash fit")
plot(x.m,y,main="raw squash data")

##### fixing...
source("smooth.c.R")

b<-gam(y~s(x,k=10,xt=list(lims=lims,sq=sq),bs="mdstp"),data=dat)

plot(b,main="fixed fit")

