# draw a pretty picture illustrating the progression from 
# quadrat sampling to strip sampling to distance sampling

# use soap for inSide
library(soap)

pdf(file="quadrat-to-ds.pdf",width=6,height=2)
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(2,2,2,2))


#set.seed(124)

# generate the data
n.dat<-50
x<-runif(n.dat)
y<-runif(n.dat)

plot.data<-function(){
   plot(x=c(0,0,1,1,0),y=c(0,1,1,0,0),xlab="",ylab="",axes=F,asp=1,type="n")
   points(x,y,pch=19)
#   axis(1,labels=F,lwd.ticks=0)
   lines(x=c(0,0,1,1,0),y=c(0,1,1,0,0))
}
# first column - quadrat sampling
plot.data()

# define a square
this.sq<-matrix(NA,5,2)
this.sq[,1]<-c(0,0,0.2,0.2,0)
this.sq[,2]<-c(0,0.2,0.2,0,0)

# offsets
xdel<-c(rep(0.2,3),rep(0.6,3))
ydel<-c(rep(seq(0.05,0.75,len=3),2))

# plot
for(i in 1:length(xdel)){
   lines(x=this.sq[,1]+xdel[i],y=this.sq[,2]+ydel[i],col="grey")
   inout<-inSide(list(x=this.sq[,1]+xdel[i],y=this.sq[,2]+ydel[i]),x,y)
   points(x[inout],y[inout],col="red",pch=19)
}


# second column - strip sampling
plot.data()

# offsets
xdel<-c(rep(0.15,3),rep(0.35,3),rep(0.55,3),rep(0.75,3))
ydel<-rep(seq(0.05,0.75,len=3),4)

# plot
this.sq[,1]<-this.sq[,1]*0.5
for(i in 1:length(xdel)){
   lines(x=this.sq[,1]+xdel[i],y=this.sq[,2]+ydel[i],col="grey")
   inout<-inSide(list(x=this.sq[,1]+xdel[i],y=this.sq[,2]+ydel[i]),x,y)
   points(x[inout],y[inout],col="red",pch=19)
}

# third column - distance sampling
plot.data()

this.line<-matrix(NA,2,2)
this.line[,1]<-c((1/3)/2,(1/3)/2)
this.line[,2]<-c(0,1)

for(i in 0:2){
   
   width<-0.1

   lines(this.line)
   lines(x=this.line[,1]-width,y=this.line[,2],lty=2,col="grey")
   lines(x=this.line[,1]+width,y=this.line[,2],lty=2,col="grey")

   this.box<-list(x=c(rep(this.line[1,1]-width,2),
                      rep(this.line[1,1]+width,2),
                      this.line[1,1]-width),
                  y=c(0,1,1,0,0))

   inout<-inSide(this.box,x,y)
   points(x[inout],y[inout],col="red",pch=19)

   ylines<-y[inout]
   xlines<-x[inout]

   for(j in 1:length(ylines)){
      lines(x=c(xlines[j],this.line[1,1]),y=rep(ylines[j],2))   
   }

   this.line[,1]<-this.line[,1]+1/3
}


dev.off()
