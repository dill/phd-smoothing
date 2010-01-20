# test to see if grids are necessary
# run from phd-smoothing/mds

source("mds.R")

# plot control
par(mfrow=c(2,2),cex=0.4,pch=19)


# points for the polygon, sideways T shape
pts.x<-c(0,0.5,0.5,0.3,0.3,0.2,0.2,0,0)
pts.y<-c(0.8,0.8,0.6,0.6,0,0,0.6,0.6,0.8)
bnd<-list(x=pts.x,y=pts.y)

# plot it
plot(bnd,type="l",xlab="",ylab="")

# grid across the whole shape
m<-25;n<-25
xm <- seq(0,0.5,length=m);yn<-seq(0,0.8,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
onoff<-onoff&inSide(list(x=-bnd$x,y=-bnd$y),-xx,-yy)
samp<-list(x=xx[onoff],y=yy[onoff])

points(samp)


# MDS the whole thing
D.full<-create_distance_matrix(samp$x,samp$y,bnd)
mds.full<-cmdscale(D.full,k=2,eig=TRUE,x.ret=TRUE)

plot(mds.full$points,xlab="",ylab="")



# now MDS the "tail"
D.tail<-create_distance_matrix(samp$x[samp$y>0.6],samp$y[samp$y>0.6],bnd)
mds.tail<-cmdscale(D.tail,k=2,eig=TRUE,x.ret=TRUE)

plot(mds.tail$points,xlim=c(-0.25,0.25),ylim=c(-0.55,0.1),asp=1,xlab="",ylab="")
# insert the remaining points
ins.tail<-insert.mds(list(x=samp$x[samp$y<=0.6],y=samp$y[samp$y<=0.6]),
                     list(x=samp$x[samp$y>0.6],y=samp$y[samp$y>0.6]),
                     mds.tail,bnd)
points(ins.tail,col="red")




# now MDS the "head"
D.head<-create_distance_matrix(samp$x[samp$y<=0.6],samp$y[samp$y<=0.6],bnd)
mds.head<-cmdscale(D.head,k=2,eig=TRUE,x.ret=TRUE)

plot(mds.head$points,ylim=c(-0.1,0.1),xlim=c(-0.3,0.55),asp=1,xlab="",ylab="")
# insert the remaining points
ins.head<-insert.mds(list(x=samp$x[samp$y>0.6],y=samp$y[samp$y>0.6]),
                     list(x=samp$x[samp$y<=0.6],y=samp$y[samp$y<=0.6]),
                     mds.head,bnd)
points(ins.head,col="red")

cat("press ctrl+c here if you want the first plot\n")
scan()


# now MDS the "tail" plus random sample, 4 times
for(i in 1:4){
   samp.ind<-sample(c(1:length(samp$x))[samp$x<0.2],5)
   s.x<-c(samp$x[samp$x>=0.2],samp$x[samp.ind])
   s.y<-c(samp$y[samp$x>=0.2],samp$y[samp.ind])
   D.tail<-create_distance_matrix(s.x,s.y,bnd)
   mds.tail<-cmdscale(D.tail,k=2,eig=TRUE,x.ret=TRUE)
   
   # insert the remaining points
   ns.x<-c(samp$x[samp$x<0.2])
   ns.y<-c(samp$y[samp$x<0.2])
   ins.tail<-insert.mds(list(x=ns.x,y=ns.y),
                        list(x=s.x,y=s.y),
                        mds.tail,bnd)
   # plot
   plot(mds.tail$points,xlim=c(-0.55,0.3),asp=1,xlab="",ylab="")
   points(ins.tail,col="red")
   points(mds.tail$points[(length(s.x)-4):length(s.x),],col="green")
}




