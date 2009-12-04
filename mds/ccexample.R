
par(mfrow=c(2,2))

# my mds code
mymds<-function(D){

   n<-dim(D)[1]

   # squaring
   D<-D^2

   # double centre D
   ones<-t(t(rep(1,n)))
   H<-diag(1,n,n)-1/n*(ones%*%t(ones))

   S<- -1/2*H%*%D%*%H

   # eigen decompose
   eS<-eigen(S,symmetric=TRUE)
   U<-eS$vectors
   lambda<-eS$values[1:2]

   X<-U[,1:2]%*%diag(sqrt(eS$values[1:2]),2)

   return(list(points=X,eig=lambda))
}

# dots
d.pts<-list(x=c(0,0.5,0,-0.5),y=c(0.5,0,-0.5,0))
# crosses
c.pts<-list(x=c(0.5,0.5,-0.5,-0.5),y=c(0.5,-0.5,-0.5,0.5))

#d.pts$x<-d.pts$x+6
#d.pts$y<-d.pts$y+3
#c.pts$x<-c.pts$x+6
#c.pts$y<-c.pts$y+3

meth="manhattan"
meth="euclidean"

# plot original config
plot(d.pts,pch=19,asp=1,main=paste("using the",meth,"metric"))
points(c.pts,pch=4)



D.d<-as.matrix(dist(matrix(c(d.pts$x,d.pts$y),4,2),diag=TRUE,upper=TRUE,method=meth))
#D.c<-as.matrix(dist(matrix(c(c.pts$x,c.pts$y),4,2),diag=TRUE,upper=TRUE))

# MDS on the dots
d.mds<-mymds(D.d)
#d.mds<-cmdscale(D.d,eig=TRUE)

plot(d.mds$points,pch=19,asp=1,main="MDS dots, insert crosses",xlim=c(-1,1),ylim=c(-1,1))

# add in crosses
D.full<-as.matrix(dist(matrix(c(d.pts$x,c.pts$x,d.pts$y,c.pts$y),8,2),diag=TRUE,upper=TRUE,method=meth))
D.dc<-diag(d.mds$points%*%t(d.mds$points))-D.full[5:8,1:4]^2
c.ins<-t(1/2*diag(1/d.mds$eig)%*%t(d.mds$points)%*%(D.dc))

points(c.ins,pch=4)

# MDS both

full.mds<-mymds(D.full)
plot(full.mds$points,pch=19,asp=1,main="MDS dots and crosses",
      xlim=c(-1,1),ylim=c(-1,1))

# sample 

plot(full.mds$points,pch=19,asp=1,main="full (red) with re-insert (black)",
      xlim=c(-2,2),ylim=c(-1,1),col="red")

# sample index
sind1<-sample(1:4,2)
sind2<-sample(1:4,2)
samp<-matrix(c(d.pts$x[sind1],c.pts$x[sind2],
         d.pts$y[sind1],c.pts$y[sind2]),4,2)
D.samp<-as.matrix(dist(samp,diag=TRUE,upper=TRUE,method=meth))

D.samp<-diag(d.mds$points%*%t(d.mds$points))-D.samp^2
samp.ins<-t(1/2*diag(1/d.mds$eig)%*%t(d.mds$points)%*%(D.samp))
points(samp.ins,pch=as.character(1:4))

D.full<-diag(d.mds$points%*%t(d.mds$points))-D.full[1:4,]^2
full.ins<-t(1/2*diag(1/d.mds$eig)%*%t(d.mds$points)%*%(D.full))
points(full.ins,pch=6)

points(samp,pch=as.character(1:4),col="light blue")

# not sample points
nsamp<-matrix(c(d.pts$x[-sind1],c.pts$x[-sind2],
         d.pts$y[-sind1],c.pts$y[-sind2]),4,2)
D.nsamp<-as.matrix(dist(nsamp,diag=TRUE,upper=TRUE,method=meth))

D.nsamp<-diag(d.mds$points%*%t(d.mds$points))-D.nsamp^2
nsamp.ins<-t(1/2*diag(1/d.mds$eig)%*%t(d.mds$points)%*%(D.nsamp))
points(nsamp.ins,pch=as.character(5:8))




