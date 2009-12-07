source("eig.align.R")
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

full.pts<-matrix(c(d.pts$x,c.pts$x,d.pts$y,c.pts$y),8,2)

#d.pts$x<-d.pts$x+6
#d.pts$y<-d.pts$y+3
#c.pts$x<-c.pts$x+6
#c.pts$y<-c.pts$y+3

meth="manhattan"
#meth="euclidean"

# plot original config
plot(d.pts,pch=19,asp=1,main=paste("using the",meth,"metric"))
points(c.pts,pch=4)



D.d<-as.matrix(dist(matrix(c(d.pts$x,d.pts$y),4,2),diag=TRUE,upper=TRUE,method=meth))
#D.c<-as.matrix(dist(matrix(c(c.pts$x,c.pts$y),4,2),diag=TRUE,upper=TRUE))

# MDS on the dots
#d.mds<-mymds(D.d)
d.mds<-cmdscale(D.d,eig=TRUE)

plot(d.mds$points,pch=19,asp=1,main="MDS dots, insert crosses",xlim=c(-1,1),ylim=c(-1,1))

# add in crosses
D.full<-as.matrix(dist(matrix(c(d.pts$x,c.pts$x,d.pts$y,c.pts$y),8,2),diag=TRUE,upper=TRUE,method=meth))
D.dc<-diag(d.mds$points%*%t(d.mds$points))-D.full[5:8,1:4]^2
c.ins<-t(1/2*diag(1/d.mds$eig)%*%t(d.mds$points)%*%(D.dc))

points(c.ins,pch=4)

# MDS both

#full.mds<-mymds(D.full)
full.mds<-cmdscale(D.full,eig=TRUE)
plot(full.mds$points,pch=19,asp=1,main="MDS dots and crosses",
      xlim=c(-1,1),ylim=c(-1,1))

# sample 

plot(full.mds$points,pch=as.character(1:8),asp=1,main="full (red) with re-insert (black)",
      xlim=c(-1,1),ylim=c(-1,1),col="red")

# sample index
sind<-sample(1:8,4)
#sind<-c(1,2,3,4)
#sind<-c(5,6,7,8)
#sind<-c(5,7,3,8)

D.samp<-D.full[1:4,sind]

x.diag<-diag(d.mds$points%*%t(d.mds$points))
x.diag<-matrix(rep(x.diag,dim(D.samp)[2]),dim(D.samp)[1],dim(D.samp)[2])

D.samp<-x.diag-D.samp^2
samp.ins<-1/2*t(diag(1/d.mds$eig)%*%t(d.mds$points)%*%(D.samp))
points(samp.ins,pch=as.character(sind))

#D.full<-diag(d.mds$points%*%t(d.mds$points))-D.full[1:4,]^2
#full.ins<-t(1/2*diag(1/d.mds$eig)%*%t(d.mds$points)%*%(D.full))
#points(full.ins,pch=6)

#points(samp,pch=as.character(1:4),col="light blue")

# not sample points
D.nsamp<-D.full[1:4,-sind]

D.nsamp<-x.diag-D.nsamp^2
nsamp.ins<-1/2*t(diag(1/d.mds$eig)%*%t(d.mds$points)%*%(D.nsamp))
points(nsamp.ins,pch=as.character(c(1:8)[-sind]))




