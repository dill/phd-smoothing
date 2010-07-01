# do a 2D example...

set.seed(1)

library(MASS)
source("mds.R")

# squash in 2D...
squash2<-function(dat,lims,sq){
   # squash the points in dat in the square lims[i,1:4],lims[i,5:8]
   # by a factor of sq[1] and sq[2] in x and y directions resp.

   # result
   res<-dat

   for(i in 1:length(lims[,1])){
      # make the boundary
      bnd<-list(x=lims[i,c(1:4,1)],y=lims[i,c(5:8,5)])
      ind<-inSide(bnd,dat$x,dat$y)
      res$x[ind]<-res$x[ind]/sq[i,1]
      res$y[ind]<-res$y[ind]/sq[i,2]
   }
   return(res)
}

# boundary, just a square
bigbnd<-list(x=c(-1,-1,1,1,-1),y=c(-1,1,1,-1,-1))

# four sub-squares, for squashing
box1<-c(-1,-1,0,0,1,0,0,1)
box2<-c(0,0,1,1,0,1,1,0)
box3<-c(-1,-1,0,0,0,-1,-1,0)
box4<-c(0,0,1,1,0,-1,-1,0)
lims<-rbind(box1,box2,box3,box4)

# squash factors, (x,y),(x,y),...
sq<-t(matrix(c(3,0.05,16,0.05,3,3,16,3),2,4))

dat<-make_soap_grid(bigbnd,50)
res<-squash2(dat,lims,sq)

par(mfrow=c(3,2),pch=".")

#plot(bigbnd,type="l",asp=1,xlab="x",ylab="y")
#points(dat)
#
#plot(res,asp=1,xlab="x*",ylab="y*")

# make the function on top
# MV Normal, centred at 0,0
#bivn<-mvrnorm(1000, mu = c(0,0), Sigma = matrix(c(0.2, 0, 0, 0.2), 2))

#bivn1<-mvrnorm(1000, mu = c(0.5,-0.5), Sigma = matrix(c(0.1, 0, 0, 0.1), 2))
#bivn2<-mvrnorm(1000, mu = c(0.5,0.5), Sigma = matrix(c(0.1, 0, 0, 0.1), 2))
#bivn3<-mvrnorm(1000, mu = c(-0.5,0.5), Sigma = matrix(c(0.1, 0, 0, 0.1), 2))
#bivn4<-mvrnorm(1000, mu = c(-0.5,-0.5), Sigma = matrix(c(0.1, 0, 0, 0.1), 2))
#biv<-rbind(bivn1,bivn2,bivn3,bivn4)


bivn1<-mvrnorm(1000, mu = c(0,-0.5), Sigma = matrix(c(0.2, 0, 0, 0.1), 2))
bivn2<-mvrnorm(1000, mu = c(0,0.5), Sigma = matrix(c(0.2, 0, 0, 0.1), 2))
biv<-rbind(bivn1,bivn2)

bivn<-kde2d(biv[,1],biv[,2], n=sqrt(length(dat$x)), lims=c(-1,1,-1,1))



dat<-data.frame(x=dat$x,y=dat$y,z=as.vector(bivn$z))
res<-data.frame(x=res$x,y=res$y,z=as.vector(bivn$z))

image(x=sort(unique(dat$x)),y=sort(unique(dat$y)),z=matrix(dat$z,length(unique(dat$y)),length(unique(dat$y))),asp=1,xlab="x",ylab="y")
contour(x=sort(unique(dat$x)),y=sort(unique(dat$y)),z=matrix(dat$z,length(unique(dat$y)),length(unique(dat$y))),add=TRUE,levels = pretty(c(min(dat$z),max(dat$z)),10),col="green")

image(x=sort(unique(res$x)),y=sort(unique(res$y)),z=matrix(res$z,length(unique(res$y)),length(unique(res$y))),xlab="x*",ylab="y*")
contour(x=sort(unique(res$x)),y=sort(unique(res$y)),z=matrix(res$z,length(unique(res$y)),length(unique(res$y))),add=TRUE,levels = pretty(c(min(dat$z),max(dat$z)),10),col="green")

# fit model to the right data
#b<-gam(z~s(x,y),data=dat)
#vis.gam(b,plot.type="contour",asp=1)

#res$z<-res$z+rnorm(length(res$z),0,0.05)
ind<-sample(1:length(dat$x),600)
dat<-data.frame(x=dat$x[ind],y=dat$y[ind],z=as.vector(bivn$z)[ind])
res<-data.frame(x=res$x[ind],y=res$y[ind],z=as.vector(bivn$z)[ind])

# fit to the squashed data
b1<-gam(z~s(x,y,k=200),data=res)
#vis.gam(b1,plot.type="contour",asp=1)


#sg<-make_soap_grid(bigbnd,c(50))
#D.grid<-create_distance_matrix(sg$x,sg$y,bigbnd,faster=0)
#grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)
source("intexp/smooth2s.c.R")
b3<-gam(z~s(x,y,k=200,bs="mdstps",xt=list(bnd=bigbnd,
#                                   op=sg,
#                                   b.grid=c(20,20),
#                                   mds.obj=grid.mds,
                                   lims=lims,
                                   sq=sq
         )),data=res)

#source("intexp/smooth2.c.R")
#b4<-gam(z~s(x,y,k=100,bs="mdstp",xt=list(bnd=bigbnd,
#                                   op=sg,
#                                   b.grid=c(20,20),
#                                   mds.obj=grid.mds
#         )),data=res)
#vis.gam(b3,plot.type="contour",asp=1)


pdat<-make_soap_grid(bigbnd,50,mat=TRUE,delta=TRUE)
pres<-squash2(list(x=pdat$x,y=pdat$y),lims,sq)

xx<-seq(min(bigbnd$x),max(bigbnd$x),pdat$deltax)
yy<-seq(min(bigbnd$y),max(bigbnd$y),pdat$deltay)


# tprs no adjust
pred1<-predict(b1,pres)
#m<-pdat$mat
#m[!is.na(m)]<-pred1
m<-matrix(pred1,length(unique(pres$x)),length(unique(pres$y)))
# transformed space
image(x=sort(unique(pres$x)),y=sort(unique(pres$y)),z=m,xlab="x*",ylab="y*",asp=1)
contour(x=sort(unique(pres$x)),y=sort(unique(pres$y)),z=m,add=TRUE,levels = pretty(c(min(dat$z),max(dat$z)),10),col="green")
# untransformed space
image(x=xx,y=yy,z=m,xlab="x*",ylab="y*",asp=1)
contour(x=sort(unique(pdat$x)),y=sort(unique(pdat$y)),z=m,add=TRUE,levels = pretty(c(min(dat$z),max(dat$z)),10),col="green")



pred3<-predict(b3,pres)
#m<-pdat$mat
#m[!is.na(m)]<-pred3
m<-matrix(pred3,length(unique(pres$x)),length(unique(pres$y)))
# transformed space
image(x=sort(unique(pres$x)),y=sort(unique(pres$y)),z=m,xlab="x*",ylab="y*",asp=1)
contour(x=sort(unique(pres$x)),y=sort(unique(pres$y)),z=m,add=TRUE,levels = pretty(c(min(dat$z),max(dat$z)),10),col="green")
# untransformed space
image(x=xx,y=yy,z=m,xlab="x*",ylab="y*",asp=1)
contour(x=sort(unique(pdat$x)),y=sort(unique(pdat$y)),z=m,add=TRUE,levels = pretty(c(min(dat$z),max(dat$z)),10),col="green")



#pred4<-predict(b4,pres)
#m<-pdat$mat
#m[!is.na(m)]<-pred4
#image(x=xx,y=yy,z=m,xlab="x*",ylab="y*",asp=1)

