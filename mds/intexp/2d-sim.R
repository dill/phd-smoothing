# do a 2D example...

# results
# [1] 0.7816314 0.7621230 0.7768502
#means= 2.354819 2.300262 2.304969 


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
#sq<-t(matrix(c(3,0.5,16,0.5,3,3,16,3),2,4))
#sq<-t(matrix(c(0.9,5,1,5,0.9,1,1,1),2,4))
sq<-t(matrix(c(0.3,5,1,5,0.3,1,1,1),2,4))


dat1<-make_soap_grid(bigbnd,50)
res1<-squash2(dat1,lims,sq)

bivn1<-mvrnorm(100, mu = c(0,-0.5), Sigma = matrix(c(0.2, 0, 0, 0.1), 2))
bivn2<-mvrnorm(100, mu = c(0,0.5), Sigma = matrix(c(0.2, 0, 0, 0.1), 2))
biv<-rbind(bivn1,bivn2)

bivn<-kde2d(biv[,1],biv[,2], n=sqrt(length(dat1$x)), lims=c(-1,1,-1,1))

sg<-make_soap_grid(bigbnd,c(50))
D.grid<-create_distance_matrix(sg$x,sg$y,bigbnd,faster=0)
grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)

mses<-matrix(NA,50,3)

for(i in 1:50){

   dat<-data.frame(x=dat1$x,y=dat1$y,z=as.vector(bivn$z))
   res<-data.frame(x=res1$x,y=res1$y,z=as.vector(bivn$z))
   
   
   res$z<-res$z+rnorm(length(res$z),0,0.05)
   ind<-sample(1:length(dat$x),300)
   dat<-data.frame(x=dat$x[ind],y=dat$y[ind],z=as.vector(bivn$z)[ind])
   res<-data.frame(x=res$x[ind],y=res$y[ind],z=as.vector(bivn$z)[ind])
   
   # set k
   k<-100
   
   # fit to the squashed data
   b1<-gam(z~s(x,y,k=k),data=res)
   
   source("intexp/smooth2s.c.R")
   b3<-gam(z~s(x,y,k=k,bs="mdstps",xt=list(bnd=bigbnd,
                                      lims=lims,
                                      sq=sq
            )),data=res)
   
   source("intexp/smooth2.c.R")
   b4<-gam(z~s(x,y,k=k,bs="mdstp",xt=list(bnd=bigbnd,
                                      op=sg,
                                      b.grid=c(20,20),
                                      mds.obj=grid.mds
            )),data=res)
   
   
   pdat<-make_soap_grid(bigbnd,50,mat=TRUE,delta=TRUE)
   pres<-squash2(list(x=pdat$x,y=pdat$y),lims,sq)
   
   xx<-seq(min(bigbnd$x),max(bigbnd$x),pdat$deltax)
   yy<-seq(min(bigbnd$y),max(bigbnd$y),pdat$deltay)
   
   
   # tprs no adjust
   pred1<-predict(b1,pres)
   #m<-pdat$mat
   #m[!is.na(m)]<-pred1
   m<-matrix(pred1,length(unique(pres$x)),length(unique(pres$y)))
   # untransformed space
   mses[i,1]<-sum((pred1-as.vector(bivn$z))^2)
   
   
   
   pred3<-predict(b3,pres)
   #m<-pdat$mat
   #m[!is.na(m)]<-pred3
   m<-matrix(pred3,length(unique(pres$x)),length(unique(pres$y)))
   # untransformed space
   mses[i,2]<-sum((pred3-as.vector(bivn$z))^2)
   
   
   pred4<-predict(b4,pres)
   #m<-pdat$mat
   #m[!is.na(m)]<-pred3
   m<-matrix(pred4,length(unique(pres$x)),length(unique(pres$y)))
   # untransformed space
   mses[i,3]<-sum((pred4-as.vector(bivn$z))^2)
   
}

cat("means=",colMeans(mses),"\n")
   
