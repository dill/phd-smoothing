# simulate something that looks like microarray data... 

library(mvtnorm)
library(mgcv)
library(mdspack)

#qdist<-function(x,method,diag,upper,root=T,...){
#   x.sq<-x^2
#   x.cs<-rowSums(x.sq)
#   #X.cs<-matrix(rep(x.cs,nrow(x)),nrow(x),nrow(x))
#
#   # x_i*x_j (in a matrix sense)
#   x.cr<-x%*%t(x)
#   
#   #res<-X.cs+t(X.cs)-2*x.cr
#
#   res<- -2*x.cr
#
#   res<-t(t(res+x.cs)+x.cs)
#
#   diag(res)<-0
#
#   if(root)
#      res<-sqrt(res)
#
#   return(res)
#}
#
#dist<-qdist

# some global options
n.sims<-1#0#100
n.samp<-100
mvn.mu<-c(0,0)
mvn.sigma<-matrix(c(0.5,0,0,0.5),2,2)

# prediction points
grid.seq<-seq(-1,1,len=15)
pred.points<-as.matrix(expand.grid(grid.seq,grid.seq,grid.seq))
pred.z<-dmvnorm(pred.points[,1:2],
                  mvn.mu,mvn.sigma)
pred.nonmds<-data.frame(x1=pred.points[,1],x2=pred.points[,2])

bigletters<-as.vector(sapply(letters,paste,letters,sep=""))
bl.len<-length(bigletters)

mses<-c()
edfs<-c()

for(i in 1:n.sims){

   # Generate the dot xs from runif(-1,1)
   xdots1<-runif(n.samp,-1,1)
   xdots2<-runif(n.samp,-1,1)
   x1<-xdots1
   x2<-xdots2
   
   # Evaluate response z from MVN using x1 and x2 z=f(x1,x2)
   z<-dmvnorm(cbind(x1,x2),mvn.mu,mvn.sigma)
   z<-z+rnorm(n.samp,0,0.05) # add error to the z
   
   # add error to the xdots
   xdots1<-xdots1#+cbind(rep(0,n.samp),0.7*runif(n.samp,-1,1))
   xdots2<-xdots2
   # extra spurious columns
#   noise.terms<-cbind(10*runif(n.samp,-1,1))
   noise.terms<-cbind(10*rnorm(n.samp))
   #noise.terms<-cbind(noise.terms,10*runif(n.samp,-1,1))
   
   # Take the dot xs, put them in a matrix
   #dotmat<-cbind(xdots1,xdots2,rnorm(n.samp)) # add in a column of noise
   dotmat<-cbind(xdots1,xdots2,noise.terms)#,runif(n.samp,-10,10))
   # calculate a distance matrix from them
   D2<-dist(dotmat)
   
   ### Fit some models
   # using the "true" parameters
   b<-gam(z~s(x1,x2,k=100),dat=data.frame(z=z,x1=x1,x2=x2))
   # Find the GCV optimal MDS projection -- smoothing over z using mdspack
   b.mds<-gam.mds.fit(z,D2,NULL,k=100) # select the MDS dimension
   #b.mds<-gam.mds.fit(z,D2,2,k=100) # just use 2d projection

   ### Prediction
   fv.nonmds<-predict(b,pred.nonmds,type="response")
   # MDS pred points
   pred.mds<-insert.mds.generic(b.mds$mds.obj,pred.points,dotmat)
   pred.mds<-as.data.frame(pred.mds)
   attr(pred.mds,"names")<-bigletters[(length(bigletters)-
                               (dim(b.mds$samp.mds)[2]-2)):length(bigletters)]
   fv.mds<-predict(b.mds$gam,pred.mds,type="response")

   #### Calculate MSEs
   #mses<-rbind(mses,c(sum((pred.z-fv.nonmds)^2),sum((pred.z-fv.mds)^2)))
   
   ### Plot all that
   par(mfrow=c(1,2))
   #vis.gam(b,plot.type="contour",asp=1)
   #points(x1,x2,pch=19,cex=0.7)
   #vis.gam(b.mds$gam,plot.type="contour",asp=1)
   #points(b.mds$samp.mds[,2:3],pch=19,cex=0.7)
   
   image(grid.seq,grid.seq,matrix(fv.nonmds,length(grid.seq),length(grid.seq)),
         asp=1,main="nonmds")
   points(x1,x2,pch=19,cex=0.7)
   image(grid.seq,grid.seq,matrix(fv.mds,length(grid.seq),length(grid.seq)),
         asp=1,main="mds")
   points(x1,x2,pch=19,cex=0.7)

   # record the EDFs
   edfs<-rbind(edfs,sum(b.mds$gam$edf))

} 
   

