# simulate something that looks like microarray data... 

library(mvtnorm)
library(mgcv)
library(mdspack)

# some global options
n.sims<-2#100
n.samp<-100
mvn.mu<-c(0,0)
mvn.sigma<-matrix(c(0.5,0,0,0.5),2,2)

# Set the parameters for the alphas
alpha1<-c(0.3,0.7)
alpha2<-c(0.2,0.8)

# prediction points
grid.seq<-seq(-1,1,len=15)
pred.points<-as.matrix(expand.grid(grid.seq,grid.seq,grid.seq))
pred.z<-dmvnorm(cbind(pred.points[,1:2]%*%alpha1,pred.points[,3]),
                  mvn.mu,mvn.sigma)
pred.nonmds<-data.frame(x1=pred.points[,1:2]%*%alpha1,x2=pred.points[,3])

bigletters<-as.vector(sapply(letters,paste,letters,sep=""))
bl.len<-length(bigletters)

mses<-c()

for(i in 1:n.sims){

   # Generate the dot xs from runif(-1,1)
   xdots1<-cbind(runif(n.samp,-1,1),runif(n.samp,-1,1))
   xdots2<-cbind(runif(n.samp,-1,1),runif(n.samp,-1,1))
   
   # Combine as x1 and x2 using the formulae
   x1<-xdots1%*%alpha1
   x2<-xdots2[,1]#%*%alpha2
   
   # Evaluate response z from MVN using x1 and x2 z=f(x1,x2)
   z<-dmvnorm(cbind(x1,x2),mvn.mu,mvn.sigma)
   
   # save the xdots
   #xdots1.save<-xdots1
   #xdots2.save<-xdots2
   
   # add error to the xdots
   xdots1<-xdots1#+cbind(rep(0,n.samp),0.7*runif(n.samp,-1,1))
   xdots2<-xdots2
   
   # can then check the SNR between the xdots
   # and xdots with error added
   # cor(xdots1,xdots1.save)^2
   # cor(xdots2,xdots2.save)^2
   
   # Take the dot xs, put them in a matrix and calculate a distance matrix from them
   #dotmat<-cbind(xdots1,xdots2,rnorm(n.samp)) # add in a column of noise
   dotmat<-cbind(xdots1,xdots2[,1])
   D2<-dist(dotmat)
   
   
   #par(mfrow=c(1,2))
   
   # using the "true" parameters
   b<-gam(z~s(x1,x2,k=100),dat=data.frame(z=z,x1=x1,x2=x2))
   ## plot that
   #vis.gam(b,plot.type="contour",asp=1)
   #points(x1,x2,pch=19,cex=0.7)

   fv.nonmds<-predict(b,pred.nonmds,type="response")

   mses<-rbind(mses,c(sum((pred.z-fv.nonmds)^2),"nonmds")) 
   
   # Find the GCV optimal MDS projection while smoothing over z using mdspack
   #b.mds<-gam.mds.fit(z,D2,NULL,k=100) # select the MDS dimension
   b.mds<-gam.mds.fit(z,D2,2,k=100)
   ## plot that
   #vis.gam(b.mds$gam,plot.type="contour",asp=1)
   #points(b.mds$samp.mds[,2:3],pch=19,cex=0.7)
   
   # MDS pred points
   pred.mds<-insert.mds.generic(b.mds$mds.obj,pred.points,dotmat)
   pred.mds<-as.data.frame(pred.mds)
   attr(pred.mds,"names")<-bigletters[(length(bigletters)-
                                      (dim(b.mds$samp.mds)[2]-2)):length(bigletters)]
   fv.mds<-predict(b.mds$gam,pred.mds,type="response")
   
   
   mses<-rbind(mses,c(sum((pred.z-fv.mds)^2),"mds")) 
   
   #
   #Do some kind of cross validation
   
} 
   
