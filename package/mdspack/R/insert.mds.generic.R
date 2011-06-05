# generic MDS insertion routine
insert.mds.generic<-function(mds.obj,new.points,old.points,dist.metric="euclidean"){

   big.points<-rbind(old.points,new.points)
   ind<-1:nrow(old.points)
   ind2<-(nrow(old.points)+1):nrow(big.points)

   lambda.inverse<-diag(1/mds.obj$eig[1:dim(mds.obj$points)[2]])

   if(dist.metric=="mahalanobis"){
      new.dist.start<-mahalanobis(big.points, big.points[1,])
      new.dist<-apply(big.points,1,mahalanobis,x=big.points,
                      cov=attr(new.dist.start,"cov.inv"))[ind,]

#      new.dist<-apply(big.points,1,mahalanobis,x=big.points,
#                      cov=cov(big.points))[ind,]
   }else{ 
      new.dist<-as.matrix(dist(big.points,method=dist.metric,diag=T,upper=T))[ind,]
   }

   new.dist<-new.dist[,ind2]
   S<- -1/2*mds.obj$x
   d<- -(new.dist^2-diag(S))
   mds.points<-t(1/2*(lambda.inverse %*% t(mds.obj$points) %*% d))

   return(mds.points)
}
