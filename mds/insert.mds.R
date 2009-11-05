"insert.mds"<-function(new.points,old.points,cmd.object,bnd){
   # insert a new point into the MDS, see Gower (Biometrika, 1968.)
   # args
   #  new.points     points to insert into the MDS
   #  old.points     old MDS points
   #  cmd.object     object returned from cmdscale, with eig=TRUE
   #  bnd            boundary
   # ret
   # coordinates of new points in the mds?
   
   # want to calculate 1/2 * lambda^(-1) * X' * d

   # find lambda^(-1)
   lambda.inverse<-matrix(0,length(cmd.object$eig),length(cmd.object$eig))
   diag(lambda.inverse)<-1/cmd.object$eig

   # take the original MDS coordinates
   # this is already double centred and dim(X)=nx2
   X<-cmd.object$points
  

   ### This code does "at once" insertion 
   # finally d
 
   # new set of distances from the old points to new
   # should take the form of a n by m matrix, where there are
   # and n original points to measure to and m new points.
 
   new.dist<-woodpath(c(old.points$x,new.points$x),c(old.points$y,new.points$y),
                      bnd,start=length(old.points$x))
 
   # the ith element of d is d_i^2-d_{i,n+1}^2
   d<-diag(X%*%t(X))-new.dist^2
   #d <- rowSums(X*X) - new.dist^2 # efficient version of above
   # finally construct the product
   ret<-1/2*((lambda.inverse %*% t(X)) %*% d)
 

#### This code does online insertion
# for this to work, need to modify wood.c too
##   ### finally d
##   # find the distances from old->new and new->new
##   # dim(new.dist)= length(old+new) x length(new)
##   new.dist<-woodpath(c(old.points$x,new.points$x),c(old.points$y,new.points$y),
##                      bnd,start=length(old.points$x))
##
##
##   for(i in (length(old.points$x)+1):length(c(old.points$x,new.points$x)) ){
##      # the ith element of d is d_i^2-d_{i,n+1}^2
##      d <- rowSums(X*X) - new.dist[1:(i-1),i-length(old.points$x)]^2 
##
##      # finally construct the product
##      new.point<-1/2*((lambda.inverse %*% t(X)) %*% d)
##
##      # append new point to X
##      X<-rbind(X,t(new.point))
##   }
##
##   # just get the new bits
##   ret<-X[length(old.points$x):length(c(old.points$x,new.points$x)),]

   return(t(ret))
}
