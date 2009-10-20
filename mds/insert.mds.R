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
   # this is already double centred
   X<-cmd.object$points
   
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

   #ret<-1/2*(t(d)%*% X %*% solve(t(X)%*%X))

   # return the transpose of the result
   # ie. mx2 rather than 2xm matrix
   return(t(ret))
}
