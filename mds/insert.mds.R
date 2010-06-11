"insert.mds"<-function(new.points,old.points,cmd.object,bnd,faster=0,debug=0){
   # insert a new point into the MDS, see Gower (Biometrika, 1968.)
   # args
   #  new.points     points to insert into the MDS
   #  old.points     old points
   #  cmd.object     object returned from cmdscale, with eig=TRUE
   #  bnd            boundary
   #  faster         use grid speedups?
   # ret
   # coordinates of new points in the mds?
   
   # want to calculate 1/2 * lambda^(-1) * X' * d

   # find lambda^(-1)
   lambda.inverse<-diag(1/cmd.object$eig)

   # take the original MDS coordinates
   # this is already double centred and dim(X)=nx2
   X<-cmd.object$points

   ### This code does "at once" insertion 
   # finally d
 
   # new set of distances from the old points to new
   # should take the form of a n by m matrix, where there are
   # and n original points to measure to and m new points.
 
   new.dist<-woodpath(c(old.points$x,new.points$x),c(old.points$y,new.points$y),
                      bnd,start=length(old.points$x),faster=faster,debug=debug)

   # the ith element of d is -(d_{i,n+1}^2 - diag(S))

   # cmd.object$x is the doubly centered symmetric distance matrix
   # from the original MDS configuration
   S<- -1/2*cmd.object$x
   #S<- -1/2*X%*%t(X)
   d<- -(new.dist^2-diag(S))
 
   # finally construct the product
   ret<-1/2*(lambda.inverse %*% t(X) %*% d)

   return(t(ret))
}
