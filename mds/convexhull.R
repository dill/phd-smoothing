# Code to create a convex hull around a collection of points in 2 dimensions.


# main routine to call for creating the convex hull
convex_hull<-function(points){
   # args:
   #  points      list(x=...,y=...) of points to find the hull of
   # return:
   #              list(x=...,y=...) vertices of the hull

   # variable to store the hull
   my.hull<-list(x=c(),y=c())

   # get rid of the obviously interior points
   
   # find the lowest point in both directions
   # this gives the index of the points
   # might need decreasing=TRUE
   order.index<-order(points$y,points$x)
   my.hull$x[1]<-points$x[order.index[1]]
   my.hull$y[1]<-points$y[order.index[1]]

   # first find all the angles and sort them
   my.angles<-angles(my.hull$x[1],my.hull$y[1],points$x[-1],points$y[-1])
   # second and third args below used to break ties
   my.angles.index<-order(my.angles,points$x[-1],points$y[-1])
   anglelist<-list(angles=c(my.angles[my.angles.index]),
                   x=c(points$x[-1][my.angles.index]),
                   y=c(points$y[-1][my.angles.index]))


   # then find the first point to go to
   my.hull$x[2]<-anglelist$x[1]
   my.hull$y[2]<-anglelist$y[1]

   # keep place in the hull
   j<-3

   # iterate over list
   for(i in 3:(length(anglelist$angles))){
      # add new point to the hull   
      my.hull$x[j]<-anglelist$x[i]
      my.hull$y[j]<-anglelist$y[i]

      #### test to see if adding this new element gets rid of
      #### older points
      # go back over all the old points
      # save the new point
      new_point<-pe(my.hull,j)
      for(k in length(my.hull$x):3){
         # if points 1 and j are on the same side of the line between
         # k and k-1...
         if(is_left(new_point,pe(my.hull,k-1),pe(my.hull,k)) < 0){
            # remove the point
            my.hull<-list(x=my.hull$x[-k],y=my.hull$y[-k])
            
            # make sure that j is still set to the right value
            # maintaining compactness of the list
            j<-j-1
         }
      }
      j<-j+1
   }

   # Add in the first point again
   my.hull<-list(x=c(my.hull$x,my.hull$x[1]),y=c(my.hull$y,my.hull$y[1]))


   # return polygon
   return(my.hull)

}


# routine to Pick Elements, pe
pe<-function(this.list,el){
   lapply(this.list,function(x) x[el])
}


# routine for finding the angles between a point (x0,y0) and a series of other
# points (x,y)
angles<-function(x0,y0,x,y){
   # args:
   #  x0,y0    point to measure from
   #  x,y      point to measure to
   # return:
   #           vector of angles

   # check that the lengths of x and y are the same
   if(length(x)!=length(y)){
      cat("x and y are not the same length!\n")
      return(FALSE)
   }

   return(atan2(y-y0,x-x0))



}

# routine for finding if we make a left turn 
is_left<-function(p1,p2,p3){
   # This is straight from CLRS p.949
   # args:
   #  p1    a point
   #  p2    another point
   #  p3    yet another point 
   # return:
   # >0     left turn 
   # 0      colinear
   # <0     right turn
   
   # calculate the criterion
   d<-(p2$x-p1$x)*(p3$y-p1$y)-(p2$y-p1$y)*(p3$x-p1$x)

   return(d)
}


# routine to find obviously interior points

