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
   my.angles.index<-order(my.angles,points$y[-1],points$x[-1])
   anglelist<-list(angles=my.angles[my.angles.index],my.angles[1],
                   x=c(points$x[-1][my.angles.index],points$x[1]),
                   y=c(points$y[-1][my.angles.index],points$y[1]))


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
      # save the previous point
      prev_point<-pe(my.hull,j)
      for(k in length(my.hull$x):3){
         # if points 1 and i are on the same side of the line between
         # k and k-1...
         if(same_side(pe(my.hull,1),prev_point,pe(my.hull,c(k,k-1))) < 0){
            # remove the point
            my.hull<-list(x=my.hull$x[-k],y=my.hull$y[-k])
            
            # make sure that j is still set to the right value
            # maintaining compactness of the list
            j<-j-1
         }
      }
      j<-j+1
   }

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
      return()
   }

   return(atan2(x-x0,y-y0))



}

# routine for finding if two points are on the same side of a line
same_side<-function(p1,p2,l){
   # This is straight from Sedgewick's "Algorithms", p. 313
   # args:
   #  p1    a point
   #  p2    another point
   #  l     a line (2 points)
   # return:
   #  -1    different sides
   #  0     on line
   #  1     same side
   
   # calculate some quantities
   dx<-l$x[2]-l$x[1]
   dy<-l$y[2]-l$y[1]
   dx1<-p1$x-l$x[1]
   dy1<-p1$y-l$y[1]
   dx2<-p2$x-l$x[2]
   dy2<-p2$y-l$y[1]

   # calculate the criterion
   d<-(dx*dy1-dy*dx1)*(dx*dy2-dy*dy2)

   # return the result
   if(d>0){
      return(1)
   # probably want to do something here...
   }else if(d>-1e-16 & d<1e-16){
      return(0)
   }else if(d<0){
      return(-1)
   }


}


# routine to find obviously interior points

