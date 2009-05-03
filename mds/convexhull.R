# Code to create a convex hull around a collection of points in 2 dimensions.


# main routine to call for creating the convex hull
convex.hull<-fuction(points){
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
   anglelist<-list(angles=my.angles[my.angles.index],x=points$x[-1][my.angles.index],
                                          y=points$y[-1][my.angles.index])


   # then find the first point to go to
   my.hull$x[2]<-anglelist$x[1]
   my.hull$y[2]<-anglelist$y[1]

   # j gives the position of the last element in the hull
   j<-2
   # iterate over list
   for(i in 1:length(anglelist$angles)){
      # if left include
      if(angles(my.hull$x[j],my.hull$y[j],anglelist$x[i],anglelist$y[i]) < ? ){


      }else{
      # if right get rid of previous point, backtrack



      }








   }

   # return polygon


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



# routine to find obviously interior points

