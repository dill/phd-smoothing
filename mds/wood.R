# Simon's algorithm for finding the path

wood_path<-function(p1,p1,bnd){
   # args:
   #  p1, p2      the two points to find the path between
   #  bnd         the boundary that got in the way
   # return:
   #  path        set of points on the path


   # make a line between p1 and p2

   # find the first intersection between p1, p2 and the boundary
   # for each point

   # do the bounding box check first, for speed
   bbindex<-c(1:(length(bnd$x)-1))[do_intersect(p1,p2,bnd)]
   # hold distances and intersection points temporarily
   dists<-c(); ips<-list(x=c(),y=c())

   for(i in bbindex){
      # calculate and save the intersection
      ip<-intersection_point(p1,p2,
                           list(x=bnd$x[c(i,i+1)],y=bnd$x[c(i,i+1)]))
      ips$x<-c(ips$x,ip$x)      
      ips$y<-c(ips$y,ip$y)      

      # find the distance and save
      dists<-c(dists,sqrt((ip$x-bnd$x[i])^2+(ip$y-bnd$y[i])^2))
   }


   # temporary cars to work out which direction to go around
   # the boundary
   tmp.sp<-bbindex[order(dists)[1]]
   tmp.ep<-bbindex[order(dists,decreasing=TRUE)[1]]
   tmp.bnd.1<-list(x=ips$x[order(dists)[1]],
                     bnd$x[c(tmp.sp:(tmp.ep-1))],
                  ips$x[order(dist,decreasing=TRUE)[1]]


   # create the initial path:
   # p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
   my.path<-list(x=c(p1$x,
                     ,p2$x),
                 y=c()
                )

   # ***
   my.path<-delete_step(my.path,bnd)

   # iterate over the points in the path:
      # alter the path, until on two consecutive runs there are
      # no changes to the path

         # for each point i, look at the line i-1 to i+1

            # does it go inside-outside-inside?
               # create a path:
               # i-1, i-1 1st intersection with bnd, some of bnd, 
               #    i+1 1st intersection with bnd, i+1
               # run from *** 


            # if it goes inside-outside or outside-inside, ignore




   # return the path




}

delete_step<-function(path, bnd){

   
   # iterate over the points in the path:
      # delete as many points as possible, making sure that the
      # path is still outside

   for(i in 2:(length(path$x)-1)){

      





   }

}








