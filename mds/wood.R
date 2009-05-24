# Simon's algorithm for finding the path

wood_path<-function(p1,p2,bnd){
   # args:
   #  p1, p2      the two points to find the path between
   #  bnd         the boundary that got in the way
   # return:
   #  path        set of points on the path


# DEBUG
plot(bnd,type="l",asp=1)
points(p1,pch=24,col="pink")
points(p2,pch=24,col="pink")

   # make a line between p1 and p2

   # find the first intersection between p1, p2 and the boundary
   # for each point

   # do the bounding box check first, for speed
   bbindex<-c(1:(length(bnd$x)-1))[do_intersect(p1,p2,bnd)]
   # hold distances and intersection points temporarily
   dists<-c(); ips<-list(x=c(),y=c())

# DEBUG
cat("bbindex=",bbindex,"\n")



   for(i in bbindex){
      # calculate and save the intersection
      ip<-intersection_point(p1,p2,pe(bnd,c(i,i+1)))
      ips$x<-c(ips$x,ip$x)      
      ips$y<-c(ips$y,ip$y)      

      # find the distance and save
      dists<-c(dists,sqrt((p1$x-ip$x)^2+(p1$y-ip$y)^2))
   }
   # the two intersection points
   ip1<-pe(ips,order(dists)[1])
   ip2<-pe(ips,order(dists,decreasing=TRUE)[1])

# DEBUG
points(ip1,pch=23,col="red")
text(ip1,labels=c("ip1"))
points(ip2,pch=23,col="red")
text(ip2,labels=c("ip2"))

   # sort the intersections by their distances from p1 and p2
   ip1.index<-bbindex[order(dists)] 
   ip2.index<-bbindex[order(dists,decreasing=TRUE)] 

   # This is quite horrible code.
   # What we do is: take the ordering that makes sense first
   # eg 1:5 not 5:1, make that set of edges, then take the difference
   # between that set and the complete set of vertices.
   picker<-sort(c(ip1.index[1],(ip1.index[length(ip1.index)]+1)))
   picker<-c(picker[1]:picker[2])
   bnd.1.sort<-pe(bnd,picker)

   bnd.2.sort<-pe(pe(bnd,c(1:(length(bnd$x)))),setdiff(c(1:length(bnd$x)),picker))
cat("diff=",setdiff(c(1:length(bnd$x)),picker),"\n")
   bnd.2.sort<-pe(bnd.2.sort,c(rev(1:(picker[1]-1)),length(bnd.2.sort$x):picker[1]))

cat("picked:",setdiff(c(1:length(bnd$x)),picker),"\n")
text(bnd,labels=c(1:length(bnd$x)))

### DEBUG
lines(bnd.1.sort,col="red",lwd=2)
a<-scan()
lines(bnd.2.sort,col="blue",lwd=2)
a<-scan()



   # create the initial path:
   # p1, p1 1st intersection, some of bnd, p2 1st intersection, p2

   # ***
#   my.path<-delete_step(my.path,bnd)

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

#delete_step<-function(path, bnd){
#
#   
#   # iterate over the points in the path:
#      # delete as many points as possible, making sure that the
#      # path is still outside
#
#   for(i in 2:(length(path$x)-1)){
#
#      
#
#
#
#
#
#   }
#
#}


