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

cat("pre-picker=",picker,"\n")

#   if(do_intersect(p1,p2,pe(bnd,picker[1:2]))){
#      picker<-picker[-1]
#cat("orly?\n")
#   }
#
#   if(do_intersect(p1,p2,pe(bnd,picker[(length(picker)-1):length(picker)]))){
#      picker<-rev(rev(picker)[-1])
#cat("orly2?\n")
#   }


   bnd.1.sort<-pe(bnd,picker)

cat("picker=",picker,"\n")

   bnd.2.sort<-pe(pe(bnd,c(1:(length(bnd$x)))),setdiff(c(1:length(bnd$x)),picker))
#cat("diff=",setdiff(c(1:length(bnd$x)),picker),"\n")
   bnd.2.sort<-pe(bnd.2.sort,c(rev(1:(picker[1]-1)),length(bnd.2.sort$x):picker[1]))

#cat("picked:",setdiff(c(1:length(bnd$x)),picker),"\n")
text(bnd,labels=c(1:length(bnd$x)))

### DEBUG
#lines(bnd.1.sort,col="red",lwd=2)
#a<-scan()
#lines(bnd.2.sort,col="blue",lwd=2)
#a<-scan()

   # pick the shorter of the boundary sections
   if(hull_length(bnd.1.sort)<hull_length(bnd.2.sort)){
      bnd.start<-bnd.1.sort
   }else{
      bnd.start<-bnd.2.sort
   }

   
   # create the initial path:
   # p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
   my.path<-list(x=c(p1$x,ip1$x,rev(bnd.start$x),ip2$x,p2$x),
                 y=c(p1$y,ip1$y,rev(bnd.start$y),ip2$y,p2$y))

### DEBUG
lines(my.path,col="red",lwd=2)
a<-scan()




   # ***
   my.path<-delete_step(my.path,bnd)

lines(my.path,col="blue",lwd=2)
a<-scan()
return(my.path)
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

# iterate over the points in the path:
# delete as many points as possible, making sure that the
# path is still outside
delete_step<-function(path, bnd){

   prev.path<-list(x=c(Inf),y=c(Inf))

   while(length(prev.path$x)!=length(path$x) & length(prev.path$y)!=length(path$y)){
      # save the previous path to compare, above
      prev.path<-path
      # start point for triplet selection
      i<-2
      while((i+1)<=length(path$x)){
         # create the current triplet to inspect
         my.trip<-pe(path,c(i-1,i,i+1))
        
         # if deleting the middle point makes the resulting line cross the
         # the boundary then keep it, else get rid of it 
         if(all(!sp_do_intersect(pe(my.trip,1),pe(my.trip,3),bnd))&
            inSide(bnd,(pe(my.trip,3)$x+pe(my.trip,1)$x)/2,
                       (pe(my.trip,3)$y+pe(my.trip,1)$y)/2)){
            path<-pe(path,-i)
         }
   
         i<-i+1
      }
   }

   return(path)

}


