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
      dists<-c(dists,sqrt((ip$x-bnd$x[i])^2+(ip$y-bnd$y[i])^2))
   }
   # the two intersection points
   ip1<-pe(ips,order(dists)[1])
   ip2<-pe(ips,order(dists,decreasing=TRUE)[1])

# DEBUG
points(ip1,pch=23,col="red")
text(ip1,labels=c("ip1"))
points(ip2,pch=23,col="red")
text(ip2,labels=c("ip2"))

cat(ip2$x,ip2$y,"\n")


   # first, sort the list such that p1's first intersection point
   # is the first element

   # vertex before the intersection point
   ip1.index<-bbindex[order(dists)[1]] 
   ip2.index<-bbindex[order(dists)[length(dists)]] 

   bnd.1.sort<-pe(bnd,c(ip1.index:(length(bnd$x)-1),1:(ip1.index-1)))
   bnd.1.sort<-pe(bnd.1.sort,c(2:bbindex[2]))#(1+abs(ip2.index-ip1.index))))


   bnd.2.sort<-pe(bnd,c(ip2.index:(length(bnd$x)-1),1:(ip2.index-1)))
   bnd.2.sort<-pe(bnd.2.sort,c(2:(abs(bbindex[1]-bbindex[2])+1)))


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


