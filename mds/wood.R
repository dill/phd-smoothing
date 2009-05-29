# Simon's algorithm for finding the path
wood_path<-function(p1,p2,bnd){
   # args:
   #  p1, p2      the two points to find the path between
   #  bnd         the boundary that got in the way
   # return:
   #  path        set of points on the path

# DEBUG
plot(bnd,type="l",asp=1)
#text(bnd,labels=c(1:length(bnd$x)))
points(p1,pch=24,col="pink")
points(p2,pch=24,col="pink")
text(p1,label="1")
text(p2,label="2")


   # HACKHACKHACK this may or may not be a fix to the
   # paths joining the wrong points
   if(p2$x<p1$x){
      tmp<-p1
      p1<-p2
      p2<-tmp
   }
   
### DEBUG
#lines(my.path,col="red",lwd=2)
#a<-scan()

   # create the initial path:
   # p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
   these.paths<-make_bnd_path(p1,p2,bnd)
   bnd.1.sort<-these.paths$path.1
   bnd.2.sort<-these.paths$path.2
   

   # create the initial paths:
   # p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
   this.path.1<-list(x=c(p1$x,bnd.1.sort$x,p2$x),
                     y=c(p1$y,bnd.1.sort$y,p2$y))
   this.path.2<-list(x=c(p1$x,bnd.2.sort$x,p2$x),
                     y=c(p1$y,bnd.2.sort$y,p2$y))

   # pick the shorter path 
   if(hull_length(this.path.1)<hull_length(this.path.2)){
      my.path<-this.path.1
   }else{
      my.path<-this.path.2
   }

   # ***
   prev.path<-list(x=c(Inf),y=c(Inf))

   # keep going until we don't remove any more points.
   while(length(prev.path$x)!=length(my.path$x) & length(prev.path$y)!=length(my.path$y)){
      prev.path<-my.path

      # delete step, remove anything that doesn't need to be there
      my.path<-delete_step(my.path,bnd)
      
      # add new vertices
      my.path<-alter_step(my.path,bnd)
   }

### DEBUG
#lines(my.path,col="blue",lwd=2)
#a<-scan()
cat("x=c(",p1$x,",",p2$x,")\n")
cat("y=c(",p1$y,",",p2$y,")\n")
#plot(bnd,type="l")
#text(bnd,labels=1:length(bnd$x))
lines(my.path,col="orange",lwd=2)
a<-scan()
   return(my.path)


}

# create a path between p1 and p2 using the boundary
make_bnd_path<-function(p1,p2,bnd){
   # find the first intersection between p1, p2 and the boundary
   # for each point

   # do the bounding box check first, for speed
   bbindex<-c(1:(length(bnd$x)-1))[do_intersect(p1,p2,bnd)]
   # hold distances and intersection points temporarily
   dists<-c(); ips<-list(x=c(),y=c())

# DEBUG
#cat("bbindex=",bbindex,"\n")

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
#points(ip1,pch=23,col="red")
#text(ip1,labels=c("ip1"))
#points(ip2,pch=23,col="red")
#text(ip2,labels=c("ip2"))
#a<-scan()

   # sort the intersections by their distances from p1 and p2
   ip1.index<-bbindex[order(dists)] 
   ip2.index<-bbindex[order(dists,decreasing=TRUE)] 

   # This is quite horrible code.
   # What we do is: take the ordering that makes sense first
   # eg 1:5 not 5:1, make that set of edges, then take the difference
   # between that set and the complete set of vertices.
   picker<-sort(c(ip1.index[1],(ip1.index[length(ip1.index)]+1)))
   picker<-c(picker[1]:picker[2])

### DEBUG
#cat("picker=",picker,"\n")


   bnd.1.sort<-pe(bnd,picker)

   bnd.2.sort<-pe(pe(bnd,c(1:(length(bnd$x)))),setdiff(c(1:length(bnd$x)),picker))
   bnd.2.sort<-pe(bnd.2.sort,c(rev(1:(picker[1]-1)),length(bnd.2.sort$x):picker[1]))

   # p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
   bnd.1.sort<-list(x=c(ip1$x,rev(bnd.1.sort$x),ip2$x),
                     y=c(ip1$y,rev(bnd.1.sort$y),ip2$y))
   bnd.2.sort<-list(x=c(ip1$x,rev(bnd.2.sort$x),ip2$x),
                     y=c(ip1$y,rev(bnd.2.sort$y),ip2$y))
   return(list(path.1=bnd.1.sort,path.2=bnd.2.sort))

}

# iterate over the points in the path:
# delete as many points as possible, making sure that the
# path is still outside
delete_step<-function(path, bnd){
   # Args:
   #  path     the current path
   #  bnd      the boundary
   # Return:
   #           revised path with dropped vertices
   prev.path<-list(x=c(Inf),y=c(Inf))

   # keep going until we don't remove any more points.
   while(length(prev.path$x)!=length(path$x) & length(prev.path$y)!=length(path$y)){
      # save the previous path to compare, above
      prev.path<-path
      # start point for triplet selection
      i<-2
      while((i+1)<=length(path$x)){
         # create the current triplet to inspect
         my.trip<-pe(path,c(i-1,i,i+1))

         if(my.trip$x[1]==my.trip$x[3]&my.trip$y[1]==my.trip$y[3]){
            # if we are going forward and back again, just
            # remove the point
            path<-pe(path,-c(i,i+1))

        
         # if deleting the middle point makes the resulting line cross the
         # the boundary then keep it, else get rid of it 
         }else if(all(!sp_do_intersect(pe(my.trip,1),pe(my.trip,3),bnd))&
            inSide(bnd,(pe(my.trip,3)$x+pe(my.trip,1)$x)/2,
                       (pe(my.trip,3)$y+pe(my.trip,1)$y)/2)){
            path<-pe(path,-i)
         }
   
         i<-i+1
      }
   }
   return(path)
}


# remove path elements outside the 


# alter the path
alter_step<-function(path,bnd){
   # Args:
   #  path     the current path
   #  bnd      the boundary
   # Return:
   #           revised path with added/ammended vertices



   prev.path<-list(x=Inf,y=Inf)
   # iterate over the points in the path:
   # alter the path, until on two(?) consecutive runs there are
   # no changes to the path
   while(length(prev.path$x)!=length(path$x) & length(prev.path$y)!=length(path$y)){
      # save the previous path for comparison
      prev.path<-path
      i<-2 # start point
      while((i+1)<=length(path$x)){
         # for each point i, look at the line i-1 to i+1
         my.trip<-pe(path,c(i-1,i,i+1))

### DEBUG
#plot(bnd,type="l")
#text(bnd,labels=1:length(bnd$x))
#lines(my.trip,lwd=2,col="grey")
#a<-scan()
   
         ep1<-pe(my.trip,1)
         ep2<-pe(my.trip,3)

         # does it go inside-outside-inside?
         if(all(facing(ep1,ep2,bnd))){
            # create a new path
            these.paths<-make_bnd_path(ep1,ep2,bnd)

            # pick the shorter path 
            if(hull_length(these.paths$path.1)<hull_length(these.paths$path.2)){
               new.path<-these.paths$path.1
            }else{
               new.path<-these.paths$path.2
            }

            # this code might check to see if things are backwards
            x.check<-(new.path$x[1:(length(new.path$x)-1)]
                      +new.path$x[2:length(new.path$x)])/2
            y.check<-(new.path$y[1:(length(new.path$y)-1)]
                      +new.path$y[2:length(new.path$y)])/2
            backwards.check<-inSide(bnd,x.check,y.check)
            if(!all(backwards.check)){
               npl<-length(new.path$x)
               new.path<-pe(new.path,c(1,2,(npl-2):3,(npl-1),npl))
               rm(npl)
            }


### DEBUG
lines(new.path,lwd=2,col="red")
text(new.path,labels=1:length(new.path$x))
a<-scan()
            new.path<-delete_step(new.path,bnd)

            if(hull_length(new.path)<hull_length(my.trip)){ 
               path<-list(x=c(path$x[1:(i-1)],new.path$x,path$x[(i+1):length(path$x)]),
                       y=c(path$y[1:(i-1)],new.path$y,path$y[(i+1):length(path$y)]))

               path<-delete_step(path,bnd)
### DEBUG
#lines(new.path,lwd=2,col="orange")
#a<-scan()
            }

         }
         i<-i+1
      }
   }
   # return the path
   return(path)

}
