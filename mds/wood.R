# Simon's algorithm for finding the path
wood_path<-function(p1,p2,bnd){
   # args:
   #  p1, p2      the two points to find the path between
   #  bnd         the boundary that got in the way
   # return:
   #  path        set of points on the path

   # HACK:make sure that the points are defined from the left,
   # this may or may not be a fix to the paths joining the wrong points
   if(p2$x<p1$x){
      tmp<-p1
      p1<-p2
      p2<-tmp
   }

### DEBUG
#plot(bnd,type="l",asp=1)
#points(p1)
#points(p2)
#cat("new point:\n")
#cat("x=c(",p1$x,",",p2$x,")\n")
#cat("y=c(",p1$y,",",p2$y,")\n")
#cat("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")

   # create the initial path:
   # p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
   these.paths<-make_bnd_path(p1,p2,bnd)
   this.path.1<-list(x=c(p1$x,these.paths$path.1$x,p2$x),
                     y=c(p1$y,these.paths$path.1$y,p2$y))
   this.path.2<-list(x=c(p1$x,these.paths$path.2$x,p2$x),
                     y=c(p1$y,these.paths$path.2$y,p2$y))

   # pick the shorter path 
   if(hull_length(this.path.1)<hull_length(this.path.2)){
      my.path<-this.path.1
   }else{
      my.path<-this.path.2
   }

   prev.path<-list(x=c(Inf),y=c(Inf))

   # convergence stop
   conv<-0
   conv_stop<-10

   # keep going until we don't remove any more points.
   while(!has_converged(prev.path,my.path) & (conv<conv_stop)){
      # save previous path
      prev.path<-my.path

### DEBUG
#lines(my.path,lwd=2,col="orange")
#text(my.path,labels=1:length(my.path$x))
#cat("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
#a<-scan()
#cat("1\n")

      # delete step, remove anything that doesn't need to be there
      my.path<-delete_step(my.path,bnd)

### DEBUG
#lines(my.path,lwd=2,col="red")
#a<-scan()
#cat("2\n")
      # add new vertices
      my.path<-alter_step(my.path,bnd)


      # increment convergence stopper 
      conv<-conv+1

### DEBUG
#lines(my.path,lwd=2,col="blue")
#a<-scan()
   }

## need this?
#   if(conv==conv_stop) my.path<-NA

### DEBUG
#lines(my.path,lwd=2,col="green")
#cat("########## END ############\n")
#a<-scan()

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

   for(i in bbindex){
      # calculate and save the intersection
      ip<-intersection_point(p1,p2,pe(bnd,c(i,i+1)))

      ips$x<-c(ips$x,ip$x)      
      ips$y<-c(ips$y,ip$y)      
      # find the distance and save
      dists<-c(dists,sqrt((p1$x-ip$x)^2+(p1$y-ip$y)^2))
   }


   # remove duplicates (ie when dist is zero)
   if(length(ips$x)>3){
      p1.ind<-which((ips$x==p1$x)&(ips$y==p1$y))
      p2.ind<-which((ips$x==p2$x)&(ips$y==p2$y))
   
      if(length(p1.ind)!=0&length(p1.ind)!=0){
         nonzero<-c(p1.ind,p2.ind)
         dists<-dists[-nonzero]
         bbindex<-bbindex[-nonzero]
         ips$x<-ips$x[-nonzero];ips$y<-ips$y[-nonzero]
      }
   }

   # the two intersection points
   ip1<-pe(ips,order(dists)[1])
   ip2<-pe(ips,order(dists,decreasing=TRUE)[1])

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

   # make sure we aren't adding a superfluous vertex (not both end of a side
   # that we don't need)
   # ie. ip1, vertex, vertex... rather than vertex,ip1,vertex

   if(length(picker)>1){

      if(on_line(ip1,pe(bnd.1.sort,1:2))){
         bnd.1.sort<-pe(bnd.1.sort,-1)
         picker<-picker[-1]
      }
      ep.1<-length(bnd.1.sort$x)

      if(on_line(ip2,pe(bnd.1.sort,(ep.1-1):ep.1))){
         bnd.1.sort<-pe(bnd.1.sort,-ep.1)
         picker<-picker[-ep.1]
      }
   }

   bnd.2.sort<-pe(pe(bnd,c(1:(length(bnd$x)-1))),setdiff(c(1:(length(bnd$x)-1)),picker))
   bnd.2.sort<-pe(bnd.2.sort,c(rev(1:(picker[1]-1)),length(bnd.2.sort$x):picker[1]))

   # make sure we aren't adding a superfluous vertex (not both end of a side
   # that we don't need)
   if(length(picker)>1){
      if(on_line(ip1,pe(bnd.2.sort,1:2))){
         bnd.2.sort<-pe(bnd.2.sort,-1)
      }
      ep.1<-length(bnd.2.sort$x)
      if(on_line(ip2,pe(bnd.2.sort,(ep.1-1):ep.1))){
         bnd.2.sort<-pe(bnd.2.sort,-ep.1)
      }
   }

   # tackle the index going out of range, ugly but necessary without 
   # a stack structure
   pickend<-picker[length(picker)]
   if(pickend==(length(bnd$x))) pickend<-1
   pickendp<-pickend+1

   pickstart<-picker[1]
   pickstartm<-pickstart-1
   if(pickstart==1) pickstartm<-length(bnd$x)-1
      
   if(on_line(ip2,pe(bnd,pickend:pickendp))&
      on_line(ip1,pe(bnd,pickstartm:pickstart))){
      bnd.1.sort<-list(x=rev(bnd.1.sort$x),
                       y=rev(bnd.1.sort$y))
      bnd.2.sort<-list(x=rev(bnd.2.sort$x),
                       y=rev(bnd.2.sort$y))
   }

   # p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
   bnd.1.sort<-list(x=c(ip1$x,rev(bnd.1.sort$x),ip2$x),
                    y=c(ip1$y,rev(bnd.1.sort$y),ip2$y))
   bnd.2.sort<-list(x=c(ip1$x,rev(bnd.2.sort$x),ip2$x),
                    y=c(ip1$y,rev(bnd.2.sort$y),ip2$y))

   # return both paths
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

   # convergence stop
   conv<-0
   conv_stop<-10
   # keep going until we don't remove any more points.
   while(!has_converged(prev.path,path)&(conv<conv_stop)){
      # save the previous path to compare, above
      prev.path<-path
      # start point for triplet selection
      i<-2
      while((i+1)<=length(path$x)){
         # create the current triplet to inspect
         my.trip<-pe(path,c(i-1,i,i+1))

         # if we are going forward and back again, just
         # remove the point
         if(my.trip$x[1]==my.trip$x[3]&my.trip$y[1]==my.trip$y[3]){
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
   conv<-conv+1
   }
   return(path)
}


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
   # convergence stop
   conv<-0
   conv_stop<-10
   while(!has_converged(prev.path,path)&(conv<conv_stop)){
      # save the previous path for comparison
      prev.path<-path
      i<-2 # start point
      while((i+1)<=length(path$x)){
         # for each point i, look at the line i-1 to i+1
         my.trip<-pe(path,c(i-1,i,i+1))

         ep1<-pe(my.trip,1)
         ep2<-pe(my.trip,3)

         # does it go inside-outside-inside?
         if(all(facing(ep1,ep2,bnd))){

            # create a new path
            these.paths<-make_bnd_path(ep1,ep2,bnd)

            # make sure that the new paths are as short as possible
### IS THIS NEEDED?
#            these.paths$path.1<-delete_step(these.paths$path.1,bnd)
#            these.paths$path.2<-delete_step(these.paths$path.2,bnd)

            # pick the shorter path 
            if(hull_length(these.paths$path.1)<hull_length(these.paths$path.2)){
               new.path<-these.paths$path.1
            }else{
               new.path<-these.paths$path.2
            }

            # create new path, compare complete new path with old one, if the
            # new one is better then keep it.
            new.path<-delete_step(list(
                    x=c(path$x[1:(i-1)],new.path$x,path$x[(i+1):length(path$x)]),
                    y=c(path$y[1:(i-1)],new.path$y,path$y[(i+1):length(path$y)])),bnd)
            my.trip<-delete_step(list(
                    x=c(path$x[1:(i-1)],my.trip$x,path$x[(i+1):length(path$x)]),
                    y=c(path$y[1:(i-1)],my.trip$y,path$y[(i+1):length(path$y)])),bnd)

            if(hull_length(new.path)<hull_length(my.trip)){
               path<-new.path
            }else{
               path<-my.trip
            }
         }
         i<-i+1
      }
      conv<-conv+1
   }
   # return the path
   return(path)
}

# check convergence
has_converged<-function(prev.path,path){
# check if the new and old paths are the same, first check length
# then their contents
   if(length(prev.path$x)==length(path$x) & length(prev.path$y)==length(path$y)){
      if(all(prev.path$x==path$x) & all(prev.path$y==path$y)){
         return(TRUE)
      }else return(FALSE)
   }else return(FALSE)
}


