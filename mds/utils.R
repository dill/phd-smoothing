# utility functions


# routine to Pick Elements, pe
pe<-function(this.list,el){
   lapply(this.list,function(x) x[el])
}

# find the intersection point between two points and a line
intersection_point<-function(p1,p2,edge){
   # args:
   #  p1, p2      the two points making up the end points of the line
   #  line         boundary of the shape to test intersection with
   # return:
   #     intersection point

   eps<-1e-16

   # calculation of intersection is straight from 
   # Handbook of Mathematics Bronstein et al. pp. 195,196

   ### calculate intersection point
   # first calculate the coefficients for the lines
   # the line between the points
   a1<- -1/(p2$x-p1$x)
   b1<- 1/(p2$y-p1$y)
   c1<- p1$x/(p2$x-p1$x)-p1$y/(p2$y-p1$y)

   # the edge
   a2<- -1/(edge$x[2]-edge$x[1])
   b2<- 1/(edge$y[2]-edge$y[1])
   c2<- edge$x[1]/(edge$x[2]-edge$x[1])-edge$y[1]/(edge$y[2]-edge$y[1])

   # handle the horizontal/vertical line case...
  
   # point line horizontal
   if(abs((p2$y-p1$y)/(p2$x-p1$x))<eps){
      a1<-0;b1<-1;c1<--p1$y
   # point line vertical
   }else if(abs((p2$x-p1$x)/(p2$y-p1$y))<eps){
      a1<-1;b1<-0;c1<--p1$x
   # edge horizontal
   }else if(abs((edge$y[2]-edge$y[1])/(edge$x[2]-edge$x[1]))<eps){
      a2<-0;b2<-1;c2<--edge$y[1]
   # edge vertical
   }else if(abs((edge$x[2]-edge$x[1])/(edge$y[2]-edge$y[1]))<eps){
      a2<-1;b2<-0;c2<--edge$x[1]
   }

   ### do something to check for infinities...
   if(all(c(a1,a2,b1,b2,c1,c2)!=Inf) & all(c(a1,a2,b1,b2,c1,c2)!=-Inf)){
      # calculate the intersection
      intx<-det(matrix(c(b1,b2,c1,c2),2,2))/det(matrix(c(a1,a2,b1,b2),2,2))
      inty<-det(matrix(c(c1,c2,a1,a2),2,2))/det(matrix(c(a1,a2,b1,b2),2,2))
      ret<-list(x=intx,y=inty)
   }else{
      ret<-list(x=Inf,y=Inf)
   }

   return(ret)

}

# calculate the length of the convex hull
hull_length<-function(hull){
   # args:
   #  hull        convex hull (list(x=,y=))
   # return
   #     its length

   hullen<-0

   for (i in 1:(length(hull$x)-1)){
      hullen<-hullen+sqrt((hull$x[i+1]-hull$x[i])^2+(hull$y[i+1]-hull$y[i])^2)
   }
   return(hullen)
}

# do two points and the boundary intersect?
do_intersect<-function(p1,p2,bnd){
   # we do this by seeing if the bounding boxes intersect
   # from Mastering Algorithms with Perl, p 451

   eps<-1e-16

   # returns a string of T/F values
   ret<-rep(TRUE,(length(bnd$x)-1))

   # bounding box around the points p1,p2
   p.bbox<-list(x=c(max(p1$x,p2$x),min(p1$x,p2$x)),
                y=c(max(p1$y,p2$y),min(p1$y,p2$y)))

### DEBUG
# same
#cat("p1=",p1$x,p1$y,"\n")
#cat("p2=",p2$x,p2$y,"\n")
#cat("p.bbox$x=",p.bbox$x,"\n")
#cat("p.bbox$y=",p.bbox$y,"\n")


   # iterate over sides (ie vertex pairs)
   # NB the last vertex should be the first
   for (i in 1:(length(bnd$x)-1)){

      # bounding box for the edge
      e.bbox<-list(x=c(max(bnd$x[c(i,i+1)]),min(bnd$x[c(i,i+1)])),
                   y=c(max(bnd$y[c(i,i+1)]),min(bnd$y[c(i,i+1)])))

### DEBUG
# same
#cat("e.bbox$x=",e.bbox$x,"\n")
#cat("e.bbox$y=",e.bbox$y,"\n")
 
      # establish whether the bounding boxes intersect
      if(e.bbox$x[1]+eps < p.bbox$x[2]) ret[i]<-FALSE
      if(p.bbox$x[1]+eps < e.bbox$x[2]) ret[i]<-FALSE
      if(e.bbox$y[1]+eps < p.bbox$y[2]) ret[i]<-FALSE
      if(p.bbox$y[1]+eps < e.bbox$y[2]) ret[i]<-FALSE

      # if the bounding boxes do intersect, check that the
      # intersection of the two lines lies within the bounding
      # boxes.
      if(ret[i]){
         # first find the intersection point
         ip<-intersection_point(p1,p2,list(x=bnd$x[c(i,i+1)],y=bnd$y[c(i,i+1)]))

cat("ip=",ip$x,ip$y,"\n")


         # first need to handle the horizontal and vertical line cases
         # then handle whether the intersection point lies within the
         # the bounding box
         if(abs(e.bbox$x[1]-e.bbox$x[2])>eps){

cat("1a\n")
cat("abs(ebbox)",abs(e.bbox$x[1]-e.bbox$x[2]),"\n\n")
cat(ip$x,">",e.bbox$x[1]," | ",ip$x,"<",e.bbox$x[2],"\n")
            if(ip$x>e.bbox$x[1] | ip$x<e.bbox$x[2]){
               ret[i]<-FALSE
cat("1b\n")
cat("ip>",ip$x>e.bbox$x[1],"   ip<",ip$x<e.bbox$x[2],"\n")
            }
         }

         if(abs(p.bbox$x[1]-p.bbox$x[2])>eps){
#cat("2a\n")
            if(ip$x>p.bbox$x[1] | ip$x<p.bbox$x[2]){
               ret[i]<-FALSE
#cat("2b\n")
            }
         }

         if(abs(e.bbox$y[1]-e.bbox$y[2])>eps){
#cat("3a\n")
            if(ip$y>e.bbox$y[1] | ip$y<e.bbox$y[2]){
#cat("3b\n")
               ret[i]<-FALSE
            }
         }

         if(abs(p.bbox$y[1]-p.bbox$y[2])>eps){
#cat("4a\n")
            if(ip$y>p.bbox$y[1] | ip$y<p.bbox$y[2]){
#cat("4b\n")
               ret[i]<-FALSE
            }
         }

      }

   }
   return(ret)
}

# special do_intersect, thinks that points that start/end at the same
# place don't intersect. Neither do exactly overlapping lines.
sp_do_intersect<-function(p1,p2,bnd){

   # returns a string of T/F values
   ret<-rep(TRUE,(length(bnd$x)-1))

   # iterate over sides (ie vertex pairs)
   # NB the last vertex should be the first
   for (i in 1:(length(bnd$x)-1)){

      # case where the lines are exactly overlapping
      if((p1$x==bnd$x[i] & p2$x==bnd$x[i+1] &
         p1$y==bnd$y[i] & p2$y==bnd$y[i+1])|
         (p2$x==bnd$x[i] & p1$x==bnd$x[i+1] &
         p2$y==bnd$y[i] & p1$y==bnd$y[i+1])) ret[i]<-FALSE

      # start/end points the same
      if((p1$x==bnd$x[i] & p1$y==bnd$y[i])|
         (p2$x==bnd$x[i] & p2$y==bnd$y[i])|
         (p1$x==bnd$x[i+1] & p1$y==bnd$y[i+1]) |
         (p2$x==bnd$x[i+1] & p2$y==bnd$y[i+1])) ret[i]<-FALSE

      # call original routine if this doesn't work
      if(ret[i]) ret[i]<-do_intersect(p1,p2,pe(bnd,i:(i+1)))
   }
   return(ret)
}

# determine whether the line between two points is facing inside or outside
facing<-function(p1,p2,bnd){
   # Args:
   #  p1, p2      the points
   #  bnd         the boundary
   # Return:
   #              logical 2-vector, TRUE= facing inside
   #                                FALSE= facing outside

   # there might be no intersections, so if there aren't
   # then just return FALSE and let the delete step take 
   # care of it
   ret<-rep(FALSE,2)

   doint<-do_intersect(p1,p2,bnd)

   if(sum(doint)>1){

      # find intersections & sort by distance
      bbindex<-c(1:(length(bnd$x)-1))[doint]
      # hold distances and intersection points temporarily
      dists<-c(); ips<-list(x=c(),y=c())

      for(j in bbindex){
         # calculate and save the intersection
         ip<-intersection_point(p1,p2,pe(bnd,c(j,j+1)))
         ips$x<-c(ips$x,ip$x)
         ips$y<-c(ips$y,ip$y)

         # find the distance and save
         dists<-c(dists,sqrt((p1$x-ip$x)^2+(p1$y-ip$y)^2))
      }

      # remove duplicates (ie when dist is zero)
      if(length(ips$x)>3){
         p1.ind<-which((ips$x==p1$x)&(ips$y==p1$y))
         p2.ind<-which((ips$x==p2$x)&(ips$y==p2$y))
         nonzero<-c(p1.ind,p2.ind)
         if(length(nonzero)!=0 & length(nonzero)!=length(dists)){
            dists<-dists[-nonzero]
            bbindex<-bbindex[-nonzero]
            ips$x<-ips$x[-nonzero];ips$y<-ips$y[-nonzero]
         }
      }

      # find first intersection between p1 and bnd
      p1.int<-pe(ips,order(dists)[1])
      # find first intersection between p2 and bnd
      p2.int<-pe(ips,order(dists,decreasing=TRUE)[1])

      # midpoint between p1 and first intersection 
      p1.mp<-list(x=(p1.int$x+p1$x)/2,y=(p1.int$y+p1$y)/2)
      # midpoint between p2 and first intersection 
      p2.mp<-list(x=(p2.int$x+p2$x)/2,y=(p2.int$y+p2$y)/2)
 
      # are the midpoints inside?
      ret<-inSide(bnd,c(p1.mp$x,p2.mp$x),c(p1.mp$y,p2.mp$y))
   }
   return(ret)   
}

# find if a point is on a line
on_line<-function(p1,this.line){

   eps<-1e-10
   # left hand side of equation
   leftside<-(p1$y-this.line$y[1])/(this.line$y[2]-this.line$y[1])
   #right hand side of equation
   rightside<-(p1$x-this.line$x[1])/(this.line$x[2]-this.line$x[1])

   if(abs(leftside-rightside)<eps){
### DEBUG
#cat("On line\n")
#a<-scan()
       return(TRUE)
   }else{
### DEBUG
#lines(this.line,col="green",lwd=2)
#points(p1)
#cat("Off line\n")
#cat("leftside=(",p1$y,"-",this.line$y[1],")/(",this.line$y[2],"-",this.line$y[1],")\n")
#cat("rightside=(",p1$x,"-",this.line$x[1],")/(",this.line$x[2],"-",this.line$x[1],")\n")
#cat("l=",leftside,"r=",rightside,"\n")
#cat("l-r=",leftside-rightside,"\n")
#a<-scan()
      return(FALSE)
   }
}
