# Multidimensional scaling approach...

# let's see what happens

# create the distance matrix
create_distance_matrix<-function(x,y,bnd,res=100){
   # requires the library soap   
   # args:
   #  x,y      data points
   #  bnd      boundary list(x=c(),y=c())
   #  res      resolution to test if line is inside

   # make sure that x and y are the same length
   if(length(x)!=length(y)){
      return(FALSE)
   }

   # create a matrix to hold the distances
   D<-matrix(0,length(x),length(x))

   # first find those points that we can use Euclidean
   # distance for
   for(i in 1:length(x)){
      p1<-list(x=x[i],y=y[i])
      for(j in (i+1):length(y)){
         p2<-list(x=x[j],y=y[j])

# DEBUG
plot(x,y,pch=".",asp=1)
lines(bnd)
points(x=c(p1$x,p2$x),y=c(p1$y,p2$y),col="red")
#lines(x=c(p1$x,p2$x),y=c(p1$y,p2$y),col="green")

         # find the intersection points (if any) 
         intp<-intersect_sides(p1,p2,bnd)
 
         if(length(intp)>=2){

            ### create a list of points to create the hulls from
            # NB. ordering here doesn't matter as convex hull is a sort


############################################################
### WEIRD THINGS STILL HAPPENING HERE!
points.1<-list(x=c(p1$x,bnd$x[intp[1]:(intp[2]+1)],p2$x),
               y=c(p1$y,bnd$y[intp[1]:(intp[2]+1)],p2$y))

points.2<-list(x=c(p1$x,bnd$x[(intp[1]+1):intp[2]],p2$x),
               y=c(p1$y,bnd$y[(intp[1]+1):intp[2]],p2$y))

# DEBUG
cat("intp:",intp,"\n")
#lines(x=bnd$x[intp[2]:(intp[1]+1)],y=bnd$y[intp[2]:(intp[1]+1)],col="blue",lwd=3)
#lines(x=bnd$x[(intp[2]+1):intp[1]],y=bnd$y[(intp[2]+1):intp[1]],col="red",lwd=3)

points(x=bnd$x[c(intp[1],intp[1]+1)],y=bnd$y[c(intp[1],intp[1]+1)],col="blue",lwd=3)
points(x=bnd$x[c(intp[2],intp[2]+1)],y=bnd$y[c(intp[2],intp[2]+1)],col="red",lwd=3)
text(x=bnd$x[c(intp[1],intp[1]+1)],y=bnd$y[c(intp[1],intp[1]+1)],labels=c("1","1+1"))
text(x=bnd$x[c(intp[2],intp[2]+1)],y=bnd$y[c(intp[2],intp[2]+1)],labels=c("2","2+1"))
a<-scan()

            # calculate the hulls
            hull.1<-convex_hull(points.1)
            hull.2<-convex_hull(points.2)
# DEBUG
lines(hull.1,col="green")
lines(hull.2,col="blue")
cat("waiting...\n")
a<-scan()
##############################################################
            # find their lengths, keeping the shortest
            if(hull_length(hull.1) < hull_length(hull.2)){
               D[i,j]<-hull_length(hull.1)-sqrt((p1$x-p2$x)^2+(p1$y-p2$y)^2)
            }else{
               D[i,j]<-hull_length(hull.2)-sqrt((p1$x-p2$x)^2+(p1$y-p2$y)^2)
            }

         }else{
            # insert the distance if they are inside
            D[i,j]<-sqrt((p1$x-p2$x)^2+(p1$y-p2$y)^2)

         }

      }
   }


   # create the lower triangle of the matrix
   # NB. diagonal should be 0
   D<-D+t(D) 

   return(D)

}

# find the intersecting side with a line between two points
intersect_sides<-function(p1,p2,bnd){
   # args:
   #  p1, p2      the two points making up the end points of the line
   #  bnd         boundary of the shape to test intersection with
   # return:
   #     side numbers that intersect (two furthest apart ones to account
   #     for the possibility for multiple edges)

   # first calculate all intersections
   
   # store the first vertex of the edge, the other vertex of the
   # edge is that +1
   edges<-c()

   # iterate over sides (ie vertex pairs)
   # NB the last vertex should be the first
   for (i in 1:(length(bnd$x)-1)){

      edge<-list(x=bnd$x[c(i,i+1)],y=bnd$y[c(i,i+1)])

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

      ### do something to check for infinities...
      if(all(c(a1,a2,b1,b2,c1,c2)!=Inf) & all(c(a1,a2,b1,b2,c1,c2)!=-Inf)){
      
         # calculate the intersection
         intx<-det(matrix(c(b1,b2,c1,c2),2,2))/det(matrix(c(a1,a2,b1,b2),2,2)) 
         inty<-det(matrix(c(c1,c2,a1,a2),2,2))/det(matrix(c(a1,a2,b1,b2),2,2)) 
     

         # find the smaller of the two segments
         if(sqrt((p2$x-p1$x)^2+(p2$y+p1$y)^2)<
            sqrt((edge$x[2]-edge$x[1])^2+(edge$y[2]-edge$y[1])^2)){
            ep<-list(x=c(p1$x,p2$x),y=c(p1$y,p2$y))
         }else{
            ep<-list(x=edge$x,y=edge$y)
         }
 
         # see if it is within the line between the two points
         if((intx > min(p1$x,p2$x)) & (intx < max(p1$x,p2$x)) &
            (inty > min(p1$y,p2$y)) & (inty < max(p1$y,p2$y))){
         # see if the intersection occurs between the two smallest points
         if((intx > min(ep$x)) & (intx < max(ep$x)) &
            (inty > min(ep$y)) & (inty < max(ep$y))){

# DEBUG
#abline(h=c(min(p1$y,p2$y), max(p1$y,p2$y)))      
#abline(v=c(min(p1$x,p2$x), max(p1$x,p2$x)))
#points(x=intx,y=inty,pch=22,cex=3,col="red")
#abline(a=-c1/b1,b=-a1/b1,col="red",lwd=2)     
#abline(a=-c2/b2,b=-a2/b2,col="red",lwd=2)     
#a<-scan()
            # if it is within range, add that vertex pair number to a list
            edges<-c(edges,i)
            cat("edges: ",edges,"\n") 
         }}
      }
   }

   # take the furthest vertex pairings
   if(length(edges)>=2){
### NEED TO FIX THIS!
#      return(c(max(edges),min(edges)))
       return(edges)
   }else{
      return(FALSE)
   }
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



