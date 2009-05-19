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

   # iterate over all of the pairs of points, only calculate the
   # upper diagonal of the matrix, since it's symmetric
   for(i in 1:(length(x)-1)){
      p1<-list(x=x[i],y=y[i])
      for(j in (i+1):length(y)){
         p2<-list(x=x[j],y=y[j])

# DEBUG
plot(bnd,type="l",lwd=2,asp=1)
points(x,y,pch=".")
points(x=c(p1$x,p2$x),y=c(p1$y,p2$y),col="red")
lines(x=c(p1$x,p2$x),y=c(p1$y,p2$y),col="red",lwd=2)

         intp<-do_intersect(p1,p2,bnd)

         if(any(intp)){

            ### create a list of points to create the hulls from
            # NB. ordering here doesn't matter as convex hull is a sort

            picker<-c((which(intp)[1]+1):(rev(which(intp))[1]-1))
            points.1<-list(x=c(p1$x,p2$x,bnd$x[picker]),
                           y=c(p1$y,p2$y,bnd$y[picker]))
            
            picker<-c((which(!intp)[1]+1):(rev(which(!intp))[1]-1))
            points.2<-list(x=c(p1$x,p2$x,bnd$x[picker]),
                           y=c(p1$y,p2$y,bnd$y[picker]))


# DEBUG
points(x=points.2$x,y=points.2$y,col="red",pch=22,cex=2)
points(x=points.1$x,y=points.1$y,col="blue",pch=23,cex=2)
text(x=points.1$x,y=points.1$y,labels=c(1:length(points.1$x)))
a<-scan()
#
#text(x=bnd$x[c(intp[1],(intp[1]+1))],y=bnd$y[c(intp[1],(intp[1]+1))],
#    labels=c("1","1+1"))
#
#text(x=bnd$x[c(intp[2],(intp[2]+1))],y=bnd$y[c(intp[2],(intp[2]+1))],
#     labels=c("2","2+1"))
#a<-scan()

            # calculate the hulls
            hull.1<-convex_hull(points.1)
            hull.2<-convex_hull(points.2)
            
# DEBUG
plot(bnd,type="l",lwd=2,asp=1)
points(x,y,pch=".")
text(x=x,y=y,col="green",labels=c(1:length(x)))
points(x=c(p1$x,p2$x),y=c(p1$y,p2$y),col="red")
lines(hull.1,col="green")
lines(hull.2,col="blue")
cat("waiting...\n")
a<-scan()

            # find their lengths, keeping the shortest
            # since the hull includes the straight path from p1 to p2
            # we need to subtract that when we put it into D
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

# find the intersection point between two points and a line
intersection_point<-function(p1,p2,edge){
   # args:
   #  p1, p2      the two points making up the end points of the line
   #  line         boundary of the shape to test intersection with
   # return:
   #     intersection point
   

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

   # iterate over sides (ie vertex pairs)
   # NB the last vertex should be the first
   for (i in 1:(length(bnd$x)-1)){

      # bounding box for the edge
      e.bbox<-list(x=c(max(bnd$x[c(i,i+1)]),min(bnd$x[c(i,i+1)])),
                   y=c(max(bnd$y[c(i,i+1)]),min(bnd$y[c(i,i+1)])))

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

         # first need to handle the horizontal and vertical line cases
         # then handle whether the intersection point lies within the
         # the bounding box
         if(abs(e.bbox$x[1]-e.bbox$x[2])>eps){
            if(ip$x>e.bbox$x[1] | ip$x<e.bbox$x[2]) ret[i]<-FALSE
         }

         if(abs(p.bbox$x[1]-p.bbox$x[2])>eps){
            if(ip$x>p.bbox$x[1] | ip$x<p.bbox$x[2]) ret[i]<-FALSE
         }

         if(abs(e.bbox$y[1]-e.bbox$y[2])>eps){
            if(ip$y>e.bbox$y[1] | ip$y<e.bbox$y[2]) ret[i]<-FALSE
         }

         if(abs(p.bbox$y[1]-p.bbox$y[2])>eps){
            if(ip$y>p.bbox$y[1] | ip$y<p.bbox$y[2]) ret[i]<-FALSE
         }

      }

#DEBUG
#if(ret[i]){
#   lines(x=e.bbox$x,y=rep(e.bbox$y[1],2),col="green",lwd=4)
#   lines(x=e.bbox$x,y=rep(e.bbox$y[2],2),col="green",lwd=4)
#   lines(x=rep(e.bbox$x[1],2),y=rep(e.bbox$y),col="green",lwd=4)
#   lines(x=rep(e.bbox$x[2],2),y=rep(e.bbox$y),col="green",lwd=4)
#   a<-scan()
#}

   }
   return(ret)

}

