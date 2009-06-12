# Multidimensional scaling approach...

# let's see what happens
source("utils.R")
source("wood.R")

# create the distance matrix
create_distance_matrix<-function(xpoints,ypoints,bnd){
   # requires the library soap   
   # args:
   #  xpoints,ypoints      data points
   #  bnd                  boundary list(x=c(),y=c())
   #  res                  resolution to test if line is inside

   # make sure that x and y are the same length
   if(length(xpoints)!=length(ypoints)){
      return(FALSE)
   }

   # create a matrix to hold the distances
   D<-matrix(0,length(xpoints),length(xpoints))

   # iterate over all of the pairs of points, only calculate the
   # upper diagonal of the matrix, since it's symmetric
   for(i in 1:(length(xpoints)-1)){
      p1<-list(x=xpoints[i],y=ypoints[i])
      for(j in (i+1):length(ypoints)){
         p2<-list(x=xpoints[j],y=ypoints[j])

         # if there are any intersections of the line p1p2 with 
         # any boundary side
cat("i=",i,"j=",j,"\n")
         intp<-do_intersect(p1,p2,bnd)
         if(sum(intp)>1){

            # call the Wood algorithm
            path<-wood_path(p1,p2,bnd)

            # find the length of the path
            D[i,j]<-hull_length(path)

         # if the line p1p2 doesn't intersect any sides
         }else{
            # insert the distance
            D[i,j]<-sqrt((p1$x-p2$x)^2+(p1$y-p2$y)^2)
         }
### DEBUG
cat(".")
      }
### DEBUG
cat("\ndone",i,"!\n")
   }
   # create the lower triangle of the matrix
   # NB. diagonal should be 0
   D<-D+t(D) 
   return(D)
}


