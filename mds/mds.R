# Multidimensional scaling approach...

# let's see what happens
source("utils.R")
source("mds-wrap.R")
source("wood.R")
#source("insert.mds.R")

# create the distance matrix
create_distance_matrix<-function(xpoints,ypoints,bnd,logfile=NA){
   # requires the library soap   
   # args:
   #  xpoints,ypoints      data points
   #  bnd                  boundary list(x=c(),y=c())
   #  res                  resolution to test if line is inside

   # make sure that x and y are the same length
   if(length(xpoints)!=length(ypoints)){
      cat("ERROR: Vector lengths not the same!\n")
      return(FALSE)
   }
   if(any(is.na(xpoints))|any(is.na(ypoints))){
      cat("ERROR: Some elements are NA!\n")
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
### DEBUG
#cat("x=c(",p1$x,",",p2$x,")\n")
#cat("y=c(",p1$y,",",p2$y,")\n")


         # if there are any intersections of the line p1p2 with 
         # any boundary side
         intp<-do_intersect(p1,p2,bnd)
         if(sum(intp)>1){

            # call the R Wood algorithm
            oldpath<-wood_path(p1,p2,bnd)
            
plot(bnd,type="l")
lines(oldpath,lwd=2,col="red")
scan()
            # C version
            D[i,j]<-woodpath(p1,p2,bnd)


#            if(any(is.na(path))){
#               D[i,j]<-NA
#            }else{
               # find the length of the path
               op<-hull_length(oldpath)
#            }

         # if the line p1p2 doesn't intersect any sides
         }else{
            # insert the distance
            D[i,j]<-sqrt((p1$x-p2$x)^2+(p1$y-p2$y)^2)
         }
### DEBUG
#cat(".")


#   if(abs(D[i,j]-op)>0.01) cat("yelp!\n")

   
      }
      # if asked to we write out a log file at each line
      if(!is.na(logfile)){
         write.csv(D,file=logfile)
      }
### DEBUG
#cat("\ndone",i,"!\n")
   }
   # create the lower triangle of the matrix
   # NB. diagonal should be 0
   D<-D+t(D) 
   return(D)
}


