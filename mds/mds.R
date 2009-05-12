# Multidimensional scaling approach...

# let's see what happens

# load soap
library(soap)

## create a boundary...
fsb <- list(fs.boundary())

## Simulate some fitting data, inside boundary...
n<-1000
v <- runif(n)*5-1;w<-runif(n)*2-1
y <- fs.test(v,w,b=1)
ind <- inSide(fsb,x=v,y=w) ## remove outsiders
y <- y[ind];v <- v[ind]; w <- w[ind]
n <- length(y)
y <- y + rnorm(n)*.3 ## add noise

names(fsb[[1]]) <- c("v","w") ## correct boundary names


# create the distance matrix
create_distance_matrix<-function(x,y){
   # requires the library soap   

   # make sure that x and y are the same length
   if(length(x)!=length(y){
      return(FALSE)
   }

   # create a matrix to hold the distances
   D<-matrix(0,length(x),length(x))


   # first find those points that we can use Euclidean
   # distance for
   for(i in 1:length(x)){
      # create sequence
      xseq<-seq(


      # see if any of the points in the sequence are not inside
      # add to list of non-inside points

      # insert the distance if they are inside

   }

   # calculate the Euclidean distance


   # calculate the convex hull distances



   # create the lower triangle of the matrix
   D<-D+t(D)-diag(D) 

   return(D)

}


plot(cmdscale(D,2),asp=1,pch=".",cex=3)












