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
create_distance_matrix<-function(x,y,bnd,res=100){
   # requires the library soap   
   # args:
   #  x,y      data points
   #  bnd      boundary list(x=c(),y=c())
   #  res      resolution to test if line is inside

   # make sure that x and y are the same length
   if(length(x)!=length(y){
      return(FALSE)
   }

   # create a matrix to hold the distances
   D<-matrix(0,length(x),length(x))

   ch.list<-list(i=c(),j=c())

   # first find those points that we can use Euclidean
   # distance for
   for(i in 1:length(x)){
      p1<-c(x[i],y[i])
      for(j in i:length(y)){
         p2<-c(x[j],y[j])

         # create sequence
         xseq<-seq(p1[1],p2[1],res)
         yseq<-seq(p2[2],p2[2],res)
  
         # see if any of the points in the sequence are not inside
         inside<-inSide(bnd,xseq,yseq) 

         # if the line is not all inside the polygon
         if(any(inside==FALSE)){
            
            # create a list of points to create the hull from
            


            # calculate the convex hull distances
         

         }else{
            # insert the distance if they are inside
            D[i,j]<-sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)
         }

      }
   }

   for(i in 1:length(ch.list$i)){
      
   }


   # create the lower triangle of the matrix
   D<-D+t(D)-diag(D) 

   return(D)

}





plot(cmdscale(D,2),asp=1,pch=".",cex=3)












