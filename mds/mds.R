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
# squared Euclidean distances
dm<-function(x,y){
   # create X matrix
   X<-cbind(x,y)
   # create B
   B<-X%*%t(X)
   
   # now create a matrix of squared Euclidean distances
   # create a blank matrix to push results into
   D<-matrix(NA,dim(B)[1],dim(B)[2])
   # fill in the gaps
   for (r in 1:dim(B)[1]){
      for (s in 1:dim(B)[1]){
         # formula from Chatfield and Collins, eqn 10.8
         D[r,s]<-B[r,r]+B[s,s]-2*B[r,s]
      }
   }

   return(D)
}

distmatrix<-dm(v,w)
plot(cmdscale(D,2),asp=1,pch=".",cex=3)


samp<-data.frame(v=,w=,y=)










