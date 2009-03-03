# Make the samples and write the csv files


# load soap
library(soap)

## create a boundary...
fsb <- list(fs.boundary())
#names(fsb[[1]]) <- c("v","w") ## correct boundary names

# how many times?
n.samples<-1000

# already have the v,w coordinates, so just load the file 
# and then modify the f value according to the function.

# No need to do this for both the mapped and unmapped, since
# the unmapped only has the x,y (v,w) coordinates in it.

# source in the function that has the alternate
# Ramsay figure. Based on fs.test from soap.
source("../ramsay.alt.R")


for(i in 1:n.samples){
   # read in the unmapped data 
   unmapped.data<-read.csv(paste("ramsey-",i,".csv",sep=""))

   y <- ramsay.alt(unmapped.data$v,unmapped.data$w,b=1)

   # shouldn't need to do this?
   #ind <- inSide(fsb,x=v,y=w) ## remove outsiders
   #y <- y[ind];v <- v[ind]; w <- w[ind]

   n <- length(y)
   y <- y + rnorm(n)*.3 ## add noise
   data.out<-as.matrix(cbind(y,unmapped.data$v,unmapped.data$w))

   # write back over the file.
   write.csv(data.out,file=paste("ramsey-",i,".csv",sep=""),
            row.names=FALSE)
}

