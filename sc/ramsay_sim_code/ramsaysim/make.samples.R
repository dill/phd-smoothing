# Make the samples and write the csv files

#noise.level<-0.3
#samp.size<-1000

# load soap
library(soap)

## create a boundary...
fsb <- list(fs.boundary())
#names(fsb[[1]]) <- c("v","w") ## correct boundary names

# how many times?
n.samples<-1000


for(i in 1:n.samples){
   ## Simulate some fitting data, inside boundary...
   n<-samp.size
   v <- runif(n)*5-1;w<-runif(n)*2-1
   y <- fs.test(v,w,b=1)
   ind <- inSide(fsb,x=v,y=w) ## remove outsiders
   y <- y[ind];v <- v[ind]; w <- w[ind]
   n <- length(y)
   y <- y + rnorm(n)*noise.level ## add noise
   data.out<-as.matrix(cbind(y,v,w))
   write.csv(data.out,file=paste("ramsey-",i,".csv",sep=""),
            row.names=FALSE)
}

