# smaller sample size test
# we do this by just taking a sample from the data we already have
# on file.

library(soap)

## create the prediction grid 
fsb <- list(fs.boundary())

# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

## truth
tru <- fs.test(xx,yy) 

# read in the predicition grid from matlab
predback.real<-read.csv("../matlab/preal.csv",header=F)
predback.imag<-read.csv("../matlab/pimag.csv",header=F)
prediction.grid<-data.frame(v=predback.real[[1]],w=predback.imag[[1]])


# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names

# log what's going on
sink(file="tp.small.sample.simrun.log")

# how many times?
n.samples<-1000

# this time we take a sample of size sample.sizes[i] from
# the data frame 
sample.sizes<-c(500,250,100)

for(j in c(1:3)){

   # blank the storage before we start
   mses<-list(tpmapped=c())

   sample.size<-sample.sizes[j]

   cat("sample size:",sample.size,"\n")

   for(i in 1:n.samples){

      # load the original data set
      orig.data<-read.csv(paste("ramsey-",i,".csv",sep=""),header=T)
   
      # make the sample
      sampled.points<-sample(c(1:dim(orig.data)[1]),sample.size)
      orig.data<-orig.data[sampled.points,]
   
      # load the mapped data set
      mapped.data<-read.csv(paste("ramsey-mapped-",i,".csv",sep=""),header=F)
      # add in the y column
      mapped.data<-cbind(orig.data[[1]],mapped.data[sampled.points,])
      # correct titles
      names(mapped.data) <- c("y","v","w")
   
      ### sc code fit tp+sc
      b.mapped<-gam(y~s(v,w),data=mapped.data)
  
      # get predictions
      fv.mapped <- predict(b.mapped,prediction.grid)
   
      # get rid of the points that are not in the grid
      fv.mapped[!insiders]<-NA
    
      # calculate the MSE
      mses$tpmapped<-c(mses$tpmapped,mean((tru-fv.mapped)^2,na.rm=T))
   
   }

   cat("got to the end!\n")

   write.csv(file=paste("tp-sample.size.",sample.size,".results.txt",sep=""),mses)

   cat("written file!\nEND\n")

}


   

sink(file=NULL)







