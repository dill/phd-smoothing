# noisey data test
# just thin plate case....
# we do this by just taking a sample from the data we already have
# on file and then adding more noise to the "heights" (ie. y value).

# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

## truth
tru <- fs.test(xx,yy) 

# read in the predicition grid from matlab
prediction.grid<-data.frame(v=xx,w=yy)

# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names

# how many times?
n.samples<-1000

# noise levels to try:
# in the other examples we have rnorm(n)*0.3, so we want more than that
#noise.levels<-c(0,0.5,1,2)

#for(j in c(1:4)){

   # blank the storage before we start
   mses<-list(tp=c())

   #noise.level<-noise.levels[j]

   #cat("noise level:",noise.level,"\n")

   for(i in 1:n.samples){

      # load the original data set
      orig.data<-read.csv(paste("ramsey-",i,".csv",sep=""),header=T)
   
      ### sc code
      # fit with sc+tp
      b.mapped<-gam(y~s(v,w),data=orig.data)
  
      # get predictions
      fv.mapped <- predict(b.mapped,prediction.grid)
   
      # get rid of the points that are not in the grid
      fv.mapped[!insiders]<-NA
    
      # calculate the MSE
      mses$tp<-c(mses$tp,mean((tru-fv.mapped)^2,na.rm=T))
   
   }

#   cat("got to the end!\n")

#   cat("mses$mapped mean for noise level",noise.level,"is",mean(mses$tpmapped),"\n")

   write.csv(file=paste(alt,"tp-",noise.level,"-",samp.size,".results.txt",sep=""),mses)

#   cat("written file!\nEND\n")

#}


   

#sink(file=NULL)

