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

# knots for soap
knots <- data.frame(v=rep(seq(-.5,3,by=.5),4),
                    w=rep(c(-.6,-.3,.3,.6),rep(8,4)))


# knots for the p-splines
knots.sc<-list(v=seq(min(prediction.grid$v),max(prediction.grid$v)
               ,length.out=4),w=seq(min(prediction.grid$w),
               max(prediction.grid$w),length.out=8))

v.spacing<-abs(knots.sc$v[2]-knots.sc$v[1])
w.spacing<-abs(knots.sc$w[2]-knots.sc$w[1])

knots.sc$v<-unique(sort(c(seq(from=min(knots.sc$v),by=-v.spacing,length.out=4) ,
               knots.sc$v,seq(from=max(knots.sc$v),by=v.spacing,length.out=4))))
knots.sc$w<-unique(sort(c(seq(from=min(knots.sc$w),by=-w.spacing,length.out=4) ,
               knots.sc$w,seq(from=max(knots.sc$w),by=w.spacing,length.out=4))))



# log what's going on
sink(file="small.sample.simrun.log")


# how many times?
n.samples<-1000


# this time we take a sample of size sample.sizes[i] from
# the data frame 
sample.sizes<-c(500,250,100)

for(j in c(1:3)){

   # blank the storage before we start
   mses<-list(soap=c(),mapped=c())

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
   
      ### soap code
      # fit with soap
      b.soap <- gam(y~s(v,w,k=40,bs="so",xt=list(bnd=fsb)),data=orig.data,knots=knots)
   
      # get predictions
      fv.soap <- predict(b.soap,newdata=data.frame(v=xx,w=yy),block.size=-1)
   
      # calculate the MSE
      mses$soap<-c(mses$soap,mean((tru-fv.soap)^2,na.rm=T))
   
   
      ### sc code
      # fit with sc
      # ie. m[1]
      pspline.order<-2
      b.mapped<-gam(y~te(v,w,bs="ps",m=pspline.order,k=c(6,10)),data=mapped.data,knots=knots.sc)
  
      # get predictions
      fv.mapped <- predict(b.mapped,prediction.grid)
   
      # get rid of the points that are not in the grid
      fv.mapped[!insiders]<-NA
    
      # calculate the MSE
      mses$mapped<-c(mses$mapped,mean((tru-fv.mapped)^2,na.rm=T))
   
   }

   cat("got to the end!\n")

   cat("mses$mapped mean for sample size",sample.size,"is",mean(mses$mapped),"\n")
   cat("mses$soap mean for sample size",sample.size,"is",mean(mses$soap),"\n")

   write.csv(file=paste("sample.size.",sample.size,".results.txt",sep=""),mses)

   cat("written file!\nEND\n")

}


   

sink(file=NULL)







