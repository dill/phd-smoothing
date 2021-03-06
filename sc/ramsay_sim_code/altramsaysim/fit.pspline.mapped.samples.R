# run the prediction simulation with the defaults.
# with comparison to soap

library(soap)

fsb <- list(fs.boundary())

# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

# load the true function
source("../ramsay.alt.R")

## truth
tru <- ramsay.alt(xx,yy) 

# read in the predicition grid from matlab
predback.real<-read.csv("../matlab/preal.csv",header=F)
predback.imag<-read.csv("../matlab/pimag.csv",header=F)
prediction.grid<-data.frame(v=predback.real[[1]],w=predback.imag[[1]])


# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names


# log what's going on
sink(file="pspline.simrun.log")


### create the knots for the p-spline
# they need to be wider than the data so that we can do prediction over the
# whole grid.
knots.sc<-list(v=seq(min(prediction.grid$v),max(prediction.grid$v)
               ,length.out=4),w=seq(min(prediction.grid$w),
               max(prediction.grid$w),length.out=8))

v.spacing<-abs(knots.sc$v[2]-knots.sc$v[1])
w.spacing<-abs(knots.sc$w[2]-knots.sc$w[1])
   
knots.sc$v<-unique(sort(c(seq(from=min(knots.sc$v),by=-v.spacing,length.out=4) ,
               knots.sc$v,seq(from=max(knots.sc$v),by=v.spacing,length.out=4))))
knots.sc$w<-unique(sort(c(seq(from=min(knots.sc$w),by=-w.spacing,length.out=4) ,
               knots.sc$w,seq(from=max(knots.sc$w),by=w.spacing,length.out=4))))


# knots for the soap film
knots <- data.frame(v=rep(seq(-.5,3,by=.5),4),
                      w=rep(c(-.6,-.3,.3,.6),rep(8,4)))





# how many times?
n.samples<-1000

# object to store the mses
mses<-list(mapped=c(),soap=c())

for(i in 1:n.samples){

   # load the original data set
   orig.data<-read.csv(paste("ramsey-",i,".csv",sep=""),header=T)


   # load the mapped data set
   mapped.data<-read.csv(paste("ramsey-mapped-",i,".csv",sep=""),header=F)
   # add in the y column
   mapped.data<-cbind(orig.data[[1]],mapped.data)
   # correct titles
   names(mapped.data) <- c("y","v","w")


   ### sc code
   # fit with sc
   # using the p-spline basis

   # ie. m[1]
   pspline.order<-2
   b.mapped<-gam(y~te(v,w,bs="ps",m=pspline.order,k=c(6,10)),data=mapped.data,knots=knots.sc)
   # get predictions
   fv.mapped <- predict(b.mapped,prediction.grid)
   # get rid of the points that are not in the grid
   fv.mapped[!insiders]<-NA
   # calculate the MSE
   mses$mapped<-c(mses$mapped,mean((tru-fv.mapped)^2,na.rm=T))


   # fit with soap
   b.soap <- gam(y~s(v,w,k=40,bs="so",xt=list(bnd=fsb)),data=orig.data,knots=knots)
   # get predictions
   fv.soap <- predict(b.soap,newdata=data.frame(v=xx,w=yy),block.size=-1)

   # calculate the MSE
   mses$soap<-c(mses$soap,mean((tru-fv.soap)^2,na.rm=T))



}

cat("got to the end!\n")

cat("mses$mapped mean is",mean(mses$mapped),"\n")
cat("mses$soap mean is",mean(mses$soap),"\n")


write.csv(file="pspline.results.txt",mses)

cat("written file!\nEND\n")

sink(file=NULL)







