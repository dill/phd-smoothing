# just do the fit for the soap film when the boundary is known


library(soap)


# need to create the prediction grid here!!!


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


# log what's going on
sink(file="simrun.log")


# how many times?
n.samples<-1000

# object to store the mses
mses<-list(soap=c(),mapped=c())


#### IGNORE ABOVE HERE ########



## Do a known boundary example (note no `k' needed)
## First define the value for the smooth at each 
## supplied boundary point...
fsb[[1]]$f <- fs.test(fsb[[1]]$v,fsb[[1]]$w,b=1,exclude=FALSE)


bnd.mse<-c()

for(i in 1:n.samples){

   # load the original data set
   orig.data<-read.csv(paste("ramsey-",i,".csv",sep=""),header=T)


   ### soap code
   # fit with soap
   bk <- gam(y~s(v,w,bs="so",xt=list(bnd=fsb)),data=orig.data,knots=knots)

   # get predictions
   fv.soap <- predict(bk,newdata=data.frame(v=xx,w=yy),block.size=-1)

   # calculate the MSE
   bnd.mse<-c(bnd.mse,mean((tru-fv.soap)^2,na.rm=T))

}





#### ignore this vvvv #########
cat("got to the end!\n")

cat("mses$mapped mean is",mean(mses$mapped),"\n")
cat("mses$soap mean is",mean(mses$soap),"\n")


write.csv(file="results.file.txt",mses)

cat("written file!\nEND\n")

sink(file=NULL)







