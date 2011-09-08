# R code to generate the BOXPLOTS
# for normal Ramsay simulations


library(ggplot2)

## First the default settings from soap-package help file
# noise = 0.3, n=1000


# sc+ps 
dat<-read.csv(paste("ramsay/pspline.results.txt",sep=""))
dat<-cbind(dat,rep("sc+ps",nrow(dat)))
dat<-dat[,-1]
big.dat<-dat
names(big.dat)<-c("mse","model")

# soap
dat<-read.csv(paste("ramsay/results.file.txt",sep=""))
dat<-cbind(dat,rep("soap",nrow(dat)))
dat<-dat[,-1]
dat<-dat[,-2]
names(dat)<-c("mse","model")
big.dat<-rbind(big.dat,dat)

# sc+tp
dat<-read.csv(paste("ramsay/tp-noisey-0.5.results.txt",sep=""))
dat<-cbind(dat,rep("sc+tp",nrow(dat)))
dat<-dat[,-1]
names(dat)<-c("mse","model")
big.dat<-rbind(big.dat,dat)

# tprs
dat<-read.csv(paste("ramsay/tp-0.3-1000.results.txt",sep=""))
dat<-cbind(dat,rep("tprs",nrow(dat)))
dat<-dat[,-1]
names(dat)<-c("mse","model")
big.dat<-rbind(big.dat,dat)

# add the extra columns
big.dat<-cbind(big.dat,
               n=rep(1000,nrow(big.dat)),
               noise=rep(0.3,nrow(big.dat)))


## Different sample sizes
sample.sizes<-c(500,250,100)

for(my.size in sample.sizes){

   dat<-read.csv(paste("ramsay/sample.size.",my.size,".results.txt",sep=""))
   dat<-dat[,-1]
   dat<-c(dat[,1],dat[,2])
   dat<-data.frame(mse=dat,
                   model=c(rep("soap",length(dat)/2),
                           rep("sc+ps",length(dat)/2)))

   
   dat1<-read.csv(paste("ramsay/tp-sample.size.",my.size,".results.txt",sep=""))
   dat1<-data.frame(mse=dat1[,2],model=rep("sc+tp",nrow(dat1)))

   dat2<-read.csv(paste("ramsay/tp-0.3-",my.size,".results.txt",sep=""))
   dat2<-data.frame(mse=dat2[,2],model=rep("tprs",nrow(dat2)))

   dat<-rbind(dat,dat1,dat2)
   dat<-cbind(dat,
              n=rep(my.size,nrow(dat)),
              noise=rep(0.3,nrow(dat)))

   big.dat<-rbind(big.dat,dat)

}


## different error levels
error.levels<-c(0.5,1,2)

for(my.error in error.levels){
   # read in the table
   dat<-read.csv(paste("ramsay/noisey-",my.error,".results.txt",sep=""))
   dat<-dat[,-1]
   dat<-c(dat[,1],dat[,2])
   dat<-data.frame(mse=dat,
                   model=c(rep("soap",length(dat)/2),
                           rep("sc+ps",length(dat)/2)))

   names(dat)<-c("mse","model")

   # read in the tp stuff    
   this.file<-read.csv(paste("ramsay/tp-noisey-",my.error,".results.txt",sep=""))
   dat1<-data.frame(mse=dat1[,2],model=rep("sc+tp",nrow(dat1)))

   this.file<-read.csv(paste("ramsay/tp-",my.error,"-1000.results.txt",sep=""))
   dat2<-data.frame(mse=dat2[,2],model=rep("tprs",nrow(dat2)))

   dat<-rbind(dat,dat1,dat2)
   dat<-cbind(dat,
              n=rep(1000,nrow(dat)),
              noise=rep(my.error,nrow(dat)))

   big.dat<-rbind(big.dat,dat)

}

big.dat$mse<-as.numeric(big.dat$mse)

p<-ggplot(big.dat)
p<-p+geom_boxplot(aes(y=mse,x=model))
p<-p+facet_wrap(noise~n)


