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


mse<-cbind(big.dat$mse[1:1000],
           big.dat$mse[1001:2000],
           big.dat$mse[2001:3000],
           big.dat$mse[3001:4000])

cols<-c()
test.against<-2 # soap
for(i in 1:4){
   if(i!=test.against){
      pv<-wilcox.test(mse[,test.against],mse[,i],paired=TRUE)$p.value
      med<-median(mse[,i]-mse[,test.against])
      if(pv<0.01 & med>0){
         cols<-c(cols,rep("red",nrow(mse)))
      }else if(pv<0.01 & med<0){
         cols<-c(cols,rep("green",nrow(mse)))
      }else{
         cols<-c(cols,rep("white",nrow(mse)))
      }
   }else{
      cols<-c(cols,rep("white",nrow(mse)))
   }
}



# add the extra columns
big.dat<-cbind(big.dat,
               n=rep(1000,nrow(big.dat)),
               cols=cols,
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

   mse<-cbind(dat$mse[1:1000],dat$mse[1001:2000],dat1[,1],dat2[,1])
   cols<-c()
   test.against<-1 # soap
   for(i in 1:4){
      if(i!=test.against){
         pv<-wilcox.test(mse[,test.against],mse[,i],paired=TRUE)$p.value
         med<-median(mse[,i]-mse[,test.against])
         if(pv<0.01 & med>0){
            cols<-c(cols,rep("red",nrow(mse)))
         }else if(pv<0.01 & med<0){
            cols<-c(cols,rep("green",nrow(mse)))
         }else{
            cols<-c(cols,rep("white",nrow(mse)))
         }
      }else{
         cols<-c(cols,rep("white",nrow(mse)))
      }
   }

   dat<-cbind(dat,
              n=rep(my.size,nrow(dat)),
              cols=cols,
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
   dat1<-read.csv(paste("ramsay/tp-noisey-",my.error,".results.txt",sep=""))
   dat1<-data.frame(mse=dat1[,2],model=rep("sc+tp",nrow(dat1)))

   dat2<-read.csv(paste("ramsay/tp-",my.error,"-1000.results.txt",sep=""))
   dat2<-data.frame(mse=dat2[,2],model=rep("tprs",nrow(dat2)))

   dat<-rbind(dat,dat1,dat2)

   mse<-cbind(dat$mse[1:1000],dat$mse[1001:2000],dat1[,1],dat2[,1])
   cols<-c()
   test.against<-1 # soap
   for(i in 1:4){
      if(i!=test.against){
         pv<-wilcox.test(mse[,test.against],mse[,i],paired=TRUE)$p.value
         med<-median(mse[,i]-mse[,test.against])
         if(pv<0.01 & med>0){
            cols<-c(cols,rep("red",nrow(mse)))
         }else if(pv<0.01 & med<0){
            cols<-c(cols,rep("green",nrow(mse)))
         }else{
            cols<-c(cols,rep("white",nrow(mse)))
         }
      }else{
         cols<-c(cols,rep("white",nrow(mse)))
      }
   }

   dat<-cbind(dat,
              n=rep(1000,nrow(dat)),
              cols=cols,
              noise=rep(my.error,nrow(dat)))

   big.dat<-rbind(big.dat,dat)

}

big.dat$mse<-as.numeric(big.dat$mse)

theme_set(theme_bw())
p<-ggplot(big.dat)
p<-p+geom_boxplot(aes(y=log(mse),x=model,fill=cols),outlier.size=1)
p<-p+facet_wrap(noise~n,ncol=4)#,scales="free_y")
p<-p+scale_fill_manual(value = c("green","red","white"),legend=FALSE)
p<-p+opts(panel.grid.major=theme_blank(),
                    panel.grid.minor=theme_blank(),
                    panel.background=theme_rect())
p<-p+labs(x="Method",y="log(mean MSE per realisation)")
print(p)

ggsave(file="ramsay-boxplot.pdf",height=4,width=8)
