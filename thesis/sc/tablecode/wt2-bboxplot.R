# make some boxplots for sc wt2 with bounding box

library(ggplot2)

big.dat<-data.frame(mse=NA,model=NA,cols=NA,n=NA,noise=NA)

for (samp.size in c(1000,500)){
   for(noise.level in c(0.02, 0.005)){

      dat.1<-read.csv(paste("wigglytop2/mses-",samp.size,"-",noise.level,".txt",sep=""))
      dat.2<-read.csv(paste("wigglytop2-bbox/bboxmses-",samp.size,"-",noise.level,".txt",sep=""))

      dat.2<-dat.2[1:500,]

      if(length(which(dat.1$mse>1000)>0)){
         ind<-which(dat.1$mse>1000)
         dat.1<-dat.1[-c(ind,ind-500,ind-1000),]
         dat.2<-dat.2[-(ind-1000),]
      }

      
      dat<-rbind(dat.1,dat.2)

      mse<-cbind(dat$mse[dat$labs=="SC+TPRS"],
                 dat$mse[dat$labs=="TPRS"],
                 dat$mse[dat$labs=="soap"],
                 dat$mse[dat$labs=="SC+TPRSbbox"])

      cols<-c()
      test.against<-3 # soap
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

      dat<-dat[,-1]
      dat<-data.frame(mse=dat[,1],
                      model=dat[,2],
                      cols=cols,
                      n=rep(samp.size,length(cols)),
                      noise=rep(noise.level,length(cols)))

      big.dat<-rbind(big.dat,dat)

   }
}

big.dat<-big.dat[-1,]

big.dat$model<-as.factor(big.dat$model)
levels(big.dat$model)<-c("sc+tprs","sc+tprs\nbox","soap","tprs")

theme_set(theme_bw())
p<-ggplot(big.dat)
p<-p+geom_boxplot(aes(y=log(mse),x=model,fill=cols),outlier.size=1)
p<-p+facet_wrap(noise~n,ncol=2)
p<-p+scale_fill_manual(value = c("red","white","green"),legend=FALSE)
p<-p+opts(panel.grid.major=theme_blank(),
                    panel.grid.minor=theme_blank(),
                    panel.background=theme_rect())
p<-p+labs(x="Method",y="log(mean MSE per realisation)")
print(p)

ggsave(file="wt2-boxplot.pdf",height=5,width=5)
