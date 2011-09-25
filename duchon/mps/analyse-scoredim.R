# what happened in the free votes sim?
library(ggplot2)
library(mgcv)

# plotting options
theme_set(theme_bw())

bigdat<-c()
big.mins<-c()

for(samp.size in c(200,300,400,500)){
   load(paste("freesim-",samp.size,".RData",sep=""))


   names(mds.dim.sel)<-c("dim","score","method","samp.size","sim")

   min.gcvs<-c()#data.frame(dim=NA,score=NA,sim=NA,method=NA)
   for(method in c("ML","GCV.Cp")){
      for(i in 1:200){
         tmp<-mds.dim.sel[mds.dim.sel$sim==i & mds.dim.sel$method==method,]
         min.gcvs<-rbind(min.gcvs,
                       cbind(tmp$dim[which.min(tmp$score)],
                             min(tmp$score,na.rm=T),
                             i,samp.size))
      }
   }
   min.gcvs<-as.data.frame(min.gcvs)
   min.gcvs<-cbind(min.gcvs,c(rep("ML",200),rep("GCV.Cp",200)))
   names(min.gcvs)<-c("dim","score","sim","samp.size","method")

   # put everything into big data frames
   bigdat<-rbind(bigdat,mds.dim.sel)
   big.mins<-rbind(big.mins,min.gcvs)

}

names(bigdat)<-c("dim","score","method","samp.size","sim")
names(big.mins)<-c("dim","score","sim","samp.size","method")

bigdat$method <-as.character(bigdat$method)
big.mins$method<-as.character(big.mins$method)

bigdat$method[bigdat$method=="GCV.Cp"]<-"GCV"
big.mins$method[big.mins$method=="GCV.Cp"]<-"GCV"

bigdat$method <-as.factor(bigdat$method)
big.mins$method<-as.factor(big.mins$method)

p<-ggplot(bigdat)
p<-p+geom_line(aes(x=dim,y=score,group=sim),alpha=0.3)
p<-p+stat_smooth(aes(x=dim,y=score),alpha=0.3,fill="green",method="gam",formula=y~s(x))
p<-p+geom_point(aes(x=dim,y=score),size=1.5,colour="red",data=big.mins)
p<-p+labs(x="MDS projection dimension",y="Score")
p<-p+facet_grid(method~samp.size,scales="free")
p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          legend.background=theme_blank(),
          legend.key=theme_blank(),
          panel.background=theme_rect())
print(p)

### ggsave!
ggsave("mps-dimselect.pdf")



