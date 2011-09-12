# make some EDF boxplots

library(ggplot2)


# ordering is mds, soap, tprs

mses<-c()

for(err.lev in c("0.1","1","10")){

   mse<-read.csv(paste("ramsay-edf-250-",err.lev,".csv",sep=""))
   mse<-mse[,-1]

#   # Wilcoxon
#   cols<-c()
#   test.against<-2 # soap
#   for(i in 1:3){
#      if(i!=test.against){
#         pv<-wilcox.test(mse[,test.against],mse[,i],paired=TRUE)$p.value
#         med<-median(mse[,i]-mse[,test.against])
#         if(pv<0.01 & med>0){
#            cols<-c(cols,rep("red",nrow(mse)))
#         }else if(pv<0.01 & med<0){
#            cols<-c(cols,rep("green",nrow(mse)))
#         }else{
#            cols<-c(cols,rep("white",nrow(mse)))
#         }
#      }else{
#         cols<-c(cols,rep("white",nrow(mse)))
#      }
#   }
#   ####

   mse<-melt(mse)
   mse<-cbind(mse,rep(err.lev,dim(mse)[1]))#,cols)

   mses<-rbind(mses,mse)
}

names(mses)<-c("method","mse","noise")#,"cols")


theme_set(theme_bw())

p<-ggplot(mses)
p<-p+stat_boxplot(aes(x=method,y=mse))#,fill=cols))
p<-p+facet_wrap(~noise,nrow=1)
p<-p+opts(panel.grid.major=theme_blank(),
                    panel.grid.minor=theme_blank(),
                    panel.background=theme_rect())
p<-p+scale_fill_manual(value = c("red","white","green"),legend=FALSE)
p<-p+labs(x="Method",y="EDF")
print(p)


ggsave(file="edf-mds-ramsay-boxplot.pdf",width=6,height=5)
