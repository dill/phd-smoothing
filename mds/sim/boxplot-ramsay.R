# make some boxplots

library(ggplot2)

pdf(file="mds-ramsay-boxplot.pdf",width=6,height=5)

# ordering is mds, soap, tprs

mses<-c()

for(err.lev in c("0.1","1","10")){

   mse<-read.csv(paste("ramsay-mse-250-",err.lev,".csv",sep=""))
   mse<-mse[,-1]

   mse<-melt(mse)
   mse<-cbind(mse,rep(err.lev,dim(mse)[1]))

   mses<-rbind(mses,mse)
}

names(mses)<-c("method","mse","noise")

mses$mse<-log(mses$mse)

theme_set(theme_bw())

p<-ggplot(mses)
p<-p+stat_boxplot(aes(x=method,y=mse))
p<-p+facet_wrap(~noise,nrow=1)
p<-p+opts(panel.grid.major=theme_blank(),
                    panel.grid.minor=theme_blank(),
                    panel.background=theme_rect())
p<-p+labs(x="Method",y="log(mean MSE per realisation)")
print(p)


dev.off()
