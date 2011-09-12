# make some boxplots - now with added ggplot2!
# EDF version!!!
# this is the first fig with just mds+rs soap and tprs


library(ggplot2)

# stick all of the results into one matrix
mses<-c()

# model names
#mod.names<-c("tprs","mds\n+tp","mds\n+cr","mds\n 3D","mds\n+adj","soap")
mod.names<-c("tprs","mds\n+tp","mds\n 3D","mds\n+adj","soap")

cols<-c()
for(err.lev in c("0.35","0.9","1.55")){

   mse<-read.csv(paste("wt2-edf-250-",err.lev,".csv",sep=""))
   mse<-mse[,-1]
   mse<-mse[,-3]
   names(mse)<-mod.names
   mse<-mse[1:200,] # why???
   spurious<-c(111,135,157)
   mse<-mse[-spurious,]

   mse<-melt(mse)
   mse<-cbind(mse,rep(err.lev,dim(mse)[1]))

   mses<-rbind(mses,mse)

}


# some names
names(mses)<-c("method","mse","noise")

theme_set(theme_bw())

p<-ggplot(mses)
p<-p+stat_boxplot(aes(x=method,y=mse),outlier.size=1)
p<-p+facet_wrap(~noise,nrow=1)
p<-p+opts(panel.grid.major=theme_blank(), 
                    panel.grid.minor=theme_blank(), 
                    panel.background=theme_rect())
p<-p+labs(x="Method",y="EDF")
print(p)


ggsave(file="big-mds-wt2-boxplot-edf.pdf",width=8,height=5)
