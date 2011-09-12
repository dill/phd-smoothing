# make some boxplots - now with added ggplot2!
# this is the first fig with just mds+rs soap and tprs


library(ggplot2)


# make the text look better for printout
par(cex.axis=0.75,las=1,mgp=c(2,0.75,0),mar=c(2,3,1,1))

# stick all of the results into one matrix
mses<-c()

# model names
#mod.names<-c("tprs","mds\n+tp","mds\n+cr","mds\n 3D","mds\n+adj","soap")
mod.names<-c("tprs","mds\n+tp","mds\n 3D","mds\n+adj","soap")

cols<-c()
for(err.lev in c("0.35","0.9","1.55")){

   mse<-read.csv(paste("wt2-mse-250-",err.lev,".csv",sep=""))
   mse<-mse[,-1]
   mse<-mse[,-3]
   names(mse)<-mod.names
   mse<-mse[1:200,] # why???
   spurious<-c(111,135,157)
   mse<-mse[-spurious,]

   # remove 3d, adj
   mse<-mse[,-c(3,4)]

   # extra Wilcoxon test stuff
   for(i in 1:2){
      pv<-wilcox.test(mse[,3],mse[,i],paired=TRUE)$p.value
      med<-median(mse[,i]-mse[,3])
      if(pv<0.01 & med>0){
         cols<-c(cols,rep("red",nrow(mse)))
      }else if(pv<0.01 & med<0){
         cols<-c(cols,rep("green",nrow(mse)))
      }else{
         cols<-c(cols,rep("white",nrow(mse)))
      }
#      cat("soap vs. ",mod.names[i],pv,"\n")
   }
   cols<-c(cols,rep("white",nrow(mse)))

   mse<-melt(mse)
   mse<-cbind(mse,rep(err.lev,dim(mse)[1]))

   mses<-rbind(mses,cbind(mse,cols))

}


# some names
names(mses)<-c("method","mse","noise","col")

theme_set(theme_bw())

p<-ggplot(mses)
p<-p+stat_boxplot(aes(x=method,y=log(mse),fill=col),outlier.size=1)
p<-p+scale_fill_manual(value = c("red","white","green"),legend=FALSE)
p<-p+facet_wrap(~noise,nrow=1)
p<-p+opts(panel.grid.major=theme_blank(), 
                    panel.grid.minor=theme_blank(), 
                    panel.background=theme_rect())
p<-p+labs(x="Method",y="log(mean MSE per realisation)")
print(p)


ggsave(file="mds-wt2-boxplot.pdf",width=8,height=5)
