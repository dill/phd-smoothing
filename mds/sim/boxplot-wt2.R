# make some boxplots - now with added ggplot2!


library(ggplot2)

pdf(file="big-mds-wt2-boxplot.pdf",width=10,height=5)

# make the text look better for printout
par(cex.axis=0.75,las=1,mgp=c(2,0.75,0),mar=c(2,3,1,1))

# stick all of the results into one matrix
mses<-c()

# model names
mod.names<-c("tprs","mds\n+tp","mds\n+cr","mds\n 3D","mds\n+adj","soap")

cols<-c()
for(err.lev in c("0.35","0.9","1.55")){

   mse<-read.csv(paste("wt2-mse-250-",err.lev,".csv",sep=""))
   mse<-mse[,-1]
   names(mse)<-mod.names
   mse<-mse[1:200,] # why???
spurious<-c(111,135,157)
mse<-mse[-spurious,]

   mse<-melt(mse)
   mse<-cbind(mse,rep(err.lev,dim(mse)[1]))

   mses<-rbind(mses,mse)

   ## extra Wilcoxon test stuff
   #for(i in 1:5){
   #   pv<-wilcox.test(mse[,6],mse[,i],paired=TRUE)$p.value
   #   med<-median(mse[,i]-mse[,6])
   #   if(pv<0.01 & med>0){
   #      cols<-c(cols,"red")
   #   }else if(pv<0.01 & med<0){
   #      cols<-c(cols,"green")
   #   }else{
   #      cols<-c(cols,"white")
   #   }
   #   cat("soap vs. ",mod.names[i],pv,"\n")
   #}
   #cols<-c(cols,"white")

}


# 3 spurious soap results, remove them
#spurious<-c(111,135,157)
#mses<-mses[-spurious,]

# some names
names(mses)<-c("method","mse","noise")

# log the results
mses$mse<-log(mses$mse)


# do the plot
#boxplot(mses,main="",names=rep(mod.names,3),
#      xlab="",
#      col=cols,
#      medlwd=1,
#      ylab="log(mean MSE per realisation)")

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
