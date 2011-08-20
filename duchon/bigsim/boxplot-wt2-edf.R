# make some boxplots
library(ggplot2)

#pdf(file="wt2-boxplot-duchon.pdf",width=13.3,height=7.88)

# make the text look better for printout
par(cex.axis=0.75,las=1,mgp=c(2,0.75,0),mar=c(2,3,1,1))

# stick all of the edfs into one matrix
edfs<-matrix(NA,100,0)

# model names
mod.names<-c("GCV","ML")

# duchon data - GCV.Cp
ddat<-read.csv(file="bigresults-GCV.Cp.csv")
names(ddat)<-c("n","noise","sim","name","dat")
ddat$noise<-as.factor(ddat$noise)
ddat.mse<-ddat$dat[ddat$name=="edf"]
ddat.noise<-ddat$noise[ddat$name=="edf"]
# duchon data - GCV.Cp
ddat.ML<-read.csv(file="bigresults-ML.csv")
names(ddat.ML)<-c("n","noise","sim","name","dat")
ddat.ML$noise<-as.factor(ddat.ML$noise)
ddat.ML.mse<-ddat.ML$dat[ddat.ML$name=="edf"]
ddat.ML.noise<-ddat.ML$noise[ddat.ML$name=="edf"]

# push the data into the right shape
for(err.lev in c("0.35","0.9","1.55")){

   cols<-c()

   edf<-cbind(ddat.mse[ddat.noise==err.lev],
              ddat.ML.mse[ddat.ML.noise==err.lev])
#   edf<-edf[,-c(1,2)]
   # 3 spurious soap results, remove them
   spurious<-c(111,135,157)
   edf<-edf[-spurious,]

   # extra Wilcoxon test stuff
   test.against<-2
   for(i in 1:2){
      if(i!=test.against){
         pv<-wilcox.test(edf[,i],edf[,test.against],paired=TRUE)$p.value
         med<-median(edf[,i]-edf[,test.against])
         if(pv<0.01 & med>0){
            cols<-c(cols,rep("red",nrow(edf)))
         }else if(pv<0.01 & med<0){
            cols<-c(cols,rep("green",nrow(edf)))
         }else{
            cols<-c(cols,rep("white",nrow(edf)))
         }
      }else{
         cols<-c(cols,rep("white",nrow(edf)))
      }
   }

   colnames(edf)<-mod.names
   edf<-melt(edf)
   edf<-edf[,-1]
   names(edf)<-c("method","edf")
   errs<-data.frame(error=rep(err.lev,nrow(edf)))
   cols<-data.frame(cols=cols)
   edf<-cbind(edf,errs,cols)
   edfs<-rbind(edfs,edf)

}

# do the plot
theme_set(theme_bw())
p<-ggplot(edfs)
p<-p+geom_boxplot(aes(x=factor(method),y=edf,fill=cols))
p<-p+facet_wrap(~error,nrow=1)
p<-p+scale_fill_manual(value = c("green","red","white"),legend=FALSE) 
p<-p+opts(panel.grid.major=theme_blank(), 
                    panel.grid.minor=theme_blank(), 
                    panel.background=theme_rect())
p<-p+labs(x="Method",y="EDF")
print(p)

#dev.off()
