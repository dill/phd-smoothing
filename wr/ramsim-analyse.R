# attempt to duplicate the simulation settings in 
# Wang and Ranalli (2007)
# Copyright David Lawrence Miller 2010
source("wr-wrapper.R")
library(ggplot2)

replicates<-200

noise.levels<-c(0.05,0.5,5)
samp.sizes<-c(100)

mse<-data.frame(samp.size=NA,noise.level=NA,mse=NA,model=NA)


for(samp.size in samp.sizes){
   for(noise.level in noise.levels){
      load(paste("ramsim-",noise.level,"-",samp.size,"-results.RData",sep=""))
      
      # calculate MSE
      for(i in 1:replicates){
         mse<-rbind(mse,
                    c(samp.size,noise.level,mean((z.truth-res$tps[i,])^2),"tps"),
                    c(samp.size,noise.level,mean((z.truth-res$wr[i,])^2),"wr"),
                    c(samp.size,noise.level,mean((z.truth-res$mdstps[i,])^2),"mdstps"),
                    c(samp.size,noise.level,mean((z.truth-res$mdstprs[i,])^2),"mdstprs"),
                    c(samp.size,noise.level,mean((z.truth-res$soap[i,])^2),"soap"))
      }
   }
}
      
mse<-mse[-1,]
mse$mse<-as.numeric(mse$mse)



p<-ggplot(mse,aes(factor(model),log(mse)))+xlab("Model")+ylab("log(MSE)")
p<-p+facet_grid(.~noise.level)         
p<-p+geom_boxplot(outlier.size=1)
print(p)


# calculate MSSE


# calculate bias


# calculate artifactiness



