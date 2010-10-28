# attempt to duplicate the simulation settings in 
# Wang and Ranalli (2007)
# Copyright David Lawrence Miller 2010
source("wr-wrapper.R")
library(ggplot2)

replicates<-200

noise.levels<-c(0.05,0.5,5)
samp.sizes<-c(100)

mse.dat<-data.frame(samp.size=NA,noise.level=NA,mse=NA,model=NA,test=NA)


for(samp.size in samp.sizes){
   for(noise.level in noise.levels){
      load(paste("ramsim-",noise.level,"-",samp.size,"-results.RData",sep=""))
      
      mse<-data.frame(samp.size=NA,noise.level=NA,mse=NA,model=NA,test=NA)
      # calculate MSE
      for(i in 1:replicates){

         # calculate the mses
         these.mses<-c(mean((z.truth-res$tps[i,])^2),    
                       mean((z.truth-res$wr[i,])^2),     
                       mean((z.truth-res$mdstps[i,])^2), 
                       mean((z.truth-res$mdstprs[i,])^2),
                       mean((z.truth-res$soap[i,])^2))

         # store them...
         mse<-rbind(mse,
                    c(samp.size,noise.level,these.mses[1],"tps",1),
                    c(samp.size,noise.level,these.mses[2],"wr",1),
                    c(samp.size,noise.level,these.mses[3],"mdstps",1),
                    c(samp.size,noise.level,these.mses[4],"mdstprs",1),
                    c(samp.size,noise.level,these.mses[5],"soap",1))
      }

      # get rid of the superfluous first row 
      mse<-mse[-1,]
      # make sure the mse is numeric
      mse$mse<-as.numeric(mse$mse)

      # do some tests!
      cols<-c() # store the colours
      tstat<-matrix(NA,200,5)
      tstat[,1]<-pe(mse,mse$model=="tps")$mse
      tstat[,2]<-pe(mse,mse$model=="wr")$mse
      tstat[,3]<-pe(mse,mse$model=="mdstps")$mse
      tstat[,4]<-pe(mse,mse$model=="mdstprs")$mse
      tstat[,5]<-pe(mse,mse$model=="soap")$mse

      mod.names<-c("tps","wr","mdstps","mdstprs")

      for(i in 1:4){
         pv<-wilcox.test(tstat[,5],tstat[,i],paired=TRUE)$p.value
         med<-median(tstat[,i]-tstat[,5])
         if(pv<0.01 & med>0){
            cols<-c(cols,"sig>soap")
         }else if(pv<0.01 & med<0){
            cols<-c(cols,"sig<soap")
         }else{
            cols<-c(cols,"nonsig")
         }
         cat("soap vs. ",mod.names[i],pv,"\n")
      }

      mse$test[mse$model=="tps"]<-rep(cols[1],200)
      mse$test[mse$model=="wr"]<-rep(cols[2],200)
      mse$test[mse$model=="mdstps"]<-rep(cols[3],200)
      mse$test[mse$model=="mdstprs"]<-rep(cols[4],200)
      mse$test[mse$model=="soap"]<-rep("soap",200)

      mse.dat<-rbind(mse.dat,mse)


   }
}


mse.dat<-mse.dat[-1,]


p<-ggplot(mse.dat,aes(factor(model),log(mse)))+xlab("Model")+ylab("log(MSE)")
p<-p+facet_grid(.~noise.level) 
p<-p+geom_boxplot(outlier.size=1,aes(fill=factor(test)))
print(p)


# calculate MSSE


# calculate bias


# calculate artifactiness



