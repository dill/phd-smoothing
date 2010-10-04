# artifact analysis code
# David Lawrence Miller 2010

library(ggplot2)
replicates<-200

load("wr-ram-arti.RData")

# calculate MSE
artidat<-data.frame(arti=NA,model=NA)

res1<-res
load("mds-ram-arti.RData")


artis<-c(res1$wr,
         res1$tps,
         res$soap,
         res$mdstps,
         res$mdstprs)

models<-c(rep("wr",replicates),
          rep("tps",replicates),
          rep("soap",replicates),
          rep("mdstps",replicates),
          rep("mdstprs",replicates))

artidat<-data.frame(model=models,arti=artis)


p<-ggplot(artidat,aes(factor(model),log(arti)))+xlab("Model")+ylab("log(Artefactiness)")
p+geom_boxplot(outlier.size=1)

