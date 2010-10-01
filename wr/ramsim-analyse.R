# attempt to duplicate the simulation settings in 
# Wang and Ranalli (2007)
# Copyright David Lawrence Miller 2010
#source("mds.R")
#source("tps.R")

source("wr-wrapper.R")

# import fields for the cover.design() function
library(fields)


#set.seed(12)

# as in the paper
samp.size<-100
noise.level<-0.05
n.knots<-40
replicates<-200

load("ram-wr-comp.RData")


# calculate MSE
mse<-data.frame(mse=NA,model=NA)
for(i in 1:replicates){
   mse<-rbind(mse,
              c(mean((z.truth-res$tps[i,])^2),"tps"),
              c(mean((z.truth-res$wr[i,])^2),"wr"))
}

mse<-mse[-1,]
mse$mse<-as.numeric(mse$mse)

# load the MDS and soap results
load("MDSram-mse-comp.RData")
# calculate MSE
for(i in 1:replicates){
   mse<-rbind(mse,
              c(mean((z.truth-res$mdstps[i,])^2),"mdstps"),
              c(mean((z.truth-res$mdstprs[i,])^2),"mdstprs"),
              c(mean((z.truth-res$soap[i,])^2),"soap"))
}

mse$mse<-as.numeric(mse$mse)




library(ggplot2)

p<-ggplot(mse,aes(factor(model),log(mse)))+xlab("Fitting method")+ylab("log(MSE)")
p+geom_boxplot(outlier.size=1)


# calculate MSSE


# calculate bias


# calculate artifactiness

