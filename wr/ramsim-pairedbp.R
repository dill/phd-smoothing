# piared boxplots for Ramsay sim results 
# David Lawrence Miller 2010

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

# now we have the MSEs, calculate the pairs by subtracting
# the soap MSEs...

# storage
pairmse<-data.frame(mse=NA,model=NA)

# soap results
soapmse<-mse$mse[mse$model=="soap"]


# comment out mdstps and tps to see the different between mdstprs and wr

for(i in 1:replicates){
   pairmse<-rbind(pairmse,
#                  c(mse$mse[mse$model=="tps"][i]-soapmse[i],"tps"),
                  c(mse$mse[mse$model=="wr"][i]-soapmse[i],"wr"),
#                  c(mse$mse[mse$model=="mdstps"][i]-soapmse[i],"mdstps"),
                  c(mse$mse[mse$model=="mdstprs"][i]-soapmse[i],"mdstprs"))
}


pairmse<-pairmse[-1,]
pairmse$mse<-as.numeric(pairmse$mse)


library(ggplot2)

p<-ggplot(pairmse,aes(factor(model),mse))+xlab("Model")+ylab("log(MSE)")
p+geom_boxplot(outlier.size=1)


# calculate MSSE


# calculate bias


# calculate artifactiness

