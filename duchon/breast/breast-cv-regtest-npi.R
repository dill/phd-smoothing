source("getdata.R")

library(glmnet)

# how sensitive are the GCV results to the data?
# using leave-one-out cross validation

# general setup
b.rows<-nrow(breast.array)
link.pow<-c()

for(i in 1:b.rows){

   # do the sampling
   breast.samp<-breast.array[-i,]
   npi.samp<-breast.dat$npi[-i]

   ### DS model

   # calculate the distance matrix for the microarray data
   breast.dist<-dist(breast.samp,diag=TRUE,upper=TRUE)

   # fit the model
   b.gcv.quasi<-gam.mds.fit(npi.samp,breast.dist,NULL,44,c(2,0.85),
                            family=quasi(link=identity,variance="constant"))

   e<-b.gcv.quasi$gam$residuals
   fv<-fitted(b.gcv.quasi$gam)
   link.pow<-rbind(link.pow,(lm(log(e^2)~log(fv))$coeff[2])^2)

}

