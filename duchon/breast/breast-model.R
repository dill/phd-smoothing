source("getdata.R")

# calculate the distance matrix for the microarray data
breast.dist<-dist(breast.array,diag=TRUE,upper=TRUE)

# fit the model
b.gcv<-gam.mds.fit(breast.dat$npi,breast.dist,NULL,45,c(2,0.85))

# what does the GCV score look like?
plot(b.gcv$gcvs$dim, b.gcv$gcvs$gcv,ylab="GCV score",xlab="MDS dimension",type="l")
