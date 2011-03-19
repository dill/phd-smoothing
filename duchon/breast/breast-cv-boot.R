source("getdata.R")

# how sensitive are the GCV results to the data?

# bootstrap time!

b.rows<-nrow(breast.array)
gcv.boot<-c()

for(i in 1:100){

   # do the sampling
   this.samp<-sample(b.rows,b.rows,replace=TRUE)
   breast.boot<-breast.array[this.samp,]
   npi.boot<-breast.dat$npi[this.samp]

   # calculate the distance matrix for the microarray data
   breast.dist<-dist(breast.boot,diag=TRUE,upper=TRUE)

   # fit the model
   b.gcv<-gam.mds.fit(npi.boot,breast.dist,NULL,45,c(3,15))

   # record the GCV
   gcv.boot<-rbind(gcv.boot,b.gcv$gcvs$gcv)
}




## what does the GCV score look like?
#plot(b.gcv$gcvs$dim, b.gcv$gcvs$gcv,ylab="GCV score",xlab="MDS dimension",type="l")
