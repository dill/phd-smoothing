# explore the Golub et al data...

library(golubEsets)

library(mdspack)


data(Golub_Train)

# extract the expression data
golub.array<-t(as.matrix(exprs(Golub_Train)))

# whether the patient has ALL/AML
golub.resp<-pData(phenoData(Golub_Train))$ALL.AML
golub.r<-rep(0,length(golub.resp))
golub.r[golub.resp=="AML"]<-1




# find the distances
mD<-apply(golub.array,1,mahalanobis,x=golub.array)



# how does the proportion of variation explained increase with dimension?
dims<-c()
for(prop in seq(0.5,1,0.01))
  dims<-c(dims,choose.mds.dim(mD,prop))
plot(seq(0.5,1,0.01),dims)

#b<-gam.mds.fit(golub.r,mD,NULL,k=30,mds.dim.bnds=c(2,.8),dist.metric="mahalanobis")

b<-gam.mds.fit(golub.r,mD,NULL,k=38,mds.dim.bnds=c(2,15),dist.metric="mahalanobis",fam=binomial,method="ML")

# model checking...
plot(b$scores$dim,b$scores$score,type="l")
abline(h=min(b$scores$score),col="red")
quartz()
gam.check(b$gam)

