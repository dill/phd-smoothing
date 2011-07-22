library(msg)
library(ggplot2)
library(smida)

# laod the data
data(breast)

# put it into a nicer format
breast.dat<-as.data.frame(cbind(breast$npi,breast$surv.time, breast$size, breast$age.at.diag,
                                breast$any.death,breast$cancer.death,
                                breast$cancer.grade))
names(breast.dat)<-c("npi","surv.time","size","age.at.diag",
                     "any.death","cancer.death",
                     "cancer.grade")
breast.array<-as.matrix(breast$dat)

# save
breast.array.full<-breast.array
breast.dat.full<-breast.dat

# pull out observations with no NPI
ind<-!is.na(breast$npi)
# microarray data
breast.array<-breast.array[ind,]
# other covariates
breast.dat<-breast.dat[ind,]

# remove columns with NAs in them
col.ind<-colSums(is.na(breast.array))>0
breast.array<-breast.array[,!col.ind]

breast.array<-as.data.frame(cbind(breast.dat$npi,breast.array))

names(breast.array)<-c("npi",paste("a",1:27,sep=""))

this.formula<-as.formula(paste("npi~s(",
     paste("a",1:26,",",sep="",collapse=""),"a27, bs=\"msg\",xt=list(mds.dim=6,metric=\"mahalanobis\"))",sep=""))


source("~/current/phd-smoothing/package/msg/R/smooth.construct.msg.smooth.spec.R")

b<-gam(this.formula,data=breast.array)

