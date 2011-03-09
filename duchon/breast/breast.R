# analyse the breast cancer data from "Statistics for Microarrays"
# by Wit and McClure

library(mdspack)
library(ggplot2)

# load the package
library(smida)
# page 240 is relevant here

# pull the data
data(breast)

## put the non-array data into a frame
# other data: 
#  npi 
#  surivival time
#  size of the tumor (mm)
#  age at diagnosis
#  whether they died
#  whether that death was from cancer
#  cancer severity grade
breast.dat<-as.data.frame(cbind(breast$npi,breast$surv.time, breast$size, breast$age.at.diag, 
#                                breast$any.death,breast$cancer.death,
                                breast$cancer.grade))
names(breast.dat)<-c("npi","surv.time","size","age.at.diag",
#                     "any.death","cancer.death",
                     "cancer.grade")



# the array data
breast.array<-as.matrix(breast$dat)


# want only those who died of cancer in the study period
ind1<-breast.dat$any.death==1
ind2<-breast.dat$cancer.death==1
ind1[is.na(ind1)]<-0
ind2[is.na(ind2)]<-0

# save first
breast.array.full<-breast.array
breast.dat.full<-breast.dat

# array data
breast.array<-breast.array[ind1 & ind2,]
# other data
breast.dat<-breast.dat[ind1 & ind2,]



# calculate the distance matrix
ddist<-dist(breast.array,diag=TRUE,upper=TRUE)

# how many dimensions do we need?
#> choose.mds.dim(ddist,0.5)
#[1] 2
#> choose.mds.dim(ddist,0.6)
#[1] 2
#> choose.mds.dim(ddist,0.7)
#[1] 4
#> choose.mds.dim(ddist,0.8)
#[1] 7
#> choose.mds.dim(ddist,0.9)
#[1] 12
#> choose.mds.dim(ddist,0.95)
#[1] 15


# could use sammon instead? p112
#sam<-sammon(ddist,k=5)$points
#colnames(sam)<-letters[(26-dim(sam)[2]+1):26]
#gam.dat<-cbind(breast.dat,sam)
#m<-c(2,dim(sam)[2]/2-1)
#gam.formula<-paste("surv.time~s(",paste(colnames(sam),collapse=","),
#                   ",",gam.options,")")


mds.proj<-as.data.frame(cmdscale(ddist,choose.mds.dim(ddist,0.7)))
colnames(mds.proj)<-letters[(26-dim(mds.proj)[2]+1):26]
gam.dat<-cbind(breast.dat,mds.proj)

# lazily construct a formula
m<-c(2,dim(mds.proj)[2]/2-1)
gam.options<-paste("bs='ds', k=10, m=c(",m[1],",",m[2],")",sep="")
gam.formula<-paste("surv.time~s(",paste(colnames(mds.proj),collapse=","),
                   ",",gam.options,")",
                   "+s(age.at.diag)+s(size)")



gam.formula<-as.formula(gam.formula)


b<-gam(gam.formula,data=gam.dat,family=Gamma(link="log"))

summary(b)




