# quick analysis of the leukemia data...
# putting them into group 10 vs !group 10

load("leuk.RData")
library(mdspack)

###############################
# re-ordering by recoding...
leuk.type<-leuk[,length(leuk)]
leuk<-leuk[,-length(leuk)]

leuk.type<-as.numeric(as.numeric(leuk.type)==9)#10)

# pull out the 40 genes that best classify ALL
# according to chisq

#ALL<-read.csv(file="T-ALL-chisq-genes.csv",header=FALSE)
#ALL<-as.matrix(as.character(levels(as.factor(ALL[,1]))))
#ALL.cols<-as.matrix(apply(ALL,1,grep,x=colnames(leuk)))
#
## some of those turned up 2 results, fix that
#ALL.cols[3,]<-116
#ALL.cols[4,]<-263
#ALL.cols[6,]<-1271
#
#ALL.cols<-as.integer(ALL.cols)
#
#leuk<-leuk[,ALL.cols]


dd.start<-mahalanobis(leuk, leuk[1,])
dd<-apply(leuk,1,mahalanobis,x=leuk,cov.inv=attr(dd.start,"cov.inv"))

# how does the proportion of variation explained increase with dimension?
dims<-c()
for(prop in seq(0.5,1,0.01))
  dims<-c(dims,choose.mds.dim(dd,prop))
plot(seq(0.5,1,0.01),dims)

b<-gam.mds.fit(leuk.type,dd,NULL,k=300,mds.dim.bnds=c(2,.8),
               dist.metric="mahalanobis",fam=binomial)

b.9<-gam.mds.fit(leuk.type,dd,9,k=300,
               dist.metric="mahalanobis",fam=binomial)

b<-gam.mds.fit(leuk.type,dd,NULL,k=300,mds.dim.bnds=c(2,.8),
               dist.metric="mahalanobis",fam=binomial,method="ML")

#> b$scores
#   dim      score
#1    2 0.07055555
#2    3 0.05954162
#3    4 0.04716905
#4    5         NA
#5    6 0.06694478
#6    7 0.08244496
#7    8 0.08865809
#8    9 0.08758275 <<< this is better
#9   10 0.07816727
#10  11 0.08162922
#11  12 0.08569197
#12  13 0.01241279 <<< MIN
#13  14 0.08772700
#14  15 0.09191823
#15  16 0.09484973
#16  17 0.06920775
#17  18 0.07492213
#18  19 0.08347683
#19  20 0.07625726
#20  21 0.07052963
#21  22 0.06963133
#22  23 0.06891374
#23  24 0.05692790
#24  25 0.05995397






# model checking...
plot(b$scores$dim,b$scores$score,type="l")
which.min(b$scores$score)
abline(h=b$scores$score[64])
gam.check(b$gam)

#library(ggplot2)



