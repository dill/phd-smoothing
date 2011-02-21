# love for nomis

load("done.RData")


nomdat<-read.csv(file="nomis.csv")

names(nomdat)<-c("con","code","pay","payC","hours","hoursC")


nom.mapper<-match(nomdat$code,shapefile$dbf$dbf$CODE)
#> nomdat$code[1]
#[1] E14000554
#> shapefile$dbf$dbf$CODE[605]
#[1] E14000554


# other way !!
nom.mapper<-match(shapefile$dbf$dbf$CODE,nomdat$code)


pay.column<-as.numeric(as.character(nomdat$pay[nom.mapper]))
# this will warn that it's put NAs in -- that's GOOD!

na.id<-which(is.na(pay.column))

pay.column<-pay.column[which(!is.na(pay.column))]

D<-dist(pay.column)

library(mdspack)
k<-choose.mds.dim(D,0.8)

mds<-cmdscale(D,k=k)

these.votes<-con.voteshare[mapper]
these.votes<-these.votes[-mapper[na.id]]

samp.data<-data.frame(votes=these.votes,
                      x=mds[,1],
                      y=mds[,2],
                      z=mds[,3])

library(mgcv)

b<-gam(votes~s(x,y,z),data=samp.data)

preds<-predict(b,samp.data)

b<-gam(votes~s(x,y,z,bs="ds",m=c(2,3/2-1)),data=samp.data)






