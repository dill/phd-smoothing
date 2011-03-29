# quick analysis of the leukemia data...

load("leuk.RData")


library(mdspack)



# last column of leuk is the diagnosis 
hist(as.numeric(factor(leuk[,length(leuk)])))

###############################
# re-ordering by recoding...
leuk.type<-leuk[,length(leuk)]
leuk<-leuk[,-length(leuk)]

#> table(as.numeric(factor(leuk.type)))
# 1  2  3  4  5  6  7  8  9 10 
#15 27 64 23  9 20 18 29 43 79 
new.order<-c(5,7,4,8,3,10,9,2,6,1)

leuk.type<-as.numeric(leuk.type)
leuk.old<-leuk.type

for(i in 1:10){
   leuk.type[leuk.old==new.order[i]]<-i
}

#leuk.type<-as.numeric(leuk.type)

# what does the histogram look like now?
hist(leuk.type,breaks=seq(0,11,by=1))

dd<-as.matrix(dist(leuk))

# how does the proportion of variation explained increase with dimension?
dims<-c()
for(prop in seq(0.5,1,0.01))
  dims<-c(dims,choose.mds.dim(dd,prop))
plot(seq(0.5,1,0.01),dims)

b<-gam.mds.fit(leuk.type,dd,NULL,k=300,mds.dim.bnds=c(2,.8),family=quasi())

# model checking...
plot(b$scores$dim,b$scores$score,type="l")
which.min(b$scores$score)
abline(h=b$scores$score[64])
gam.check(b$gam)

#library(ggplot2)



