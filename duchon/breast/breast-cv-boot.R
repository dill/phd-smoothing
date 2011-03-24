source("getdata.R")

# how sensitive are the GCV results to the data?

# use a grid here - justification 

#  Euclidean distances okay to just use extremes since they will be
# the main eigenvectors ?! 
#base.grid<-t(cbind(as.matrix(digitsBase(1:(2^27-1))),rep(0,27)))
#base.grid2<-!base.grid
#
#base.grid<-base.grid*matrix(rep(apply(breast.array,2,min),dim(base.grid)[1]),dim(base.grid)[1],27)
#base.grid2<-base.grid*apply(breast.array,2,max)
#
#base.grid<-base.grid+base.grid2


base.grid<-diag(27)
#base.grid.dist<-dist(base.grid)
#base.grid.mds<-cmdscale(base.grid.dist)

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
   #b.gcv<-gam.mds.fit(npi.boot,breast.dist,NULL,25,c(3,15),gaussian(),base.grid,breast.boot)
   b.gcv<-gam.mds.fit(npi.boot,breast.dist,NULL,25,c(9,18),gaussian())

   # record the GCV
   gcv.boot<-rbind(gcv.boot,b.gcv$gcvs$gcv)
}

# data mudging
gcv.mins<-data.frame(gcv=apply(gcv.boot,1,min),dim=apply(gcv.boot,1,which.min))

gcv.boot<-melt(gcv.boot)
names(gcv.boot)<-c("sim","dim","gcv")

# plotting
p<-ggplot(gcv.boot)
p<-p+geom_line(aes(x=dim+2,y=gcv,group=sim),alpha=0.3)
p<-p+geom_smooth(aes(x=dim+2,y=gcv))
p<-p+geom_point(aes(x=dim+2,y=gcv),data=gcv.mins,colour="red")
print(p)


## what does the GCV score look like?
#plot(b.gcv$gcvs$dim, b.gcv$gcvs$gcv,ylab="GCV score",xlab="MDS dimension",type="l")
