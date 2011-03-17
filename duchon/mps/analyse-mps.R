# what happened in the free votes sim?
library(ggplot2)


# plotting options
theme_set(theme_bw())
plot.rows<-3
plot.cols<-1
Layout <- grid.layout(nrow = plot.rows, ncol = plot.cols,
                      widths = unit(rep(3,plot.rows*plot.cols),"null"),
                      heights = unit(rep(3,plot.rows*plot.cols), "null"))

subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)
vplayout <- function(...) {
     grid.newpage()
     pushViewport(viewport(layout = Layout))
}
grid.newpage()
pushViewport(viewport(layout = Layout))


# load the data and calculate a summary
load("freesim.RData")
labbin<-labbin[-del.rows,]
lookup<-lookup[-del.rows,]


# store the min GCV per sim and the dimension
min.gcvs<-as.data.frame(cbind(apply(gcv.res,1,min),mds.bnds[apply(gcv.res,1,which.min)]))
names(min.gcvs)<-c("gcv","dim")

# first plot the lines of the GCV scores...
dimnames(gcv.res)[[1]]<-1:200
gcv.res<-melt(gcv.res)
names(gcv.res)<-c("sim","dim","gcv")
gcv.res$dim<-mds.bnds[gcv.res$dim]

p<-ggplot(gcv.res)
p<-p+geom_line(aes(x=dim,y=gcv,group=sim),alpha=0.3)
p<-p+geom_smooth(aes(x=dim,y=gcv),alpha=0.3,fill="green")
p<-p+geom_point(aes(x=dim,y=gcv),colour="red",data=min.gcvs)
p<-p+labs(x="MDS dimension",y="GCV score")
print(p,vp=subplot(1,1))


# now looking at the errors...
res<-wrong.mat[,-686]
res<-matrix(as.numeric(res),nrow(res),ncol(res))
res<-res[,mpid]

#### per simulation results

sim.res<-data.frame(mse=685-rowSums(res),model=wrong.mat[,686],sim=as.numeric(sapply(1:200,rep,2)))

p<-ggplot(sim.res)
p<-p+geom_histogram(aes(y=mse,sim,binwidth=1),stat="identity")
p<-p+facet_wrap(~model,nrow=1)
p<-p+labs(x="Simulation",y="Missclassifications")
print(p,vp=subplot(2,1))


#### per MP simulations

ds.mps<-res[seq(1,400,2),]
lasso.mps<-res[seq(2,400,2),]

lasso.mps<-data.frame(wrong=200-colSums(lasso.mps),id=mpid,party=mpparty,model=rep("lasso",676))
ds.mps<-data.frame(wrong=200-colSums(ds.mps),id=mpid,party=mpparty,model=rep("ds",676))

mp.res<-rbind(lasso.mps,ds.mps)
p<-p+labs(x="MPs",y="Missclassifications")

p<-ggplot(mp.res)
p<-p+geom_histogram(aes(y=wrong,id,colour=party),stat="identity",legend=FALSE)
p<-p+facet_wrap(~model,nrow=1)
p<-p+labs(x="MPs",y="Missclassifications")
print(p,vp=subplot(3,1))





