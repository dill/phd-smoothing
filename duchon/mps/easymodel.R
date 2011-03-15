source("getdata.R")


# take a sample, fit a model, predict back

samp.size<-100

# create sample and prediction data sets
samp.ind<-sample(1:dim(votemat)[1],samp.size)
samp.dat<-votemat[samp.ind,]
pred.dat<-votemat[-samp.ind,]


D.samp<-dist(samp.dat)

mds.dim<-choose.mds.dim(D.samp,0.75)

mds.obj<-cmdscale(D.samp,mds.dim,eig=TRUE,k=mds.dim,x.ret=TRUE)
samp.mds<-mds.obj$points

samp.mds<-cbind(mpparty[samp.ind],samp.mds)
attr(samp.mds,"dimnames")[[2]]<-c("lab",letters[(26-(dim(samp.mds)[2]-2)):26])
attr(samp.mds,"dimnames")[[1]]<-mpid[samp.ind]
samp.mds<-as.data.frame(samp.mds)

# model setup
m<-c(2,mds.dim/2-1)
gam.options<-paste("bs='ds',k=100, m=c(",m[1],",",m[2],")",sep="")

# find the prediction terms
pred.terms<-letters[(26-(dim(samp.mds)[2]-2)):26]
pred.terms<-paste(pred.terms,collapse=",")

# create the gam formula
gam.formula<-paste("lab~s(",paste(pred.terms,collapse=","),",",gam.options,")")
gam.formula<-as.formula(gam.formula)

# run the model
b<-gam(gam.formula,data=samp.mds,family=binomial(link="logit"))


# predictions

# map the predictions
# using code from insert.mds
lambda.inverse<-diag(1/mds.obj$eig[1:dim(mds.obj$points)[2]])
new.dist<-as.matrix(dist(votemat))[samp.ind,]
new.dist<-new.dist[,-samp.ind]
S<- -1/2*mds.obj$x
d<- -(new.dist^2-diag(S))
pred.mds<-t(1/2*(lambda.inverse %*% t(mds.obj$points) %*% d))

# predict back over _all_ MPs
pred.grid<-matrix(NA,length(mpid),mds.dim)
pred.grid[samp.ind,]<-mds.obj$points
pred.grid[-samp.ind,]<-pred.mds
pred.grid<-as.data.frame(pred.grid)
attr(pred.grid,"names")<-letters[(26-(dim(samp.mds)[2]-2)):26]

pr<-predict(b,pred.grid,type="response")

pr[pr<=0.5]<-0
pr[pr>0.5]<-1

# mse
ds.mse<-sum((pr-mpparty)^2)

wrong<-mpid[(t(t(pr))-mpparty)!=0]
wrong.ind<-match(wrong,lookup$mpid)

wrong.names<-paste(lookup$firstname[wrong.ind],lookup$surname[wrong.ind])  
cat(wrong.names,sep="\n")

