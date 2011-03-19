source("getdata.R")

library(mdspack)

insert.mds.generic<-function(mds.obj,new.points,old.points,dist.metric="euclidean"){

   big.points<-rbind(old.points,new.points)
   ind<-1:nrow(old.points)
   ind2<-(nrow(old.points)+1):nrow(big.points)

   lambda.inverse<-diag(1/mds.obj$eig[1:dim(mds.obj$points)[2]])
   new.dist<-as.matrix(dist(big.points,method=dist.metric,diag=T,upper=T))[ind,]
   new.dist<-new.dist[,ind2]
   S<- -1/2*mds.obj$x
   d<- -(new.dist^2-diag(S))
   mds.points<-t(1/2*(lambda.inverse %*% t(mds.obj$points) %*% d))

   return(mds.points)
}

# model for free votes only

# these are the free votes for the 1997-2001 parliament

# 22 March 2001       Election of a Speaker 
# 17 January 2001     Hunting Bill 
# 17 January 2001     Hunting Bill 
# 17 January 2001     Hunting Bill
# 20 December 2000    Hunting Bill 
# 19 December 2000    Human Fertilisation and Embryology
#    
# 31 October 2000     Stem Cell Research 
# 14 April 2000       Medical Treatment (Prevention of Euthanasia) Bill 
# 28 February 2000    Sexual Offences (Amendment) Bill 
# 10 February 2000    Sexual Offences (Amendment) Bill 
# 28 January 2000     Medical Treatment (Prevention of Euthanasia) Bill
#    
# 25 January 1999     Sexual Offences (Amendment) Bill
#    
# 22 June 1998       Crime and Disorder Bill 
# 22 June 1998       Crime and Disorder Bill 
# 28 November 1997   Wild Mammals (Hunting with Dogs) Bill

# in reverse order up the list above...
freevotes<-c(1174,964,965,850,520,833,484,407,260,196,181,150,145,148,149,150,45)
# so there are 17 free votes

# does 45 - election of speaker by secret ballot screw things up?
#           let's find out!

# now to pick out those columns that are freevotes
votemat<-votemat[,freevotes]


## Remove some duplicates:
# NOT those who changed party

# Mark Oaten 459, 663
#votemat<-votemat[-455,]
# Fiona Jones 318, 668
votemat[316,]<-c(votemat[316,1:4], votemat[663,5:17])
#votemat<-votemat[-663]


# do these deletions at the end so the index is right!
del.rows<-c(455,663)
votemat<-votemat[-del.rows,]
mpparty<-mpparty[-del.rows]
mpid<-mpid[-del.rows]


################################################

# take a sample, fit a model, predict back

samp.size<-200

# create sample and prediction data sets
samp.ind<-sample(1:dim(votemat)[1],samp.size)
samp.dat<-votemat[samp.ind,]
pred.dat<-votemat[-samp.ind,]

# which metric to use
#dist.metric<-"manhattan"
dist.metric<-"euclidean"

bigletters<-c(letters,paste("a",letters,sep=""),paste("b",letters,sep=""))
bl.len<-length(bigletters)

###########################################
# make a grid
library(sfsmisc)
#base.grid<-t(digitsBase(sample(1:2^17,10000),2,17))
#base.grid[base.grid==0]<- -1
base.grid<-rbind(diag(17),-diag(17))

D.grid<-dist(base.grid,method=dist.metric)

####### ##############
gcvs<-c()
models<-list()
mds.bnds<-2:choose.mds.dim(D.grid,0.8)
i<-1
for(mds.dim in mds.bnds){
k<-100

   mds.obj<-cmdscale(D.grid,mds.dim,eig=TRUE,k=mds.dim,x.ret=TRUE)
   samp.mds<-insert.mds.generic(mds.obj,samp.dat,base.grid)

   samp.mds<-cbind(mpparty[samp.ind],samp.mds)
   attr(samp.mds,"dimnames")[[2]]<-c("response",
                                     bigletters[(bl.len-(dim(samp.mds)[2]-2)):bl.len])
   attr(samp.mds,"dimnames")[[1]]<-mpid[samp.ind]
   samp.mds<-as.data.frame(samp.mds)

   # model setup
   m<-c(2,mds.dim/2-1)
   gam.options<-paste("bs='ds',k=",k,", m=c(",m[1],",",m[2],")",sep="")

   # find the prediction terms
   pred.terms<-bigletters[(bl.len-(dim(samp.mds)[2]-2)):bl.len]
   pred.terms<-paste(pred.terms,collapse=",")

   # create the gam formula
   gam.formula<-paste("response","~s(",paste(pred.terms,collapse=","),",",gam.options,")")
   gam.formula<-as.formula(gam.formula)

   # run the model
   models[[i]]<-gam(gam.formula,data=samp.mds,family=binomial(link="logit"))
   gcvs<-c(gcvs,models[[i]]$gcv.ubre)
   i<-i+1
}

and.the.winner.is<-which.min(gcvs)
b<-models[[and.the.winner.is]]
mds.dim<-mds.bnds[and.the.winner.is]
mds.obj<-cmdscale(D.grid,mds.dim,eig=TRUE,k=mds.dim,x.ret=TRUE)
samp.mds<-insert.mds.generic(mds.obj,samp.dat,base.grid)

samp.mds<-cbind(mpparty[samp.ind],samp.mds)
attr(samp.mds,"dimnames")[[2]]<-c("response",
                                  bigletters[(bl.len-(dim(samp.mds)[2]-2)):bl.len])
attr(samp.mds,"dimnames")[[1]]<-mpid[samp.ind]
samp.mds<-as.data.frame(samp.mds)



######## ###############

###########################################

#D.samp<-dist(samp.dat,method=dist.metric)


#gam.ret<-gam.mds.fit(mpparty[samp.ind],D.samp)
#attach(gam.ret)
#b<-gam.ret$gam

### predictions

# map the predictions
# using code from insert.mds

pred.mds<-insert.mds.generic(mds.obj,pred.dat,base.grid)

# predict back over _all_ MPs
pred.grid<-matrix(NA,length(mpid),mds.dim)
pred.grid[samp.ind,]<-as.matrix(samp.mds)[,-1]
pred.grid[-samp.ind,]<-pred.mds
pred.grid<-as.data.frame(pred.grid)
attr(pred.grid,"names")<-bigletters[(length(bigletters)-(dim(samp.mds)[2]-2)):length(bigletters)]

pr<-predict(b,pred.grid,type="response")

pr[pr<=0.5]<-0
pr[pr>0.5]<-1

# mse
ds.mse<-sum((pr-mpparty)^2)
cat("ds MSE=",ds.mse,"\n")

#wrong<-mpid[(t(t(pr))-mpparty)!=0]
#wrong.ind<-match(wrong,lookup$mpid)
#
#cat("wrong=",length(wrong),"\n")
#
#wrong.names<-paste(lookup$firstname[wrong.ind],lookup$surname[wrong.ind])  
#cat(wrong.names,sep="\n")



## what about using the Lasso

#library(lars)
#lars.obj<-lars(as.matrix(samp.dat),mpparty[samp.ind])
#pp<-predict.lars(lars.obj,votemat)
#cv.obj<-cv.lars(as.matrix(samp.dat),mpparty[samp.ind],plot.it=FALSE,fraction=pp$fraction)
#cv.min<-which.min(cv.obj$cv)
#pp<-pp$fit[,cv.min]
#
#pp[pp<=0.5]<-0
#pp[pp>0.5]<-1
#lasso.mse<-sum((pp-mpparty)^2)
#cat("lasso MSE=",lasso.mse,"\n")
#
##detach(gam.ret)


# trying glmnet lasso instead?
library(glmnet)
cv.lasso<-cv.glmnet(as.matrix(samp.dat),mpparty[samp.ind],family="binomial")
lasso.obj<-glmnet(as.matrix(samp.dat),mpparty[samp.ind],family="binomial",lambda=cv.lasso$lambda.min)
pp<-predict(lasso.obj,votemat,type="response")
#
pp[pp<=0.5]<-0
pp[pp>0.5]<-1
lasso.mse<-sum((pp-mpparty)^2)
cat("lasso MSE=",lasso.mse,"\n")
