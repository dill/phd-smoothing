source("getdata.R")

# simulation for the free vote data

library(mdspack)
library(lars)
library(glmnet)

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
# make a grid
#library(sfsmisc)
#base.grid<-t(digitsBase(sample(1:2^17,10000),2,17))
#base.grid[base.grid==0]<- -1
#base.grid<-rbind(diag(17),-diag(17))
#
## which metric to use
##dist.metric<-"manhattan"
#dist.metric<-"euclidean"
#
#D.grid<-dist(base.grid,method=dist.metric)
#mds.bnds<-2:choose.mds.dim(D.grid,0.8)

# take a sample, fit a model, predict back

samp.size<-200
gcv.res<-c()
wrong.mat<-c()
retvec<-rep(1,max(mpid))

k<-100
bigletters<-c(letters,paste("a",letters,sep=""),paste("b",letters,sep=""))
bl.len<-length(bigletters)

for(n.sim in 1:200){

   # create sample and prediction data sets
   samp.ind<-sample(1:dim(votemat)[1],samp.size)
   samp.dat<-votemat[samp.ind,]
   pred.dat<-votemat[-samp.ind,]
   
   D.samp<-dist(samp.dat,upper=T,diag=T)

   mpparty.samp<-as.data.frame(mpparty[samp.ind])
   rownames(mpparty.samp)<-mpid[samp.ind]

   gam.obj<-gam.mds.fit(mpparty.samp,D.samp,NULL,k,c(4,16),fam=binomial(link="logit"))

   b<-gam.obj$gam
   mds.dim<-gam.obj$mds.dim
   mds.obj<-gam.obj$mds.obj
   samp.mds<-gam.obj$samp.mds
   gcvs<-gam.obj$gcvs

   samp.mds<-as.data.frame(samp.mds)
   
   ### predictions
   # map the predictions
   # using code from insert.mds
   
   pred.mds<-insert.mds.generic(mds.obj,pred.dat,samp.dat)
   
   # predict back over _all_ MPs
   pred.grid<-matrix(NA,length(mpid),mds.dim)
   pred.grid[samp.ind,]<-as.matrix(samp.mds)[,-1]
   pred.grid[-samp.ind,]<-pred.mds
   pred.grid<-as.data.frame(pred.grid)
   attr(pred.grid,"names")<-bigletters[(length(bigletters)-(dim(samp.mds)[2]-2)):length(bigletters)]
   
   pr<-predict(b,pred.grid,type="response")
   
   pr[pr<=0.5]<-0
   pr[pr>0.5]<-1
   
   
   wrong.ds<-mpid[(t(t(pr))-mpparty)!=0]
   wrongvec<-retvec
   wrongvec[wrong.ds]<-0
   wrong.mat<-rbind(wrong.mat,c(wrongvec,"ds"))
   
   ##################################################
   ## what about using the Lasso
   lars.obj<-lars(as.matrix(samp.dat),mpparty[samp.ind])
   pp<-predict.lars(lars.obj,votemat)
   cv.obj<-cv.lars(as.matrix(samp.dat),mpparty[samp.ind],plot.it=FALSE,index=pp$fraction,mode="fraction")
   cv.min<-which.min(cv.obj$cv)
   pp<-pp$fit[,cv.min]
   
   pp[pp<=0.5]<-0
   pp[pp>0.5]<-1
   
   wrong.lasso<-mpid[(t(t(pp))-mpparty)!=0]
   wrongvec<-retvec
   wrongvec[wrong.lasso]<-0
   wrong.mat<-rbind(wrong.mat,c(wrongvec,"lasso"))



   ##################################################
   # trying glmnet lasso instead?
   cv.lasso<-cv.glmnet(as.matrix(samp.dat),mpparty[samp.ind],family="binomial")
   lasso.obj<-glmnet(as.matrix(samp.dat),mpparty[samp.ind],family="binomial",
                     lambda=cv.lasso$lambda.min)

   pp<-predict(lasso.obj,votemat,type="response")

   pp[pp<=0.5]<-0
   pp[pp>0.5]<-1

   wrong.glmnet<-mpid[(t(t(pp))-mpparty)!=0]
   wrongvec<-retvec
   wrongvec[wrong.glmnet]<-0
   wrong.mat<-rbind(wrong.mat,c(wrongvec,"glmnet"))

   gcv.res<-rbind(gcv.res,gcvs$gcv)
}

colnames(gcv.res)<-gcvs$dim

save.image("freesim.RData")

