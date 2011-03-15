source("getdata.R")

library(mdspack)

# simulation for free votes only

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

samp.size<-100
sim.size<-200

#library(foreach)
#library(doMC)
#registerDoMC()
#options(cores=2)

result<-c()

#result<-foreach(i=1:sim.size,.combine=rbind,.init=c()) %dopar% {
for(i in 1:sim.size){

   retvec<-rep(0,length(mpid))

   # create sample and prediction data sets
   samp.ind<-sample(1:dim(votemat)[1],samp.size)
   samp.dat<-votemat[samp.ind,]
   pred.dat<-votemat[-samp.ind,]
   
   # which metric to use
   #dist.metric<-"manhattan"
   dist.metric<-"euclidean"
   
   D.samp<-dist(samp.dat,method=dist.metric)
   
   bigletters<-c(letters,paste("a",letters,sep=""),paste("b",letters,sep=""))
   
   gam.ret<-gam.mds.fit(mpparty[samp.ind],D.samp,mds.dim.bnds=c(4,0.8))
#   attach(gam.ret)
   mds.obj<-gam.ret$mds.obj
   mds.dim<-gam.ret$mds.dim
   samp.mds<-gam.ret$samp.mds
   b<-gam.ret$gam
   
   # predictions
   
   # map the predictions
   # using code from insert.mds
   lambda.inverse<-diag(1/mds.obj$eig[1:dim(mds.obj$points)[2]])
   new.dist<-as.matrix(dist(votemat,method=dist.metric))[samp.ind,]
   new.dist<-new.dist[,-samp.ind]
   S<- -1/2*mds.obj$x
   d<- -(new.dist^2-diag(S))
   pred.mds<-t(1/2*(lambda.inverse %*% t(mds.obj$points) %*% d))
   
   # predict back over _all_ MPs
   pred.grid<-matrix(NA,length(mpid),mds.dim)
   pred.grid[samp.ind,]<-mds.obj$points
   pred.grid[-samp.ind,]<-pred.mds
   pred.grid<-as.data.frame(pred.grid)
   attr(pred.grid,"names")<-bigletters[(length(bigletters)-(dim(samp.mds)[2]-2)):length(bigletters)]
   
   pr<-predict(b,pred.grid,type="response")
   
   pr[pr<=0.5]<-0
   pr[pr>0.5]<-1
   
   wrong<-mpid[(t(t(pr))-mpparty)!=0]
   wrong.ind<-match(wrong,lookup$mpid)
   
   # return vector, put 1s where we messed up
   retvec[wrong.ind]<-1

#   return(retvec)
   result<-rbind(result,retvec)
}

save.image("freesim.RData")
