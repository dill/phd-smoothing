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

n.sims<-200

library(foreach)
library(doMC)
registerDoMC()
options(cores=6)

samp.sizes<-c(200,300,400,500)

#dist.metric<-"euclidean"
dist.metric<-"binary"

#for(samp.size in samp.sizes){
foreach.result<-foreach(samp.size=samp.sizes,.combine="+",.init=0) %dopar% {

   ####
   # things to store
   score.res<-c()
   wrong.mat<-c()
   retvec<-rep(NA,max(mpid))
   # store the MDS dimension selection info
   mds.dim.sel<-c()

   brier<-data.frame(dsgcv=NA,dsml=NA,glmnet=NA,glm=NA)
   mse<-data.frame(dsgcv=NA,dsml=NA,glmnet=NA,glm=NA)
   edf<-data.frame(dsgcv=NA,dsml=NA)
   
   k<-100
   bigletters<-as.vector(sapply(letters,paste,letters,sep=""))
   bl.len<-length(bigletters)
   
   for(n.sim in 1:n.sims){

      this.brier<-c()
      this.mse<-c()
   
      # create sample and prediction data sets
      samp.ind<-sample(1:dim(votemat)[1],samp.size)
      samp.dat<-votemat[samp.ind,]
      pred.dat<-votemat[-samp.ind,]
      
      D.samp<-dist(samp.dat,upper=T,diag=T,dist.metric=dist.metric)
   
      mpparty.samp<-as.data.frame(mpparty[samp.ind])
      rownames(mpparty.samp)<-mpid[samp.ind]
   
      this.edf<-c()
   
      # using both ML and GCV scoring...
      for(method in c("GCV.Cp","ML")){

         gam.obj<-gam.mds.fit(mpparty.samp,D.samp,NULL,k,c(2,0.85),
                              dist.metric=dist.metric,
                              family=binomial(link="logit"),method=method)

         # record the EDF
         this.edf<-c(this.edf,sum(gam.obj$gam$edf))

         # record the score
         this.score<-as.data.frame(gam.obj$scores)
         this.score<-cbind(this.score,rep(method,nrow(this.score)))
         score.res<-rbind(score.res,this.score)

         # assign some objects
         b<-gam.obj$gam
         mds.dim<-gam.obj$mds.dim
         mds.obj<-gam.obj$mds.obj
         samp.mds<-gam.obj$samp.mds
   
         samp.mds<-as.data.frame(samp.mds)
         
         ### predictions
         pred.mds<-insert.mds.generic(mds.obj,pred.dat,samp.dat)
         pred.mds<-as.data.frame(pred.mds)
         attr(pred.mds,"names")<-bigletters[(length(bigletters)-
                                             (dim(samp.mds)[2]-2)):length(bigletters)]
         pr<-predict(b,pred.mds,type="response")
         
         # calculate the Brier score
         this.brier<-c(this.brier,mean((t(t(pr))-mpparty[-samp.ind])^2))

         pr[pr<=0.5]<-0
         pr[pr>0.5]<-1
         # and the MSE
         a.mse<-(t(t(pr))-mpparty[-samp.ind])^2
         this.mse<-c(this.mse,mean(a.mse))
         
         # record what was right and what was wrong
         #  1 - CORRECT
         #  0 - INCORRECT
         # NA - IN THE SAMPLE
         wrongvec<-retvec
         wrongvec[-samp.ind][a.mse==0]<-1
         wrongvec[-samp.ind][a.mse!=0]<-0
         wrong.mat<-rbind(wrong.mat,wrongvec)

         # store the MDS dimension selection info
         tmp<-gam.obj$scores
         ntmp<-nrow(tmp)
         mds.dim.sel<-rbind(mds.dim.sel,
                            cbind(tmp,
                                  rep(method,ntmp),
                                  rep(samp.size,ntmp),
                                  rep(n.sim,ntmp)
                                 ))
         rm(tmp)
      }

      # store the EDF
      edf<-rbind(edf,this.edf)      

      ##################################################
      # glmnet lasso 
      cv.lasso<-cv.glmnet(as.matrix(samp.dat),mpparty[samp.ind],family="binomial")
      lasso.obj<-glmnet(as.matrix(samp.dat),mpparty[samp.ind],family="binomial",
                        lambda=cv.lasso$lambda.min)
   
      pp<-predict(lasso.obj,votemat[-samp.ind,],type="response")

      # record the Brier score
      this.brier<-c(this.brier,mean((t(t(pp))-mpparty[-samp.ind])^2))
   
      pp[pp<=0.5]<-0
      pp[pp>0.5]<-1

      # record the MSE
      a.mse<-(t(t(pp))-mpparty[-samp.ind])^2
      this.mse<-c(this.mse,mean(a.mse))

      wrongvec<-retvec
      wrongvec[-samp.ind][a.mse==0]<-1
      wrongvec[-samp.ind][a.mse!=0]<-0
      wrong.mat<-rbind(wrong.mat,wrongvec)
      ##################################################

      ##################################################
      # plain ole glm
      glm.samp<-as.data.frame(cbind(mpparty[samp.ind],samp.dat))
      names(glm.samp)<-c("response",letters[1:17])
   
      b.glm<-glm(as.formula(paste("response~",paste(letters[1:17],collapse="+"),sep="")),
                 family=binomial(link="logit"),
                 data=glm.samp)
      glm.step<-step(b.glm,trace=0)
      gvotemat<-as.data.frame(votemat[-samp.ind,])
      names(gvotemat)<-letters[1:17]
      pp<-predict(glm.step,gvotemat,type="response")

      this.brier<-c(this.brier,mean((t(t(pp))-mpparty[-samp.ind])^2))

      pp[pp<=0.5]<-0
      pp[pp>0.5]<-1
   
      a.mse<-(t(t(pp))-mpparty[-samp.ind])^2
      this.mse<-c(this.mse,mean(a.mse))

      wrongvec<-retvec
      wrongvec[-samp.ind][a.mse==0]<-1
      wrongvec[-samp.ind][a.mse!=0]<-0
      wrong.mat<-rbind(wrong.mat,wrongvec)

      ##################################################

      brier<-rbind(brier,this.brier)
      mse<-rbind(mse,this.mse)
   }
   
   # save some data to file
   save(brier,mse,edf,wrong.mat,mds.dim.sel,
        file=paste("freesim-",samp.size,"-",dist.metric,".RData",sep=""))

   return(1)
}
