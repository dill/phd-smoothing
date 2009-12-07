# make some tables


#ramsay
basefilename<-"ramsay-250-"
errlevs<-c(0.1,1,10)

cat(" & MDS & soap & thin plate\\\\ \n")

for(errlev in errlevs){

   cat(errlev," & ")
   dat<-read.csv(paste(basefilename,errlev,".csv",sep=""))
   mses<-c(mean(dat$mds),mean(dat$soap),mean(dat$tprs))
   ses<-c(sd(dat$mds),sd(dat$soap),sd(dat$tprs))



   cat(round(mses[1],4)," (",round(ses[1],4),") & ",sep="")
   cat(round(mses[2],4)," (",round(ses[2],4),") & ",sep="")
   cat(round(mses[3],4)," (",round(ses[3],4),")\\\\ \n",sep="")

}

cat("\n\n")

#wt2
basefilename<-"wt2-250-"
errlevs<-c(0.05,0.5,5)

cat(" & MDS & MDS (tensor) & soap & thin plate\\\\ \n")
for(errlev in errlevs){

   dat<-read.csv(paste(basefilename,errlev,".csv",sep=""))

   # here I remove the two simulations where the PDE solution grid and knots
   # line up and cause soap to fail

   mses<-c(mean(dat$mds),mean(dat$mdstp),mean(dat$soap[dat$soap<10]),mean(dat$tprs))
   ses<-c(sd(dat$mds),sd(dat$mdstp),sd(dat$soap[dat$soap<10]),sd(dat$tprs))

   cat(round(mses[1],4)," (",round(ses[1],4),") & ",sep="")
   cat(round(mses[2],4)," (",round(ses[2],4),") & ",sep="")
   cat(round(mses[3],4)," (",round(ses[3],4),") & ",sep="")
   cat(round(mses[4],4)," (",round(ses[4],4),")\\\\ \n",sep="")

}
