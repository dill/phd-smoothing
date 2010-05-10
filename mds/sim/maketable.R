# make some tables


#ramsay
basefilename.mse<-"ramsay-mse-250-"
basefilename.edf<-"ramsay-edf-250-"
#errlevs<-c(0.1,1,10)
errlevs<-c(0.35,0.9,1.55)


sqrtn<-sqrt(250)
cat(" & & MSE & &  & EDF & \\\\ \n")
cat(" & MDS & soap & thin plate & MDS & soap & thin plate\\\\ \n")

for(errlev in errlevs){

   cat(errlev," & ")
   mse.dat<-read.csv(paste(basefilename.mse,errlev,".csv",sep=""))
   mses<-c(mean(mse.dat$mds),mean(mse.dat$soap),mean(mse.dat$tprs))
   ses<-c(sd(mse.dat$mds),sd(mse.dat$soap),sd(mse.dat$tprs))/sqrtn

   edf.dat<-read.csv(paste(basefilename.edf,errlev,".csv",sep=""))
   edfs<-c(mean(edf.dat$mds),mean(edf.dat$soap),mean(edf.dat$tprs))
   edfse<-c(sd(edf.dat$mds),sd(edf.dat$soap),sd(edf.dat$tprs))/sqrtn


   cat(round(mses[1],4)," (",round(ses[1],5),") & ",sep="")
   cat(round(mses[2],4)," (",round(ses[2],5),") & ",sep="")
   cat(round(mses[3],4)," (",round(ses[3],5),") &",sep="")
   cat(round(edfs[1],4)," (",round(edfse[1],5),") & ",sep="")
   cat(round(edfs[2],4)," (",round(edfse[2],5),") & ",sep="")
   cat(round(edfs[3],4)," (",round(edfse[3],5),")\\\\ \n",sep="")

}

cat("\n\n")

#wt2
basefilename.mse<-"wt2-mse-250-"
basefilename.edf<-"wt2-edf-250-"
errlevs<-c(0.05,0.5,5)

cat(" & & MSE & &  & EDF & \\\\ \n")
cat(" & MDS & MDS (tensor) & soap & thin plate & MDS & MDS (tensor) & soap & thin plate\\\\ \n")

for(errlev in errlevs){

   dat<-read.csv(paste(basefilename.mse,errlev,".csv",sep=""))

   # here I remove the two simulations where the PDE solution grid and knots
   # line up and cause soap to fail

   mses<-c(mean(dat$mds),mean(dat$mdstp),mean(dat$soap[dat$soap<10]),mean(dat$tprs))
   ses<-c(sd(dat$mds),sd(dat$mdstp),sd(dat$soap[dat$soap<10]),sd(dat$tprs))/sqrtn

   edf.dat<-read.csv(paste(basefilename.edf,errlev,".csv",sep=""))
   edfs<-c(mean(edf.dat$mds),mean(edf.dat$mdstp),mean(edf.dat$soap),mean(edf.dat$tprs))
   edfse<-c(sd(edf.dat$mds),sd(edf.dat$mdstp),sd(edf.dat$soap),sd(edf.dat$tprs))/sqrtn


   cat(errlev," & ")
   cat(round(mses[1],4)," (",round(ses[1],5),") & ",sep="")
   cat(round(mses[2],4)," (",round(ses[2],5),") & ",sep="")
   cat(round(mses[3],4)," (",round(ses[3],5),") &",sep="")
   cat(round(mses[4],4)," (",round(ses[4],5),") &",sep="")
   cat(round(edfs[1],4)," (",round(edfse[1],5),") & ",sep="")
   cat(round(edfs[2],4)," (",round(edfse[2],5),") & ",sep="")
   cat(round(edfs[3],4)," (",round(edfse[3],5),") & ",sep="")
   cat(round(edfs[4],4)," (",round(edfse[4],5),")\\\\ \n",sep="")

}
