# make the EDF adn MSE results tables for the Ramsay horseshoe simulation

basefilename.mse<-"ramsay-mse-250-"
basefilename.edf<-"ramsay-edf-250-"
errlevs<-c(0.1,1,10)

sqrtn<-sqrt(250)
cat(" & & MSE & &  & EDF & \\\\ \n")
mods<-c("TPRS","MDS (tprs)","Soap film")
cat(paste(mods,mods,sep=" & "))
cat("\\\\ \n")

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
