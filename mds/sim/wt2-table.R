# make tables for big wt2 simulation

basefilename.mse<-"wt2-mse-250-"
basefilename.edf<-"wt2-edf-250-"
#errlevs<-c(0.05,0.5,5)
errlevs<-c(0.35,0.9,1.55)
sqrtn<-200

mods<-c("tprs","mds+tp","mds+cr","mds+tp 3D","mds+tp+adj","soap")


# MSE table
cat(" & ")
cat(mods,sep=" & ")
cat("\\\\ \n")

for(errlev in errlevs){
   # load the data
   dat<-read.csv(paste(basefilename.mse,errlev,".csv",sep=""))
   dat<-dat[,-1]

   # here I remove the three simulations where the PDE solution grid and knots
   # line up and cause soap to fail
   dat<-dat[-which(dat$soap>10),]

   mses<-colMeans(dat)
   ses<-apply(dat,2,sd)/sqrtn

   cat(errlev," & ")

   for(i in 1:6){
      cat(round(mses[i],4)," (",round(ses[i],5),") & ",sep="")
   }
   cat("\\\\ \n",sep="")
}

# EDF table
cat(" & ")
cat(mods,sep=" & ")
cat("\\\\ \n")

for(errlev in errlevs){
   # load the data
   edf.dat<-read.csv(paste(basefilename.edf,errlev,".csv",sep=""))
   edf.dat<-edf.dat[,-1]
   dat<-read.csv(paste(basefilename.mse,errlev,".csv",sep=""))
   dat<-dat[,-1]

   # here I remove the three simulations where the PDE solution grid and knots
   # line up and cause soap to fail
   edf.dat<-edf.dat[-which(dat$soap>10),]

   edfs<-colMeans(edf.dat)
   edfse<-apply(edf.dat,2,sd)/sqrtn

   cat(errlev," & ")
   for(i in 1:6){
      cat(round(edfs[i],4)," (",round(edfse[i],5),") & ",sep="")
   }
   cat("\\\\ \n",sep="")
}
