# make tables for big wt2 simulation

basefilename.mse<-"sim-mse"
basefilename.edf<-"sim-edf"
errlevs<-c(0.95,0.75,0.5)
samp.sizes<-c(100,250,500)
sqrtn<-sqrt(100)

mods<-c("$n$","$\\sigma$","tprs","mds+tp","mds+cr","mds+tp 3D","mds+tp+adj","soap")


# MSE table
cat(" & ")
cat(mods,sep=" & ")
cat("\\\\ \n")

for(samp.size in samp.sizes){
   for(errlev in errlevs){
      # load the data
      dat<-read.csv(paste(basefilename.mse,"-",samp.size,"-",errlev,".csv",sep=""))
      dat<-dat[,-1]
      dat<-dat[1:100,]
   
      mses<-colMeans(dat)
      ses<-apply(dat,2,sd)/sqrtn
   
      cat(samp.size," & ",errlev," & ")
   
      for(i in 1:6){
         cat(round(mses[i],4)," (",round(ses[i],5),") & ",sep="")
      }
      cat("\\\\ \n",sep="")
   }
}

# EDF table
cat(" & ")
cat(mods,sep=" & ")
cat("\\\\ \n")

for(samp.size in samp.sizes){
   for(errlev in errlevs){
      # load the data
      edf.dat<-read.csv(paste(basefilename.edf,"-",samp.size,"-",errlev,".csv",sep=""))
      edf.dat<-edf.dat[,-1]
      edf.dat<-edf.dat[1:100,]
   
      edfs<-colMeans(edf.dat)
      edfse<-apply(edf.dat,2,sd)/sqrtn
   
      cat(samp.size," & ",errlev," & ")
      for(i in 1:6){
         cat(round(edfs[i],4)," (",round(edfse[i],5),") & ",sep="")
      }
      cat("\\\\ \n",sep="")
   }
}
