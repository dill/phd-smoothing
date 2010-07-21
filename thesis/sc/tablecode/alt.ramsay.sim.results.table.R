# R code to generate the tables
# for normal Ramsay simulations

# just generates the middle of the table

# column headings are:
# Sample size & Noise level & P-spline MSE (\emph{se}) & Thin plate MSE (\emph{se}) & Soap MSE (\emph{se}) \\

sqrtn<-sqrt(1000)

rounded<-5


## First the default settings from soap-package help file
this.file<-read.csv(paste("altramsay/pspline.results.txt",sep=""))
mapped.mse<-round(mean(this.file$mapped),rounded)
mapped.se<-round(sd(this.file$mapped)/sqrtn,rounded)

soap.mse<-round(mean(this.file$soap),rounded)
soap.se<-round(sd(this.file$soap)/sqrtn,rounded)

this.file<-read.csv(paste("altramsay/tp-noisey-0.5.results.txt",sep=""))
tp.mse<-round(mean(this.file$tpmapped),rounded)
tp.se<-round(sd(this.file$tpmapped)/sqrtn,rounded)

this.file<-read.csv(paste("altramsay/alttp-0.3-1000.results.txt",sep=""))
tprs.mse<-round(mean(this.file$tp),rounded)
tprs.se<-round(sd(this.file$tp)/sqrtn,rounded)


# cat it out
cat("1000 & 0.3 & ",
   tprs.mse," (",tprs.se,") & ",
   mapped.mse," (",mapped.se,") & ",
   tp.mse," (",tp.se,") & ",
   soap.mse," (",soap.se,") \\\\ \n",sep="")



## Different sample sizes
sample.sizes<-c(500,250,100)

for(my.size in sample.sizes){

   sqrtn<-sqrt(my.size)

   # read in the table
   this.file<-read.csv(paste("altramsay/sample.size.",my.size,".results.txt",sep=""))

   # calculate the mses
   soap.mse<-round(mean(this.file$soap),rounded)
   mapped.mse<-round(mean(this.file$mapped),rounded)
   
   # calculate the se(mse)
   soap.se<-round(sd(this.file$soap)/sqrtn,rounded)
   mapped.se<-round(sd(this.file$mapped)/sqrtn,rounded)

   # read in the tp stuff    
   this.file<-read.csv(paste("altramsay/tp-sample.size.",my.size,".results.txt",sep=""))
   tp.mse<-round(mean(this.file$tpmapped),rounded)
   tp.se<-round(sd(this.file$tpmapped)/sqrtn,rounded)

   this.file<-read.csv(paste("altramsay/alttp-0.3-",my.size,".results.txt",sep=""))
   tprs.mse<-round(mean(this.file$tp),rounded)
   tprs.se<-round(sd(this.file$tp)/sqrtn,rounded)

   # cat it out
   cat(my.size," & 0.3 & ",
      tprs.mse," (",tprs.se,") & ",
      mapped.mse," (",mapped.se,") & ",
      tp.mse," (",tp.se,") & ",
      soap.mse," (",soap.se,") \\\\ \n",sep="")

}


## different error levels
error.levels<-c(0.5,1,2)

sqrtn<-sqrt(1000)

for(my.error in error.levels){
   # read in the table
   this.file<-read.csv(paste("altramsay/noisey-",my.error,".results.txt",sep=""))

   # calculate the mses
   soap.mse<-round(mean(this.file$soap),rounded)
   mapped.mse<-round(mean(this.file$mapped),rounded)
   
   # calculate the se(mse)
   soap.se<-round(sd(this.file$soap)/sqrtn,rounded)
   mapped.se<-round(sd(this.file$mapped)/sqrtn,rounded)

   # read in the tp stuff    
   this.file<-read.csv(paste("altramsay/tp-noisey-",my.error,".results.txt",sep=""))
   tp.mse<-round(mean(this.file$tpmapped),rounded)
   tp.se<-round(sd(this.file$tpmapped)/sqrtn,rounded)

   this.file<-read.csv(paste("altramsay/alttp-",my.error,"-1000.results.txt",sep=""))
   tprs.mse<-round(mean(this.file$tp),rounded)
   tprs.se<-round(sd(this.file$tp)/sqrtn,rounded)

   # cat it out
   cat("1000 & ",my.error," & ",
      tprs.mse," (",tprs.se,") & ",
      mapped.mse," (",mapped.se,") & ",
      tp.mse," (",tp.se,") & ",
      soap.mse," (",soap.se,") \\\\ \n",sep="")

}


