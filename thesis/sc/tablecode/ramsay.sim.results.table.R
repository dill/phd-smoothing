# R code to generate the tables
# for normal Ramsay simulations

# just generates the middle of the table

# column headings are:
# Sample size & Noise level & P-spline MSE (\emph{se}) & Thin plate MSE (\emph{se}) & Soap MSE (\emph{se}) \\

sqrtn<-sqrt(1000)

## First the default settings from soap-package help file
this.file<-read.csv(paste("ramsay/pspline.results.txt",sep=""))
mapped.mse<-round(mean(this.file$mapped),5)
mapped.se<-round(sd(this.file$mapped)/sqrtn,5)

this.file<-read.csv(paste("ramsay/results.file.txt",sep=""))
soap.mse<-round(mean(this.file$soap),5)
soap.se<-round(sd(this.file$soap)/sqrtn,5)

this.file<-read.csv(paste("ramsay/tp-noisey-0.5.results.txt",sep=""))
tp.mse<-round(mean(this.file$tpmapped),5)
tp.se<-round(sd(this.file$tpmapped)/sqrtn,5)

this.file<-read.csv(paste("ramsay/tp-0.3-1000.results.txt",sep=""))
tprs.mse<-round(mean(this.file$tp),5)
tprs.se<-round(sd(this.file$tp)/sqrtn,5)


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
   this.file<-read.csv(paste("ramsay/sample.size.",my.size,".results.txt",sep=""))

   # calculate the mses
   soap.mse<-round(mean(this.file$soap),5)
   mapped.mse<-round(mean(this.file$mapped),5)
   
   # calculate the se(mse)
   soap.se<-round(sd(this.file$soap)/sqrtn,5)
   mapped.se<-round(sd(this.file$mapped)/sqrtn,5)

   # read in the tp stuff    
   this.file<-read.csv(paste("ramsay/tp-sample.size.",my.size,".results.txt",sep=""))
   tp.mse<-round(mean(this.file$tpmapped),5)
   tp.se<-round(sd(this.file$tpmapped)/sqrtn,5)

   this.file<-read.csv(paste("ramsay/tp-0.3-",my.size,".results.txt",sep=""))
   tprs.mse<-round(mean(this.file$tp),5)
   tprs.se<-round(sd(this.file$tp)/sqrtn,5)

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
   this.file<-read.csv(paste("ramsay/noisey-",my.error,".results.txt",sep=""))

   # calculate the mses
   soap.mse<-round(mean(this.file$soap),5)
   mapped.mse<-round(mean(this.file$mapped),5)
   
   # calculate the se(mse)
   soap.se<-round(sd(this.file$soap)/sqrtn,5)
   mapped.se<-round(sd(this.file$mapped)/sqrtn,5)

   # read in the tp stuff    
   this.file<-read.csv(paste("ramsay/tp-noisey-",my.error,".results.txt",sep=""))
   tp.mse<-round(mean(this.file$tpmapped),5)
   tp.se<-round(sd(this.file$tpmapped)/sqrtn,5)

   this.file<-read.csv(paste("ramsay/tp-",my.error,"-1000.results.txt",sep=""))
   tprs.mse<-round(mean(this.file$tp),5)
   tprs.se<-round(sd(this.file$tp)/sqrtn,5)

   # cat it out
   cat("1000 & ",my.error," & ",
      tprs.mse," (",tprs.se,") & ",
      mapped.mse," (",mapped.se,") & ",
      tp.mse," (",tp.se,") & ",
      soap.mse," (",soap.se,") \\\\ \n",sep="")

}


