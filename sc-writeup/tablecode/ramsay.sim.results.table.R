# R code to generate the tables
# for normal Ramsay simulations

# just generates the middle of the table

# column headings are:
# Sample size & Noise level & P-spline MSE (\emph{se}) & Thin plate MSE (\emph{se}) & Soap MSE (\emph{se}) \\


## First the default settings from soap-package help file
this.file<-read.csv(paste("../../ramseysim/pspline.results.txt",sep=""))
mapped.mse<-signif(mean(this.file$mapped),3)
mapped.se<-signif(sd(this.file$mapped),3)

this.file<-read.csv(paste("../../ramseysim/results.file.txt",sep=""))
soap.mse<-signif(mean(this.file$soap),3)
soap.se<-signif(sd(this.file$soap),3)

this.file<-read.csv(paste("../../ramseysim/tp-noisey-0.5.results.txt",sep=""))
tp.mse<-signif(mean(this.file$tpmapped),3)
tp.se<-signif(sd(this.file$tpmapped),3)

# cat it out
cat("1000 & 0.3 & ",mapped.mse," (",mapped.se,") & ",
    tp.mse," (",tp.se,") & ",
    soap.mse," (",soap.se,") \\\\ \n",sep="")



## Different sample sizes
sample.sizes<-c(500,250,100)

for(my.size in sample.sizes){
   # read in the table
   this.file<-read.csv(paste("../../ramseysim/sample.size.",my.size,".results.txt",sep=""))

   # calculate the mses
   soap.mse<-signif(mean(this.file$soap),3)
   mapped.mse<-signif(mean(this.file$mapped),3)
   
   # calculate the se(mse)
   soap.se<-signif(sd(this.file$soap),3)
   mapped.se<-signif(sd(this.file$mapped),3)

   # read in the tp stuff    
   this.file<-read.csv(paste("../../ramseysim/tp-sample.size.",my.size,".results.txt",sep=""))
   tp.mse<-signif(mean(this.file$tpmapped),3)
   tp.se<-signif(sd(this.file$tpmapped),3)

   # cat it out
   cat(my.size," & 0.3 & ",mapped.mse," (",mapped.se,") & ",
   tp.mse," (",tp.se,") & ",
   soap.mse," (",soap.se,") \\\\ \n",sep="")

}


## different error levels
error.levels<-c(0.5,1,2)

for(my.error in error.levels){
   # read in the table
   this.file<-read.csv(paste("../../ramseysim/noisey-",my.error,".results.txt",sep=""))

   # calculate the mses
   soap.mse<-signif(mean(this.file$soap),3)
   mapped.mse<-signif(mean(this.file$mapped),3)
   
   # calculate the se(mse)
   soap.se<-signif(sd(this.file$soap),3)
   mapped.se<-signif(sd(this.file$mapped),3)

   # read in the tp stuff    
   this.file<-read.csv(paste("../../ramseysim/tp-noisey-",my.error,".results.txt",sep=""))
   tp.mse<-signif(mean(this.file$tpmapped),3)
   tp.se<-signif(sd(this.file$tpmapped),3)

   # cat it out
   cat("1000 & ",my.error," & ",mapped.mse," (",mapped.se,") & ",
   tp.mse," (",tp.se,") & ",
   soap.mse," (",soap.se,") \\\\ \n",sep="")

}


