# R code to generate the tables
# for normal Ramsay simulations

# just generates the middle of the table

# column headings are:
# Sample size & Noise level & P-spline MSE (\emph{se}) & Soap MSE (\emph{se}) \\


## First the default settings from soap-package help file
this.file<-read.csv(paste("../../ramseysim/pspline.results.txt",sep=""))
mapped.mse<-mean(this.file$mapped)
mapped.se<-sd(this.file$mapped)

this.file<-read.csv(paste("../../ramseysim/results.file.txt",sep=""))
soap.mse<-mean(this.file$soap)
soap.se<-sd(this.file$soap)

# cat it out
cat("1000 & 0.3 & ",mapped.mse," (",mapped.se,") & ",
    soap.mse," & (",soap.se,") \\\\ \n")



## Different sample sizes
sample.sizes<-c(500,250,100)

for(my.size in sample.sizes){
   # read in the table
   this.file<-read.csv(paste("../../ramseysim/sample.size.",my.size,".results.txt",sep=""))

   # calculate the mses
   soap.mse<-mean(this.file$soap)
   mapped.mse<-mean(this.file$mapped)
   
   # calculate the se(mse)
   soap.se<-sd(this.file$soap)
   mapped.se<-sd(this.file$mapped)


   # cat it out
   cat(my.size," & 0.3 & ",mapped.mse," (",mapped.se,") & ",
       soap.mse," & (",soap.se,") \\\\ \n")

}


## different error levels
error.levels<-c(0.5,1,2)

for(my.error in error.levels){
   # read in the table
   this.file<-read.csv(paste("../../ramseysim/noisey-",my.error,".results.txt",sep=""))

   # calculate the mses
   soap.mse<-mean(this.file$soap)
   mapped.mse<-mean(this.file$mapped)
   
   # calculate the se(mse)
   soap.se<-sd(this.file$soap)
   mapped.se<-sd(this.file$mapped)


   # cat it out
   cat("1000 & ",my.error," & ",mapped.mse," (",mapped.se,") & ",
       soap.mse," & (",soap.se,") \\\\ \n")

}


