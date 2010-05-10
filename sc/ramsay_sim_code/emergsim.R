# do some emergency simulations

nl<-c(0.3,0.3,0.3,0.3,0.5,1,2)
samps<-c(1000,500,250,100,1000,1000,1000)

j<-1

for(j in 1:length(nl)){

   # set the noise level
   noise.level<-nl[j]
   samp.size<-samps[j]

   source("make.samples.R")

   source("fit.tp.R")


}
