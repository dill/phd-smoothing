# run all the simulations

for (domain in c("disk","rect")){
   for (samp.size in c(1000,100)){
      for(noise.level in c(0.02, 2)){
         source("runsim.R")

      }
   }
}



