# run all the simulations

# this should be pretty obvious

for (samp.size in c(1000,100)){
#   for(noise.level in c(0.02, 2)){
   for(noise.level in c(2)){
      source("runsim.R")
   }
}



