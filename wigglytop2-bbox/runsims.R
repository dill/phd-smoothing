# run all the simulations

# this should be pretty obvious

for (samp.size in c(1000,500)){
   for(noise.level in c(0.02, 2)){
      source("runsim.R")
   }
}



