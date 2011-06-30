
pdf(file="alterstep-proof.pdf",width=7,height=2)

par(mfrow=c(1,3))
par(mar=c(0,1.8,0,1.8))

source("alterstep-simplest.R")
source("alterstep-crater.R")
source("alterstep-convex.R")

dev.off()
