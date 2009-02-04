# plot the canonical domains

# source the shapes
source("canonical.shapes.R")

par(mfrow=c(3,2))

for(i in working.figs){
   plot(my.shapes[[i]]$points,main=my.shapes[[i]]$title,pch=".",xlab="",ylab="")
   lines(c(my.shapes[[i]]$points,my.shapes[[i]]$points[1]))
   points(my.shapes[[i]]$wc)
   text(x=Re(my.shapes[[i]]$wc),y=Im(my.shapes[[i]]$wc),labels="wc",adj=1.5,col="blue")
}
