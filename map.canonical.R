# setup some canonical shapes in R and map them using the unit disk transform.
# mainly used to compare withe transforms to rectangle done in matlab

# load the shapes
source("canonical.shapes.R")


par(mfrow=c(3,3))

for(i in 1:9){
   plot(my.shapes[[i]]$points,main=my.shapes[[i]]$title,pch=".",xlab="",ylab="")
   lines(c(my.shapes[[i]]$points,my.shapes[[i]]$points[1]))
   points(my.shapes[[i]]$wc)
   text(x=Re(my.shapes[[i]]$wc),y=Im(my.shapes[[i]]$wc),labels="wc",adj=1.5,col="blue")
}
