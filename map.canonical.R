# setup some canonical shapes in R and map them using the unit disk transform.
# mainly used to compare with the transforms to rectangle done in matlab

# pdf output
pdf("canonical.map.pdf",20,5)


# load the shapes
source("canonical.shapes.R")

j<-1

for(i in working.figs){
   # plot the originals
#   plot(my.shapes[[i]]$points,main=my.shapes[[i]]$title,pch=".",xlab="",ylab="")
#   lines(c(my.shapes[[i]]$points,my.shapes[[i]]$points[1]))
#   points(my.shapes[[i]]$wc)
#   text(x=Re(my.shapes[[i]]$wc),y=Im(my.shapes[[i]]$wc),labels="wc",adj=1.5,col="blue")

   polyvertices<-my.shapes[[i]]$points
   nvertices<-length(polyvertices)
   wc<-my.shapes[[i]]$wc


   # do the transform
   source("sc.R")


   resolution<-0.1

   # write it out to a pdf
   pdf(paste("R-test-",j,".pdf",sep=""),12,6)

   j<-j+1

   # call the gridcode
   source("gridcode.R")

   # Unload the shared object
   dyn.unload("scpack.so")

   # pdf close
   dev.off()
}




