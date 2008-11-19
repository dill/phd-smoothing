# Just to see how we do with the FELSPLINE example.

library(soap)

fel<-fs.boundary()


nvertices<-length(fel)
polyvertices<-complex(nvertices-1,fel$x[-1],fel$y[-1])



#this doesn't work, wc is non within the polygon
#source("poly.centre.R")
#wc<-poly.centre(polyvertices)
wc<-complex(1,-0.5,0)

source("sc.R")
