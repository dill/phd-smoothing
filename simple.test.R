# Simple test of square

# Number of vertices
nvertices<-4
# Position of vertices
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,5,5)
polyvertices[2]<-complex(1,0,5)
polyvertices[3]<-complex(1,0,0)
polyvertices[4]<-complex(1,5,0)
source("poly.centre.R")
wc<-poly.centre(polyvertices)



# the mapping
source("sc.R")

resolution<-0.1


# call the gridcode
source("gridcode.R")


