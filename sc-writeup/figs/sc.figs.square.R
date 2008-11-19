# Code to draw diagrams.
# They worked at the time!
# Needs to be run in the same directory as sc.r

# required packages
library(mgcv)
library(soap)

# Square domain

### Square
# Number of vertices
nvertices<-4
# Position of vertices
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,10,0)
polyvertices[2]<-complex(1,0,10)
polyvertices[3]<-complex(1,-10,0)
polyvertices[4]<-complex(1,0,-10)
wc<-complex(1,0,sqrt(2))

# Set the variables
polyvertices<-ret$vertices
nvertices<-length(polyvertices)
wc<-ret$centre

# Do the mapping
source("sc.R")

# Set up the polygon again
polyvertices<-ret$vertices
polyvertices[nvertices+1]<-polyvertices[1]
resolution<-0.5

# set to print to pdf
pdf("squaredomain.pdf",width=10,height=5)


# call gridcode
source("gridcode.R")

# kill the output
dev.off()

# Unload the shared object
dyn.unload("scpack.so")
