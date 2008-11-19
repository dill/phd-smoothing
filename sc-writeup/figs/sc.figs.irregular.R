# Code to draw diagrams.
# They worked at the time!
# Needs to be run in the same directory as sc.r

# required packages
library(mgcv)
library(soap)

# Irregular domain
# Number of vertices
nvertices<-9
# Position of vertices
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,8.5921,0.3638116)
polyvertices[2]<-complex(1,8.82615,6.103527)
polyvertices[3]<-complex(1,3.403966,6.233975)
polyvertices[4]<-complex(1,2.896856,8.799454)
polyvertices[5]<-complex(1,-3.617567,7.755869)
polyvertices[6]<-complex(1,-2.837397,2.755360)
polyvertices[7]<-complex(1,-9.35182,0.9290866)
polyvertices[8]<-complex(1,-6.270146,-6.202075)
polyvertices[9]<-complex(1,5.900512,-7.637003)

# Find the centre
source("poly.centre.R")
wc<-poly.centre(polyvertices)

# Do the mapping
source("sc.R")

# Set up the polygon again
polyvertices<-ret$vertices
polyvertices[nvertices+1]<-polyvertices[1]
resolution<-0.5


# set to print to pdf
pdf("irregulardomain.pdf",width=10,height=5)

# set the resolution
resolution<-0.5

# call gridcode
source("gridcode.R")

# kill the output
dev.off()

# Unload the shared object
dyn.unload("scpack.so")
