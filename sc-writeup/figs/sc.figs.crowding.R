# Code to draw diagrams.
# They worked at the time!
# Needs to be run in the same directory as sc.r

# required packages
library(mgcv)
library(soap)

# Irregular domain
# Number of vertices
nvertices<-14
# Position of vertices
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,7.6967,8.877934)
polyvertices[2]<-complex(1,6.897026,9.486692)
polyvertices[3]<-complex(1,4.732053,9.725847)
polyvertices[4]<-complex(1,4.322464,8.182211)
polyvertices[5]<-complex(1,5.102634,8.443108)
polyvertices[6]<-complex(1,5.258668,7.747385)
polyvertices[7]<-complex(1,5.960821,8.030022)
polyvertices[8]<-complex(1,5.804787,7.421264)
polyvertices[9]<-complex(1,5.161147,6.442904)
polyvertices[10]<-complex(1,2.430550,7.464747)
polyvertices[11]<-complex(1,1.143269,4.290511)
polyvertices[12]<-complex(1,3.074191,1.072792)
polyvertices[13]<-complex(1,7.05306,1.725032)
polyvertices[14]<-complex(1,7.540666,4.11658)


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
pdf("crowdeddisk.pdf",width=10,height=5)

# set the resolution
resolution<-0.5

# call gridcode
source("gridcode.R")

# kill the output
dev.off()

# Unload the shared object
dyn.unload("scpack.so")
