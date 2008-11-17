# temp file to write some wrapper code....

# First load the polygon.locator function
source("polygon.locator.R")

# Run it
ret<-polygon.locator()
# **click click click**

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

# call the gridcode
source("gridcode.R")

