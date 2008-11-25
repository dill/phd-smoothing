# Some test shapes.

### Square
# Number of vertices
nvertices<-4
# Position of vertices
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,10,0)
polyvertices[2]<-complex(1,0,10)
polyvertices[3]<-complex(1,-10,0)
polyvertices[4]<-complex(1,0,-10)
source("poly.centre.R")
wc<-poly.centre(polyvertices)

# slightly different square, all in + quadrant
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

# Different square again, now aligned to the axes
nvertices<-4
# Position of vertices
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,5,2.5)
polyvertices[2]<-complex(1,2.5,5)
polyvertices[3]<-complex(1,0,2.5)
polyvertices[4]<-complex(1,2.5,0)
source("poly.centre.R")
wc<-poly.centre(polyvertices)




### L-shape
# Number of vertices
nvertices<-6
# Position of vertices
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,0,0)
polyvertices[2]<-complex(1,2,0)
polyvertices[3]<-complex(1,2,1)
polyvertices[4]<-complex(1,1,1)
polyvertices[5]<-complex(1,1,2)
polyvertices[6]<-complex(1,0,2)
wc<-complex(1,0.5,0.5)

