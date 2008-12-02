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


# points near the vertices
nr.polyvertices<-polyvertices
nr.polyvertices[1]<-nr.polyvertices[1]-0.1
nr.polyvertices[2]<-nr.polyvertices[2]-0.1i
nr.polyvertices[3]<-nr.polyvertices[3]+0.1
nr.polyvertices[4]<-nr.polyvertices[4]+0.1i


ret.vertices<-sc.map.backwards(nr.polyvertices,nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)

mapped.vertices<-ret.vertices$eval.points

text(x=Re(mapped.vertices),y=Im(mapped.vertices),labels=seq(1,nvertices,1)[!ret.vertices$errors],adj=1.5,col="blue")















