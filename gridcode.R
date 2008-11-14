
# need the routine inSide from soap
# http://www.maths.bath.ac.uk/~sw283/simon/software.html
require(mgcv)
require(soap)

# Setup the polygon
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,10,0)
polyvertices[2]<-complex(1,0,10)
polyvertices[3]<-complex(1,-10,0)
polyvertices[4]<-complex(1,0,-10)

# Add extra vertex for going back to the start
polyvertices[5]<-polyvertices[1]


# set the bounds for the grid program
real.points<-c()
imag.points<-c()

# be a bit crafty
my.index<-c(1,2,3,4)

for (i in my.index){
   real.points<-c(real.points,Re(polyvertices[i]),Re(polyvertices[i+1]),NA)
   imag.points<-c(imag.points,Im(polyvertices[i]),Im(polyvertices[i+1]),NA)
}


bnd<-list()
bnd$x<-real.points
bnd$y<-imag.points
inSide(bnd,c(1,2,4555,-10),c(1,1221,3,0))
plot(bnd)
lines(bnd)


