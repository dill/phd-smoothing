
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

bnd<-list()
bnd$x<-Re(polyvertices)
bnd$y<-Im(polyvertices)


gridd<-seq(-10,10,0.5)

my.grid<-c()
it<-1

for (i in gridd){
   for (j in gridd){
      my.grid[it]<-complex(1,i,j)
      it<-it+1
   }
}




inside.points<-inSide(bnd,Re(my.grid),Im(my.grid))

par(mfrow=c(2,1),pch=".")

plot(my.grid[inside.points],type="p")
lines(bnd)


eval.points<-sc.map.backwards(my.grid[inside.points],nvertices,betam,nptsq,qwork,accuracy,prevertices,polyvertices,complex.scale.factor,wc)


plot(eval.points)

lines(bnd)
