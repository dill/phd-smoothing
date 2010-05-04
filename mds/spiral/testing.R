# make sure the spiral looks okay...
source("spiral/make_spiral.R")

#

spdat<-make_spiral(spiral.res=45,n.grid=45)
spdat<-make_spiral(spiral.res=100,n.grid=100)


image(z=spdat$mat,
      col=heat.colors(1000))

