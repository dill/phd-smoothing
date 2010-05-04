# make sure the spiral looks okay...
source("spiral/make_spiral.R")

#

spdat<-make_spiral()


image(z=spdat$mat,
      col=heat.colors(1000))

