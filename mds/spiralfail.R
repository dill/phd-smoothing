
source("mds.R")
source("spiral/make_spiral.R")



spir.dat<-make_spiral(50,n.grid=10)
bnd<-spir.dat$bnd


tst<-list(x=c(3.737994,4.196905),
          y=c(6.204425,6.204425))
D.grid<-create_distance_matrix(tst$x,tst$y,bnd)

