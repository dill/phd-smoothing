
source("mds.R")
source("spiral/make_spiral.R")
source("makesoapgrid.R")



spir.dat<-make_spiral(spiral.res=25,n.grid=50)
bnd<-spir.dat$bnd

my.grid<-make_soap_grid(bnd,15)


D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd)

