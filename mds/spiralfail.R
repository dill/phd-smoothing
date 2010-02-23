options(echo=FALSE)
source("mds.R")
source("spiral/make_spiral.R")



spir.dat<-make_spiral(50,n.grid=10)
bnd<-spir.dat$bnd

tst<-list()
tst$x<-c(4.295244,3.115185)
tst$y<-c(3.455492,5.123384)





D.grid<-create_distance_matrix(tst$x,tst$y,bnd)

options(echo=TRUE)
