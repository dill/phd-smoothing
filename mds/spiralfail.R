options(echo=FALSE)
source("mds.R")
source("spiral/make_spiral.R")



spir.dat<-make_spiral(30,n.grid=50)
bnd<-spir.dat$bnd

my.grid<-make_soap_grid(bnd,15)

#tst<-list()
#tst$x<-c(4.295244,3.115185)
#tst$y<-c(3.455492,5.123384)

tst<-pe(my.grid,c(5,45))



D.grid<-create_distance_matrix(tst$x,tst$y,bnd,faster=1)

options(echo=TRUE)
