
source("mds.R")
source("spiral/make_spiral.R")
source("makesoapgrid.R")



spir.dat<-make_spiral(spiral.res=25,n.grid=50)
bnd<-spir.dat$bnd

my.grid<-make_soap_grid(bnd,15)


D<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=0)

D.t<-read.csv("path-tests/spir-D.csv")

D.t<-as.matrix(D.t)
D.t<-D.t[,2:69]

if(max(abs(D-D.t))>(.Machine$double.eps)){
   cat("# Uh oh!",max(abs(D-D.t))," not the same as \"truth\"\n")
}
options(echo=TRUE)

