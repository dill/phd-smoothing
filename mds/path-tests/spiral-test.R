# spiral check and timing

source("mds.R")
source("spiral/make_spiral.R")
source("makesoapgrid.R")

# calculate the partial paths?
faster=0


spir.dat<-make_spiral(spiral.res=25,n.grid=50)
bnd<-spir.dat$bnd

my.grid<-make_soap_grid(bnd,15)


timed<-system.time(D<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=faster))[3]

D.t<-read.csv("path-tests/spir-D.csv")

D.t<-as.matrix(D.t)
D.t<-D.t[,2:69]

if(max(abs(D-D.t))>(.Machine$double.eps)){
   cat("# Uh oh!",max(abs(D-D.t))," not the same as \"truth\"\n")
}


cat("Running create_distance_matrix (faster=",faster,") took: ",timed,"\n",sep="")

options(echo=TRUE)

