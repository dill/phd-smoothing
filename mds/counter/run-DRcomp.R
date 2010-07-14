# run the comparison between D and Euclidean distance in MDS space

source("mds.R")
source("DRcomp.R")
source("latlong2km.R")


# first the comb...
cat("comb\n")
# start bit
start<-t(matrix(c(0,0,0,20,1,20,1,1),2,4))
# middle pattern
patt<-t(matrix(c(0,1,0,20,1,20,1,1),2,4))
# add to make the next pattern
add <-t(matrix(c(2,0,2,0,2,0,2,0),2,4))
# end bit
end<-t(matrix(c(0,1,0,20,1,20,1,0),2,4))

# make it shorter
dec<-t(matrix(c(1,1,1,0.75,1,0.75,1,1),2,4))
bnd<-start
n<-2
for(i in 1:n){
   bnd<-rbind(bnd,(patt+i*add))
}

dec<-dec^6
big<-15
bnd<-rbind(bnd,(patt+add*big)*dec)
bnd<-rbind(bnd,(patt+add*(big+1))*dec)
bnd<-rbind(bnd,(end+(big+2)*add)*dec,bnd[1,])
source("makesoapgrid.R")
bnd<-list(x=bnd[,1],y=bnd[,2])
gr<-make_soap_grid(bnd,c(71,50))
D<-create_distance_matrix(gr$x,gr$y,bnd)

cat("k=2, crit=",DRcomp(D,2),"\n",sep="")
cat("k=3, crit=",DRcomp(D,3),"\n",sep="")
cat("k=4, crit=",DRcomp(D,4),"\n",sep="")
cat("k=5, crit=",DRcomp(D,5),"\n",sep="")
cat("k=6, crit=",DRcomp(D,6),"\n",sep="")

gc()

############################################
# Ramsay
bnd <- fs.boundary()
bnd<-pe(bnd,seq(1,length(bnd$x),8))
bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))
# create points within the boundary 
m<-45;n<-25
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]
my.grid<-list(x=xx,y=yy)
D<-create_distance_matrix(xx,yy,bnd)


cat("\n\n")
cat("Ramsay\n")
cat("k=2, crit=",DRcomp(D,2),"\n",sep="")
cat("k=3, crit=",DRcomp(D,3),"\n",sep="")
cat("k=4, crit=",DRcomp(D,4),"\n",sep="")
cat("k=5, crit=",DRcomp(D,5),"\n",sep="")
cat("k=6, crit=",DRcomp(D,6),"\n",sep="")

gc()
###################################################
# wt2
bnd <- read.csv("wt2-verts.csv",header=FALSE)
names(bnd)<-c("x","y")
# create the grid
my.grid<-create_refgrid(bnd,120)
# create D
D<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=0)
cat("\n\n")
cat("wt2\n")
cat("k=2, crit=",DRcomp(D,2),"\n",sep="")
cat("k=3, crit=",DRcomp(D,3),"\n",sep="")
cat("k=4, crit=",DRcomp(D,4),"\n",sep="")
cat("k=5, crit=",DRcomp(D,5),"\n",sep="")
cat("k=6, crit=",DRcomp(D,6),"\n",sep="")

gc()

###############################
# aral
bnd<-read.csv("aral/aralbnd.csv")
bnd.km<-latlong2km(bnd[,2],bnd[,3],59.5,45)
bnd<-list(x=bnd.km$km.e,y=bnd.km$km.n)

# create the grid
my.grid<-create_refgrid(bnd,50)
# create D
D<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=0)
cat("\n\n")
cat("aral\n")
cat("k=2, crit=",DRcomp(D,2),"\n",sep="")
cat("k=3, crit=",DRcomp(D,3),"\n",sep="")
cat("k=4, crit=",DRcomp(D,4),"\n",sep="")
cat("k=5, crit=",DRcomp(D,5),"\n",sep="")
cat("k=6, crit=",DRcomp(D,6),"\n",sep="")

gc()






