# make a comb


bnd<-matrix

0,0
0,5
1,5
1,1
2,1



# start bit
start<-t(matrix(c(0,0,0,5,1,5,1,1),2,4))
# middle pattern
patt<-t(matrix(c(0,1,0,5,1,5,1,1),2,4))
# add to make the next pattern
add <-t(matrix(c(2,0,2,0,2,0,2,0),2,4))
# end bit
end<-t(matrix(c(0,1,0,5,1,5,1,0),2,4))

# make it shorter
dec<-t(matrix(c(1,1,1,0.75,1,0.75,1,1),2,4))

# check it works
#plot(0:10,0:10,asp=1,type="n")
#lines(start,lwd=2)
#lines((patt+add)*dec,col="red",lwd=2)
#lines((patt+2*add)*dec^2,col="green",lwd=2)
#lines((end+3*add)*dec^3,lwd=2,col="blue")

# make the bnd
bnd<-rbind(start,(patt+add)*dec,(patt+2*add)*dec^2,(end+3*add)*dec^3)
bnd<-rbind(bnd,bnd[1,])


# make grid and plot
par(mfrow=c(4,4))
source("makesoapgrid.R")
bnd<-list(x=bnd[,1],y=bnd[,2])
plot(bnd,type="l",asp=1)
gr<-make_soap_grid(bnd,30)
points(gr,pch=19,cex=0.3)

# make some space
plot(0:10,0:10)
plot(0:10,0:10)
plot(0:10,0:10)


source("mds.R")

D<-create_distance_matrix(gr$x,gr$y,bnd)

grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)
plot(grid.mds$points,pch=19,cex=0.3)

plot(0:10,0:10)
plot(0:10,0:10)
plot(0:10,0:10)

grid.mds<-cmdscale(D,eig=TRUE,k=3,x.ret=TRUE)
plot(grid.mds$points[,1],grid.mds$points[,2],pch=19,cex=0.3)
plot(grid.mds$points[,1],grid.mds$points[,3],pch=19,cex=0.3)
plot(grid.mds$points[,2],grid.mds$points[,3],pch=19,cex=0.3)


