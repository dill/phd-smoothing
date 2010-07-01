# make a comb

source("mds.R")

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

# check it works
#plot(0:10,0:10,asp=1,type="n")
#lines(start,lwd=2)
#lines((patt+add)*dec,col="red",lwd=2)
#lines((patt+2*add)*dec^2,col="green",lwd=2)
#lines((end+3*add)*dec^3,lwd=2,col="blue")

# make the bnd
#bnd<-rbind(start,(patt+add)*dec,(patt+2*add)*dec^2,(end+3*add)*dec^3)
#bnd<-rbind(start,patt+add,patt+2*add,end+3*add)

bnd<-start
n<-2
for(i in 1:n){
#   bnd<-rbind(bnd,(patt+i*add)*dec^(0.5*i))
   bnd<-rbind(bnd,(patt+i*add))
}

dec<-dec^6

big<-15
bnd<-rbind(bnd,(patt+add*big)*dec)
bnd<-rbind(bnd,(patt+add*(big+1))*dec)


#bnd<-rbind(bnd,(end+(i+1)*add)*dec^(0.5*(i+1)),bnd[1,])
bnd<-rbind(bnd,(end+(big+2)*add)*dec,bnd[1,])


# make grid and plot
par(mfrow=c(3,3))
source("makesoapgrid.R")
bnd<-list(x=bnd[,1],y=bnd[,2])
plot(bnd,type="l",asp=1)
gr<-make_soap_grid(bnd,100)
points(gr,pch=19,cex=0.3)

# make some space
plot(0:10,0:10,axes=FALSE,type="n",xlab="",ylab="")
plot(0:10,0:10,axes=FALSE,type="n",xlab="",ylab="")


D<-create_distance_matrix(gr$x,gr$y,bnd)

# 2d mds plot
grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)
plot(grid.mds$points,pch=19,cex=0.3)

plot(0:10,0:10,axes=FALSE,type="n",xlab="",ylab="")
plot(0:10,0:10,axes=FALSE,type="n",xlab="",ylab="")

# 3d mds plot
grid.mds3<-cmdscale(D,eig=TRUE,k=3,x.ret=TRUE)
plot(grid.mds3$points[,1],grid.mds3$points[,2],pch=19,cex=0.3)
plot(grid.mds3$points[,1],grid.mds3$points[,3],pch=19,cex=0.3)
plot(grid.mds3$points[,2],grid.mds3$points[,3],pch=19,cex=0.3)

#library(rgl)
#plot3d(grid.mds3$points[,1],grid.mds3$points[,2],grid.mds3$points[,3])

# 4d mds plot
quartz()
par(mfrow=c(2,3))
grid.mds4<-cmdscale(D,eig=TRUE,k=4,x.ret=TRUE)
plot(grid.mds4$points[,1],grid.mds4$points[,2],pch=19,cex=0.3)
plot(grid.mds4$points[,2],grid.mds4$points[,3],pch=19,cex=0.3)
plot(grid.mds4$points[,1],grid.mds4$points[,3],pch=19,cex=0.3)
plot(grid.mds4$points[,3],grid.mds4$points[,4],pch=19,cex=0.3)
plot(grid.mds4$points[,4],grid.mds4$points[,1],pch=19,cex=0.3)
plot(grid.mds4$points[,2],grid.mds4$points[,4],pch=19,cex=0.3)

