# setup some canonical shapes in R


my.shapes<-list()

### square
nvertices<-4
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-3,-3)
polyvertices[2]<-complex(1,3,-3)
polyvertices[3]<-complex(1,3,-3)
polyvertices[4]<-complex(1,-3,3)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[1]]<-list(points=polyvertices,title="square",wc=wc)

### triangle
nvertices<-3
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-3,-3)
polyvertices[2]<-complex(1,3,-3)
polyvertices[3]<-complex(1,0,3)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[2]]<-list(points=polyvertices,title="triangle",wc=wc)

### Ramsey's horse shoe
nvertices<-10
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-2.5,-0.5)
polyvertices[2]<-complex(1,2.5,-0.5)
polyvertices[3]<-complex(1,3.5,0.5)
polyvertices[4]<-complex(1,3.5,2)
polyvertices[5]<-complex(1,2.5,3)
polyvertices[6]<-complex(1,-2.5,3)
polyvertices[7]<-complex(1,-2.5,1.5)
polyvertices[8]<-complex(1,2,1.5)
polyvertices[9]<-complex(1,2,1)
polyvertices[10]<-complex(1,-2.5,1)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[3]]<-list(points=polyvertices,title="horse shoe",wc=wc)



### wiggly top 
nvertices<-20
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-3,-2)
polyvertices[2]<-complex(1,3,-2)
polyvertices[3]<-complex(1,3,2)
polyvertices[4]<-complex(1,2.5,0.5)
polyvertices[5]<-complex(1,2,1)
polyvertices[6]<-complex(1,1.5,0.5)
polyvertices[7]<-complex(1,1.5,1.5)
polyvertices[8]<-complex(1,1,0.5)
polyvertices[9]<-complex(1,0.5,1.5)
polyvertices[10]<-complex(1,0,0.5)
polyvertices[11]<-complex(1,0,2)
polyvertices[12]<-complex(1,-0.5,0.5)
polyvertices[13]<-complex(1,-1,2)
polyvertices[14]<-complex(1,-1,0.5)
polyvertices[15]<-complex(1,-1.5,1)
polyvertices[16]<-complex(1,-2,0.5)
polyvertices[17]<-complex(1,-2,1)
polyvertices[18]<-complex(1,-2.5,0.5)
polyvertices[19]<-complex(1,-2.5,2)
polyvertices[20]<-complex(1,-3,0.5)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[4]]<-list(points=polyvertices,title="wiggly top",wc=wc)


### spiked wiggly top 
nvertices<-18
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-3,-2)
polyvertices[2]<-complex(1,3,-2)
polyvertices[3]<-complex(1,3,0.5)
polyvertices[4]<-complex(1,2.5,1)
polyvertices[5]<-complex(1,2.5,0.5)
polyvertices[6]<-complex(1,2,1)
polyvertices[7]<-complex(1,1.5,0.5)
polyvertices[8]<-complex(1,1,1)
polyvertices[9]<-complex(1,0.5,0.5)
polyvertices[10]<-complex(1,0,1)
polyvertices[11]<-complex(1,-0.5,0.5)
polyvertices[12]<-complex(1,-0.5,1)
polyvertices[13]<-complex(1,-1,0.5)
polyvertices[14]<-complex(1,-2,1.5)
polyvertices[15]<-complex(1,-2.5,3)
polyvertices[16]<-complex(1,-3,3.5)
polyvertices[17]<-complex(1,-2.5,1.5)
polyvertices[18]<-complex(1,-3,0.5)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[5]]<-list(points=polyvertices,title="wiggly top with spike",wc=wc)


### kite 
nvertices<-6
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,0,-2)
polyvertices[2]<-complex(1,0.5,1)
polyvertices[3]<-complex(1,1.5,2)
polyvertices[4]<-complex(1,0,3.5)
polyvertices[5]<-complex(1,-1.5,2)
polyvertices[6]<-complex(1,-0.5,1)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[6]]<-list(points=polyvertices,title="kite",wc=wc)


### beak 
nvertices<-9
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-0.5,-3)
polyvertices[2]<-complex(1,1.5,-3)
polyvertices[3]<-complex(1,1.5,3)
polyvertices[4]<-complex(1,-0.5,3)
polyvertices[5]<-complex(1,-0.5,1)
polyvertices[6]<-complex(1,-2.5,2)
polyvertices[7]<-complex(1,-1,0)
polyvertices[8]<-complex(1,-2.5,-2.5)
polyvertices[9]<-complex(1,-0.5,-1)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[7]]<-list(points=polyvertices,title="beak",wc=wc)



### kink 
nvertices<-10
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-3.5,0.5)
polyvertices[2]<-complex(1,-1,0.5)
polyvertices[3]<-complex(1,0,-0.5)
polyvertices[4]<-complex(1,1,0.5)
polyvertices[5]<-complex(1,3.5,0.5)
polyvertices[6]<-complex(1,3.5,1.5)
polyvertices[7]<-complex(1,1,1.5)
polyvertices[8]<-complex(1,0,0.5)
polyvertices[9]<-complex(1,-1,1.5)
polyvertices[10]<-complex(1,-3.5,1.5)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[8]]<-list(points=polyvertices,title="kink",wc=wc)


### lobe
nvertices<-18
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-3,-1.5)
polyvertices[2]<-complex(1,-2,-1)
polyvertices[3]<-complex(1,-2,0)
polyvertices[4]<-complex(1,-1.5,1.5)
polyvertices[5]<-complex(1,-1,2.5)
polyvertices[6]<-complex(1,-0.5,3.5)
polyvertices[7]<-complex(1,1.5,3)
polyvertices[8]<-complex(1,2.5,1.5)
polyvertices[9]<-complex(1,2.5,0)
polyvertices[10]<-complex(1,3.5,-0.5)
polyvertices[11]<-complex(1,3,-2.5)
polyvertices[12]<-complex(1,1,-3)
polyvertices[13]<-complex(1,-1,-2)
polyvertices[14]<-complex(1,0,0.5)
polyvertices[15]<-complex(1,0,2)
polyvertices[16]<-complex(1,-1,1)
polyvertices[17]<-complex(1,-1.5,-1)
polyvertices[18]<-complex(1,-2,-2.5)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[9]]<-list(points=polyvertices,title="lobe",wc=wc)


par(mfrow=c(3,3))

for(i in 1:9){
   plot(my.shapes[[i]]$points,main=my.shapes[[i]]$title,pch=".",xlab="",ylab="")
   lines(c(my.shapes[[i]]$points,my.shapes[[i]]$points[1]))
   points(my.shapes[[i]]$wc)
   text(x=Re(my.shapes[[i]]$wc),y=Im(my.shapes[[i]]$wc),labels="wc",adj=1.5,col="blue")
}
