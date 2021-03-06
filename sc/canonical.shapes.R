# setup some canonical shapes in R
# take the shapes and put them into a list called my.shapes

# those shapes that do not currently work in matlab are commented out


# when the calculated wc was outside of the polygon we set it manually
# otherwise we use the simple script to find the centre
source("poly.centre.R")

my.shapes<-list()

# define here the figures that are working
working.figs<-c(1,3,5,6,7,8)


### square
nvertices<-4
polyvertices<-vector("complex",nvertices)
polyvertices[1]<-complex(1,-3,-3)
polyvertices[2]<-complex(1,3,-3)
polyvertices[3]<-complex(1,3,3)
polyvertices[4]<-complex(1,-3,3)
wc<-poly.centre(polyvertices)
my.shapes[[1]]<-list(points=polyvertices,title="square",wc=wc)

### triangle
#nvertices<-3
#polyvertices<-vector("complex",nvertices)
#polyvertices[1]<-complex(1,-3,-3)
#polyvertices[2]<-complex(1,3,-3)
#polyvertices[3]<-complex(1,0,3)
#wc<-poly.centre(polyvertices)
#my.shapes[[2]]<-list(points=polyvertices,title="triangle",wc=wc)

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
wc<-complex(1,2.5,1.25)
my.shapes[[3]]<-list(points=polyvertices,title="horse shoe",wc=wc)



### wiggly top 
#nvertices<-20
#polyvertices<-vector("complex",nvertices)
#polyvertices[1]<-complex(1,-3,-2)
#polyvertices[2]<-complex(1,3,-2)
#polyvertices[3]<-complex(1,3,2)
#polyvertices[4]<-complex(1,2.5,0.5)
#polyvertices[5]<-complex(1,2,1)
#polyvertices[6]<-complex(1,1.5,0.5)
#polyvertices[7]<-complex(1,1.5,1.5)
#polyvertices[8]<-complex(1,1,0.5)
#polyvertices[9]<-complex(1,0.5,1.5)
#polyvertices[10]<-complex(1,0,0.5)
#polyvertices[11]<-complex(1,0,2)
#polyvertices[12]<-complex(1,-0.5,0.5)
#polyvertices[13]<-complex(1,-1,2)
#polyvertices[14]<-complex(1,-1,0.5)
#polyvertices[15]<-complex(1,-1.5,1)
#polyvertices[16]<-complex(1,-2,0.5)
#polyvertices[17]<-complex(1,-2,1)
#polyvertices[18]<-complex(1,-2.5,0.5)
#polyvertices[19]<-complex(1,-2.5,2)
#polyvertices[20]<-complex(1,-3,0.5)
#wc<-poly.centre(polyvertices)
#my.shapes[[4]]<-list(points=polyvertices,title="wiggly top",wc=wc)


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
wc<-complex(1,0,-1)
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
wc<-complex(1,0.5,0)
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
wc<-complex(1,0,0)
my.shapes[[8]]<-list(points=polyvertices,title="kink",wc=wc)


### lobe
#nvertices<-18
#polyvertices<-vector("complex",nvertices)
#polyvertices[1]<-complex(1,-3,-1.5)
#polyvertices[2]<-complex(1,-2,-1)
#polyvertices[3]<-complex(1,-2,0)
#polyvertices[4]<-complex(1,-1.5,1.5)
#polyvertices[5]<-complex(1,-1,2.5)
#polyvertices[6]<-complex(1,-0.5,3.5)
#polyvertices[7]<-complex(1,1.5,3)
#polyvertices[8]<-complex(1,2.5,1.5)
#polyvertices[9]<-complex(1,2.5,0)
#polyvertices[10]<-complex(1,3.5,-0.5)
#polyvertices[11]<-complex(1,3,-2.5)
#polyvertices[12]<-complex(1,1,-3)
#polyvertices[13]<-complex(1,-1,-2)
#polyvertices[14]<-complex(1,0,0.5)
#polyvertices[15]<-complex(1,0,2)
#polyvertices[16]<-complex(1,-1,1)
#polyvertices[17]<-complex(1,-1.5,-1)
#polyvertices[18]<-complex(1,-2,-2.5)
#wc<-poly.centre(polyvertices)
#my.shapes[[9]]<-list(points=polyvertices,title="lobe",wc=wc)


