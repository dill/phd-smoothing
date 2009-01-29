% setup the canonical domains in matlab



% create a grid
x_points=[-5:0.1:5]
y_points=[-5:0.1:5]
[X,Y]=meshgrid(x_points,y_points)




%%% square
poly=[-3 -3; 3 -3; 3 3;-3 3]
vertex_points=[1 2 3 4]

% find the internal points
internal_points=inpoly([X(:) Y(:)], poly)

% create a complex set of points and polygon
complex_points=complex(X(internal_points),Y(internal_points))
complex_poly=polygon(complex(poly(:,1),poly(:,2)))

% with rectmap, you need to specify the vertices but not the centre
f=rectmap(complex_poly, vertex_points)
%f=center(f,0)
plot(evalinv(f,complex_points),'.')


%%% triangle
-3 -3; 3 -3; 0 3;

%%% Ramsey's horse shoe
poly= [-2.5 -0.5; 2.5 -0.5; 3.5 0.5; 3.5 2; 2.5 3; -2.5 3; -2.5 1.5; 2 1.5; 2 1;]
vertex_points=[1 9 6 7]


%%% wiggly top 
-3 -2;
3 -2;
3 2;
2.5 0.5;
2 1;
1.5 0.5;
1.5 1.5;
1 0.5;
0.5 1.5;
0 0.5;
0 2;
-0.5 0.5;
-1 2;
-1 0.5;
-1.5 1;
-2 0.5;
-2 1;
-2.5 0.5;
-2.5 2;
-3 0.5;


%%% spiked wiggly top 
nvertices<-18
polyvertices<-vector("complex",nvertices)
,-3,-2)
,3,-2)
,3,0.5)
,2.5,1)
,2.5,0.5)
,2,1)
,1.5,0.5)
,1,1)
,0.5,0.5)
,0,1)
,-0.5,0.5)
,-0.5,1)
,-1,0.5)
,-2,1.5)
,-2.5,3)
,-3,3.5)
,-2.5,1.5)
,-3,0.5)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[5]]<-list(points=polyvertices,title="wiggly top with spike",wc=wc)


%%% kite 
nvertices<-6
polyvertices<-vector("complex",nvertices)
,0,-2)
,0.5,1)
,1.5,2)
,0,3.5)
,-1.5,2)
,-0.5,1)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[6]]<-list(points=polyvertices,title="kite",wc=wc)


%%% beak 
nvertices<-9
polyvertices<-vector("complex",nvertices)
,-0.5,-3)
,1.5,-3)
,1.5,3)
,-0.5,3)
,-0.5,1)
,-2.5,2)
,-1,0)
,-2.5,-2.5)
,-0.5,-1)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[7]]<-list(points=polyvertices,title="beak",wc=wc)



%%% kink 
nvertices<-10
polyvertices<-vector("complex",nvertices)
,-3.5,0.5)
,-1,0.5)
,0,-0.5)
,1,0.5)
,3.5,0.5)
,3.5,1.5)
,1,1.5)
,0,0.5)
,-1,1.5)
,-3.5,1.5)
source("poly.centre.R")
wc<-poly.centre(polyvertices)
my.shapes[[8]]<-list(points=polyvertices,title="kink",wc=wc)


%%% lobe
nvertices<-18
polyvertices<-vector("complex",nvertices)
,-3,-1.5)
,-2,-1)
,-2,0)
,-1.5,1.5)
,-1,2.5)
,-0.5,3.5)
,1.5,3)
,2.5,1.5)
,2.5,0)
,3.5,-0.5)
,3,-2.5)
,1,-3)
,-1,-2)
,0,0.5)
,0,2)
,-1,1)
,-1.5,-1)
,-2,-2.5)
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
