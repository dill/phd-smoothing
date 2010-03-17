# create a bounding box around wigglytop2

verts<-read.csv("figverts-real.csv")
names(verts)<-c("x","y")

# bounding box object
bbox<-list(x=c(),y=c())

# now some boring faffing around. Start at bottom left.
bbox$x[1]<-min(verts$x);bbox$y[1]<-min(verts$y);
bbox$x[2]<-min(verts$x);bbox$y[2]<-max(verts$y);
bbox$x[3]<--1.75;bbox$y[3]<-3;
bbox$x[4]<--1.75;bbox$y[4]<--1;
bbox$x[5]<--1.5;bbox$y[5]<--1;
bbox$x[6]<--1.5;bbox$y[6]<-3;
bbox$x[7]<--0.5;bbox$y[7]<-3;
bbox$x[8]<--0.5;bbox$y[8]<--1;
bbox$x[9]<-0;bbox$y[9]<-2;
bbox$x[10]<-0.75;bbox$y[10]<-1;
bbox$x[11]<-1.25;bbox$y[11]<--1;
bbox$x[12]<-1.5;bbox$y[12]<-0.25;
bbox$x[13]<-2.25;bbox$y[13]<-1;
bbox$x[14]<-3.25;bbox$y[14]<--0.5;
bbox$x[15]<-3.25;bbox$y[15]<--3;
bbox$x[16]<-min(verts$x);bbox$y[16]<--3;

bbox$x<-bbox$x[length(bbox$x):1]
bbox$y<-bbox$y[length(bbox$y):1]


write.csv(bbox,"figverts.csv",row.names=FALSE)
