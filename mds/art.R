
load("aral/aralfit.RData")

source("mds.R")
source("mds-wrap.R")
source("insert.mds.R")
source("intexp/smooth2.c.R")
library(soap)

#insert.mds(bnd,mds.grid,grid.mds,bnd,faster=0,debug=1)


mds.fit<-gam(chl~s(x,y,k=70,bs="mdstp",xt=list(bnd=bnd,op=mds.grid,mds.obj=grid.mds)),data=aral.mds,family=Gamma(link="log"))

#p1=list(x=15.789678,y=-105.085315); p2=list(x=-9.887906,y=153.667789);

