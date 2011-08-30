# generate figure 1....

# layout:
#  3 panels
#     1 - detection function humpback from Williams and Thomas w. hist
#     2 - average detection function pilot whale from Dan Pike w. hist
#     3 - levels of BSS covar for pilot whale from Dan Pike w.o. hist

postscript(file="figure1.eps",width=7.5,height=3,paper="special",horizontal=FALSE)

par(mfrow=c(1,3),mar=c(4,4.2,4,2))

# 1
# read in the plot data. 3 columns: histogram x coords, hist y coords, detfct
hb.plot<-read.table("humpback.txt")

# make the histogram
plot(hb.plot$V1,hb.plot$V2,type="l",axes=F,xlab="Distance (m)",
     ylab="Probability of detection",main="Humpback")
axis(2,at=c(0,0.5,1))
axis(1,at=c(0,1000,2000))
# add in the baseline and the first bar line
lines(x=c(2000,0,0),y=c(0,0,0.39636))

lines(x=hb.plot$V1,y=hb.plot$V3)
box()

#### Dan Pike
# 2
h.breaks<-c(seq(0,1000,len=10),seq(1250,3000,250)) 

dp.df<-read.table("danpike-detfct.txt")

dp.dat<-read.csv("../analyses/danpike.csv")
dp.dat<-dp.dat$distance[dp.dat$distance<=3000]

a<-hist(dp.dat,breaks=h.breaks,plot=F)

auh<-sum(a$density*diff(a$breaks))
auc<-sum(dp.df$V3[-1]*diff(dp.df$V1))

#dp.df$V3<-dp.df$V3*1/(auh/auc)

a$density<-a$density/(auh/auc)

plot(a,axes=F,xlab="Distance (m)",freq=FALSE,
     ylab="Probability of detection",main="Long-finned pilot whale")


#plot(dp.df$V1,dp.df$V2,type="l",axes=F,xlab="Distance (m)",
#     ylab="Probability of detection",main="Long-finned pilot whale")
axis(2,at=c(0,0.5,1))
axis(1,at=c(0,1000,2000,3000))
# add in the baseline and the first bar line
#lines(x=c(3000,0,0),y=c(0,0,1.1197))

lines(x=dp.df$V1,y=dp.df$V3)
box()

# 3
# quantiles at v2, v3, v4  are BSS=1.5,2.0,3.0 
dp.df<-read.table("danpike-covar.txt")

#auc<-sum(dp.df$V2[-1]*diff(dp.df$V1))
dp.df$V2<-dp.df$V2*(auh/auc)
#auc<-sum(dp.df$V3[-1]*diff(dp.df$V1))
dp.df$V3<-dp.df$V3*(auh/auc)
#auc<-sum(dp.df$V4[-1]*diff(dp.df$V1))
dp.df$V4<-dp.df$V4*(auh/auc)

ymax<-max(dp.df$V2,dp.df$V3,dp.df$V4)

plot(dp.df$V1,dp.df$V2,type="l",axes=F,xlab="Distance (m)",
     ylim=c(0,ymax),
     ylab="Probability of detection",main="Long-finned pilot whale\nQuantiles of Beaufort sea state",col=grey(0.25))
axis(2,at=c(0,ymax/2,ymax),labels=c(0,0.5,1))
axis(1,at=c(0,1000,2000,3000))

lines(x=dp.df$V1,y=dp.df$V3,col=grey(0.5))
lines(x=dp.df$V1,y=dp.df$V4,col=grey(0.75))
box()

dev.off()
