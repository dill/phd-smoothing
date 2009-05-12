# Script to create the PDFs of the boxplots.

# preliminary stuff...
# make the horseshoe
library(soap)

fsb <- list(fs.boundary())
# create the grid
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
## truth
tru <- fs.test(xx,yy)
# read in the predicition grid from matlab
predback.real<-read.csv("../matlab/preal.csv",header=F)
predback.imag<-read.csv("../matlab/pimag.csv",header=F)
prediction.grid<-data.frame(v=predback.real[[1]],w=predback.imag[[1]])

# bit of faffing with titles
names(fsb[[1]]) <- c("x","y") ## correct boundary names
insiders<-inSide(fsb,x=xx,y=yy)
names(fsb[[1]]) <- c("v","w") ## correct boundary names

# make our rough horseshoe
horse<-list(x=c(),y=c())
horse$x[1]<-max(fsb[[1]]$v)
horse$y[1]<-max(fsb[[1]]$w)
horse$x[2]<-min(fsb[[1]]$v)
horse$y[2]<-max(fsb[[1]]$w)
horse$x[3]<-min(fsb[[1]]$v)
horse$y[3]<-min(fsb[[1]]$w)
horse$x[4]<-max(fsb[[1]]$v)
horse$y[4]<-min(fsb[[1]]$w)
horse$x[5]<-max(fsb[[1]]$v)
horse$y[5]<-max(fsb[[1]]$w[fsb[[1]]$w<0 & fsb[[1]]$v>0.5])
horse$x[6]<-max(fsb[[1]]$v[fsb[[1]]$v<0])
horse$y[6]<-max(fsb[[1]]$w[fsb[[1]]$w<0 & fsb[[1]]$v>0.5])
horse$x[7]<-max(fsb[[1]]$v[fsb[[1]]$v<0])
horse$y[7]<-min(fsb[[1]]$w[fsb[[1]]$w>0 & fsb[[1]]$v>0.5])
horse$x[8]<-max(fsb[[1]]$v)
horse$y[8]<-min(fsb[[1]]$w[fsb[[1]]$w>0 & fsb[[1]]$v>0.5])

############# Setup done by now... ##########

### the horseshoe along with it's morphed version (grid)
pdf("mapping-grid.pdf",6,6)

par(mfrow=c(1,2))

# real domain
plot(xx[insiders],pch=".",yy[insiders],xlab="",ylab="",asp=1)
# put the vertex labels in the right place
text(horse$x[c(1,8,5,4)],horse$y[c(1,8,5,4)],labels=c(1,8,5,4),pos=4,offset=0.3)
text(horse$x[c(2,3)],horse$y[c(2,3)],labels=c(2,3),pos=2,offset=0.2)
text(horse$x[c(6,7)],horse$y[c(6,7)],labels=c(6,7),pos=2,offset=0.2)
lines(horse$x[c(1,8)],horse$y[c(1,8)])
lines(horse)

# mapped domain
plot(prediction.grid,pch=".",xlab="",ylab="")
lines()
dev.off()


##################################################
### the horseshoe along with it's morphed version (heatmap)
pdf("mapping-heatmap.pdf",6,6)
par(mfrow=c(1,2))
image(xm,yn,matrix(tru,m,n),col=heat.colors(100),xlab="",ylab="",asp=1)

dev.off()

##################################################

##################################################
### 4 pane boxplot, same as in soap-package
# soap, soap with boundary, sc+tp, sc+ps

##################################################

#################################################
### noisey data results 
# 3 levels of noise * 2 functions = 6 panes

pdf("noisey.boxplots.pdf",3,9)

# load data
lo<-read.csv("noisey-0.5.results.txt",header=T)
me<-read.csv("noisey-1.results.txt",header=T)
hi<-read.csv("noisey-2.results.txt",header=T)

# plot the boxplots
par(mfrow=c(3,2))

# first row low noise
ylims<-c(0,max(lo$mapped,lo$soap))
# soap 
boxplot(lo$soap,ylim=ylims)
# mapped
boxplot(lo$mapped,ylim=ylims)

# second row medium noise
ylims<-c(0,max(me$mapped,me$soap))
# soap 
boxplot(me$soap,ylim=ylims)
# mapped
boxplot(me$mapped,ylim=ylims)

# third row, high noise
ylims<-c(0,max(hi$mapped,hi$soap))
# soap 
boxplot(hi$soap,ylim=ylims)
# mapped
boxplot(hi$mapped,ylim=ylims)

dev.off()
##################################################

##################################################
### sample size results
# 3 sizes * 2 functions = 6 panes
# load the data
sm<-read.csv("sample.size.100.results.txt",header=T)
med<-read.csv("sample.size.250.results.txt",header=T)
large<-read.csv("sample.size.500.results.txt",header=T)

# log transform
sm<-log(sm)
med<-log(med)
large<-log(large)

# write to pdf
pdf("sample.size.boxplots.pdf",4,4)

# plot the boxplots
par(mfrow=c(3,2))

# first row small
ylims<-c(min(sm$mapped,sm$soap),max(sm$mapped,sm$soap))
# soap 
boxplot(sm$soap,ylim=ylims)
#mapped
boxplot(sm$mapped,ylim=ylims)

# first row small
ylims<-c(min(med$mapped,med$soap),max(med$mapped,med$soap))
# soap 
boxplot(med$soap,ylim=ylims)
#mapped
boxplot(med$mapped,ylim=ylims)

# first row small
ylims<-c(min(large$mapped,large$soap),max(large$mapped,large$soap))
# soap 
boxplot(large$soap,ylim=ylims)
#mapped
boxplot(large$mapped,ylim=ylims)

dev.off()

##################################################

